-----------------------------------------------------------------------------
--
-- Module      :  IDE.ArgumentHelper
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.ArgumentHelper (
    initArgumentHelper
) where
import Prelude hiding(getChar, getLine)

import Data.List as List (stripPrefix, isPrefixOf, filter)
import Data.Char
import qualified Data.ByteString.Char8 as Char8
import Data.Maybe (fromJust, isJust)
import Data.IORef
import Control.Monad
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk.SourceView (SourceView)
import qualified Graphics.UI.Gtk as Gtk hiding(onKeyPress, onKeyRelease)
import Graphics.UI.Gtk (AttrOp (..)) -- import := unqualified for convenience
import Graphics.UI.Gtk.Gdk.EventM as Gtk
import IDE.Core.State

import qualified IDE.Metainfo.Provider as MetaInfoProvider
import qualified IDE.Core.CTypes as CTypes
import Data.Maybe (fromMaybe)

import Control.Monad.Reader.Class (ask)
import IDE.TextEditor

import qualified IDE.ArgumentHelper.Parser as Parser
import IDE.Core.Types (argsHelperMarks, argsHelperConnections)
import IDE.CompletionHelper
    (getIsWordChar, replaceWordStart, findWordStart, findWordEnd, longestCommonPrefix)



-- | various defs
tagName = "arg_helper_highlight" -- tag name for highlighting



-- | Open a new argument helper popup window.
initArgumentHelper :: String -- ^ Function name
        -> EditorView
        -> (Int, Int) -- ^ Top left position of the popup
        -> IDEAction
initArgumentHelper functionName sourceView (x, y) = do
    liftIO $ putStrLn $ "functionName: " ++ functionName
    window          <- openNewWindow
    descriptions    <- MetaInfoProvider.getDescriptionList functionName
    saveMethodDescs $ generateSaveableFromList descriptions
    setLayout window
    updateSelectedMethodDesc window

{- TODO only add when at end of line
(or somehow else check if inserting arguments as text into sourceView is disturbing the workflow) -}
    addArgumentsToSourceView sourceView functionName
    registerHandler window sourceView
    liftIO $ Gtk.windowMove window x y
    liftIO $ Gtk.widgetShowAll window

--
-- | open a new popup window
--
openNewWindow :: IDEM (Gtk.Window)
openNewWindow = do
    let width = 600
    let height = 150
    windows    <- getWindows
    window     <- liftIO Gtk.windowNewPopup
    liftIO $ Gtk.windowSetTransientFor window (head windows)
    liftIO $ Gtk.set window [
                 Gtk.windowTypeHint      := Gtk.WindowTypeHintUtility,
                 Gtk.windowDecorated     := False,
                 Gtk.windowResizable     := True,
                 Gtk.windowDefaultWidth  := width,
                 Gtk.windowDefaultHeight := height
                 ]
    liftIO $ Gtk.containerSetBorderWidth window 1
    return window

--
-- | Set Layout for documentation window
--
setLayout :: Gtk.Window -> IDEAction
setLayout window = do
    prefs                           <- readIDE prefs
    curDescBuffer                   <- newGtkBuffer Nothing ""
    (GtkEditorView curDescView)     <- newViewWithoutScrolledWindow curDescBuffer (textviewFont prefs)

    otherDescsBuffer                <- newGtkBuffer Nothing ""
    (GtkEditorView otherDescsView)  <- newViewWithoutScrolledWindow otherDescsBuffer (textviewFont prefs)
    saveMethodDescBuffers (curDescBuffer, otherDescsBuffer)

    vbox <- liftIO $ Gtk.vBoxNew False 1
    liftIO $ Gtk.containerAdd window vbox
    liftIO $ Gtk.containerAdd vbox curDescView
    liftIO $ Gtk.containerAdd vbox otherDescsView
    return ()

--
-- TODO register mouseclicks to close window
-- | Register keys to be handled by the window.
--
registerHandler :: Gtk.Window -> EditorView -> IDEAction
registerHandler window sourceView = do
    buffer <- getBuffer sourceView
    connections <- sourceView `onKeyPress` \name modifier keyVal -> do
        liftIO $ putStrLn name
        let closeIfVisible = (do
                visible <- liftIO $ Gtk.get window Gtk.widgetVisible
                if visible then (do
                        highlightRemoveAll buffer
                        liftIO $ Gtk.widgetDestroy window
                        connections <- readIDE argsHelperConnections
                        liftIO $ signalDisconnectAll connections
                        (mbCurMarks, marks, start, end) <- readIDE argsHelperMarks
                        if (isJust mbCurMarks) then
                            (mapM_ (\(m1, m2) -> do
                                deleteMark buffer m1
                                deleteMark buffer m2) ((fromJust mbCurMarks):(start, end):marks))
                            else
                                (mapM_ (\(m1, m2) -> do
                                    deleteMark buffer m1
                                    deleteMark buffer m2) ((start, end):marks))
                        return True
                    )
                    else return False
                )
        let focusNextMarks cycleListFn = (do
                liftIO $ putStrLn "focusNextMarks called"
                (curMarks, marks, s, e) <- readIDE argsHelperMarks
                case (marks) of
                    [] -> return False
                    _ -> do
                        let (Just nextMarks, newMarks) = cycleListFn curMarks marks
                        printMarks buffer nextMarks -- debug output
                        modifyIDE_ $ \ide -> ide{argsHelperMarks = (Just nextMarks, newMarks, s, e)}
                        setFocusBetweenMarks buffer nextMarks
                        return True
                )
        case (name, modifier) of
            ("Return", _) -> closeIfVisible
            ("Escape", _) -> removeArgumentsFromSourceView buffer >> closeIfVisible -- TODO verify that this is desired behavior
            ("Tab", _) -> focusNextMarks cycleList >> return True
            ("ISO_Left_Tab", _) -> focusNextMarks cycleListBackwards >> return True
            ("Up", _) -> cycleToMethodType window buffer cycleListBackwards >> return True
            ("Down", _) -> cycleToMethodType window buffer cycleList >> return True
            -- don't signal that these keys have been handled:
            ("Control_L", _) -> closeIfVisible >> return False
            ("Control_R", _) -> closeIfVisible >> return False
            ("Alt_L", _) -> closeIfVisible >> return False
            (_, _) -> return False
    modifyIDE_ $ \ide -> ide{argsHelperConnections = connections}
    return ()


-- | Cycles through available types for given function name using given @cycleListFn@.
cycleToMethodType :: Gtk.Window -> EditorBuffer -> (Maybe String -> [String] -> (Maybe String, [String])) -> IDEAction
cycleToMethodType window buffer cycleListFn = do
    removeArgumentsFromSourceView buffer
    decl@(mbCurDecl, newDecls) <- cycleStrings cycleListFn argsHelperMethodDecls
    saveMethodDecls decl
    desc@(mbCurDesc, newDescs) <- cycleStrings cycleListFn argsHelperMethodDescs
    saveMethodDescs desc

    if (isJust mbCurDecl) then
        updateSelectedMethodDesc window >>
        (addArgumentsToSourceView' buffer $ Parser.parseArgumentsFromMethodDeclaration (fromJust mbCurDecl))
            else return ()

    where cycleStrings cycleFn getter = do
            (old, othersOld) <- readIDE getter
            let x = cycleFn old othersOld
            return x


updateSelectedMethodDesc :: Gtk.Window -> IDEAction
updateSelectedMethodDesc window = do
    (mbCurDesc, otherDescs) <- readIDE argsHelperMethodDescs
    if (isJust mbCurDesc) then (do
        (curBuf, otherBuf)<- getDeclBuffers
        setText curBuf $ fromJust mbCurDesc
        setText otherBuf $ unlines otherDescs
        return ()
        )
            else return ()

getDeclBuffers :: IDEM (EditorBuffer, EditorBuffer)
getDeclBuffers = readIDE argsHelperMethodDescBuffers


-- -----
-- Arguments
-- -----

addArgumentsToSourceView :: EditorView -> String -> IDEAction
#ifdef LEKSAH_WITH_YI
addArgumentsToSourceView _ _ = saveMarks [] >> return ()
#endif
addArgumentsToSourceView sourceView functionName = do
    workspaceInfo' <- MetaInfoProvider.getWorkspaceInfo
    case workspaceInfo' of
        Nothing -> return ()
        Just ((GenScopeC (PackScope _ symbolTable1)),(GenScopeC (PackScope _ symbolTable2))) -> do
            -- get type string of all matching functions
            liftIO $ putStrLn $ unlines typeList

            buffer <- getBuffer sourceView
            saveMethodDecls $ generateSaveableFromList typeList
            unless (typeList == []) (do
                let argTypes = Parser.parseArgumentsFromMethodDeclaration $ head typeList
                addArgumentsToSourceView' buffer argTypes
                )

            where mbTypesList = map CTypes.dscMbTypeStr mbDescrList
                  typeList    = map (Char8.unpack . fromJust) $
                                    filter (/= Nothing) mbTypesList
                  mbDescrList = MetaInfoProvider.getIdentifierDescr functionName symbolTable1 symbolTable2


addArgumentsToSourceView' :: EditorBuffer -> [Parser.ArgumentType] -> IDEAction
addArgumentsToSourceView' buffer argTypes = do
    -- save start of arguments
    sI <- getInsertIter buffer
    start <- createMark buffer sI True
    end <- createMark buffer sI False

    marksListList <- mapM (insertArgument buffer " ") argTypes
    let marksList = foldl (++) [] marksListList -- flatten lists
    case (marksList) of
        [] -> do
            liftIO $ putStrLn "no marks"
            saveMarks (Nothing, [], start, end)
            return ()
        (curMarks:newMarks) -> do
            saveMarks (Just curMarks, newMarks, start, end)
            mapM_ (highlightBetweenMarks buffer) marksList
            setFocusBetweenMarks buffer curMarks



insertArgument :: EditorBuffer -> String -> Parser.ArgumentType -> IDEM [(EditorMark, EditorMark)]
insertArgument buffer spacing (Parser.ArgumentTypePlain arg) = do
    insertIter <- getInsertIter buffer
    insert buffer insertIter spacing
    marks <- insertTextWithMarks buffer arg
    return [marks]
-- there should be at least one argType
insertArgument buffer spacing (Parser.ArgumentTypeTuple (firstArg:argTypes)) = do
    insertIter <- getInsertIter buffer
    insert buffer insertIter $ spacing ++ "("
    firstMarks <- insertArgument buffer "" firstArg
    otherMarksList <- mapM (insertArgument buffer ", ") argTypes
    -- flatten lists
    let allMarks = foldl (++) [] (firstMarks:otherMarksList)
    insertIterN <- getInsertIter buffer
    insert buffer insertIterN ")"
    return allMarks
insertArgument _ _ _ = return []


removeArgumentsFromSourceView :: EditorBuffer -> IDEAction
removeArgumentsFromSourceView buffer = do
    (_, _, start, end) <- readIDE argsHelperMarks
    deleteBetweenMarks buffer start end


-- -----
-- Marks
-- -----

setFocusBetweenMarks :: EditorBuffer -> (EditorMark, EditorMark) -> IDEAction
setFocusBetweenMarks buf (start, end) = do
    startI <- getIterAtMark buf start
    endI <- getIterAtMark buf end
    selectRange buf startI endI

{- | insertTextWithMarks @buffer@ @str@: inserts str in buffer at current cursor
    and moves curser to the end of str.
    Returns 'EditorMark' of start and end of inserted @str@. Marks have
    leftGravity.
 -}
insertTextWithMarks :: EditorBuffer -> String -> IDEM (EditorMark, EditorMark)
insertTextWithMarks buffer str = do
    cursorIter <- getInsertIter buffer
    startMark <- createMark buffer cursorIter True
    insert buffer cursorIter str
    startIter <- getIterAtMark buffer startMark
    endIter <- forwardCharsC startIter (length str)
    placeCursor buffer endIter
    endMark <- createMark buffer endIter True
    return (startMark, endMark)

deleteBetweenMarks :: EditorBuffer -> EditorMark -> EditorMark -> IDEAction
deleteBetweenMarks buffer s e = do
    sI <- getIterAtMark buffer s
    eI <- getIterAtMark buffer e
    delete buffer sI eI


-- -----
-- Highlighting
-- -----

highlightBetweenMarks :: EditorBuffer -> (EditorMark, EditorMark) -> IDEAction
highlightBetweenMarks buffer (s, e) = do
    -- get/create highlight-tag
    tagTable <- getTagTable buffer
    mbTag <- lookupTag tagTable tagName
    unless (isJust mbTag) (do
            t <- newTag tagTable tagName
            background t $ Gtk.Color 45000 45000 45000 -- some grey value...
            return ()
            )
    -- actual highlighting
    si <- getIterAtMark buffer s
    ei <- getIterAtMark buffer e
    applyTagByName buffer tagName si ei

highlightRemoveAll :: EditorBuffer -> IDEAction
highlightRemoveAll buffer = do
    s<- getStartIter buffer
    e <- getEndIter buffer
    removeTagByName buffer tagName s e


-- -----
-- Save helpers
-- -----

-- | Saves given marks to the IDE state.
saveMarks :: (Maybe (EditorMark, EditorMark), [(EditorMark, EditorMark)], EditorMark, EditorMark) -> IDEAction
saveMarks marks = do
    modifyIDE_ $ \ide -> ide{argsHelperMarks = marks}
    return ()

saveMethodDecls :: (Maybe String, [String]) -> IDEAction
saveMethodDecls mDecls = do
    modifyIDE_ $ \ide -> ide{argsHelperMethodDecls = mDecls}
    return ()

saveMethodDescs :: (Maybe String, [String]) -> IDEAction
saveMethodDescs mDescs = do
    modifyIDE_ $ \ide -> ide{argsHelperMethodDescs = mDescs}
    return ()

saveMethodDescBuffers :: (EditorBuffer, EditorBuffer) -> IDEAction
saveMethodDescBuffers bufs = do
    modifyIDE_ $ \ide -> ide{argsHelperMethodDescBuffers = bufs}
    return ()


-- -----
-- Various helpers
-- -----

cycleList :: Maybe a -> [a] -> (Maybe a, [a])
cycleList Nothing _ = (Nothing, [])
cycleList mbX [] = (mbX, [])
cycleList (Just oldCur) (x:xs) = (Just x, xs ++ [oldCur])

cycleListBackwards :: Maybe a -> [a] -> (Maybe a, [a])
cycleListBackwards Nothing _ = (Nothing, [])
cycleListBackwards mbX [] = (mbX, [])
cycleListBackwards (Just oldCur) l = (Just $ last l, (oldCur):(init l))

generateSaveableFromList :: [a] -> (Maybe a, [a])
generateSaveableFromList [] = (Nothing, [])
generateSaveableFromList (x:xs) = (Just x, xs)

-- | Helper function to print marks to console
printMarks :: EditorBuffer -> (EditorMark, EditorMark) -> IDEAction
printMarks buf (start, end) = do
        startI <- getIterAtMark buf start
        startPos <- getOffset startI
        endI <- getIterAtMark buf end
        endPos <- getOffset endI
        liftIO $ putStrLn $ "marks: start, end" ++ (show startPos) ++ ", " ++ (show endPos)
        return ()


--placeWindow :: Window -> EditorView -> IDEAction
--placeWindow window sourceView = do
--    buffer <- getBuffer sourceView
--    (selStart, end) <- getSelectionBounds buffer
--    isWordChar <- getIsWordChar sourceView
--    start <- findWordStart selStart isWordChar
--    currentWordStart <- getText buffer start end True
--    newWordStart <- do
--        if selectLCP && currentWordStart == wordStart && (not $ null options)
--            then do
--                let lcp = foldl1 longestCommonPrefix options
--                return lcp
--            else
--                return currentWordStart
--
--    when (isPrefixOf wordStart newWordStart) $ do
--        -- refill listStore with new completion possibilities/options
--        liftIO $ listStoreClear store
--        let newOptions = List.filter (isPrefixOf newWordStart) options
--        liftIO $ forM_ (take 200 newOptions) (listStoreAppend store)
--
--        Just namesSW                         <- liftIO $ widgetGetParent tree
--        (widthNamesWidget, _)                          <- liftIO $ widgetGetSize namesSW
--        Just paned                           <- liftIO $ widgetGetParent namesSW
--        Just first                           <- liftIO $ panedGetChild1 (castToPaned paned)
--        Just second                          <- liftIO $ panedGetChild2 (castToPaned paned)
--
--        Rectangle iterBufferX iterBufferY width height  <- getIterLocation sourceView start
--        (wWindow, hWindow)                              <- liftIO $ windowGetSize window
--        (iterWindowRelativeX, iterWindowRelativeY)      <- bufferToWindowCoords sourceView (iterBufferX, iterBufferY+height)
--        drawWindow                                      <- getDrawWindow sourceView
--        (windowOriginX, windowOriginY)                  <- liftIO $ drawWindowGetOrigin drawWindow
--
--        let iterAbsoluteX = windowOriginX + iterWindowRelativeX
--            iterAbsoluteY = windowOriginY + iterWindowRelativeY
--        screen                               <- liftIO $ windowGetScreen window
--        monitor                              <- liftIO $ screenGetMonitorAtPoint screen (iterAbsoluteX) (iterAbsoluteY)
--        monitorLeft                          <- liftIO $ screenGetMonitorAtPoint screen (iterAbsoluteX - wWindow + widthNamesWidget) (iterAbsoluteY)
--        monitorRight                         <- liftIO $ screenGetMonitorAtPoint screen (iterAbsoluteX + wWindow) (iterAbsoluteY)
--        monitorBelow                         <- liftIO $ screenGetMonitorAtPoint screen (iterAbsoluteX) (iterAbsoluteY + hWindow)
--        wScreen                              <- liftIO $ screenGetWidth screen
--        hScreen                              <- liftIO $ screenGetHeight screen
--
--        top <- if monitorBelow /= monitor || (iterAbsoluteY+hWindow) > hScreen
--            -- calculate Y position of the window after scrolling
--            then do
--                -- scroll text buffer to be able to show the whole window at the correct text position
--                sourceSW        <- getScrolledWindow sourceView
--                (_, hSource)    <- liftIO $ widgetGetSize sourceSW
--                scrollToIter sourceView end 0.1 (Just (1.0, 1.0 - (fromIntegral hWindow / fromIntegral hSource)))
--                (_, newY)       <- bufferToWindowCoords sourceView (iterBufferX, iterBufferY + height)
--                return (windowOriginY + newY)
--            else return (iterAbsoluteY)
--
--        swap <- if (monitorRight /= monitor || (iterAbsoluteX + wWindow) > wScreen) && monitorLeft == monitor && (iterAbsoluteX - wWindow + widthNamesWidget) > 0
--            -- reposition the completion window
--            -- calculate if swap of the two panes is needed
--            -- (if completion window is too far on the right of the screen, so the whole window can be shown)
--            then do
--                liftIO $ windowMove window (iterAbsoluteX - wWindow + widthNamesWidget) top
--                return $ first == namesSW
--            else do
--                liftIO $ windowMove window (iterAbsoluteX) top
--                return $ first /= namesSW
--
--        when swap $ liftIO $ do
--
--            pos <- panedGetPosition (castToPaned paned)
--            containerRemove (castToPaned paned) first
--            containerRemove (castToPaned paned) second
--            panedAdd1 (castToPaned paned) second
--            panedAdd2 (castToPaned paned) first
--            panedSetPosition (castToPaned paned) (wWindow - pos)
--
--        when (not $ null newOptions) $ liftIO $ treeViewSetCursor tree [0] Nothing
--
--        liftIO $ widgetShowAll window
--
--    when (newWordStart /= currentWordStart) $
--        replaceWordStart sourceView isWordChar newWordStart


