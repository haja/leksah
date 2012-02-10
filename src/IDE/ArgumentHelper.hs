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

import Data.Char
import qualified Data.ByteString.Char8 as Char8 (unpack)
import Data.Maybe (fromJust, isJust)
import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Graphics.UI.Gtk as Gtk hiding(onKeyPress, onKeyRelease)
import Graphics.UI.Gtk (AttrOp (..)) -- import := unqualified for convenience
import Graphics.UI.Gtk.Gdk.EventM as Gtk

import IDE.Core.State
import qualified IDE.Metainfo.Provider as MetaInfoProvider
import qualified IDE.Core.CTypes as CTypes
import IDE.TextEditor

import qualified IDE.ArgumentHelper.Parser as Parser
import IDE.Core.Types



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
    setWindowLayout window
    updateSelectedMethodDesc window

{- only add when at end of line
(or somehow else check if inserting arguments as text into sourceView is disturbing the workflow) -}
    doInsertArguments <- getBuffer sourceView >>= getInsertIter >>= endsLine
    saveDoInsertArguments doInsertArguments
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
setWindowLayout :: Gtk.Window -> IDEAction
setWindowLayout window = do
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
                        mbMarks <- readIDE argsHelperMarks
                        when (isJust mbMarks) (do
                            let (curMarks, marks, start, end) = fromJust mbMarks
                            mapM_ (\(m1, m2) -> do
                                deleteMark buffer m1
                                deleteMark buffer m2)
                                    ((curMarks):(start, end):marks)
                            )
                        return True
                    )
                    else return False
                )
        let focusNextMarks cycleListFn = (do
                liftIO $ putStrLn "focusNextMarks called"
                mbMarks <- readIDE argsHelperMarks
                case (mbMarks) of
                    Nothing -> return False
                    (Just (curMarks, marks, s, e)) -> do
                        let (Just nextMarks, newMarks) = cycleListFn (Just curMarks) marks
                        printMarks buffer nextMarks -- debug output
                        modifyIDE_ $ \ide -> ide{argsHelperMarks = Just (nextMarks, newMarks, s, e)}
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
        (curBuf, otherBuf)<- getDescBuffers
        setText curBuf $ fromJust mbCurDesc
        setText otherBuf $ unlines otherDescs
        return ()
        )
            else return ()

getDescBuffers :: IDEM (EditorBuffer, EditorBuffer)
getDescBuffers = readIDE argsHelperMethodDescBuffers


-- -----
-- Arguments
-- -----

addArgumentsToSourceView :: EditorView -> String -> IDEAction
#ifdef LEKSAH_WITH_YI
addArgumentsToSourceView _ _ = saveMarks [] >> return ()
#endif
addArgumentsToSourceView sourceView functionName = do
    saveMarks Nothing -- create record field in any case
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
    doInsert <- readIDE argsHelperDoInsertArguments
    when doInsert (do
        -- save start of arguments
        sI <- getInsertIter buffer
        start <- createMark buffer sI True
        end <- createMark buffer sI False
        marksListList <- mapM (insertArgument buffer " ") argTypes
        let marksList = foldl (++) [] marksListList -- flatten lists
        case (marksList) of
            [] -> do
                liftIO $ putStrLn "no marks"
                saveMarks Nothing
                return ()
            (curMarks:newMarks) -> do
                saveMarks $ Just (curMarks, newMarks, start, end)
                mapM_ (highlightBetweenMarks buffer) marksList
                setFocusBetweenMarks buffer curMarks)


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
    mbMarks <- readIDE argsHelperMarks
    case mbMarks of
        (Just (_, _, start, end)) -> deleteBetweenMarks buffer start end
        Nothing -> return ()


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
saveMarks :: Maybe ((EditorMark, EditorMark), [(EditorMark, EditorMark)], EditorMark, EditorMark) -> IDEAction
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

saveDoInsertArguments :: Bool -> IDEAction
saveDoInsertArguments doInsertArguments = do
    modifyIDE_ $ \ide -> ide{argsHelperDoInsertArguments = doInsertArguments}
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
