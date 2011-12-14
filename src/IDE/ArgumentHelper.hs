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
import Data.Maybe (fromJust)
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



-- | Open a new argument helper popup window.
initArgumentHelper :: String -- ^ Function name
        -> EditorView
        -> (Int, Int) -- ^ Top left position of the popup
        -> IDEAction
initArgumentHelper functionName sourceView (x, y) = do
    liftIO $ putStrLn $ "functionName: " ++ functionName

    -- TODO begin user action somewhere
    window          <- openNewWindow
    description     <- MetaInfoProvider.getDescription functionName
    addContent window description
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
-- TODO register mouseclicks to close window
-- | Register keys to be handled by the window.
--
registerHandler :: Gtk.Window -> EditorView -> IDEAction
registerHandler window sourceView = do
    buffer <- getBuffer sourceView
    connections <- sourceView `onKeyPress` \name modifier keyVal -> do
        let closeIfVisible = (do
                visible <- liftIO $ Gtk.get window Gtk.widgetVisible
                if visible then (do
                        liftIO $ Gtk.widgetDestroy window
                        connections <- readIDE argsHelperConnections
                        liftIO $ signalDisconnectAll connections
                        (marks, start, end) <- readIDE argsHelperMarks
                        mapM_ (\(m1, m2) -> do
                            deleteMark buffer m1
                            deleteMark buffer m2) ((start, end):marks)
                        return True
                    )
                    else return False
                )
        let focusNextMarks = (do
                liftIO $ putStrLn "focusNextMarks called"
                (marks, s, e) <- readIDE argsHelperMarks
                case (marks) of
                    [] -> return False
                    _ -> do
                        let (nextMarks, newMarks) = cycleList marks
                        printMarks buffer nextMarks -- debug output
                        modifyIDE_ $ \ide -> ide{argsHelperMarks = (newMarks, s, e)}
                        setFocusBetweenMarks buffer nextMarks
                        return True
                )
        case (name, modifier) of
            ("Return", _) -> closeIfVisible
            ("Escape", _) -> closeIfVisible
            ("Tab", _) -> focusNextMarks >> return True
            ("Up", _) -> cycleToPrevMethodType window buffer >> return True
            ("Down", _) -> cycleToNextMethodType window buffer >> return True
            -- don't signal that these keys have been handled:
            ("Control_L", _) -> closeIfVisible >> return False
            ("Control_R", _) -> closeIfVisible >> return False
            (_, _) -> return False
    modifyIDE_ $ \ide -> ide{argsHelperConnections = connections}
    return ()

-- TODO change window to highlight selected method decl
cycleToNextMethodType :: Gtk.Window -> EditorBuffer -> IDEAction
cycleToNextMethodType window buffer = do
    removeArgumentsFromSourceView buffer
    methodDecls <- readIDE argsHelperMethodDecls
    let (_, newDecls) = cycleList methodDecls
    let (curDecl, _) = cycleList methodDecls
    saveMethodDecls newDecls
    addArgumentsToSourceView' buffer $ Parser.parseArgumentsFromMethodDeclaration curDecl

cycleToPrevMethodType :: Gtk.Window -> EditorBuffer -> IDEAction
cycleToPrevMethodType _ _ = return ()

removeArgumentsFromSourceView :: EditorBuffer -> IDEAction
removeArgumentsFromSourceView buffer = do
    (_, start, end) <- readIDE argsHelperMarks
    deleteBetweenMarks buffer start end


deleteBetweenMarks :: EditorBuffer -> EditorMark -> EditorMark -> IDEAction
deleteBetweenMarks buffer s e = do
    sI <- getIterAtMark buffer s
    eI <- getIterAtMark buffer e
    delete buffer sI eI

addContent :: Gtk.Window -> String -> IDEAction
addContent window description = do
    prefs               <- readIDE prefs
    descriptionBuffer   <- newGtkBuffer Nothing description
    descriptionView     <- newView descriptionBuffer (textviewFont prefs)
    _ <- if (Nothing /= (getSourceView descriptionView)) then do
            --TODO upadte window size on content length (doesn't work yet)
            (heigth, width)     <- liftIO $ Gtk.widgetGetSize $ fromJust $ getSourceView descriptionView
            return ()
        else return ()

    descriptionScrolledWindow <- getScrolledWindow descriptionView
    liftIO $ Gtk.containerAdd window descriptionScrolledWindow


getSourceView :: EditorView -> Maybe SourceView
getSourceView (GtkEditorView s) = Just s
#ifdef LEKSAH_WITH_YI
getSourceView YiEditorView _ = Nothing
#endif


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
            saveMethodDecls typeList
            addArgumentsToSourceView' buffer argTypes

            where mbTypesList = map CTypes.dscMbTypeStr mbDescrList
                  typeList    = map (tail . getFirstLine . show . fromJust) $
                                    filter (/= Nothing) mbTypesList
                  mbDescrList = MetaInfoProvider.getIdentifierDescr functionName symbolTable1 symbolTable2
                  argTypes =    Parser.parseArgumentsFromMethodDeclaration $ head typeList

saveMethodDecls :: [String] -> IDEAction
saveMethodDecls list = do
    modifyIDE_ $ \ide -> ide{argsHelperMethodDecls = list}
    return ()

addArgumentsToSourceView' :: EditorBuffer -> [String] -> IDEAction
addArgumentsToSourceView' buffer argTypes = do
    -- save start of arguments
    sI <- getInsertIter buffer
    start <- createMark buffer sI True
    end <- createMark buffer sI False
    -- TODO simplify this in one map?
    marksList <- mapM (\str -> do
        insertIter <- getInsertIter buffer
        insert buffer insertIter " "
        insertTextWithMarks buffer str
        ) argTypes
    case (marksList) of
        [] -> do
            liftIO $ putStrLn "no marks"
            saveMarks ([], start, end)
            return ()
        _ -> do
            let (curMarks, newMarks) = cycleList marksList
            saveMarks (newMarks, start, end)
            mapM_ (highlightBetweenMarks buffer) newMarks
            setFocusBetweenMarks buffer curMarks


-- TODO implement this method
highlightBetweenMarks :: EditorBuffer -> (EditorMark, EditorMark) -> IDEAction
highlightBetweenMarks _ _ = return ()

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

-- | Saves given marks to the IDE state.
saveMarks :: ([(EditorMark, EditorMark)], EditorMark, EditorMark) -> IDEAction
saveMarks marks = do
    modifyIDE_ $ \ide -> ide{argsHelperMarks = marks}
    return ()

cycleList :: [a] -> (a, [a])
cycleList (x:xs) = (x, xs ++ [x])

cycleListBackwards :: [a] -> (a, [a])
cycleListBackwards l = (last l, (last l):(init l))

-- | Helper function to print marsk to console
printMarks :: EditorBuffer -> (EditorMark, EditorMark) -> IDEAction
printMarks buf (start, end) = do
        startI <- getIterAtMark buf start
        startPos <- getOffset startI
        endI <- getIterAtMark buf end
        endPos <- getOffset endI
        liftIO $ putStrLn $ "marks: start, end" ++ (show startPos) ++ ", " ++ (show endPos)
        return ()

-- TODO replace method to replace escaped with real newline.
-- | Get the part of a string, until an "escaped" newline is found, i.e. "...\\n..."
getFirstLine :: String -> String
getFirstLine "" = ""
getFirstLine ('\\':'n':xs) = "" -- stop at first "escaped" newline
getFirstLine (x:xs) = x:(getFirstLine xs)


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


