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


import IDE.CompletionHelper
    (getIsWordChar, replaceWordStart, findWordStart, findWordEnd, longestCommonPrefix)



-- | Open a new argument helper popup window.
initArgumentHelper :: String -- ^ Function name
        -> EditorView
        -> (Int, Int) -- ^ Top left position of the popup
        -> IDEAction
initArgumentHelper functionName sourceView (x, y) = do
    liftIO $ putStrLn $ "functionName: " ++ functionName
    window          <- openNewWindow
    registerHandler window sourceView
    description     <- MetaInfoProvider.getDescription functionName
    addContent window description
    addArgumentsToSourceView sourceView functionName

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
    sourceView `onKeyPress` \name modifier keyVal -> do
        let closeIfVisible = (do
                visible <- liftIO $ Gtk.get window Gtk.widgetVisible
                if visible then (do
                        liftIO $ Gtk.widgetDestroy window
                        return True
                    )
                    else return False
                )

        case (name, modifier) of
            ("Return", _) -> closeIfVisible
            ("Escape", _) -> closeIfVisible
            -- don't signal that these keys have been handled:
            ("Up", _) -> closeIfVisible >> return False
            ("Down", _) -> closeIfVisible >> return False
            ("Control_L", _) -> closeIfVisible >> return False
            ("Control_R", _) -> closeIfVisible >> return False
            (_, _) -> return False
    return ()


addContent :: Gtk.Window -> String -> IDEAction
addContent window description = do
    prefs               <- readIDE prefs
    descriptionBuffer   <- newGtkBuffer Nothing description
    descriptionView     <- newView descriptionBuffer (textviewFont prefs)
    _ <- if (Nothing /= (getSourceView descriptionView)) then do
            --TODO upadte window size on content length (doesn't work yet)
            (heigth, width)     <- liftIO $ Gtk.widgetGetSize $ fromJust $ getSourceView descriptionView
            liftIO $ putStrLn $ (show width) ++ (show heigth)
            return ()
        else return ()

    descriptionScrolledWindow <- getScrolledWindow descriptionView
    liftIO $ Gtk.containerAdd window descriptionScrolledWindow


getSourceView :: EditorView -> Maybe SourceView
getSourceView (GtkEditorView s) = Just s
#ifdef LEKSAH_WITH_YI
getSourceView YiEditorView _ = Nothing
#endif


-- TODO only add when at end of line (or somehow else check if inserting arguments as text into sourceView is disturbing the workflow)
addArgumentsToSourceView :: EditorView -> String -> IDEAction
addArgumentsToSourceView sourceView functionName = do
    workspaceInfo' <- MetaInfoProvider.getWorkspaceInfo
    case workspaceInfo' of
        Nothing -> return ()
        Just ((GenScopeC (PackScope _ symbolTable1)),(GenScopeC (PackScope _ symbolTable2))) ->
            liftIO $ putStrLn $ unlines $ map (getFirstLine . show . fromJust) $ filter (/= Nothing) mbTypesList
            where mbTypesList = map CTypes.dscMbTypeStr (MetaInfoProvider.getIdentifierDescr functionName symbolTable1 symbolTable2)

-- TODO this function does not work as expected
getFirstLine :: String -> String
getFirstLine "" = ""
getFirstLine (x:"") = x:""
getFirstLine ('\\':'n':xs) = ""
getFirstLine (x:xs) = getFirstLine xs


#ifdef LEKSAH_WITH_YI
addTypesToSourceView (YiEditorView _) _ = return ()
#endif
--
--            return ((foldr (\d f -> shows (Present d) .  showChar '\n' . f) id
--                (MetaInfoProvider.getIdentifierDescr name symbolTable1 symbolTable2)) "")



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


