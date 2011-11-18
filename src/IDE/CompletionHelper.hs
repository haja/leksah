-----------------------------------------------------------------------------
--
-- Module      :  IDE.CompletionHelper
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

module IDE.CompletionHelper (
    getIsWordChar
    , replaceWordStart
    , findWordStart
    , findWordEnd
    , longestCommonPrefix
) where
import Prelude hiding(getChar, getLine)

import Data.List as List (stripPrefix, isPrefixOf, filter)
import Data.Char
import Data.IORef
import Control.Monad
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk as Gtk hiding(onKeyPress, onKeyRelease)
import Graphics.UI.Gtk.Gdk.EventM as Gtk
import IDE.Core.State
import IDE.Metainfo.Provider(getDescription,getCompletionOptions)
import Control.Monad.Reader.Class (ask)
import IDE.TextEditor



getIsWordChar :: EditorView -> IDEM (Char -> Bool)
getIsWordChar sourceView = do
    ideR <- ask
    buffer <- getBuffer sourceView
    (_, end) <- getSelectionBounds buffer
    sol <- backwardToLineStartC end
    eol <- forwardToLineEndC end
    line <- getSlice buffer sol eol False

    let isImport = "import " `isPrefixOf` line
        isIdent a = isAlphaNum a || a == '\'' || a == '_'  || (isImport && a == '.')
        isOp    a = isSymbol   a || a == ':'  || a == '\\' || a == '*' || a == '/' || a == '-'
                                 || a == '!'  || a == '@'  || a == '%' || a == '&' || a == '?'
    prev <- backwardCharC end
    prevChar <- getChar prev
    case prevChar of
        Just prevChar | isIdent prevChar -> return isIdent
        Just prevChar | isOp    prevChar -> return isOp
        _                                -> return $ const False


replaceWordStart :: EditorView -> (Char -> Bool) -> String -> IDEM ()
replaceWordStart sourceView isWordChar name = do
    buffer <- getBuffer sourceView
    (selStart, selEnd) <- getSelectionBounds buffer
    start <- findWordStart selStart isWordChar
    wordStart <- getText buffer start selEnd True
    case stripPrefix wordStart name of
        Just extra -> do
            end <- findWordEnd selEnd isWordChar
            wordFinish <- getText buffer selEnd end True
            case (wordFinish, stripPrefix wordFinish extra) of
                (_:_,Just extra2) -> do
                    selectRange buffer end end
                    insert buffer end extra2
                _                 -> insert buffer selEnd extra
        Nothing    -> return ()


findWordStart :: EditorIter -> (Char -> Bool) -> IDEM EditorIter
findWordStart iter isWordChar = do
    maybeWS <- backwardFindCharC iter (not . isWordChar) Nothing
    case maybeWS of
        Nothing -> atOffset iter 0
        Just ws -> forwardCharC ws


findWordEnd :: EditorIter -> (Char -> Bool) -> IDEM EditorIter
findWordEnd iter isWordChar = do
    maybeWE <- forwardFindCharC iter (not . isWordChar) Nothing
    case maybeWE of
        Nothing -> forwardToLineEndC iter
        Just we -> return we


longestCommonPrefix (x:xs) (y:ys) | x == y = x : longestCommonPrefix xs ys
longestCommonPrefix _ _ = []
