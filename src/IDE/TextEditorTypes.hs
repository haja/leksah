-----------------------------------------------------------------------------
--
-- Module      :  IDE.TextEditor
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.TextEditorTypes (
-- Data Types
    EditorBuffer(..)
,   EditorView(..)
,   EditorMark(..)
,   EditorIter(..)
,   EditorTagTable(..)
,   EditorTag(..)
) where

import qualified Graphics.UI.Gtk as Gtk hiding(afterToggleOverwrite)
import qualified Graphics.UI.Gtk.SourceView as Gtk
import Graphics.UI.Gtk.Multiline.TextTagTable
import qualified Graphics.UI.Gtk.Multiline.TextBuffer as Gtk
import qualified  Graphics.UI.Gtk.Multiline.TextView as Gtk
import Graphics.UI.Gtk.Multiline.TextTag

#ifdef LEKSAH_WITH_YI
import qualified Yi as Yi hiding(withBuffer)
import qualified Yi.UI.Pango.Control as Yi
import qualified Yi.Keymap.Cua as Yi
#endif

-- Data types
data EditorBuffer = GtkEditorBuffer Gtk.SourceBuffer
#ifdef LEKSAH_WITH_YI
    | YiEditorBuffer Yi.Buffer
#endif

data EditorView = GtkEditorView Gtk.SourceView
#ifdef LEKSAH_WITH_YI
    | YiEditorView Yi.View
#endif

data EditorMark = GtkEditorMark Gtk.TextMark
#ifdef LEKSAH_WITH_YI
    | YiEditorMark Yi.Mark
#endif

data EditorIter = GtkEditorIter Gtk.TextIter
#ifdef LEKSAH_WITH_YI
    | YiEditorIter Yi.Iter
#endif

data EditorTagTable = GtkEditorTagTable Gtk.TextTagTable
#ifdef LEKSAH_WITH_YI
    | YiEditorTagTable
#endif

data EditorTag = GtkEditorTag Gtk.TextTag
#ifdef LEKSAH_WITH_YI
    | YiEditorTag
#endif
