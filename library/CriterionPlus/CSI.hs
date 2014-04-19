module CriterionPlus.CSI where

import CriterionPlus.Prelude.Basic
import CriterionPlus.Prelude.Data

esc = "\ESC["
csi = "\o033["
cursorUp n = csi <> (cs $ show n) <> "A"
cursorDown n = csi <> (cs $ show n) <> "B"
-- | Moves cursor to beginning of the line n (default 1) lines down.
cursorNextLine n = csi <> (cs $ show n) <> "E"
-- | Moves cursor to beginning of the line n (default 1) lines up.
cursorPreviousLine n = csi <> (cs $ show n) <> "F"
-- | Moves the cursor to column n.
cursorHorizontalAbsolute n = csi <> (cs $ show n) <> "G"
-- | Clears part of the screen. If n is zero (or missing), clear from cursor to end of screen. If n is one, clear from cursor to beginning of the screen. If n is two, clear entire screen (and moves cursor to upper left on MS-DOS ANSI.SYS).
eraseData n = csi <> (cs $ show n) <> "J"
-- | Erases part of the line. If n is zero (or missing), clear from cursor to the end of the line. If n is one, clear from cursor to beginning of the line. If n is two, clear entire line. Cursor position does not change.
eraseInLine n = csi <> (cs $ show n) <> "K"
eraseLineToEnd = eraseInLine 0
eraseLineToBeginning = eraseInLine 1
eraseLine = eraseInLine 2
saveCursorPosition = csi <> "s"
restoreCursorPosition = csi <> "u"
hideCursor = csi <> "?25l"
showCursor = csi <> "?25h"
