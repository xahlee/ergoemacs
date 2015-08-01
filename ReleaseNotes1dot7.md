# ErgoEmacs version 1.7 Release Notes #

Major user visible changes:

## New Modes ##

• Added lua-mode.el for editing the lua language source code.

• Added a dos.el and batch-mode.el modes for editing Windows's command line batch script.

• Added expresso-mode for editing javascript.

• Added xbbcode-mode for editing bbcode (a markup used in many online forum).

• Added a new mode for AutoHotKey (xahk-mode). The xahk-mode replaces the previous ahk-mode.

• All modes for editing special files can be accessed from the menu “File‣Language Modes”.

• Most modes with unintuitive names now have a alias. For example, calling js-mode loads the expresso-mode designed for editing javascript. bbcode-mode calls xbbcode-mode, lsl-mode calls xlsl-mode, ahk-mode calls xahk-mode, etc.

## Menu Changes ##

• Added a menu “File‣Language Modes‣List Text Editing Modes”. This is intended to show all modes that are used for editing special files.

## Key shortcut changes ##

• Alt+n will cancel commands (GNU Emacs's Ctrl+g). Experimental.

• Ctrl+Shift+Home and Ctrl+Shift+End now select text.