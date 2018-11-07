Git Editor
======================================================

Author:   David Hoyle

Version:  0.9.2

Date:     07 Nov 2018

Web Page: [TBC](http://www.davidghoyle.co.uk/WordPress/)

## Overview

This is a simple syntax highlighting text editor for use with the command-line version of Git or for any
other text editing you want. I created it because I didn't like VIM and there were times that I needed to
edit source files outside of RAD Studio when building backward compatability into RAD Studio plug-ins
(the IDE adds unnecessary units which break backward compatability).

## Use

This is a single file at a time syntax highlighting editor which is designed to close on pressing Escape
where upon it will prompt you to save any changes. The following bullet points outline the capabilities:

 * New - Aave any changes to an existing files and create a new blank text file.
 * Open - Allows you to open a file from disk.
 * Save - Saves any changes made to the file.
 * Save As - Allows you to save the files to another filename.
 * Close - Closes the open file and create a new blank file.
 * Undo - Undoes the last change in the editor.
 * Redo - Redoes the last nudone edit in the editor.
 * Cut - Cuts the selected text to the clipboard.
 * Copy - Copies the selected text to the clipboard.
 * Paste - Pastes text from the clipboard to the edit at the current cursor position.
 * Delete - Deletes the selected text from the editor.
 * Select All - Selects all the text in the editor.
 * Find - Find text in the editor.
 * Replace - Allows you to fin and replace text in the editor.
 * Options - Displays a comprehansive options dialogue for the currently open editor and associated    highlighter that allows you to configure the properties of the Editor, Gutter, Keystroke behaviour and the attributes of the associated highlighter.

Additionally the editor supports VCL Themes and chaning the Higlighter that is used by right clicking on
the statusbar panel where the current VCL Theme or Highlighter displayed and selecting from the displayed
popup menu.

## Current Limitations

This project has to dependencies implemented via Git SubModules. The first is a version of
**SynEdit 2.1.0**
[https://github.com/DGH2112/SynEdit-2-1-0-with-Theming](https://github.com/DGH2112/SynEdit-2-1-0-with-Theming)
where I've implemented VCL Themes and the second is a set of custom SynEdit controls that I've used in
this and other project of mine named **DGHCustomSynEdit**
[https://github.com/DGH2112/DGHCustomSynEditControls](https://github.com/DGH2112/DGHCustomSynEditControls).

## Binaries

There are currently no binaries available has this project is still in-progress so you will have to build
the project to get a binary (I'm using 10.2.3 Tokyo).