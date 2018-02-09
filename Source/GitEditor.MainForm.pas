(**
  
  This module contaisn a class which represent the main form for the application - a single window
  text editor.

  @Author  David Hoyle
  @Version 1.0
  @Date    09 Feb 2018
  
**)
Unit GitEditor.MainForm;

Interface

Uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.ActnMan,
  Vcl.ActnCtrls,
  SynHighlighterPas,
  SynHighlighterCpp,
  SynHighlighterHtml,
  SynHighlighterBat,
  SynHighlighterDfm,
  SynHighlighterIni,
  SynEditOptionsDialog,
  System.ImageList,
  Vcl.ImgList,
  Vcl.StdActns,
  System.Actions,
  Vcl.ActnList,
  Vcl.Menus,
  Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnPopup,
  SynEdit,
  SynHighlighterMD,
  SynEditHighlighter,
  SynHighlighterBNF,
  Vcl.AppEvnts,
  SynEditMiscClasses,
  SynEditRegexSearch, 
  SynEditSearchReplaceForm,
  SynEditSearch;

Type
  (** A class to represent the main form of the aplpication - a single window editor. **)
  TfrmGEMainForm = Class(TForm)
    sehBNF: TSynBNFSyn;
    sehMD: TSynMDSyn;
    pabrContextMenu: TPopupActionBar;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N2: TMenuItem;
    SelectAll1: TMenuItem;
    amActions: TActionManager;
    actToolsOptions: TAction;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileExit: TAction;
    actEditUndo: TEditUndo;
    actEditRedo: TAction;
    actEditCut: TEditCut;
    actEditCopy: TEditCopy;
    actEditPaste: TEditPaste;
    actEditDelete: TEditDelete;
    actEditSelectAll: TEditSelectAll;
    actEditFind: TAction;
    actEditReplace: TAction;
    ilImages: TImageList;
    seOptionsDialog: TSynEditOptionsDialog;
    sehIni: TSynIniSyn;
    sehDfm: TSynDfmSyn;
    sehBat: TSynBatSyn;
    sehHTML: TSynHTMLSyn;
    sehCPP: TSynCppSyn;
    sehPascal: TSynPasSyn;
    atbrToolBar: TActionToolBar;
    sbrStatusbar: TStatusBar;
    aeEvents: TApplicationEvents;
    dlgTask: TTaskDialog;
    dlgSave: TFileSaveDialog;
    dlgOpen: TFileOpenDialog;
    seRegexSearch: TSynEditRegexSearch;
    seSearch: TSynEditSearch;
    actEditFindNext: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actFileExitExecute(Sender: TObject);
    procedure actToolsOptionsExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveUpdate(Sender: TObject);
    procedure actEditRedoExecute(Sender: TObject);
    procedure actEditRedoUpdate(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actEditFindExecute(Sender: TObject);
    procedure actEditReplaceExecute(Sender: TObject);
    procedure aeEventsHint(Sender: TObject);
    procedure actEditFindNextExecute(Sender: TObject);
  Strict Private
    FIniFile       : String;
    FEditor        : TSynEdit;
    FFileName      : String;
    FSearch        : String;
    FReplace       : String;
    FSearchOptions : TSearchOptions;
  Strict Protected
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure CreateEditor;
    Procedure EditorStatusChange(Sender : TObject; Changes : TSynStatusChanges);
    Procedure OpenFile(Const strFileName : String);
    Procedure UpdateCaption;
    Procedure HookHighlighter;
    Function  SaveFile(Const strFileName : String) : Boolean;
    Function  SaveFileAs(Const strFileName : String) : Boolean;
    Procedure SearchMessage(Const strMsg : String);
    Procedure EditorReplaceText(Sender: TObject; Const ASearch, AReplace: String; Line, Column: Integer;
      Var Action: TSynReplaceAction);
  Public
  End;

Var
  (** A Delphi managed form variable. **)
  frmGEMainForm: TfrmGEMainForm;

Implementation

{$R *.dfm}

Uses
  {$IFDEF CODESITE}
  CodeSiteLogging,
  {$ENDIF}
  System.StrUtils,
  System.IniFiles, 
  System.UITypes,
  System.TypInfo,
  WinAPI.SHFolder,
  GitEditor.CommonFunctions,
  SynEditKeyCmds, 
  SynEditOptionsForm, 
  SynHighlighterUtils, 
  SynEditTypes;

Type
  (** An enumerate to define the statusbar columns. @nohints **)
  TGEStatusColumn = (
    scCaret,
    scInsert,
    scModified,
    scLines,
    scSize,
    scFileType
  );
  (** A record helper to convert the above enumerate to column indexes. @nohints **)
  TGEStatusColumnHelper = Record Helper For TGEStatusColumn
    Function ColumnIndex : Integer;
  End;
  (** A helper class for task dialogues to make it easier to intialise the dialogue and added button
      @nohints **)
  TGETaskDialogHelper = Class Helper For TTaskDialog
    Procedure AddButton(Const strCaption : String; Const iModalResult : TModalResult;
      Const boolDefault : Boolean = False);
  End;
  
ResourceString
  (** A string constant for the Window Position INI Section **)
  strWindowPosition = 'Window Position';
  (** A string constant for the INI file name pattern. **)
  strINIPattern = '%s Settings for %s on %s.INI';
  (** A string constant for the profile sub-directory for storing the INI file. **)
  strSeasonsFall = '\Season''s Fall\';
  (** Default filename. **)
  strUntitled = 'Untitled.txt';
  (** A filter description and file extensions for text files. **)
  strDefaultFilter = 'Text Files (*.txt)|*.txt';
  (** A filter description for all files. **)
  strAllFiles = 'All files (*.*)';

Const
  (** An ini key for the left of the main window position. **)
  strLeft = 'Left';
  (** An ini key for the top of the main window position. **)
  strTop = 'Top';
  (** An ini key for the height of the main window size. **)
  strHeight = 'Height';
  (** An ini key for the width of the main window size. **)
  strWidth = 'Width';
  (** A constant for the default Open / Save dialogue file extension. **)
  strDefaultExt = '*.txt';
  (** An ini section for the search options. **)
  strSearchOptionsIniSection = 'Search Options';

(**

  This method converts the enumerate to an integer.

  @precon  None.
  @postcon An integer is returned.

  @return  an Integer

**)
Function TGEStatusColumnHelper.ColumnIndex: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TGEStatusColumnHelper.ColumnIndex', tmoTiming);{$ENDIF}
  Result := Integer(Self);
End;

(**

  This method adds a button to the Task Dialog.

  @precon  None.
  @postcon A button is added to the task dialog with the given caption and modal result.

  @param   strCaption   as a String as a constant
  @param   iModalResult as a TModalResult as a constant
  @param   boolDefault  as a Boolean as a constant

**)
Procedure TGETaskDialogHelper.AddButton(Const strCaption: String; Const iModalResult: TModalResult;
  Const boolDefault: Boolean);

Var
  B : TTaskDialogBaseButtonItem;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AddButton', tmoTiming);{$ENDIF}
  B := Buttons.Add;
  B.Caption := strCaption;
  B.ModalResult := iModalResult;
  B.Default := boolDefault;
End;

(**

  This is an on execute event handler for the Edit Find action.

  @precon  None.
  @postcon Displays a Find dialogue for a regular expression search.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actEditFindExecute(Sender: TObject);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'actEditFindExecute', tmoTiming);{$ENDIF}
  SearchFind(Self, FEditor, SearchMessage, FSearch, FReplace, FSearchOptions, seRegexSearch, seSearch,
    FINIFile);
End;

(**

  This is an on execute event handler for the Edit Find Next action.

  @precon  None.
  @postcon Finds the next occurrance of the last search text.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actEditFindNextExecute(Sender: TObject);

Var
  SO : TSearchOptions;
  
Begin
  SO := FSearchOptions;
  Try
    Exclude(FSearchOptions, soBackward);
    If FSearch <> '' Then
      SearchFindNext(FEditor, SearchMessage, FSearch, FReplace, FSearchOptions, seRegexSearch, seSearch)
    Else
      SearchFind(Self, FEditor, SearchMessage, FSearch, FReplace, FSearchOptions, seRegexSearch,
        seSearch, FINIFile);
  Finally
    FSearchOptions := SO;
  End;
End;

(**

  This method is an on execute event handler for the Edit Redo action.

  @precon  None.
  @postcon Redoes the last undo in the editor.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actEditRedoExecute(Sender: TObject);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'actEditRedoExecute', tmoTiming);{$ENDIF}
  FEditor.Redo();
End;

(**

  This is an on update event handler for the Edit Redo action.

  @precon  None.
  @postcon Updates the enabled property of the action based on the state of the editor.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actEditRedoUpdate(Sender: TObject);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'actEditRedoUpdate', tmoTiming);{$ENDIF}
  If Sender Is TAction Then
    (Sender As TAction).Enabled := FEditor.CanRedo;
End;

(**

  This is an on execute event handler for the Edit Replace action.

  @precon  None.
  @postcon Displays a Replace dialogue for a regular expression search and replace.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actEditReplaceExecute(Sender: TObject);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'actEditReplaceExecute', tmoTiming);{$ENDIF}
  SearchReplace(Self, FEditor, SearchMessage, FSearch, FReplace, FSearchOptions, seRegexSearch, seSearch,
    FINIFile);
End;

(**

  This is an on execute event handler for the File Exit action.

  @precon  None.
  @postcon Closes the form / application.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actFileExitExecute(Sender: TObject);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'actFileExitExecute', tmoTiming);{$ENDIF}
  Close;
End;

(**

  This is an on execute event handler for the File New event handler.

  @precon  None.
  @postcon Creates a blank new file.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actFileNewExecute(Sender: TObject);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'actFileNewExecute', tmoTiming);{$ENDIF}
  SaveFile(FFileName);
  FFileName := ExpandFileName(strUntitled);
  FEditor.Clear;
  UpdateCaption;
  HookHighlighter;
  LoadFromINIFile(FIniFile, FEditor);
End;

(**

  This is an on execute event handler for the File Open event handler.

  @precon  None.
  @postcon Prompts the user to open a new file after saving the current file if changed.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actFileOpenExecute(Sender: TObject);

ResourceString
  strOpenFileTitle = 'Open Text File';
  strOpenBtnLbl = 'Open';
  
Var
  iComponent: Integer;
  H : TSynCustomHighlighter;
  FTI : TFileTypeItem;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'actFileOpenExecute', tmoTiming);{$ENDIF}
  If SaveFile(FFileName) Then
    Begin
      dlgOpen.DefaultExtension := strDefaultExt;
      dlgOpen.FileTypes.Clear;
      For iComponent := 0 To ComponentCount - 1 Do
        If Components[iComponent] Is TSynCustomHighlighter Then
          Begin
            H := Components[iComponent] As TSynCustomHighlighter;
            FTI := dlgOpen.FileTypes.Add;
            FTI.DisplayName := GetShortHint(H.DefaultFilter);
            FTI.FileMask := GetLongHint(H.DefaultFilter);
          End;
      FTI := dlgOpen.FileTypes.Add;
      FTI.DisplayName := GetShortHint(strDefaultFilter);
      FTI.FileMask := GetLongHint(strDefaultFilter);
      FTI := dlgOpen.FileTypes.Add;
      FTI.DisplayName := strAllFiles;
      FTI.FileMask := '*.*';
      dlgOpen.FileTypeIndex := dlgOpen.FileTypes.Count -  1;
      dlgOpen.DefaultFolder := GetCurrentDir;
      dlgOpen.Title := strOpenFileTitle;
      dlgOpen.FileName := '';
      dlgOpen.OkButtonLabel := strOpenBtnLbl;
      If dlgOpen.Execute(Handle) Then
        OpenFile(dlgOpen.FileName);
    End;
End;

(**

  This is an on execute event handler for the File SaveAs event handler.

  @precon  None.
  @postcon Prompts the user to save the file to another fiename.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actFileSaveAsExecute(Sender: TObject);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'actFileSaveAsExecute', tmoTiming);{$ENDIF}
  SaveFileAs(FFileName);
End;

(**

  This method is an on execute event handler for the File Save action.

  @precon  None.
  @postcon Saves the file to disk.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actFileSaveExecute(Sender: TObject);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'actFileSaveExecute', tmoTiming);{$ENDIF}
  SaveFile(FFileName);
End;

(**

  This is an on update event handler for the File Save action.

  @precon  None.
  @postcon Updates the enabled property of the action base on whether the file is modified.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actFileSaveUpdate(Sender: TObject);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'actFileSaveUpdate', tmoTiming);{$ENDIF}
  If Sender Is Taction Then
    (Sender As TAction).Enabled := FEditor.Modified;
End;

(**

  This is an on execute event handelr for the Tools Options action.

  @precon  None.
  @postcon Displays the editor options dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actToolsOptionsExecute(Sender: TObject);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'actToolsOptionsExecute', tmoTiming);{$ENDIF}
  TfrmEditorOptions.Execute(Self, FEditor, True);
End;

(**

  This is an application on hint event handler.

  @precon  None.
  @postcon Displays the action hint in the status bar.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.aeEventsHint(Sender: TObject);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'aeEventsHint', tmoTiming);{$ENDIF}
  sbrStatusBar.SimplePanel := Application.Hint <> '';
  sbrStatusBar.SimpleText  := Application.Hint;
End;

(**

  This method creates the synedit editor in the form - this done as visually adding this to the form
  seems to make the IDE unstable and the form does not show correctly.

  @precon  None.
  @postcon The editor is created, inserted into the form and configured.

**)
Procedure TfrmGEMainForm.CreateEditor;

Const
  iDefaultFontheight = -11;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CreateEditor', tmoTiming);{$ENDIF}
  FEditor := TSynEdit.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := alClient;
  FEditor.Font.Charset := DEFAULT_CHARSET;
  FEditor.Font.Height := iDefaultFontheight;
  FEditor.TabOrder := 0;
  FEditor.Gutter.Font.Assign(FEditor.Font);
  FEditor.AddKey(ecDeleteChar, VK_DELETE, [ssCtrl], 0, []);
  FEditor.AddKey(ecWordLeft, VK_LEFT, [ssCtrl], 0, []);
  FEditor.AddKey(ecWordRight, VK_RIGHT, [ssCtrl], 0, []);
  FEditor.AddKey(ecSelPageLeft, VK_LEFT, [ssAlt, ssShift], 0, []);
  FEditor.AddKey(ecSelPageRight, VK_RIGHT, [ssAlt, ssShift], 0, []);
  FEditor.AddKey(ecScrollLeft, VK_LEFT, [ssAlt], 0, []);
  FEditor.AddKey(ecScrollRight, VK_RIGHT, [ssAlt], 0, []);
  FEditor.AddKey(ecCommentBlock, Ord('C'), [ssAlt, ssCtrl], 0, []);
  FEditor.AddKey(ecAutoCompletion, Ord('J'), [ssCtrl], 0, []);
  FEditor.SearchEngine := seRegexSearch;
  FEditor.PopupMenu := pabrContextMenu;
  FEditor.OnStatusChange := EditorStatusChange;
  FEditor.OnReplaceText := EditorReplaceText;
  EditorStatusChange(Self, [scAll]);
End;

(**

  This method haves the prompting for the replacement of text.

  @precon  None.
  @postcon A confirmtion dialogue is displayed for replacements.

  @param   Sender   as a TObject
  @param   ASearch  as a String as a constant
  @param   AReplace as a String as a constant
  @param   Line     as an Integer
  @param   Column   as an Integer
  @param   Action   as a TSynReplaceAction as a reference

**)
Procedure TfrmGEMainForm.EditorReplaceText(Sender: TObject; Const ASearch, AReplace: String; Line,
  Column: Integer; Var Action: TSynReplaceAction);

Begin
  SearchReplaceText(FEditor, ASearch, AReplace, Line, Column, Action);
End;

(**

  This method is an on status change event handler for the editor control.

  @precon  None.
  @postcon Updates the cursor position, insert mode and modified statusbar panels.

  @param   Sender  as a TObject
  @param   Changes as a TSynStatusChanges

**)
Procedure TfrmGEMainForm.EditorStatusChange(Sender : TObject; Changes : TSynStatusChanges);

ResourceString
  strInsert = 'Insert';
  strOverwrite = 'Overwrite';
  strModified = 'Modified';
  strReadOnly = 'Read Only';
  strLines = '%1.0n Lines';
  strKBytes = '%1.1n KBytes';

Const
  dblKBytes = 1024.0;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'EditorStatusChange', tmoTiming);{$ENDIF}
  sbrStatusbar.Panels[scCaret.ColumnIndex].Text := Format('%1.0n:%1.0n', [Int(FEditor.CaretY),
    Int(FEditor.CaretX)]);
  sbrStatusbar.Panels[scInsert.ColumnIndex].Text := IfThen(FEditor.InsertMode, strInsert, strOverwrite);
  sbrStatusbar.Panels[scModified.ColumnIndex].Text := IfThen(FEditor.Modified, strModified,
    IfThen(FEditor.ReadOnly, strReadOnly));
  sbrStatusbar.Panels[scLines.ColumnIndex].Text := Format(strLines, [Int(FEditor.Lines.Count)]);
  sbrStatusbar.Panels[scSize.ColumnIndex].Text := Format(strKBytes, [
    Int(Length(FEditor.Lines.Text)) / dblKBytes]);
End;

(**

  This is an on close query event hanler for the form.

  @precon  None.
  @postcon If the editor has changed, a message diaogue is shown asking whether the file should be saved
           . If yes is pressed the file is saved.

  @parma   CanClose as a bool as a reference

  @param   Sender   as a TObject
  @param   CanClose as a Boolean as a reference

**)
Procedure TfrmGEMainForm.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);

ResourceString
  strModifiedFile = 'Modified File';
  strFileHasBeenModified = 'The file "%s" has been modified!';
  strSaveChangesToFile = 'Save the changes to the file "%s"';
  strDiscardChangesToFile = 'Discard the changes to the file "%s"';
  strCancelAndDonCloseEditor = 'Cancel and don''t close the editor';

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'FormCloseQuery', tmoTiming);{$ENDIF}
  If FEditor.Modified Then
    Begin
      dlgTask.Title := strModifiedFile;
      dlgTask.Caption := Application.Title;
      dlgTask.Flags := [tfUseHiconMain, tfAllowDialogCancellation, tfUseCommandLinks];
      dlgTask.MainIcon := tdiWarning;
      dlgTask.Text := Format(strFileHasBeenModified, [FFileName]);
      dlgTask.CommonButtons := [];
      dlgTask.Buttons.Clear;
      dlgTask.AddButton(Format(strSaveChangesToFile, [FFileName]), mrYes, True);
      dlgTask.AddButton(Format(strDiscardChangesToFile, [FFileName]), mrNo);
      dlgTask.AddButton(strCancelAndDonCloseEditor, mrCancel);
      If dlgTask.Execute(Handle) Then
        Case dlgTask.ModalResult Of
          mrYes:    CanClose := SaveFile(FFileName);
          mrNo:     CanClose := true;
          mrCancel: CanClose := false;
        End Else
          CanClose := False;
    End;
End;

(**

  This is an on form create event handler for the form.

  @precon  None.
  @postcon Builds the INI Filename and then loads the applications settings.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.FormCreate(Sender: TObject);

Var
  strBuffer : String;
  iSize : Integer;
  strFileName : String;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'FormCreate', tmoTiming);{$ENDIF}
  SetLength(strBuffer, MAX_PATH);
  iSize := GetModuleFileName(HInstance, PChar(strBuffer), MAX_PATH);
  SetLength(strBuffer, iSize);
  FIniFile := ChangeFileExt(strBuffer, '');
  FIniFile := Format(strINIPattern, [FIniFile, UserName, ComputerName]);
  SetLength(strBuffer, MAX_PATH);
  SHGetFolderPath(0, CSIDL_APPDATA Or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT, PChar(strBuffer));
  strBuffer := StrPas(PChar(strBuffer));
  strBuffer := strBuffer + strSeasonsFall;
  If Not DirectoryExists(strBuffer) Then
    ForceDirectories(strBuffer);
  FIniFile := strBuffer + ExtractFileName(FIniFile);
  CreateEditor;
  LoadSettings;
  If ParamCount = 0 Then
    strFileName := ExpandFileName(strUntitled)
  Else
    strFileName := ExpandFileName(ParamStr(1));
  OpenFile(strFileName);
End;

(**

  This is an on form destroy event handler for the form class.

  @precon  None.
  @postcon Saves the applications settings.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.FormDestroy(Sender: TObject);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'FormDestroy', tmoTiming);{$ENDIF}
  SaveSettings;
  SaveToINIFile(FIniFile, FEditor);
End;

(**

  This method hooks the specific highlight associated with the file extension.

  @precon  None.
  @postcon The highlight is set if an appropriate one exists for the file.

**)
Procedure TfrmGEMainForm.HookHighlighter;

  (**

    This method iterates through the highlighters trying to match the given extension to one of their
    known extensions. If found the editors highlighter is set

    @precon  None.
    @postcon If a highlighter implements the file extension the editor highlighter is set.

    @param   strExt as a String as a constant

  **)
  Procedure IterateHighlighters(Const strExt : String);

  Var
    iComponent : Integer;
    strExts : String;
    H : TSynCustomHighlighter;
    strHExt : TArray<String>;
    i : Integer;
  
  Begin
    {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'HookHighlighter/IterateHighlighters', tmoTiming);{$ENDIF}
    For iComponent := 0 To ComponentCount - 1 Do
      Begin
        If Components[iComponent] Is TSynCustomHighlighter Then
          Begin
            H := Components[iComponent] As TSynCustomHighlighter;
            strExts := GetLongHint(H.DefaultFilter);
            strHExt := strExts.Split([';', ',', '|']);
            For i := Low(strHExt) To High(strHExt) Do
              If LowerCase(strHExt[i]) = strExt Then
                Begin
                  FEditor.Highlighter := H;
                  sbrStatusbar.Panels[scFileType.ColumnIndex].Text := HighlighterName(H);
                  Break;
                End;
          End;
      End;
  End;

ResourceString
  strPlainTextFiles = 'Plain Text Files';

Var
  strExt : String;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'HookHighlighter', tmoTiming);{$ENDIF}
  FEditor.Highlighter := Nil;
  sbrStatusbar.Panels[scFileType.ColumnIndex].Text := strPlainTextFiles;
  strExt := LowerCase(ExtractFileExt(FFileName));
  If Length(strExt) = 0 Then
    Begin
      FEditor.Highlighter := sehMD;
      sbrStatusbar.Panels[scFileType.ColumnIndex].Text := HighlighterName(FEditor.Highlighter);
    End Else
      Iteratehighlighters('*' + strExt);
End;

(**

  This method loads the applications settings from the INI file.

  @precon  None.
  @postcon The application settings are loaded from the INI file.

**)
Procedure TfrmGEMainForm.LoadSettings;

Const
  DefaultOptions : TSearchOptions = [soPrompt, soEntireScope];

Var
  iniFile : TCustomIniFile;
  iOp: TSearchOption;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'LoadSettings', tmoTiming);{$ENDIF}
  iniFile := TMemIniFile.Create(FIniFile);
  Try
    Left := iniFile.ReadInteger(strWindowPosition, strLeft, Left);
    Top := iniFile.ReadInteger(strWindowPosition, strTop, Top);
    Height := iniFile.ReadInteger(strWindowPosition, strHeight, Height);
    Width := iniFile.ReadInteger(strWindowPosition, strWidth, Width);
    FSearchOptions := [];
    For iOp := Low(TSearchOption) To High(TSearchOption) Do
      If iniFile.ReadBool(strSearchOptionsIniSection, GetEnumName(TypeInfo(TSearchOption), Ord(iOp)),
        iOp In DefaultOptions) Then
        Include(FSearchOptions, iOp);
  Finally
    iniFile.Free;
  End;
End;

(**

  If a filename is passed on the command line, the file is opened in the editor.

  @precon  None.
  @postcon The file from the first parameter of the command line is open if provied else a default
           untitled file is assumed.

  @param   strFileName as a String as a constant

**)
Procedure TfrmGEMainForm.OpenFile(Const strFileName : String);

ResourceString
  strCreateTextFile = 'Create a Text File';
  strFileDoesNotExist = 'The file "%s" does not exist!';
  strCreateFile = 'Create the file "%s"?';
  strDoNOTCreateFile = 'Do NOT create the file "%s"!';
  strFileNotFound = 'The file "%s" was not found (the directory does not exist)!';

Var
  sl : TStringList;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'OpenFile', tmoTiming);{$ENDIF}
  FFileName := strFileName;
  If FileExists(FFileName) Then
    Begin
      FEditor.Lines.LoadFromFile(FFileName);
      FEditor.ReadOnly := (GetFileAttributes(PChar(FFileName)) And FILE_ATTRIBUTE_READONLY <> 0);
    End Else
    Begin
      If DirectoryExists(ExtractFilePath(FFileName)) Then
        Begin
          dlgTask.Title := strCreateTextFile;
          dlgTask.Caption := Application.Title;
          dlgTask.Flags := [tfUseHiconMain, tfAllowDialogCancellation, tfUseCommandLinks];
          dlgTask.MainIcon := tdiInformation;
          dlgTask.Text := Format(strFileDoesNotExist, [FFileName]);
          dlgTask.CommonButtons := [];
          dlgTask.Buttons.Clear;
          dlgTask.AddButton(Format(strCreateFile, [FFileName]), mrYes, True);
          dlgTask.AddButton(Format(strDoNOTCreateFile, [FFileName]), mrNo);
          If dlgTask.Execute(handle) Then
            Case dlgTask.ModalResult Of
              mrYes:
                Begin
                  sl := TStringList.Create;
                  Try
                    sl.SaveToFile(FFileName);
                  Finally
                    sl.Free;
                  End;
                End;
              mrNo: FFileName := ExpandFileName(strUntitled)
            End;
        End Else
        Begin
          TaskMessageDlg(Application.Title, Format(strFileNotFound, [FFileName]), mtError, [mbOK], 0);
          FFileName := ExpandFileName(strUntitled)
        End;
    End;
  EditorStatusChange(Nil, [scAll]);
  UpdateCaption;
  HookHighlighter;
  LoadFromINIFile(FIniFile, FEditor);
  
End;

(**

  This method saves the file in the editor.

  @precon  None.
  @postcon The file is saved if the underlying filename exists else a SaveAs dialogue is shown. If the 
           file is saved this function returns true.

  @param   strFileName as a String as a constant
  @return  a Boolean

**)
Function TfrmGEMainForm.SaveFile(Const strFileName : String) : Boolean;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SaveFile', tmoTiming);{$ENDIF}
  If FEditor.Modified Then
    Begin
      If FileExists(FFileName) Then
        Begin
          FEditor.Lines.SaveToFile(strFileName);
          SaveToINIFile(FIniFile, FEditor);
          FEditor.Modified := False;
          FEditor.MarkModifiedLinesAsSaved();
          Result := True;
        End Else
          Result := SaveFileAs(strFileName);
    End Else
      Result := True;
End;

(**

  This method prompts the user tp saves the file to a new filename.

  @precon  None.
  @postcon The methods returns true if the file was saved to a new filename.

  @param   strFileName as a String as a constant
  @return  a Boolean

**)
Function TfrmGEMainForm.SaveFileAs(Const strFileName: String): Boolean;

Const
  strSaveFileAsTitle = 'Save File As';
  strSaveBtnLbl = 'Save';

Var
  FTI : TFileTypeItem;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SaveFileAs', tmoTiming);{$ENDIF}
  dlgSave.DefaultExtension := ExtractFileExt(strFileName);
  If dlgSave.DefaultExtension = '' Then
    dlgSave.DefaultExtension := strDefaultExt;
  dlgSave.FileTypes.Clear;
  If Assigned(FEditor.Highlighter) Then
    Begin
      FTI := dlgSave.FileTypes.Add;
      FTI.DisplayName := GetShortHint(FEditor.Highlighter.DefaultFilter);
      FTI.FileMask := GetLongHint(FEditor.Highlighter.DefaultFilter);
    End Else
    Begin
      FTI := dlgSave.FileTypes.Add;
      FTI.DisplayName := GetShortHint(strDefaultFilter);
      FTI.FileMask := GetLongHint(strDefaultFilter);
    End;
  FTI := dlgSave.FileTypes.Add;
  FTI.DisplayName := strAllFiles;
  FTI.FileMask := '*.*';
  If DirectoryExists(ExtractFilePath(strFileName)) Then
    dlgSave.DefaultFolder := ExtractFilePath(strFileName);
  dlgSave.Title := strSaveFileAsTitle;
  dlgSave.FileName := ExtractFileName(strFileName);
  dlgSave.OkButtonLabel := strSaveBtnLbl;
  dlgSave.FileTypeIndex := 1;
  Result := dlgSave.Execute(Handle);
  If Result Then
    FFileName := dlgSave.FileName;
End;

(**

  This method saves the applications settings to the INI file.

  @precon  None.
  @postcon The application settings are saved to the INI file.

**)
Procedure TfrmGEMainForm.SaveSettings;

Var
  iniFile : TCustomIniFile;
  iOp : TSearchOption;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SaveSettings', tmoTiming);{$ENDIF}
  iniFile := TMemIniFile.Create(FIniFile);
  Try
    iniFile.WriteInteger(strWindowPosition, strLeft, Left);
    iniFile.WriteInteger(strWindowPosition, strTop, Top);
    iniFile.WriteInteger(strWindowPosition, strHeight, Height);
    iniFile.WriteInteger(strWindowPosition, strWidth, Width);
    For iOp := Low(TSearchOption) To High(TSearchOption) Do
      iniFile.WriteBool(strSearchOptionsIniSection, GetEnumName(TypeInfo(TSearchOption), Ord(iOp)),
        iOp In FSearchOptions);
    iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
End;

(**

  This method displays a message.

  @precon  None.
  @postcon The message is displayed

  @param   strMsg as a String as a constant

**)
Procedure TfrmGEMainForm.SearchMessage(Const strMsg: String);

Begin
  TaskMessageDlg(Application.Title, strMsg, mtInformation, [mbOK], 0);
End;

(**

  This method updates the caption of the form.

  @precon  None.
  @postcon The caption of the form is updated with the full filename.

**)
Procedure TfrmGEMainForm.UpdateCaption;

Const
  strBugFix = ' abcedfghijklmnopqrstuvwxyz';

ResourceString
  strGitEditorBuild = 'Git Editor %d.%d%s (Build %d.%d.%d.%d): ';

Var
  BuildInfo : TGEBuildInfo;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'UpdateCaption', tmoTiming);{$ENDIF}
  GetBuildInfo(BuildInfo);
  Caption := Format(strGitEditorBuild, [
      BuildInfo.FMajor,
      BuildInfo.FMinor,
      strBugFix[BuildInfo.FRelease + 1],
      BuildInfo.FMajor,
      BuildInfo.FMinor,
      BuildInfo.FRelease,
      BuildInfo.FBuild
    ]) + ExpandFileName(FFileName);
End;

End.
