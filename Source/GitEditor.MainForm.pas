(**
  
  This module contains a class which represent the main form for the application - a single window
  text editor.

  @Author  David Hoyle
  @Version 1.0
  @Date    07 Nov 2018

**)
Unit GitEditor.MainForm;

Interface

Uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.ImageList,
  System.Actions,
  System.IniFiles,
  Generics.Defaults,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.ActnMan,
  Vcl.ActnCtrls,
  Vcl.ImgList,
  Vcl.StdActns,
  Vcl.ActnList,
  Vcl.Menus,
  Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnPopup,
  Vcl.AppEvnts,
  Vcl.ExtCtrls,
  Winapi.Windows,
  Winapi.Messages,
  SynEdit,
  SynHighlighterPas,
  SynHighlighterCpp,
  SynHighlighterHtml,
  SynHighlighterBat,
  SynHighlighterDfm,
  SynHighlighterIni,
  SynEditOptionsDialog,
  SynHighlighterMD,
  SynEditHighlighter,
  SynHighlighterBNF,
  SynEditMiscClasses,
  SynEditRegexSearch, 
  SynEditSearchReplaceForm,
  SynEditSearch,
  SynHighlighterXML,
  SynHighlighterVB,
  SynHighlighterSml,
  SynHighlighterSQL,
  SynHighlighterRC,
  SynHighlighterPython,
  SynHighlighterPHP,
  SynHighlighterPerl,
  SynHighlighterDWS,
  SynHighlighterVBScript,
  SynHighlighterJScript,
  SynHighlighterJava,
  SynHighlighterInno,
  SynHighlighterCSS,
  SynHighlighterGeneral;

Type
  (** An enumerate to define the statusbar columns. @nohints **)
  TGEStatusColumn = (
    scCaret,
    scInsert,
    scModified,
    scLines,
    scSize,
    scFileType,
    scVCLTheme,
    scMemoryUseage
  );

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
    tmMemory: TTimer;
    sehCss: TSynCssSyn;
    sehInno: TSynInnoSyn;
    sehJava: TSynJavaSyn;
    sehJScript: TSynJScriptSyn;
    sehVBScript: TSynVBScriptSyn;
    sehPerl: TSynPerlSyn;
    sehPython: TSynPythonSyn;
    sehRC: TSynRCSyn;
    sehSQL: TSynSQLSyn;
    sehSML: TSynSMLSyn;
    sehVB: TSynVBSyn;
    sehXML: TSynXMLSyn;
    Editor: TSynEdit;
    pmStatusbar: TPopupMenu;
    SynGeneralSyn: TSynGeneralSyn;
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
    procedure tmMemoryTimer(Sender: TObject);
    procedure sbrStatusbarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure EditorReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: Integer;
      var Action: TSynReplaceAction);
    procedure sbrStatusbarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
  Strict Private
    Type
      (** An enumerate to define the blocks of memory. @nohints **)
      TMemorySize = (dmtLarge, dmtMedium, dmtSmall);
      (** A record to describe a name index pairing for use in sorting the Highighters and VCL
          Themings @nohints **)
      TGENameIndexRec = Record
        FName  : String;
        FIndex : Integer;
        Constructor Create(Const strName : String; Const iIndex : Integer);
      End;
    (** An IComparer class to allow for custom sorting of the TList<T> collection. **)
    TGENameIndexComparer = Class(TComparer<TGENameIndexRec>)
    Strict Private
    Strict Protected
    Public
      Function Compare(Const Left, Right : TGENameIndexRec) : Integer; Override;
    End;
  Strict Private
    FINIFile       : TMemIniFile;
    FFileName      : String;
    FSearch        : String;
    FReplace       : String;
    FSearchOptions : TSearchOptions;
    FUsed          : NativeUInt;
    FReserved      : NativeUInt;
    FMemoryBlock   : Array[Low(TMemorySize)..High(TMemorySize)] Of Cardinal;
    FPercentage    : Double;
  Strict Protected
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure PatchEditor;
    Procedure OpenFile(Const strFileName : String);
    Procedure UpdateCaption;
    Procedure HookHighlighter;
    Function  SaveFile(Const strFileName : String) : Boolean;
    Function  SaveFileAs(Const strFileName : String) : Boolean;
    Procedure SearchMessage(Const strMsg : String);
    Procedure UpdateAppTitle;
    Function  SaveFileToDisk(Const strFileName : String) : Boolean;
    Function  MonitorProfile : String;
    Procedure UpdateStatusBar(Const eStatusColumn : TGEStatusColumn; Const strText : String);
    procedure ShowVCLThemePopup(Const Pt : TPoint);
    Procedure VCLThemeClick(Sender : TObject);
    Procedure ShowHighlighterPopup(Const Pt : TPoint);
    Procedure HighlighterClick(Sender : TObject);
  Public
  End;

Var
  (** A Delphi managed form variable. **)
  frmGEMainForm: TfrmGEMainForm;

Implementation

{$R *.dfm}

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  System.StrUtils,
  System.UITypes,
  System.TypInfo,
  System.Types,
  System.Generics.Collections,
  VCL.Themes,
  VCL.StdCtrls,
  WinAPI.SHFolder,
  SynEditKeyCmds, 
  SynEditOptionsForm, 
  SynHighlighterUtils, 
  SynEditTypes,
  GitEditor.CommonFunctions;

Type
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
  strWindowPosition = 'Window Position:%s';
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
  (** A constant for the INI Section name for the VCL Theme Key **)
  strSetupINISection = 'Setup';
  (** A constant name for the VCL Theme key in the INI file. **)
  strVCLThemeKey = 'VCL Theme';

(**

  This function calculates a shorter representation of a memory size and returns a string representing 
  the value.

  @precon  None.
  @postcon Returns a string representation of the given size.

  @param   iSize as a Cardinal as a constant
  @return  a String

**)
Function CalcSize(Const iSize: Cardinal): String;

ResourceString
  strUnknown = 'Unknown';

Const
  dblKILOBYTE: Double = 1024.0;
  dblMEGABYTE: Double = 1024.0 * 1024.0;
  strKiloBytes = '%1.2nK';
  strMegaBytes = '%1.2nM';

Begin
  Result := strUnknown;
  If iSize < dblKILOBYTE Then
    Result := Format('%1.0n', [Int(iSize)])
  Else If iSize < dblMEGABYTE Then
    Result := Format(strKiloBytes, [Int(iSize) / dblKILOBYTE])
  Else
    Result := Format(strMegaBytes, [Int(iSize) / dblMEGABYTE])
End;

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

  A constructor for the TGENameIndexRec class.

  @precon  None.
  @postcon Initialises the record.

  @param   strName as a String as a constant
  @param   iIndex  as an Integer as a constant

**)
Constructor TfrmGEMainForm.TGENameIndexRec.Create(Const strName: String; Const iIndex: Integer);

Begin
  FName := strName;
  FIndex := iIndex;
End;

(**

  This is an overridden Ciompare method of the IComparer interface.

  @precon  None.
  @postcon This method sorts the TGENameIndexRec records by their FName field.

  @param   Left  as a TGENameIndexRec as a constant
  @param   Right as a TGENameIndexRec as a constant
  @return  an Integer

**)
Function TfrmGEMainForm.TGENameIndexComparer.Compare(Const Left, Right: TGENameIndexRec): Integer;

Begin
  Result := CompareText(Left.FName, Right.FName);
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
  SearchFind(Self, Editor, SearchMessage, FSearch, FReplace, FSearchOptions, seRegexSearch, seSearch,
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
      SearchFindNext(Editor, SearchMessage, FSearch, FReplace, FSearchOptions, seRegexSearch, seSearch)
    Else
      SearchFind(Self, Editor, SearchMessage, FSearch, FReplace, FSearchOptions, seRegexSearch,
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
  Editor.Redo();
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
    (Sender As TAction).Enabled := Editor.CanRedo;
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
  SearchReplace(Self, Editor, SearchMessage, FSearch, FReplace, FSearchOptions, seRegexSearch, seSearch,
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
  Editor.Clear;
  UpdateCaption;
  HookHighlighter;
  TDGHCustomSynEditFunctions.LoadFromINIFile(FINIFile, Editor);
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
  UpdateCaption;
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
    (Sender As TAction).Enabled := Editor.Modified;
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
  TfrmEditorOptions.Execute(Self, Editor, True);
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
  SearchReplaceText(Editor, ASearch, AReplace, Line, Column, Action);
End;

(**

  This method is an on status change event handler for the editor control.

  @precon  None.
  @postcon Updates the cursor position, insert mode and modified statusbar panels.

  @param   Sender  as a TObject
  @param   Changes as a TSynStatusChanges

**)
Procedure TfrmGEMainForm.EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);

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
  UpdateStatusBar(scCaret, Format('%1.0n:%1.0n', [Int(Editor.CaretY), Int(Editor.CaretX)]));
  UPdateStatusBar(scInsert, IfThen(Editor.InsertMode, strInsert, strOverwrite));
  UpdateStatusBar(scModified, IfThen(Editor.Modified, strModified,
    IfThen(Editor.ReadOnly, strReadOnly)));
  UpdateStatusBar(scLines, Format(strLines, [Int(Editor.Lines.Count)]));
  UpdateStatusBar(scSize, Format(strKBytes, [Int(Length(Editor.Lines.Text)) / dblKBytes]));
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
  strInformation = 'Information';
  strFileHasBeenModified = 'The file "%s" has been modified!';
  strSaveChangesToFile = 'Save the changes to the file "%s"';
  strDiscardChangesToFile = 'Discard the changes to the file "%s"';
  strCancelAndDonCloseEditor = 'Cancel and don''t close the editor';

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'FormCloseQuery', tmoTiming);{$ENDIF}
  If Editor.Modified Then
    Begin
      dlgTask.Title := strInformation;
      dlgTask.Caption := Application.Title;
      dlgTask.Flags := [tfAllowDialogCancellation, tfUseCommandLinks, tfPositionRelativeToWindow];
      dlgTask.MainIcon := tdiInformation;
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

ResourceString
  strINIPattern = '%s Settings for %s on %s.INI';
  strSeasonsFall = '\Season''s Fall\';
  
Var
  strBuffer : String;
  iSize : Integer;
  strFileName : String;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'FormCreate', tmoTiming);{$ENDIF}
  UpdateAppTitle;
  SetLength(strBuffer, MAX_PATH);
  iSize := GetModuleFileName(HInstance, PChar(strBuffer), MAX_PATH);
  SetLength(strBuffer, iSize);
  strFileName := ChangeFileExt(strBuffer, '');
  strFileName := Format(strINIPattern, [strFileName, UserName, ComputerName]);
  SetLength(strBuffer, MAX_PATH);
  SHGetFolderPath(0, CSIDL_APPDATA Or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT, PChar(strBuffer));
  strBuffer := StrPas(PChar(strBuffer));
  strBuffer := strBuffer + strSeasonsFall;
  If Not DirectoryExists(strBuffer) Then
    ForceDirectories(strBuffer);
  FINIFile := TMemIniFile.Create(strBuffer + ExtractFileName(strFileName));
  PatchEditor;
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
  TDGHCustomSynEditFunctions.SaveToINIFile(FINIFile, Editor);
  FINIFile.Free;
End;

(**

  This method changes the highlighter to the on associated with the selected menu option (in its tag
  property by main form component index).

  @precon  None.
  @postcon The highlighter for the editor is changed to the given editor.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.HighlighterClick(Sender: TObject);

Var
  MI : TMenuItem;
  
Begin
  If Sender Is TMenuItem Then
    Begin
      MI := Sender As TMenuItem;
      TDGHCustomSynEditFunctions.SaveToIniFile(FINIFile, Editor);
      Editor.Highlighter := Components[MI.Tag] As TSynCustomHighlighter;
      UpdateStatusBar(scFileType, TDGHCustomSynEditFunctions.HighlighterName(Editor.Highlighter));
      TDGHCustomSynEditFunctions.LoadFromIniFile(FINIFile, Editor);
    End;
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
                  Editor.Highlighter := H;
                  UpdateStatusBar(scFileType, TDGHCustomSynEditFunctions.HighlighterName(H));
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
  Editor.Highlighter := Nil;
  UpdateStatusBar(scFileType, strPlainTextFiles);
  strExt := LowerCase(ExtractFileExt(FFileName));
  If Length(strExt) = 0 Then
    Begin
      Editor.Highlighter := sehMD;
      UpdateStatusBar(scFileType, TDGHCustomSynEditFunctions.HighlighterName(Editor.Highlighter));
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
  strDefaultTheme = 'Windows';

Var
  strSectionName : String;
  iOp: TSearchOption;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'LoadSettings', tmoTiming);{$ENDIF}
  TStyleManager.TrySetStyle(FINIFile.ReadString(strSetupINISection, strVCLThemeKey, strDefaultTheme));
  UpdateStatusBar(scVCLTheme, TStyleManager.ActiveStyle.Name);
  strSectionName := Format(strWindowPosition, [MonitorProfile]);
  Left := FINIFile.ReadInteger(strSectionName, strLeft, Left);
  Top := FINIFile.ReadInteger(strSectionName, strTop, Top);
  Height := FINIFile.ReadInteger(strSectionName, strHeight, Height);
  Width := FINIFile.ReadInteger(strSectionName, strWidth, Width);
  FSearchOptions := [];
  For iOp := Low(TSearchOption) To High(TSearchOption) Do
    If FINIFile.ReadBool(strSearchOptionsIniSection, GetEnumName(TypeInfo(TSearchOption), Ord(iOp)),
      iOp In DefaultOptions) Then
      Include(FSearchOptions, iOp);
End;

(**

  This method returns a unique profile string for the monitor configuration so that the position and size
  of the maion window can be monitor configuration specific.

  @precon  None.
  @postcon A unqiue string for the monitor configuration is returned.

  @return  a String

**)
Function TfrmGEMainForm.MonitorProfile: String;

Const
  strMask = '%d=%dDPI(%s,%d,%d,%d,%d)';

Var
  iMonitor: Integer;
  M : TMonitor;

Begin
  Result := '';
  For iMonitor := 0 To Screen.MonitorCount - 1 Do
    Begin
      If Result <> '' Then
        Result := Result + ':';
      M := Screen.Monitors[iMonitor];
      Result := Result + Format(strMask, [M.MonitorNum, M.PixelsPerInch, BoolToStr(M.Primary, True),
        M.Left, M.Top, M.Width, M.Height]);
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
  strWarning = 'Warning';
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
      Editor.Highlighter := Nil;
      Editor.WordWrap := False; //: @note Next 2 lines are for the performance of loading LARGE files.
      Editor.Gutter.AutoSize := False;
      Editor.Lines.LoadFromFile(FFileName);
      Editor.ReadOnly := (GetFileAttributes(PChar(FFileName)) And FILE_ATTRIBUTE_READONLY <> 0);
    End Else
    Begin
      If DirectoryExists(ExtractFilePath(FFileName)) Then
        Begin
          dlgTask.Title := strWarning;
          dlgTask.Caption := Application.Title;
          dlgTask.Flags := [tfAllowDialogCancellation, tfUseCommandLinks, tfPositionRelativeToWindow];
          dlgTask.MainIcon := tdiWarning;
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
  TDGHCustomSynEditFunctions.LoadFromINIFile(FINIFile, Editor);  
End;

(**

  This method creates the synedit editor in the form - this done as visually adding this to the form
  seems to make the IDE unstable and the form does not show correctly.

  @precon  None.
  @postcon The editor is created, inserted into the form and configured.

**)
Procedure TfrmGEMainForm.PatchEditor;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CreateEditor', tmoTiming);{$ENDIF}
  Editor.Gutter.Font.Assign(Editor.Font);
  Editor.AddKey(ecDeleteChar, VK_DELETE, [ssCtrl], 0, []);
  Editor.AddKey(ecWordLeft, VK_LEFT, [ssCtrl], 0, []);
  Editor.AddKey(ecWordRight, VK_RIGHT, [ssCtrl], 0, []);
  Editor.AddKey(ecSelPageLeft, VK_LEFT, [ssAlt, ssShift], 0, []);
  Editor.AddKey(ecSelPageRight, VK_RIGHT, [ssAlt, ssShift], 0, []);
  Editor.AddKey(ecScrollLeft, VK_LEFT, [ssAlt], 0, []);
  Editor.AddKey(ecScrollRight, VK_RIGHT, [ssAlt], 0, []);
  Editor.AddKey(ecCommentBlock, Ord('C'), [ssAlt, ssCtrl], 0, []);
  Editor.AddKey(ecAutoCompletion, Ord('J'), [ssCtrl], 0, []);
  EditorStatusChange(Self, [scAll]);
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
  If Editor.Modified Then
    Begin
      If FileExists(FFileName) Then
        Result := SaveFileToDisk(strFileName)
      Else
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
  If Assigned(Editor.Highlighter) Then
    Begin
      FTI := dlgSave.FileTypes.Add;
      FTI.DisplayName := GetShortHint(Editor.Highlighter.DefaultFilter);
      FTI.FileMask := GetLongHint(Editor.Highlighter.DefaultFilter);
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
    Begin
      FFileName := dlgSave.FileName;
      SaveFileToDisk(FFileName);
    End;
End;

(**

  This method actually saves the file to disk without any checks as it is expected that all checks will 
  have been done before calling this method.

  @precon  strFileName is a valid filename.
  @postcon The file is saved to disk.

  @param   strFileName as a String as a constant
  @return  a Boolean

**)
Function TfrmGEMainForm.SaveFileToDisk(Const strFileName : String) : Boolean;

Begin
  Result := False;
  Try
    Editor.Lines.SaveToFile(strFileName);
    Result := True;
  Except
    On E : EWriteError Do
      Begin
        SearchMessage(E.Message);
        Abort;
      End;
  End;
  TDGHCustomSynEditFunctions.SaveToINIFile(FINIFile, Editor);
  Editor.Modified := False;
  Editor.MarkModifiedLinesAsSaved();
End;

(**

  This method saves the applications settings to the INI file.

  @precon  None.
  @postcon The application settings are saved to the INI file.

**)
Procedure TfrmGEMainForm.SaveSettings;

Var
  strSectionName : String;
  iOp : TSearchOption;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SaveSettings', tmoTiming);{$ENDIF}
  strSectionName := Format(strWindowPosition, [MonitorProfile]);
  FINIFile.WriteInteger(strSectionName, strLeft, Left);
  FINIFile.WriteInteger(strSectionName, strTop, Top);
  FINIFile.WriteInteger(strSectionName, strHeight, Height);
  FINIFile.WriteInteger(strSectionName, strWidth, Width);
  For iOp := Low(TSearchOption) To High(TSearchOption) Do
    FINIFile.WriteBool(strSearchOptionsIniSection, GetEnumName(TypeInfo(TSearchOption), Ord(iOp)),
      iOp In FSearchOptions);
  FINIFile.WriteString(strSetupINISection, strVCLThemeKey, TStyleManager.ActiveStyle.Name);
  FINIFile.UpdateFile;
End;

(**

  This is an on draw event handler for the statusbar panels.

  @precon  None.
  @postcon Draws the memory useage.

  @nocheck MissingConstInParam
  @nohint  Panel

  @param   StatusBar as a TStatusBar
  @param   Panel     as a TStatusPanel
  @param   Rect      as a TRect as a constant

**)
Procedure TfrmGEMainForm.sbrStatusbarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  Const Rect: TRect);

Const
  iMargin = 2;
  iLightRed = $8080FF;
  iLightGreen = $80FF80;
  dblPercentageMulitplier = 100.0;

ResourceString
  strFullStatus = 'Used %s in %s bytes (%1.1n%%) [Blocks: %dL, %dM, %dS]';
  strMediumStatus = 'Used %s in %s bytes (%1.1n%%)';
  strSmallerStatus = '%s in %s (%1.1n%%)';
  strMinimumStatus = '%s (%1.1n%%)';

Var
  R, T : TRect;
  strU, strR, strStatus : String;
  
Begin
  R := Rect;
  Inc(R.Top);
  Inc(R.Bottom);
  // Render Background
  StatusBar.Canvas.Brush.Color := iLightRed;
  StatusBar.Canvas.FillRect(R);
  StatusBar.Canvas.Font.Color := clBlack;
  // Write Background text
  strU      := CalcSize(FUsed);
  strR      := CalcSize(FReserved);
  strStatus := Format(strFullStatus,
    [strU, strR, Int(FPercentage), FMemoryBlock[dmtLarge], FMemoryBlock[dmtMedium],
      FMemoryBlock[dmtSmall]]);
  If StatusBar.Canvas.TextWidth(strStatus) > R.Right - R.Left - iMargin Then
    strStatus := Format(strMediumStatus, [strU, strR, Int(FPercentage)]);
  If StatusBar.Canvas.TextWidth(strStatus) > R.Right - R.Left - iMargin Then
    strStatus := Format(strSmallerStatus, [strU, strR, Int(FPercentage)]);
  If StatusBar.Canvas.TextWidth(strStatus) > R.Right - R.Left - iMargin Then
    strStatus := Format(strMinimumStatus, [strU, Int(FPercentage)]);
  T.Left := R.Left + ((R.Right - R.Left) - StatusBar.Canvas.TextWidth(strStatus)) Div 2;
  T.Right := T.Left + StatusBar.Canvas.TextWidth(strStatus);
  T.Top := R.Top;
  T.Bottom := R.Bottom;
  DrawText(StatusBar.Canvas.Handle, PChar(strStatus), Length(strStatus), T, DT_SINGLELINE Or DT_BOTTOM);
  StatusBar.Canvas.Brush.Color := iLightGreen;
  R.Right := R.Left + Trunc((R.Right - R.Left) * FPercentage / dblPercentageMulitplier);
  StatusBar.Canvas.FillRect(R);
  // Render % of memory text.
  T.Right := R.Right;
  StatusBar.Canvas.Font.Color := clBlack;
  If T.Right >= T.Left Then
    DrawText(StatusBar.Canvas.Handle, PChar(strStatus), Length(strStatus), T, DT_SINGLELINE Or
        DT_BOTTOM);
End;

(**

  This is an on mouse down event handler for the status bar.

  @precon  None.
  @postcon Displays popup menus for the VCL Themes and Highlight panels.

  @param   Sender as a TObject
  @param   Button as a TMouseButton
  @param   Shift  as a TShiftState
  @param   X      as an Integer
  @param   Y      as an Integer

**)
Procedure TfrmGEMainForm.sbrStatusbarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

Var
  iPanel: Integer;
  P : TStatusPanel;
  iLeft : integer;
  Pt : TPoint;

Begin
  iLeft := 0;
  If (Shift = [ssRight]) And (mbRight = Button) Then
    For iPanel := 0 To sbrStatusbar.Panels.Count - 1 Do
      Begin
        P := sbrStatusbar.Panels[iPanel];
        Pt := Point(X, y);
        Pt := sbrStatusbar.ClientToScreen(Pt);
        If (X >= iLeft) And (X <= iLeft + P.Width) Then
          Begin
            Case TGEStatusColumn(iPanel) Of
              scFileType: ShowHighlighterPopup(Pt);
              scVCLTheme: ShowVCLThemePopup(Pt);
            End;
            Break;
          End;
        Inc(iLeft, P.Width);
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

  This method searches the form for highlighters and adds them to a popup menu with the component index 
  in the menu item tag property and then displays the menu at the mouse position provided.

  @precon  None.
  @postcon A popup menu is displayed with a list of the available highlighters.

  @param   Pt as a TPoint as a constant

**)
Procedure TfrmGEMainForm.ShowHighlighterPopup(Const Pt : TPoint);

Var
  iComponent : Integer;
  H : TSynCustomHighlighter;
  MenuItem : TMenuItem;
  Names : TList<TGENameIndexRec>;
  
Begin
  pmStatusbar.Items.Clear;
  Names := TList<TGENameIndexRec>.Create(TGENameIndexComparer.Create);
  Try
    For iComponent := 0 To ComponentCount - 1 Do
      If Components[iComponent] Is TSynCustomHighlighter Then
        Begin
          H := Components[iComponent] As TSynCustomHighlighter;
          Names.Add(TGENameIndexRec.Create(TDGHCustomSynEditFunctions.HighlighterName(H), iComponent));
        End;
    Names.Sort;
    For iComponent := 0 To Names.Count - 1 Do
      Begin
        MenuItem := TMenuItem.Create(Self);
        MenuItem.Caption := Names[iComponent].FName;
        MenuItem.Tag := Names[iComponent].FIndex;
        MenuItem.OnClick := HighlighterClick;
        pmStatusbar.Items.Add(MenuItem);
      End;
    pmStatusbar.Popup(Pt.X, Pt.Y);
  Finally
    Names.Free;
  End;
End;

(**

  This method displays the VCL Themes statusbar panel popup menu.

  @precon  None.
  @postcon The VCL Themes to select from are displayed in the popup menu.

  @param   Pt as a TPoint as a constant

**)
procedure TfrmGEMainForm.ShowVCLThemePopup(Const Pt : TPoint);

Var
  astrNames : TArray<String>;
  iName: Integer;
  MenuItem : TMenuItem;
  Names : TList<TGENameIndexRec>;
  
begin
  pmStatusbar.Items.Clear;
  astrNames := TStyleManager.StyleNames;
  Names := TList<TGENameIndexRec>.Create(TGENameIndexComparer.Create);
  Try
    For iName := Low(astrNames) To High(astrNames) Do
      Names.Add(TGENameIndexRec.Create(astrNames[iName], iName));
    Names.Sort;
    For iName := 0 To Names.Count - 1 Do
      Begin
        MenuItem := TMenuItem.Create(Self);
        MenuItem.Caption := Names[iName].FName;
        MenuItem.Tag := Names[iName].FIndex;
        MenuItem.OnClick := VCLThemeClick;
        pmStatusbar.Items.Add(MenuItem);
      End;
    pmStatusbar.Popup(Pt.X, Pt.Y);
  Finally
    Names.Free;
  End;
end;

(**

  This is an on timer event handler for the memeory timer.

  @precon  None.
  @postcon Updates the memory statusbar panel.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.tmMemoryTimer(Sender: TObject);

Const
  dblPercentageMultiplier = 100.0;

Var
  MMS : TMemoryManagerState;
  i   : Integer;
  SBTS: TSmallBlockTypeState;

Begin
  GetMemoryManagerState(MMS);
  FUsed := MMS.TotalAllocatedLargeBlockSize + MMS.TotalAllocatedMediumBlockSize;
  FReserved := MMS.ReservedLargeBlockAddressSpace + MMS.ReservedMediumBlockAddressSpace;
  FMemoryBlock[dmtLarge]  := MMS.AllocatedLargeBlockCount;
  FMemoryBlock[dmtMedium] := MMS.AllocatedMediumBlockCount;
  FMemoryBlock[dmtSmall]  := 0;
  For i := Low(MMS.SmallBlockTypeStates) To High(MMS.SmallBlockTypeStates) Do
    Begin
      SBTS := MMS.SmallBlockTypeStates[i];
      Inc(FMemoryBlock[dmtSmall], SBTS.AllocatedBlockCount);
      Inc(FUsed, SBTS.UseableBlockSize * SBTS.AllocatedBlockCount);
      Inc(FReserved, SBTS.ReservedAddressSpace);
    End;
  FPercentage := 0.0;
  If FReserved > 0.0 Then
    FPercentage := Int(FUsed) / Int(FReserved) * dblPercentageMultiplier;
  sbrStatusbar.Panels[scMemoryUseage.ColumnIndex].Text := Format('%1.1n%%', [FPercentage]);
End;

(**

  This method updates the application title with version / build information.

  @precon  None.
  @postcon The applciation title is updated with version and build information.

**)
Procedure TfrmGEMainForm.UpdateAppTitle;

Const
  strBugFix = ' abcedfghijklmnopqrstuvwxyz';

ResourceString
  {$IFDEF DEBUG}
  strGitEditorBuild = 'Git Editor %d.%d%s (DEBUG Build %d.%d.%d.%d)';
  {$ELSE}
  strGitEditorBuild = 'Git Editor %d.%d%s (Build %d.%d.%d.%d)';
  {$ENDIF}

Var
  BuildInfo : TGEBuildInfo;
  
Begin
  GetBuildInfo(BuildInfo);
  Application.Title := Format(strGitEditorBuild, [
      BuildInfo.FMajor,
      BuildInfo.FMinor,
      strBugFix[BuildInfo.FRelease + 1],
      BuildInfo.FMajor,
      BuildInfo.FMinor,
      BuildInfo.FRelease,
      BuildInfo.FBuild
    ])
End;

(**

  This method updates the caption of the form.

  @precon  None.
  @postcon The caption of the form is updated with the full filename.

**)
Procedure TfrmGEMainForm.UpdateCaption;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'UpdateCaption', tmoTiming);{$ENDIF}
  Caption := Application.Title + ': ' + ExpandFileName(FFileName);
End;

(**

  Thisd method updates the given status bar panel with the given text ensuring the panel is width enough
  to contain the text plus a margin.

  @precon  None.
  @postcon The given statusbar panel is updated with the given text.

  @param   eStatusColumn as a TGEStatusColumn as a constant
  @param   strText       as a String as a constant

**)
Procedure TfrmGEMainForm.UpdateStatusBar(Const eStatusColumn : TGEStatusColumn; Const strText : String);

Const
  iTextPadding = 2 * 10;

Begin
  sbrStatusbar.Canvas.Font.Assign(sbrStatusbar.Font);
  sbrStatusbar.Panels[eStatusColumn.ColumnIndex].Width := sbrStatusbar.Canvas.TextWidth(strText) +
    iTextPadding;
  sbrStatusbar.Panels[eStatusColumn.ColumnIndex].Text := strText;
End;

(**

  This is an on click event handler for the status bar VCL themes panel.

  @precon  None.
  @postcon Changes the VCL Theme to the one indexed in the popup menus tag property.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.VCLThemeClick(Sender: TObject);

Var
  MI : TMenuItem;
  
Begin
  If Sender Is TMenuItem Then
    Begin
      MI := Sender As TMenuItem;
      TStyleManager.TrySetStyle(TStyleManager.StyleNames[MI.Tag]);
      UpdateStatusBar(scVCLTheme, TStyleManager.ActiveStyle.Name);
    End;
End;

End.
