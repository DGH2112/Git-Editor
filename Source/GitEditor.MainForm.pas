(**
  
  This module contaisn a class which represent the main form for the application - a single window
  text editor.

  @Author  David Hoyle
  @Version 1.0
  @Date    20 Jan 2018
  
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
  SynHighlighterBNF;

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
  Strict Private
    FIniFile : String;
    FEditor : TSynEdit;
    FFileName : String;
  Strict Protected
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure CreateEditor;
    Procedure EditorStatusChange(Sender : TObject; Changes : TSynStatusChanges);
    Procedure OpenFile;
    Procedure UpdateCaption;
    Procedure HookHighlighter;
    Function SaveFile: Boolean;
  Public
  End;

Var
  (** A Delphi managed form variable. **)
  frmGEMainForm: TfrmGEMainForm;

Implementation

{$R *.dfm}

Uses
  System.StrUtils,
  System.IniFiles, 
  System.UITypes,
  System.TypInfo,
  WinAPI.SHFolder,
  GitEditor.CommonFunctions,
  SynEditKeyCmds, 
  GitEditor.SynEditOptionsForm, 
  GitEditor.SynHighlighterUtils;

Type
  (** An enumerate to define the statusbar columns. **)
  TGEStatusColumn = (scCaret, scInsert, scModified, scFileType);
  (** A record helper to convert the above enumerate to column indexes. **)
  TGEStatusColumnHelper = Record Helper For TGEStatusColumn
    Function ColumnIndex : Integer;
  End;
  
ResourceString
  (** A string constant for the Window Position INI Section **)
  strWindowPosition = 'Window Position';
  (** A string constant for the INI file name pattern. **)
  strINIPattern = '%s Settings for %s on %s.INI';
  (** A string constant for the profile sub-directory for storing the INI file. **)
  strSeasonsFall = '\Season''s Fall\';
  (** A resource string for prompting the user to save the file. **)
  strSaveFile = 'The file "%s" you are editing has changed! Would you like to save the changed?';
  (** A resource string for a filel not found. **)
  strFileNotFound = 'This file "%s" was not found!';

{ TGEStatusColumnHelper }

(**

  This method converts the enumerate to an integer.

  @precon  None.
  @postcon An integer is returned.

  @return  an Integer

**)
Function TGEStatusColumnHelper.ColumnIndex: Integer;

Begin
  Result := Integer(Self);
End;

Procedure TfrmGEMainForm.actEditFindExecute(Sender: TObject);

Begin
  ShowMessage('NOT IMPLEMENTED YET!');
  //: @todo Implement Find
End;

(**

  This method is an on execute event handler for the Edit Redo action.

  @precon  None.
  @postcon Redoes the last undo in the editor.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actEditRedoExecute(Sender: TObject);

Begin
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
  If Sender Is TAction Then
    (Sender As TAction).Enabled := FEditor.CanRedo;
End;

Procedure TfrmGEMainForm.actEditReplaceExecute(Sender: TObject);

Begin
  ShowMessage('NOT IMPLEMENTED YET!');
  //: @todo Implement Replace
End;

(**

  This is an on execute event handler for the File Exit action.

  @precon  None.
  @postcon Closes the form / application.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actFileExitExecute(Sender: TObject);

Begin
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
  //: @todo Implement FileNew
  ShowMessage('NOT IMPLEMENTED YET!');
End;

(**

  This is an on execute event handler for the File Open event handler.

  @precon  None.
  @postcon Prompts the user to open a new file after saving the current file if changed.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actFileOpenExecute(Sender: TObject);

Begin
  SaveFile();
  ShowMessage('NOT IMPLEMENTED YET!');
  //: @todo Omplement File Open
End;

(**

  This is an on execute event handler for the File SaveAs event handler.

  @precon  None.
  @postcon Prompts the user to save the file to another fiename.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actFileSaveAsExecute(Sender: TObject);

Begin
  ShowMessage('NOT IMPLEMENTED YET!');
  //: @todo Implement File Save
End;

(**

  This method is an on execute event handler for the File Save action.

  @precon  None.
  @postcon Saves the file to disk.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actFileSaveExecute(Sender: TObject);

Begin
  SaveFile;
End;

(**

  This is an on update event handler for the File Save action.

  @precon  None.
  @postcon Updates the enabled property of the action base on whether the file is modified.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.actFileSaveUpdate(Sender: TObject);

Begin
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
  TfrmEditorOptions.Execute(Self, FEditor, True);
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
  FEditor.PopupMenu := pabrContextMenu;
  FEditor.OnStatusChange := EditorStatusChange;
  EditorStatusChange(Self, [scAll]);
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

Begin
  sbrStatusbar.Panels[scCaret.ColumnIndex].Text := Format('%1.0n:%1.0n', [Int(FEditor.CaretY),
    Int(FEditor.CaretX)]);
  sbrStatusbar.Panels[scInsert.ColumnIndex].Text := IfThen(FEditor.InsertMode, strInsert, strOverwrite);
  sbrStatusbar.Panels[scModified.ColumnIndex].Text := IfThen(FEditor.Modified, strModified);
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

Begin
  If FEditor.Modified Then
    Case MessageDlg(Format(strSaveFile, [FFileName]), mtConfirmation, [mbYes, mbNo, mbCancel], 0) Of
      mrYes:    CanClose := SaveFile;
      mrNo:     CanClose := true;
      mrCancel: CanClose := false;
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
  
Begin
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
  OpenFile;
End;

(**

  This is an on form destroy event handler for the form class.

  @precon  None.
  @postcon Saves the applications settings.

  @param   Sender as a TObject

**)
Procedure TfrmGEMainForm.FormDestroy(Sender: TObject);

Begin
  SaveSettings;
  SaveToINIFile(FIniFile, FEditor);
End;

(**

  This method hooks the specific highlight associated with the file extension.

  @precon  None.
  @postcon The highlight is set if an appropriate one exists for the file.

**)
Procedure TfrmGEMainForm.HookHighlighter;

Var
  strExt : String;
  strExts : String;
  iComponent : Integer;
  H : TSynCustomHighlighter;
  strHExt : TArray<String>;
  i : Integer;
  
Begin
  strExt := '*' + LowerCase(ExtractFileExt(FFileName));
  sbrStatusbar.Panels[scFileType.ColumnIndex].Text := 'Plain Text Files';
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

(**

  This method loads the applications settings from the INI file.

  @precon  None.
  @postcon The application settings are loaded from the INI file.

**)
Procedure TfrmGEMainForm.LoadSettings;

Var
  iniFile : TCustomIniFile;
  
Begin
  iniFile := TMemIniFile.Create(FIniFile);
  Try
    Left := iniFile.ReadInteger(strWindowPosition, 'Left', Left);
    Top := iniFile.ReadInteger(strWindowPosition, 'Top', Top);
    Height := iniFile.ReadInteger(strWindowPosition, 'Height', Height);
    Width := iniFile.ReadInteger(strWindowPosition, 'Width', Width);
  Finally
    iniFile.Free;
  End;
End;

(**

  If a filename is passed on the command line, the file is opened in the editor.

  @precon  None.
  @postcon The file from the first parameter of the command line is open if provied else a default
           untitled file is assumed.

**)
Procedure TfrmGEMainForm.OpenFile;

Begin
  If ParamCount = 0 Then
    FFileName := '(untitled)'
  Else
    Begin
      FFileName := ParamStr(1);
      If FileExists(FFileName) Then
        FEditor.Lines.LoadFromFile(FFileName)
      Else
        Begin
          // Add the ability to create the file.
          ShowMessage(Format(strFileNotFound, [FFileName]));
        End;
    End;
  UpdateCaption;
  HookHighlighter;
  LoadFromINIFile(FIniFile, FEditor);
End;

(**

  This method saves the file in the editor.

  @precon  None.
  @postcon The file is saved if the underlying filename exists else a SaveAs dialogue is shown. If the 
           file is saved this function returns true.

  @todo    Allow a filename to be passed so this can be used from SaveAs.

  @return  a Boolean

**)
Function TfrmGEMainForm.SaveFile : Boolean;

Begin
  If FileExists(FFileName) Then
    Begin
      FEditor.Lines.SaveToFile(FFileName);
      SaveToINIFile(FIniFile, FEditor);
      FEditor.Modified := False;
      FEditor.MarkModifiedLinesAsSaved();
      Result := True;
    End Else
      // Add file save as
      Result := False;
End;

(**

  This method saves the applications settings to the INI file.

  @precon  None.
  @postcon The application settings are saved to the INI file.

**)
Procedure TfrmGEMainForm.SaveSettings;

Var
  iniFile : TCustomIniFile;
  
Begin
  iniFile := TMemIniFile.Create(FIniFile);
  Try
    iniFile.WriteInteger(strWindowPosition, 'Left', Left);
    iniFile.WriteInteger(strWindowPosition, 'Top', Top);
    iniFile.WriteInteger(strWindowPosition, 'Height', Height);
    iniFile.WriteInteger(strWindowPosition, 'Width', Width);
    iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
End;

(**

  This method updates the caption of the form.

  @precon  None.
  @postcon The caption of the form is updated with the full filename.

**)
Procedure TfrmGEMainForm.UpdateCaption;

Const
  strBugFix = ' abcedfghijklmnopqrstuvwxyz';

Var
  BuildInfo : TGEBuildInfo;
  
Begin
  GetBuildInfo(BuildInfo);
  Caption := Format('Git Editor %d.%d%s (Build %d.%d.%d.%d): ',
    [
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
