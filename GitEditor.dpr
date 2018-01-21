(**
  
  This project is a simple lightweight text editor (single window) for use with Git or for simple and
  quick tetx edit changes.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jan 2018

  @nocheck HardCodedString
  
**)
Program GitEditor;

{$R 'GitEditorITHVerInfo.res' 'GitEditorITHVerInfo.RC'}

uses
  {$IFDEF EurekaLog}
  EMemLeaks,
  EResLeaks,
  ESendMailMAPI,
  ESendMailSMAPI,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  EDebugExports,
  EDebugJCL,
  EMapWin32,
  EAppVCL,
  ExceptionLog7,
  {$ENDIF EurekaLog}
  Vcl.Forms,
  GitEditor.MainForm in 'Source\GitEditor.MainForm.pas' {frmGEMainForm},
  SynHighlighterBNF in '..\..\Components\Source\SynHighlighterBNF.pas',
  SynHighlighterMD in '..\..\Components\Source\SynHighlighterMD.pas',
  SynHighlighterRegEx in '..\..\Components\Source\SynHighlighterRegEx.pas',
  GitEditor.CommonFunctions in 'Source\GitEditor.CommonFunctions.pas',
  GitEditor.SynEditOptionsForm in 'Source\GitEditor.SynEditOptionsForm.pas' {frmEditorOptions},
  GitEditor.SynHighlighterUtils in 'Source\GitEditor.SynHighlighterUtils.pas',
  GitEditor.SearchReplaceForm in 'Source\GitEditor.SearchReplaceForm.pas' {frmSearchAndReplace},
  GitEditor.ConfirmationDlgForm in 'Source\GitEditor.ConfirmationDlgForm.pas' {frmConfirmationDlg};

{$R *.res}


Begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  {$IFDEF EurekaLog}
  SetEurekaLogState(DebugHook = 0);
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Git Editor';
  Application.CreateForm(TfrmGEMainForm, frmGEMainForm);
  Application.Run;
End.
