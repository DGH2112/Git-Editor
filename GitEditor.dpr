program GitEditor;

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
  GitEditor.SynHighlighterUtils in 'Source\GitEditor.SynHighlighterUtils.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  {$IFDEF EurekaLog}
  SetEurekaLogState(DebugHook = 0);
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Git Editor';
  Application.CreateForm(TfrmGEMainForm, frmGEMainForm);
  Application.Run;
end.
