(**
  
  This project is a simple lightweight text editor (single window) for use with Git or for simple and
  quick tetx edit changes.

  @Author  David Hoyle
  @Version 1.0
  @Date    26 Jan 2018

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
  GitEditor.CommonFunctions in 'Source\GitEditor.CommonFunctions.pas',
  Vcl.Themes,
  Vcl.Styles;

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
