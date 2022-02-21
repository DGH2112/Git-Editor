(**
  
  This project is a simple lightweight text editor (single window) for use with Git or for simple and
  quick tetx edit changes.

  @Author  David Hoyle
  @Version 1.004
  @Date    20 Feb 2022

  @nocheck HardCodedString
  
**)
Program GitEditor64;

{$R 'GitEditorITHVerInfo.res' 'GitEditorITHVerInfo.RC'}

uses
  {$IFDEF EurekaLog}
  EMemLeaks,
  EResLeaks,
  EDebugExports,
  EDebugJCL,
  EMapWin32,
  EAppVCL,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  ESendMailMAPI,
  ESendMailSMAPI,
  ExceptionLog7,
  {$ENDIF EurekaLog}
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Themes,
  Vcl.Styles,
  DDetours,
  Vcl.Styles.Fixes,
  Vcl.Styles.Hooks,
  GitEditor.MainForm in 'Source\GitEditor.MainForm.pas' {frmGEMainForm},
  GitEditor.CommonFunctions in 'Source\GitEditor.CommonFunctions.pas';

{$R *.res}

Begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Git Editor';
  Application.CreateForm(TfrmGEMainForm, frmGEMainForm);
  Application.Run;
End.

