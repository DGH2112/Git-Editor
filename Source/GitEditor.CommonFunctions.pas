(**
  
  This module contains a number of functions that can be used through the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    20 Jan 2018
  
**)
Unit GitEditor.CommonFunctions;

Interface

Type
  (** A record to describe the EXE version information. **)
  TGEBuildInfo = Record
    FMajor   : Integer;
    FMinor   : Integer;
    FRelease : Integer;
    FBuild   : Integer;
  End;

  Function UserName : String;
  Function ComputerName : String;
  Procedure GetBuildInfo(Var BuildInfo : TGEBuildInfo);
  
Implementation

Uses
  System.SysUtils,
  WinAPI.Windows;

(**

  This function returns a unicode string of the current Computer Name.

  @precon  None.
  @postcon The computer name is returned.

  @return  a String

**)
Function ComputerName : String;

Var
  iSize : Cardinal;
  
Begin
  iSize := MAX_PATH;
  SetLength(Result, iSize);
  GetComputerName(PChar(Result), iSize);
  Result := StrPas(PChar(Result));
End;

(**

  This method updates the application caption to include the applications version number and build 
  information.

  @precon  None.
  @postcon The applications caption is updated with version and build information.

  @param   BuildInfo as a TGEBuildInfo as a reference

**)
Procedure GetBuildInfo(Var BuildInfo : TGEBuildInfo);

Const
  iWordMask = $FFFF;
  iShift16 = 16;

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;

Begin
  BuildInfo.FMajor := 0;
  BuildInfo.FMinor := 0;
  BuildInfo.FRelease := 0;
  BuildInfo.FBuild := 0;   
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      BuildInfo.FMajor := VerValue^.dwFileVersionMS Shr iShift16;
      BuildInfo.FMinor := VerValue^.dwFileVersionMS And iWordMask;
      BuildInfo.FRelease := VerValue^.dwFileVersionLS Shr iShift16;
      BuildInfo.FBuild := VerValue^.dwFileVersionLS And iWordMask;
      FreeMem(VerInfo, VerInfoSize);
    End;
End;

(**

  This function returns a unicode string of the current Users Logon Name.

  @precon  None.
  @postcon The username of the user is returned.

  @return  a String

**)
Function UserName : String;

Var
  iSize : Cardinal;

Begin
  iSize := MAX_PATH;
  SetLength(Result, iSize);
  GetUserName(PChar(Result), iSize);
  Result := StrPas(PChar(Result));
End;

End.
