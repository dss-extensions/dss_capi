program TestDLL;

//{$mode objfpc}{$H+}
{$mode delphi}
//{$MODESWITCH ADVANCEDRECORDS}
{$MACRO ON}

{$IFDEF Windows}
{$DEFINE DSS_CALL:=stdcall}
//{$DEFINE DSS_CALL:=cdecl}
{$ELSE} // Darwin and Unix
{$DEFINE DSS_CALL:=cdecl}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, dynlibs;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  private
    FLibHandle: TLibHandle;
    FuncError: Boolean;
    dss_DSSI: procedure;DSS_CALL;
    function find_dss_function (name: String): Pointer;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  // stop program loop
  Terminate;
end;

function TMyApplication.find_dss_function (name: String): Pointer;
begin
  Result := GetProcedureAddress (FLibHandle, name);
  if Result = nil then begin
    writeln ('DSS library found, but missing function ', name);
    FuncError := True;
  end;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
var
  dll_name: string;
begin
  inherited Create(TheOwner);
  // StopOnException:=True;
  dll_name := './test/libsample.' + SharedSuffix;
  dll_name := './test/libopendssdirect.' + SharedSuffix;
  writeln ('Try to load:', dll_name);
{$IFDEF Windows}
  FLibHandle := SafeLoadLibrary (dll_name);
{$ELSE} // Darwin and Unix
  FLibHandle := SafeLoadLibrary (dll_name);
  writeln ('Return from SafeLoadLibrary with FLibHandle = ', FLibHandle);
{$ENDIF}
  if FLibHandle <> DynLibs.NilHandle then begin
    FuncError := False;
    @dss_DSSI := find_dss_function ('DSSI');
    if FuncError then begin
      UnloadLibrary(FlibHandle);
      FLibHandle := DynLibs.NilHandle;
    end;
  end;
end;

destructor TMyApplication.Destroy;
begin
  If FLibHandle <> DynLibs.NilHandle Then Begin
    UnloadLibrary(FLibHandle);
  End;
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

