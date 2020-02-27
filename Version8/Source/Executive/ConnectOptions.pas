unit ConnectOptions;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2017, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Command;

const
    NumConnectOptions = 2;

function DoConnectCmd: Integer;
function DoDisConnectCmd: Integer;

var
    ConnectOption,
    ConnectHelp: array[1..NumConnectOptions] of String;
    ConnectCommands: TCommandList;

implementation

uses
  {$IFNDEF Linux}
    TCP_IP,
  {$ENDIF}
    DSSGlobals,
    SysUtils,
    ParserDel,
    Utilities;

procedure DefineOptions;
begin
    ConnectOption[1] := 'address';
    ConnectOption[2] := 'port';

    ConnectHelp[1] := 'Address is a string containing the IP address of a particular system with which OpenDSS should form a connection';
    ConnectHelp[2] := 'Port is the ID of the desired server connection:' + CRLF +
        '47625 = OpenDSS Viewer';
end;

function DoConnectCmd: Integer;
//Var
//   ParamName, Param:String;
//   ParamPointer, i:Integer;
begin
    Result := 0;

//    If NoFormsAllowed Then Begin Result :=1; Exit; End;
  {$IFNDEF Linux}
    if not Assigned(DSSConnectObj) then
        DSSConnectObj := TDSSConnect.Create;
    DSSConnectObj.SetDefaults;
    with DSSConnectObj do
    begin
        Connect;
    end;
  {$ENDIF}
end;

function DoDisConnectCmd: Integer;
//Var
//  ParamName, Param:String;
//  ParamPointer, i:Integer;
begin
    Result := 0;

//    If NoFormsAllowed Then Begin Result :=1; Exit; End;
  {$IFNDEF Linux}
    if Assigned(DSSConnectObj) then
    begin
        with DSSConnectObj do
        begin
            Disconnect;
        end;
    end;
  {$ENDIF}
end;


procedure DisposeStrings;
var
    i: Integer;

begin
    for i := 1 to NumConnectOptions do
    begin
        ConnectOption[i] := '';
        ConnectHelp[i] := '';
    end;

end;


initialization
    DefineOptions;
    ConnectCommands := TCommandList.Create(ConnectOption);
    ConnectCommands.Abbrev := TRUE;

finalization
    DoDisConnectCmd;
    DisposeStrings;
    ConnectCommands.Free;
end.
