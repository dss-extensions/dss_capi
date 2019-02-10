unit ImplDSS_Executive;

 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TDSS_Executive = class(TAutoObject, IDSS_Executive)
    PROTECTED
        function Get_Command(i: Integer): Widestring; SAFECALL;
        function Get_NumCommands: Integer; SAFECALL;
        function Get_NumOptions: Integer; SAFECALL;
        function Get_Option(i: Integer): Widestring; SAFECALL;
        function Get_CommandHelp(i: Integer): Widestring; SAFECALL;
        function Get_OptionHelp(i: Integer): Widestring; SAFECALL;
        function Get_OptionValue(i: Integer): Widestring; SAFECALL;

    end;

implementation

uses
    ComServ,
    DSSGlobals,
    ExecCommands,
    ExecOptions,
    Executive;

function TDSS_Executive.Get_Command(i: Integer): Widestring;
begin
    Result := ExecCommand[i];
end;

function TDSS_Executive.Get_NumCommands: Integer;
begin
    Result := NumExecCommands;
end;

function TDSS_Executive.Get_NumOptions: Integer;
begin
    Result := NumExecOptions;
end;

function TDSS_Executive.Get_Option(i: Integer): Widestring;
begin
    Result := ExecOption[i];
end;

function TDSS_Executive.Get_CommandHelp(i: Integer): Widestring;
begin
    Result := CommandHelp[i];
end;

function TDSS_Executive.Get_OptionHelp(i: Integer): Widestring;
begin
    Result := OptionHelp[i];
end;

function TDSS_Executive.Get_OptionValue(i: Integer): Widestring;
begin
    DSSExecutive.Command := 'get ' + ExecOption[i];
    Result := GlobalResult;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TDSS_Executive, Class_DSS_Executive,
        ciInternal, tmApartment);
end.
