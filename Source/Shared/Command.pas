unit Command;

 {
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}


{$M+}

interface

uses
    ArrayDef,
    Hashlist;

type
    TCommandList = class(TObject)
    PRIVATE
        CommandList: THashList;
        AbbrevAllowed: Boolean;
        function Get_NumCommands: Integer;
    PROTECTED

    PUBLIC
        constructor Create(Commands: pStringArray; n: Integer); OVERLOAD;
        constructor Create(Commands: array of String); OVERLOAD;
        destructor Destroy; OVERRIDE;
        procedure AddCommand(const cmd: String);
        function Getcommand(const Cmd: String): Integer;
        function CheckifValid(): Boolean;
        function Get(i: Integer): String;
        property Abbrev: Boolean READ AbbrevAllowed WRITE AbbrevAllowed;
        property NumCommands: Integer READ Get_NumCommands;
        property IsValidPtr: Boolean READ CheckifValid;
    PUBLISHED

    end;


implementation

uses
    sysutils;

constructor TCommandList.Create(Commands: pStringArray; n: Integer);
var
    i: Integer;
begin
    inherited Create;
    CommandList := THashList.Create(n);
    for i := 1 to n do
    begin
        CommandList.Add(Commands^[i]);
    end;

    AbbrevAllowed := TRUE;
end;

constructor TCommandList.Create(Commands: array of String);
var
    i: Integer;
begin
    inherited Create;
    CommandList := THashList.Create(High(Commands) + 1);
    for i := 0 to High(Commands) do
    begin
        CommandList.Add(Commands[i]);
    end;

    AbbrevAllowed := TRUE;
end;

destructor TCommandList.Destroy;

begin
    FreeAndNil(CommandList);
    inherited Destroy;
end;

procedure TCommandList.AddCommand(const cmd: String);
begin
    CommandList.Add(Cmd);
end;

function TCommandList.Getcommand(const Cmd: String): Integer;

begin

    Result := CommandList.Find(Cmd);
{If no match found on whole command, check for abbrev}
{This routine will generally be faster if one enters the whole command}
    if Result = 0 then
        if AbbrevAllowed then
            Result := CommandList.FindAbbrev(Cmd);

end;


function TCommandList.Get(i: Integer): String;
begin
    Result := CommandList.Get(i);
end;

function TCommandList.Get_NumCommands: Integer;
begin
    Result := CommandList.ListSize;
end;

function TCommandList.CheckifValid(): Boolean;
// Used to verify if the object is valid or has been cleared by someone else
// Practical when destroying common Lists while working in parallel
begin
    Result := CommandList.IsValidPtr;
end;

end.
