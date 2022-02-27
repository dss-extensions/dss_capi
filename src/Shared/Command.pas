unit Command;

 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Hashlist;

type
    TCommandList = class(TObject)
    PRIVATE
        CommandList: TCommandHashListType;
        AbbrevAllowed: Boolean;
        function Get_NumCommands: Integer;
    PUBLIC
        constructor Create(Commands: array of String; AllowAbbrev: Boolean = True);
        destructor Destroy; OVERRIDE;
        procedure AddCommand(const cmd: String);
        function Getcommand(const Cmd: String): Integer;
        function Get(i: Integer): String;
        property Abbrev: Boolean READ AbbrevAllowed WRITE AbbrevAllowed;
        property NumCommands: Integer READ Get_NumCommands;
    end;


implementation

constructor TCommandList.Create(Commands: array of String; AllowAbbrev: Boolean = True);
var
    i: Integer;
begin
    inherited Create;
    CommandList := TCommandHashListType.Create(High(Commands) + 1);

    for i := 0 to High(Commands) do
    begin
      // Fill the HashList
        CommandList.Add(Commands[i]);
    end;

    AbbrevAllowed := AllowAbbrev;
end;

destructor TCommandList.Destroy;

begin
    CommandList.Free;
    inherited Destroy;
end;

procedure TCommandList.AddCommand(const cmd: String);
begin
    CommandList.Add(Cmd);
end;

function TCommandList.Getcommand(const Cmd: String): Integer;

begin
    Result := CommandList.Find(Cmd);
    // If no match found on whole command, check for abbrev
    // This routine will generally be faster if one enters the whole command
    if Result = 0 then
        if AbbrevAllowed then
            Result := CommandList.FindAbbrev(Cmd);
end;


function TCommandList.Get(i: Integer): String;
begin
    Result := CommandList.NameOfIndex(i);
end;

function TCommandList.Get_NumCommands: Integer;
begin
    Result := CommandList.Count;
end;

end.
