unit Command;
//  ----------------------------------------------------------
//  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
//  All rights reserved.
//  ----------------------------------------------------------
interface

uses
    Hashlist;

type
    TCommandList = class(TObject)
    PRIVATE
        CommandList: TCommandHashListType;
        abbrevCommandList: TCommandHashListType;
    PUBLIC
        Abbrev: Boolean;

        constructor Create(Commands: array of String);
        destructor Destroy; OVERRIDE;
        function GetCommand(const Cmd: String): Integer;
        function Get(i: Integer): String;
        function Count: Integer;
    end;

implementation

constructor TCommandList.Create(Commands: array of String);
var
    i, j: Integer;
    abbrevStr: String;
begin
    inherited Create;
    CommandList := TCommandHashListType.Create(High(Commands) + 1);
    abbrevCommandList := TCommandHashListType.Create(High(Commands) + 1);
    Abbrev := True;

    // Fill the HashList
    for i := 0 to High(Commands) do
    begin
        CommandList.Add(Commands[i]);
        abbrevCommandList.Add(Commands[i]);
    end;

    for i := 0 to High(Commands) do
    begin
        for j := 1 to (Length(Commands[i]) - 1) do
        begin
            abbrevStr := Copy(Commands[i], 1, j);
            if abbrevCommandList.Find(abbrevStr) = 0 then
            begin
                abbrevCommandList.Add(abbrevStr, i + 1);
            end;
        end;
    end;
end;

destructor TCommandList.Destroy;
begin
    CommandList.Free;
    abbrevCommandList.Free;
    inherited Destroy;
end;

function TCommandList.GetCommand(const Cmd: String): Integer;
begin
    //TODO: benchmark if checking the smaller CommandList first
    // is faster in general, or just using abbrevCommandList, 
    // always, is faster.
    if not abbrev then
        Result := CommandList.Find(Cmd)
    else
        Result := abbrevCommandList.Find(Cmd);
end;

function TCommandList.Get(i: Integer): String;
begin
    Result := CommandList.NameOfIndex(i);
end;

function TCommandList.Count: Integer;
begin
    Result := CommandList.Count;
end;

end.
