unit NamedObject;

// ----------------------------------------------------------
// Copyright (c) 2009-2022, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

type

    TUuid = TGuid;    // this is a GUID compliant to RFC 4122, v4

    TNamedObject = class(TObject)
    // TODO: remove TNamedObject as a whole. Use an extra structure to track the data here.
    PROTECTED
        pUuid: ^TUuid;  // compliant to RFC 4122, v4
    PUBLIC
        LocalName: String;  // localName is unique within a class, like the old FName
        DisplayName: String;

        constructor Create(ClassName_: String);
        destructor Destroy; OVERRIDE;

        function GetCIM_ID(): String;
        function GetID(): String;
        function GetUUID(): TUuid;
        procedure SetUUID(const Value: TUuid);
    end;

function CreateUUID4(out UUID: TUuid): Integer;
function StringToUUID(const S: String): TUuid;
function UUIDToString(const UUID: TUuid): String;
function UUIDToCIMString(UUID: TUuid): String;

implementation

uses
    Sysutils,
    StrUtils;

function CreateUUID4(out UUID: TUuid): Integer;
begin
    Result := CreateGUID(UUID);
    UUID.D3 := (UUID.D3 and $0fff) or $4000;   // place a 4 at character 13
    UUID.D4[0] := (UUID.D4[0] and $3f) or $80; // character 17 to be 8, 9, A or B
end;

function StringToUUID(const S: String): TUuid;
begin
    Result := StringToGUID(S);
end;

function UUIDToString(const UUID: TUuid): String;
begin
    Result := GuidToString(UUID);
end;

function UUIDToCIMString(UUID: TUuid): String;
var
    s: String;
begin
    s := GUIDToString(UUID);
    Result := MidStr(s, 2, Length(s) - 2);
end;

constructor TNamedObject.Create(ClassName_: String);
begin
    inherited Create;
    LocalName := '';
    DisplayName := '';
    pUuid := NIL;
end;

destructor TNamedObject.Destroy;
begin
    if pUuid <> NIL then
        Dispose(pUuid);
    inherited Destroy;
end;

procedure TNamedObject.SetUUID(const Value: TUuid);
begin
    if pUuid = NIL then
        New(pUuid);
    pUuid^ := Value;
end;

function TNamedObject.GetUUID(): TUuid;
begin
    if pUuid = NIL then
    begin
        New(pUuid);
        CreateUUID4(pUuid^);
    end;
    Result := pUuid^;
end;

function TNamedObject.GetID(): String;
begin
    Result := GUIDToString(GetUUID());
end;

function TNamedObject.GetCIM_ID(): String;
begin
    Result := UUIDToCIMString(GetUUID());
end;

end.
