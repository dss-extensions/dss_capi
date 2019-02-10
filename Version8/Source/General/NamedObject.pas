unit NamedObject;

{
  ----------------------------------------------------------
  Copyright (c) 2009-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

type

    TNamedObject = class(TObject)
    PRIVATE
        PName: String;  // path name, or class name for DSS
        LName: String;  // localName is unique within a class, like the old FName
        DName: String;  // for optional display, does not have to be unique
        pGuid: ^TGuid;

        function Get_QualifiedName: String;
        function Get_DisplayName: String;
        procedure Set_DisplayName(const Value: String);
        function Get_GUID: TGuid;
        function Get_ID: String;
        function Get_CIM_ID: String;
        procedure Set_GUID(const Value: TGUID);
    PUBLIC
        constructor Create(ClassName: String);
        destructor Destroy; OVERRIDE;

        property DSSClassName: String READ PName WRITE PName;
        property LocalName: String READ LName WRITE LName;
        property QualifiedName: String READ Get_QualifiedName;
        property DisplayName: String READ Get_DisplayName WRITE Set_DisplayName;
        property GUID: TGuid READ Get_GUID WRITE Set_GUID;
        property ID: String READ Get_ID;
        property CIM_ID: String READ Get_CIM_ID;
    end;

function GUIDToCIMString(GUID: TGUID): String;

implementation

uses
    Sysutils,
    StrUtils;

function GUIDToCIMString(GUID: TGUID): String;
var
    s: String;
begin
    s := GUIDToString(GuID);
    Result := '_' + MidStr(s, 2, Length(s) - 2);
end;

constructor TNamedObject.Create(ClassName: String);
begin
    inherited Create;
    PName := ClassName;
    LName := '';
    DName := '';
    pGuid := NIL;
end;

destructor TNamedObject.Destroy;
begin
    if pGuid <> NIL then
        Dispose(pGuid);
    inherited Destroy;
end;


procedure TNamedObject.Set_DisplayName(const Value: String);
begin
    DName := Value;
end;

function TNamedObject.Get_DisplayName: String;
begin
    if DName = '' then
        Result := PName + '_' + LName
    else
        Result := DName;
end;

function TNamedObject.Get_QualifiedName: String;
begin
    Result := PName + '.' + LName
end;

procedure TNamedObject.Set_GUID(const Value: TGUID);
begin
    if pGuid = NIL then
        New(pGuid);
    pGuid^ := Value;
end;

function TNamedObject.Get_GUID: TGuid;
begin
    if pGuid = NIL then
    begin
        New(pGuid);
        CreateGuid(pGuid^);
    end;
    Result := pGuid^;
end;

function TNamedObject.Get_ID: String;
begin
    Result := GUIDToString(Get_GUID);
end;

function TNamedObject.Get_CIM_ID: String;
begin
    Result := GUIDToCIMString(Get_GUID);
end;

end.
