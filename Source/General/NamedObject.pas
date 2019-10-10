unit NamedObject;
{
  ----------------------------------------------------------
  Copyright (c) 2009-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

type
	
  TUuid = TGuid;    // this is a GUID compliant to RFC 4122, v4

  TNamedObject = class(TObject)
  private
    PName: String;  // path name, or class name for DSS
    LName: String;  // localName is unique within a class, like the old FName
    DName: String;  // for optional display, does not have to be unique
    pUuid: ^TUuid;  // compliant to RFC 4122, v4

    function Get_QualifiedName: String;
    function Get_DisplayName: String;
    procedure Set_DisplayName(const Value: String);
    function Get_UUID: TUuid;
    function Get_ID: String;
    function Get_CIM_ID: String;
    procedure Set_UUID(const Value: TUuid);
  public
    constructor Create(ClassName:String);
    destructor Destroy; override;

    Property DSSClassName:String Read PName Write PName;
    Property LocalName:String Read LName Write LName;
    Property QualifiedName:String Read Get_QualifiedName;
    Property DisplayName:String Read Get_DisplayName Write Set_DisplayName;
    Property UUID:TUuid Read Get_UUID Write Set_UUID;
    Property ID:String read Get_ID;
    Property CIM_ID:String read Get_CIM_ID;
  end;

function CreateUUID4 (out UUID: TUuid):Integer;
function StringToUUID (const S: string):TUuid;
function UUIDToString(const UUID: TUuid):string;
function UUIDToCIMString (UUID: TUuid): string;

implementation

Uses Sysutils, StrUtils;

function CreateUUID4(out UUID: TUuid):Integer;
begin
	Result := CreateGUID (UUID);
	UUID.D3 := (UUID.D3 and $0fff) or $4000;   // place a 4 at character 13
	UUID.D4[0] := (UUID.D4[0] and $3f) or $80; // character 17 to be 8, 9, A or B
end;

function StringToUUID (const S: string):TUuid;
begin
	Result := StringToGUID (S);
end;

function UUIDToString(const UUID: TUuid):string;
begin
	Result := GuidToString (UUID);
end;

function UUIDToCIMString (UUID: TUuid): string;
var
  s: String;
begin
  s := GUIDToString (UUID);
  Result := '_' + MidStr (s, 2, Length (s)-2);
end;

constructor TNamedObject.Create(ClassName:String);
BEGIN
   Inherited Create;
   PName := ClassName;
   LName := '';
   DName := '';
   pUuid := nil;
END;

destructor TNamedObject.Destroy;
BEGIN
   if pUuid <> nil then Dispose (pUuid);
   Inherited Destroy;
END;


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

procedure TNamedObject.Set_UUID(const Value: TUuid);
begin
  if pUuid = nil then New (pUuid);
  pUuid^ := Value;
end;

function TNamedObject.Get_UUID: TUuid;
begin
  if pUuid = nil then begin
    New (pUuid);
    CreateUUID4 (pUuid^);
  end;
  Result := pUuid^;
end;

function TNamedObject.Get_ID: String;
begin
  Result := GUIDToString (Get_UUID);
end;

function TNamedObject.Get_CIM_ID: String;
begin
  Result := UUIDToCIMString (Get_UUID);
end;

end.
