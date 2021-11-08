program test;

uses SysUtils;

type
  TUuid = TGuid;    // this is a GUID compliant to RFC 4122, v4
 
function CreateUUID4(out UUID: TUuid):Integer;
begin
	Result := CreateGUID (UUID);
	UUID.D3 := (UUID.D3 and $0fff) or $4000;
	UUID.D4[0] := (UUID.D4[0] and $3f) or $80;
end;

function StringToUUID (const S: string):TUuid;
begin
	Result := StringToGUID (S);
end;

function UUIDToString(const UUID: TUuid):string;
begin
	Result := GuidToString (UUID);
end;

var
	val: TUuid;
begin
	CreateUUID4(val);
{*	writeln(val.D1);
	writeln(val.D2);
	writeln(val.D3);
	writeln(val.D4[0]);
	writeln(val.D4[1]);
	writeln(val.D4[2]);
	writeln(val.D4[3]);
	writeln(val.D4[4]);
	writeln(val.D4[5]);
	writeln(val.D4[6]);
	writeln(val.D4[7]); *}
	writeln(UuidToString(val));
//	val.D3 := (val.D3 and $0fff) or $4000;
//	writeln(Format('D3 = {%0.4X}', [val.D3]));
//	writeln(Format('D4[0] = {%0.2X}', [val.D4[0]]));
//	val.D4[0] := (val.D4[0] and $3f) or $80;
//	writeln(Format('D4[0] = {%0.2X}', [val.D4[0]]));
//	writeln(UuidToString(val));
end.
