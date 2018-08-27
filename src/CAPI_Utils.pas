UNIT CAPI_Utils;
{$inline on}

INTERFACE

uses sysutils;

Type
    DoubleArray = Array[0..0] of Double;
    PDoubleArray = ^DoubleArray;
    
    PAnsiCharArray = Array[0..0] of PAnsiChar;
    PPAnsiCharArray = ^PAnsiCharArray;


function DSS_GetAsPAnsiChar(s: AnsiString): PAnsiChar; inline;
procedure DSS_ResetStringBuffer();cdecl;

function DSS_CopyStringAsPChar(s: AnsiString): PAnsiChar; // TODO: check possible memory leaks for := DSS_CopyStringAsPChar('NONE')

procedure DSS_Dispose_PByte(var p: PByte);cdecl;
procedure DSS_Dispose_PDouble(var p: PDouble);cdecl;
procedure DSS_Dispose_PInteger(var p: PInteger);cdecl;
procedure DSS_Dispose_PPAnsiChar(var p: PPAnsiChar; cnt: Integer);cdecl;

function DSS_CreateArray_PByte(var p: PByte; cnt: PInteger; Const incount: Integer): PByteArray;
function DSS_CreateArray_PDouble(var p: PDouble; cnt: PInteger; Const incount: Integer): PDoubleArray;
function DSS_CreateArray_PInteger(var p: PInteger; cnt: PInteger; Const incount: Integer): PIntegerArray;
function DSS_CreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PInteger; Const incount: Integer): PPAnsiCharArray;

// NOTE: these procedures do not copy to copy old values
procedure DSS_RecreateArray_PByte(var res: PByteArray; var p: PByte; cnt: PInteger; Const incount: Integer);
procedure DSS_RecreateArray_PDouble(var res: PDoubleArray; var p: PDouble; cnt: PInteger; Const incount: Integer);
procedure DSS_RecreateArray_PInteger(var res: PIntegerArray; var p: PInteger; cnt: PInteger; Const incount: Integer);
procedure DSS_RecreateArray_PPAnsiChar(var res: PPAnsiCharArray; var p: PPAnsiChar; cnt: PInteger; Const incount: Integer);


IMPLEMENTATION

Var
    tempBuffer: AnsiString;

function DSS_CopyStringAsPChar(s: AnsiString): PAnsiChar;
begin
    result := GetMem(Length(s) * (sizeof(AnsiChar) + 1));
    StrCopy(result, PAnsiChar(s));
end;
    
function DSS_GetAsPAnsiChar(s: AnsiString): PAnsiChar; inline;
begin
    // keep a reference to the string to make sure the memory is not deallocated
    tempBuffer := s;
    result := PAnsiChar(tempBuffer);
end;

procedure DSS_ResetStringBuffer();cdecl;
begin
    tempBuffer := '';
end;

procedure DSS_Dispose_PByte(var p: PByte);cdecl;
begin
    Dispose(p);
    p := nil;
end;

procedure DSS_Dispose_PDouble(var p: PDouble);cdecl;
begin
    Dispose(p);
    p := nil;
end;

procedure DSS_Dispose_PInteger(var p: PInteger);cdecl;
begin
    Dispose(p);
    p := nil;
end;

procedure DSS_Dispose_PPAnsiChar(var p: PPAnsiChar; cnt: Integer);cdecl;
var
    i: Integer;
    tmp: PPAnsiChar;
begin
    tmp := p;
    for i := 0 to (cnt-1) do
    begin
        FreeMem(tmp^);
        inc(tmp);
    end;
    FreeMem(p);
    p := nil;
end;

function DSS_CreateArray_PByte(var p: PByte; cnt: PInteger; Const incount: Integer): PByteArray;
begin
    cnt := incount;
    p := AllocMem(incount * sizeof(Byte));
    result := PByteArray(p);
end;

function DSS_CreateArray_PDouble(var p: PDouble; cnt: PInteger; Const incount: Integer): PDoubleArray;
begin
    if ((cnt <> 0) and (p <> nil)) then
    begin
        DSS_RecreateArray_PDouble(result, p, cnt, incount);
    end
    else 
    begin
        cnt := incount;
        p := AllocMem(incount * sizeof(Double));
        result := PDoubleArray(p);
    end;
end;

function DSS_CreateArray_PInteger(var p: PInteger; cnt: PInteger; Const incount: Integer): PIntegerArray;
begin
    cnt := incount;
    p := AllocMem(incount * sizeof(Integer));
    result := PIntegerArray(p);
end;

function DSS_CreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PInteger; Const incount: Integer): PPAnsiCharArray;
begin
    cnt := incount;
    p := AllocMem(incount * sizeof(PAnsiChar));
    result := PPAnsiCharArray(p);
end;


procedure DSS_RecreateArray_PByte(var res: PByteArray; var p: PByte; cnt: PInteger; Const incount: Integer);
begin
    DSS_Dispose_PByte(p);
    res := DSS_CreateArray_PByte(p, cnt, incount);
end;

procedure DSS_RecreateArray_PDouble(var res: PDoubleArray; var p: PDouble; cnt: PInteger; Const incount: Integer);
begin
    if ((p = nil) or (incount > cnt)) then // only resize if we actually need to
    begin
        DSS_Dispose_PDouble(p);
        res := DSS_CreateArray_PDouble(p, cnt, incount);
    end
    else
    begin
        res := PDoubleArray(p);
    end;
end;

procedure DSS_RecreateArray_PInteger(var res: PIntegerArray; var p: PInteger; cnt: PInteger; Const incount: Integer);
begin
    DSS_Dispose_PInteger(p);
    res := DSS_CreateArray_PInteger(p, cnt, incount);
end;

procedure DSS_RecreateArray_PPAnsiChar(var res: PPAnsiCharArray; var p: PPAnsiChar; cnt: PInteger; Const incount: Integer);
begin
    DSS_Dispose_PPAnsiChar(p, cnt[1]); 
    res := DSS_CreateArray_PPAnsiChar(p, cnt, incount);
end;

END.