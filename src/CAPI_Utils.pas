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

// NOTE: these do not copy to copy old values
procedure DSS_RecreateArray_PByte(var res: PByteArray; var p: PByte; cnt: PInteger; Const incount: Integer);
procedure DSS_RecreateArray_PDouble(var res: PDoubleArray; var p: PDouble; cnt: PInteger; Const incount: Integer);
procedure DSS_RecreateArray_PInteger(var res: PIntegerArray; var p: PInteger; cnt: PInteger; Const incount: Integer);
procedure DSS_RecreateArray_PPAnsiChar(var res: PPAnsiCharArray; var p: PPAnsiChar; cnt: PInteger; Const incount: Integer);
function DSS_RecreateArray_PByte(var p: PByte; cnt: PInteger; Const incount: Integer): PByteArray;
function DSS_RecreateArray_PDouble(var p: PDouble; cnt: PInteger; Const incount: Integer): PDoubleArray;
function DSS_RecreateArray_PInteger(var p: PInteger; cnt: PInteger; Const incount: Integer): PIntegerArray;
function DSS_RecreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PInteger; Const incount: Integer): PPAnsiCharArray;




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

//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
function DSS_CreateArray_PByte(var p: PByte; cnt: PInteger; Const incount: Integer): PByteArray;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(Byte));
    result := PByteArray(p);
end;

function DSS_CreateArray_PDouble(var p: PDouble; cnt: PInteger; Const incount: Integer): PDoubleArray;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(Double));
    result := PDoubleArray(p);
end;

function DSS_CreateArray_PInteger(var p: PInteger; cnt: PInteger; Const incount: Integer): PIntegerArray;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(Integer));
    result := PIntegerArray(p);
end;

function DSS_CreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PInteger; Const incount: Integer): PPAnsiCharArray;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(PAnsiChar));
    result := PPAnsiCharArray(p);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PInteger(var p: PInteger; cnt: PInteger; Const incount: Integer): PIntegerArray;
begin
    if (cnt[1] < incount) then
    begin
        DSS_Dispose_PInteger(p);
        Result := DSS_CreateArray_PInteger(p, cnt, incount);
    end 
    else
    begin
        cnt[0] := incount;
        Result := PIntegerArray(p);
    end;
end;
procedure DSS_RecreateArray_PInteger(var res: PIntegerArray; var p: PInteger; cnt: PInteger; Const incount: Integer);
begin
    res := DSS_RecreateArray_PInteger(p, cnt, incount);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PInteger; Const incount: Integer): PPAnsiCharArray;
begin
    // no size optimization for strings yet
    DSS_Dispose_PPAnsiChar(p, cnt[1]); 
    Result := DSS_CreateArray_PPAnsiChar(p, cnt, incount);
end;
procedure DSS_RecreateArray_PPAnsiChar(var res: PPAnsiCharArray; var p: PPAnsiChar; cnt: PInteger; Const incount: Integer);
begin
    res := DSS_RecreateArray_PPAnsiChar(p, cnt, incount);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PDouble(var p: PDouble; cnt: PInteger; Const incount: Integer): PDoubleArray;
begin
    if (cnt[1] < incount) then
    begin
        DSS_Dispose_PDouble(p);
        Result := DSS_CreateArray_PDouble(p, cnt, incount);
    end 
    else
    begin
        cnt[0] := incount;
        Result := PDoubleArray(p);
    end;
end;
procedure DSS_RecreateArray_PDouble(var res: PDoubleArray; var p: PDouble; cnt: PInteger; Const incount: Integer);
begin
    res := DSS_RecreateArray_PDouble(p, cnt, incount);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PByte(var p: PByte; cnt: PInteger; Const incount: Integer): PByteArray;
begin
    if (cnt[1] < incount) then
    begin
        DSS_Dispose_PByte(p);
        Result := DSS_CreateArray_PByte(p, cnt, incount);
    end 
    else
    begin
        cnt[0] := incount;
        Result := PByteArray(p);
    end;
end;
procedure DSS_RecreateArray_PByte(var res: PByteArray; var p: PByte; cnt: PInteger; Const incount: Integer);
begin
    res := DSS_RecreateArray_PByte(p, cnt, incount);
end;

//------------------------------------------------------------------------------
END.