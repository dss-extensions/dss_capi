unit CAPI_Utils;

{$inline on}

interface

uses
    sysutils;

type
    DoubleArray = array[0..0] of Double;
    PDoubleArray = ^DoubleArray;

    PAnsiCharArray = array[0..0] of PAnsiChar;
    PPAnsiCharArray = ^PAnsiCharArray;

    PPDouble = ^PDouble;
    PPInteger = ^PInteger;
    PPByte = ^PByte;
    PPPAnsiChar = ^PPAnsiChar;

var
    GR_DataPtr_PPAnsiChar: PPAnsiChar;
    GR_DataPtr_PDouble: PDouble;
    GR_DataPtr_PInteger: PInteger;
    GR_DataPtr_PByte: PByte;

    GR_CountPtr_PPAnsiChar: PInteger;
    GR_CountPtr_PDouble: PInteger;
    GR_CountPtr_PInteger: PInteger;
    GR_CountPtr_PByte: PInteger;

procedure DSS_GetGRPointers(
    // Pointers to the global variables that contains the actual pointers.
    var DataPtr_PPAnsiChar: PPPAnsiChar;
    var DataPtr_PDouble: PPDouble;
    var DataPtr_PInteger: PPInteger;
    var DataPtr_PByte: PPByte;

    // These are not reallocated during the execution, can return the actual pointer values
    var CountPtr_PPAnsiChar: PInteger;
    var CountPtr_PDouble: PInteger;
    var CountPtr_PInteger: PInteger;
    var CountPtr_PByte: PInteger); CDECL;
procedure DSS_DisposeGRData(); CDECL;

function DSS_GetAsPAnsiChar(s: Ansistring): PAnsiChar; inline;
procedure DSS_ResetStringBuffer(); CDECL;

function DSS_CopyStringAsPChar(s: Ansistring): PAnsiChar; // TODO: check possible memory leaks for := DSS_CopyStringAsPChar('NONE')

procedure DSS_Dispose_PByte(var p: PByte); CDECL;
procedure DSS_Dispose_PDouble(var p: PDouble); CDECL;
procedure DSS_Dispose_PInteger(var p: PInteger); CDECL;
procedure DSS_Dispose_PPAnsiChar(var p: PPAnsiChar; cnt: Integer); CDECL;

function DSS_CreateArray_PByte(var p: PByte; cnt: PInteger; const incount: Integer): PByteArray;
function DSS_CreateArray_PDouble(var p: PDouble; cnt: PInteger; const incount: Integer): PDoubleArray;
function DSS_CreateArray_PInteger(var p: PInteger; cnt: PInteger; const incount: Integer): PIntegerArray;
function DSS_CreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PInteger; const incount: Integer): PPAnsiCharArray;

// NOTE: these do not copy to copy old values
procedure DSS_RecreateArray_PByte(var res: PByteArray; var p: PByte; cnt: PInteger; const incount: Integer);
procedure DSS_RecreateArray_PDouble(var res: PDoubleArray; var p: PDouble; cnt: PInteger; const incount: Integer);
procedure DSS_RecreateArray_PInteger(var res: PIntegerArray; var p: PInteger; cnt: PInteger; const incount: Integer);
procedure DSS_RecreateArray_PPAnsiChar(var res: PPAnsiCharArray; var p: PPAnsiChar; cnt: PInteger; const incount: Integer);
function DSS_RecreateArray_PByte(var p: PByte; cnt: PInteger; const incount: Integer): PByteArray;
function DSS_RecreateArray_PDouble(var p: PDouble; cnt: PInteger; const incount: Integer): PDoubleArray;
function DSS_RecreateArray_PInteger(var p: PInteger; cnt: PInteger; const incount: Integer): PIntegerArray;
function DSS_RecreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PInteger; const incount: Integer): PPAnsiCharArray;

// MATLAB doesn't handle pointers that well,
// this just gets a single string from the pointer of strings
function DSS_Get_PAnsiChar(var p: Pointer; Index: Integer): PAnsiChar; CDECL;

implementation

var
    tempBuffer: Ansistring;

function DSS_CopyStringAsPChar(s: Ansistring): PAnsiChar;
begin
    result := GetMem(Length(s) * (sizeof(AnsiChar) + 1));
    StrCopy(result, PAnsiChar(s));
end;

function DSS_GetAsPAnsiChar(s: Ansistring): PAnsiChar; inline;
begin
    // keep a reference to the string to make sure the memory is not deallocated
    tempBuffer := s;
    result := PAnsiChar(tempBuffer);
end;

procedure DSS_ResetStringBuffer(); CDECL;
begin
    tempBuffer := '';
end;

//------------------------------------------------------------------------------
procedure DSS_Dispose_PByte(var p: PByte); CDECL;
begin
    Dispose(p);
    p := NIL;
end;

procedure DSS_Dispose_PDouble(var p: PDouble); CDECL;
begin
    Dispose(p);
    p := NIL;
end;

procedure DSS_Dispose_PInteger(var p: PInteger); CDECL;
begin
    Dispose(p);
    p := NIL;
end;

procedure DSS_Dispose_PPAnsiChar(var p: PPAnsiChar; cnt: Integer); CDECL;
var
    i: Integer;
    tmp: PPAnsiChar;
begin
    tmp := p;
    for i := 0 to (cnt - 1) do
    begin
        FreeMem(tmp^);
        inc(tmp);
    end;
    FreeMem(p);
    p := NIL;
end;

function DSS_Get_PAnsiChar(var p: Pointer; Index: Integer): PAnsiChar; CDECL;
begin
    result := PPAnsiChar(p)[Index];
end;

//------------------------------------------------------------------------------
function DSS_CreateArray_PByte(var p: PByte; cnt: PInteger; const incount: Integer): PByteArray;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(Byte));
    result := PByteArray(p);
end;

function DSS_CreateArray_PDouble(var p: PDouble; cnt: PInteger; const incount: Integer): PDoubleArray;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(Double));
    result := PDoubleArray(p);
end;

function DSS_CreateArray_PInteger(var p: PInteger; cnt: PInteger; const incount: Integer): PIntegerArray;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(Integer));
    result := PIntegerArray(p);
end;

function DSS_CreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PInteger; const incount: Integer): PPAnsiCharArray;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(PAnsiChar));
    result := PPAnsiCharArray(p);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PInteger(var p: PInteger; cnt: PInteger; const incount: Integer): PIntegerArray;
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
        FillByte(Result^, incount * sizeof(Integer), 0); // needs to zero it for compatibility
    end;
end;

procedure DSS_RecreateArray_PInteger(var res: PIntegerArray; var p: PInteger; cnt: PInteger; const incount: Integer);
begin
    res := DSS_RecreateArray_PInteger(p, cnt, incount);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PInteger; const incount: Integer): PPAnsiCharArray;
begin
    // no size optimization for strings yet
    DSS_Dispose_PPAnsiChar(p, cnt[1]);
    Result := DSS_CreateArray_PPAnsiChar(p, cnt, incount);
end;

procedure DSS_RecreateArray_PPAnsiChar(var res: PPAnsiCharArray; var p: PPAnsiChar; cnt: PInteger; const incount: Integer);
begin
    res := DSS_RecreateArray_PPAnsiChar(p, cnt, incount);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PDouble(var p: PDouble; cnt: PInteger; const incount: Integer): PDoubleArray;
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
        FillByte(Result^, incount * sizeof(Double), 0); // needs to zero it for compatibility
    end;
end;

procedure DSS_RecreateArray_PDouble(var res: PDoubleArray; var p: PDouble; cnt: PInteger; const incount: Integer);
begin
    res := DSS_RecreateArray_PDouble(p, cnt, incount);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PByte(var p: PByte; cnt: PInteger; const incount: Integer): PByteArray;
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
        FillByte(Result^, incount * sizeof(Byte), 0); // needs to zero it for compatibility
    end;
end;

procedure DSS_RecreateArray_PByte(var res: PByteArray; var p: PByte; cnt: PInteger; const incount: Integer);
begin
    res := DSS_RecreateArray_PByte(p, cnt, incount);
end;

//------------------------------------------------------------------------------
procedure DSS_GetGRPointers(
    var DataPtr_PPAnsiChar: PPPAnsiChar;
    var DataPtr_PDouble: PPDouble;
    var DataPtr_PInteger: PPInteger;
    var DataPtr_PByte: PPByte;
    var CountPtr_PPAnsiChar: PInteger;
    var CountPtr_PDouble: PInteger;
    var CountPtr_PInteger: PInteger;
    var CountPtr_PByte: PInteger); CDECL;
begin
    DataPtr_PPAnsiChar := @GR_DataPtr_PPAnsiChar;
    DataPtr_PDouble := @GR_DataPtr_PDouble;
    DataPtr_PInteger := @GR_DataPtr_PInteger;
    DataPtr_PByte := @GR_DataPtr_PByte;
    CountPtr_PPAnsiChar := GR_CountPtr_PPAnsiChar;
    CountPtr_PDouble := GR_CountPtr_PDouble;
    CountPtr_PInteger := GR_CountPtr_PInteger;
    CountPtr_PByte := GR_CountPtr_PByte;
end;
//------------------------------------------------------------------------------
procedure DSS_DisposeGRData(); CDECL;
begin
    DSS_Dispose_PByte(GR_DataPtr_PByte);
    DSS_Dispose_PDouble(GR_DataPtr_PDouble);
    DSS_Dispose_PInteger(GR_DataPtr_PInteger);
    DSS_Dispose_PPAnsiChar(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar[1]);

    GR_CountPtr_PPAnsiChar[0] := 0;
    GR_CountPtr_PDouble[0] := 0;
    GR_CountPtr_PInteger[0] := 0;
    GR_CountPtr_PByte[0] := 0;

    GR_CountPtr_PPAnsiChar[1] := 0;
    GR_CountPtr_PDouble[1] := 0;
    GR_CountPtr_PInteger[1] := 0;
    GR_CountPtr_PByte[1] := 0;
end;
//------------------------------------------------------------------------------
initialization
    // allocate (and initialize to zero) the global result pointers
    GR_CountPtr_PPAnsiChar := AllocMem(sizeof(Integer) * 2);
    GR_CountPtr_PDouble := AllocMem(sizeof(Integer) * 2);
    GR_CountPtr_PInteger := AllocMem(sizeof(Integer) * 2);
    GR_CountPtr_PByte := AllocMem(sizeof(Integer) * 2);

finalization
    Dispose(GR_CountPtr_PPAnsiChar);
    Dispose(GR_CountPtr_PDouble);
    Dispose(GR_CountPtr_PInteger);
    Dispose(GR_CountPtr_PByte);
end.
