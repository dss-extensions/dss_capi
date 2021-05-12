unit CAPI_Utils;

{$mode objfpc}

interface

uses
    sysutils,
    DSSPointerList;

type
    DoubleArray = array[0..$effffff] of Double;
    PDoubleArray = ^DoubleArray;

    PAnsiCharArray = array[0..$effffff] of PAnsiChar;
    PPAnsiCharArray = ^PAnsiCharArray;

    PPDouble = ^PDouble;
    PPInteger = ^PInteger;
    PPByte = ^PByte;
    PPPAnsiChar = ^PPAnsiChar;

    Float32 = Single;
    Float32Array = array[0..$effffff] of Float32;
    PFloat32Array = ^Float32Array;
    PFloat32 = ^Float32;
    PPFloat32 = ^PFloat32;

    SingleArray = Float32Array;
    PSingleArray = PFloat32Array;
    PSingle = PFloat32;
    PPSingle = PPFloat32;
    
    // TODO: for 0.13, update to Int64 and Boolean
    TAPISize = Int32;
    PAPISize = ^Int32;
    TAPIBoolean = WordBool;
    

var
    GR_DataPtr_PPAnsiChar: PPAnsiChar;
    GR_DataPtr_PDouble: PDouble;
    GR_DataPtr_PInteger: PInteger;
    GR_DataPtr_PByte: PByte;

    GR_CountPtr_PPAnsiChar: PAPISize;
    GR_CountPtr_PDouble: PAPISize;
    GR_CountPtr_PInteger: PAPISize;
    GR_CountPtr_PByte: PAPISize;

procedure DSS_GetGRPointers(
    // Pointers to the global variables that contains the actual pointers.
    var DataPtr_PPAnsiChar: PPPAnsiChar;
    var DataPtr_PDouble: PPDouble;
    var DataPtr_PInteger: PPInteger;
    var DataPtr_PByte: PPByte;

    // These are not reallocated during the execution, can return the actual pointer values
    var CountPtr_PPAnsiChar: PAPISize;
    var CountPtr_PDouble: PAPISize;
    var CountPtr_PInteger: PAPISize;
    var CountPtr_PByte: PAPISize); CDECL;


// Separate simple functions for MATLAB, which return the current pointer
// directly, instead of a pointer to pointer.
function DSS_GR_DataPtr_PDouble(): PDouble; CDECL;
function DSS_GR_DataPtr_PInteger(): PInteger; CDECL;
function DSS_GR_DataPtr_PByte(): PByte; CDECL;
function DSS_GR_CountPtr_PDouble(): PAPISize; CDECL;
function DSS_GR_CountPtr_PInteger(): PAPISize; CDECL;
function DSS_GR_CountPtr_PByte(): PAPISize; CDECL;

procedure DSS_DisposeGRData(); CDECL;

function DSS_GetAsPAnsiChar(s: Ansistring): PAnsiChar;
procedure DSS_ResetStringBuffer(); CDECL;

function DSS_CopyStringAsPChar(s: Ansistring): PAnsiChar; // TODO: check possible memory leaks for := DSS_CopyStringAsPChar('NONE')

procedure DSS_Dispose_PByte(var p: PByte); CDECL;
procedure DSS_Dispose_PSingle(var p: PSingle); CDECL;
procedure DSS_Dispose_PDouble(var p: PDouble); CDECL;
procedure DSS_Dispose_PInteger(var p: PInteger); CDECL;
procedure DSS_Dispose_PPAnsiChar(var p: PPAnsiChar; cnt: TAPISize); CDECL;

function DSS_CreateArray_PByte(var p: PByte; cnt: PAPISize; const incount: TAPISize): PByteArray;
function DSS_CreateArray_PSingle(var p: PSingle; cnt: PAPISize; const incount: TAPISize): PSingleArray;
function DSS_CreateArray_PDouble(var p: PDouble; cnt: PAPISize; const incount: TAPISize): PDoubleArray;
function DSS_CreateArray_PInteger(var p: PInteger; cnt: PAPISize; const incount: TAPISize): PIntegerArray;
function DSS_CreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PAPISize; const incount: TAPISize): PPAnsiCharArray;

// NOTE: these do not copy to copy old values
procedure DSS_RecreateArray_PByte(var res: PByteArray; var p: PByte; cnt: PAPISize; const incount: TAPISize);
procedure DSS_RecreateArray_PSingle(var res: PSingleArray; var p: PSingle; cnt: PAPISize; const incount: TAPISize);
procedure DSS_RecreateArray_PDouble(var res: PDoubleArray; var p: PDouble; cnt: PAPISize; const incount: TAPISize);
procedure DSS_RecreateArray_PInteger(var res: PIntegerArray; var p: PInteger; cnt: PAPISize; const incount: TAPISize);
procedure DSS_RecreateArray_PPAnsiChar(var res: PPAnsiCharArray; var p: PPAnsiChar; cnt: PAPISize; const incount: TAPISize);
function DSS_RecreateArray_PByte(var p: PByte; cnt: PAPISize; const incount: TAPISize): PByteArray;
function DSS_RecreateArray_PSingle(var p: PSingle; cnt: PAPISize; const incount: TAPISize): PSingleArray;
function DSS_RecreateArray_PDouble(var p: PDouble; cnt: PAPISize; const incount: TAPISize): PDoubleArray;
function DSS_RecreateArray_PInteger(var p: PInteger; cnt: PAPISize; const incount: TAPISize): PIntegerArray;
function DSS_RecreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PAPISize; const incount: TAPISize): PPAnsiCharArray;

// MATLAB doesn't handle pointers that well,
// this just gets a single string from the pointer of strings
function DSS_Get_PAnsiChar(var p: Pointer; Index: TAPISize): PAnsiChar; CDECL;

procedure Generic_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; pList: TDSSPointerList; const Restore: Boolean); inline;

function InvalidCircuit(): Boolean; inline;
function MissingSolution(): Boolean; inline;

procedure DefaultResult(var ResultPtr: PByte; ResultCount: PAPISize; Value: Byte = 0); overload; inline;
procedure DefaultResult(var ResultPtr: PInteger; ResultCount: PAPISize; Value: Integer = 0); overload; inline;
procedure DefaultResult(var ResultPtr: PDouble; ResultCount: PAPISize; Value: Double = 0); overload; inline;
procedure DefaultResult(var ResultPtr: PSingle; ResultCount: PAPISize; Value: Single = 0); overload; inline;
procedure DefaultResult(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; Value: String = 'NONE'); overload; inline;

implementation

Uses DSSObject, DSSGlobals;

var
    tempBuffer: Ansistring;
    
//------------------------------------------------------------------------------
procedure DefaultResult(var ResultPtr: PByte; ResultCount: PAPISize; Value: Byte = 0); overload; inline;
begin
    if not DSS_CAPI_COM_DEFAULTS then
    begin
        DSS_RecreateArray_PByte(ResultPtr, ResultCount, 0);
        Exit;
    end;
    DSS_RecreateArray_PByte(ResultPtr, ResultCount, 1);
    ResultPtr^ := Value;
end;
//------------------------------------------------------------------------------
procedure DefaultResult(var ResultPtr: PInteger; ResultCount: PAPISize; Value: Integer = 0); overload; inline;
begin
    if not DSS_CAPI_COM_DEFAULTS then
    begin
        DSS_RecreateArray_PInteger(ResultPtr, ResultCount, 0);
        Exit;
    end;
    DSS_RecreateArray_PInteger(ResultPtr, ResultCount, 1);
    ResultPtr^ := Value;
end;
//------------------------------------------------------------------------------
procedure DefaultResult(var ResultPtr: PDouble; ResultCount: PAPISize; Value: Double = 0); overload; inline;
begin
    if not DSS_CAPI_COM_DEFAULTS then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 0);
        Exit;
    end;
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
    ResultPtr^ := Value;
end;
//------------------------------------------------------------------------------
procedure DefaultResult(var ResultPtr: PSingle; ResultCount: PAPISize; Value: Single = 0); overload; inline;
begin
    if not DSS_CAPI_COM_DEFAULTS then
    begin
        DSS_RecreateArray_PSingle(ResultPtr, ResultCount, 0);
        Exit;
    end;
    DSS_RecreateArray_PSingle(ResultPtr, ResultCount, 1);
    ResultPtr^ := Value;
end;
//------------------------------------------------------------------------------
procedure DefaultResult(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; Value: String = 'NONE'); overload; inline;
begin
    if not DSS_CAPI_COM_DEFAULTS then
    begin
        DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 0);
        Exit;
    end;
    DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    ResultPtr^ := DSS_CopyStringAsPChar(Value);
end;
//------------------------------------------------------------------------------
function InvalidCircuit(): Boolean; inline;
begin
    if ActiveCircuit = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('There is no active circuit! Create a circuit and retry.', 8888);
        end;
        Result := True;
        Exit;
    end;
    Result := False;
end;
//------------------------------------------------------------------------------
function MissingSolution(): Boolean; inline;
begin
    Result := InvalidCircuit;
    if Result then
        Exit;
        
    if ActiveCircuit.Solution.NodeV = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('Solution state is not initialized for the active circuit!', 8899);
        end;
        Result := True;
        Exit;
    end;
    Result := False;
end;
//------------------------------------------------------------------------------
function DSS_CopyStringAsPChar(s: Ansistring): PAnsiChar;
begin
    result := GetMem(Length(s) * (sizeof(AnsiChar) + 1));
    StrPCopy(result, s);
end;
//------------------------------------------------------------------------------
function DSS_GetAsPAnsiChar(s: Ansistring): PAnsiChar;
begin
    // keep a reference to the string to make sure the memory is not deallocated
    tempBuffer := s;
    result := PAnsiChar(tempBuffer);
end;
//------------------------------------------------------------------------------
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

procedure DSS_Dispose_PSingle(var p: PSingle); CDECL;
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

procedure DSS_Dispose_PPAnsiChar(var p: PPAnsiChar; cnt: TAPISize); CDECL;
var
    i: TAPISize;
    tmp: PPAnsiChar;
begin
    tmp := p;
    i := 0;
    while i < cnt do
    begin
        FreeMem(tmp^);
        inc(tmp);
        inc(i);
    end;
    FreeMem(p);
    p := NIL;
end;

function DSS_Get_PAnsiChar(var p: Pointer; Index: TAPISize): PAnsiChar; CDECL;
begin
    result := PPAnsiChar(p)[Index];
end;

//------------------------------------------------------------------------------
function DSS_CreateArray_PByte(var p: PByte; cnt: PAPISize; const incount: TAPISize): PByteArray;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(Byte));
    result := PByteArray(p);
end;

function DSS_CreateArray_PSingle(var p: PSingle; cnt: PAPISize; const incount: TAPISize): PSingleArray;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(Single));
    result := PSingleArray(p);
end;

function DSS_CreateArray_PDouble(var p: PDouble; cnt: PAPISize; const incount: TAPISize): PDoubleArray;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(Double));
    result := PDoubleArray(p);
end;

function DSS_CreateArray_PInteger(var p: PInteger; cnt: PAPISize; const incount: TAPISize): PIntegerArray;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(Integer));
    result := PIntegerArray(p);
end;

function DSS_CreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PAPISize; const incount: TAPISize): PPAnsiCharArray;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(PAnsiChar));
    result := PPAnsiCharArray(p);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PInteger(var p: PInteger; cnt: PAPISize; const incount: TAPISize): PIntegerArray;
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

procedure DSS_RecreateArray_PInteger(var res: PIntegerArray; var p: PInteger; cnt: PAPISize; const incount: TAPISize);
begin
    res := DSS_RecreateArray_PInteger(p, cnt, incount);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PAPISize; const incount: TAPISize): PPAnsiCharArray;
begin
    // no size optimization for strings yet
    DSS_Dispose_PPAnsiChar(p, cnt[1]);
    Result := DSS_CreateArray_PPAnsiChar(p, cnt, incount);
end;

procedure DSS_RecreateArray_PPAnsiChar(var res: PPAnsiCharArray; var p: PPAnsiChar; cnt: PAPISize; const incount: TAPISize);
begin
    res := DSS_RecreateArray_PPAnsiChar(p, cnt, incount);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PSingle(var p: PSingle; cnt: PAPISize; const incount: TAPISize): PSingleArray;
begin
    if (cnt[1] < incount) then
    begin
        DSS_Dispose_PSingle(p);
        Result := DSS_CreateArray_PSingle(p, cnt, incount);
    end
    else
    begin
        cnt[0] := incount;
        Result := PSingleArray(p);
        FillByte(Result^, incount * sizeof(Single), 0); // needs to zero it for compatibility
    end;
end;

procedure DSS_RecreateArray_PSingle(var res: PSingleArray; var p: PSingle; cnt: PAPISize; const incount: TAPISize);
begin
    res := DSS_RecreateArray_PSingle(p, cnt, incount);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PDouble(var p: PDouble; cnt: PAPISize; const incount: TAPISize): PDoubleArray;
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

procedure DSS_RecreateArray_PDouble(var res: PDoubleArray; var p: PDouble; cnt: PAPISize; const incount: TAPISize);
begin
    res := DSS_RecreateArray_PDouble(p, cnt, incount);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PByte(var p: PByte; cnt: PAPISize; const incount: TAPISize): PByteArray;
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

procedure DSS_RecreateArray_PByte(var res: PByteArray; var p: PByte; cnt: PAPISize; const incount: TAPISize);
begin
    res := DSS_RecreateArray_PByte(p, cnt, incount);
end;

//------------------------------------------------------------------------------
procedure DSS_GetGRPointers(
    var DataPtr_PPAnsiChar: PPPAnsiChar;
    var DataPtr_PDouble: PPDouble;
    var DataPtr_PInteger: PPInteger;
    var DataPtr_PByte: PPByte;
    var CountPtr_PPAnsiChar: PAPISize;
    var CountPtr_PDouble: PAPISize;
    var CountPtr_PInteger: PAPISize;
    var CountPtr_PByte: PAPISize); CDECL;
begin
    if (@DataPtr_PPAnsiChar <> nil) then DataPtr_PPAnsiChar := @GR_DataPtr_PPAnsiChar;
    if (@DataPtr_PDouble <> nil) then DataPtr_PDouble := @GR_DataPtr_PDouble;
    if (@DataPtr_PInteger <> nil) then DataPtr_PInteger := @GR_DataPtr_PInteger;
    if (@DataPtr_PByte <> nil) then DataPtr_PByte := @GR_DataPtr_PByte;
    if (@CountPtr_PPAnsiChar <> nil) then CountPtr_PPAnsiChar := GR_CountPtr_PPAnsiChar;
    if (@CountPtr_PDouble <> nil) then CountPtr_PDouble := GR_CountPtr_PDouble;
    if (@CountPtr_PInteger <> nil) then CountPtr_PInteger := GR_CountPtr_PInteger;
    if (@CountPtr_PByte <> nil) then CountPtr_PByte := GR_CountPtr_PByte;
end;
//------------------------------------------------------------------------------
// Separate simple functions for MATLAB, which return the current pointer
// directly, instead of a pointer to pointer.

function DSS_GR_DataPtr_PDouble(): PDouble; CDECL;
begin
    Result := GR_DataPtr_PDouble;
end;
function DSS_GR_DataPtr_PInteger(): PInteger; CDECL;
begin
    Result := GR_DataPtr_PInteger;
end;
function DSS_GR_DataPtr_PByte(): PByte; CDECL;
begin
    Result := GR_DataPtr_PByte;
end;
function DSS_GR_CountPtr_PDouble(): PAPISize; CDECL;
begin
    Result := GR_CountPtr_PDouble;
end;
function DSS_GR_CountPtr_PInteger(): PAPISize; CDECL;
begin
    Result := GR_CountPtr_PInteger;
end;
function DSS_GR_CountPtr_PByte(): PAPISize; CDECL;
begin
    Result := GR_CountPtr_PByte;
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
procedure Generic_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; pList: TDSSPointerList; const Restore: Boolean); inline;
var
    Result: PPAnsiCharArray;
    idx_before, k: Integer;
    elem: TDSSObject;
begin
    if pList.Count <= 0 then
        Exit;
    DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, pList.Count);
    idx_before := pList.ActiveIndex;
    k := 0;
    elem := TDSSObject(pList.First);
    while elem <> NIL do
    begin
        ResultPtr[k] := DSS_CopyStringAsPChar(elem.Name);
        Inc(k);
        elem := TDSSObject(pList.Next);
    end;
    if Restore and ((idx_before > 0) and (idx_before <= pList.Count)) then 
        pList.Get(idx_before);
end;
//------------------------------------------------------------------------------
initialization
    // allocate (and initialize to zero) the global result pointers
    GR_CountPtr_PPAnsiChar := AllocMem(sizeof(TAPISize) * 2);
    GR_CountPtr_PDouble := AllocMem(sizeof(TAPISize) * 2);
    GR_CountPtr_PInteger := AllocMem(sizeof(TAPISize) * 2);
    GR_CountPtr_PByte := AllocMem(sizeof(TAPISize) * 2);

finalization
    Dispose(GR_CountPtr_PPAnsiChar);
    Dispose(GR_CountPtr_PDouble);
    Dispose(GR_CountPtr_PInteger);
    Dispose(GR_CountPtr_PByte);
end.
