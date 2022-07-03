unit CAPI_Utils;

{$mode objfpc}

interface

uses
    sysutils,
    DSSPointerList,
    DSSClass,
    CAPI_Types;

type
    TDSSContext = DSSClass.TDSSContext;

procedure ctx_DSS_GetGRPointers(
    DSS: TDSSContext;

    // Pointers to the global variables that contains the actual pointers.
    var DataPtr_PPAnsiChar: PPPAnsiChar;
    var DataPtr_PDouble: PPDouble;
    var DataPtr_PInteger: PPInteger;
    var DataPtr_PByte: PPByte;

    // These are not reallocated during the execution, can return the actual pointer values
    var CountPtr_PPAnsiChar: PAPISize;
    var CountPtr_PDouble: PAPISize;
    var CountPtr_PInteger: PAPISize;
    var CountPtr_PByte: PAPISize
); CDECL;

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
    var CountPtr_PByte: PAPISize
); CDECL;

// Separate simple functions for MATLAB, which return the current pointer
// directly, instead of a pointer to pointer.
function ctx_DSS_GR_DataPtr_PDouble(DSS: TDSSContext): PDouble; CDECL;
function ctx_DSS_GR_DataPtr_PInteger(DSS: TDSSContext): PInteger; CDECL;
function ctx_DSS_GR_DataPtr_PByte(DSS: TDSSContext): PByte; CDECL;
function ctx_DSS_GR_CountPtr_PDouble(DSS: TDSSContext): PAPISize; CDECL;
function ctx_DSS_GR_CountPtr_PInteger(DSS: TDSSContext): PAPISize; CDECL;
function ctx_DSS_GR_CountPtr_PByte(DSS: TDSSContext): PAPISize; CDECL;
function DSS_GR_DataPtr_PDouble(): PDouble; CDECL;
function DSS_GR_DataPtr_PInteger(): PInteger; CDECL;
function DSS_GR_DataPtr_PByte(): PByte; CDECL;
function DSS_GR_CountPtr_PDouble(): PAPISize; CDECL;
function DSS_GR_CountPtr_PInteger(): PAPISize; CDECL;
function DSS_GR_CountPtr_PByte(): PAPISize; CDECL;

procedure ctx_DSS_DisposeGRData(DSS: TDSSContext); CDECL;
procedure DSS_DisposeGRData(); CDECL;

function DSS_GetAsPAnsiChar(DSS: TDSSContext; s: Ansistring): PAnsiChar;

procedure ctx_DSS_ResetStringBuffer(DSS: TDSSContext); CDECL;
procedure DSS_ResetStringBuffer(); CDECL;

function DSS_CopyStringAsPChar(s: Ansistring): PAnsiChar; // TODO: check possible memory leaks for := DSS_CopyStringAsPChar('NONE')

procedure DSS_Dispose_PByte(var p: PByte); CDECL;
procedure DSS_Dispose_PSingle(var p: PSingle); CDECL;
procedure DSS_Dispose_PDouble(var p: PDouble); CDECL;
procedure DSS_Dispose_PInteger(var p: PInteger); CDECL;
procedure DSS_Dispose_PPAnsiChar(var p: PPAnsiChar; cnt: TAPISize); CDECL;
procedure DSS_Dispose_PPointer(var p: PPointer); CDECL;

function DSS_CreateArray_PByte(var p: PByte; cnt: PAPISize; const incount: TAPISize): PByteArray;
function DSS_CreateArray_PSingle(var p: PSingle; cnt: PAPISize; const incount: TAPISize): PSingleArray0;
function DSS_CreateArray_PDouble(var p: PDouble; cnt: PAPISize; const incount: TAPISize): PDoubleArray0;
function DSS_CreateArray_PInteger(var p: PInteger; cnt: PAPISize; const incount: TAPISize): PIntegerArray0;
function DSS_CreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PAPISize; const incount: TAPISize): PPAnsiCharArray0;
function DSS_CreateArray_PPointer(var p: PPointer; cnt: PAPISize; const incount: TAPISize): PPointerArray0;


// NOTE: these do not copy to copy old values
procedure DSS_RecreateArray_PByte(var res: PByteArray; var p: PByte; cnt: PAPISize; const incount: TAPISize);
procedure DSS_RecreateArray_PSingle(var res: PSingleArray0; var p: PSingle; cnt: PAPISize; const incount: TAPISize);
procedure DSS_RecreateArray_PDouble(var res: PDoubleArray0; var p: PDouble; cnt: PAPISize; const incount: TAPISize);
procedure DSS_RecreateArray_PInteger(var res: PIntegerArray0; var p: PInteger; cnt: PAPISize; const incount: TAPISize);
procedure DSS_RecreateArray_PPAnsiChar(var res: PPAnsiCharArray0; var p: PPAnsiChar; cnt: PAPISize; const incount: TAPISize);
procedure DSS_RecreateArray_PPointer(var res: PPointerArray0; var p: PPointer; cnt: PAPISize; const incount: TAPISize);
function DSS_RecreateArray_PByte(var p: PByte; cnt: PAPISize; const incount: TAPISize): PByteArray;
function DSS_RecreateArray_PSingle(var p: PSingle; cnt: PAPISize; const incount: TAPISize): PSingleArray0;
function DSS_RecreateArray_PDouble(var p: PDouble; cnt: PAPISize; const incount: TAPISize): PDoubleArray0;
function DSS_RecreateArray_PInteger(var p: PInteger; cnt: PAPISize; const incount: TAPISize): PIntegerArray0;
function DSS_RecreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PAPISize; const incount: TAPISize): PPAnsiCharArray0;
function DSS_RecreateArray_PPointer(var p: PPointer; cnt: PAPISize; const incount: TAPISize): PPointerArray0;
// MATLAB doesn't handle pointers that well,
// this just gets a single string from the pointer of strings
function DSS_Get_PAnsiChar(var p: Pointer; Index: TAPISize): PAnsiChar; CDECL;

procedure Generic_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; pList: TDSSPointerList; const Restore: Boolean); inline;
function Generic_CktElement_Get_First(DSS: TDSSContext; pList: TDSSPointerList): Integer; inline;
function Generic_CktElement_Get_Next(DSS: TDSSContext; pList: TDSSPointerList): Integer; inline;

function InvalidCircuit(DSS: TDSSContext): Boolean; inline;
function MissingSolution(DSS: TDSSContext): Boolean; inline;

procedure DefaultResult(var ResultPtr: PByte; ResultCount: PAPISize; Value: Byte = 0); overload; inline;
procedure DefaultResult(var ResultPtr: PInteger; ResultCount: PAPISize; Value: Integer = 0); overload; inline;
procedure DefaultResult(var ResultPtr: PDouble; ResultCount: PAPISize; Value: Double = 0); overload; inline;
procedure DefaultResult(var ResultPtr: PSingle; ResultCount: PAPISize; Value: Single = 0); overload; inline;
procedure DefaultResult(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; Value: String = 'NONE'); overload; inline;

function DSS_BeginPascalThread(func: Pointer; paramptr: Pointer): PtrUInt; CDECL;
procedure DSS_WaitPascalThread(handle: PtrUInt); CDECL;

implementation

Uses 
    DSSObject, 
    DSSGlobals, 
    CktElement, 
    DSSHelper;

type
    TCDECLThreadFunc = function (user_data: Pointer): Pointer; CDECL;
    PCDECLThreadFuncData = ^TCDECLThreadFuncData;

    TCDECLThreadFuncData = record
        Func: TCDECLThreadFunc;
        Data: Pointer;
    end;

// The Pascal thread calls the cdecl function
function C2P_Translator(FuncData: pointer) : ptrint;
var
  ThreadData: TCdeclThreadFuncData;
begin
  ThreadData := PCdeclThreadFuncData(FuncData)^;
  Result := ptrint(ThreadData.Func(ThreadData.Data));
end;

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
function InvalidCircuit(DSS: TDSSContext): Boolean; inline;
begin
    if DSS.ActiveCircuit = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, _('There is no active circuit! Create a circuit and retry.'), 8888);
        end;
        Result := True;
        Exit;
    end;
    Result := False;
end;
//------------------------------------------------------------------------------
function MissingSolution(DSS: TDSSContext): Boolean; inline;
begin
    Result := InvalidCircuit(DSS);
    if Result then
        Exit;
        
    if DSS.ActiveCircuit.Solution.NodeV = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, _('Solution state is not initialized for the active circuit!'), 8899);
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
function DSS_GetAsPAnsiChar(DSS: TDSSContext; s: Ansistring): PAnsiChar;
begin
    // keep a reference to the string to make sure the memory is not deallocated
    DSS.tempBuffer := s;
    result := PAnsiChar(DSS.tempBuffer);
end;
//------------------------------------------------------------------------------
procedure ctx_DSS_ResetStringBuffer(DSS: TDSSContext); CDECL;
begin
    DSS.tempBuffer := '';
end;
//------------------------------------------------------------------------------
procedure DSS_ResetStringBuffer(); CDECL;
begin
    DSSPrime.tempBuffer := '';
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

procedure DSS_Dispose_PPointer(var p: PPointer); CDECL;
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

function DSS_CreateArray_PSingle(var p: PSingle; cnt: PAPISize; const incount: TAPISize): PSingleArray0;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(Single));
    result := PSingleArray0(p);
end;

function DSS_CreateArray_PDouble(var p: PDouble; cnt: PAPISize; const incount: TAPISize): PDoubleArray0;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(Double));
    result := PDoubleArray0(p);
end;

function DSS_CreateArray_PInteger(var p: PInteger; cnt: PAPISize; const incount: TAPISize): PIntegerArray0;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(Integer));
    result := PIntegerArray0(p);
end;

function DSS_CreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PAPISize; const incount: TAPISize): PPAnsiCharArray0;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(PAnsiChar));
    result := PPAnsiCharArray0(p);
end;

function DSS_CreateArray_PPointer(var p: PPointer; cnt: PAPISize; const incount: TAPISize): PPointerArray0;
begin
    cnt[0] := incount;
    cnt[1] := incount;
    p := AllocMem(incount * sizeof(Pointer));
    result := PPointerArray0(p);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PInteger(var p: PInteger; cnt: PAPISize; const incount: TAPISize): PIntegerArray0;
begin
    if (cnt[1] < incount) then
    begin
        DSS_Dispose_PInteger(p);
        Result := DSS_CreateArray_PInteger(p, cnt, incount);
    end
    else
    begin
        cnt[0] := incount;
        Result := PIntegerArray0(p);
        FillByte(Result^, incount * sizeof(Integer), 0); // needs to zero it for compatibility
    end;
end;

procedure DSS_RecreateArray_PInteger(var res: PIntegerArray0; var p: PInteger; cnt: PAPISize; const incount: TAPISize);
begin
    res := DSS_RecreateArray_PInteger(p, cnt, incount);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PPAnsiChar(var p: PPAnsiChar; cnt: PAPISize; const incount: TAPISize): PPAnsiCharArray0;
begin
    // no size optimization for strings yet
    DSS_Dispose_PPAnsiChar(p, cnt[1]);
    Result := DSS_CreateArray_PPAnsiChar(p, cnt, incount);
end;

procedure DSS_RecreateArray_PPAnsiChar(var res: PPAnsiCharArray0; var p: PPAnsiChar; cnt: PAPISize; const incount: TAPISize);
begin
    res := DSS_RecreateArray_PPAnsiChar(p, cnt, incount);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PPointer(var p: PPointer; cnt: PAPISize; const incount: TAPISize): PPointerArray0;
begin
    if (cnt[1] < incount) then
    begin
        DSS_Dispose_PPointer(p);
        Result := DSS_CreateArray_PPointer(p, cnt, incount);
    end
    else
    begin
        cnt[0] := incount;
        Result := PPointerArray0(p);
        FillByte(Result^, incount * sizeof(Pointer), 0); // needs to zero it for compatibility
    end;
end;

procedure DSS_RecreateArray_PPointer(var res: PPointerArray0; var p: PPointer; cnt: PAPISize; const incount: TAPISize);
begin
    res := DSS_RecreateArray_PPointer(p, cnt, incount);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PSingle(var p: PSingle; cnt: PAPISize; const incount: TAPISize): PSingleArray0;
begin
    if (cnt[1] < incount) then
    begin
        DSS_Dispose_PSingle(p);
        Result := DSS_CreateArray_PSingle(p, cnt, incount);
    end
    else
    begin
        cnt[0] := incount;
        Result := PSingleArray0(p);
        FillByte(Result^, incount * sizeof(Single), 0); // needs to zero it for compatibility
    end;
end;

procedure DSS_RecreateArray_PSingle(var res: PSingleArray0; var p: PSingle; cnt: PAPISize; const incount: TAPISize);
begin
    res := DSS_RecreateArray_PSingle(p, cnt, incount);
end;

//------------------------------------------------------------------------------
function DSS_RecreateArray_PDouble(var p: PDouble; cnt: PAPISize; const incount: TAPISize): PDoubleArray0;
begin
    if (cnt[1] < incount) then
    begin
        DSS_Dispose_PDouble(p);
        Result := DSS_CreateArray_PDouble(p, cnt, incount);
    end
    else
    begin
        cnt[0] := incount;
        Result := PDoubleArray0(p);
        FillByte(Result^, incount * sizeof(Double), 0); // needs to zero it for compatibility
    end;
end;

procedure DSS_RecreateArray_PDouble(var res: PDoubleArray0; var p: PDouble; cnt: PAPISize; const incount: TAPISize);
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
procedure ctx_DSS_GetGRPointers(
    DSS: TDSSContext;
    
    var DataPtr_PPAnsiChar: PPPAnsiChar;
    var DataPtr_PDouble: PPDouble;
    var DataPtr_PInteger: PPInteger;
    var DataPtr_PByte: PPByte;
    var CountPtr_PPAnsiChar: PAPISize;
    var CountPtr_PDouble: PAPISize;
    var CountPtr_PInteger: PAPISize;
    var CountPtr_PByte: PAPISize
); CDECL;
begin
    with DSS do
    begin
        if (@DataPtr_PPAnsiChar <> nil) then DataPtr_PPAnsiChar := @GR_DataPtr_PPAnsiChar;
        if (@DataPtr_PDouble <> nil) then DataPtr_PDouble := @GR_DataPtr_PDouble;
        if (@DataPtr_PInteger <> nil) then DataPtr_PInteger := @GR_DataPtr_PInteger;
        if (@DataPtr_PByte <> nil) then DataPtr_PByte := @GR_DataPtr_PByte;
        CountPtr_PPAnsiChar := @GR_Counts_PPAnsiChar[0];
        CountPtr_PDouble := @GR_Counts_PDouble[0];
        CountPtr_PInteger := @GR_Counts_PInteger[0];
        CountPtr_PByte := @GR_Counts_PByte[0];
    end;
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
    var CountPtr_PByte: PAPISize
); CDECL;
begin
    ctx_DSS_GetGRPointers(
        DSSPrime,
        DataPtr_PPAnsiChar,
        DataPtr_PDouble,
        DataPtr_PInteger,
        DataPtr_PByte,
        CountPtr_PPAnsiChar,
        CountPtr_PDouble,
        CountPtr_PInteger,
        CountPtr_PByte
    );
end;
//------------------------------------------------------------------------------
// Separate simple functions for MATLAB, which return the current pointer
// directly, instead of a pointer to pointer.

function ctx_DSS_GR_DataPtr_PDouble(DSS: TDSSContext): PDouble; CDECL;
begin
    Result := DSS.GR_DataPtr_PDouble;
end;
function ctx_DSS_GR_DataPtr_PInteger(DSS: TDSSContext): PInteger; CDECL;
begin
    Result := DSS.GR_DataPtr_PInteger;
end;
function ctx_DSS_GR_DataPtr_PByte(DSS: TDSSContext): PByte; CDECL;
begin
    Result := DSS.GR_DataPtr_PByte;
end;
function ctx_DSS_GR_CountPtr_PDouble(DSS: TDSSContext): PAPISize; CDECL;
begin
    Result := @DSS.GR_Counts_PDouble[0];
end;
function ctx_DSS_GR_CountPtr_PInteger(DSS: TDSSContext): PAPISize; CDECL;
begin
    Result := @DSS.GR_Counts_PInteger[0];
end;
function ctx_DSS_GR_CountPtr_PByte(DSS: TDSSContext): PAPISize; CDECL;
begin
    Result := @DSS.GR_Counts_PByte[0];
end;

function DSS_GR_DataPtr_PDouble(): PDouble; CDECL;
begin
    Result := DSSPrime.GR_DataPtr_PDouble;
end;
function DSS_GR_DataPtr_PInteger(): PInteger; CDECL;
begin
    Result := DSSPrime.GR_DataPtr_PInteger;
end;
function DSS_GR_DataPtr_PByte(): PByte; CDECL;
begin
    Result := DSSPrime.GR_DataPtr_PByte;
end;
function DSS_GR_CountPtr_PDouble(): PAPISize; CDECL;
begin
    Result := @DSSPrime.GR_Counts_PDouble[0];
end;
function DSS_GR_CountPtr_PInteger(): PAPISize; CDECL;
begin
    Result := @DSSPrime.GR_Counts_PInteger[0];
end;
function DSS_GR_CountPtr_PByte(): PAPISize; CDECL;
begin
    Result := @DSSPrime.GR_Counts_PByte[0];
end;

//------------------------------------------------------------------------------
procedure ctx_DSS_DisposeGRData(DSS: TDSSContext); CDECL;
begin
    with DSS do
    begin
        DSS_Dispose_PByte(GR_DataPtr_PByte);
        DSS_Dispose_PDouble(GR_DataPtr_PDouble);
        DSS_Dispose_PInteger(GR_DataPtr_PInteger);
        DSS_Dispose_PPAnsiChar(GR_DataPtr_PPAnsiChar, GR_Counts_PPAnsiChar[1]);

        GR_Counts_PPAnsiChar[0] := 0;
        GR_Counts_PDouble[0] := 0;
        GR_Counts_PInteger[0] := 0;
        GR_Counts_PByte[0] := 0;

        GR_Counts_PPAnsiChar[1] := 0;
        GR_Counts_PDouble[1] := 0;
        GR_Counts_PInteger[1] := 0;
        GR_Counts_PByte[1] := 0;
    end;
end;
//------------------------------------------------------------------------------
procedure DSS_DisposeGRData(); CDECL;
begin
    ctx_DSS_DisposeGRData(DSSPrime);
end;
//------------------------------------------------------------------------------
procedure Generic_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; pList: TDSSPointerList; const Restore: Boolean); inline;
var
    Result: PPAnsiCharArray0;
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
function Generic_CktElement_Get_First(DSS: TDSSContext; pList: TDSSPointerList): Integer; inline;
var
    elem: TDSSCktElement;
begin
    Result := 0;
    elem := TDSSCktElement(pList.First);
    if elem = NIL then
        Exit;
        
    repeat
        if (DSS_CAPI_ITERATE_DISABLED = 1) or elem.Enabled then
        begin
            DSS.ActiveCircuit.ActiveCktElement := elem;
            Result := 1;
        end
        else
            elem := TDSSCktElement(pList.Next);
    until (Result = 1) or (elem = NIL);
end;
//------------------------------------------------------------------------------
function Generic_CktElement_Get_Next(DSS: TDSSContext; pList: TDSSPointerList): Integer; inline;
var
    elem: TDSSCktElement;
begin
    Result := 0;
    elem := TDSSCktElement(pList.Next);
    if elem = NIL then
        Exit;
        
    repeat
        if (DSS_CAPI_ITERATE_DISABLED = 1) or elem.Enabled then
        begin
            DSS.ActiveCircuit.ActiveCktElement := elem;
            Result := pList.ActiveIndex;
        end
        else
            elem := TDSSCktElement(pList.Next);
    until (Result > 0) or (elem = NIL);
end;
//------------------------------------------------------------------------------
function C2P_Translator(FuncData: pointer): PtrUInt;
var
    ThreadData: TCDECLThreadFuncData;
begin
    ThreadData := PCDECLThreadFuncData(FuncData)^;
    Result := PtrUInt(ThreadData.Func(ThreadData.Data));
end;

function DSS_BeginPascalThread(func: Pointer; paramptr: Pointer): PtrUInt; CDECL;
var
    ThreadData: PCDECLThreadFuncData;
begin
    New(ThreadData);
    ThreadData^.Func := TCDECLThreadFunc(func);
    ThreadData^.Data := paramptr;
    Result := BeginThread(@C2P_Translator, ThreadData);
end;

procedure DSS_WaitPascalThread(handle: PtrUInt); CDECL;
begin
    WaitForThreadTerminate(TThreadID(handle), MAXLONGINT);
end;

end.
