unit CAPI_Reactors;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure Reactors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
function Reactors_Get_Name(): PAnsiChar; CDECL;
procedure Reactors_Set_Name(const Value: PAnsiChar); CDECL;
function Reactors_Get_First(): Integer; CDECL;
function Reactors_Get_Next(): Integer; CDECL;
function Reactors_Get_Count(): Integer; CDECL;
function Reactors_Get_kV(): Double; CDECL;
procedure Reactors_Set_kV(Value: Double); CDECL;
function Reactors_Get_kvar(): Double; CDECL;
procedure Reactors_Set_kvar(Value: Double); CDECL;
function Reactors_Get_Phases(): Integer; CDECL;
procedure Reactors_Set_Phases(Value: Integer); CDECL;
function Reactors_Get_SpecType(): Integer; CDECL;
function Reactors_Get_IsDelta(): TAPIBoolean; CDECL;
procedure Reactors_Set_IsDelta(Value: TAPIBoolean); CDECL;
function Reactors_Get_Parallel(): TAPIBoolean; CDECL;
procedure Reactors_Set_Parallel(Value: TAPIBoolean); CDECL;
function Reactors_Get_LmH(): Double; CDECL;
procedure Reactors_Set_LmH(Value: Double); CDECL;
function Reactors_Get_Bus1(): PAnsiChar; CDECL;
function Reactors_Get_Bus2(): PAnsiChar; CDECL;
procedure Reactors_Set_Bus1(const Value: PAnsiChar); CDECL;
procedure Reactors_Set_Bus2(const Value: PAnsiChar); CDECL;
function Reactors_Get_R(): Double; CDECL;
procedure Reactors_Set_R(Value: Double); CDECL;
function Reactors_Get_X(): Double; CDECL;
procedure Reactors_Set_X(Value: Double); CDECL;
function Reactors_Get_Rp(): Double; CDECL;
procedure Reactors_Set_Rp(Value: Double); CDECL;
function Reactors_Get_RCurve(): PAnsiChar; CDECL;
procedure Reactors_Set_RCurve(const Value: PAnsiChar); CDECL;
function Reactors_Get_LCurve(): PAnsiChar; CDECL;
procedure Reactors_Set_LCurve(const Value: PAnsiChar); CDECL;
procedure Reactors_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Reactors_Get_Rmatrix_GR(); CDECL;
procedure Reactors_Set_Rmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Reactors_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Reactors_Get_Xmatrix_GR(); CDECL;
procedure Reactors_Set_Xmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Reactors_Get_Z(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Reactors_Get_Z_GR(); CDECL;
procedure Reactors_Set_Z(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Reactors_Get_Z1(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Reactors_Get_Z1_GR(); CDECL;
procedure Reactors_Set_Z1(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Reactors_Get_Z2(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Reactors_Get_Z2_GR(); CDECL;
procedure Reactors_Set_Z2(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Reactors_Get_Z0(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Reactors_Get_Z0_GR(); CDECL;
procedure Reactors_Set_Z0(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
function Reactors_Get_idx(): Integer; CDECL;
procedure Reactors_Set_idx(Value: Integer); CDECL;
function Reactors_Get_Pointer(): Pointer; CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    DSSClass,
    DSSHelper,
    DSSObjectHelper,
    Executive,
    Reactor,
    SysUtils,
    DSSPointerList,
    Utilities,
    UComplex, DSSUcomplex;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TReactorObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.ActiveCircuit.Reactors.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['Reactor'], 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Reactors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.Reactors, False);
end;
//------------------------------------------------------------------------------
function Reactors_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.Reactors);
end;
//------------------------------------------------------------------------------
function Reactors_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.Reactors);
end;
//------------------------------------------------------------------------------
function Reactors_Get_Name(): PAnsiChar; CDECL;
var
    elem: TReactorObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name());
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.ReactorClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.SetActiveCktElement(DSSPrime.ReactorClass.ElementList.Active);
        DSSPrime.ActiveCircuit.Reactors.Get(DSSPrime.ReactorClass.ActiveIndex());
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Reactor "%s" not found in Active Circuit.', [Value], 5003);
    end;
end;
//------------------------------------------------------------------------------
function Reactors_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Reactors.Count;
end;
//------------------------------------------------------------------------------
function Reactors_Get_Bus1(): PAnsiChar; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, pReactor.GetBus(1));
end;
//------------------------------------------------------------------------------
function Reactors_Get_Bus2(): PAnsiChar; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, pReactor.GetBus(2));
end;
//------------------------------------------------------------------------------
function Reactors_Get_LCurve(): PAnsiChar; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;

    if pReactor.LCurveObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, pReactor.LCurveObj.Name());
end;
//------------------------------------------------------------------------------
function Reactors_Get_RCurve(): PAnsiChar; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    
    if pReactor.RCurveObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, pReactor.RCurveObj.Name());
end;
//------------------------------------------------------------------------------
function Reactors_Get_Parallel(): TAPIBoolean; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    Result := pReactor.IsParallel;
end;
//------------------------------------------------------------------------------
function Reactors_Get_IsDelta(): TAPIBoolean; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    Result := (pReactor.Connection = TReactorConnection.Delta);
end;
//------------------------------------------------------------------------------
function Reactors_Get_kV(): Double; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    Result := pReactor.kVRating;
end;
//------------------------------------------------------------------------------
function Reactors_Get_kvar(): Double; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    Result := pReactor.kvarRating;
end;
//------------------------------------------------------------------------------
function Reactors_Get_LmH(): Double; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    Result := pReactor.L * 1000.0;
end;
//------------------------------------------------------------------------------
function Reactors_Get_Phases(): Integer; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    Result := pReactor.Nphases;
end;
//------------------------------------------------------------------------------
function Reactors_Get_SpecType(): Integer; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    Result := pReactor.SpecType;
end;
//------------------------------------------------------------------------------
function Reactors_Get_R(): Double; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    Result := pReactor.Z.re;
end;
//------------------------------------------------------------------------------
function Reactors_Get_X(): Double; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    Result := pReactor.Z.im;
end;
//------------------------------------------------------------------------------
function Reactors_Get_Rp(): Double; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    Result := pReactor.Rp;
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_IsDelta(Value: TAPIBoolean); CDECL;
var
    pReactor: TReactorObj;
    conn: TGeneralConnection;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;

    if Value then
        conn := TReactorConnection.Delta
    else
        conn := TReactorConnection.Wye;

    pReactor.SetInteger(ord(TReactorProp.conn), ord(conn), []);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Parallel(Value: TAPIBoolean); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        pReactor.SetInteger(ord(TReactorProp.Parallel), Integer(Value), []);
        Exit;
    end;
    pReactor.IsParallel := Value;
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Bus1(const Value: PAnsiChar); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    pReactor.SetString(ord(TReactorProp.bus1), Value, []);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Bus2(const Value: PAnsiChar); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    pReactor.SetString(ord(TReactorProp.bus2), Value, []);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_LCurve(const Value: PAnsiChar); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;

    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        pReactor.SetString(ord(TReactorProp.LCurve), Value, []);
        Exit;
    end;
    // Note: EPRI's code actually just sets the curve name, doesn't load it here.
    pReactor.LCurveObj := DSSPrime.XYCurveClass.Find(Value);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_RCurve(const Value: PAnsiChar); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;

    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        pReactor.SetString(ord(TReactorProp.RCurve), Value, []);
        Exit;
    end;
    // Note: EPRI's code actually just sets the curve name, doesn't load it here.
    pReactor.RCurveObj := DSSPrime.XYCurveClass.Find(Value);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_kV(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        pReactor.SetDouble(ord(TReactorProp.kV), Value, []);
        Exit;
    end;
    pReactor.kVRating := Value;
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_kvar(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        pReactor.SetDouble(ord(TReactorProp.kvar), Value, []);
        Exit;
    end;
    pReactor.kvarRating := Value;
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_LmH(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        pReactor.SetDouble(ord(TReactorProp.LmH), Value, []);
        Exit;
    end;
    pReactor.L := Value / 1000.0;
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Phases(Value: Integer); CDECL;
var
    elem: TReactorObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if Value < 1 then
    begin
        DoSimpleMsg(DSSPrime, '%s: Number of phases must be a positive integer!', [elem.FullName()], 6568);
        Exit;
    end;
    elem.SetInteger(ord(TReactorProp.Phases), Value, []);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_R(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        pReactor.SetDouble(ord(TReactorProp.R), Value, []);
        Exit;
    end;
    pReactor.Z.re := Value;
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_X(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        pReactor.SetDouble(ord(TReactorProp.X), Value, []);
        Exit;
    end;
    pReactor.Z.im := Value;
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Rp(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        pReactor.SetDouble(ord(TReactorProp.Rp), Value, []);
        Exit;
    end;
    pReactor.Rp := Value;
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Rmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
        
    if Sqr(pReactor.Nphases) <> ValueCount then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, 'The number of values provided (%d) does not match the expected (%d).', [ValueCount, Sqr(pReactor.Nphases)], 5024);
        end;
        Exit;
    end;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        pReactor.SetDoubles(ord(TReactorProp.RMatrix), ValuePtr, ValueCount, []);
        Exit;
    end;
    Reallocmem(pReactor.Rmatrix, SizeOf(Double) * Sqr(pReactor.Nphases));
    Move(ValuePtr^, pReactor.Rmatrix[1], ValueCount * SizeOf(Double));
    pReactor.SetYprimInvalid(true);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Xmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    if Sqr(pReactor.Nphases) <> ValueCount then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, 'The number of values provided (%d) does not match the expected (%d).', [ValueCount, Sqr(pReactor.Nphases)], 5024);
        end;
        Exit;
    end;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        pReactor.SetDoubles(ord(TReactorProp.XMatrix), ValuePtr, ValueCount, []);
        Exit;
    end;
    Reallocmem(pReactor.Xmatrix, SizeOf(Double) * Sqr(pReactor.Nphases));
    Move(ValuePtr^, pReactor.Xmatrix[1], ValueCount * SizeOf(Double));
    pReactor.SetYprimInvalid(true);
end;
//------------------------------------------------------------------------------
procedure Reactors_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    pReactor: TReactorObj;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    
    if pReactor.Rmatrix = NIL then
        Exit; //TODO: would an error here be useful?

    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, pReactor.Nphases * pReactor.Nphases, pReactor.Nphases, pReactor.Nphases);
    Move(pReactor.Rmatrix[1], ResultPtr^, ResultCount^ * SizeOf(Double));
end;

procedure Reactors_Get_Rmatrix_GR(); CDECL;
// Same as Reactors_Get_Rmatrix but uses global result (GR) pointers
begin
    Reactors_Get_Rmatrix(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Reactors_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    pReactor: TReactorObj;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, pReactor) then
        Exit;

    if pReactor.Xmatrix = NIL then
        Exit;
        
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, pReactor.Nphases * pReactor.Nphases, pReactor.Nphases, pReactor.Nphases);
    Move(pReactor.Xmatrix[1], ResultPtr^, ResultCount^ * SizeOf(Double));
end;

procedure Reactors_Get_Xmatrix_GR(); CDECL;
// Same as Reactors_Get_Xmatrix but uses global result (GR) pointers
begin
    Reactors_Get_Xmatrix(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Reactors_Get_Z1(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    pReactor: TReactorObj;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, pReactor) then
        Exit;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
    Result[0] := pReactor.Z1.Re;
    Result[1] := pReactor.Z1.Im;
end;

procedure Reactors_Get_Z1_GR(); CDECL;
// Same as Reactors_Get_Z1 but uses global result (GR) pointers
begin
    Reactors_Get_Z1(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Reactors_Get_Z2(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    pReactor: TReactorObj;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, pReactor) then
        Exit;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
    Result[0] := pReactor.Z2.Re;
    Result[1] := pReactor.Z2.Im;
end;

procedure Reactors_Get_Z2_GR(); CDECL;
// Same as Reactors_Get_Z2 but uses global result (GR) pointers
begin
    Reactors_Get_Z2(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Reactors_Get_Z0(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    pReactor: TReactorObj;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, pReactor) then
        Exit;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
    Result[0] := pReactor.Z0.Re;
    Result[1] := pReactor.Z0.Im;
end;

procedure Reactors_Get_Z0_GR(); CDECL;
// Same as Reactors_Get_Z0 but uses global result (GR) pointers
begin
    Reactors_Get_Z0(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Reactors_Get_Z(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    pReactor: TReactorObj;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, pReactor) then
        Exit;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
    Result[0] := pReactor.Z.re;
    Result[1] := pReactor.Z.im;
end;

procedure Reactors_Get_Z_GR(); CDECL;
// Same as Reactors_Get_Z but uses global result (GR) pointers
begin
    Reactors_Get_Z(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Reactors_Set_Z2(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray0;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray0(ValuePtr);
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    
    if (ValueCount <> 2) then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, 'The number of values provided (%d) does not match the expected (%d).', [ValueCount, 2], 5024);
        end;
        Exit;
    end;

    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        pReactor.SetDoubles(ord(TReactorProp.Z2), ValuePtr, ValueCount, []);
        Exit;
    end;
    pReactor.Z2 := Cmplx(Value[0], Value[1]);
    pReactor.SetYprimInvalid(true);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Z1(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray0;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray0(ValuePtr);
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    
    if (ValueCount <> 2) then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, 'The number of values provided (%d) does not match the expected (%d).', [ValueCount, 2], 5024);
        end;
        Exit;
    end;

    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        pReactor.SetDoubles(ord(TReactorProp.Z1), ValuePtr, ValueCount, []);
        Exit;
    end;
    pReactor.Z1 := Cmplx(Value[0], Value[1]);
    pReactor.SetYprimInvalid(true);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Z0(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray0;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray0(ValuePtr);
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    
    if (ValueCount <> 2) then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, 'The number of values provided (%d) does not match the expected (%d).', [ValueCount, 2], 5024);
        end;
        Exit;
    end;
    
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        pReactor.SetDoubles(ord(TReactorProp.Z0), ValuePtr, ValueCount, []);
        Exit;
    end;
    pReactor.Z0 := Cmplx(Value[0], Value[1]);
    pReactor.SetYprimInvalid(true);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Z(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray0;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray0(ValuePtr);
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    
    if (ValueCount <> 2) then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, 'The number of values provided (%d) does not match the expected (%d).', [ValueCount, 2], 5024);
        end;
        Exit;
    end;

    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        pReactor.SetDoubles(ord(TReactorProp.Z), ValuePtr, ValueCount, []);
        Exit;
    end;
    pReactor.Z := Cmplx(Value[0], Value[1]);
    pReactor.SetYprimInvalid(true);
end;
//------------------------------------------------------------------------------
function Reactors_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Reactors.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_idx(Value: Integer); CDECL;
var
    pReactor: TReactorObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    pReactor := DSSPrime.ActiveCircuit.Reactors.Get(Value);
    if pReactor = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['Reactor', Value], 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.SetActiveCktElement(pReactor);
end;
//------------------------------------------------------------------------------
function Reactors_Get_Pointer(): Pointer; CDECL;
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Reactors.Active
end;
//------------------------------------------------------------------------------
end.
