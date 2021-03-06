unit CAPI_Reactors;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure Reactors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Reactors_Get_AllNames_GR(); CDECL;
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


implementation

uses
    CAPI_Constants,
    DSSGlobals,
    DSSClass,
    DSSHelper,
    Executive,
    Reactor,
    SysUtils,
    DSSPointerList,
    Utilities,
    ucomplex;

type
    ReactorProps = (bus1 = 1, bus2, phases, kvar, kv, conn, Rmatrix, Xmatrix, Parallel, R, X, Rp, Z1, Z2, Z0, Z, RCurve, LCurve, LmH);

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
            DoSimpleMsg(DSS, 'No active Reactor object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure ReactorPropSideEffects(DSS: TDSSContext; prop: ReactorProps; reactor: TReactorObj);
begin
    with reactor do
    begin
    // Some specials ...
        case prop of
            ReactorProps.bus1:
            begin
                PropertyValue[2] := GetBus(2);   // this gets modified
                PrpSequence^[2] := 0;       // Reset this for save function
            end;
            ReactorProps.bus2:
                if CompareText(StripExtension(GetBus(1)), StripExtension(GetBus(2))) <> 0 then
                begin
                    IsShunt := FALSE;
                    Bus2Defined := TRUE;
                end;
            ReactorProps.phases: //IF Fnphases <> Parser.IntValue THEN 
            begin
            // Nphases := Parser.IntValue ;
                NConds := nphases;  // Force Reallocation of terminal info
                Yorder := Fnterms * Fnconds;
            end;
            ReactorProps.kvar:
                SpecType := 1;   // X specified by kvar, kV
            ReactorProps.Rmatrix, ReactorProps.Xmatrix:
                SpecType := 3;
            ReactorProps.X:
                SpecType := 2;   // X specified directly rather than computed from kvar
            ReactorProps.Rp:
                RpSpecified := TRUE;
            ReactorProps.Z1:
            begin
                SpecType := 4;    // have to set Z1 to get this mode
                if not Z2Specified then
                    Z2 := Z1;
                if not Z0Specified then
                    Z0 := Z1;
            end;
            ReactorProps.Z2:
                Z2Specified := TRUE;
            ReactorProps.Z0:
                Z0Specified := TRUE;
            ReactorProps.Z:
            begin
                R := Z.re;
                X := Z.im;
                SpecType := 2;
            end;
            ReactorProps.RCurve:
                RCurveObj := DSS.XYCurveClass.Find(RCurve);
            ReactorProps.LCurve:
                LCurveObj := DSS.XYCurveClass.Find(LCurve);
            ReactorProps.LmH:
            begin
                SpecType := 2;
                X := L * TwoPi * BaseFrequency;
            end
        else
        end;

    //YPrim invalidation on anything that changes impedance values
        case prop of
            ReactorProps.phases..ReactorProps.Z:
                YprimInvalid := TRUE;
            ReactorProps.RCurve:
                if RCurveObj = NIL then
                    DoSimpleMsg('Resistance-frequency curve XYCurve.' + RCurve + ' not Found.', 2301);
            ReactorProps.LCurve:
                if LCurveObj = NIL then
                    DoSimpleMsg('Inductance-frequency curve XYCurve.' + LCurve + ' not Found.', 2301);
            ReactorProps.LmH:
                YprimInvalid := TRUE;
        else
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Reactors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.Reactors, False);
end;

procedure Reactors_Get_AllNames_GR(); CDECL;
// Same as Reactors_Get_AllNames but uses global result (GR) pointers
begin
    Reactors_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
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
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.ReactorClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.ReactorClass.ElementList.Active;
        DSSPrime.ActiveCircuit.Reactors.Get(DSSPrime.ReactorClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Reactor "' + Value + '" Not Found in Active Circuit.', 5003);
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
    Result := DSS_GetAsPAnsiChar(DSSPrime, pReactor.LCurve);
end;
//------------------------------------------------------------------------------
function Reactors_Get_RCurve(): PAnsiChar; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, pReactor.RCurve);
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
    Result := pReactor.kvrating;
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
    Result := pReactor.R;
end;
//------------------------------------------------------------------------------
function Reactors_Get_X(): Double; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    Result := pReactor.X;
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
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;

    if Value then 
        pReactor.Connection := TReactorConnection.Delta
    else
        pReactor.Connection := TReactorConnection.Wye;
        
    ReactorPropSideEffects(DSSPrime, ReactorProps.conn, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Parallel(Value: TAPIBoolean); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    pReactor.IsParallel := Value;
    ReactorPropSideEffects(DSSPrime, ReactorProps.Parallel, pReactor);
end;
//------------------------------------------------------------------------------
procedure _ReactorSetbus1(pReactor: TReactorObj; const s: String);
var
    s2: String;
    i, dotpos: Integer;
   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0
begin
    with pReactor do
    begin
        SetBus(1, S);
     // Default Bus2 to zero node of Bus1 if not already defined. (Wye Grounded connection)
        if not Bus2Defined then
        begin
         // Strip node designations from S
            dotpos := Pos('.', S);
            if dotpos > 0 then
                S2 := Copy(S, 1, dotpos - 1)
            else
                S2 := Copy(S, 1, Length(S));  // copy up to Dot
            for i := 1 to Fnphases do
                S2 := S2 + '.0';
            SetBus(2, S2);
            IsShunt := TRUE;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Bus1(const Value: PAnsiChar); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    _ReactorSetbus1(pReactor, Value);
    ReactorPropSideEffects(DSSPrime, ReactorProps.bus1, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Bus2(const Value: PAnsiChar); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    pReactor.SetBus(2, Value);
    ReactorPropSideEffects(DSSPrime, ReactorProps.bus2, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_LCurve(const Value: PAnsiChar); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    pReactor.LCurve := Value;
    ReactorPropSideEffects(DSSPrime, ReactorProps.LCurve, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_RCurve(const Value: PAnsiChar); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    pReactor.RCurve := Value;
    ReactorPropSideEffects(DSSPrime, ReactorProps.RCurve, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_kV(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    pReactor.kvrating := Value;
    ReactorPropSideEffects(DSSPrime, ReactorProps.kv, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_kvar(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    pReactor.kvarRating := Value;
    ReactorPropSideEffects(DSSPrime, ReactorProps.kvar, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_LmH(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    pReactor.L := Value / 1000.0;
    ReactorPropSideEffects(DSSPrime, ReactorProps.LmH, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Phases(Value: Integer); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    if Value = pReactor.NPhases then
        Exit;
    pReactor.NPhases := Value;
    ReactorPropSideEffects(DSSPrime, ReactorProps.phases, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_R(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    pReactor.R := Value;
    ReactorPropSideEffects(DSSPrime, ReactorProps.R, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_X(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    pReactor.X := Value;
    ReactorPropSideEffects(DSSPrime, ReactorProps.X, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Rp(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if not _activeObj(DSSPrime, pReactor) then
        Exit;
    pReactor.Rp := Value;
    ReactorPropSideEffects(DSSPrime, ReactorProps.Rp, pReactor);
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
            DoSimpleMsg(DSSPrime, Format('The number of values provided (%d) does not match the expected (%d).', [ValueCount, Sqr(pReactor.Nphases)]), 5024);
        end;
        Exit;
    end;
    with pReactor do
    begin
        Move(ValuePtr^, Rmatrix[1], ValueCount * SizeOf(Double));
        ReactorPropSideEffects(DSSPrime, ReactorProps.Rmatrix, pReactor);
    end;
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
            DoSimpleMsg(DSSPrime, Format('The number of values provided (%d) does not match the expected (%d).', [ValueCount, Sqr(pReactor.Nphases)]), 5024);
        end;
        Exit;
    end;
    with pReactor do
    begin
        Move(ValuePtr^, Xmatrix[1], ValueCount * SizeOf(Double));
        ReactorPropSideEffects(DSSPrime, ReactorProps.Xmatrix, pReactor);
    end;
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

    with pReactor do
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, Sqr(Nphases));
        Move(Rmatrix[1], ResultPtr^, ResultCount^ * SizeOf(Double));
    end;
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
        
    with pReactor do
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, Sqr(Nphases));
        Move(Xmatrix[1], ResultPtr^, ResultCount^ * SizeOf(Double));
    end;
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
    with pReactor do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        Result[0] := Z1.Re;
        Result[1] := Z1.Im;
    end;
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
    with pReactor do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        Result[0] := Z2.Re;
        Result[1] := Z2.Im;
    end;
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
    with pReactor do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        Result[0] := Z0.Re;
        Result[1] := Z0.Im;
    end;
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
    with pReactor do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        Result[0] := R;
        Result[1] := X;
    end;
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
        Exit;

    with pReactor do
    begin
        Z2 := Cmplx(Value[0], Value[1]);
        ReactorPropSideEffects(DSSPrime, ReactorProps.Z2, pReactor);
    end;
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
        Exit;

    with pReactor do
    begin
        Z1 := Cmplx(Value[0], Value[1]);
        ReactorPropSideEffects(DSSPrime, ReactorProps.Z1, pReactor);
    end;
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
        Exit;
    
    with pReactor do
    begin
        Z0 := Cmplx(Value[0], Value[1]);
        ReactorPropSideEffects(DSSPrime, ReactorProps.Z0, pReactor);
    end;
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
        Exit;

    with pReactor do
    begin
        Z := Cmplx(Value[0], Value[1]);
        ReactorPropSideEffects(DSSPrime, ReactorProps.Z, pReactor);
    end;
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
        DoSimpleMsg(DSSPrime, 'Invalid Reactor index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pReactor;
end;
//------------------------------------------------------------------------------
end.
