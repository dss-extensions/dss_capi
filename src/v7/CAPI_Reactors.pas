unit CAPI_Reactors;

{$inline on}

interface

uses
    CAPI_Utils;

procedure Reactors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
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
function Reactors_Get_IsDelta(): Boolean; CDECL;
procedure Reactors_Set_IsDelta(Value: Boolean); CDECL;
function Reactors_Get_Parallel(): Boolean; CDECL;
procedure Reactors_Set_Parallel(Value: Boolean); CDECL;
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
procedure Reactors_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Reactors_Get_Rmatrix_GR(); CDECL;
procedure Reactors_Set_Rmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure Reactors_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Reactors_Get_Xmatrix_GR(); CDECL;
procedure Reactors_Set_Xmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure Reactors_Get_Z(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Reactors_Get_Z_GR(); CDECL;
procedure Reactors_Set_Z(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure Reactors_Get_Z1(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Reactors_Get_Z1_GR(); CDECL;
procedure Reactors_Set_Z1(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure Reactors_Get_Z2(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Reactors_Get_Z2_GR(); CDECL;
procedure Reactors_Set_Z2(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure Reactors_Get_Z0(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Reactors_Get_Z0_GR(); CDECL;
procedure Reactors_Set_Z0(ValuePtr: PDouble; ValueCount: Integer); CDECL;
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
    PointerList,
    Utilities,
    ucomplex;

//------------------------------------------------------------------------------
function ActiveReactor(out e: TReactorObj): TReactorObj; inline;
begin
    e := NIL;
    if ActiveCircuit <> NIL then
        e := ActiveCircuit.Reactors.Active;
    Result := e;
end;
//------------------------------------------------------------------------------
procedure ReactorPropSideEffects(prop: TReactorProp; reactor: TReactorObj);
begin
    with reactor do
    begin
    // Some specials ...
        case prop of
            TReactorProp.bus1:
            begin
                PropertyValue[2] := GetBus(2);   // this gets modified
                PrpSequence^[2] := 0;       // Reset this for save function
            end;
            TReactorProp.bus2:
                if CompareText(StripExtension(GetBus(1)), StripExtension(GetBus(2))) <> 0 then
                begin
                    IsShunt := FALSE;
                    Bus2Defined := TRUE;
                end;
            TReactorProp.phases: //IF Fnphases <> Parser.IntValue THEN 
            begin
            // Nphases := Parser.IntValue ;
                NConds := nphases;  // Force Reallocation of terminal info
                Yorder := Fnterms * Fnconds;
            end;
            TReactorProp.kvar:
                SpecType := 1;   // X specified by kvar, kV
            TReactorProp.Rmatrix, TReactorProp.Xmatrix:
                SpecType := 3;
            TReactorProp.X:
                SpecType := 2;   // X specified directly rather than computed from kvar
            TReactorProp.Rp:
                RpSpecified := TRUE;
            TReactorProp.Z1:
            begin
                SpecType := 4;    // have to set Z1 to get this mode
                if not Z2Specified then
                    Z2 := Z1;
                if not Z0Specified then
                    Z0 := Z1;
            end;
            TReactorProp.Z2:
                Z2Specified := TRUE;
            TReactorProp.Z0:
                Z0Specified := TRUE;
            TReactorProp.Z:
            begin
                R := Z.re;
                X := Z.im;
                SpecType := 2;
            end;
            TReactorProp.RCurve:
                RCurveObj := DSSPrime.XYCurveClass.Find(RCurve);
            TReactorProp.LCurve:
                LCurveObj := DSSPrime.XYCurveClass.Find(LCurve);
            TReactorProp.LmH:
            begin
                SpecType := 2;
                X := L * TwoPi * BaseFrequency;
            end
        else
        end;

    //YPrim invalidation on anything that changes impedance values
        case prop of
            TReactorProp.phases..TReactorProp.Z:
                YprimInvalid := TRUE;
            TReactorProp.RCurve:
                if RCurveObj = NIL then
                    DoSimpleMsg('Resistance-frequency curve XYCurve.' + RCurve + ' not Found.', 2301);
            TReactorProp.LCurve:
                if LCurveObj = NIL then
                    DoSimpleMsg('Inductance-frequency curve XYCurve.' + LCurve + ' not Found.', 2301);
            TReactorProp.LmH:
                YprimInvalid := TRUE;
        else
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Reactors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit = NIL then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.Reactors, False);
end;
//------------------------------------------------------------------------------
function Reactors_Get_First(): Integer; CDECL;
var
    elem: TReactorObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit = NIL then
        exit;
    lst := ActiveCircuit.Reactors;
    elem := lst.First;
    if elem <> NIL then
    begin
        repeat
            if elem.Enabled then
            begin
                ActiveCircuit.ActiveCktElement := elem;
                Result := 1;
            end
            else
                elem := lst.Next;
        until (Result = 1) or (elem = NIL);
    end;
end;
//------------------------------------------------------------------------------
function Reactors_Get_Name_AnsiString(): Ansistring; inline;
var
    elem: TReactorObj;
begin
    Result := '';
    if ActiveReactor(elem) = NIL then
        exit;
    Result := elem.Name;
end;

function Reactors_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Reactors_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit = NIL then
        Exit;
    if DSSPrime.ReactorClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := DSSPrime.ReactorClass.ElementList.Active;
        ActiveCircuit.Reactors.Get(DSSPrime.ReactorClass.Active);
    end
    else
    begin
        DoSimpleMsg('Reactor "' + Value + '" Not Found in Active Circuit.', 5003);
    end;
end;
//------------------------------------------------------------------------------
function Reactors_Get_Next(): Integer; CDECL;
var
    elem: TReactorObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        lst := ActiveCircuit.Reactors;
        elem := lst.Next;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := elem;
                    Result := lst.ActiveIndex;
                end
                else
                    elem := lst.Next;
            until (Result > 0) or (elem = NIL);
        end
    end;
end;
//------------------------------------------------------------------------------
function Reactors_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if Assigned(ActiveCircuit) then
        Result := ActiveCircuit.Reactors.ListSize;
end;
//------------------------------------------------------------------------------
function Reactors_Get_Bus1_AnsiString(): Ansistring; inline;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) = NIL then
        exit;
    Result := pReactor.GetBus(1);
end;

function Reactors_Get_Bus1(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Reactors_Get_Bus1_AnsiString());
end;
//------------------------------------------------------------------------------
function Reactors_Get_Bus2_AnsiString(): Ansistring; inline;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) = NIL then
        exit;
    Result := pReactor.GetBus(2);
end;

function Reactors_Get_Bus2(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Reactors_Get_Bus2_AnsiString());
end;
//------------------------------------------------------------------------------
function Reactors_Get_LCurve_AnsiString(): Ansistring; inline;
var
    pReactor: TReactorObj;
begin
    Result := '';
    if ActiveReactor(pReactor) = NIL then
        exit;
    Result := pReactor.LCurve;
end;

function Reactors_Get_LCurve(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Reactors_Get_LCurve_AnsiString());
end;
//------------------------------------------------------------------------------
function Reactors_Get_RCurve_AnsiString(): Ansistring; inline;
var
    pReactor: TReactorObj;
begin
    Result := '';
    if ActiveReactor(pReactor) = NIL then
        exit;
    Result := pReactor.RCurve;
end;

function Reactors_Get_RCurve(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Reactors_Get_RCurve_AnsiString());
end;
//------------------------------------------------------------------------------
function Reactors_Get_Parallel(): Boolean; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := FALSE;
    if ActiveReactor(pReactor) = NIL then
        exit;
    Result := pReactor.IsParallel;
end;
//------------------------------------------------------------------------------
function Reactors_Get_IsDelta(): Boolean; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := FALSE;
    if ActiveReactor(pReactor) = NIL then
        exit;
    if pReactor.Connection <> TReactorConnection.Wye then
        Result := TRUE;
end;
//------------------------------------------------------------------------------
function Reactors_Get_kV(): Double; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if ActiveReactor(pReactor) = NIL then
        exit;
    with pReactor do
    begin
        Result := kvrating;
    end
end;
//------------------------------------------------------------------------------
function Reactors_Get_kvar(): Double; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if ActiveReactor(pReactor) = NIL then
        exit;
    Result := pReactor.kvarRating;
end;
//------------------------------------------------------------------------------
function Reactors_Get_LmH(): Double; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if ActiveReactor(pReactor) = NIL then
        exit;
    Result := pReactor.L * 1000.0;
end;
//------------------------------------------------------------------------------
function Reactors_Get_Phases(): Integer; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0;
    if ActiveReactor(pReactor) = NIL then
        exit;
    Result := pReactor.Nphases;
end;
//------------------------------------------------------------------------------
function Reactors_Get_SpecType(): Integer; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0;
    if ActiveReactor(pReactor) = NIL then
        exit;
    Result := pReactor.SpecType;
end;
//------------------------------------------------------------------------------
function Reactors_Get_R(): Double; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if ActiveReactor(pReactor) = NIL then
        exit;
    Result := pReactor.R;
end;
//------------------------------------------------------------------------------
function Reactors_Get_X(): Double; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if ActiveReactor(pReactor) = NIL then
        exit;
    Result := pReactor.X;
end;
//------------------------------------------------------------------------------
function Reactors_Get_Rp(): Double; CDECL;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if ActiveReactor(pReactor) = NIL then
        exit;
    Result := pReactor.Rp;
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_IsDelta(Value: Boolean); CDECL;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) = NIL then
        exit;
    if Value then
        pReactor.Connection := TReactorConnection.Delta
    else    
        pReactor.Connection := TReactorConnection.Wye;
    ReactorPropSideEffects(TReactorProp.conn, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Parallel(Value: Boolean); CDECL;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) = NIL then
        exit;
    pReactor.IsParallel := Value;
    ReactorPropSideEffects(TReactorProp.Parallel, pReactor);
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
    if ActiveReactor(pReactor) = NIL then
        exit;
    _ReactorSetbus1(pReactor, Value);
    ReactorPropSideEffects(TReactorProp.bus1, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Bus2(const Value: PAnsiChar); CDECL;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) = NIL then
        exit;
    pReactor.SetBus(2, Value);
    ReactorPropSideEffects(TReactorProp.bus2, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_LCurve(const Value: PAnsiChar); CDECL;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) = NIL then
        exit;
    pReactor.LCurve := Value;
    ReactorPropSideEffects(TReactorProp.LCurve, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_RCurve(const Value: PAnsiChar); CDECL;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) = NIL then
        exit;
    pReactor.RCurve := Value;
    ReactorPropSideEffects(TReactorProp.RCurve, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_kV(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) = NIL then
        exit;
    pReactor.kvrating := Value;
    ReactorPropSideEffects(TReactorProp.kv, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_kvar(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) = NIL then
        exit;
    pReactor.kvarRating := Value;
    ReactorPropSideEffects(TReactorProp.kvar, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_LmH(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) = NIL then
        exit;
    pReactor.L := Value / 1000.0;
    ReactorPropSideEffects(TReactorProp.LmH, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Phases(Value: Integer); CDECL;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) = NIL then
        exit;
    if Value = pReactor.NPhases then
        exit;
    pReactor.NPhases := Value;
    ReactorPropSideEffects(TReactorProp.phases, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_R(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) = NIL then
        exit;
    pReactor.R := Value;
    ReactorPropSideEffects(TReactorProp.R, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_X(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) = NIL then
        exit;
    pReactor.X := Value;
    ReactorPropSideEffects(TReactorProp.X, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Rp(Value: Double); CDECL;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) = NIL then
        exit;
    pReactor.Rp := Value;
    ReactorPropSideEffects(TReactorProp.Rp, pReactor);
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Rmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    k: Integer;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray(ValuePtr);
    if (ActiveReactor(pReactor) <> NIL) and (Sqr(pReactor.Nphases) = ValueCount) then
        with pReactor do
        begin
            for k := 0 to ValueCount do
            begin
                Rmatrix[k + 1] := ValuePtr[k];
            end;
            ReactorPropSideEffects(TReactorProp.Rmatrix, pReactor);
        end;
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Xmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    k: Integer;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray(ValuePtr);
    if (ActiveReactor(pReactor) <> NIL) and (Sqr(pReactor.Nphases) = ValueCount) then
        with pReactor do
        begin
            for k := 0 to ValueCount do
            begin
                Xmatrix[k + 1] := ValuePtr[k];
            end;
            ReactorPropSideEffects(TReactorProp.Xmatrix, pReactor);
        end;
end;
//------------------------------------------------------------------------------
procedure Reactors_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    k: Integer;
    pReactor: TReactorObj;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if (ActiveReactor(pReactor) <> NIL) and (pReactor.Rmatrix <> NIL) then
        with pReactor do
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (Sqr(Nphases) - 1) + 1);
            for k := 0 to ResultCount[0] do
            begin
                ResultPtr[k] := Rmatrix[k + 1];
            end;
        end;
end;

procedure Reactors_Get_Rmatrix_GR(); CDECL;
// Same as Reactors_Get_Rmatrix but uses global result (GR) pointers
begin
    Reactors_Get_Rmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Reactors_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    k: Integer;
    pReactor: TReactorObj;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if (ActiveReactor(pReactor) <> NIL) and (pReactor.Xmatrix <> NIL) then
        with pReactor do
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (Sqr(Nphases) - 1) + 1);
            for k := 0 to ResultCount[0] do
            begin
                ResultPtr[k] := Xmatrix[k + 1];
            end;
        end;
end;

procedure Reactors_Get_Xmatrix_GR(); CDECL;
// Same as Reactors_Get_Xmatrix but uses global result (GR) pointers
begin
    Reactors_Get_Xmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Reactors_Get_Z1(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pReactor: TReactorObj;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if ActiveReactor(pReactor) = NIL then
        exit;
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
    Reactors_Get_Z1(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Reactors_Get_Z2(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pReactor: TReactorObj;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if ActiveReactor(pReactor) = NIL then
        exit;
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
    Reactors_Get_Z2(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Reactors_Get_Z0(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pReactor: TReactorObj;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if ActiveReactor(pReactor) = NIL then
        exit;
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
    Reactors_Get_Z0(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Reactors_Get_Z(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pReactor: TReactorObj;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if ActiveReactor(pReactor) = NIL then
        exit;
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
    Reactors_Get_Z(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Reactors_Set_Z2(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray(ValuePtr);
    if (ActiveReactor(pReactor) <> NIL) and (ValueCount = 2) then
        with pReactor do
        begin
            Z2 := Cmplx(Value[0], Value[1]);
            ReactorPropSideEffects(TReactorProp.Z2, pReactor);
        end;
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Z1(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray(ValuePtr);
    if (ActiveReactor(pReactor) <> NIL) and (ValueCount = 2) then
        with pReactor do
        begin
            Z1 := Cmplx(Value[0], Value[1]);
            ReactorPropSideEffects(TReactorProp.Z1, pReactor);
        end;
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Z0(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray(ValuePtr);
    if (ActiveReactor(pReactor) <> NIL) and (ValueCount = 2) then
        with pReactor do
        begin
            Z0 := Cmplx(Value[0], Value[1]);
            ReactorPropSideEffects(TReactorProp.Z0, pReactor);
        end;
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Z(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray(ValuePtr);
    if (ActiveReactor(pReactor) <> NIL) and (ValueCount = 2) then
        with pReactor do
        begin
            Z := Cmplx(Value[0], Value[1]);
            ReactorPropSideEffects(TReactorProp.Z, pReactor);
        end;
end;
//------------------------------------------------------------------------------
function Reactors_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Reactors.ActiveIndex
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_idx(Value: Integer); CDECL;
var
    pReactor: TReactorObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pReactor := ActiveCircuit.Reactors.Get(Value);
    if pReactor = NIL then
    begin
        DoSimpleMsg('Invalid Reactor index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := pReactor;
end;
//------------------------------------------------------------------------------
end.
