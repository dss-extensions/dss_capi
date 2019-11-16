unit CAPI_CktElement;

{$inline on}

interface

uses
    CAPI_Utils;

procedure CktElement_Get_BusNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure CktElement_Get_BusNames_GR(); CDECL;
function CktElement_Get_Name(): PAnsiChar; CDECL;
function CktElement_Get_NumConductors(): Integer; CDECL;
function CktElement_Get_NumPhases(): Integer; CDECL;
function CktElement_Get_NumTerminals(): Integer; CDECL;
procedure CktElement_Set_BusNames(ValuePtr: PPAnsiChar; ValueCount: Integer); CDECL;
procedure CktElement_Get_Currents(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure CktElement_Get_Currents_GR(); CDECL;
procedure CktElement_Get_Voltages(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure CktElement_Get_Voltages_GR(); CDECL;
function CktElement_Get_EmergAmps(): Double; CDECL;
function CktElement_Get_Enabled(): Boolean; CDECL;
procedure CktElement_Get_Losses(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure CktElement_Get_Losses_GR(); CDECL;
function CktElement_Get_NormalAmps(): Double; CDECL;
procedure CktElement_Get_PhaseLosses(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure CktElement_Get_PhaseLosses_GR(); CDECL;
procedure CktElement_Get_Powers(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure CktElement_Get_Powers_GR(); CDECL;
procedure CktElement_Get_SeqCurrents(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure CktElement_Get_SeqCurrents_GR(); CDECL;
procedure CktElement_Get_SeqPowers(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure CktElement_Get_SeqPowers_GR(); CDECL;
procedure CktElement_Get_SeqVoltages(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure CktElement_Get_SeqVoltages_GR(); CDECL;
procedure CktElement_Close(Term, Phs: Integer); CDECL;
procedure CktElement_Open(Term, Phs: Integer); CDECL;
procedure CktElement_Set_EmergAmps(Value: Double); CDECL;
procedure CktElement_Set_Enabled(Value: Boolean); CDECL;
procedure CktElement_Set_NormalAmps(Value: Double); CDECL;
function CktElement_IsOpen(Term, Phs: Integer): Boolean; CDECL;
procedure CktElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure CktElement_Get_AllPropertyNames_GR(); CDECL;
function CktElement_Get_NumProperties(): Integer; CDECL;
procedure CktElement_Get_Residuals(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure CktElement_Get_Residuals_GR(); CDECL;
procedure CktElement_Get_Yprim(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure CktElement_Get_Yprim_GR(); CDECL;
function CktElement_Get_DisplayName(): PAnsiChar; CDECL;
function CktElement_Get_GUID(): PAnsiChar; CDECL;
function CktElement_Get_Handle(): Integer; CDECL;
procedure CktElement_Set_DisplayName(const Value: PAnsiChar); CDECL;
function CktElement_Get_Controller(idx: Integer): PAnsiChar; CDECL;
function CktElement_Get_EnergyMeter(): PAnsiChar; CDECL;
function CktElement_Get_HasVoltControl(): Boolean; CDECL;
function CktElement_Get_HasSwitchControl(): Boolean; CDECL;
procedure CktElement_Get_CplxSeqVoltages(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure CktElement_Get_CplxSeqVoltages_GR(); CDECL;
procedure CktElement_Get_CplxSeqCurrents(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure CktElement_Get_CplxSeqCurrents_GR(); CDECL;
procedure CktElement_Get_AllVariableNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure CktElement_Get_AllVariableNames_GR(); CDECL;
procedure CktElement_Get_AllVariableValues(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure CktElement_Get_AllVariableValues_GR(); CDECL;
function CktElement_Get_Variable(const MyVarName: PAnsiChar; out Code: Integer): Double; CDECL;
function CktElement_Get_Variablei(Idx: Integer; out Code: Integer): Double; CDECL;
procedure CktElement_Get_NodeOrder(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
procedure CktElement_Get_NodeOrder_GR(); CDECL;
function CktElement_Get_HasOCPDevice(): Boolean; CDECL;
function CktElement_Get_NumControls(): Integer; CDECL;
function CktElement_Get_OCPDevIndex(): Integer; CDECL;
function CktElement_Get_OCPDevType(): Integer; CDECL;
procedure CktElement_Get_CurrentsMagAng(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure CktElement_Get_CurrentsMagAng_GR(); CDECL;
procedure CktElement_Get_VoltagesMagAng(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure CktElement_Get_VoltagesMagAng_GR(); CDECL;

// API Extensions
function CktElement_Get_IsIsolated(): Boolean; CDECL;

implementation

uses
    CAPI_Constants,
    DSSClassDefs,
    DSSGlobals,
    UComplex,
    Sysutils,
    PDElement,
    PCElement,
    MathUtil,
    CktElement,
    Utilities;

procedure CalcSeqCurrents(pActiveElement: TDSSCktElement; i012: pComplexArray);
{Assumes V012 is properly allocated before call.}
var
    Nvalues, i, j, k, iV: Integer;
    IPh, I012a: array[1..3] of Complex;
    cBuffer: pComplexArray;

begin
    with pActiveElement, ActiveCircuit[ActiveActor] do
    begin
        Nvalues := NPhases;
        if Nvalues <> 3 then
        begin
        {Handle non-3 phase elements}
            if (Nphases = 1) and PositiveSequence then
            begin
                NValues := NConds * NTerms;
                cBuffer := Allocmem(sizeof(Complex) * NValues);
                GetCurrents(cBuffer, ActiveActor);

                for i := 1 to 3 * NTerms do
                    i012^[i] := CZERO;   // Initialize Result
                iV := 2;  // pos seq is 2nd element in array
                {Populate only phase 1 quantities in Pos seq}
                for j := 1 to NTerms do
                begin
                    k := (j - 1) * NConds;
                    i012^[iV] := cBuffer^[1 + k];
                    Inc(iV, 3);  // inc to pos seq of next terminal
                end;
                Reallocmem(cBuffer, 0);
            end
           // if neither 3-phase or pos seq model, just put in -1.0 for each element
            else
                for i := 1 to 3 * NTerms do
                    i012^[i] := Cmplx(-1.0, 0.0);  // Signify n/A
        end
        else
        begin    // for 3-phase elements
            iV := 1;
            NValues := NConds * NTerms;
            cBuffer := Allocmem(sizeof(Complex) * NValues);
            GetCurrents(cBuffer, ActiveActor);
            for j := 1 to NTerms do
            begin
                k := (j - 1) * NConds;
                for i := 1 to 3 do
                    Iph[i] := cBuffer^[k + i];
                Phase2SymComp(@Iph, @I012a);

                for i := 1 to 3 do
                begin     // Stuff it in the result array
                    i012^[iV] := i012a[i];
                    Inc(iV);
                end;
            end;
            Reallocmem(cBuffer, 0);
        end;
    end;
end;


{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
procedure CalcSeqVoltages(pActiveElement: TDSSCktElement; V012: pComplexArray);
{Assumes V012 is properly allocated before call.}
var
    Nvalues, i, j, k, iV: Integer;
    VPh, V012a: array[1..3] of Complex;
begin
    with pActiveElement, ActiveCircuit[ActiveActor] do
    begin
        Nvalues := NPhases;
        if Nvalues <> 3 then
        begin
        {Handle non-3 phase elements}
            if (Nphases = 1) and PositiveSequence then
            begin
                for i := 1 to 3 * NTerms do
                    V012^[i] := CZERO;   // Initialize Result
                iV := 2;  // pos seq is 2nd element in array
                {Populate only phase 1 quantities in Pos seq}
                for j := 1 to NTerms do
                begin
                    k := (j - 1) * NConds;
                    V012^[iV] := Solution.NodeV^[NodeRef^[1 + k]];
                    Inc(iV, 3);  // inc to pos seq of next terminal
                end;
            end
           // if neither 3-phase or pos seq model, just put in -1.0 for each element
            else
                for i := 1 to 3 * NTerms do
                    V012^[i] := Cmplx(-1.0, 0.0);  // Signify n/A
        end
        else
        begin    // for 3-phase elements
            iV := 1;
            for j := 1 to NTerms do
            begin
                k := (j - 1) * NConds;
                for i := 1 to 3 do
                    Vph[i] := Solution.NodeV^[NodeRef^[i + k]];
                Phase2SymComp(@Vph, @V012a);   // Compute Symmetrical components

                for i := 1 to 3 do
                begin     // Stuff it in the result array
                    V012^[iV] := V012a[i];
                    Inc(iV);
                end;
            end;
        end;
    end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
function IsPDElement: Boolean;
begin
    Result := ((ActiveCircuit[ActiveActor].ActiveCktElement.DSSObjType and 3) = PD_ELEMENT)
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
procedure CktElement_Get_BusNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    i: Integer;
begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        with ActiveCircuit[ActiveActor] do
        begin
            Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (ActiveCktElement.Nterms - 1) + 1);
            for i := 1 to ActiveCktElement.Nterms do
            begin
                Result[i - 1] := DSS_CopyStringAsPChar(ActiveCktElement.GetBus(i));
            end;
        end;
    end
    else
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure CktElement_Get_BusNames_GR(); CDECL;
// Same as CktElement_Get_BusNames but uses global result (GR) pointers
begin
    CktElement_Get_BusNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function CktElement_Get_Name_AnsiString(): Ansistring; inline;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor].ActiveCktElement do
        begin
            Result := ParentClass.Name + '.' + Name;
        end
    else
        Result := '';
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

function CktElement_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(CktElement_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function CktElement_Get_NumConductors(): Integer; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].ActiveCktElement.NConds
    else
        Result := 0;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
function CktElement_Get_NumPhases(): Integer; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].ActiveCktElement.NPhases
    else
        Result := 0;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
function CktElement_Get_NumTerminals(): Integer; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].ActiveCktElement.NTerms
    else
        Result := 0;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
procedure CktElement_Set_BusNames(ValuePtr: PPAnsiChar; ValueCount: Integer); CDECL;
var
    Value: PPAnsiCharArray;
    i: Integer;
    Count, Low: Integer;
begin
    Value := PPAnsiCharArray(ValuePtr);

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        with ActiveCircuit[ActiveActor] do
        begin
            Low := (0);
            Count := (ValueCount - 1) - Low + 1;
            if Count > ActiveCktElement.NTerms then
                Count := ActiveCktElement.NTerms;
            for i := 1 to Count do
            begin
                ActiveCktElement.SetBus(i, Value[i - 1 + Low]);
            end;
        end;
    end;
end;


{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
procedure CktElement_Get_Currents(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    cBuffer: pComplexArray;
    NValues, iV, i: Integer;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor].ActiveCktElement do
        begin
            NValues := NConds * NTerms;
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2 * NValues - 1) + 1);
            cBuffer := Allocmem(sizeof(Complex) * NValues);
            GetCurrents(cBuffer, ActiveActor);
            iV := 0;
            for i := 1 to NValues do
            begin
                Result[iV] := cBuffer^[i].re;
                Inc(iV);
                Result[iV] := cBuffer^[i].im;
                Inc(iV);
            end;
            Reallocmem(cBuffer, 0);
        end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure CktElement_Get_Currents_GR(); CDECL;
// Same as CktElement_Get_Currents but uses global result (GR) pointers
begin
    CktElement_Get_Currents(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_Voltages(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
// Bus Voltages at active terminal
var
    Result: PDoubleArray;
    numcond, i, n, iV: Integer;
    Volts: Complex;

begin

// Return voltages for all terminals

    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    numcond := NConds * Nterms;
                    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2 * numcond - 1) + 1);
         // k := (Terminal-1)*numcond;    // RCD 8-30-00 Changed
                    iV := 0;
                    for i := 1 to numcond do
                    begin
                        n := ActiveCktElement.NodeRef^[i];
                        Volts := Solution.NodeV^[n]; // ok if =0
                        Result[iV] := Volts.re;
                        Inc(iV);
                        Result[iV] := Volts.im;
                        Inc(iV);
                    end;
                end;
        end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure CktElement_Get_Voltages_GR(); CDECL;
// Same as CktElement_Get_Voltages but uses global result (GR) pointers
begin
    CktElement_Get_Voltages(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function CktElement_Get_EmergAmps(): Double; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT then
            begin
                with ActiveCktElement as TPDElement do
                    Result := EmergAmps;
                Exit;
            end
        end;
    Result := 0.0;
end;

//------------------------------------------------------------------------------
function CktElement_Get_Enabled(): Boolean; CDECL;
begin

    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].ActiveCktElement.Enabled
    else
        Result := FALSE;

end;

//------------------------------------------------------------------------------
procedure CktElement_Get_Losses(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    LossValue: complex;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
            begin
                Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
                LossValue := ActiveCktElement.Losses[ActiveActor];
                Result[0] := LossValue.re;
                Result[1] := LossValue.im;
                Exit;
            end;
        end;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
    Result[0] := 0;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure CktElement_Get_Losses_GR(); CDECL;
// Same as CktElement_Get_Losses but uses global result (GR) pointers
begin
    CktElement_Get_Losses(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function CktElement_Get_NormalAmps(): Double; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT then
            begin
                with ActiveCktElement as TPDElement do
                    Result := NormAmps;

                Exit;
            end
        end;
    Result := 0.0;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
procedure CktElement_Get_PhaseLosses(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
// Returns Phase losses in kW, kVar
var
    Result: PDoubleArray;
    cBuffer: pComplexArray;
    NValues, i, iV: Integer;

begin


    if ActiveCircuit[ActiveActor] <> NIL then

        with ActiveCircuit[ActiveActor].ActiveCktElement do
        begin
            NValues := NPhases;
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2 * NValues - 1) + 1);
            cBuffer := Allocmem(sizeof(Complex) * NValues);
            GetPhaseLosses(NValues, cBuffer, ActiveActor);
            iV := 0;
            for i := 1 to NValues do
            begin
                Result[iV] := cBuffer^[i].re * 0.001;
                Inc(iV);
                Result[iV] := cBuffer^[i].im * 0.001;
                Inc(iV);
            end;
            Reallocmem(cBuffer, 0);
        end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure CktElement_Get_PhaseLosses_GR(); CDECL;
// Same as CktElement_Get_PhaseLosses but uses global result (GR) pointers
begin
    CktElement_Get_PhaseLosses(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_Powers(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
// Return complex kW, kvar in each conductor for each terminal
var
    Result: PDoubleArray;
    NValues,
    i: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor].ActiveCktElement do
        begin
            NValues := NConds * Nterms;
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues);
            GetPhasePower(pComplexArray(ResultPtr), ActiveActor);
            for i := 0 to (2 * NValues) - 1 do
            begin
                Result[i] := Result[i] * 0.001;
            end;
        end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure CktElement_Get_Powers_GR(); CDECL;
// Same as CktElement_Get_Powers but uses global result (GR) pointers
begin
    CktElement_Get_Powers(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_SeqCurrents(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
// All sequence currents of active ciruit element
// returns magnitude only.
var
    Result: PDoubleArray;
    i: Integer;
    i012: pComplexArray;
    S: String;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                    if Enabled then
                    begin
                        try
                            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (3 * NTerms - 1) + 1);

                            i012 := Allocmem(sizeof(Complex) * 3 * Nterms);
            // get complex seq voltages
                            CalcSeqCurrents(ActiveCktElement, i012);
            // return 0 based array
                            for i := 1 to 3 * Nterms do
                                Result[i - 1] := Cabs(i012^[i]);  // return mag only

                            Reallocmem(i012, 0);  // throw away temp memory

                        except
                            On E: Exception do
                            begin
                                S := E.message + CRLF +
                                    'Element=' + ActiveCktElement.Name + CRLF +
                                    'Nphases=' + IntToStr(Nphases) + CRLF +
                                    'NTerms=' + IntToStr(NTerms) + CRLF +
                                    'NConds =' + IntToStr(NConds);
                                DoSimpleMsg(S, 5012);
                            end;
                        end;
                    end
                    else
                        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // Disabled

        end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure CktElement_Get_SeqCurrents_GR(); CDECL;
// Same as CktElement_Get_SeqCurrents but uses global result (GR) pointers
begin
    CktElement_Get_SeqCurrents(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_SeqPowers(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
// All seq Powers of active 3-phase ciruit element
// returns kW + j kvar
var
    Result: PDoubleArray;
    Nvalues, i, j, k, n, icount: Integer;
    S: Complex;
    VPh, V012: array[1..3] of Complex;
    IPh, I012: array[1..3] of Complex;
    cBuffer: pComplexArray;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2 * 3 * NTerms - 1) + 1); // allocate for kW and kvar
                    if NPhases <> 3 then
                    begin
                        if (Nphases = 1) and PositiveSequence then
                        begin
                            NValues := NConds * NTerms;
                            cBuffer := Allocmem(sizeof(Complex) * NValues);
                            GetCurrents(cBuffer, ActiveActor);

                            for i := 0 to 2 * 3 * NTerms - 1 do
                                Result[i] := 0.0;   // Initialize Result
                            iCount := 2;  // Start with kW1
                {Put only phase 1 quantities in Pos seq}
                            for j := 1 to NTerms do
                            begin
                                k := (j - 1) * NConds;
                                n := NodeRef^[k + 1];
                                Vph[1] := Solution.NodeV^[n];  // Get voltage at node
                                S := Cmul(Vph[1], conjg(cBuffer^[k + 1]));   // Compute power per phase
                                Result[icount] := S.re * 0.003; // 3-phase kW conversion
                                inc(icount);
                                Result[icount] := S.im * 0.003; // 3-phase kvar conversion
                                inc(icount, 6);
                            end;
                            Reallocmem(cBuffer, 0);
                        end

                        else
                            for i := 0 to 2 * 3 * NTerms - 1 do
                                Result[i] := -1.0;  // Signify n/A
                    end
                    else
                    begin
                        NValues := NConds * NTerms;
                        cBuffer := Allocmem(sizeof(Complex) * NValues);
                        GetCurrents(cBuffer, ActiveActor);
                        icount := 0;
                        for j := 1 to NTerms do
                        begin
                            k := (j - 1) * NConds;
                            for i := 1 to 3 do
                                Vph[i] := Solution.NodeV^[NodeRef^[i + k]];
                            for i := 1 to 3 do
                                Iph[i] := cBuffer^[k + i];
                            Phase2SymComp(@Iph, @I012);
                            Phase2SymComp(@Vph, @V012);
                            for i := 1 to 3 do
                            begin
                                S := Cmul(V012[i], conjg(I012[i]));
                                Result[icount] := S.re * 0.003; // 3-phase kW conversion
                                inc(icount);
                                Result[icount] := S.im * 0.003; // 3-phase kW conversion
                                inc(icount);
                            end;
                        end;
                        Reallocmem(cBuffer, 0);
                    end;
                end;
        end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure CktElement_Get_SeqPowers_GR(); CDECL;
// Same as CktElement_Get_SeqPowers but uses global result (GR) pointers
begin
    CktElement_Get_SeqPowers(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_SeqVoltages(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
// All voltages of active ciruit element
// magnitude only
// returns a set of seq voltages (3) for each terminal
// 0, 1, 2 sequence  (0, +, -)
var
    Result: PDoubleArray;
    i: Integer;
    V012: pComplexArray;
    S: String;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                    if Enabled then
                    begin
                        try
                            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (3 * NTerms - 1) + 1);

                            V012 := Allocmem(sizeof(Complex) * 3 * Nterms);
            // get complex seq voltages
                            CalcSeqVoltages(ActiveCktElement, V012);
            // return 0 based array
                            for i := 1 to 3 * Nterms do
                                Result[i - 1] := Cabs(V012^[i]);  // return mag only

                            Reallocmem(V012, 0);  // throw away temp memory

                        except
                            On E: Exception do
                            begin
                                S := E.message + CRLF +
                                    'Element=' + ActiveCktElement.Name + CRLF +
                                    'Nphases=' + IntToStr(Nphases) + CRLF +
                                    'NTerms=' + IntToStr(NTerms) + CRLF +
                                    'NConds =' + IntToStr(NConds);
                                DoSimpleMsg(S, 5012);
                            end;
                        end;
                    end
                    else
                        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // Disabled

        end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure CktElement_Get_SeqVoltages_GR(); CDECL;
// Same as CktElement_Get_SeqVoltages but uses global result (GR) pointers
begin
    CktElement_Get_SeqVoltages(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure CktElement_Close(Term, Phs: Integer); CDECL;
begin

    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    ActiveTerminal := Terminals^[Term];
                    Closed[Phs, ActiveActor] := TRUE;
                end;
        end;

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
procedure CktElement_Open(Term, Phs: Integer); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    ActiveTerminal := Terminals^[Term];
                    Closed[Phs, ActiveActor] := FALSE;
                end;
        end;

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
procedure CktElement_Set_EmergAmps(Value: Double); CDECL;
begin

    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if IsPDElement then
            begin
                with ActiveCktElement as TPDElement do
                    EmergAmps := Value;
            end;  {Else Do Nothing}
        end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
procedure CktElement_Set_Enabled(Value: Boolean); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].ActiveCktElement.Enabled := Value;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
//------------------------------------------------------------------------------
procedure CktElement_Set_NormalAmps(Value: Double); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        if IsPDElement then
        begin
            with ActiveCircuit[ActiveActor] do
                with ActiveCktElement as TPDElement do
                    NormAmps := Value;
        end;  {Else Do Nothing}
    end;
end;
//------------------------------------------------------------------------------
function CktElement_IsOpen(Term, Phs: Integer): Boolean; CDECL;
var
    i: Integer;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            with ActiveCktElement do
                ActiveTerminal := Terminals^[Term];
            if Phs = 0 then // At least one must be open
            begin
                Result := FALSE;
                for i := 1 to ActiveCktElement.NConds do
                    if not ActiveCktElement.Closed[i, ActiveActor] then
                    begin
                        Result := TRUE;
                        Exit;
                    end;
            end
            else // Check a specific phase or conductor
            begin
                Result := not ActiveCktElement.Closed[Phs, ActiveActor];
            end;
        end;
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    k: Integer;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    with ParentClass do
                    begin
                        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumProperties - 1) + 1);
                        for k := 1 to NumProperties do
                        begin
                            Result[k - 1] := DSS_CopyStringAsPChar(PropertyName^[k]);
                        end;
                    end;
                end
        end;
end;

procedure CktElement_Get_AllPropertyNames_GR(); CDECL;
// Same as CktElement_Get_AllPropertyNames but uses global result (GR) pointers
begin
    CktElement_Get_AllPropertyNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function CktElement_Get_NumProperties(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    Result := ParentClass.NumProperties;
                end
        end;


end;
//------------------------------------------------------------------------------
procedure CktElement_Get_Residuals(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    cBuffer: pComplexArray;
    iV, i, j, k: Integer;
    cResid: Complex;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor].ActiveCktElement do
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2 * NTerms - 1) + 1);    // 2 values per terminal
            cBuffer := Allocmem(sizeof(Complex) * Yorder);
            GetCurrents(cBuffer, ActiveActor);
            iV := 0;
            for i := 1 to NTerms do
            begin
                cResid := CZERO;
                k := (i - 1) * Nconds;
                for j := 1 to Nconds do
                begin
                    inc(k);
                    Caccum(cResid, CBuffer^[k]);
                end;
                Result[iV] := Cabs(cResid);
                Inc(iV);
                Result[iV] := CDang(cResid);
                Inc(iV);
            end;
            Reallocmem(cBuffer, 0);
        end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;

procedure CktElement_Get_Residuals_GR(); CDECL;
// Same as CktElement_Get_Residuals but uses global result (GR) pointers
begin
    CktElement_Get_Residuals(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_Yprim(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
{ Return the YPrim matrix for this element }

var
    Result: PDoubleArray;
    iV: Integer;
    i: Integer;
    NValues: Integer;
    cValues: pComplexArray;

begin
    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    end
    else
        with ActiveCircuit[ActiveActor] do
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    NValues := SQR(Yorder);
                    cValues := GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
                    if cValues = NIL then
                    begin   // check for unassigned array
                        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // just return null array
                        Exit;  // Get outta here
                    end;
                    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2 * NValues - 1) + 1);  // Make variant array
                    iV := 0;

                    for i := 1 to NValues do
                    begin    // Plunk the values in the variant array
                        Result[iV] := cValues^[i].re;
                        Inc(iV);
                        Result[iV] := cValues^[i].im;
                        Inc(iV);
                    end;
                end
            else
                Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // just return null array

end;

procedure CktElement_Get_Yprim_GR(); CDECL;
// Same as CktElement_Get_Yprim but uses global result (GR) pointers
begin
    CktElement_Get_Yprim(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function CktElement_Get_DisplayName_AnsiString(): Ansistring; inline;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].ActiveCktElement.DisplayName
    else
        Result := '';
end;

function CktElement_Get_DisplayName(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(CktElement_Get_DisplayName_AnsiString());
end;
//------------------------------------------------------------------------------
function CktElement_Get_GUID_AnsiString(): Ansistring; inline;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].ActiveCktElement.ID
    else
        Result := '';
end;

function CktElement_Get_GUID(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(CktElement_Get_GUID_AnsiString());
end;
//------------------------------------------------------------------------------
function CktElement_Get_Handle(): Integer; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].ActiveCktElement.Handle
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_DisplayName(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].ActiveCktElement.DisplayName := Value;
end;
//------------------------------------------------------------------------------
function CktElement_Get_Controller_AnsiString(idx: Integer): Ansistring; inline;
var
    ctrl: TDSSCktElement;
begin
    Result := '';
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if (idx > 0) and (idx <= ActiveCktElement.ControlElementList.Listsize) then
            begin
                ctrl := ActiveCktElement.ControlElementList.Get(idx);
                if ctrl <> NIL then
                    Result := Format('%s.%s', [ctrl.ParentClass.Name, ctrl.Name]);
            end;
        end;
end;

function CktElement_Get_Controller(idx: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(CktElement_Get_Controller_AnsiString(idx));
end;
//------------------------------------------------------------------------------
function CktElement_Get_EnergyMeter_AnsiString(): Ansistring; inline;
var
    pd: TPDElement;
begin
    Result := '';
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        if ActiveCircuit[ActiveActor].ActiveCktElement.HasEnergyMeter then
        begin
            pd := ActiveCircuit[ActiveActor].ActiveCktElement as TPDElement;
            Result := pd.MeterObj.Name;
        end;
    end;
end;

function CktElement_Get_EnergyMeter(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(CktElement_Get_EnergyMeter_AnsiString());
end;
//------------------------------------------------------------------------------
function CktElement_Get_HasVoltControl(): Boolean; CDECL;
// Returns true if any of the controls is a capcontrol or a regcontrol
var
    ctrl: TDSSCktElement;
begin
    Result := FALSE;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ctrl := ActiveCircuit[ActiveActor].ActiveCktElement.ControlElementlist.First;
        while ctrl <> NIL do
        begin
            case (ctrl.DSSObjType and CLASSMASK) of
                CAP_CONTROL,
                REG_CONTROL:
                    Result := TRUE;
            else
                Result := FALSE;
            end;
            if Result then
                Exit;

            ctrl := ActiveCircuit[ActiveActor].ActiveCktElement.ControlElementlist.Next;
        end;
    end;
end;
//------------------------------------------------------------------------------
function CktElement_Get_HasSwitchControl(): Boolean; CDECL;
var
    ctrl: TDSSCktElement;
begin
    Result := FALSE;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ctrl := ActiveCircuit[ActiveActor].ActiveCktElement.ControlElementList.First;
        while ctrl <> NIL do
        begin
            case (ctrl.DSSObjType and CLASSMASK) of
                SWT_CONTROL:
                    Result := TRUE;
            else
                Result := FALSE;
            end;
            if Result then
                Exit;

            ctrl := ActiveCircuit[ActiveActor].ActiveCktElement.ControlElementlist.Next;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_CplxSeqVoltages(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
{returns Seq Voltages as array of complex values}
var
    Result: PDoubleArray;
    i, iV: Integer;
    V012: pComplexArray;
    S: String;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                    if Enabled then
                    begin
                        try
                            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2 * 3 * NTerms - 1) + 1);

                            V012 := Allocmem(sizeof(Complex) * 3 * Nterms);
            // get complex seq voltages
                            CalcSeqVoltages(ActiveCktElement, V012);
            // return 0 based array
                            iV := 0;
                            for i := 1 to 3 * Nterms do
                            begin
                                Result[iV] := V012^[i].re;
                                inc(iV);
                                Result[iV] := V012^[i].im;
                                inc(iV);
                            end;

                            Reallocmem(V012, 0);  // throw away temp memory

                        except
                            On E: Exception do
                            begin
                                S := E.message + CRLF +
                                    'Element=' + ActiveCktElement.Name + CRLF +
                                    'Nphases=' + IntToStr(Nphases) + CRLF +
                                    'NTerms=' + IntToStr(NTerms) + CRLF +
                                    'NConds =' + IntToStr(NConds);
                                DoSimpleMsg(S, 5012);
                            end;
                        end;
                    end
                    else
                        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // Disabled

        end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;

procedure CktElement_Get_CplxSeqVoltages_GR(); CDECL;
// Same as CktElement_Get_CplxSeqVoltages but uses global result (GR) pointers
begin
    CktElement_Get_CplxSeqVoltages(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_CplxSeqCurrents(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
{returns Seq Voltages as array of complex values}
var
    Result: PDoubleArray;
    i, iV: Integer;
    i012: pComplexArray;
    S: String;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                    if Enabled then
                    begin
                        try
                            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2 * 3 * NTerms - 1) + 1);

                            i012 := Allocmem(sizeof(Complex) * 3 * Nterms);
            // get complex seq voltages
                            CalcSeqCurrents(ActiveCktElement, i012);
            // return 0 based array
                            iV := 0;
                            for i := 1 to 3 * Nterms do
                            begin
                                Result[iV] := i012^[i].re;
                                inc(iV);
                                Result[iV] := i012^[i].im;
                                inc(iV);
                            end;

                            Reallocmem(i012, 0);  // throw away temp memory

                        except
                            On E: Exception do
                            begin
                                S := E.message + CRLF +
                                    'Element=' + ActiveCktElement.Name + CRLF +
                                    'Nphases=' + IntToStr(Nphases) + CRLF +
                                    'NTerms=' + IntToStr(NTerms) + CRLF +
                                    'NConds =' + IntToStr(NConds);
                                DoSimpleMsg(S, 5012);
                            end;
                        end;
                    end
                    else
                        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);  // Disabled

        end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);


end;

procedure CktElement_Get_CplxSeqCurrents_GR(); CDECL;
// Same as CktElement_Get_CplxSeqCurrents but uses global result (GR) pointers
begin
    CktElement_Get_CplxSeqCurrents(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_AllVariableNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    k: Integer;
    pPCElem: TPCElement;

begin

    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin

                    if (DSSObjType and BASECLASSMASK) = PC_ELEMENT then
                    begin
                        pPCElem := (ActiveCktElement as TPCElement);
                        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (pPCElem.NumVariables - 1) + 1);
                        for k := 1 to pPCElem.NumVariables do
                        begin
                            Result[k - 1] := DSS_CopyStringAsPChar(pPCElem.VariableName(k));
                        end;
                    end;

         {Else zero-length array null string}
                end
        end;

end;

procedure CktElement_Get_AllVariableNames_GR(); CDECL;
// Same as CktElement_Get_AllVariableNames but uses global result (GR) pointers
begin
    CktElement_Get_AllVariableNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_AllVariableValues(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
{Return array of doubles with values of all variables if PCElement}
var
    Result: PDoubleArray;
    k: Integer;
    pPCElem: TPCElement;

begin

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin

                    if (DSSObjType and BASECLASSMASK) = PC_ELEMENT then
                    begin
                        pPCElem := (ActiveCktElement as TPCElement);
                        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (pPCElem.NumVariables - 1) + 1);
                        for k := 1 to pPCElem.NumVariables do
                        begin
                            Result[k - 1] := pPCElem.Variable[k];
                        end;
                    end;

         {Else zero-length array null string}
                end
        end;

end;

procedure CktElement_Get_AllVariableValues_GR(); CDECL;
// Same as CktElement_Get_AllVariableValues but uses global result (GR) pointers
begin
    CktElement_Get_AllVariableValues(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function CktElement_Get_Variable(const MyVarName: PAnsiChar; out Code: Integer): Double; CDECL;
var
    pPCElem: TPCElement;
    VarIndex: Integer;

begin
    Result := 0.0;
    Code := 1; // Signifies an error; no value set
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin

                    if (DSSObjType and BASECLASSMASK) = PC_ELEMENT then
                    begin
                        pPCElem := (ActiveCktElement as TPCElement);
                        VarIndex := pPCElem.LookupVariable(MyVarName);
                        if (VarIndex > 0) and (VarIndex <= pPCElem.NumVariables) then
                        begin
                            Result := pPCElem.Variable[VarIndex];
                            Code := 0;  // Signify result is OK.
                        end;
                    end;

         {Else zero-length array null string}
                end
        end;

end;
//------------------------------------------------------------------------------
function CktElement_Get_Variablei(Idx: Integer; out Code: Integer): Double; CDECL;
{Get Value of a variable by index}
var
    pPCElem: TPCElement;

begin
    Result := 0.0;
    Code := 1; // Signifies an error; no value set
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin

                    if (DSSObjType and BASECLASSMASK) = PC_ELEMENT then
                    begin
                        pPCElem := (ActiveCktElement as TPCElement);
                        if (Idx > 0) and (Idx <= pPCElem.NumVariables) then
                        begin
                            Result := pPCElem.Variable[Idx];
                            Code := 0;  // Signify result is OK.
                        end;
                    end;

         {Else zero-length array null string}
                end
        end;

end;
//------------------------------------------------------------------------------
procedure CktElement_Get_NodeOrder(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
var
    Result: PIntegerArray;
    k: Integer;
    i: Integer;
    j: Integer;
begin

    Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (0) + 1);
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin

            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    k := 0;
                    Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (NTerms * Nconds - 1) + 1);

                    for i := 1 to Nterms do
                    begin
                        for j := (i - 1) * NConds + 1 to i * Nconds do
                        begin
                            Result[k] := GetNodeNum(NodeRef^[j]);
                            inc(k);
                        end;
                    end;
                end
        end;


end;

procedure CktElement_Get_NodeOrder_GR(); CDECL;
// Same as CktElement_Get_NodeOrder but uses global result (GR) pointers
begin
    CktElement_Get_NodeOrder(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
function CktElement_Get_HasOCPDevice(): Boolean; CDECL;
// Check for presence of a fuse, recloser, etc.
begin
    Result := FALSE;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].ActiveCktElement.HasOCPDevice;
    end;
end;
//------------------------------------------------------------------------------
function CktElement_Get_NumControls(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].ActiveCktElement.ControlElementList.listSize;
    end;
end;
//------------------------------------------------------------------------------
function CktElement_Get_OCPDevIndex(): Integer; CDECL;
var
    iControl: Integer;
    pCktElement: TDSSCktElement;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            iControl := 1;
            repeat
           // cycle through the list of controls until we find a fuse, recloser, or relay
                pCktElement := ActiveCktElement.ControlElementList.Get(iControl);
                if pCktElement <> NIL then
                    case (pCktElement.DSSObjType and CLASSMASK) of

                        FUSE_CONTROL:
                            Result := iControl;
                        RECLOSER_CONTROL:
                            Result := iControl;
                        RELAY_CONTROL:
                            Result := iControl;

                    end;
                inc(iControl);
            until (iControl > ActiveCktElement.ControlElementList.listSize) or (Result > 0);
        end;
end;
//------------------------------------------------------------------------------
function CktElement_Get_OCPDevType(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            Result := GetOCPDeviceType(ActiveCktElement);     // see Utilities.pas
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_CurrentsMagAng(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
// return currents in magnitude, angle array
var
    Result: PDoubleArray;
    cBuffer: pComplexArray;
    CMagAng: polar;
    NValues, iV, i: Integer;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor].ActiveCktElement do
        begin
            NValues := NConds * NTerms;
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2 * NValues - 1) + 1);
            cBuffer := Allocmem(sizeof(Complex) * NValues);
            GetCurrents(cBuffer, ActiveActor);
            iV := 0;
            for i := 1 to NValues do
            begin
                CMagAng := ctopolardeg(cBuffer^[i]); // convert to mag/angle
                Result[iV] := CMagAng.mag;
                Inc(iV);
                Result[iV] := CMagAng.ang;
                Inc(iV);
            end;
            Reallocmem(cBuffer, 0);
        end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;

procedure CktElement_Get_CurrentsMagAng_GR(); CDECL;
// Same as CktElement_Get_CurrentsMagAng but uses global result (GR) pointers
begin
    CktElement_Get_CurrentsMagAng(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_VoltagesMagAng(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
// Bus Voltages in magnitude, angle at all terminal
var
    Result: PDoubleArray;
    numcond, i, n, iV: Integer;
    Volts: Polar;

begin

// Return voltages for all terminals

    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    numcond := NConds * Nterms;
                    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2 * numcond - 1) + 1);
         // k := (Terminal-1)*numcond;    // RCD 8-30-00 Changed
                    iV := 0;
                    for i := 1 to numcond do
                    begin
                        n := ActiveCktElement.NodeRef^[i];
                        Volts := ctopolardeg(Solution.NodeV^[n]); // ok if =0
                        Result[iV] := Volts.mag;
                        Inc(iV);
                        Result[iV] := Volts.ang;
                        Inc(iV);
                    end;
                end;
        end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;

procedure CktElement_Get_VoltagesMagAng_GR(); CDECL;
// Same as CktElement_Get_VoltagesMagAng but uses global result (GR) pointers
begin
    CktElement_Get_VoltagesMagAng(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;
//------------------------------------------------------------------------------
function CktElement_Get_IsIsolated(): Boolean; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].ActiveCktElement.IsIsolated
    else
        Result := FALSE;
end;
//------------------------------------------------------------------------------
end.
