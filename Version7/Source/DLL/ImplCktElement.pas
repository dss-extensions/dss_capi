unit ImplCktElement;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   5-17-00 Fixed bug in SeqCurrents and SeqPowers with location of Reallocmem
   9-1-13 Added NodeOrder  array that corresponds to voltages, currents, powers
}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSEngine_TLB,
    StdVcl;

type
    TCktElement = class(TAutoObject, ICktElement)
    PROTECTED
        function Get_BusNames: Olevariant; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_NumConductors: Integer; SAFECALL;
        function Get_NumPhases: Integer; SAFECALL;
        function Get_NumTerminals: Integer; SAFECALL;
        function Get_Properties(Index: Olevariant): IDSSProperty; SAFECALL;
        procedure Set_BusNames(Value: Olevariant); SAFECALL;

        function Get_Currents: Olevariant; SAFECALL;
        function Get_Voltages: Olevariant; SAFECALL;
        function Get_EmergAmps: Double; SAFECALL;
        function Get_Enabled: Wordbool; SAFECALL;
        function Get_Losses: Olevariant; SAFECALL;
        function Get_NormalAmps: Double; SAFECALL;
        function Get_PhaseLosses: Olevariant; SAFECALL;
        function Get_Powers: Olevariant; SAFECALL;
        function Get_SeqCurrents: Olevariant; SAFECALL;
        function Get_SeqPowers: Olevariant; SAFECALL;
        function Get_SeqVoltages: Olevariant; SAFECALL;
        procedure Close(Term, Phs: Integer); SAFECALL;
        procedure Open(Term, Phs: Integer); SAFECALL;
        procedure Set_EmergAmps(Value: Double); SAFECALL;
        procedure Set_Enabled(Value: Wordbool); SAFECALL;
        procedure Set_NormalAmps(Value: Double); SAFECALL;
        function IsOpen(Term, Phs: Integer): Wordbool; SAFECALL;
        function Get_AllPropertyNames: Olevariant; SAFECALL;
        function Get_NumProperties: Integer; SAFECALL;
        function Get_Residuals: Olevariant; SAFECALL;
        function Get_Yprim: Olevariant; SAFECALL;
        function Get_DisplayName: Widestring; SAFECALL;
        function Get_GUID: Widestring; SAFECALL;
        function Get_Handle: Integer; SAFECALL;
        procedure Set_DisplayName(const Value: Widestring); SAFECALL;
        function Get_Controller(idx: Integer): Widestring; SAFECALL;
        function Get_EnergyMeter: Widestring; SAFECALL;
        function Get_HasVoltControl: Wordbool; SAFECALL;
        function Get_HasSwitchControl: Wordbool; SAFECALL;
        function Get_CplxSeqVoltages: Olevariant; SAFECALL;
        function Get_CplxSeqCurrents: Olevariant; SAFECALL;
        function Get_AllVariableNames: Olevariant; SAFECALL;
        function Get_AllVariableValues: Olevariant; SAFECALL;
        function Get_Variable(const MyVarName: Widestring; out Code: Integer): Double; SAFECALL;
        function Get_Variablei(Idx: Integer; out Code: Integer): Double; SAFECALL;
        function Get_NodeOrder: Olevariant; SAFECALL;
        function Get_HasOCPDevice: Wordbool; SAFECALL;
        function Get_NumControls: Integer; SAFECALL;
        function Get_OCPDevIndex: Integer; SAFECALL;
        function Get_OCPDevType: Integer; SAFECALL;
        function Get_CurrentsMagAng: Olevariant; SAFECALL;
        function Get_VoltagesMagAng: Olevariant; SAFECALL;
    end;

implementation

uses
    ComServ,
    DSSClassDefs,
    DSSGlobals,
    UComplex,
    Sysutils,
    PDElement,
    PCElement,
    MathUtil,
    ImplGlobals,
    Variants,
    CktElement,
    Utilities;

{ - - - - - - - - - - - - -Helper Function- - - - - - - - - - - - - - - - - - -}
function IsPDElement: Boolean;
begin
    Result := ((ActiveCircuit.ActiveCktElement.DSSObjType and 3) = PD_ELEMENT)
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_BusNames: Olevariant;

var
    i: Integer;
begin

    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit do
        begin
            Result := VarArrayCreate([0, ActiveCktElement.Nterms - 1], varOleStr);
            for i := 1 to ActiveCktElement.Nterms do
            begin
                Result[i - 1] := ActiveCktElement.GetBus(i);
            end;
        end;
    end
    else
        Result := VarArrayCreate([0, 0], varOleStr);

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_Name: Widestring;
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit.ActiveCktElement do
        begin
            Result := ParentClass.Name + '.' + Name;
        end
    else
        Result := '';
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_NumConductors: Integer;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.ActiveCktElement.NConds
    else
        Result := 0;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_NumPhases: Integer;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.ActiveCktElement.NPhases
    else
        Result := 0;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_NumTerminals: Integer;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.ActiveCktElement.NTerms
    else
        Result := 0;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_Properties(Index: Olevariant): IDSSProperty;

var
    Str: String;
    i: Integer;
begin

    if ActiveCircuit <> NIL then
    begin

        case (Vartype(Index) and VarTypeMask) of
            VarSmallint, VarInteger:
                FPropIndex := Integer(Index) + 1;    // INdex is zero based to match arrays
            VarOleStr:
            begin
                FPropClass := ActiveDSSObject.ParentClass;
                FPropIndex := 0;
                Str := Index;
                if FPropClass <> NIL then
                    with FPropClass do
                        for i := 1 to NumProperties do
                        begin
                            if CompareText(Str, PropertyName^[i]) = 0 then
                            begin
                                FPropIndex := i;
                                Break;
                            end;
                        end;
            end;
        else
            DoSimpleMsg('Illegal Var Type Passed to Properties Interface: ' + Format('$%x', [VarType(Index)]), 5011);
        end;

    end;

  // DoSimpleMsg('Properties: FPropIndex ='+ IntToStr(FPropIndex));

    Result := FDSSProperty as IDSSProperty;

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure TCktElement.Set_BusNames(Value: Olevariant);
var
    i: Integer;
    Count, Low: Integer;
begin

    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit do
        begin
            Low := VarArrayLowBound(Value, 1);
            Count := VarArrayHighBound(Value, 1) - Low + 1;
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
function TCktElement.Get_Currents: Olevariant;
var
    cBuffer: pComplexArray;
    NValues, iV, i: Integer;

begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit.ActiveCktElement do
        begin
            NValues := NConds * NTerms;
            Result := VarArrayCreate([0, 2 * NValues - 1], varDouble);
            cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
            GetCurrents(cBuffer);
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
        Result := VarArrayCreate([0, 0], varDouble);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_Voltages: Olevariant;

// Bus Voltages at active terminal

var
    numcond, i, n, iV: Integer;
    Volts: Complex;

begin

// Return voltages for all terminals

    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    numcond := NConds * Nterms;
                    Result := VarArrayCreate([0, 2 * numcond - 1], varDouble);
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
        Result := VarArrayCreate([0, 0], varDouble);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_EmergAmps: Double;
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT then
            begin
                with ActiveCktElement as TPDElement do
                    Result := EmergAmps;
            end
            else
                Result := 0.0;
        end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_Enabled: Wordbool;
begin

    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.ActiveCktElement.Enabled
    else
        Result := FALSE;

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_Losses: Olevariant;

var
    LossValue: complex;
begin

    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
            begin
                Result := VarArrayCreate([0, 1], varDouble);
                LossValue := ActiveCktElement.Losses;
                Result[0] := LossValue.re;
                Result[1] := LossValue.im;
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varDouble);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_NormalAmps: Double;
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT then
            begin
                with ActiveCktElement as TPDElement do
                    Result := NormAmps;
            end
            else
                Result := 0.0;
        end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_PhaseLosses: Olevariant;

// Returns Phase losses in kW, kVar

var
    cBuffer: pComplexArray;
    NValues, i, iV: Integer;

begin


    if ActiveCircuit <> NIL then

        with ActiveCircuit.ActiveCktElement do
        begin
            NValues := NPhases;
            Result := VarArrayCreate([0, 2 * NValues - 1], varDouble);
            cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
            GetPhaseLosses(NValues, cBuffer);
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
        Result := VarArrayCreate([0, 0], varDouble);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_Powers: Olevariant;

// Return complex kW, kvar in each conductor for each terminal

var
    cBuffer: pComplexArray;
    NValues,
    i,
    iV: Integer;

begin

    if ActiveCircuit <> NIL then
        with ActiveCircuit.ActiveCktElement do
        begin
            NValues := NConds * Nterms;
            Result := VarArrayCreate([0, 2 * NValues - 1], varDouble);
            cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
            GetPhasePower(cBuffer);
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
        Result := VarArrayCreate([0, 0], varDouble);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure CalcSeqCurrents(pActiveElement: TDSSCktElement; i012: pComplexArray);
{Assumes V012 is properly allocated before call.}
var
    Nvalues, i, j, k, iV: Integer;
    IPh, I012a: array[1..3] of Complex;
    cBuffer: pComplexArray;

begin
    with pActiveElement, ActiveCircuit do
    begin
        Nvalues := NPhases;
        if Nvalues <> 3 then
        begin
        {Handle non-3 phase elements}
            if (Nphases = 1) and PositiveSequence then
            begin
                NValues := NConds * NTerms;
                cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
                GetCurrents(cBuffer);

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
            cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
            GetCurrents(cBuffer);
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
function TCktElement.Get_SeqCurrents: Olevariant;

// All sequence currents of active ciruit element
// returns magnitude only.

var
    i: Integer;
    i012: pComplexArray;
    S: String;

begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                    if Enabled then
                    begin
                        try
                            Result := VarArrayCreate([0, 3 * NTerms - 1], varDouble);

                            i012 := Allocmem(Sizeof(i012^[1]) * 3 * Nterms);
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
                        Result := VarArrayCreate([0, 0], varDouble);  // Disabled

        end
    else
        Result := VarArrayCreate([0, 0], varDouble);

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_SeqPowers: Olevariant;


// All seq Powers of active 3-phase ciruit element
// returns kW + j kvar

var
    Nvalues, i, j, k, n, icount: Integer;
    S: Complex;
    VPh, V012: array[1..3] of Complex;
    IPh, I012: array[1..3] of Complex;
    cBuffer: pComplexArray;

begin

    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    Result := VarArrayCreate([0, 2 * 3 * NTerms - 1], varDouble); // allocate for kW and kvar
                    if NPhases <> 3 then
                    begin
                        if (Nphases = 1) and PositiveSequence then
                        begin
                            NValues := NConds * NTerms;
                            cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
                            GetCurrents(cBuffer);

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
                        cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
                        GetCurrents(cBuffer);
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
        Result := VarArrayCreate([0, 0], varDouble);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure CalcSeqVoltages(pActiveElement: TDSSCktElement; V012: pComplexArray);
{Assumes V012 is properly allocated before call.}
var
    Nvalues, i, j, k, iV: Integer;
    VPh, V012a: array[1..3] of Complex;
begin
    with pActiveElement, ActiveCircuit do
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
function TCktElement.Get_SeqVoltages: Olevariant;
// All voltages of active ciruit element
// magnitude only
// returns a set of seq voltages (3) for each terminal
// 0, 1, 2 sequence  (0, +, -)

var
    i: Integer;
    V012: pComplexArray;
    S: String;

begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                    if Enabled then
                    begin
                        try
                            Result := VarArrayCreate([0, 3 * NTerms - 1], varDouble);

                            V012 := Allocmem(Sizeof(V012^[1]) * 3 * Nterms);
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
                        Result := VarArrayCreate([0, 0], varDouble);  // Disabled

        end
    else
        Result := VarArrayCreate([0, 0], varDouble);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure TCktElement.Close(Term, Phs: Integer);

begin

    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    ActiveTerminal := Terminals^[Term];
                    Closed[Phs] := TRUE;
                end;
        end;

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure TCktElement.Open(Term, Phs: Integer);

begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    ActiveTerminal := Terminals^[Term];
                    Closed[Phs] := FALSE;
                end;
        end;

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure TCktElement.Set_EmergAmps(Value: Double);

begin

    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if IsPDElement then
            begin
                with ActiveCktElement as TPDElement do
                    EmergAmps := Value;
            end;  {Else Do Nothing}
        end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure TCktElement.Set_Enabled(Value: Wordbool);
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.ActiveCktElement.Enabled := Value;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure TCktElement.Set_NormalAmps(Value: Double);
begin
    if ActiveCircuit <> NIL then
    begin
        if IsPDElement then
        begin
            with ActiveCircuit do
                with ActiveCktElement as TPDElement do
                    NormAmps := Value;
        end;  {Else Do Nothing}
    end;
end;


function TCktElement.IsOpen(Term, Phs: Integer): Wordbool;

var
    i: Integer;

begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            with ActiveCktElement do
                ActiveTerminal := Terminals^[Term];
            if Phs = 0 then // At least one must be open
            begin
                Result := FALSE;
                for i := 1 to ActiveCktElement.NConds do
                    if not ActiveCktElement.Closed[i] then
                    begin
                        Result := TRUE;
                        Exit;
                    end;
            end
            else // Check a specific phase or conductor
            begin
                Result := not ActiveCktElement.Closed[Phs];
            end;
        end;
end;

function TCktElement.Get_AllPropertyNames: Olevariant;

var
    k: Integer;
begin
    Result := VarArrayCreate([0, 0], varOleStr);
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    with ParentClass do
                    begin
                        Result := VarArrayCreate([0, NumProperties - 1], varOleStr);
                        for k := 1 to NumProperties do
                        begin
                            Result[k - 1] := PropertyName^[k];
                        end;
                    end;
                end
        end;
end;

function TCktElement.Get_NumProperties: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    Result := ParentClass.NumProperties;
                end
        end;


end;

function TCktElement.Get_Residuals: Olevariant;
var
    cBuffer: pComplexArray;
    iV, i, j, k: Integer;
    cResid: Complex;

begin

    if ActiveCircuit <> NIL then
        with ActiveCircuit.ActiveCktElement do
        begin
            Result := VarArrayCreate([0, 2 * NTerms - 1], varDouble);    // 2 values per terminal
            cBuffer := Allocmem(sizeof(cBuffer^[1]) * Yorder);
            GetCurrents(cBuffer);
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
        Result := VarArrayCreate([0, 0], varDouble);

end;

function TCktElement.Get_Yprim: Olevariant;
{ Return the YPrim matrix for this element }

var
    iV: Integer;
    i: Integer;
    NValues: Integer;
    cValues: pComplexArray;

begin
    if ActiveCircuit = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble);
    end
    else
        with ActiveCircuit do
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    NValues := SQR(Yorder);
                    cValues := GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
                    if cValues = NIL then
                    begin   // check for unassigned array
                        Result := VarArrayCreate([0, 0], varDouble);  // just return null array
                        Exit;  // Get outta here
                    end;
                    Result := VarArrayCreate([0, 2 * NValues - 1], varDouble);  // Make variant array
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
                Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

function TCktElement.Get_DisplayName: Widestring;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.ActiveCktElement.DisplayName
    else
        Result := '';
end;

function TCktElement.Get_GUID: Widestring;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.ActiveCktElement.ID
    else
        Result := '';
end;

function TCktElement.Get_Handle: Integer;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.ActiveCktElement.Handle
    else
        Result := 0;
end;

procedure TCktElement.Set_DisplayName(const Value: Widestring);
begin
    if ActiveCircuit <> NIL then
        ActiveCircuit.ActiveCktElement.DisplayName := Value;
end;

function TCktElement.Get_Controller(idx: Integer): Widestring;
var
    ctrl: TDSSCktElement;
begin
    Result := '';
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if (idx > 0) and (idx <= ActiveCktElement.ControlElementList.Listsize) then
            begin
                ctrl := ActiveCktElement.ControlElementList.Get(idx);
                if ctrl <> NIL then
                    Result := Format('%s.%s', [ctrl.ParentClass.Name, ctrl.Name]);
            end;
        end;
end;

function TCktElement.Get_EnergyMeter: Widestring;
var
    pd: TPDElement;
begin
    Result := '';
    if ActiveCircuit <> NIL then
    begin
        if ActiveCircuit.ActiveCktElement.HasEnergyMeter then
        begin
            pd := ActiveCircuit.ActiveCktElement as TPDElement;
            Result := pd.MeterObj.Name;
        end;
    end;
end;

function TCktElement.Get_HasVoltControl: Wordbool;

// Returns true if any of the controls is a capcontrol or a regcontrol
var
    ctrl: TDSSCktElement;
begin
    Result := FALSE;
    if ActiveCircuit <> NIL then
    begin
        ctrl := ActiveCircuit.ActiveCktElement.ControlElementlist.First;
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

            ctrl := ActiveCircuit.ActiveCktElement.ControlElementlist.Next;
        end;
    end;
end;

function TCktElement.Get_HasSwitchControl: Wordbool;
var
    ctrl: TDSSCktElement;
begin
    Result := FALSE;
    if ActiveCircuit <> NIL then
    begin
        ctrl := ActiveCircuit.ActiveCktElement.ControlElementList.First;
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

            ctrl := ActiveCircuit.ActiveCktElement.ControlElementlist.Next;
        end;
    end;
end;

function TCktElement.Get_CplxSeqVoltages: Olevariant;
{returns Seq Voltages as array of complex values}
var
    i, iV: Integer;
    V012: pComplexArray;
    S: String;

begin

    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                    if Enabled then
                    begin
                        try
                            Result := VarArrayCreate([0, 2 * 3 * NTerms - 1], varDouble);

                            V012 := Allocmem(Sizeof(V012^[1]) * 3 * Nterms);
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
                        Result := VarArrayCreate([0, 0], varDouble);  // Disabled

        end
    else
        Result := VarArrayCreate([0, 0], varDouble);

end;

function TCktElement.Get_CplxSeqCurrents: Olevariant;
{returns Seq Voltages as array of complex values}
var
    i, iV: Integer;
    i012: pComplexArray;
    S: String;

begin

    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                    if Enabled then
                    begin
                        try
                            Result := VarArrayCreate([0, 2 * 3 * NTerms - 1], varDouble);

                            i012 := Allocmem(Sizeof(i012^[1]) * 3 * Nterms);
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
                        Result := VarArrayCreate([0, 0], varDouble);  // Disabled

        end
    else
        Result := VarArrayCreate([0, 0], varDouble);


end;

function TCktElement.Get_AllVariableNames: Olevariant;
var
    k: Integer;
    pPCElem: TPCElement;

begin

    Result := VarArrayCreate([0, 0], varOleStr);
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin

                    if (DSSObjType and BASECLASSMASK) = PC_ELEMENT then
                    begin
                        pPCElem := (ActiveCktElement as TPCElement);
                        Result := VarArrayCreate([0, pPCElem.NumVariables - 1], varOleStr);
                        for k := 1 to pPCElem.NumVariables do
                        begin
                            Result[k - 1] := pPCElem.VariableName(k);
                        end;
                    end;

         {Else zero-length array null string}
                end
        end;

end;

function TCktElement.Get_AllVariableValues: Olevariant;

{Return array of doubles with values of all variables if PCElement}
var
    k: Integer;
    pPCElem: TPCElement;

begin

    Result := VarArrayCreate([0, 0], varDouble);
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin

                    if (DSSObjType and BASECLASSMASK) = PC_ELEMENT then
                    begin
                        pPCElem := (ActiveCktElement as TPCElement);
                        Result := VarArrayCreate([0, pPCElem.NumVariables - 1], varDouble);
                        for k := 1 to pPCElem.NumVariables do
                        begin
                            Result[k - 1] := pPCElem.Variable[k];
                        end;
                    end;

         {Else zero-length array null string}
                end
        end;

end;

function TCktElement.Get_Variable(const MyVarName: Widestring; out Code: Integer): Double;

var
    pPCElem: TPCElement;
    VarIndex: Integer;

begin
    Result := 0.0;
    Code := 1; // Signifies an error; no value set
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
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

function TCktElement.Get_Variablei(Idx: Integer; out Code: Integer): Double;

{Get Value of a variable by index}
var
    pPCElem: TPCElement;

begin
    Result := 0.0;
    Code := 1; // Signifies an error; no value set
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
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

function TCktElement.Get_NodeOrder: Olevariant;
var
    k: Integer;
    i: Integer;
    j: Integer;
begin

    Result := VarArrayCreate([0, 0], varInteger);
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin

            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    k := 0;
                    Result := VarArrayCreate([0, NTerms * Nconds - 1], varInteger);

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

function TCktElement.Get_HasOCPDevice: Wordbool;

// Check for presence of a fuse, recloser, etc.
begin
    Result := FALSE;
    if ActiveCircuit <> NIL then
    begin
        Result := ActiveCircuit.ActiveCktElement.HasOCPDevice;
    end;
end;

function TCktElement.Get_NumControls: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        Result := ActiveCircuit.ActiveCktElement.ControlElementList.listSize;
    end;
end;

function TCktElement.Get_OCPDevIndex: Integer;
var
    iControl: Integer;
    pCktElement: TDSSCktElement;

begin
    Result := 0;
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
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

function TCktElement.Get_OCPDevType: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
            Result := GetOCPDeviceType(ActiveCktElement);     // see Utilities.pas
end;

function TCktElement.Get_CurrentsMagAng: Olevariant;
// return currents in magnitude, angle array
var
    cBuffer: pComplexArray;
    CMagAng: polar;
    NValues, iV, i: Integer;

begin

    if ActiveCircuit <> NIL then
        with ActiveCircuit.ActiveCktElement do
        begin
            NValues := NConds * NTerms;
            Result := VarArrayCreate([0, 2 * NValues - 1], varDouble);
            cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
            GetCurrents(cBuffer);
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
        Result := VarArrayCreate([0, 0], varDouble);

end;

function TCktElement.Get_VoltagesMagAng: Olevariant;

// Bus Voltages in magnitude, angle at all terminal

var
    numcond, i, n, iV: Integer;
    Volts: Polar;

begin

// Return voltages for all terminals

    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    numcond := NConds * Nterms;
                    Result := VarArrayCreate([0, 2 * numcond - 1], varDouble);
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
        Result := VarArrayCreate([0, 0], varDouble);

end;

initialization
    TAutoObjectFactory.Create(ComServer, TCktElement, Class_CktElement, ciInternal, tmApartment);
end.
