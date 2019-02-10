unit DCktElement;

interface

function CktElementI(mode: Longint; arg: Longint): Longint; CDECL;
function CktElementF(mode: Longint; arg: Double): Double; CDECL;
function CktElementS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure CktElementV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    UComplex,
    Sysutils,
    PDElement,
    PCElement,
    MathUtil,
    Variants,
    CktElement,
    Utilities;

var
    i, count, low: Integer;
    ctrl: TDSSCktElement;
    pPCElem: TPCElement;
    pPDElem: TPDElement;
    BData: Wordbool;
    numcond, n, iV: Integer;
    Volts, cResid: Complex;
    cBuffer: pComplexArray;
    S: String;

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
                cBuffer := Allocmem(sizeof(Complex) * NValues);
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

function IsPDElement: Boolean;
begin
    Result := ((ActiveCircuit.ActiveCktElement.DSSObjType and 3) = PD_ELEMENT)
end;

function CktElementI(mode: Longint; arg: Longint): Longint; CDECL;
var
    pCktElement: TDSSCktElement;
    i, iControl: Integer;

begin
    Result := 0;  // Default return value
    case mode of
        0:
        begin                                    // CktElement.Numterminals
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.ActiveCktElement.NTerms
            else
                Result := 0;
        end;
        1:
        begin                                    // CktElement.NumConductors
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.ActiveCktElement.NConds
            else
                Result := 0;
        end;
        2:
        begin                                    // CktElement.NumPhases
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.ActiveCktElement.NPhases
            else
                Result := 0;
        end;
        3:
        begin                                    // CktElement.Open
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                        begin
                            ActiveTerminal := Terminals^[arg];
                            Closed[3] := FALSE;
                        end;
                end;
            Result := 0;
        end;
        4:
        begin                                    // CktElement.Close
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                        begin
                            ActiveTerminal := Terminals^[arg];
                            Closed[3] := TRUE;
                        end;
                end;
            Result := 0;
        end;
        5:
        begin                                    // CktElement.IsOpen
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    with ActiveCktElement do
                        ActiveTerminal := Terminals^[arg];
                    Result := 0;
                    for i := 1 to ActiveCktElement.NConds do
                        if not ActiveCktElement.Closed[i] then
                        begin
                            Result := 1;
                            Exit;
                        end;
                end;
        end;
        6:
        begin                                    // CktElement.NumProperties
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
        7:
        begin                                    // CktElement.HasSwitchControl
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                ctrl := ActiveCircuit.ActiveCktElement.ControlElementList.First;
                while ctrl <> NIL do
                begin
                    case (ctrl.DSSObjType and CLASSMASK) of
                        SWT_CONTROL:
                            Result := 1;
                    else
                        Result := 0;
                    end;
                    if Result = 1 then
                        Exit;
                    ctrl := ActiveCircuit.ActiveCktElement.ControlElementlist.Next;
                end;
            end;
        end;
        8:
        begin                                    // CktElement.HasVoltControl
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                ctrl := ActiveCircuit.ActiveCktElement.ControlElementlist.First;
                while ctrl <> NIL do
                begin
                    case (ctrl.DSSObjType and CLASSMASK) of
                        CAP_CONTROL,
                        REG_CONTROL:
                            Result := 1;
                    else
                        Result := 0;
                    end;
                    if Result = 1 then
                        Exit;
                    ctrl := ActiveCircuit.ActiveCktElement.ControlElementlist.Next;
                end;
            end;
        end;
        9:
        begin                                    // CktElement.NumControls
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                Result := ActiveCircuit.ActiveCktElement.ControlElementList.listSize;
            end;
        end;
        10:
        begin                                   // CktElement.OCPDevIndex
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
                                    Result := Longint(iControl);
                                RECLOSER_CONTROL:
                                    Result := Longint(iControl);
                                RELAY_CONTROL:
                                    Result := Longint(iControl);
                            end;
                        inc(iControl);
                    until (iControl > ActiveCktElement.ControlElementList.listSize) or (Result > 0);
                end;
        end;
        11:
        begin                                   // CktElement.OCPDevType
            Result := 0;
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                    Result := GetOCPDeviceType(ActiveCktElement);     // see Utilities.pas
        end;
        12:
        begin                                   // CktElement.enabled -read
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                if ActiveCircuit.ActiveCktElement.Enabled then
                    Result := 1;
            end
            else
                Result := 0;
        end;
        13:
        begin                                   // CktElement.enabled -Write
            if arg = 1 then
                BData := TRUE
            else
                BData := FALSE;
            if ActiveCircuit <> NIL then
                ActiveCircuit.ActiveCktElement.Enabled := BData;
        end
    else
        Result := -1;
    end;

end;

//**************************Float commands****************************************
function CktElementF(mode: Longint; arg: Double): Double; CDECL;
begin
    Result := 0.0;  // Default return value
    case mode of
        0:
        begin                                        // CktElement.NormalAmps - read
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
        1:
        begin                                        // CktElement.NormalAmps - Write
            if ActiveCircuit <> NIL then
            begin
                if IsPDElement then
                begin
                    with ActiveCircuit do
                        with ActiveCktElement as TPDElement do
                            NormAmps := arg;
                end;  {Else Do Nothing}
            end;
        end;
        2:
        begin                                      // CktElement.EmergAmps - read
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
        3:
        begin                                        // CktElement.EmergAmps - Write
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if IsPDElement then
                    begin
                        with ActiveCktElement as TPDElement do
                            EmergAmps := arg;
                    end;  {Else Do Nothing}
                end;
        end;
        4:
        begin                                        // CktElement.variablei
            Result := 0.0; // Signifies an error; no value set
            i := trunc(arg);
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                        begin
                            if (DSSObjType and BASECLASSMASK) = PC_ELEMENT then
                            begin
                                pPCElem := (ActiveCktElement as TPCElement);
                                if (i > 0) and (i <= pPCElem.NumVariables) then
                                begin
                                    Result := pPCElem.Variable[i];
                                end;
                            end;
               {Else zero-length array null string}
                        end
                end;
        end
    else
        Result := -1;
    end;
end;

//**************************String commands****************************************
function CktElementS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
begin
    Result := pAnsiChar(Ansistring('0'));  // Default return value
    case mode of
        0:
        begin                                          // CktElement.Name
            if ActiveCircuit <> NIL then
                with ActiveCircuit.ActiveCktElement do
                begin
                    Result := pAnsiChar(Ansistring(ParentClass.Name + '.' + Name));
                end
            else
                Result := pAnsiChar(Ansistring(''));
        end;
        1:
        begin                                          // CktElement.Display - read
            if ActiveCircuit <> NIL then
                Result := pAnsiChar(Ansistring(ActiveCircuit.ActiveCktElement.DisplayName))
            else
                Result := pAnsiChar(Ansistring(''));
        end;
        2:
        begin                                          // CktElement.Display - Write
            if ActiveCircuit <> NIL then
                ActiveCircuit.ActiveCktElement.DisplayName := String(arg);
            Result := pAnsiChar(Ansistring(''));
        end;
        3:
        begin                                          // CktElement.GUID
            if ActiveCircuit <> NIL then
                Result := pAnsiChar(Ansistring(ActiveCircuit.ActiveCktElement.ID))
            else
                Result := pAnsiChar(Ansistring(''));
        end;
        4:
        begin                                          // CktElement.EnergyMeter
            Result := pAnsiChar(Ansistring(''));
            if ActiveCircuit <> NIL then
            begin
                if ActiveCircuit.ActiveCktElement.HasEnergyMeter then
                begin
                    pPDElem := ActiveCircuit.ActiveCktElement as TPDElement;
                    Result := pAnsiChar(Ansistring(pPDElem.MeterObj.Name));
                end;
            end;
        end;
        5:
        begin                                          // CktElement.Controller
            Result := pAnsiChar(Ansistring(''));
            i := strtoInt(String(arg));
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if (i > 0) and (i <= ActiveCktElement.ControlElementList.Listsize) then
                    begin
                        ctrl := ActiveCktElement.ControlElementList.Get(i);
                        if ctrl <> NIL then
                            Result := pAnsiChar(Ansistring(Format('%s.%s', [ctrl.ParentClass.Name, ctrl.Name])));
                    end;
                end;
        end
    else
        Result := pAnsiChar(Ansistring('Error'));
    end;
end;
//**************************Variant commands****************************************
procedure CktElementV(mode: Longint; out arg: Variant); CDECL;

var
    VPh, V012: array[1..3] of Complex;
    IPh, I012: array[1..3] of Complex;
    i, j, k: Integer;
    NValues: Integer;
    cValues: pComplexArray;
    CMagAng: polar;

begin
    case mode of
        0:
        begin                                          // CktElement.BusNames - read
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit do
                begin
                    arg := VarArrayCreate([0, ActiveCktElement.Nterms - 1], varOleStr);
                    for i := 1 to ActiveCktElement.Nterms do
                    begin
                        arg[i - 1] := ActiveCktElement.GetBus(i);
                    end;
                end;
            end
            else
                arg := VarArrayCreate([0, 0], varOleStr);
        end;
        1:
        begin                                          // CktElement.BusNames - Write
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit do
                begin
                    Low := VarArrayLowBound(String(arg), 1);
                    Count := VarArrayHighBound(String(arg), 1) - Low + 1;
                    if Count > ActiveCktElement.NTerms then
                        Count := ActiveCktElement.NTerms;
                    for i := 1 to Count do
                    begin
                        ActiveCktElement.SetBus(i, String(arg[i - 1 + Low]));
                    end;
                end;
            end;
        end;
        2:
        begin                                          // CktElement.Voltages
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                        begin
                            numcond := NConds * Nterms;
                            arg := VarArrayCreate([0, 2 * numcond - 1], varDouble);
         // k := (Terminal-1)*numcond;    // RCD 8-30-00 Changed
                            iV := 0;
                            for i := 1 to numcond do
                            begin
                                n := ActiveCktElement.NodeRef^[i];
                                Volts := Solution.NodeV^[n]; // ok if =0
                                arg[iV] := Volts.re;
                                Inc(iV);
                                arg[iV] := Volts.im;
                                Inc(iV);
                            end;
                        end;
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        3:
        begin                                          // CktElement.Currents
            if ActiveCircuit <> NIL then
                with ActiveCircuit.ActiveCktElement do
                begin
                    numcond := NConds * NTerms;
                    arg := VarArrayCreate([0, 2 * numcond - 1], varDouble);
                    cBuffer := Allocmem(sizeof(cBuffer^[1]) * numcond);
                    GetCurrents(cBuffer);
                    iV := 0;
                    for i := 1 to numcond do
                    begin
                        arg[iV] := cBuffer^[i].re;
                        Inc(iV);
                        arg[iV] := cBuffer^[i].im;
                        Inc(iV);
                    end;
                    Reallocmem(cBuffer, 0);
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        4:
        begin                                          // CktElement.Powers
            if ActiveCircuit <> NIL then
                with ActiveCircuit.ActiveCktElement do
                begin
                    numcond := NConds * Nterms;
                    arg := VarArrayCreate([0, 2 * numcond - 1], varDouble);
                    cBuffer := Allocmem(sizeof(cBuffer^[1]) * numcond);
                    GetPhasePower(cBuffer);
                    iV := 0;
                    for i := 1 to numcond do
                    begin
                        arg[iV] := cBuffer^[i].re * 0.001;
                        Inc(iV);
                        arg[iV] := cBuffer^[i].im * 0.001;
                        Inc(iV);
                    end;
                    Reallocmem(cBuffer, 0);
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        5:
        begin                                          // CktElement.Losses
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveCktElement <> NIL then
                    begin
                        arg := VarArrayCreate([0, 1], varDouble);
                        Volts := ActiveCktElement.Losses;
                        arg[0] := Volts.re;
                        arg[1] := Volts.im;
                    end;
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        6:
        begin                                          // CktElement.Phaselosses
            if ActiveCircuit <> NIL then
                with ActiveCircuit.ActiveCktElement do
                begin
                    numcond := NPhases;
                    arg := VarArrayCreate([0, 2 * numcond - 1], varDouble);
                    cBuffer := Allocmem(sizeof(cBuffer^[1]) * numcond);
                    GetPhaseLosses(numcond, cBuffer);
                    iV := 0;
                    for i := 1 to numcond do
                    begin
                        arg[iV] := cBuffer^[i].re * 0.001;
                        Inc(iV);
                        arg[iV] := cBuffer^[i].im * 0.001;
                        Inc(iV);
                    end;
                    Reallocmem(cBuffer, 0);
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        7:
        begin                                          // CktElement.SeqVoltages
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                            if Enabled then
                            begin
                                try
                                    arg := VarArrayCreate([0, 3 * NTerms - 1], varDouble);

                                    cbuffer := Allocmem(Sizeof(cbuffer^[1]) * 3 * Nterms);
                // get complex seq voltages
                                    CalcSeqVoltages(ActiveCktElement, cbuffer);
                // return 0 based array
                                    for i := 1 to 3 * Nterms do
                                        arg[i - 1] := Cabs(cbuffer^[i]);  // return mag only
                                    Reallocmem(cbuffer, 0);  // throw away temp memory
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
                                arg := VarArrayCreate([0, 0], varDouble);  // Disabled
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        8:
        begin                                          // CktElement.SeqCurrents
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                            if Enabled then
                            begin
                                try
                                    arg := VarArrayCreate([0, 3 * NTerms - 1], varDouble);
                                    cbuffer := Allocmem(Sizeof(cbuffer^[1]) * 3 * Nterms);
                // get complex seq voltages
                                    CalcSeqCurrents(ActiveCktElement, cbuffer);
                // return 0 based array
                                    for i := 1 to 3 * Nterms do
                                        arg[i - 1] := Cabs(cbuffer^[i]);  // return mag only
                                    Reallocmem(cbuffer, 0);  // throw away temp memory
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
                                arg := VarArrayCreate([0, 0], varDouble);  // Disabled
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        9:
        begin                                          // CktElement.Seqpowers
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                        begin
                            arg := VarArrayCreate([0, 2 * 3 * NTerms - 1], varDouble); // allocate for kW and kvar
                            if NPhases <> 3 then
                            begin
                                if (Nphases = 1) and PositiveSequence then
                                begin
                                    numcond := NConds * NTerms;
                                    cBuffer := Allocmem(sizeof(cBuffer^[1]) * numcond);
                                    GetCurrents(cBuffer);
                                    for i := 0 to 2 * 3 * NTerms - 1 do
                                        arg[i] := 0.0;   // Initialize Result
                                    Count := 2;  // Start with kW1
                    {Put only phase 1 quantities in Pos seq}
                                    for j := 1 to NTerms do
                                    begin
                                        k := (j - 1) * NConds;
                                        n := NodeRef^[k + 1];
                                        Vph[1] := Solution.NodeV^[n];  // Get voltage at node
                                        Volts := Cmul(Vph[1], conjg(cBuffer^[k + 1]));   // Compute power per phase
                                        arg[count] := Volts.re * 0.003; // 3-phase kW conversion
                                        inc(count);
                                        arg[count] := Volts.im * 0.003; // 3-phase kvar conversion
                                        inc(count, 6);
                                    end;
                                    Reallocmem(cBuffer, 0);
                                end
                                else
                                    for i := 0 to 2 * 3 * NTerms - 1 do
                                        arg[i] := -1.0;  // Signify n/A
                            end
                            else
                            begin
                                numcond := NConds * NTerms;
                                cBuffer := Allocmem(sizeof(cBuffer^[1]) * numcond);
                                GetCurrents(cBuffer);
                                count := 0;
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
                                        Volts := Cmul(V012[i], conjg(I012[i]));
                                        arg[count] := Volts.re * 0.003; // 3-phase kW conversion
                                        inc(count);
                                        arg[count] := Volts.im * 0.003; // 3-phase kW conversion
                                        inc(count);
                                    end;
                                end;
                                Reallocmem(cBuffer, 0);
                            end;
                        end;
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        10:
        begin                                         // CktElement.AllpropertyNames
            arg := VarArrayCreate([0, 0], varOleStr);
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                        begin
                            with ParentClass do
                            begin
                                arg := VarArrayCreate([0, NumProperties - 1], varOleStr);
                                for k := 1 to NumProperties do
                                begin
                                    arg[k - 1] := PropertyName^[k];
                                end;
                            end;
                        end
                end;
        end;
        11:
        begin                                         // CktElement.Residuals
            if ActiveCircuit <> NIL then
                with ActiveCircuit.ActiveCktElement do
                begin
                    arg := VarArrayCreate([0, 2 * NTerms - 1], varDouble);    // 2 values per terminal
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
                        arg[iV] := Cabs(cResid);
                        Inc(iV);
                        arg[iV] := CDang(cResid);
                        Inc(iV);
                    end;
                    Reallocmem(cBuffer, 0);
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        12:
        begin                                         // CktElement.YPrim
            if ActiveCircuit = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble);
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
                                arg := VarArrayCreate([0, 0], varDouble);  // just return null array
                                Exit;  // Get outta here
                            end;
                            arg := VarArrayCreate([0, 2 * NValues - 1], varDouble);  // Make variant array
                            iV := 0;
                            for i := 1 to NValues do
                            begin    // Plunk the values in the variant array
                                arg[iV] := cValues^[i].re;
                                Inc(iV);
                                arg[iV] := cValues^[i].im;
                                Inc(iV);
                            end;
                        end
                    else
                        arg := VarArrayCreate([0, 0], varDouble);  // just return null array
        end;
        13:
        begin                                         // CktElement.CplxSeqVoltages
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                            if Enabled then
                            begin
                                try
                                    arg := VarArrayCreate([0, 2 * 3 * NTerms - 1], varDouble);
                                    cValues := Allocmem(Sizeof(cValues^[1]) * 3 * Nterms);
                // get complex seq voltages
                                    CalcSeqVoltages(ActiveCktElement, cValues);
                // return 0 based array
                                    iV := 0;
                                    for i := 1 to 3 * Nterms do
                                    begin
                                        arg[iV] := cValues^[i].re;
                                        inc(iV);
                                        arg[iV] := cValues^[i].im;
                                        inc(iV);
                                    end;
                                    Reallocmem(cValues, 0);  // throw away temp memory
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
                                arg := VarArrayCreate([0, 0], varDouble);  // Disabled
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        14:
        begin                                         // CktElement.CplxSeqCurrents
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                            if Enabled then
                            begin
                                try
                                    arg := VarArrayCreate([0, 2 * 3 * NTerms - 1], varDouble);
                                    cValues := Allocmem(Sizeof(cValues^[1]) * 3 * Nterms);
                // get complex seq voltages
                                    CalcSeqCurrents(ActiveCktElement, cValues);
                // return 0 based array
                                    iV := 0;
                                    for i := 1 to 3 * Nterms do
                                    begin
                                        arg[iV] := cValues^[i].re;
                                        inc(iV);
                                        arg[iV] := cValues^[i].im;
                                        inc(iV);
                                    end;
                                    Reallocmem(cValues, 0);  // throw away temp memory
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
                                arg := VarArrayCreate([0, 0], varDouble);  // Disabled
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        15:
        begin                                         // CktElement.AllVariableNames
            arg := VarArrayCreate([0, 0], varOleStr);
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                        begin
                            if (DSSObjType and BASECLASSMASK) = PC_ELEMENT then
                            begin
                                pPCElem := (ActiveCktElement as TPCElement);
                                arg := VarArrayCreate([0, pPCElem.NumVariables - 1], varOleStr);
                                for k := 1 to pPCElem.NumVariables do
                                begin
                                    arg[k - 1] := pPCElem.VariableName(k);
                                end;
                            end;
             {Else zero-length array null string}
                        end
                end;
        end;
        16:
        begin                                         // CktElement.AllVariableValues
            arg := VarArrayCreate([0, 0], varDouble);
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                        begin
                            if (DSSObjType and BASECLASSMASK) = PC_ELEMENT then
                            begin
                                pPCElem := (ActiveCktElement as TPCElement);
                                arg := VarArrayCreate([0, pPCElem.NumVariables - 1], varDouble);
                                for k := 1 to pPCElem.NumVariables do
                                begin
                                    arg[k - 1] := pPCElem.Variable[k];
                                end;
                            end;
             {Else zero-length array null string}
                        end
                end;
        end;
        17:
        begin                                         // CktElement.Nodeorder
            arg := VarArrayCreate([0, 0], varInteger);
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin

                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                        begin
                            k := 0;
                            arg := VarArrayCreate([0, NTerms * Nconds - 1], varInteger);

                            for i := 1 to Nterms do
                            begin
                                for j := (i - 1) * NConds + 1 to i * Nconds do
                                begin
                                    arg[k] := GetNodeNum(NodeRef^[j]);
                                    inc(k);
                                end;
                            end;
                        end
                end;
        end;
        18:
        begin                                         // CktElement.CurrentsMagAng
            if ActiveCircuit <> NIL then
                with ActiveCircuit.ActiveCktElement do
                begin
                    NValues := NConds * NTerms;
                    arg := VarArrayCreate([0, 2 * NValues - 1], varDouble);
                    cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
                    GetCurrents(cBuffer);
                    iV := 0;
                    for i := 1 to NValues do
                    begin
                        CMagAng := ctopolardeg(cBuffer^[i]); // convert to mag/angle
                        arg[iV] := CMagAng.mag;
                        Inc(iV);
                        arg[iV] := CMagAng.ang;
                        Inc(iV);
                    end;
                    Reallocmem(cBuffer, 0);
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        19:
        begin
// Return voltages for all terminals
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveCktElement <> NIL then
                        with ActiveCktElement do
                        begin
                            numcond := NConds * Nterms;
                            arg := VarArrayCreate([0, 2 * numcond - 1], varDouble);
             // k := (Terminal-1)*numcond;    // RCD 8-30-00 Changed
                            iV := 0;
                            for i := 1 to numcond do
                            begin
                                n := ActiveCktElement.NodeRef^[i];
                                CMagAng := ctopolardeg(Solution.NodeV^[n]); // ok if =0
                                arg[iV] := CMagAng.mag;
                                Inc(iV);
                                arg[iV] := CMagAng.ang;
                                Inc(iV);
                            end;
                        end;
                end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end
    else
        arg := VarArrayCreate([0, 0], varOleStr);
    end;
end;

end.
