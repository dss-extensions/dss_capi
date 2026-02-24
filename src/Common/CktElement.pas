unit CktElement;

// ----------------------------------------------------------
// Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

uses
    Classes,
    UComplex, DSSUcomplex,
    Ucmatrix,
    CAPI_Types,
    Terminal,
    DSSObject,
    DSSClass,
    DSSPointerList,
    DSSClassDefs,
    fpjson;

type

    TDSSCktElement = class(TDSSObject)
    PUBLIC
        FEnabled: WordBool;
    PRIVATE
        FBusNames: ArrayOfString; // Bus + Nodes (a.1.2.3.0)
        FYPrimInvalid: Boolean;
        cachedLosses: Complex;

        procedure DoYprimCalcs(Ymatrix: TCMatrix);
    PROTECTED
        procedure SetNTerms(Value: Int8);
    PUBLIC
        FActiveTerminal: Int8;
        FNTerms: Int8;
        FNConds: Int8;  // no. conductors per terminal

        IterminalSolutionCount: Integer;

        BusIndex: Integer;
        YPrim_Series,
        YPrim_Shunt,
        YPrim: TCMatrix;   // Order will be NTerms() * Ncond
        FYprimFreq: Double;     // Frequency at which YPrim has been computed

    PUBLIC
        Handle: Integer;

        // Total Noderef array for element
        NodeRef: pIntegerArray;  // Need fast access to this
        Yorder: Integer;
        Fnphases: Integer;  // Phases, this device -- TODO: Int8 someday...

        // LastTerminalChecked: Int8;  // Flag used in tree searches -- UNUSED

        ControlElementList: TDSSPointerList; //Pointer to control for this device

        Iterminal: pComplexArray;  // Others need this
        Vterminal: pComplexArray;

        BaseFrequency: Double;

        Terminals: Array of TPowerTerminal;
        TerminalsChecked: Array of Boolean;
        ActiveTerminal: ^TPowerTerminal;

        PublicDataSize: Integer;  // size of PublicDataStruct
        PublicDataStruct: Pointer;  // Generic Pointer to public data Block that may be access by other classes of elements
                             // Accessing app has to know the structure
                             // Inited to Nil.  If Nil, accessing app should ignore

        constructor Create(ParClass: TDSSClass; objName: String);
        destructor Destroy; OVERRIDE;
        procedure MakeLike(OtherObj: Pointer); override;
        function FirstBus(): String;
        function NextBus(): String;

        function AllConductorsClosed(): Boolean;
        function GetYPrim(var Ymatrix: TCmatrix; Opt: Integer): Integer; VIRTUAL;  //returns values of array
        function GetYPrimValues(Opt: Integer): pComplexArray; VIRTUAL;
        function MaxTerminalOneIMag(): Double;   // Max of Iterminal 1 phase currents
        procedure ComputeITerminal();  // Computes Iterminal for this device
        procedure ComputeVTerminal();
        procedure ZeroITerminal(); inline;
        procedure GetCurrents(Curr: pComplexArray); VIRTUAL; OVERLOAD; ABSTRACT; //Get present value of terminal Curr for reports
        procedure GetCurrents(Curr: ArrayOfComplex); VIRTUAL; OVERLOAD; //Get present value of terminal Curr for reports
        function InjCurrents(): Integer; VIRTUAL; // Applies to PC Elements Puts straight into Solution Array

        function GetBus(i: Integer): String;  // Get bus name by index
        procedure SetBus(i: Integer; const s: String); virtual;  // Set bus name by index
        procedure SetNodeRef(iTerm: Integer; NodeRefArray: pIntegerArray); VIRTUAL;  // Set NodeRef Array for fast solution with intrinsics
        procedure RecalcElementData(); VIRTUAL; ABSTRACT;
        procedure CalcYPrim(); VIRTUAL;

        procedure MakePosSequence(); VIRTUAL;  // Make a positive Sequence Model

        procedure GetTermVoltages(iTerm: Integer; VBuffer: PComplexArray);
        procedure GetPhasePower(PowerBuffer: pComplexArray); VIRTUAL;
        procedure GetPhaseLosses(var Num_Phases: Integer; LossBuffer: pComplexArray); VIRTUAL;
        procedure GetLosses(var TotalLosses, LoadLosses, NoLoadLosses: Complex); VIRTUAL;
        procedure GetSeqLosses(var PosSeqLosses, NegSeqLosses, ZeroModeLosses: complex); VIRTUAL;

        procedure DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;


        function Power(idxTerm: Integer): Complex;    // Get total complex power in active terminal
        // function Get_MaxPower(idxTerm: Integer): Complex;    // Get equivalent total complex power in active terminal based on phase with max current
        function MaxCurrent(idxTerm: Integer): Double; // Get equivalent total complex current on phase with max current
        function MaxCurrentAng(idxTerm:Integer): Double; // Get equivalent angle of the total complex current on phase with max current
        function MaxVoltageC(idxTerm: Integer): Complex; // Get equivalent total complex voltage on phase
        function MaxVoltage(idxTerm: Integer): Double; // Get equivalent **magnitude** of total complex voltage on phase
        function MaxVoltageAng(idxTerm:Integer): Double; // Get equivalent angle of the total complex voltage on phase

        function Enabled(): WordBool;
        procedure SetEnabled(Value: WordBool); VIRTUAL;
        function YPrimInvalid(): Boolean;
        procedure SetYprimInvalid(const Value: Boolean);
        function NTerms(): Int8;
        function NConds(): Int8;
        procedure SetNConds(Value: Int8);
        function NPhases(): Integer;
        function Losses(): Complex;   // Get total losses for property...
        function ActiveTerminalIdx(): Int8; inline;
        procedure SetActiveTerminalIdx(value: Int8); inline;

        function ConductorClosed(Index: Integer): Boolean; inline;
        procedure SetConductorClosed(Index: Integer; Value: Boolean); VIRTUAL;
        function PCEValue(idxTerm:Integer; ValType:Integer): Double; // Get a value for the active PCE such as P, Q, Vmag, IMag, etc.
        procedure SumCurrents();
        procedure Get_Current_Mags(var cMBuffer: ArrayOfDouble); // Returns the Currents vector in magnitude

        procedure StateToJSON(joptions: Integer; var json: TJSONObject); virtual;
    end;


implementation

uses
    DSSGlobals,
    SysUtils,
    Utilities,
    Math,
    Solution,
    DSSHelper,
    DSSObjectHelper,
    TypInfo,
    CktElementClass;

const
    cEpsilon : Complex = (re: EPSILON; im: 0.0);

constructor TDSSCktElement.Create(ParClass: TDSSClass; objName: String);
begin
    inherited Create(ParClass, objName);

    NodeRef := NIL;
    YPrim_Series := NIL;
    YPrim_Shunt := NIL;
    YPrim := NIL;
    FBusNames := NIL;
    Vterminal := NIL;
    Iterminal := NIL;  // present value of terminal current
    Terminals := NIL;
    TerminalsChecked := NIL;

    PublicDataStruct := NIL;   // pointer to fixed struct of data to be shared
    PublicDataSize := 0;

    Handle := -1;
    BusIndex := 0;
    FNTerms := 0;
    FNConds := 0;
    Fnphases := 0;
    DSSObjType := 0;
    Yorder := 0;

    SetYprimInvalid(true);
    FEnabled := TRUE;

    // Make list for a small number of controls with an increment of 1
    ControlElementList := TDSSPointerList.Create(1);

    FActiveTerminal := 0;
    // LastTerminalChecked := 0;

    // Indicates which solution Itemp is computed for
    IterminalSolutionCount := -1;

    BaseFrequency := ActiveCircuit.Fundamental;
end;

destructor TDSSCktElement.Destroy;
begin
    if DSS = NIL then
    begin
        inherited Destroy;
        exit;
    end;
    SetLength(Terminals, 0);
    SetLength(TerminalsChecked, 0);
    Reallocmem(Iterminal, 0);
    Reallocmem(Vterminal, 0);
    Reallocmem(NodeRef, 0);

    if assigned(ControlElementList) then
        ControlElementList.Free;

    // Dispose YPrims
    if (Yprim <> NIL) AND (Yprim <> Yprim_Shunt) AND (Yprim <> Yprim_Series) then
        Yprim.Free;
    if Yprim_Series <> NIL then
        Yprim_Series.Free;
    if Yprim_Shunt <> NIL then
        Yprim_Shunt.Free;

    inherited Destroy;
end;

function TDSSCktElement.YPrimInvalid(): Boolean;
begin
    result := FYPrimInvalid;
end;

procedure TDSSCktElement.SetYprimInvalid(const Value: Boolean);
begin
    FYPrimInvalid := value and (not (Flg.ForceYPrim in Flags));
    if Value and FEnabled then
        // If this device is in the circuit, then we have to rebuild Y on a change in Yprim
        ActiveCircuit.Solution.InvalidateSystemY();
end;

function TDSSCktElement.ActiveTerminalIdx(): Int8; inline;
begin
    Result := FActiveTerminal + 1;
end;

procedure TDSSCktElement.SetActiveTerminalIdx(value: Int8);
begin
    if (Value > 0) and (Value <= FNTerms) then
    begin
        FActiveTerminal := Value - 1;
        ActiveTerminal := @Terminals[FActiveTerminal];
    end;
end;

function TDSSCktElement.ConductorClosed(Index: Integer): Boolean; inline;
// return state of selected conductor
// if index=0 return true if all phases closed, else false
var
    i: Integer;
begin
    if (Index = 0) then
    begin
        Result := TRUE;
        for i := 1 to Fnphases do
        begin
            if not Terminals[FActiveTerminal].ConductorsClosed[i - 1] then
            begin
                Result := FALSE;
                Break;
            end;
        end;
    end
    else
    if (Index > 0) and (Index <= FNConds) then
        Result := Terminals[FActiveTerminal].ConductorsClosed[Index - 1]
    else
        Result := FALSE;
end;

procedure TDSSCktElement.SetConductorClosed(Index: Integer; Value: Boolean);
var
    i: Integer;
    prevValue: Boolean;
begin
    if (Index = 0) then
    begin  // Do all conductors
        for i := 0 to Fnphases - 1 do
        begin
            prevValue := Terminals[FActiveTerminal].ConductorsClosed[i];
            if (prevValue <> Value) then
            begin
                Terminals[FActiveTerminal].ConductorsClosed[i] := Value;
                if (Value) then
                begin
                    Dec(ActiveCircuit.numConductorsOpen);
                end
                else
                begin
                    Inc(ActiveCircuit.numConductorsOpen);
                end;
            end;
        end;
        SetYprimInvalid(true); // this also sets the global SystemYChanged flag
    end
    else
    begin
        if (Index > 0) and (Index <= FNConds) then
        begin
            prevValue := Terminals[FActiveTerminal].ConductorsClosed[index - 1];
            if (prevValue <> Value) then
            begin
                Terminals[FActiveTerminal].ConductorsClosed[index - 1] := Value;
                if (Value) then
                begin
                    Dec(ActiveCircuit.numConductorsOpen);
                end
                else
                begin
                    Inc(ActiveCircuit.numConductorsOpen);
                end;
            end;
            SetYprimInvalid(true); // DSS-Extensions: kept outside the "if" for compatibility
        end;
    end;
end;

function TDSSCktElement.NConds(): Int8;
begin
    result := FNConds;
end;

procedure TDSSCktElement.SetNConds(Value: Int8);
begin
    // Check for an almost certain programming error
    if Value <= 0 then
    begin
        DoSimpleMsg('Invalid number of terminals (%d) for "%s"',
            [Value, FullName()], 749);
        Exit;
    end;

    if Value <> FNConds then
        ActiveCircuit.SetBusNameRedefined();
    FNConds := Value;
    SetNTerms(FNTerms);  // ReallocTerminals    NEED MORE EFFICIENT WAY TO DO THIS
end;

function TDSSCktElement.NPhases(): Integer;
begin
    result := Fnphases;
end;

function TDSSCktElement.NTerms(): Int8;
begin
    result := FNTerms;
end;

procedure TDSSCktElement.SetNTerms(Value: Int8);
var
    i: Integer;
begin
    // Check for an almost certain programming error
    if Value <= 0 then
    begin
        DoSimpleMsg('Invalid number of terminals (%d) for "%s"',
            [Value, FullName()], 749);
        Exit;
    end;

    // If value is same as present value, no reallocation necessary;
    // If either NTerms() or Nconds has changed then reallocate
    if (Value = FNTerms) and ((Value * FNConds) = Yorder) then
        Exit;
    
    // Sanity Check
    if FNConds > 101 then
    begin
        DoSimpleMsg('Warning: Number of conductors is very large (%d) for Circuit Element: "%s". Possible error in specifying the Number of Phases for element.',
            [FNConds, FullName()], 750);
    end;


     // ReAllocate BusNames
     // because they are Strings, we have to do it differently

    if FBusNames = NIL then
        FNTerms := 0;

    SetLength(FBusNames, Value); // Keeps old values; may truncate storage
    // First allocation
    for i := FNTerms to Value - 1 do
    begin
        FBusNames[i] := Name + '_' + IntToStr(i + 1);
        // Make up a bus name to stick in.
        // This is so devices like transformers which may be defined on multiple commands
        // will have something in the BusNames array.
    end;

    // Reallocate Terminals if Nconds or NTerms() changed
    SetLength(Terminals, Value);
    SetLength(TerminalsChecked, Value);
    for i := 0 to Value - 1 do
        TerminalsChecked[i] := False;

    FNTerms := Value;    // Set new number of terminals
    Yorder := FNTerms * FNConds;
    ReallocMem(Vterminal, Sizeof(Complex) * Yorder);
    ReallocMem(Iterminal, Sizeof(Complex) * Yorder);
    for i := 0 to Value - 1 do
        Terminals[i].Init(FNConds);
end;

function TDSSCktElement.Enabled(): WordBool;
begin
    result := FEnabled;
end;

procedure TDSSCktElement.SetEnabled(Value: WordBool);
//  If disabled, but defined, just have to processBusDefs.  Adding a bus OK
// If being removed from circuit, could remove a node or bus so have to rebuild
begin
    if Value = FEnabled then
        Exit;
        
    FEnabled := Value;
    ActiveCircuit.SetBusNameRedefined();  // forces rebuilding of Y matrix and bus lists
end;

function TDSSCktElement.GetYPrim(var Ymatrix: TCmatrix; Opt: Integer): Integer;
//returns pointer to actual YPrim
begin
    case Opt of
        ALL_YPRIM:
            Ymatrix := Yprim;
        SERIES:
            YMatrix := YPrim_Series;
        SHUNT:
            YMatrix := YPrim_Shunt;
    end;
    Result := 0;
end;

function TDSSCktElement.GetYPrimValues(Opt: Integer): pComplexArray;
// Return a pointer to the Beginning the storage arrays for fast access
var
    Norder: Integer;
begin
    Result := NIL;
    case Opt of
        ALL_YPRIM:
            if YPrim <> NIL then
                Result := Yprim.GetValuesArrayPtr(Norder);
        SERIES:
            if YPrim_Series <> NIL then
                Result := Yprim_Series.GetValuesArrayPtr(Norder);
        SHUNT:
            if YPrim_Shunt <> NIL then
                Result := YPrim_Shunt.GetValuesArrayPtr(Norder);
    end;
end;

procedure TDSSCktElement.GetLosses(var TotalLosses, LoadLosses,
    NoLoadLosses: Complex);
begin
    // For no override, Default behavior is:
    // Just return total losses and set LoadLosses=total losses and noload losses =0

    TotalLosses := Losses();  // Watts, vars
    LoadLosses := TotalLosses;
    NoLoadLosses := 0;
end;

function TDSSCktElement.InjCurrents(): Integer;  // Applies to PC Elements
begin
    Result := 0;
    DoErrorMsg(Format(_('Improper call to InjCurrents for Element: "%s".'), [FullName()]), '****',
        'Called CktElement class base function instead of actual.', 753)
end;

procedure TDSSCktElement.SetNodeRef(iTerm: Integer; NodeRefArray: pIntegerArray);
var
    Size, Size2: Integer;
begin
    // Allocate NodeRef and move new values into it.
    Size := Yorder * SizeOf(NodeRef[1]);
    Size2 := SizeOf(NodeRef[1]) * FNConds;  // Size for one terminal
    ReallocMem(NodeRef, Size);  // doesn't do anything if already properly allocated
    Move(NodeRefArray[1], NodeRef[(iTerm - 1) * FNConds + 1], Size2);  // Zap
    Move(NodeRefArray[1], Terminals[iTerm - 1].TermNodeRef[0], Size2);  // Copy in Terminal as well
end;

function TDSSCktElement.FirstBus(): String;
begin
    if FNTerms > 0 then
    begin
        BusIndex := 1;
        Result := FBusNames[BusIndex - 1];
    end
    else
        Result := '';
end;

function TDSSCktElement.NextBus(): String;
begin
    Result := '';
    if FNTerms > 0 then
    begin
        Inc(BusIndex);
        if BusIndex <= FNTerms then
            Result := FBusNames[BusIndex - 1]
        else
            BusIndex := FNTerms;
    end;
end;

function TDSSCktElement.GetBus(i: Integer): String;  // Get bus name by index

begin
    if i <= FNTerms then
        Result := FBusNames[i - 1]
    else
        Result := '';
end;

procedure TDSSCktElement.SetBus(i: Integer; const s: String); // Set bus name by index
begin
    if i <= FNTerms then
    begin
        FBusNames[i - 1] := DSSLowerCase(S);
        ActiveCircuit.SetBusNameRedefined();  // Set Global Flag to signal circuit to rebuild busdefs
    end
    else
        DoSimpleMsg('Attempt to set bus name for non-existent circuit element terminal (%d): "%s"', [i, s], 7541);
end;

procedure TDSSCktElement.CalcYPrim();
begin
    if YPrim_Series <> NIL then
        DoYPrimCalcs(Yprim_Series);
    if YPrim_Shunt <> NIL then
        DoYPrimCalcs(YPrim_Shunt);
    if YPrim <> NIL then
        DoYPrimCalcs(YPrim);

    // This is now also the behavior in EPRI's OpenDSS since r3859
    if ((ActiveCircuit.Solution.SolverOptions and ord(TSolverOptions.AlwaysResetYPrimInvalid)) <> 0) then
    // if ((DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.DontResetYPrimInvalid)) = 0) then
        FYPrimInvalid := false;
end;

procedure TDSSCktElement.ComputeITerminal();
begin
    // to save time, only recompute if a different solution than last time it was computed.
    if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
    begin
        GetCurrents(Iterminal);
        IterminalSolutionCount := ActiveCircuit.Solution.SolutionCount;
        Exclude(flags, TDSSObjectFlag.CachedLosses);
    end;
end;

function TDSSCktElement.MaxTerminalOneIMag(): Double;
// Get max of phase currents on the first terminal; Requires computing Iterminal
var
    i: Integer;
begin
    Result := 0.0;
    if (not FEnabled) or (NodeRef = NIL) then
        Exit;
        
    for i := 1 to Fnphases do
        Result := Max(Result, SQR(Iterminal[i].re) + SQR(Iterminal[i].im));
            
    Result := Sqrt(Result);  // just do the sqrt once and save a little time
end;

procedure TDSSCktElement.Get_Current_Mags(var cMBuffer: ArrayOfDouble);
var
    i: Integer;
begin
    SetLength(cMBuffer, Fnphases);
    for i := 1 to Fnphases do
        cMBuffer[i - 1] := cabs(Iterminal[i]);
end;

function TDSSCktElement.Power(idxTerm: Integer): Complex;    // Get total complex power in active terminal
var
    i, k, n: Integer;
    NodeV: pNodeVarray;
begin
    Result := 0;
    SetActiveTerminalIdx(idxTerm);
    if (not FEnabled) or (NodeRef = NIL) then
        Exit;
        
    ComputeITerminal();

    // Method: Sum complex power going into phase conductors of active terminal
    NodeV := ActiveCircuit.Solution.NodeV;
    k := (idxTerm - 1) * FNConds;
    for i := 1 to FNConds do     // 11-7-08 Changed from Fnphases - was not accounting for all conductors
    begin
        n := ActiveTerminal^.TermNodeRef[i - 1]; // don't bother for grounded node
        if n > 0 then
            Result += NodeV[n] * cong(Iterminal[k + i]);
    end;
    // If this is a positive sequence circuit, then we need to multiply by 3 to get the 3-phase power
    if ActiveCircuit.PositiveSequence then
        Result := Result * 3.0;
end;

function TDSSCktElement.Losses(): Complex;
// get total losses in circuit element, all phases, all terminals.
// Returns complex losses (watts, vars)
var
    i, j, k, n: Integer;
    NodeV: pNodeVarray;
begin
    Result := 0;
    if (not FEnabled) or (NodeRef = NIL) then
        Exit;

    if (IterminalSolutionCount = ActiveCircuit.Solution.SolutionCount) and (TDSSObjectFlag.CachedLosses in flags) then
    begin
        Result := cachedLosses;
        Exit;
    end;

    ComputeITerminal();

    // Method: Sum complex power going into all conductors of all terminals
    // Special for AutoTransformer - sum based on NPhases rather then Yorder

    NodeV := ActiveCircuit.Solution.NodeV;
    if (CLASSMASK and self.DSSObjType) = AUTOTRANS_ELEMENT then
    begin
        k := 0;
        for j := 1 to FNTerms do
        begin
            for i := 1 to Nphases do
            begin
                Inc(k);
                n := NodeRef[k];
                if n <= 0 then
                    continue;

                Result += NodeV[n] * cong(Iterminal[k]);
            end;
            Inc(k, Nphases)
        end;
    end
    else  // for all other elements
    begin
        for k := 1 to Yorder do
        begin
            n := NodeRef[k];
            if n <= 0 then
                continue;

            Result += NodeV[n] * cong(Iterminal[k]);
        end;
    end;

    if ActiveCircuit.PositiveSequence then
        Result *= 3.0;

    cachedLosses := Result;
    Include(flags, TDSSObjectFlag.CachedLosses);
end;

function TDSSCktElement.MaxVoltageC(idxTerm: Integer): Complex;
// Get Voltage at the specified terminal 09/17/2019
var
    volts: Complex;
    ClassIdx,
    i, k,
    nrefN,
    nref: Integer;
    MaxCurr,
    CurrMag: Double;
    MaxPhase: Integer;
    NodeV: pNodeVarray;
begin
    SetActiveTerminalIdx(idxTerm);   // set active Terminal
    Result := 0;
    if (not FEnabled) or (NodeRef = NIL) then
        Exit;
        
    ComputeITerminal();

    // Method: Checks what's the phase with maximum current
    // retunrs the voltage for that phase

    MaxCurr := 0.0;
    MaxPhase := 1;  // Init this so it has a non zero value
    k := (idxTerm - 1) * FNConds; // starting index of terminal
    for i := 1 to Fnphases do
    begin
        CurrMag := Cabs(Iterminal[k + i]);
        if CurrMag > MaxCurr then
        begin
            MaxCurr := CurrMag;
            MaxPhase := i
        end;
    end;

    NodeV := ActiveCircuit.Solution.NodeV;
    ClassIdx := DSSObjType and CLASSMASK;              // gets the parent class descriptor (int)
    nref := ActiveTerminal^.TermNodeRef[MaxPhase - 1]; // reference to the phase voltage with the max current
    nrefN := ActiveTerminal^.TermNodeRef[FNConds - 1];  // reference to the ground terminal (GND or other phase)
    // Get power into max phase of active terminal
    if not (ClassIdx = XFMR_ELEMENT) then  // Only for transformers
        volts := NodeV[nref]
    else
        volts := NodeV[nref] - NodeV[nrefN];
    Result := volts;
end;

function TDSSCktElement.MaxVoltage(idxTerm: Integer): double;
begin
    Result := cabs(MaxVoltageC(idxTerm));
end;

function TDSSCktElement.MaxVoltageAng(idxTerm: Integer): double;
begin
    Result := cang(MaxVoltageC(idxTerm));
end;

// function TDSSCktElement.Get_MaxPower(idxTerm: Integer): Complex;
// //Get power in the phase with the max current and return equivalent power as if it were balanced in all phases
// // 2/12/2019
// var
//     volts: Complex;
//     ClassIdx,
//     i, k,
//     nrefN,
//     nref: Integer;
//     MaxCurr,
//     CurrMag: Double;
//     MaxPhase: Integer;
//     NodeV: pNodeVarray;
// begin
//     SetActiveTerminalIdx(idxTerm);   // set active Terminal
//     Result := 0;
//     if (not FEnabled) or (NodeRef = NIL) then
//         Exit;
        
//     ComputeITerminal();

//     // Method: Get power in the phase with max current of active terminal
//     // Multiply by Nphases and return

//     MaxCurr := 0.0;
//     MaxPhase := 1;  // Init this so it has a non zero value
//     k := (idxTerm - 1) * FNConds; // starting index of terminal
//     for i := 1 to Fnphases do
//     begin
//         CurrMag := Cabs(Iterminal[k + i]);
//         if CurrMag > MaxCurr then
//         begin
//             MaxCurr := CurrMag;
//             MaxPhase := i
//         end;
//     end;

//     NodeV := ActiveCircuit.Solution.NodeV;
//     ClassIdx := DSSObjType and CLASSMASK;              // gets the parent class descriptor (int)
//     nref := ActiveTerminal^.TermNodeRef[MaxPhase - 1]; // reference to the phase voltage with the max current
//     nrefN := ActiveTerminal^.TermNodeRef[FNConds - 1];  // reference to the ground terminal (GND or other phase)
    
//     // Get power into max phase of active terminal
//     if not (ClassIdx = XFMR_ELEMENT) then  // Only for transformers
//         volts := NodeV[nref]
//     else
//         volts := NodeV[nref] - NodeV[nrefN];
//     Result := volts * cong(Iterminal[k + MaxPhase]);

//     // Compute equivalent total power of all phases assuming equal to max power in all phases
//     Result := Result * Fnphases;

//     // If this is a positive sequence circuit (Fnphases=1),
//     // then we need to multiply by 3 to get the 3-phase power
//     if ActiveCircuit.PositiveSequence then
//         Result := Result * 3.0;
// end;

function TDSSCktElement.MaxCurrent(idxTerm: Integer): Double;
// returns the magnitude fo the maximum current at the element's terminal
var
    i, k: Integer;
    CurrMag: Double;
    // MaxPhase: Integer;
begin
    SetActiveTerminalIdx(idxTerm);   // set active Terminal
    Result := 0.0;
    if (not FEnabled) or (NodeRef = NIL) then
        Exit;
        
    ComputeITerminal();
    // Method: Get max current at terminal (magnitude)
    // MaxPhase := 1;  // Init this so it has a non zero value
    k := (idxTerm - 1) * FNConds; // starting index of terminal
    for i := 1 to Fnphases do
    begin
        CurrMag := Cabs(Iterminal[k + i]);
        if CurrMag > Result then
        begin
            Result := CurrMag;
            // MaxPhase := i
        end;
    end;
end;

function TDSSCktElement.MaxCurrentAng(idxTerm: Integer): Double;
// returns the angle fo the maximum current at the element's terminal
var
    i, k: Integer;
    CurrAng,
    MaxCurr,
    CurrMag: Double;
    // nref: Integer;
    // MaxPhase: Integer;
begin
    SetActiveTerminalIdx(idxTerm);   // set active Terminal
    Result := 0.0;
    if (not FEnabled) or (NodeRef = NIL) then
        Exit;

    CurrAng := 0.0;
    ComputeITerminal();
    // Method: Get max current at terminal (magnitude)
    MaxCurr := 0.0;
    // MaxPhase := 1;  // Init this so it has a non zero value
    k := (idxTerm - 1) * FNConds; // starting index of terminal
    for i := 1 to Fnphases do
    begin
        CurrMag := Cabs(Iterminal[k + i]);
        if CurrMag > MaxCurr then
        begin
            MaxCurr := CurrMag;
            CurrAng := Cang(Iterminal[k + i]);
            // MaxPhase := i
        end;
    end;
    Result := CurrAng;
end;

function TDSSCktElement.PCEValue(idxTerm: Integer; ValType: Integer): Double;
begin
    case ValType of
        0, 7:
            Result := -Power(1).re;             // P, P0
        1, 8:
            Result := -Power(1).im;             // Q, Q0
        2:
            Result := MaxVoltage(1);             // VMag
        3:
            Result := MaxVoltageAng(1);          // VAng
        4:
            Result := MaxCurrent(1);             // IMag
        5:
            Result := MaxCurrentAng(1);          // IAng
        6:
            Result := cabs(Power(1));            // S
    else
        Result := 0;
    end;
end;

procedure TDSSCktElement.GetPhasePower(PowerBuffer: pComplexArray);
// Get the power in each phase (complex losses) of active terminal
// neutral conductors are ignored by this routine
var
    i, n: Integer;
    NodeV: pNodeVarray;
begin
    if (not FEnabled) or (NodeRef = NIL) then
    begin
        FillByte(PowerBuffer^, Yorder * (SizeOf(Double) * 2), 0);
        Exit;
    end;
    
    ComputeITerminal();

    NodeV := ActiveCircuit.Solution.NodeV;
    for i := 1 to Yorder do
    begin
        n := NodeRef[i]; // increment through terminals
        if n > 0 then
        begin
            if ActiveCircuit.PositiveSequence then
                PowerBuffer[i] := NodeV[n] * cong(Iterminal[i]) * 3.0
            else
                PowerBuffer[i] := NodeV[n] * cong(Iterminal[i]);
        end;
    end;
end;

procedure TDSSCktElement.GetPhaseLosses(var Num_Phases: Integer; LossBuffer: pComplexArray);
// Get the losses in each phase (complex losses);  Power difference coming out
// each phase. Note: This can be misleading if the nodev voltage is greatly unbalanced.
// neutral conductors are ignored by this routine
var
    i, j, k, n: Integer;
    cLoss: Complex;
    NodeV: pNodeVarray;
begin
    Num_Phases := Fnphases;

    if (not FEnabled) or (NodeRef = NIL) then
    begin
        FillByte(LossBuffer^, Fnphases * (SizeOf(Double) * 2), 0);
        Exit;
    end;
    
    ComputeITerminal();

    NodeV := ActiveCircuit.Solution.NodeV;
    for i := 1 to Num_Phases do
    begin
        cLoss := 0;
        for j := 1 to FNTerms do
        begin
            k := (j - 1) * FNConds + i;
            n := NodeRef[k]; // increment through terminals
            if n > 0 then
            begin
                if ActiveCircuit.PositiveSequence then
                    cLoss += NodeV[n] * cong(Iterminal[k]) * 3.0
                else
                    cLoss += NodeV[n] * cong(Iterminal[k]);
            end;
        end;
        LossBuffer[i] := cLoss;
    end;
end;

procedure TDSSCktElement.DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean);
var
    i, j: Integer;
begin
    inherited DumpProperties(F, Complete, Leaf);

    if FEnabled then
        FSWriteln(F, '! ENABLED')
    else
        FSWriteln(F, '! DISABLED');
    if Complete then
    begin
        FSWriteln(F, '! NPhases = ', IntToStr(Fnphases));
        FSWriteln(F, '! Nconds = ', IntToStr(FNConds));
        FSWriteln(F, '! NTerms() = ', IntToStr(FNTerms));
        FSWriteln(F, '! Yorder = ', IntToStr(Yorder));
        FSWrite(F, '! NodeRef = "');
        if NodeRef = NIL then
            FSWrite(F, 'nil')
        else
            for i := 1 to Yorder do
                FSWrite(F, IntToStr(NodeRef[i]), ' ');
        FSWriteln(F, '"');
        FSWrite(F, '! Terminal Status: [');
        for i := 1 to FNTerms do
            for j := 1 to FNConds do
            begin
                if Terminals[i - 1].ConductorsClosed[j - 1] then
                    FSWrite(F, 'C ')
                else
                    FSWrite(F, 'O ');
            end;
        FSWriteln(F, ']');
        FSWrite(F, '! Terminal Bus Ref: [');
        for i := 1 to FNTerms do
            for j := 1 to FNConds do
            begin
                FSWrite(F, IntToStr(Terminals[i - 1].BusRef), ' ');
            end;
        FSWriteln(F, ']');
        FSWriteln(F);

        if YPrim <> NIL then
        begin
            FSWriteln(F, '! YPrim (G matrix)');
            for i := 1 to Yorder do
            begin
                FSWrite(F, '! ');
                for j := 1 to Yorder do
                    FSWrite(F, Format(' %13.10g |', [YPrim[i, j].re]));
                FSWriteln(F);
            end;
            FSWriteln(F, '! YPrim (B Matrix) = ');
            for i := 1 to Yorder do
            begin
                FSWrite(F, '! ');
                for j := 1 to Yorder do
                    FSWrite(F, Format(' %13.10g |', [YPrim[i, j].im]));
                FSWriteln(F);
            end;
        end;
    end;
end;

procedure TDSSCktElement.DoYprimCalcs(Ymatrix: TCMatrix);
var
    i, j, k, ii, jj, ElimRow: Integer;
    Ynn, Yij, Yin, Ynj: Complex;
    RowEliminated: pIntegerArray;
    ElementOpen: Boolean;
begin
    // Now Account for Open Conductors
    // Perform a Kron Reduction on rows where I is forced to zero.
    // Then for any conductor that is open, zero out row and column.
    ElementOpen := FALSE;
    k := 0;
    for i := 1 to FNTerms do
    begin
        for j := 1 to FNConds do
        begin
            if not Terminals[i - 1].ConductorsClosed[j - 1] then
            begin
                if not ElementOpen then
                begin
                    RowEliminated := AllocMem(Sizeof(Integer) * Yorder);
                    ElementOpen := TRUE;
                end;
                // First do Kron Reduction
                ElimRow := j + k;
                Ynn := Ymatrix[ElimRow, ElimRow];
                if Cabs(Ynn) = 0.0 then
                    Ynn.re := EPSILON;
                RowEliminated[ElimRow] := 1;
                for ii := 1 to Yorder do
                begin
                    if RowEliminated[ii] = 0 then
                    begin
                        Yin := Ymatrix[ii, ElimRow];
                        for jj := ii to Yorder do
                            if RowEliminated[jj] = 0 then
                            begin
                                Yij := Ymatrix[ii, jj];
                                Ynj := Ymatrix[ElimRow, jj];
                                Ymatrix[ii, jj] := Yij - ((Yin * Ynj) / Ynn);
                                Ymatrix[jj, ii] := Ymatrix[ii, jj];
                            end;
                    end;
                end;
                // Now zero out row and column
                Ymatrix.ZeroRow(ElimRow);
                Ymatrix.ZeroCol(ElimRow);
                // put a small amount on the diagonal in case node gets isolated
                Ymatrix[ElimRow, ElimRow] := cEpsilon;
            end;
        end;
        k := k + FNConds;
    end;
    // Clean up at end of loop.
    // Add in cEpsilon to diagonal elements of remaining rows to avoid leaving a bus hanging.
    // This happens on low-impedance simple from-to elements when one terminal opened.
    if ElementOpen then
    begin
        for ii := 1 to Yorder do
            if RowEliminated[ii] = 0 then
                Ymatrix.AddElement(ii, ii, cEpsilon);

        Reallocmem(RowEliminated, 0);
    end;
end;

procedure TDSSCktElement.SumCurrents();
// sum Terminal Currents into System  Currents Array
// Primarily for Newton Iteration
var
    i: Integer;
    Currents: pNodeVArray;
begin
    if (not FEnabled) or (NodeRef = NIL) then
        Exit;
        
    ComputeITerminal();
    Currents := ActiveCircuit.Solution.Currents;
    for i := 1 to Yorder do
        Currents[NodeRef[i]] += Iterminal[i];  // Noderef=0 is OK
end;

procedure TDSSCktElement.GetTermVoltages(iTerm: Integer; VBuffer: PComplexArray);
// Bus Voltages at indicated terminal
// Fill Vbuffer array which must be adequately allocated by calling routine
var
    ncond, i: Integer;
    NodeV: pNodeVarray;
begin
    try
        ncond := FNConds;

        // return Zero if terminal number improperly specified
        if (iTerm < 1) or (iTerm > FNTerms) then
        begin
            for i := 1 to ncond do
                VBuffer[i] := 0;
            Exit;
        end;

        NodeV := ActiveCircuit.Solution.NodeV;
        for i := 1 to ncond do
            Vbuffer[i] := NodeV[Terminals[iTerm - 1].TermNodeRef[i - 1]];

    except
        On E: Exception do
            DoSimpleMsg('Error filling voltage buffer in GetTermVoltages for Circuit Element: "%s". Probable Cause: Invalid definition of element. System Error Message: %s', [FullName(), E.Message], 755);
    end;
end;

procedure TDSSCktElement.GetSeqLosses(var PosSeqLosses, NegSeqLosses, ZeroModeLosses: complex);
begin
// For the base class, just return 0
// Derived classes have to supply appropriate function
    PosSeqLosses := 0;
    NegSeqLosses := 0;
    ZeroModeLosses := 0;
end;

function IsGroundBus(const S: String): Boolean;
var
    i: Integer;
begin
    Result := TRUE;
    i := pos('.1', S);
    if i > 0 then
        Result := FALSE;
    i := pos('.2', S);
    if i > 0 then
        Result := FALSE;
    i := pos('.3', S);
    if i > 0 then
        Result := FALSE;
    i := pos('.', S);
    if i = 0 then
        Result := FALSE;
end;

procedure TDSSCktElement.MakePosSequence();
var
    i: Integer;
    grnd: Boolean;
begin
    for i := 0 to FNTerms - 1 do
    begin
        grnd := IsGroundBus(FBusNames[i]);
        FBusNames[i] := StripExtension(FBusNames[i]);
        if grnd then
            FBusNames[i] := FBusNames[i] + '.0';
    end;
end;

procedure TDSSCktElement.ComputeVTerminal();
// Put terminal voltages in an array
var
    i: Integer;
    vterm: PDouble;
    nref: PInteger;
    nv0, nv: PDouble;
begin
    if NodeRef = NIL then 
        Exit;

    vterm := PDouble(VTerminal);
    nref := PInteger(NodeRef);
    nv0 := PDouble(ActiveCircuit.solution.NodeV);
    for i := 1 to Yorder do
    begin
        nv := nv0 + 2 * nref^;
        vterm^ := nv^;
        (vterm + 1)^ := (nv + 1)^;
        inc(vterm, 2);
        inc(nref);
        // VTerminal[i] := NodeV[NodeRef[i]];
    end;
end;

procedure TDSSCktElement.ZeroITerminal(); inline;
var
    i: Integer;
    it: PDouble;
begin
    // Somehow this is slower?! FillDWord(ITerminal^, Yorder * ((SizeOf(Double) * 2) div 4), 0);
    it := PDouble(Iterminal);
    for i := 1 to Yorder do
    begin
        it^ := 0;
        (it + 1)^ := 0;
        inc(it, 2);
    end;
    //for i := 1 to Yorder do
    //    ITerminal[i] := 0;
end;

procedure TDSSCktElement.MakeLike(OtherObj: Pointer);
var
    OtherCktObj: TDSSCktElement;
begin
    inherited MakeLike(OtherObj);

    OtherCktObj := TDSSCktElement(OtherObj);
    BaseFrequency := OtherCktObj.BaseFrequency;
    SetEnabled(TRUE);
end;

function TDSSCktElement.AllConductorsClosed(): Boolean;
var
    i, j: Integer;
begin
    Result := TRUE;
    for i := 1 to FNTerms do
    begin
        for j := 1 to FNConds do
        begin
            if not Terminals[i - 1].ConductorsClosed[j - 1] then
            begin
                Result := FALSE;
                Exit;
            end;
        end;
    end;
end;

procedure TDSSCktElement.GetCurrents(Curr: ArrayOfComplex);
begin
    GetCurrents(pComplexArray(@Curr[0]));
end;

procedure TDSSCktElement.StateToJSON(joptions: Integer; var json: TJSONObject);
var
    tmpArray, tmpArray2: TJSONArray;
    tmpObj: TJSONObject;
    i, j: Integer;
    totalLosses, loadLosses, noLoadLosses: Complex;
    cbuffer: Array of Complex; //TODO? pass as workspace
    NodeV: pNodeVArray;
    
    regNames: ArrayOfString;
    regValues: pDoubleArray;
    numRegisters: Integer;
begin
    //TODO: separate sections?
    // TODO? SeqCurrents, SeqPowers, SeqVoltages

    json.Add('Index', Handle);
    json.Add('Conductors', NConds());
    json.Add('Terminals', NTerms());

    tmpArray := TJSONArray.Create();
    for i := 1 to NTerms() do
    begin
        tmpArray.Add(GetBus(i));
    end;
    json.Add('Buses', tmpArray);

    if (ControlElementList <> NIL) and (ControlElementList.Count > 0) then
    begin
        tmpArray := TJSONArray.Create();
        for i := 1 to ControlElementList.Count do
        begin
            tmpArray.Add(TDSSObject(ControlElementList.Get(i)).FullName());
        end;
        json.Add('ControlElements', tmpArray);
    end
    else
    begin
        json.Add('ControlElements', TJSONNull.Create());
    end;

    SetLength(cbuffer, NConds() * NTerms());

    GetCurrents(cbuffer);
    json.Add('Currents', GetDSSArray_JSON(cbuffer, joptions));
    
    tmpArray := TJSONArray.Create();
    NodeV := ActiveCircuit.Solution.NodeV;
    for i := 1 to NConds() * NTerms() do
    begin
        tmpArray.Add(ToJSON(NodeV[NodeRef[i]]));
    end;
    json.Add('Voltages', tmpArray);

    GetPhasePower(pComplexArray(@cbuffer[0]));
    json.Add('Powers', GetDSSArray_JSON(cbuffer, joptions));
    
    GetLosses(totalLosses, loadLosses, noLoadLosses);
    json.Add('Losses', TJSONObject.Create([
        'Total', ToJSON(totalLosses),
        'LoadLosses', ToJSON(loadLosses),
        'NoLoadLosses', ToJSON(noLoadLosses)
    ]));
    
    SetLength(cbuffer, NPhases());
    GetPhaseLosses(i, pComplexArray(@cbuffer[0]));
    json.Add('PhasesLosses', GetDSSArray_JSON(cbuffer, joptions));

    json.Add('YPrim', YPrim.ToJSON(joptions));

    if (AllConductorsClosed()) then
    begin
        json.Add('OpenConductors', TJSONNull.Create());
    end
    else
    begin
        tmpArray := TJSONArray.Create();
        for i := 1 to FNTerms do
        begin
            tmpArray2 := TJSONArray.Create();
            for j := 1 to FNConds do
            begin
                if not Terminals[i - 1].ConductorsClosed[j - 1] then
                begin
                    tmpArray2.Add(j);
                end;
            end;
            tmpArray.Add(tmpArray2);
        end;
        json.Add('OpenConductors', tmpArray);
    end;


    regNames := TCktElementClass(ParentClass).GetRegisterNames(self);
    if regNames <> NIL then
    begin
        regValues := TCktElementClass(ParentClass).GetRegisterValues(self, numRegisters);
        tmpObj := TJSONObject.Create();
        for i := 1 to numRegisters do
        begin
            tmpObj.Add(regNames[i - 1], regValues[i]);
        end;
        json.Add('Registers', tmpObj);
    end;
end;

end.
