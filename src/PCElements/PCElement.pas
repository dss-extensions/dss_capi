unit PCElement;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

uses
    Classes,
    CktElement,
    UComplex, DSSUcomplex,
    DSSClass,
    Spectrum,
    Arraydef,
    Meterelement;

type
    TPCElement = class(TDSSCktElement)
    PRIVATE
        FIterminalUpdated: Boolean;
    PROTECTED
        procedure GetTerminalCurrents(Curr: pComplexArray); VIRTUAL;
        procedure CalcVTerminalPhase();
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer); // This base version uses the Generator convention (the version in Load.pas negates Curr)
        procedure SetNTerms(Value: Int8);
    PUBLIC
        ComplexBuffer: pComplexArray;
        Connection: TGeneralConnection;
        SpectrumObj: TSpectrumObj;

        MeterObj,  // Upline Energymeter
        SensorObj: TMeterElement; // Upline Sensor for this element

        InjCurrent: pComplexArray;
        elementSolutionCount: Integer;


        constructor Create(ParClass: TDSSClass; objName: String);
        destructor Destroy; OVERRIDE;
        procedure MakeLike(OtherObj: Pointer); override;
        procedure ZeroInjCurrent();

        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Get present values of terminal
        procedure ComputeITerminal(); OVERRIDE;
        function InjCurrents(): Integer; OVERRIDE;
        procedure CalcYPrimContribution(Curr: pComplexArray); INLINE;
        procedure DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;

        // For Harmonics Mode
        procedure InitHarmonics(); VIRTUAL;

        // For Dynamics Mode and Control Devices
        procedure InitStateVars(); VIRTUAL;
        procedure IntegrateStates(); VIRTUAL;
        function NumVariables(): Integer; VIRTUAL;
        procedure GetAllVariables(var States: ArrayOfDouble); VIRTUAL;

        function VariableName(i: Integer): String; VIRTUAL;
        function LookupVariable(const s: String; const matchLength: Boolean = false): Integer;
        function GetVariable(i: Integer): Double; VIRTUAL;
        procedure SetVariable(i: Integer; Value: Double); VIRTUAL;
        function ITerminalUpdated(): Boolean;
        procedure SetITerminalUpdated(const Value: Boolean);
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Utilities,
    PCClass,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;


constructor TPCElement.Create(ParClass: TDSSClass; objName: String);
begin
    inherited Create(ParClass, objName);
    SpectrumObj := DSS.SpectrumClass.DefaultGeneral;
    SensorObj := NIL;
    MeterObj := NIL;
    InjCurrent := NIL;
    ComplexBuffer := NIL;
    FIterminalUpdated := FALSE;
    Connection := TGeneralConnection.Wye;

    // elementSolutionCount is used only in Load, but many PCEs did fill the value before
    // we moved it here with CalcVTerminalPhase
    elementSolutionCount := -1; // For keeping track of the present solution in Injcurrent calcs

    DSSObjType := PC_ELEMENT;
end;

destructor TPCElement.Destroy;
begin
    Reallocmem(ComplexBuffer, 0);
    if Assigned(InjCurrent) then
        Reallocmem(InjCurrent, 0);
    inherited Destroy;
end;

function TPCElement.InjCurrents(): Integer;
// Add injection currents into System currents array
var
    i: Integer;
begin
    Result := 0;
    for i := 1 to Yorder do
        ActiveCircuit.Solution.Currents[NodeRef[i]] += InjCurrent[i];
end;

procedure TPCElement.GetTerminalCurrents(Curr: pComplexArray);
// This is called only if we need to compute the terminal currents from the inj currents
// Such as for Harmonic model
var
    i: Integer;
begin
    if ITerminalUpdated() then
    begin   // Just copy iTerminal unless iTerminal=Curr
        if Curr <> ITerminal then
            for i := 1 to Yorder do
                Curr[i] := ITerminal[i];
    end
    else
    begin
        YPrim.MVmult(Curr, VTerminal);
        for i := 1 to Yorder do
            Curr[i] -= InjCurrent[i];
        SetITerminalUpdated(TRUE);
    end;
    IterminalSolutionCount := ActiveCircuit.Solution.SolutionCount;
end;

procedure TPCElement.GetCurrents(Curr: pComplexArray);
// Gets total Currents going INTO a devices terminals
var
    i: Integer;
begin
    try
        if FEnabled then
        begin
            if (ActiveCircuit.Solution.LastSolutionWasDirect) and (not (ActiveCircuit.Solution.IsDynamicModel or ActiveCircuit.Solution.IsHarmonicModel)) then
            begin
                // Take a short cut and get Currents from YPrim only
                // For case where model is entirely in Y matrix
                CalcYPrimContribution(Curr);
            end
            else
            begin
                GetTerminalCurrents(Curr);
            end;
        end
        else
        begin   // not enabled
            for i := 1 to Yorder do
                Curr[i] := 0;
        end;


    except
        On E: Exception do
            DoErrorMsg(Format(_('GetCurrents for Element: %s.'), [FullName()]), E.Message,
                _('Inadequate storage allotted for circuit element.'), 641);
    end;
end;

procedure TPCElement.CalcYPrimContribution(Curr: pComplexArray);
begin
    ComputeVTerminal();
      // Apply these voltages to Yprim
    YPrim.MVMult(Curr, Vterminal);
end;

procedure TPCElement.InitHarmonics();
begin
  // By default do nothing in the base class
end;

procedure TPCElement.InitStateVars();
begin
    // By default do nothing
end;

procedure TPCElement.IntegrateStates();
begin
 // inherited;
 // By default do nothing
end;

procedure TPCElement.GetAllVariables(var States: ArrayOfDouble);
begin
    // Do Nothing
end;

function TPCElement.NumVariables(): Integer;
begin
    Result := 0;
end;

function TPCElement.VariableName(i: Integer): String;
begin
    // Do Nothing
    Result := '';
end;

function TPCElement.LookupVariable(const S: String; const matchLength: Boolean = false): Integer;
// Search through variable name list and return index if found
// Compare up to length of S
var
    i, TestLength: Integer;
    sl: String;
begin
    Result := -1;   // Returns -1 for error not found

    if matchLength then
    begin
        sl := LowerCase(S);
        for i := 1 to NumVariables do
        begin
            if sl = LowerCase(VariableName(i)) then
            begin
                Result := i;
                Exit;
            end;
        end;
        Exit;
    end;

    TestLength := Length(S);
    for i := 1 to NumVariables do
    begin
        if AnsiCompareText(Copy(VariableName(i), 1, TestLength), S) = 0 then
        begin
            Result := i;
            Exit;
        end;
    end;
end;

procedure TPCElement.DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean);
var
    i: Integer;
begin
    inherited DumpProperties(F, Complete);

    if Complete then
    begin
        FSWriteln(F, '! VARIABLES');
        for i := 1 to NumVariables do
        begin
            FSWriteln(F, Format('! %2d: %s = %-.5g', [i, VariableName(i), GetVariable(i)]));
        end;
    end;

    if Leaf then
    begin
        for i := 1 to ParentClass.NumProperties do
        begin
            FSWriteln(F, '~ ' + ParentClass.PropertyName[i] + '=' + PropertyValue(i));
        end;

        if Complete then
        begin
            FSWriteln(F);
            FSWriteln(F);
        end;
    end;
end;

function TPCElement.GetVariable(i: Integer): Double;
begin
    // do Nothing here -- up to override function
    Result := -9999.99;
end;

procedure TPCElement.SetVariable(i: Integer; Value: Double);
begin
    // Do Nothing
end;

procedure TPCElement.ComputeITerminal();
begin
    if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
    begin
        GetCurrents(Iterminal);
        IterminalSolutionCount := ActiveCircuit.Solution.SolutionCount;
    end;
end;

procedure TPCElement.ZeroInjCurrent();
var
    i: Integer;
begin
    for i := 1 to Yorder do
        InjCurrent[i] := 0;
end;

function TPCElement.ITerminalUpdated(): Boolean;
begin
    result := FITerminalUpdated;
end;

procedure TPCElement.SetITerminalUpdated(const Value: Boolean);
begin
    FITerminalUpdated := Value;
    if Value then
        ITerminalSolutionCount := ActiveCircuit.Solution.SolutionCount;
end;

procedure TPCElement.MakeLike(OtherObj: Pointer);
var
    Other: TPCElement;
begin
    inherited MakeLike(OtherObj);

    Other := TPCElement(OtherObj);

    SpectrumObj := Other.SpectrumObj;
end;

procedure TPCElement.CalcVTerminalPhase();
var
    i, j: Integer;
begin
    // Establish phase voltages and stick in Vterminal
    case Connection of

        TGeneralConnection.Wye:
        begin
            for i := 1 to Fnphases do
                Vterminal[i] := ActiveCircuit.Solution.VDiff(NodeRef[i], NodeRef[FNConds]);
        end;

        TGeneralConnection.Delta:
        begin
            for i := 1 to Fnphases do
            begin
                j := i + 1;
                if j > FNConds then
                    j := 1;
                Vterminal[i] := ActiveCircuit.Solution.VDiff(NodeRef[i], NodeRef[j]);
            end;
        end;
    end;
    elementSolutionCount := ActiveCircuit.Solution.SolutionCount;
end;

procedure TPCElement.StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);
// Add the current into the proper location according to connection
// 
// Reverse of similar routine in load  (Cnegates are switched)
var
    j: Integer;
begin
    case Connection of
        TGeneralConnection.Wye:
        begin
            TermArray[i] += Curr;
            TermArray[FNConds] -= Curr; // Neutral
        end;
        TGeneralConnection.Delta:
        begin
            TermArray[i] += Curr;
            j := i + 1;
            if j > FNConds then
                j := 1;
            TermArray[j] -= Curr;
        end;
    end;
end;

procedure TPCElement.SetNTerms(Value: Int8);
begin
    inherited SetNTerms(Value);
    ReallocMem(ComplexBuffer, Sizeof(Complex) * Yorder);
end;

end.
