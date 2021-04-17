unit CktElement;

{
   ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
     2-17-00 Modified Get_ConductorClosed to handle Index=0
     7-14-01 Changed way Enabled property works.
     8-17-06 Caught BusnameRedefined error when nconds changed
}

interface

uses
    Ucomplex,
    Ucmatrix,
    ArrayDef,
    Terminal,
    DSSObject,
    DSSClass,
    PointerList;

type

    TDSSCktElement = class(TDSSObject)
    PRIVATE

        FBusNames: pStringArray;
        FEnabled: Boolean;
        FEnabledProperty: Integer;
        FActiveTerminal: Integer;
        FYPrimInvalid: Boolean;
        FHandle: Integer;

        procedure Set_Freq(Value: Double);  // set freq and recompute YPrim.

        procedure Set_Nconds(Value: Integer);
        procedure Set_NPhases(Value: Integer);
        procedure Set_ActiveTerminal(value: Integer);
        function Get_ConductorClosed(Index: Integer): Boolean;
        procedure Set_YprimInvalid(Value: Boolean);
        function Get_FirstBus: String;
        function Get_NextBus: String;
        function Get_Losses: Complex;   // Get total losses for property...
        function Get_Power(idxTerm: Integer): Complex;    // Get total complex power in active terminal
        function Get_MaxPower(idxTerm: Integer): Complex;    // Get eauivalent total complex power in active terminal based on phase with max current

        procedure DoYprimCalcs(Ymatrix: TCMatrix);

    PROTECTED

        Fnterms: Integer;
        Fnconds: Integer;  // no. conductors per terminal
        Fnphases: Integer;  // Phases, this device


        ComplexBuffer: pComplexArray;

        IterminalSolutionCount: Integer;

        BusIndex: Integer;
        YPrim_Series,
        YPrim_Shunt,
        YPrim: TCMatrix;   // Order will be NTerms * Ncond
        FYprimFreq: Double;     // Frequency at which YPrim has been computed

        procedure Set_Enabled(Value: Boolean); VIRTUAL;
        procedure Set_ConductorClosed(Index: Integer; Value: Boolean); VIRTUAL;
        procedure Set_NTerms(Value: Integer); VIRTUAL;
        procedure Set_Handle(Value: Integer);
    PUBLIC

      {Total Noderef array for element}
        NodeRef: pIntegerArray;  // Need fast access to this
        Yorder: Integer;

        LastTerminalChecked: Integer;  // Flag used in tree searches

        Checked,
        HasEnergyMeter,
        HasSensorObj,
        IsIsolated,
        HasControl,
        IsMonitored,
        IsPartofFeeder,
        Drawn: Boolean;  // Flag used in tree searches etc

        HasOCPDevice: Boolean; // Fuse, Relay, or Recloser
        HasAutoOCPDevice: Boolean; // Relay or Recloser only
        HasSwtControl: Boolean; // Has a remotely-controlled Switch
        ControlElementList: TPointerList; //Pointer to control for this device

        Iterminal: pComplexArray;  // Others need this
        Vterminal: pComplexArray;

        BaseFrequency: Double;

        Terminals: pTerminalList;
        ActiveTerminal: TPowerTerminal;

        PublicDataSize: Integer;  // size of PublicDataStruct
        PublicDataStruct: Pointer;  // Generic Pointer to public data Block that may be access by other classes of elements
                             // Accessing app has to know the structure
                             // Inited to Nil.  If Nil, accessing app should ignore

        constructor Create(ParClass: TDSSClass);
        destructor Destroy; OVERRIDE;

        function GetYPrim(var Ymatrix: TCmatrix; Opt: Integer): Integer; VIRTUAL;  //returns values of array
        function GetYPrimValues(Opt: Integer): pComplexArray; VIRTUAL;
        function MaxTerminalOneIMag: Double;   // Max of Iterminal 1 phase currents
        procedure ComputeIterminal; VIRTUAL;   // Computes Iterminal for this device
        procedure ComputeVterminal;
        procedure ZeroITerminal;
        procedure GetCurrents(Curr: pComplexArray); VIRTUAL; //Get present value of terminal Curr for reports
        procedure GetInjCurrents(Curr: pComplexArray); VIRTUAL;   // Returns Injextion currents
        function InjCurrents: Integer; VIRTUAL; // Applies to PC Elements Puts straight into Solution Array

        function GetBus(i: Integer): String;  // Get bus name by index
        procedure SetBus(i: Integer; const s: String);  // Set bus name by index
        procedure SetNodeRef(iTerm: Integer; NodeRefArray: pIntegerArray); VIRTUAL;  // Set NodeRef Array for fast solution with intrinsics
        procedure RecalcElementData; VIRTUAL;
        procedure CalcYPrim; VIRTUAL;
      // radial solution removed PROCEDURE BackwardSweep; Virtual;

        procedure MakePosSequence; VIRTUAL;  // Make a positive Sequence Model

        procedure GetTermVoltages(iTerm: Integer; VBuffer: PComplexArray);
        procedure GetPhasePower(PowerBuffer: pComplexArray); VIRTUAL;
        procedure GetPhaseLosses(var Num_Phases: Integer; LossBuffer: pComplexArray); VIRTUAL;
        procedure GetLosses(var TotalLosses, LoadLosses, NoLoadLosses: Complex); VIRTUAL;
        procedure GetSeqLosses(var PosSeqLosses, NegSeqLosses, ZeroModeLosses: complex); VIRTUAL;

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

        property Handle: Integer READ FHandle WRITE Set_Handle;
        property Enabled: Boolean READ FEnabled WRITE Set_Enabled;
        property YPrimInvalid: Boolean READ FYPrimInvalid WRITE set_YprimInvalid;
        property YPrimFreq: Double READ FYprimFreq WRITE Set_Freq;
        property NTerms: Integer READ Fnterms WRITE Set_NTerms;
        property NConds: Integer READ Fnconds WRITE Set_Nconds;
        property NPhases: Integer READ Fnphases WRITE Set_NPhases;
        property FirstBus: String READ Get_FirstBus;
        property NextBus: String READ Get_NextBus;    // null string if no more values
        property Losses: Complex READ Get_Losses;
        property Power[idxTerm: Integer]: Complex READ Get_Power;  // Total power in active terminal
        property MaxPower[idxTerm: Integer]: Complex READ Get_MaxPower;  // Total power in active terminal
        property ActiveTerminalIdx: Integer READ FActiveTerminal WRITE Set_ActiveTerminal;
        property Closed[Index: Integer]: Boolean READ Get_ConductorClosed WRITE Set_ConductorClosed;
        procedure SumCurrents;

    end;


implementation

uses
    DSSGlobals,
    SysUtils,
    Utilities,
    Math;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TDSSCktElement.Create(ParClass: TDSSClass);
begin

    inherited Create(ParClass);
    NodeRef := NIL;
    YPrim_Series := NIL;
    YPrim_Shunt := NIL;
    YPrim := NIL;
    Terminals := NIL;
    FBusNames := NIL;
    Vterminal := NIL;
    Iterminal := NIL;  // present value of terminal current

    ComplexBuffer := NIL;
    PublicDataStruct := NIL;   // pointer to fixed struct of data to be shared
    PublicDataSize := 0;

    FHandle := -1;
    BusIndex := 0;
    FNterms := 0;
    Fnconds := 0;
    Fnphases := 0;
    DSSObjType := 0;
    Yorder := 0;

    YPrimInvalid := TRUE;
    FEnabled := TRUE;
    HasEnergyMeter := FALSE;
    HasSensorObj := FALSE;
    HasOCPDevice := FALSE;
    HasAutoOCPDevice := FALSE;
    HasSwtControl := FALSE;
    HasControl := FALSE;
    IsMonitored := FALSE; // indicates some control is monitoring this element
    IsPartofFeeder := FALSE;
    IsIsolated := FALSE;

    Drawn := FALSE;

     // Make list for a small number of controls with an increment of 1
    ControlElementList := PointerList.TPointerList.Create(1);

    FActiveTerminal := 1;
    LastTerminalChecked := 0;

{    Indicates which solution Itemp is computed for    }
    IterminalSolutionCount := -1;

    BaseFrequency := ActiveCircuit.Fundamental;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TDSSCktElement.Destroy;
var
    i: Integer;
begin
    for i := 1 to FNTerms do
        Terminals^[i].Free;
    for i := 1 to FNTerms do
        FBusNames^[i] := ''; // Free up strings

    Reallocmem(Terminals, 0);
    Reallocmem(FBusNames, 0);
    Reallocmem(Iterminal, 0);
    Reallocmem(Vterminal, 0);
    Reallocmem(NodeRef, 0);
    Reallocmem(ComplexBuffer, 0);

    if assigned(ControlElementList) then
        ControlElementList.Free;


    {Dispose YPrims}
    if Yprim_Series <> NIL then
        Yprim_Series.Free;
    if Yprim_Shunt <> NIL then
        Yprim_Shunt.Free;
    if Yprim <> NIL then
        Yprim.Free;

    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.Set_YprimInvalid(Value: Boolean);
begin
    FYPrimInvalid := value;
    if Value then
    begin

        // If this device is in the circuit, then we have to rebuild Y on a change in Yprim
        if FEnabled then
            ActiveCircuit.Solution.SystemYChanged := TRUE;

    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.Set_ActiveTerminal(value: Integer);
begin
    if (Value > 0) and (Value <= fNterms) then
    begin
        FActiveTerminal := Value;
        ActiveTerminal := Terminals^[Value];
    end;
end;

procedure TDSSCktElement.Set_Handle(value: Integer);
begin
    FHandle := value;
end;

function TDSSCktElement.Get_ConductorClosed(Index: Integer): Boolean;

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
            if not Terminals^[FActiveTerminal].Conductors^[i].Closed then
            begin
                Result := FALSE;
                Break;
            end;
        end;
    end
    else
    if (Index > 0) and (Index <= Fnconds) then
        Result := Terminals^[FActiveTerminal].Conductors^[Index].Closed
    else
        Result := FALSE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.Set_ConductorClosed(Index: Integer; Value: Boolean);
var
    i: Integer;
begin

    if (Index = 0) then
    begin  // Do all conductors

        for i := 1 to Fnphases do
            Terminals^[FActiveTerminal].Conductors^[i].Closed := Value;
        ActiveCircuit.Solution.SystemYChanged := TRUE;  // force Y matrix rebuild
        YPrimInvalid := TRUE;

    end
    else
    begin

        if (Index > 0) and (Index <= Fnconds) then
        begin
            Terminals^[FActiveTerminal].Conductors^[index].Closed := Value;
            ActiveCircuit.Solution.SystemYChanged := TRUE;
            YPrimInvalid := TRUE;
        end;

    end;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.Set_NConds(Value: Integer);

begin
// Check for an almost certain programming error
    if Value <= 0 then
    begin
        DoSimpleMsg(Format('Invalid number of terminals (%d) for "%s.%s"',
            [Value, Parentclass.Name, name]), 749);
        Exit;
    end;

    if Value <> Fnconds then
        ActiveCircuit.BusNameRedefined := TRUE;
    Fnconds := Value;
    Set_Nterms(fNterms);  // ReallocTerminals    NEED MORE EFFICIENT WAY TO DO THIS
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.Set_NPhases(Value: Integer);
begin
    if Value > 0 then
        Fnphases := Value;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.Set_NTerms(Value: Integer);
var
    i: Integer;
    NewBusNames: pStringArray;

begin

// Check for an almost certain programming error
    if Value <= 0 then
    begin
        DoSimpleMsg(Format('Invalid number of terminals (%d) for "%s.%s"',
            [Value, Parentclass.Name, name]), 749);
        Exit;
    end;

// If value is same as present value, no reallocation necessary;
// If either Nterms or Nconds has changed then reallocate
    if (value <> FNterms) or (Value * Fnconds <> Yorder) then
    begin

        {Sanity Check}
        if Fnconds > 101 then
        begin
            DoSimpleMsg(Format('Warning: Number of conductors is very large (%d) for Circuit Element: "%s.%s.' +
                'Possible error in specifying the Number of Phases for element.',
                [Fnconds, Parentclass.Name, name]), 750);
        end;


         {ReAllocate BusNames    }
         // because they are Strings, we have to do it differently

        if Value < fNterms then
            ReallocMem(FBusNames, Sizeof(FBusNames^[1]) * Value)  // Keeps old values; truncates storage
        else
        begin
            if FBusNames = NIL then
            begin
                // First allocation
                  {  Always allocate  arrays of strings with AllocMem so that the pointers are all nil
                     else Delphi thinks non-zero values are pointing to an existing string.}
                FBusNames := AllocMem(Sizeof(FBusNames^[1]) * Value); //    fill with zeros or strings will crash
                for i := 1 to Value do
                    FBusNames^[i] := Name + '_' + IntToStr(i);  // Make up a bus name to stick in.
                     // This is so devices like transformers which may be defined on multiple commands
                     // will have something in the BusNames array.
            end
            else
            begin
                NewBusNames := AllocMem(Sizeof(FBusNames^[1]) * Value);  // make some new space
                for i := 1 to fNterms do
                    NewBusNames^[i] := FBusNames^[i];   // copy old into new
                for i := 1 to fNterms do
                    FBusNames^[i] := '';   // decrement usage counts by setting to nil string
                for i := fNterms + 1 to Value do
                    NewBusNames^[i] := Name + '_' + IntToStr(i);  // Make up a bus name to stick in.
                ReAllocMem(FBusNames, 0);  // dispose of old array storage
                FBusNames := NewBusNames;
            end;
        end;

         {Reallocate Terminals if Nconds or NTerms changed}
        if Terminals <> NIL then
            for i := 1 to FNTerms do
                Terminals^[i].Free;  // clean up old storage

        ReallocMem(Terminals, Sizeof(Terminals^[1]) * Value);

        FNterms := Value;    // Set new number of terminals
        Yorder := FNterms * Fnconds;
        ReallocMem(Vterminal, Sizeof(Vterminal^[1]) * Yorder);
        ReallocMem(Iterminal, Sizeof(Iterminal^[1]) * Yorder);
        ReallocMem(ComplexBuffer, Sizeof(ComplexBuffer^[1]) * Yorder);    // used by both PD and PC elements

        for i := 1 to Value do
            Terminals^[i] := TPowerTerminal.Create(Fnconds);
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.Set_Enabled(Value: Boolean);
//  If disabled, but defined, just have to processBusDefs.  Adding a bus OK
// If being removed from circuit, could remove a node or bus so have to rebuild
//VAR
//  NumNodesSaved:Integer;

begin
    with ActiveCircuit do
        if Value <> FEnabled then
        begin  // don't change unless this represents a change

       // This code was too cute and prevented rebuilding of meter zones
       // Removed 7/24/01
       (*IF Value THEN Begin
     
         NumNodesSaved := NumNodes;
         ProcessBusDefs;     // If we create new nodes, force rebuild of bus lists
         If NumNodes>NumNodesSaved Then BusNameRedefined := True
         ELSE Solution.SystemYChanged:= True; //  just rebuild of yPrim
       End
       ELSE   BusNameRedefined := True;  // Force Rebuilding of BusLists anyway
       *)

            FEnabled := Value;
            BusNameRedefined := TRUE;  // forces rebuilding of Y matrix and bus lists

        end;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.GetCurrents(Curr: pComplexArray);  //Get present value of terminal Curr for reports

begin

    DoErrorMsg('Something is Wrong.  Got to base CktElement GetCurrents for Object:' + CRLF + DSSClassName + '.' + Name,
        'N/A',
        'Should not be able to get here. Probable Programming Error.', 751);

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.GetInjCurrents(Curr: pComplexArray);

begin

    DoErrorMsg('Something is Wrong.  Got to base CktElement GetInjCurrents for Object:' + CRLF + DSSClassName + '.' + Name, '****',
        'Should not be able to get here. Probable Programming Error.', 752);

end;

procedure TDSSCktElement.GetLosses(var TotalLosses, LoadLosses,
    NoLoadLosses: Complex);
begin
  {For no override, Default behavior is:
    Just return total losses and set LoadLosses=total losses and noload losses =0}

    TotalLosses := Losses;  // Watts, vars
    LoadLosses := TotalLosses;
    NoLoadLosses := CZERO;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDSSCktElement.InjCurrents: Integer;  // Applies to PC Elements

begin
    Result := 0;
    DoErrorMsg(('Improper call to InjCurrents for Element: ' + Name + '.'), '****',
        'Called CktElement class base function instead of actual.', 753)

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.SetNodeRef(iTerm: Integer; NodeRefArray: pIntegerArray);

// Also allocates VTemp  & Itemp

var
    Size, Size2: Integer;
begin
// Allocate NodeRef and move new values into it.
    Size := Yorder * SizeOf(NodeRef^[1]);
    Size2 := SizeOf(NodeRef^[1]) * Fnconds;  // Size for one terminal
    ReallocMem(NodeRef, Size);  // doesn't do anything if already properly allocated
    Move(NodeRefArray^[1], NodeRef^[(iTerm - 1) * Fnconds + 1], Size2);  // Zap
    Move(NodeRefArray^[1], Terminals^[iTerm].TermNodeRef^[1], Size2);  // Copy in Terminal as well

// Allocate temp array used to hold voltages and currents for calcs
    ReallocMem(Vterminal, Yorder * SizeOf(Vterminal^[1]));
    ReallocMem(Iterminal, Yorder * SizeOf(Iterminal^[1]));
    ReallocMem(ComplexBuffer, Yorder * SizeOf(ComplexBuffer^[1]));
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDSSCktElement.Get_FirstBus: String;
begin
    if FNterms > 0 then
    begin
        BusIndex := 1;
        Result := FBusNames^[BusIndex];
    end
    else
        Result := '';
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDSSCktElement.Get_NextBus: String;
begin
    Result := '';
    if FNterms > 0 then
    begin
        Inc(BusIndex);
        if BusIndex <= FNterms then
            Result := FBusNames^[BusIndex]
        else
            BusIndex := FNterms;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDSSCktElement.GetBus(i: Integer): String;  // Get bus name by index

begin
    if i <= FNTerms then
        Result := FBusNames^[i]
    else
        Result := '';
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.SetBus(i: Integer; const s: String); // Set bus name by index
begin
    if i <= FNterms then
    begin
        FBusNames^[i] := lowercase(S);
        ActiveCircuit.BusNameRedefined := TRUE;  // Set Global Flag to signal circuit to rebuild busdefs
    end
    else
        DoSimpleMsg(Format('Attempt to set bus name for non-existent circuit element terminal(%d): "%s"', [i, s]), 7541);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.Set_Freq(Value: Double);
begin
    if Value > 0.0 then
        FYprimFreq := Value;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.RecalcElementData;
begin
    DoSimpleMsg('Virtual proc RecalcElementData in Base CktElement Class Called for Device = "' + Name + '"', 754);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.CalcYPrim;

begin

    if YPrim_Series <> NIL then
        DoYPrimCalcs(Yprim_Series);
    if YPrim_Shunt <> NIL then
        DoYPrimCalcs(YPrim_Shunt);
    if YPrim <> NIL then
        DoYPrimCalcs(YPrim);

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.ComputeIterminal;
begin

// to save time, only recompute if a different solution than last time it was computed.
    if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
    begin
        GetCurrents(Iterminal);
        IterminalSolutionCount := ActiveCircuit.Solution.SolutionCount;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDSSCktElement.MaxTerminalOneIMag: Double;

{ Get max of phase currents on the first terminal; Requires computing Iterminal
}
var
    i: Integer;

begin
    Result := 0.0;
    if FEnabled then
        for i := 1 to Fnphases do
            with Iterminal^[i] do
                Result := Max(Result, SQR(re) + SQR(im));
    Result := Sqrt(Result);  // just do the sqrt once and save a little time
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDSSCktElement.Get_Power(idxTerm: Integer): Complex;    // Get total complex power in active terminal

var
    cPower: Complex;
    i, k, n: Integer;

begin

    cPower := CZERO;
    ActiveTerminalIdx := idxTerm;

    if FEnabled then
    begin
        ComputeIterminal;

    // Method: Sum complex power going into phase conductors of active terminal
        with ActiveCircuit.Solution do
        begin
            k := (idxTerm - 1) * Fnconds;
            for i := 1 to Fnconds do     // 11-7-08 Changed from Fnphases - was not accounting for all conductors
            begin
                n := ActiveTerminal.TermNodeRef^[i]; // don't bother for grounded node
                if n > 0 then
                    Caccum(cPower, Cmul(NodeV^[n], conjg(Iterminal[k + i])));
            end;
        end;

       {If this is a positive sequence circuit, then we need to multiply by 3 to get the 3-phase power}
        if ActiveCircuit.PositiveSequence then
            cPower := cMulReal(cPower, 3.0);
    end;

    Result := cPower;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDSSCktElement.Get_Losses: Complex;
// get total losses in circuit element, all phases, all terminals.
// Returns complex losses (watts, vars)

var
    cLoss: Complex;
    k, n: Integer;

begin

    cLoss := CZERO;

    if FEnabled then
    begin
        ComputeIterminal;

    // Method: Sum complex power going into all conductors of all terminals
        with ActiveCircuit.Solution do
            for k := 1 to Yorder do
            begin
                n := NodeRef^[k];
                if n > 0 then
                begin
                    if ActiveCircuit.PositiveSequence then
                        Caccum(cLoss, CmulReal(Cmul(NodeV^[n], conjg(Iterminal^[k])), 3.0))
                    else
                        Caccum(cLoss, Cmul(NodeV^[n], conjg(Iterminal^[k])));
                end;
            end;
    end;


    Result := cLoss;

end;

function TDSSCktElement.Get_MaxPower(idxTerm: Integer): Complex;
{Get power in the phase with the max current and return equivalent power as if it were balanced in all phases
 2/12/2019}
var
    cPower: Complex;
    i, k,
    nref: Integer;
    MaxCurr,
    CurrMag: Double;
    MaxPhase: Integer;

begin

    ActiveTerminalIdx := idxTerm;   // set active Terminal
    cPower := CZERO;
    if FEnabled then
    begin
        ComputeIterminal;

    // Method: Get power in the phase with max current of active terminal
    // Multiply by Nphases and return

        MaxCurr := 0.0;
        MaxPhase := 1;  // Init this so it has a non zero value
        k := (idxTerm - 1) * Fnconds; // starting index of terminal
        for i := 1 to Fnphases do
        begin
            CurrMag := Cabs(Iterminal[k + i]);
            if CurrMag > MaxCurr then
            begin
                MaxCurr := CurrMag;
                MaxPhase := i
            end;
        end;

        nref := ActiveTerminal.TermNodeRef^[k + MaxPhase]; // grounded node will give zero
        with ActiveCircuit.Solution do     // Get power into max phase of active terminal
            Cpower := Cmul(NodeV^[nref], conjg(Iterminal[k + MaxPhase]));

       // Compute equivalent total power of all phases assuming equal to max power in all phases
        with Cpower do
        begin
            re := re * Fnphases;  // let compiler handle type coercion
            im := im * Fnphases;
        end;

       {If this is a positive sequence circuit (Fnphases=1),
        then we need to multiply by 3 to get the 3-phase power}
        if ActiveCircuit.PositiveSequence then
            cPower := cMulReal(cPower, 3.0);
    end;

    Result := cPower;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.GetPhasePower(PowerBuffer: pComplexArray);
// Get the power in each phase (complex losses) of active terminal
// neutral conductors are ignored by this routine
var
    i, n: Integer;
begin

    if FEnabled then
    begin
        ComputeIterminal;

        with ActiveCircuit.Solution do
            for i := 1 to Yorder do
            begin
                n := NodeRef^[i]; // increment through terminals
                if n > 0 then
                begin
                    if ActiveCircuit.PositiveSequence then
                        PowerBuffer^[i] := CmulReal(Cmul(NodeV^[n], conjg(Iterminal^[i])), 3.0)
                    else
                        PowerBuffer^[i] := Cmul(NodeV^[n], conjg(Iterminal^[i]));
                end;
            end;
    end
    else
        for i := 1 to Yorder do
            PowerBuffer^[i] := CZERO;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.GetPhaseLosses(var Num_Phases: Integer; LossBuffer: pComplexArray);
// Get the losses in each phase (complex losses);  Power difference coming out
// each phase. Note: This can be misleading if the nodev voltage is greatly unbalanced.
// neutral conductors are ignored by this routine
var
    i, j, k, n: Integer;
    cLoss: Complex;
begin


    Num_Phases := Fnphases;
    if FEnabled then
    begin
        ComputeIterminal;

        with ActiveCircuit.Solution do
            for i := 1 to Num_Phases do
            begin
                cLoss := cmplx(0.0, 0.0);
                for j := 1 to FNTerms do
                begin
                    k := (j - 1) * FNconds + i;
                    n := NodeRef^[k]; // increment through terminals
                    if n > 0 then
                    begin
                        if ActiveCircuit.PositiveSequence then
                            Caccum(cLoss, CmulReal(Cmul(NodeV^[n], conjg(Iterminal^[k])), 3.0))
                        else
                            Caccum(cLoss, Cmul(NodeV^[n], conjg(Iterminal^[k])));
                    end;
                end;
                LossBuffer^[i] := cLoss;
            end;

    end
    else
        for i := 1 to Num_Phases do
            LossBuffer^[i] := CZERO;
end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCktElement.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i, j: Integer;

begin

    inherited DumpProperties(F, Complete);

    if FEnabled then
        Writeln(F, '! ENABLED')
    else
        Writeln(F, '! DISABLED');
    if Complete then
    begin

        Writeln(F, '! NPhases = ', Fnphases: 0);
        Writeln(F, '! Nconds = ', Fnconds: 0);
        Writeln(F, '! Nterms = ', fNterms: 0);
        Writeln(F, '! Yorder = ', Yorder: 0);
        Write(F, '! NodeRef = "');
        if NodeRef = NIL then
            Write(F, 'nil')
        else
            for i := 1 to Yorder do
                Write(F, NodeRef^[i]: 0, ' ');
        Writeln(F, '"');
        Write(F, '! Terminal Status: [');
        for i := 1 to fNTerms do
            for j := 1 to Fnconds do
            begin
                if Terminals^[i].Conductors^[j].Closed then
                    Write(F, 'C ')
                else
                    Write(F, 'O ');
            end;
        Writeln(F, ']');
        Write(F, '! Terminal Bus Ref: [');
        for i := 1 to fNTerms do
            for j := 1 to Fnconds do
            begin
                Write(F, Terminals^[i].BusRef: 0, ' ');
            end;
        Writeln(F, ']');
        Writeln(F);

        if YPrim <> NIL then
        begin
            Writeln(F, '! YPrim (G matrix)');
            for i := 1 to Yorder do
            begin
                Write(F, '! ');
                for j := 1 to Yorder do
                    Write(F, Format(' %13.10g |', [YPrim.GetElement(i, j).re]));
                Writeln(F);
            end;
            Writeln(F, '! YPrim (B Matrix) = ');
            for i := 1 to Yorder do
            begin
                Write(F, '! ');
                for j := 1 to Yorder do
                    Write(F, Format(' %13.10g |', [YPrim.GetElement(i, j).im]));
                Writeln(F);
            end;
        end;

    end;  {If complete}


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TDSSCktElement.DoYprimCalcs(Ymatrix: TCMatrix);

var
    i, j, k, ii, jj, ElimRow: Integer;
    Ynn, Yij, Yin, Ynj: Complex;
    RowEliminated: pIntegerArray;
    ElementOpen: Boolean;
    cEpsilon: Complex;

begin
     {Now Account for Open Conductors
      Perform a Kron Reduction on rows where I is forced to zero.
      Then for any conductor that is open, zero out row and column.
      }
    with Ymatrix do
    begin
        ElementOpen := FALSE;
        k := 0;
        for i := 1 to fNTerms do
        begin
            for j := 1 to Fnconds do
            begin
                if not Terminals^[i].Conductors^[j].Closed then
                begin
                    if not ElementOpen then
                    begin
                        RowEliminated := AllocMem(Sizeof(Integer) * Yorder);
                        ElementOpen := TRUE;
                        cEpsilon := Cmplx(EPSILON, 0.0);
                    end;
                // First do Kron Reduction
                    ElimRow := j + k;
                    Ynn := GetElement(ElimRow, ElimRow);
                    if Cabs(Ynn) = 0.0 then
                        Ynn.re := EPSILON;
                    RowEliminated^[ElimRow] := 1;
                    for ii := 1 to Yorder do
                    begin
                        if RowEliminated^[ii] = 0 then
                        begin
                            Yin := GetElement(ii, ElimRow);
                            for jj := ii to Yorder do
                                if RowEliminated^[jj] = 0 then
                                begin
                                    Yij := GetElement(ii, jj);
                                    Ynj := GetElement(ElimRow, jj);
                                    SetElemSym(ii, jj, Csub(Yij, Cdiv(cmul(Yin, Ynj), Ynn)));
                                end;
                        end;
                    end;
                // Now zero out row and column
                    ZeroRow(ElimRow);
                    ZeroCol(ElimRow);
                // put a small amount on the diagonal in case node gets isolated
                    SetElement(ElimRow, ElimRow, cEpsilon);
                end;
            end;
            k := k + Fnconds;
        end;
       { Clean up at end of loop.
         Add in cEpsilon to diagonal elements of remaining rows to avoid leaving a bus hanging.
         This happens on low-impedance simple from-to elements when one terminal opened.
       }
        if ElementOpen then
        begin
            for ii := 1 to Yorder do
                if RowEliminated^[ii] = 0 then
                    AddElement(ii, ii, cEpsilon);

            Reallocmem(RowEliminated, 0);
        end;
    end;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure TDSSCktElement.SumCurrents;

// sum Terminal Currents into System  Currents Array
// Primarily for Newton Iteration

var
    i: Integer;

begin
    if FEnabled then
    begin
        ComputeIterminal;
        with ActiveCircuit.Solution do
            for i := 1 to Yorder do
                Caccum(Currents^[NodeRef^[i]], Iterminal^[i]);  // Noderef=0 is OK
    end;
end;

procedure TDSSCktElement.GetTermVoltages(iTerm: Integer; VBuffer: PComplexArray);

// Bus Voltages at indicated terminal
// Fill Vbuffer array which must be adequately allocated by calling routine

var
    ncond, i: Integer;

begin

    try
        ncond := NConds;

     {return Zero if terminal number improperly specified}
        if (iTerm < 1) or (iTerm > fNterms) then
        begin
            for i := 1 to Ncond do
                VBuffer^[i] := CZERO;
            Exit;
        end;

        with ActiveCircuit.Solution do
            for i := 1 to NCond do
                Vbuffer^[i] := NodeV^[Terminals^[iTerm].TermNodeRef^[i]];

    except
        On E: Exception do
            DoSimpleMsg('Error filling voltage buffer in GetTermVoltages for Circuit Element:' + DSSclassName + '.' + Name + CRLF +
                'Probable Cause: Invalid definition of element.' + CRLF +
                'System Error Message: ' + E.Message, 755);
    end;

end;


procedure TDSSCktElement.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[ArrayOffset + 1] := Format('%-g', [BaseFrequency]);  // Base freq
    PropertyValue[ArrayOffset + 2] := 'true';  // Enabled
    FEnabledProperty := ArrayOffset + 2;     // keep track of this

    inherited InitPropertyValues(ArrayOffset + 2);
end;

function TDSSCktElement.GetPropertyValue(Index: Integer): String;
begin
    if Index = FEnabledProperty then
    begin
        if Enabled then
            Result := 'true'
        else
            Result := 'false';
           // *** RCD 6-18-03 commented out PropertyValue[FEnabledProperty] := Result; // Keep this in synch
    end
    else
        Result := inherited GetPropertyValue(Index);
end;

procedure TDSSCktElement.GetSeqLosses(var PosSeqLosses, NegSeqLosses, ZeroModeLosses: complex);
begin

{ For the base class, just return CZERO}

{Derived classes have to supply appropriate function}

    PosSeqLosses := CZERO;
    NegSeqLosses := CZERO;
    ZeroModeLosses := CZERO;

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

procedure TDSSCktElement.MakePosSequence;
var
    i: Integer;
    grnd: Boolean;
begin
    for i := 1 to FNterms do
    begin
        grnd := IsGroundBus(FBusNames^[i]);
        FBusNames^[i] := StripExtension(FBusNames^[i]);
        if grnd then
            FBusNames^[i] := FBusNames^[i] + '.0';
    end;
end;

procedure TDSSCktElement.ComputeVterminal;

{Put terminal voltages in an array}

var
    i: Integer;

begin
    with ActiveCircuit.solution do
        for i := 1 to Yorder do
            VTerminal^[i] := NodeV^[NodeRef^[i]];
end;


procedure TDSSCktElement.ZeroITerminal;
var
    i: Integer;

begin
    for i := 1 to Yorder do
        ITerminal^[i] := CZERO;
end;

end.
