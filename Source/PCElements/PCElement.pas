unit PCElement;

{$M+}
{
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    CktElement,
    ucomplex,
    DSSClass,
    Spectrum,
    Arraydef,
    Meterelement,
    Fmonitor,
    DynamicExp;

type
    TPCElement = class(TDSSCktElement)
    PRIVATE
        FIterminalUpdated: Boolean;

    PROTECTED
        procedure GetTerminalCurrents(Curr: pComplexArray; ActorID: Integer); VIRTUAL;
        function Get_Variable(i: Integer): Double; VIRTUAL;
        procedure Set_Variable(i: Integer; Value: Double); VIRTUAL;

    PUBLIC

        DynamicEq,                                 // Name of the local Dynamic equation (if any)
        Spectrum: String;
        SpectrumObj: TSpectrumObj;

        MeterObj,  {Upline Energymeter}
        SensorObj: TMeterElement;            // Upline Sensor for this element
       {by Dahei}
        FMonObj: TFMonitorObj;
        cluster_num: Integer;
        NdNumInCluster: Integer;
        nVLeaders: Integer;                  // How many virtual leaders for this pcelement
        FMonObj2: TFMonitorObj;
        cluster_num2: Integer;
        NdNumInCluster2: Integer;

        InjCurrent: pComplexArray;
        DynamicEqObj: TDynamicExpObj;           // Reference to the local Dynamic equation (if any)

        DynamicEqVals: array of DynSlot;         // Memory space for the variable values in time
        DynOut,                                    // Memory space for referencing the output values
        DynamicEqPair: array of Integer;         // Memory space for assigning calculated values to vars


        constructor Create(ParClass: TDSSClass);
        destructor Destroy; OVERRIDE;

        procedure ZeroInjCurrent;

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;     // Get present values of terminal
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;  // Get present values of terminal
        procedure ComputeIterminal(ActorID: Integer); OVERRIDE;
        function InjCurrents(ActorID: Integer): Integer; OVERRIDE;
        procedure CalcYPrimContribution(Curr: pComplexArray; ActorID: Integer);
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

      // Sweep solution removed  PROCEDURE BackwardSweep;Override;

      // For Harmonics Mode
        procedure InitHarmonics(ActorID: Integer); VIRTUAL;
        procedure set_ITerminalUpdated(const Value: Boolean; ActorID: Integer);
       // For Dynamics Mode and Control Devices
        procedure InitStateVars(ActorID: Integer); VIRTUAL;
        procedure IntegrateStates(ActorID: Integer); VIRTUAL;
        function NumVariables: Integer; VIRTUAL;
        procedure GetAllVariables(States: pDoubleArray); VIRTUAL;

        function VariableName(i: Integer): String; VIRTUAL;
        function LookupVariable(const s: String): Integer;

        function CheckIfDynVar(myVar: String; ActorID: Integer): Integer;
        procedure SetDynOutput(myVar: String);
        function GetDynOutputStr(): String;
       // Functions for inverter based PCE
        function Get_InverterON: Boolean; VIRTUAL;
        function Get_Varmode: Integer; VIRTUAL;
        function Get_VWmode: Boolean; VIRTUAL;
        function Get_VVmode: Boolean; VIRTUAL;
        function Get_WPmode: Boolean; VIRTUAL;
        function Get_WVmode: Boolean; VIRTUAL;
        function Get_DRCmode: Boolean; VIRTUAL;
        function Get_AVRmode: Boolean; VIRTUAL;

        procedure Set_InverterON(const Value: Boolean); VIRTUAL;
        procedure Set_Varmode(const Value: Integer); VIRTUAL;
        procedure Set_VWmode(const Value: Boolean); VIRTUAL;
        procedure Set_VVmode(const Value: Boolean); VIRTUAL;
        procedure Set_WPmode(const Value: Boolean); VIRTUAL;
        procedure Set_WVmode(const Value: Boolean); VIRTUAL;
        procedure Set_DRCmode(const Value: Boolean); VIRTUAL;
        procedure Set_AVRmode(const Value: Boolean); VIRTUAL;

        property Variable[i: Integer]: Double READ Get_Variable WRITE Set_Variable;

//       Property ITerminalUpdated:Boolean read FITerminalUpdated write set_ITerminalUpdated;

    end;


implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Classes,
    Utilities;

constructor TPCElement.Create(ParClass: TDSSClass);
begin
    inherited Create(ParClass);
    Spectrum := 'default';
    SpectrumObj := NIL;  // have to allocate later because not guaranteed there will be one now.
    SensorObj := NIL;
    MeterObj := NIL;
    InjCurrent := NIL;
    DynamicEq := '';
    DynamicEqObj := NIL;
    setlength(DynamicEqVals, 0);
    setlength(DynamicEqPair, 0);
    setlength(DynOut, 0);
    FIterminalUpdated := FALSE;

    DSSObjType := PC_ELEMENT;
end;

destructor TPCElement.Destroy;
begin
    if Assigned(InjCurrent) then
        Reallocmem(InjCurrent, 0);
    inherited Destroy;
end;

function TPCElement.InjCurrents(ActorID: Integer): Integer;

// Add injection currents into System currents array

var
    i: Integer;
begin
    Result := 0;
    with ActiveCircuit[ActorID].Solution do
        for i := 1 to Yorder do
            Caccum(Currents^[NodeRef^[i]], InjCurrent^[i]);
end;

procedure TPCElement.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);
begin
    DoErrorMsg('PCElement.InjCurrents', ('Improper call to GetInjCurrents for Element: ' + Name + '.'),
        'Called PCELEMENT class virtual function instead of actual.', 640)
end;

//----------------------------------------------------------------------------
{Evaluates if the value provided corresponds to a constant value or to an operand
 for calculating the value using the simulation results}
function TPCElement.CheckIfDynVar(myVar: String; ActorID: Integer): Integer;
var
    myOp: Integer;        // Operator found
    myValue: String;         // Value entered by the user
begin

    Result := -1;
    if Assigned(DynamicEqObj) then
    begin

        Result := DynamicEqObj.Get_Var_Idx(myVar);
        if (Result >= 0) and (Result < 50000) then
        begin
            myValue := Parser[ActorID].StrValue;
            if (DynamicEqObj.Check_If_CalcValue(myValue, myOp)) then
            begin
        // Adss the pair (var index + operand index)
                setlength(DynamicEqPair, length(DynamicEqPair) + 2);
                DynamicEqPair[High(DynamicEqPair) - 1] := Result;
                DynamicEqPair[High(DynamicEqPair)] := myOp;
            end
            else // Otherwise, move the value to the values array
                DynamicEqVals[Result][0] := Parser[ActorID].DblValue;
        end
        else
            Result := -1;     // in case is a constant

    end;

end;

//----------------------------------------------------------------------------
{Returns the names of the variables to be used as outputs for the dynamic expression}
function TPCElement.GetDynOutputStr(): String;
var
    idx: Integer;
begin
    Result := '[';                   // Open array str
    if DynamicEqObj <> NIL then        // Making sure we have a dynamic eq linked
    begin
        for idx := 0 to High(DynOut) do
            Result := Result + DynamicEqObj.Get_VarName(DynOut[idx]) + ',';
    end;

    Result := Result + ']';         // Close array str
end;

//----------------------------------------------------------------------------
{Obtains the indexes of the given variables to use them as reference for setting
the dynamic output for the generator}
procedure TPCElement.SetDynOutput(myVar: String);
var
    VarIdx,
    idx: Integer;
    myStrArray: TStringList;
begin
    if DynamicEqObj <> NIL then        // Making sure we have a dynamic eq linked
    begin
    // First, set the length for the index array, 2 variables in this case
        setlength(DynOut, 2);
        myStrArray := TStringList.Create;
        InterpretTStringListArray(myVar, myStrArray);
    // ensuring they are lower case
        for idx := 0 to (myStrArray.Count - 1) do
        begin

            myStrArray[idx] := LowerCase(myStrArray[idx]);
            VarIdx := DynamicEqObj.Get_Out_Idx(myStrArray[idx]);
            if (VarIdx < 0) then
        // Being here means that the given name doesn't exist or is a constant
                DoSimpleMsg('DynamicExp variable "' + myStrArray[idx] + '" not found or not defined as an output.', 50008)
            else
                DynOut[idx] := VarIdx;

        end;

        myStrArray.Free;
    end
    else
        DoSimpleMsg('A DynamicExp object needs to be assigned to this element before this declaration: DynOut = [' + myVar + ']', 50007);
end;

//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure TPCElement.GetTerminalCurrents(Curr: pComplexArray; ActorID: Integer);

// This is called only if we need to compute the terminal currents from the inj currents
// Such as for Harmonic model

var
    i: Integer;
begin

    if FITerminalUpdated then
    begin   // Just copy iTerminal unless iTerminal=Curr
        if Curr <> ITerminal then
            for i := 1 to Yorder do
                Curr^[i] := ITerminal^[i];
    end
    else
    begin
        YPrim.MVmult(Curr, VTerminal);
        for i := 1 to Yorder do
            CAccum(Curr^[i], CNegate(Injcurrent^[i]));
        set_ITerminalUpdated(TRUE, ActorID);
    end;
    IterminalSolutionCount[ActorID] := ActiveCircuit[ActorID].Solution.SolutionCount;
end;

//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure TPCElement.GetCurrents(Curr: pComplexArray; ActorID: Integer);

{Gets total Currents going INTO a devices terminals}

var
    i: Integer;

begin
    try

        with ActiveCircuit[ActorID].Solution do
        begin
            if (Enabled) then
            begin

                if (LastSolutionWasDirect) and (not (IsDynamicModel or IsHarmonicModel)) then
                begin

           // Take a short cut and get Currents from YPrim only
           // For case where model is entirely in Y matrix

                    CalcYPrimContribution(Curr, ActorID);

                end
                else
                begin

                    GetTerminalCurrents(Curr, ActorID);
                end; {IF}

            end
            else
            begin   // not enabled
                for i := 1 to Yorder do
                    Curr^[i] := CZERO;
            end;
        end;  {With}


    except
        On E: Exception do
            DoErrorMsg(('GetCurrents for Element: ' + Name + '.'), E.Message,
                'Inadequate storage allotted for circuit element.', 641);
    end;

end;

procedure TPCElement.CalcYPrimContribution(Curr: pComplexArray; ActorID: Integer);

begin
    ComputeVTerminal(ActorID);
      // Apply these voltages to Yprim
    YPrim.MVMult(Curr, Vterminal);
end;

procedure TPCElement.InitHarmonics(ActorID: Integer);
begin
  // By default do nothing in the base class

end;

procedure TPCElement.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[ArrayOffset + 1] := Spectrum;

    inherited InitPropertyValues(ArrayOffset + 1);

end;

procedure TPCElement.InitStateVars(ActorID: Integer);
begin
    // By default do nothing

end;

procedure TPCElement.IntegrateStates(ActorID: Integer);
begin
 // inherited;
 // By default do nothing

end;

procedure TPCElement.GetAllVariables(States: pDoubleArray);
begin
     {Do Nothing}
end;

function TPCElement.NumVariables: Integer;
begin
    Result := 0;
end;

function TPCElement.VariableName(i: Integer): String;
begin
   {Do Nothing}
    Result := '';
end;

function TPCElement.LookupVariable(const S: String): Integer;

{Search through variable name list and return index if found}
{Compare up to length of S}

var
    i, TestLength: Integer;

begin
    Result := -1;   // Returns -1 for error not found
    TestLength := Length(S);
    for i := 1 to NumVariables do
    begin
        if CompareText(Copy(VariableName(i), 1, TestLength), S) = 0 then
        begin
            Result := i;
            Break;
        end;
    end;
end;


procedure TPCElement.DumpProperties(var F: TextFile; Complete: Boolean);
var
    i: Integer;
begin
    inherited DumpProperties(F, Complete);

    if Complete then
    begin
        Writeln(F, '! VARIABLES');
        for i := 1 to NumVariables do
        begin
            Writeln(F, '! ', i: 2, ': ', VariableName(i), ' = ', Format('%-.5g', [Get_Variable(i)]));
        end;
    end;

end;

function TPCElement.Get_Variable(i: Integer): Double;
begin
   {do Nothing here -- up to override function}
    Result := -9999.99;
end;

procedure TPCElement.Set_Variable(i: Integer; Value: Double);
begin
  {Do Nothing}
end;

function TPCElement.Get_InverterON: Boolean;
begin
    Result := FALSE;
end;
// ============================================================Get_Varmode===============================
function TPCElement.Get_Varmode: Integer;
begin
    Result := 0;
end;
// ============================================================Get_VWmode===============================
function TPCElement.Get_VWmode: Boolean;
begin
    Result := FALSE;
end;
// ============================================================Get_VVmode===============================
function TPCElement.Get_VVmode: Boolean;
begin
    Result := FALSE;
end;
// ============================================================Get_WPmode===============================
function TPCElement.Get_WPmode: Boolean;
begin
    Result := FALSE;
end;
// ============================================================Get_WVmode===============================
function TPCElement.Get_WVmode: Boolean;
begin
    Result := FALSE;                                                                //  engaged from InvControl (not ExpControl)
end;
// ============================================================Get_DRCmode===============================
function TPCElement.Get_DRCmode: Boolean;
begin
    Result := FALSE;                                                               //  engaged from InvControl (not ExpControl)
end;

// ============================================================Get_AVRmode===============================
function TPCElement.Get_AVRmode: Boolean;
begin
    Result := FALSE;                                                               //  engaged from InvControl (not ExpControl)
end;

procedure TPCElement.Set_InverterON(const Value: Boolean);
begin
    // Do nothing if reaches this instance
end;

procedure TPCElement.Set_Varmode(const Value: Integer);
begin
    // Do nothing if reaches this instance
end;

procedure TPCElement.Set_VWmode(const Value: Boolean);
begin
    // Do nothing if reaches this instance
end;

procedure TPCElement.Set_VVmode(const Value: Boolean);
begin
    // Do nothing if reaches this instance
end;

procedure TPCElement.Set_WPmode(const Value: Boolean);
begin
    // Do nothing if reaches this instance
end;

procedure TPCElement.Set_WVmode(const Value: Boolean);
begin
    // Do nothing if reaches this instance
end;

procedure TPCElement.Set_DRCmode(const Value: Boolean);
begin
    // Do nothing if reaches this instance
end;

procedure TPCElement.Set_AVRmode(const Value: Boolean);
begin
    // Do nothing if reaches this instance
end;


procedure TPCElement.ComputeIterminal(ActorID: Integer);
var
    i: Integer;
begin
    if IterminalSolutionCount[ActorID] <> ActiveCircuit[ActorID].Solution.SolutionCount then
    begin
        GetCurrents(Iterminal, ActorID);
        IterminalSolutionCount[ActorID] := ActiveCircuit[ActorID].Solution.SolutionCount;
    end;

end;

procedure TPCElement.ZeroInjCurrent;
var
    i: Integer;
begin
    for i := 1 to Yorder do
        InjCurrent^[i] := CZERO;
end;

procedure TPCElement.set_ITerminalUpdated(const Value: Boolean; ActorID: Integer);
begin
    FITerminalUpdated := Value;
    if Value then
        ITerminalSolutionCount[ActorID] := ActiveCircuit[ActorID].Solution.SolutionCount;
end;

end.
