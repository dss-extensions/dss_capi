unit PCElement;

{$M+}
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
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
    Meterelement;

type
    TPCElement = class(TDSSCktElement)
    PRIVATE
        FIterminalUpdated: Boolean;
        procedure set_ITerminalUpdated(const Value: Boolean);
    PROTECTED
        procedure GetTerminalCurrents(Curr: pComplexArray); VIRTUAL;
        function Get_Variable(i: Integer): Double; VIRTUAL;
        procedure Set_Variable(i: Integer; Value: Double); VIRTUAL;

    PUBLIC


        Spectrum: String;
        SpectrumObj: TSpectrumObj;

        MeterObj,  {Upline Energymeter}
        SensorObj: TMeterElement; // Upline Sensor for this element

        InjCurrent: pComplexArray;


        constructor Create(ParClass: TDSSClass);
        destructor Destroy; OVERRIDE;

        procedure ZeroInjCurrent;

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Get present values of terminal
        procedure GetInjCurrents(Curr: pComplexArray); OVERRIDE; // Get present values of terminal
        procedure ComputeIterminal; OVERRIDE;
        function InjCurrents: Integer; OVERRIDE;
        procedure CalcYPrimContribution(Curr: pComplexArray);
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

      // Sweep solution removed  PROCEDURE BackwardSweep;Override;

      // For Harmonics Mode
        procedure InitHarmonics; VIRTUAL;

       // For Dynamics Mode and Control Devices
        procedure InitStateVars; VIRTUAL;
        procedure IntegrateStates; VIRTUAL;
        function NumVariables: Integer; VIRTUAL;
        procedure GetAllVariables(States: pDoubleArray); VIRTUAL;

        function VariableName(i: Integer): String; VIRTUAL;
        function LookupVariable(const s: String): Integer;

        property Variable[i: Integer]: Double READ Get_Variable WRITE Set_Variable;

        property ITerminalUpdated: Boolean READ FITerminalUpdated WRITE set_ITerminalUpdated;

    end;


implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils;

constructor TPCElement.Create(ParClass: TDSSClass);
begin
    inherited Create(ParClass);
    Spectrum := 'default';
    SpectrumObj := NIL;  // have to allocate later because not guaranteed there will be one now.
    SensorObj := NIL;
    MeterObj := NIL;
    InjCurrent := NIL;
    FIterminalUpdated := FALSE;

    DSSObjType := PC_ELEMENT;
end;

destructor TPCElement.Destroy;
begin
    if Assigned(InjCurrent) then
        Reallocmem(InjCurrent, 0);
    inherited Destroy;
end;

function TPCElement.InjCurrents: Integer;

// Add injection currents into System currents array

var
    i: Integer;
begin
    Result := 0;
    with ActiveCircuit.Solution do
        for i := 1 to Yorder do
            Caccum(Currents^[NodeRef^[i]], InjCurrent^[i]);
end;

procedure TPCElement.GetInjCurrents(Curr: pComplexArray);
begin
    DoErrorMsg('PCElement.InjCurrents', ('Improper call to GetInjCurrents for Element: ' + Name + '.'),
        'Called PCELEMENT class virtual function instead of actual.', 640)
end;

//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure TPCElement.GetTerminalCurrents(Curr: pComplexArray);

// This is called only if we need to compute the terminal currents from the inj currents
// Such as for Harmonic model

var
    i: Integer;
begin

    if ITerminalUpdated then
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
        IterminalUpdated := TRUE;
    end;
    IterminalSolutionCount := ActiveCircuit.Solution.SolutionCount;
end;

//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =

procedure TPCElement.GetCurrents(Curr: pComplexArray);

{Gets total Currents going INTO a devices terminals}

var
    i: Integer;

begin
    try

        with ActiveCircuit.Solution do
        begin
            if (Enabled) then
            begin

                if (LastSolutionWasDirect) and (not (IsDynamicModel or IsHarmonicModel)) then
                begin

           // Take a short cut and get Currents from YPrim only
           // For case where model is entirely in Y matrix

                    CalcYPrimContribution(Curr);

                end
                else
                begin

                    GetTerminalCurrents(Curr);
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

procedure TPCElement.CalcYPrimContribution(Curr: pComplexArray);

begin
    ComputeVTerminal;
      // Apply these voltages to Yprim
    YPrim.MVMult(Curr, Vterminal);
end;

procedure TPCElement.InitHarmonics;
begin
  // By default do nothing in the base class

end;

procedure TPCElement.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[ArrayOffset + 1] := Spectrum;

    inherited InitPropertyValues(ArrayOffset + 1);

end;

procedure TPCElement.InitStateVars;
begin
    // By default do nothing

end;

procedure TPCElement.IntegrateStates;
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


procedure TPCElement.ComputeIterminal;
begin
    if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
    begin
        GetCurrents(Iterminal);
        IterminalSolutionCount := ActiveCircuit.Solution.SolutionCount;
    end;

end;

procedure TPCElement.ZeroInjCurrent;
var
    i: Integer;
begin
    for i := 1 to Yorder do
        InjCurrent^[i] := CZERO;
end;

procedure TPCElement.set_ITerminalUpdated(const Value: Boolean);
begin
    FITerminalUpdated := Value;
    if Value then
        ITerminalSolutionCount := ActiveCircuit.Solution.SolutionCount;
end;

end.
