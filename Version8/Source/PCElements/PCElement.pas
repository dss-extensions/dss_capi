unit PCElement;

{$M+}
{
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

USES  CktElement, ucomplex, DSSClass, Spectrum, Arraydef, Meterelement,
      Fmonitor, DynamicExp;

TYPE
   TPCElement = class(TDSSCktElement)
  private
      FIterminalUpdated:Boolean;

    Protected
      Procedure GetTerminalCurrents(Curr:pComplexArray; ActorID : Integer); virtual;
      function Get_Variable(i: Integer): Double; virtual;
      procedure Set_Variable(i: Integer;  Value: Double);  virtual;

     public

       DynamicEq,                                 // Name of the local Dynamic equation (if any)
       Spectrum       : String;
       SpectrumObj    : TSpectrumObj;

       MeterObj,  {Upline Energymeter}
       SensorObj      : TMeterElement;            // Upline Sensor for this element
       {by Dahei}
       FMonObj        : TFMonitorObj;
       cluster_num    : integer;
       NdNumInCluster : integer;
       nVLeaders      : integer;                  // How many virtual leaders for this pcelement
       FMonObj2       : TFMonitorObj;
       cluster_num2   : integer;
       NdNumInCluster2: integer;

       InjCurrent     : pComplexArray;
       DynamicEqObj   : TDynamicExpObj;           // Reference to the local Dynamic equation (if any)

       DynamicEqVals  : array of DynSlot;         // Memory space for the variable values in time
       DynOut,                                    // Memory space for referencing the output values
       DynamicEqPair  : array of Integer;         // Memory space for assigning calculated values to vars



       constructor Create(ParClass:TDSSClass);
       destructor Destroy; override;

       Procedure ZeroInjCurrent;

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       Procedure GetCurrents(Curr: pComplexArray; ActorID : Integer); Override;     // Get present values of terminal
       Procedure GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override;  // Get present values of terminal
       Procedure ComputeIterminal(ActorID : Integer);Override;
       Function  InjCurrents(ActorID : Integer):Integer; Override;
       Procedure CalcYPrimContribution(Curr: pComplexArray; ActorID : Integer);
       Procedure DumpProperties(Var F:TextFile; Complete:Boolean);Override;

      // Sweep solution removed  PROCEDURE BackwardSweep;Override;

      // For Harmonics Mode
       Procedure InitHarmonics(ActorID : Integer); Virtual;
       procedure set_ITerminalUpdated(const Value: Boolean; ActorID : Integer);
       // For Dynamics Mode and Control Devices
       Procedure InitStateVars(ActorID : Integer); Virtual;
       Procedure IntegrateStates(ActorID : Integer);Virtual;
       Function NumVariables:Integer; Virtual;
       Procedure GetAllVariables( States:pDoubleArray);Virtual;

       Function VariableName(i:Integer):String;Virtual;
       Function LookupVariable(const s:string):Integer;

       FUNCTION CheckIfDynVar(myVar  : String; ActorID : Integer):Integer;
       PROCEDURE SetDynOutput(myVar  : String);
       FUNCTION GetDynOutputStr(): string;
       // Functions for inverter based PCE
       FUNCTION Get_InverterON:Boolean; Virtual;
       function Get_Varmode: Integer; Virtual;
       function Get_VWmode: Boolean; Virtual;
       function Get_VVmode: Boolean; Virtual;
       function Get_WPmode: Boolean; Virtual;
       function Get_WVmode: Boolean; Virtual;
       function Get_DRCmode: Boolean; Virtual;
       function Get_AVRmode: Boolean; Virtual;

       PROCEDURE Set_InverterON(const Value: Boolean); Virtual;
       procedure Set_Varmode(const Value: Integer); Virtual;
       procedure Set_VWmode(const Value: Boolean); Virtual;
       procedure Set_VVmode(const Value: Boolean); Virtual;
       procedure Set_WPmode(const Value: Boolean); Virtual;
       procedure Set_WVmode(const Value: Boolean); Virtual;
       procedure Set_DRCmode(const Value: Boolean); Virtual;
       procedure Set_AVRmode(const Value: Boolean); Virtual;

       Property Variable[i:Integer]:Double read Get_Variable write Set_Variable;

//       Property ITerminalUpdated:Boolean read FITerminalUpdated write set_ITerminalUpdated;

   end;


implementation

USES
    DSSClassDefs, DSSGlobals, Sysutils, Classes, Utilities;


Constructor TPCElement.Create(ParClass:TDSSClass);
Begin
    Inherited Create(ParClass);
    Spectrum    := 'default';
    SpectrumObj :=  NIL;  // have to allocate later because not guaranteed there will be one now.
    SensorObj   :=  NIL;
    MeterObj    :=  NIL;
    InjCurrent  :=  NIL;
    DynamicEq   :=  '';
    DynamicEqObj:=  NIL;
    setlength(DynamicEqVals,0);
    setlength(DynamicEqPair,0);
    setlength(DynOut,0);
    FIterminalUpdated := FALSE;
    
    DSSObjType := PC_ELEMENT;
End;

destructor TPCElement.Destroy;
Begin
   If Assigned(InjCurrent) Then Reallocmem(InjCurrent, 0);
   Inherited Destroy;
End;

Function TPCElement.InjCurrents(ActorID : Integer):Integer;

// Add injection currents into System currents array

VAR
   i:Integer;
Begin
    Result := 0;
    With ActiveCircuit[ActorID].Solution Do
    FOR i := 1 TO Yorder Do Caccum(Currents^[NodeRef^[i]], InjCurrent^[i]);
End;

Procedure TPCElement.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);
Begin
    DoErrorMsg('PCElement.InjCurrents',('Improper call to GetInjCurrents for Element: ' + Name + '.'),
        'Called PCELEMENT class virtual function instead of actual.', 640)
End;

//----------------------------------------------------------------------------
{Evaluates if the value provided corresponds to a constant value or to an operand
 for calculating the value using the simulation results}
FUNCTION TPCElement.CheckIfDynVar(myVar  : String; ActorID : Integer):Integer;
var
  myOp    : Integer;        // Operator found
  myValue : String;         // Value entered by the user
Begin

  Result := -1;
  If Assigned(DynamicEqObj) then
  Begin

    Result  :=   DynamicEqObj.Get_Var_Idx(myVar);
    if (Result >= 0) and (Result < 50000) then
    Begin
      myValue :=  Parser[ActorID].StrValue;
      if (DynamicEqObj.Check_If_CalcValue(myValue, myOp)) then
      Begin
        // Adss the pair (var index + operand index)
        setlength(DynamicEqPair,length(DynamicEqPair) + 2);
        DynamicEqPair[High(DynamicEqPair) - 1]  :=  Result;
        DynamicEqPair[High(DynamicEqPair)]      :=  myOp;
      End
      else // Otherwise, move the value to the values array
         DynamicEqVals[Result][0]  :=  Parser[ActorID].DblValue;
    End
    else
      Result := -1;     // in case is a constant

  End;

End;

//----------------------------------------------------------------------------
{Returns the names of the variables to be used as outputs for the dynamic expression}
FUNCTION TPCElement.GetDynOutputStr(): string;
var
  idx   : Integer;
Begin
  Result  :=  '[';                   // Open array str
  if DynamicEqObj <> nil then        // Making sure we have a dynamic eq linked
  Begin
    for idx := 0 to High(DynOut) do
      Result  :=  Result + DynamicEqObj.Get_VarName(DynOut[idx]) + ',';
  End;

  Result  :=  Result + ']';         // Close array str
End;

//----------------------------------------------------------------------------
{Obtains the indexes of the given variables to use them as reference for setting
the dynamic output for the generator}
PROCEDURE TPCElement.SetDynOutput(myVar  : String);
var
  VarIdx,
  idx         : Integer;
  myStrArray  : TStringList;
Begin
  if DynamicEqObj <> nil then        // Making sure we have a dynamic eq linked
  Begin
    // First, set the length for the index array, 2 variables in this case
    setlength(DynOut,2);
    myStrArray  :=  TStringList.Create;
    InterpretTStringListArray(myVar, myStrArray);
    // ensuring they are lower case
    for idx := 0 to ( myStrArray.Count - 1 ) do
    Begin

      myStrArray[idx]  :=  LowerCase(myStrArray[idx]);
      VarIdx           :=  DynamicEqObj.Get_Out_Idx(myStrArray[idx]);
      if ( VarIdx < 0 ) then
        // Being here means that the given name doesn't exist or is a constant
        DoSimpleMsg('DynamicExp variable "' + myStrArray[idx] + '" not found or not defined as an output.', 50008)
      else
        DynOut[idx] :=  VarIdx;

    End;

    myStrArray.Free;
  End
  else
      DoSimpleMsg('A DynamicExp object needs to be assigned to this element before this declaration: DynOut = [' + myVar + ']', 50007);
End;

//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =
Procedure TPCElement.GetTerminalCurrents(Curr:pComplexArray; ActorID : Integer);

// This is called only if we need to compute the terminal currents from the inj currents
// Such as for Harmonic model

Var i:Integer;
Begin

    If FITerminalUpdated Then
    Begin   // Just copy iTerminal unless iTerminal=Curr
       If Curr <> ITerminal Then
         For i := 1 to Yorder Do Curr^[i] := ITerminal^[i];
    End
    Else Begin
        YPrim.MVmult(Curr, VTerminal);
        For i := 1 to Yorder Do CAccum(Curr^[i], CNegate(Injcurrent^[i]));
        set_ITerminalUpdated(TRUE, ActorID);
    End;
    IterminalSolutionCount[ActorID] := ActiveCircuit[ActorID].Solution.SolutionCount;
End;

//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =

Procedure TPCElement.GetCurrents(Curr: pComplexArray; ActorID : Integer);

{Gets total Currents going INTO a devices terminals}

VAR
   i:Integer;

Begin
  TRY

   WITH ActiveCircuit[ActorID].Solution DO  Begin
     If  (Enabled)
     THEN Begin

       IF (LastSolutionWasDirect) AND (NOT (IsDynamicModel or IsHarmonicModel))
       THEN Begin
       
           // Take a short cut and get Currents from YPrim only
           // For case where model is entirely in Y matrix

           CalcYPrimContribution(Curr, ActorID);

       End
       ELSE Begin

           GetTerminalCurrents(Curr, ActorID);
       End; {IF}

     End
     ELSE Begin   // not enabled
          FOR i := 1 TO Yorder DO Curr^[i] := CZERO;
     End;
   End;  {With}


  EXCEPT
    On E: Exception Do DoErrorMsg(('GetCurrents for Element: ' + Name + '.'), E.Message,
        'Inadequate storage allotted for circuit element.', 641);
  End;

End;

PROCEDURE TPCElement.CalcYPrimContribution(Curr: pComplexArray; ActorID : Integer);

begin
      ComputeVTerminal(ActorID);
      // Apply these voltages to Yprim
      YPrim.MVMult(Curr, Vterminal);
end;

procedure TPCElement.InitHarmonics(ActorID : Integer);
begin
  // By default do nothing in the base class

end;

procedure TPCElement.InitPropertyValues(ArrayOffset: Integer);
begin

  PropertyValue[ArrayOffset + 1] := Spectrum;

  inherited InitPropertyValues(ArrayOffset + 1);

end;

procedure TPCElement.InitStateVars(ActorID : Integer);
begin
    // By default do nothing

end;

procedure TPCElement.IntegrateStates(ActorID : Integer);
begin
 // inherited;
 // By default do nothing

end;

procedure TPCElement.GetAllVariables( States: pDoubleArray);
begin
     {Do Nothing}
end;

function TPCElement.NumVariables: Integer;
begin
     Result := 0;
end;

Function TPCElement.VariableName(i: Integer):String;
begin
   {Do Nothing}
   Result := '';
end;

function TPCElement.LookupVariable(const S: string): Integer;

{Search through variable name list and return index if found}
{Compare up to length of S}

Var i, TestLength:integer;

begin
     Result := -1;   // Returns -1 for error not found
     TestLength := Length(S);
     For i := 1 to NumVariables Do
       Begin
         If CompareText(Copy(VariableName(i),1,TestLength), S) = 0 Then
           Begin
             Result := i;
             Break;
           End;
       End;
end;


procedure TPCElement.DumpProperties(var F: TextFile; Complete: Boolean);
Var
  i:Integer;
begin
  inherited DumpProperties(F, Complete);

  If Complete then
    Begin
        Writeln(F,'! VARIABLES');
        For i := 1 to NumVariables Do
          Begin
              Writeln(F, '! ',i:2, ': ',VariableName(i),' = ', Format('%-.5g',[Get_Variable(i)]));
          End;
    End;

end;

function TPCElement.Get_Variable(i: Integer): Double;
begin
   {do Nothing here -- up to override function}
   Result := -9999.99;
end;

procedure TPCElement.Set_Variable(i: Integer;  Value: Double);
begin
  {Do Nothing}
end;

function  TPCElement.Get_InverterON:Boolean;
  begin
    Result := False;
  end;
// ============================================================Get_Varmode===============================
function TPCElement.Get_Varmode: Integer;
  begin
    Result := 0;
  end;
// ============================================================Get_VWmode===============================
function TPCElement.Get_VWmode: Boolean;
  begin
    Result := False;
  end;
// ============================================================Get_VVmode===============================
function TPCElement.Get_VVmode: Boolean;
  begin
    Result :=False;
  end;
// ============================================================Get_WPmode===============================
function TPCElement.Get_WPmode: Boolean;
  begin
    Result := False;
  end;
// ============================================================Get_WVmode===============================
function TPCElement.Get_WVmode: Boolean;
  begin
    Result := False;                                                                //  engaged from InvControl (not ExpControl)
  end;
// ============================================================Get_DRCmode===============================
function TPCElement.Get_DRCmode: Boolean;
  begin
    Result := False;                                                               //  engaged from InvControl (not ExpControl)
  end;

// ============================================================Get_AVRmode===============================
function TPCElement.Get_AVRmode: Boolean;
  begin
    Result := False;                                                               //  engaged from InvControl (not ExpControl)
  end;

PROCEDURE TPCElement.Set_InverterON(const Value: Boolean);
  Begin
    // Do nothing if reaches this instance
  End;

PROCEDURE TPCElement.Set_Varmode(const Value: Integer);
  Begin
    // Do nothing if reaches this instance
  End;

PROCEDURE TPCElement.Set_VWmode(const Value: Boolean);
  Begin
    // Do nothing if reaches this instance
  End;

PROCEDURE TPCElement.Set_VVmode(const Value: Boolean);
  Begin
    // Do nothing if reaches this instance
  End;

PROCEDURE TPCElement.Set_WPmode(const Value: Boolean);
  Begin
    // Do nothing if reaches this instance
  End;

PROCEDURE TPCElement.Set_WVmode(const Value: Boolean);
  Begin
    // Do nothing if reaches this instance
  End;

PROCEDURE TPCElement.Set_DRCmode(const Value: Boolean);
  Begin
    // Do nothing if reaches this instance
  End;

  PROCEDURE TPCElement.Set_AVRmode(const Value: Boolean);
  Begin
    // Do nothing if reaches this instance
  End;


procedure TPCElement.ComputeIterminal(ActorID : Integer);
begin
  IF IterminalSolutionCount[ActorID] <> ActiveCircuit[ActorID].Solution.SolutionCount THEN
  Begin
    GetCurrents(Iterminal, ActorID);
    IterminalSolutionCount[ActorID] := ActiveCircuit[ActorID].Solution.SolutionCount;
  End;

end;

procedure TPCElement.ZeroInjCurrent;
Var i:Integer;
begin
  For i := 1 to Yorder Do InjCurrent^[i] := CZERO ;
end;

procedure TPCElement.set_ITerminalUpdated(const Value: Boolean; ActorID : Integer);
begin
  FITerminalUpdated := Value;
  If Value Then ITerminalSolutionCount[ActorID] :=  ActiveCircuit[ActorID].Solution.SolutionCount;
end;

end.
