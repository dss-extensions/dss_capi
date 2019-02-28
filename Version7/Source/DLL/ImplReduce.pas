unit ImplReduce;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TReduce = class(TAutoObject, IReduce)
  protected
    function Get_Zmag: Double; safecall;
    procedure Set_Zmag(Value: Double); safecall;
    function Get_KeepLoad: WordBool; safecall;
    procedure Set_KeepLoad(Value: WordBool); safecall;
    function Get_EditString: WideString; safecall;
    procedure Set_EditString(const Value: WideString); safecall;
    function Get_StartPDElement: WideString; safecall;
    procedure Set_StartPDElement(const Value: WideString); safecall;
    function Get_EnergyMeter: WideString; safecall;
    procedure SaveCircuit(const CktName: WideString); safecall;
    procedure Set_EnergyMeter(const Value: WideString); safecall;
    procedure DoDefault; safecall;
    procedure DoShortLines; safecall;
    procedure Do1phLaterals; safecall;
    procedure DoBranchRemove; safecall;
    procedure DoDangling; safecall;
    procedure DoLoopBreak; safecall;
    procedure DoParallelLines; safecall;
    procedure DoSwitches; safecall;

  end;

implementation

uses Circuit, DSSGlobals, ComServ, Executive, EnergyMeter, ReduceAlgs, PDElement;

Var  ReduceEditString : String;
     EnergyMeterName  : String;
     FirstPDelement   : String;  // Full name

function TReduce.Get_Zmag: Double;
begin
     if Assigned(ActiveCircuit) then
        Result := ActiveCircuit.ReductionZmag
end;

procedure TReduce.Set_Zmag(Value: Double);
begin
     if Assigned(ActiveCircuit) then
        ActiveCircuit.ReductionZmag := Value;
end;

function TReduce.Get_KeepLoad: WordBool;
begin
     if Assigned(ActiveCircuit) then
        Result := ActiveCircuit.ReduceLateralsKeepLoad;
end;

procedure TReduce.Set_KeepLoad(Value: WordBool);
begin
     if Assigned(ActiveCircuit) then
        ActiveCircuit.ReduceLateralsKeepLoad := Value;
end;

function TReduce.Get_EditString: WideString;
begin
     Result := ReduceEditString;
end;

procedure TReduce.Set_EditString(const Value: WideString);
begin
     ReduceEditString := Value;
end;

function TReduce.Get_StartPDElement: WideString;
begin
     Result := FirstPDelement;
end;

procedure TReduce.Set_StartPDElement(const Value: WideString);
begin
     FirstPDelement := Value;
end;

function TReduce.Get_EnergyMeter: WideString;
begin
    Result := EnergyMeterName;
end;

procedure TReduce.SaveCircuit(const CktName: WideString);
begin
      DSSExecutive.Command := 'Save Circuit Dir=' + CktName;
end;

procedure TReduce.Set_EnergyMeter(const Value: WideString);
begin
      EnergyMeterName := Value;
end;

procedure TReduce.DoDefault;
begin
       If EnergyMeterClass.SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass.ElementList.Active;
       if Assigned(ActiveEnergyMeterObj) then
       With ActiveEnergyMeterObj Do   Begin
       If not assigned(BranchList) Then MakeMeterZoneLists;
           DoReduceDefault(BranchList );
       End;
end;

procedure TReduce.DoShortLines;
begin
       If EnergyMeterClass.SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass.ElementList.Active;
       if Assigned(ActiveEnergyMeterObj) then
       With ActiveEnergyMeterObj Do   Begin
           If not assigned(BranchList) Then MakeMeterZoneLists;
           DoReduceShortLines(BranchList );
       End;
end;

procedure TReduce.Do1phLaterals;
begin
       If EnergyMeterClass.SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass.ElementList.Active;
       if Assigned(ActiveEnergyMeterObj) then
       With ActiveEnergyMeterObj Do   Begin
           If not assigned(BranchList) Then MakeMeterZoneLists;
           DoRemoveAll_1ph_Laterals(BranchList );
       End;
end;

procedure TReduce.DoBranchRemove;
begin
     if Assigned(ActiveCircuit) then Begin
       If EnergyMeterClass.SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass.ElementList.Active;
       if Assigned(ActiveEnergyMeterObj) then
       With ActiveEnergyMeterObj Do   Begin
           If not assigned(BranchList) Then MakeMeterZoneLists;
           With ActiveCircuit Do Begin
               If SetElementActive(FirstPDelement)>= 0 Then // element was found  0-based array
               DoRemoveBranches(BranchList, ActiveCktElement as TPDElement, ReduceLateralsKeepLoad, ReduceEditString);
           End;
       End;
     End;
end;

procedure TReduce.DoDangling;
begin
       If EnergyMeterClass.SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass.ElementList.Active;
       if Assigned(ActiveEnergyMeterObj) then
       With ActiveEnergyMeterObj Do   Begin
           If not assigned(BranchList) Then MakeMeterZoneLists;
           DoReduceDangling(BranchList );
       End;
end;

procedure TReduce.DoLoopBreak;
begin
       If EnergyMeterClass.SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass.ElementList.Active;
       if Assigned(ActiveEnergyMeterObj) then
       With ActiveEnergyMeterObj Do   Begin
           If not assigned(BranchList) Then MakeMeterZoneLists;
           DoBreakLoops(BranchList );
       End;
end;

procedure TReduce.DoParallelLines;
begin
       If EnergyMeterClass.SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass.ElementList.Active;
       if Assigned(ActiveEnergyMeterObj) then
       With ActiveEnergyMeterObj Do   Begin
           If not assigned(BranchList) Then MakeMeterZoneLists;
           DoMergeParallelLines(BranchList );
       End;
end;

procedure TReduce.DoSwitches;
begin
       If EnergyMeterClass.SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass.ElementList.Active;
       if Assigned(ActiveEnergyMeterObj) then
       With ActiveEnergyMeterObj Do   Begin
           If not assigned(BranchList) Then MakeMeterZoneLists;
           DoRemoveAll_1ph_Laterals(BranchList );
       End;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TReduce, Class_Reduce,
    ciInternal, tmApartment);

  ReduceEditString := ''; // Init to null string
  EnergyMeterName  := '';
  FirstPDelement := '';
end.
