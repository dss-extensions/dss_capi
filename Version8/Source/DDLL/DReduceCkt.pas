unit DReduceCkt;

interface

function ReduceCktI(mode: longint; arg: longint):longint ;cdecl;
function ReduceCktF(mode: longint; arg: double):double ;cdecl;
function ReduceCktS(mode: longint; arg: pAnsiChar):pAnsiChar ;cdecl;

implementation

uses DSSGlobals, Executive, EnergyMeter,ControlElem, Variants, SysUtils, PointerList, ReduceAlgs, PDElement;

Var  ReduceEditString : String;
     EnergyMeterName  : String;
     FirstPDelement   : String;  // Full name


function ReduceCktI(mode: longint; arg: longint):longint ;cdecl;

begin
  Result:=0;          // Default return value
  case mode of
  0: begin  // ReduceCkt.Do1phLaterals
       If EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass[ActiveActor].ElementList.Active;
       if Assigned(ActiveEnergyMeterObj) then
         With ActiveEnergyMeterObj Do
         Begin
             If not assigned(BranchList) Then MakeMeterZoneLists(ActiveActor);
             DoRemoveAll_1ph_Laterals(BranchList );
         End;
     end;
  1: begin  // ReduceCkt.DoBranchRemove
       if Assigned(ActiveCircuit[ActiveActor]) then Begin
         If EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass[ActiveActor].ElementList.Active;
         if Assigned(ActiveEnergyMeterObj) then
         With ActiveEnergyMeterObj Do   Begin
             If not assigned(BranchList) Then MakeMeterZoneLists(ActiveActor);
             With ActiveCircuit[ActiveActor] Do Begin
                 If SetElementActive(FirstPDelement)>= 0 Then // element was found  0-based array
                 DoRemoveBranches(BranchList, ActiveCktElement as TPDElement, ReduceLateralsKeepLoad, ReduceEditString);
             End;
         End;
       End;
     end;
  2: begin  // ReduceCkt.DoDangling
       If EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass[ActiveActor].ElementList.Active;
       if Assigned(ActiveEnergyMeterObj) then
       With ActiveEnergyMeterObj Do   Begin
           If not assigned(BranchList) Then MakeMeterZoneLists(ActiveActor);
           DoReduceDangling(BranchList);
       End;
     end;
  3: begin  // ReduceCkt.DoDefault
       If EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass[ActiveActor].ElementList.Active;
       if Assigned(ActiveEnergyMeterObj) then
         With ActiveEnergyMeterObj Do
         Begin
           If not assigned(BranchList) Then MakeMeterZoneLists(ActiveActor);
           DoReduceDefault(BranchList );
         End;
     end;
  4: begin  // ReduceCkt.DoLoopBreak
       If EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass[ActiveActor].ElementList.Active;
       if Assigned(ActiveEnergyMeterObj) then
         With ActiveEnergyMeterObj Do
         Begin
           If not assigned(BranchList) Then MakeMeterZoneLists(ActiveActor);
           DoBreakLoops(BranchList );
         End;
     end;
  5: begin  // ReduceCkt.DoParallelLines
       If EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass[ActiveActor].ElementList.Active;
       if Assigned(ActiveEnergyMeterObj) then
       With ActiveEnergyMeterObj Do   Begin
           If not assigned(BranchList) Then MakeMeterZoneLists(ActiveActor);
           DoMergeParallelLines(BranchList );
       End;
     end;
  6: begin  // ReduceCkt.DoShortLines
       If EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass[ActiveActor].ElementList.Active;
       if Assigned(ActiveEnergyMeterObj) then
       With ActiveEnergyMeterObj Do   Begin
           If not assigned(BranchList) Then MakeMeterZoneLists(ActiveActor);
           DoReduceShortLines(BranchList );
       End;
     end;
  7: begin  // ReduceCkt.DoSwitches
       If EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) Then ActiveEnergyMeterObj:= EnergyMeterClass[ActiveActor].ElementList.Active;
       if Assigned(ActiveEnergyMeterObj) then
       With ActiveEnergyMeterObj Do   Begin
           If not assigned(BranchList) Then MakeMeterZoneLists(ActiveActor);
           DoReduceShortLines(BranchList );
       End;
     end;
  8: begin  // ReduceCkt.KeepLoad - Read
       if Assigned(ActiveCircuit[ActiveActor]) then
       begin
        if ActiveCircuit[ActiveActor].ReduceLateralsKeepLoad then Result  := 1;
       end;
     end;
  9: begin  // ReduceCkt.KeepLoad - Write
       if Assigned(ActiveCircuit[ActiveActor]) then
       begin
          if arg <> 0 then
            ActiveCircuit[ActiveActor].ReduceLateralsKeepLoad := true
          else
            ActiveCircuit[ActiveActor].ReduceLateralsKeepLoad := false;
       end;
     end

  end;

end;

//***********************Floating poitn type properties***************************

function ReduceCktF(mode: longint; arg: double):double ;cdecl;
begin
  Result:=0.0;          // Default return value
  case mode of
  0: begin  // ReduceCkt.Zmag - Read
       if Assigned(ActiveCircuit) then
          Result := ActiveCircuit[ActiveActor].ReductionZmag
     end;
  1: begin  // ReduceCkt.Zmag - Write
       if Assigned(ActiveCircuit[ActiveActor]) then
          ActiveCircuit[ActiveActor].ReductionZmag := arg;
     end
  end;
end;

//********************String type properties**************************************

function ReduceCktS(mode: longint; arg: pAnsiChar):pAnsiChar ;cdecl;
begin
  Result:= pAnsiChar(AnsiString(''));  // Default return value
  case mode of
  0: begin  // ReduceCkt.EditString - Read
        Result := pAnsiChar(AnsiString(ReduceEditString));
     end;
  1: begin  // ReduceCkt.EditString - Write
        ReduceEditString := string(arg);
     end;
  2: begin  // ReduceCkt.EnergyMeter - Read
        Result := pAnsiChar(AnsiString(EnergyMeterName));
     end;
  3: begin  // ReduceCkt.EnergyMeter - Write
        EnergyMeterName := string(arg);
     end;
  4: begin  // ReduceCkt.SaveCircuit
        DSSExecutive[ActiveActor].Command := 'Save Circuit Dir=' + arg;
        // Master file name is returned in DSSText.Result
     end;
  5: begin  // ReduceCkt.StartPDElement - Read
        Result := pAnsiChar(AnsiString(FirstPDelement));
     end;
  6: begin  // ReduceCkt.StartPDElement - Write
        FirstPDelement := string(arg);
     end
  end;

end;

end.
