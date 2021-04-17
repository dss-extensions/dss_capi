unit DReduceCkt;

interface

function ReduceCktI(mode: Longint; arg: Longint): Longint; CDECL;
function ReduceCktF(mode: Longint; arg: Double): Double; CDECL;
function ReduceCktS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

implementation

uses
    DSSGlobals,
    Executive,
    EnergyMeter,
    ControlElem,
    Variants,
    SysUtils,
    PointerList,
    ReduceAlgs,
    PDElement;

var
    ReduceEditString: String;
    EnergyMeterName: String;
    FirstPDelement: String;  // Full name


function ReduceCktI(mode: Longint; arg: Longint): Longint; CDECL;

begin
    Result := 0;          // Default return value
    case mode of
        0:
        begin  // ReduceCkt.Do1phLaterals
            if EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) then
                ActiveEnergyMeterObj := EnergyMeterClass[ActiveActor].ElementList.Active;
            if Assigned(ActiveEnergyMeterObj) then
                with ActiveEnergyMeterObj do
                begin
                    if not assigned(BranchList) then
                        MakeMeterZoneLists(ActiveActor);
                    DoRemoveAll_1ph_Laterals(BranchList);
                end;
        end;
        1:
        begin  // ReduceCkt.DoBranchRemove
            if Assigned(ActiveCircuit[ActiveActor]) then
            begin
                if EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) then
                    ActiveEnergyMeterObj := EnergyMeterClass[ActiveActor].ElementList.Active;
                if Assigned(ActiveEnergyMeterObj) then
                    with ActiveEnergyMeterObj do
                    begin
                        if not assigned(BranchList) then
                            MakeMeterZoneLists(ActiveActor);
                        with ActiveCircuit[ActiveActor] do
                        begin
                            if SetElementActive(FirstPDelement) >= 0 then // element was found  0-based array
                                DoRemoveBranches(BranchList, ActiveCktElement as TPDElement, ReduceLateralsKeepLoad, ReduceEditString);
                        end;
                    end;
            end;
        end;
        2:
        begin  // ReduceCkt.DoDangling
            if EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) then
                ActiveEnergyMeterObj := EnergyMeterClass[ActiveActor].ElementList.Active;
            if Assigned(ActiveEnergyMeterObj) then
                with ActiveEnergyMeterObj do
                begin
                    if not assigned(BranchList) then
                        MakeMeterZoneLists(ActiveActor);
                    DoReduceDangling(BranchList);
                end;
        end;
        3:
        begin  // ReduceCkt.DoDefault
            if EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) then
                ActiveEnergyMeterObj := EnergyMeterClass[ActiveActor].ElementList.Active;
            if Assigned(ActiveEnergyMeterObj) then
                with ActiveEnergyMeterObj do
                begin
                    if not assigned(BranchList) then
                        MakeMeterZoneLists(ActiveActor);
                    DoReduceDefault(BranchList);
                end;
        end;
        4:
        begin  // ReduceCkt.DoLoopBreak
            if EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) then
                ActiveEnergyMeterObj := EnergyMeterClass[ActiveActor].ElementList.Active;
            if Assigned(ActiveEnergyMeterObj) then
                with ActiveEnergyMeterObj do
                begin
                    if not assigned(BranchList) then
                        MakeMeterZoneLists(ActiveActor);
                    DoBreakLoops(BranchList);
                end;
        end;
        5:
        begin  // ReduceCkt.DoParallelLines
            if EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) then
                ActiveEnergyMeterObj := EnergyMeterClass[ActiveActor].ElementList.Active;
            if Assigned(ActiveEnergyMeterObj) then
                with ActiveEnergyMeterObj do
                begin
                    if not assigned(BranchList) then
                        MakeMeterZoneLists(ActiveActor);
                    DoMergeParallelLines(BranchList);
                end;
        end;
        6:
        begin  // ReduceCkt.DoShortLines
            if EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) then
                ActiveEnergyMeterObj := EnergyMeterClass[ActiveActor].ElementList.Active;
            if Assigned(ActiveEnergyMeterObj) then
                with ActiveEnergyMeterObj do
                begin
                    if not assigned(BranchList) then
                        MakeMeterZoneLists(ActiveActor);
                    DoReduceShortLines(BranchList);
                end;
        end;
        7:
        begin  // ReduceCkt.DoSwitches
            if EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) then
                ActiveEnergyMeterObj := EnergyMeterClass[ActiveActor].ElementList.Active;
            if Assigned(ActiveEnergyMeterObj) then
                with ActiveEnergyMeterObj do
                begin
                    if not assigned(BranchList) then
                        MakeMeterZoneLists(ActiveActor);
                    DoReduceShortLines(BranchList);
                end;
        end;
        8:
        begin  // ReduceCkt.KeepLoad - Read
            if Assigned(ActiveCircuit[ActiveActor]) then
            begin
                if ActiveCircuit[ActiveActor].ReduceLateralsKeepLoad then
                    Result := 1;
            end;
        end;
        9:
        begin  // ReduceCkt.KeepLoad - Write
            if Assigned(ActiveCircuit[ActiveActor]) then
            begin
                if arg <> 0 then
                    ActiveCircuit[ActiveActor].ReduceLateralsKeepLoad := TRUE
                else
                    ActiveCircuit[ActiveActor].ReduceLateralsKeepLoad := FALSE;
            end;
        end

    end;

end;

//***********************Floating poitn type properties***************************

function ReduceCktF(mode: Longint; arg: Double): Double; CDECL;
begin
    Result := 0.0;          // Default return value
    case mode of
        0:
        begin  // ReduceCkt.Zmag - Read
            if Assigned(ActiveCircuit) then
                Result := ActiveCircuit[ActiveActor].ReductionZmag
        end;
        1:
        begin  // ReduceCkt.Zmag - Write
            if Assigned(ActiveCircuit[ActiveActor]) then
                ActiveCircuit[ActiveActor].ReductionZmag := arg;
        end
    end;
end;

//********************String type properties**************************************

function ReduceCktS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
begin
    Result := pAnsiChar(Ansistring(''));  // Default return value
    case mode of
        0:
        begin  // ReduceCkt.EditString - Read
            Result := pAnsiChar(ReduceEditString);
        end;
        1:
        begin  // ReduceCkt.EditString - Write
            ReduceEditString := Widestring(arg);
        end;
        2:
        begin  // ReduceCkt.EnergyMeter - Read
            Result := pAnsiChar(EnergyMeterName);
        end;
        3:
        begin  // ReduceCkt.EnergyMeter - Write
            EnergyMeterName := Widestring(arg);
        end;
        4:
        begin  // ReduceCkt.SaveCircuit
            DSSExecutive[ActiveActor].Command := 'Save Circuit Dir=' + Widestring(arg);
        // Master file name is returned in DSSText.Result
        end;
        5:
        begin  // ReduceCkt.StartPDElement - Read
            Result := pAnsiChar(FirstPDelement);
        end;
        6:
        begin  // ReduceCkt.StartPDElement - Write
            FirstPDelement := Widestring(arg);
        end
    end;

end;

end.
