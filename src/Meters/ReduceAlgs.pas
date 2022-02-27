unit ReduceAlgs;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

// Reduction Algorithms

// Primarily called from EnergyMeter

interface

uses
    CktTree,
    PDElement,
    DSSClass;

procedure DoReduceDefault(DSS: TDSSContext; var BranchList: TCktTree);
procedure DoReduceShortLines(DSS: TDSSContext; var BranchList: TCktTree);
procedure DoReduceDangling(DSS: TDSSContext; var BranchList: TCktTree);
procedure DoBreakLoops(DSS: TDSSContext; var BranchList: TCktTree);
procedure DoMergeParallelLines(DSS: TDSSContext; var BranchList: TCktTree);
procedure DoReduceSwitches(DSS: TDSSContext; var Branchlist: TCktTree);
procedure DoRemoveAll_1ph_Laterals(DSS: TDSSContext; var Branchlist: TCktTree);
procedure DoRemoveBranches(DSS: TDSSContext; var BranchList: TCktTree; FirstPDElement: TPDElement; KeepLoad: Boolean; const EditStr: String);

implementation

uses
    Line,
    Utilities,
    DSSGlobals,
    DSSClassDefs,
    Load,
    UComplex, DSSUcomplex,
    CktElement,
    sysutils,
    ExecHelper,
    Bus,
    DSSHelper;

const
    SERIESMERGE: Boolean = TRUE;
    PARALLELMERGE: Boolean = FALSE;

procedure DoMergeParallelLines(DSS: TDSSContext; var BranchList: TCktTree);
// Merge all lines in this zone that are marked in parallel
var
    LineElement: TLineObj;
begin
    if BranchList <> NIL then
    begin
        BranchList.First;
        LineElement := BranchList.GoForward; // Always keep the first element
        while LineElement <> NIL do
        begin
            if BranchList.PresentBranch.IsParallel then
            begin
                // There will always be two lines in parallel.  The first operation will disable the second
                if LineElement.Enabled then
                    LineElement.MergeWith(TLineObj(BranchList.PresentBranch.LoopLineObj), PARALLELMERGE);  // Guaranteed to be a line
            end;
            LineElement := BranchList.GoForward;
        end;
    end;
end;

procedure DoBreakLoops(DSS: TDSSContext; var BranchList: TCktTree);
// Break loops
var
    LineElement: TLineObj;
begin
    if BranchList <> NIL then
    begin
        BranchList.First;
        LineElement := BranchList.GoForward; // Always keep the first element
        while LineElement <> NIL do
        begin
            if BranchList.PresentBranch.IsLoopedHere then
            begin
                // There will always be two lines in the loop.  The first operation will disable the second
                if LineElement.Enabled then
                    TLineObj(BranchList.PresentBranch.LoopLineObj).Enabled := FALSE; // Disable the other
            end;
            LineElement := BranchList.GoForward;
        end;
    end;
end;

procedure DoReduceDangling(DSS: TDSSContext; var BranchList: TCktTree);
var
    pLineElem1: TDSSCktElement;
    ToBusRef: Integer;
begin
    if BranchList <> NIL then
    begin
        // Let's throw away all dangling end branches
        BranchList.First;
        pLineElem1 := BranchList.GoForward; // Always keep the first element

        while pLineElem1 <> NIL do
        begin
            if IsLineElement(pLineElem1) then
                with  BranchList.PresentBranch do
                begin
                    // If it is at the end of a section and has no load,cap, reactor, or coordinate, just throw it away
                    if IsDangling then
                    begin
                        ToBusRef := ToBusReference;  // only access this property once!
                        if ToBusRef > 0 then
                            with DSS.ActiveCircuit.Buses^[ToBusRef] do
                                if not (Keep) then
                                    pLineElem1.Enabled := FALSE;
                    end;
                end;
            pLineElem1 := BranchList.GoForward;
        end;
    end;
end;

function IsShortLine(DSS: TDSSContext; const Elem: TDSSCktElement): Boolean;
var
    Ztest: Double;
    LineElement: TLineObj;

begin
    LineElement := TLineObj(Elem);
    // Get Positive Sequence or equivalent from matrix
    if LineElement.SymComponentsModel then
        with LineElement do
            Ztest := Cabs(Cmplx(R1, X1)) * Len
    else // Get impedance from Z matrix  Zs - Zm ... approximates Z1
        with LineElement do
        begin
            if NPhases > 1 then
                Ztest := Cabs(Z.Getelement(1, 1) - Z.GetElement(1, 2)) * Len
            else
                Ztest := Cabs(Z.Getelement(1, 1)) * Len;
        end;

    if Ztest <= DSS.ActiveCircuit.ReductionZmag then
        Result := TRUE
    else
        Result := FALSE;
end;

procedure DoReduceShortLines(DSS: TDSSContext; var BranchList: TCktTree);
// Eliminate short lines with impedance < Zmag and merge with lines on either side
var
    LineElement1, LineElement2: TLineObj;
    ShuntElement: TDSSCktElement;
    ParentNode: TCktTreeNode;
    MergeOK: Boolean;

begin
    if BranchList <> NIL then
    begin  // eliminate really short lines
        // First, flag all elements that need to be merged
        LineElement1 := BranchList.First;
        LineElement1 := BranchList.GoForward; // Always keep the first element
        while LineElement1 <> NIL do
        begin
            if IsLineElement(LineElement1) then
            begin
                if IsShortLine(DSS, LineElement1) then // Too small: Mark for merge with something
                    Include(LineElement1.Flags, flg.Flag)
                else
                    Exclude(LineElement1.Flags, flg.Flag);
            end;
            LineElement1 := BranchList.GoForward;  // traverse the whole meter zone  (circuit tree)
        end;

        LineElement1 := BranchList.First;
        LineElement1 := BranchList.GoForward; // Always keep the first element in the Tree
        while LineElement1 <> NIL do
        begin
            if LineElement1.enabled then    // else skip

                if not (Flg.HasControl in LineElement1.Flags) then
                    if not (Flg.IsMonitored in LineElement1.Flags) then   // Skip if controlled element or control is monitoring ,,,

                        if flg.Flag in LineElement1.Flags then  // too short; Try to merge this element out
                        begin
                            with BranchList do
                            begin
                                if (PresentBranch.NumChildBranches = 0) and (PresentBranch.NumShuntObjects = 0) then
                                    LineElement1.Enabled := FALSE     // just discard it
                                else
                                if (PresentBranch.NumChildBranches = 0) then //Merge with Parent and move shunt elements to TO node on parent branch
                                begin
                                    ParentNode := PresentBranch.ParentBranch;
                                    if ParentNode <> NIL then
                                    begin
                                        if ParentNode.NumChildBranches = 1 then   // only works for in-line
                                            if not DSS.ActiveCircuit.Buses^[PresentBranch.ToBusReference].Keep then
                                            begin     // Check Keeplist
                                                // Let's consider merging
                                                // First Check for any Capacitors. Skip if any
                                                MergeOK := TRUE;
                                                if ParentNode.NumShuntObjects > 0 then
                                                begin
                                                    ShuntElement := ParentNode.FirstShuntObject;
                                                    while ShuntElement <> NIL do
                                                    begin
                                                        if ((ShuntElement.DSSObjType and CLASSMASK) = CAP_ELEMENT) or
                                                            ((ShuntElement.DSSObjType and CLASSMASK) = REACTOR_ELEMENT) then
                                                        begin
                                                            MergeOK := FALSE;
                                                            Break;  // outta loop
                                                        end;
                                                        ShuntElement := PresentBranch.NextShuntObject;
                                                    end;
                                                end;

                                                if MergeOK then
                                                begin
                                                    LineElement2 := ParentNode.CktObject;
                                                    if LineElement2.enabled then  // Check to make sure it hasn't been merged out
                                                        if IsLineElement(LineElement2) then
                                                            if LineElement2.MergeWith(LineElement1, SERIESMERGE) then
                                                            begin // Move any loads to ToBus Reference of parent branch
                                                                if ParentNode.NumShuntObjects > 0 then
                                                                begin
                                                                    // Redefine bus connection for PC elements hanging on the bus that is eliminated
                                                                    ShuntElement := ParentNode.FirstShuntObject;
                                                                    while ShuntElement <> NIL do
                                                                    begin
                                                                        DSS.Parser.CmdString := 'bus1="' + DSS.ActiveCircuit.BusList.NameOfIndex(PresentBranch.ToBusReference) + GetNodeString(ShuntElement.GetBus(1)) + '"';
                                                                        ShuntElement.Edit(DSS.Parser);
                                                                        ShuntElement := ParentNode.NextShuntObject;
                                                                    end;
                                                                end;
                                                            end;
                                                end;
                                            end; // IF
                                    end; // IF ParentNode
                                end

                                else

                                if (PresentBranch.NumChildBranches = 1) then // Merge with child
                                begin
                                    if not DSS.ActiveCircuit.Buses^[PresentBranch.ToBusReference].Keep then    // check keeplist
                                    begin
                                        // Let's consider merging
                                        // First Check for any Capacitors. Skip if any
                                        MergeOK := TRUE;
                                        if PresentBranch.NumShuntObjects > 0 then
                                        begin
                                            ShuntElement := PresentBranch.FirstShuntObject;
                                            while ShuntElement <> NIL do
                                            begin
                                                if ((ShuntElement.DSSObjType and CLASSMASK) = CAP_ELEMENT) or
                                                    ((ShuntElement.DSSObjType and CLASSMASK) = REACTOR_ELEMENT) then
                                                begin
                                                    MergeOK := FALSE;
                                                    Break;  // outta loop
                                                end;
                                                ShuntElement := PresentBranch.NextShuntObject;
                                            end;
                                        end;

                                        if MergeOK then
                                        begin
                                            LineElement2 := PresentBranch.FirstChildBranch.CktObject; // child of PresentBranch
                                            if LineElement2.enabled then  // Check to make sure it hasn't been merged out
                                                if IsLineElement(LineElement2) then
                                                    if LineElement2.MergeWith(LineElement1, SERIESMERGE) then
                                                    begin
                                                        if PresentBranch.NumShuntObjects > 0 then
                                                        begin
                                                            // Redefine bus connection to upline bus
                                                            ShuntElement := PresentBranch.FirstShuntObject;
                                                            while ShuntElement <> NIL do
                                                            begin
                                                                DSS.Parser.CmdString := 'bus1="' + DSS.ActiveCircuit.BusList.NameOfIndex(PresentBranch.FromBusReference) + GetNodeString(ShuntElement.GetBus(1)) + '"';
                                                                ShuntElement.Edit(DSS.Parser);
                                                                ShuntElement := PresentBranch.NextShuntObject;
                                                            end; 
                                                        end;
                                                        LineElement1 := BranchList.GoForward; // skip to next branch since we eliminated a bus
                                                    end;
                                        end;
                                    end; // IF not
                                end; // ELSE
                            end;
                        end;
            LineElement1 := BranchList.GoForward;
        end;

        with DSS.ActiveCircuit do
        begin
            ReprocessBusDefs;  // to get new load added and account for disabled devices
            DoResetMeterZones;  // without eliminated devices
            Solution.SystemYChanged := TRUE; // force rebuild of Y
        end;
    end;
end;

procedure DoReduceSwitches(DSS: TDSSContext; var Branchlist: TCktTree);
// Merge switches in with lines or delete if dangling
var
    LineElement1, LineElement2: TLineObj;
begin
    if BranchList <> NIL then
    begin
        LineElement1 := BranchList.First;
        LineElement1 := BranchList.GoForward; // Always keep the first element
        while LineElement1 <> NIL do
        begin
            if LineElement1.Enabled then   // maybe we threw it away already
                if IsLineElement(LineElement1) then
                    if LineElement1.IsSwitch then
                        with BranchList.PresentBranch do
                            // see if eligible for merging
                            case NumChildBranches of
                                0: // Throw away if dangling
                                    if NumShuntObjects = 0 then
                                        LineElement1.Enabled := FALSE;

                                1:
                                    if NumShuntObjects = 0 then
                                        if not DSS.ActiveCircuit.Buses^[ToBusReference].Keep then
                                        begin
                                            // Let's consider merging
                                            LineElement2 := FirstChildBranch.CktObject;
                                            if IsLineElement(LineElement2) then
                                                if not LineElement2.IsSwitch then
                                                    LineElement2.MergeWith(LineElement1, SERIESMERGE){Series Merge}
                                        end;
                            else //Nada
                            end;

            LineElement1 := BranchList.GoForward;
        end;
    end;
end;

procedure DoReduceDefault(DSS: TDSSContext; var BranchList: TCktTree);

var
    LineElement1, LineElement2: TLineObj;
begin
    if BranchList <> NIL then
    begin
        // Now merge remaining lines
        LineElement1 := BranchList.First;
        LineElement1 := BranchList.GoForward; // Always keep the first element
        while LineElement1 <> NIL do
        begin
            if IsLineElement(LineElement1) then
                if not LineElement1.IsSwitch then         // Exceptions
                    if not (Flg.HasControl in LineElement1.Flags) then
                        if not (Flg.IsMonitored in LineElement1.Flags) then
                            if LineElement1.Enabled then   // maybe we threw it away already
                                with BranchList do
                                begin
                                    // see if eligible for merging
                                    if PresentBranch.NumChildBranches = 1 then
                                        if PresentBranch.NumShuntObjects = 0 then
                                            if not DSS.ActiveCircuit.Buses^[PresentBranch.ToBusReference].Keep then
                                            begin
                                                // Let's consider merging
                                                LineElement2 := PresentBranch.FirstChildBranch.CktObject;

                                                if IsLineElement(LineElement2) then
                                                    if not LineElement2.IsSwitch then
                                                        LineElement2.MergeWith(LineElement1, SERIESMERGE){Series Merge}
                                            end;

                                end;

            LineElement1 := BranchList.GoForward;
        end;
    end;
end;

procedure DoRemoveBranches(DSS: TDSSContext; var BranchList: TCktTree; FirstPDElement: TPDElement; KeepLoad: Boolean; const EditStr: String);
var
    PDElem: TPDElement;
    BusName: String;
    TotalkVA: Complex;
  // pLoad : TLoadObj;
    NewLoadName: String;
    pShunt: TDSSCktElement;
    LoadBus: TDSSBus;
    LoadBasekV: Double;
    StartLevel: Integer;
begin
    // Position BranchList at "FirstPDElement"
    PDElem := BranchList.First;
    while (PDElem <> FirstPDElement) and (PDElem <> NIL) do
        PDElem := BranchList.GoForward;

    StartLevel := BranchList.level;

    if PDElem = NIL then
    begin
        DoSimpleMsg(DSS, '%s not found (Remove Command).', [FirstPDElement.FullName], 5432100);
    end
    else
    begin
        // If KeepLoad, create a new Load object at upstream bus (from bus).
        if KeepLoad then
            with BranchList.PresentBranch do
            begin
                BusName := FirstPDElement.GetBus(FromTerminal);
                TotalkVA := PDelem.Power[FromTerminal] / 1000.0;
                NewLoadName := Format('Eq_%s_%s', [FirstPDElement.Name, StripExtension(BusName)]);
                // Pick up the kV Base for the From bus
                LoadBus := DSS.ActiveCircuit.Buses^[FromBusReference];
                if Loadbus.kVBase > 0.0 then
                    LoadBasekV := LoadBus.kVBase
                else
                begin    // Try to guess from the present voltage at the first node on the bus
                    DSS.ActiveCircuit.Solution.UpdateVBus;
                    LoadBasekV := Cabs(Loadbus.Vbus^[1]) * 0.001;
                end;
                if FirstPDElement.NPhases > 1 then
                    LoadBasekV := LoadBasekV * Sqrt3;
                // Load up DSS.Parser with definition of equivalent load
                DSS.Parser.CmdString := Format(' phases=%d Bus1=%s kW=%g kvar=%g kV=%g %s', [FirstPDElement.NPhases, Busname, TotalkVA.re, TotalkVA.im, LoadBasekV, EditStr]);
                DSS.DSSExecutive.AddObject('load', NewLoadName); // Add new load to circuit
            end;

        // Disable all elements in the tree downline from the start element
        while PDElem <> NIL do
        begin
            with BranchList do
            begin
                pShunt := PresentBranch.FirstShuntObject;
                while pShunt <> NIL do
                begin
                    pShunt.Enabled := FALSE;
                    pShunt := PresentBranch.NextShuntObject;
                end;
            end;

            PDElem.Enabled := FALSE;
            PDElem := BranchList.GoForward;

         // Check to see if we are back where we started. If so, stop.
            if BranchList.Level <= StartLevel then
                PDElem := NIL;
        end;
    end;

    with DSS.ActiveCircuit do
    begin
        ReprocessBusDefs;  // to get new load added and account for disabled devices
        DoResetMeterZones;  // without eliminated devices
        Solution.SystemYChanged := TRUE; // force rebuild of Y
    end;
end;

procedure DoRemoveAll_1ph_Laterals(DSS: TDSSContext; var Branchlist: TCktTree);
// Remove all 1-phase laterals in Branchlist and lump total load back to main feeder
//  This removes all elements on all 1ph laterals and moves the net load back to the main feeder tap point does not
var
    PDelem: TPDElement;
    BusName: String;
    pShunt: TDSSCktElement;
    HeadBus: TDSSBus;
    HeadBasekV: Double = 1.0;
    StartLevel: Integer;
    pBus: TDSSBus;
    // strNodes: String;
begin
    // Just march down the feeder until we encounter a 1-phase PD element
    // Position BranchList at "beginning"
    PDElem := BranchList.First;

    while PDElem <> NIL do
    begin
        if PDElem.nphases = 1 then   // ELIMINATE THIS LATERAL
        begin
            // Check to see if this is a 1-phase switch or other branch in the middle of a 3-phase branch and go on
            // If the To bus has more than 1 phase, keep this branch else lump the load at the From node
            pBus := DSS.ActiveCircuit.Buses^[Branchlist.PresentBranch.ToBusReference];  //To Bus

            if pBus.NumNodesThisBus = 1 then // Eliminate the lateral starting with this branch
            begin
                // If KeepLoad (ReduceLateralsKeepLoad), create a new Load object at upstream bus (from bus).

                if DSS.ActiveCircuit.ReduceLateralsKeepLoad then
                    with BranchList do
                    begin
                        BusName := PDElem.GetBus(PresentBranch.FromTerminal);
                        // Make sure there is a node reference .. default to 1
                        if Pos('.', BusName) = 0 then
                            BusName := BusName + '.1';

                        // Pick up the kV Base for the From bus
                        HeadBus := DSS.ActiveCircuit.Buses^[PresentBranch.FromBusReference];
                        if (HeadBus.kVBase > 0.0) then
                            HeadBasekV := HeadBus.kVBase  // Use defined voltage base
                        else
                        begin    // Try to guess voltage base from the present voltage at the first node on the bus
                            DSS.ActiveCircuit.Solution.UpdateVBus;
                            HeadBasekV := Cabs(HeadBus.Vbus^[1]) * 0.001;
                        end;
                    end;

                // Disable all PDelements in the tree downline from the beginning of the 1-phase lateral
                // Move 1-phase shunts to Headbus

                StartLevel := BranchList.level;   // record level of first 1-phase branch in this lateral
                while PDElem <> NIL do
                begin
                    // Get rid of loads and other shunt elements connected to this branch
                    with BranchList do
                    begin
                        pShunt := PresentBranch.FirstShuntObject;
                        while pShunt <> NIL do
                        begin
                            DSS.Parser.CmdString := Format('Bus1=%s kV=%.6g ', [Busname, HeadBasekV]);
                            pShunt.Edit(DSS.Parser);
                            pShunt := PresentBranch.NextShuntObject;
                        end;
                    end;

                    PDElem.Enabled := FALSE;
                    PDElem := BranchList.GoForward;

                    // Check to see if we are back where we started. If so, stop with this lateral and get on to the next.
                    if PDElem <> NIL then
                        if (BranchList.Level <= StartLevel) then
                            Break;
                end;

            end
            else
                PDElem := BranchList.GoForward;


        end
        else
            PDElem := BranchList.GoForward;

    end;

    with DSS.ActiveCircuit do
    begin
        ReprocessBusDefs;  // to get new load added and account for disabled devices
        DoResetMeterZones;  // without eliminated devices
        Solution.SystemYChanged := TRUE; // force rebuild of Y
    end;
end;

end.
