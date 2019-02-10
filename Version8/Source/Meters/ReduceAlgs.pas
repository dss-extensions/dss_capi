unit ReduceAlgs;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Reduction Algorithms}

{Primarily called from EnergyMeter}

interface

uses
    CktTree,
    PDELement;

procedure DoReduceDefault(var BranchList: TCktTree);
procedure DoReduceStubs(var BranchList: TCktTree);
procedure DoReduceDangling(var BranchList: TCktTree);
{  procedure DoReduceTapEnds(var BranchList:TCktTree);}
procedure DoBreakLoops(var BranchList: TCktTree);
procedure DoMergeParallelLines(var BranchList: TCktTree);
procedure DoReduceSwitches(var Branchlist: TCktTree);
procedure DoReduceLaterals(var Branchlist: TCktTree);

procedure DoRemoveBranches(var BranchList: TCktTree; FirstPDElement: TPDElement; KeepLoad: Boolean; const EditStr: String);


implementation

uses
    Line,
    Utilities,
    DSSGlobals,
    Load,
    uComplex,
    ParserDel,
    CktElement,
    sysutils,
    ExecHelper,
    Bus;

procedure DoMergeParallelLines(var BranchList: TCktTree);
{Merge all lines in this zone that are marked in parallel}

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
               {There will always be two lines in parallel.  The first operation will disable the second}
                if LineElement.Enabled then
                    LineElement.MergeWith(TLineObj(BranchList.PresentBranch.LoopLineObj), FALSE);  // Guaranteed to be a line
            end;
            LineElement := BranchList.GoForward;
        end;
    end;
end;

procedure DoBreakLoops(var BranchList: TCktTree);

{Break loops}
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
               {There will always be two lines in the loop.  The first operation will disable the second}
                if LineElement.Enabled then
                    TLineObj(BranchList.PresentBranch.LoopLineObj).Enabled := FALSE; // Disable the other
            end;
            LineElement := BranchList.GoForward;
        end;
    end;
end;


procedure DoReduceTapEnds(var BranchList: TCktTree);
(*Var
   pLineElem1, pLineElem2:TLineObj;
   ToBusRef:Integer;
   AngleTest:Double;
   ParentNode:TCktTreeNode;
*)
begin

end;


procedure DoReduceDangling(var BranchList: TCktTree);
var
    pLineElem1: TDSSCktElement;
    ToBusRef: Integer;
begin
    if BranchList <> NIL then
    begin
     {Let's throw away all dangling end branches}
        BranchList.First;
        pLineElem1 := BranchList.GoForward; // Always keep the first element

        while pLineElem1 <> NIL do
        begin

            if IsLineElement(pLineElem1) then
                with  BranchList.PresentBranch do
                begin

             {If it is at the end of a section and has no load,cap, reactor,
             or coordinate, just throw it away}
                    if IsDangling then
                    begin
                        ToBusRef := ToBusReference;  // only access this property once!
                        if ToBusRef > 0 then
                            with ActiveCircuit[ActiveActor].Buses^[ToBusRef] do
                                if not (Keep) then
                                    pLineElem1.Enabled := FALSE;
                    end; {IF}
                end;  {If-With}
            pLineElem1 := BranchList.GoForward;
        end;
    end;

end;


procedure DoReduceStubs(var BranchList: TCktTree);
{Eliminate short stubs and merge with lines on either side}
var
    LineElement1, LineElement2: TLineObj;
    LoadElement: TLoadObj;
    ParentNode: TCktTreeNode;

begin
    if BranchList <> NIL then
    begin  {eliminate really short lines}
      {First, flag all elements that need to be merged}
        LineElement1 := BranchList.First;
        LineElement1 := BranchList.GoForward; // Always keep the first element
        while LineElement1 <> NIL do
        begin
            if IsLineElement(LineElement1) then
            begin
                if IsStubLine(LineElement1) then
                    LineElement1.Flag := TRUE   {Too small: Mark for merge with something}
                else
                    LineElement1.Flag := FALSE;
            end; {IF}
            LineElement1 := BranchList.GoForward;
        end; {WHILE}

        LineElement1 := BranchList.First;
        LineElement1 := BranchList.GoForward; // Always keep the first element
        while LineElement1 <> NIL do
        begin
            if LineElement1.Flag then  // Merge this element out
            begin
                with BranchList.PresentBranch do
                begin
                    if (NumChildBranches = 0) and (NumShuntObjects = 0) then
                        LineElement1.Enabled := FALSE     // just discard it
                    else
                    if (NumChildBranches = 0) or (NumChildBranches > 1) then
                 {Merge with Parent and move loads on parent to To node}
                    begin
                        ParentNode := ParentBranch;
                        if ParentNode <> NIL then
                        begin
                            if ParentNode.NumChildBranches = 1 then   // only works for in-line
                                if not ActiveCircuit[ActiveActor].Buses^[ParentNode.ToBusReference].Keep then
                                begin
                             {Let's consider merging}
                                    LineElement2 := ParentNode.CktObject;
                                    if LineElement2.enabled then  // Check to make sure it hasn't been merged out
                                        if IsLineElement(LineElement2) then
                                            if LineElement2.MergeWith(LineElement1, TRUE) then {Move any loads to ToBus Reference of downline branch}
                                                if ParentNode.NumShuntObjects > 0 then
                                                begin
                                   {Redefine bus connection for PC elements hanging on the bus that is eliminated}
                                                    LoadElement := ParentNode.FirstShuntObject;
                                                    while LoadElement <> NIL do
                                                    begin
                                                        Parser[ActiveActor].CmdString := 'bus1="' + ActiveCircuit[ActiveActor].BusList.Get(ToBusReference) + '"';
                                                        LoadElement.Edit(ActiveActor);
                                                        LoadElement := ParentNode.NextShuntObject;
                                                    end;  {While}
                                                end; {IF}
                                end; {IF}
                        end; {IF ParentNode}
                    end
                    else
                    begin{Merge with child}
                        if not ActiveCircuit[ActiveActor].Buses^[ToBusReference].Keep then
                        begin
                       {Let's consider merging}
                            LineElement2 := FirstChildBranch.CktObject;
                            if IsLineElement(LineElement2) then
                                if LineElement2.MergeWith(LineElement1, TRUE) then
                                    if FirstChildBranch.NumShuntObjects > 0 then
                                    begin
                               {Redefine bus connection to upline bus}
                                        LoadElement := FirstChildBranch.FirstShuntObject;
                                        while LoadElement <> NIL do
                                        begin
                                            Parser[ActiveActor].CmdString := 'bus1="' + ActiveCircuit[ActiveActor].BusList.Get(FromBusReference) + '"';
                                            LoadElement.Edit(ActiveActor);
                                            LoadElement := FirstChildBranch.NextShuntObject;
                                        end;  {While}
                                    end; {IF}
                        end; {IF not}
                    end; {ELSE}
                end;
            end;
            LineElement1 := BranchList.GoForward;
        end;

    end;
end;

procedure DoReduceSwitches(var Branchlist: TCktTree);

{Merge switches in with lines or delete if dangling}

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
             {see if eligble for merging}
                            case NumChildBranches of
                                0: {Throw away if dangling}
                                    if NumShuntObjects = 0 then
                                        LineElement1.Enabled := FALSE;

                                1:
                                    if NumShuntObjects = 0 then
                                        if not ActiveCircuit[ActiveActor].Buses^[ToBusReference].Keep then
                                        begin
                     {Let's consider merging}
                                            LineElement2 := FirstChildBranch.CktObject;
                                            if IsLineElement(LineElement2) then
                                                if not LineElement2.IsSwitch then
                                                    LineElement2.MergeWith(LineElement1, TRUE){Series Merge}
                                        end;
                            else {Nada}
                            end;

            LineElement1 := BranchList.GoForward;
        end;
    end;

end;

procedure DoReduceDefault(var BranchList: TCktTree);

var
    LineElement1, LineElement2: TLineObj;
begin
    if BranchList <> NIL then
    begin

     {Now merge remaining lines}

        LineElement1 := BranchList.First;
        LineElement1 := BranchList.GoForward; // Always keep the first element
        while LineElement1 <> NIL do
        begin

            if IsLineElement(LineElement1) then
                if not LineElement1.IsSwitch then
                    if LineElement1.Enabled then   // maybe we threw it away already
                        with BranchList.PresentBranch do
                        begin
                 {see if eligble for merging}
                            if NumChildBranches = 1 then
                                if NumShuntObjects = 0 then
                                    if not ActiveCircuit[ActiveActor].Buses^[ToBusReference].Keep then
                                    begin
                     {Let's consider merging}
                                        LineElement2 := FirstChildBranch.CktObject;

                                        if IsLineElement(LineElement2) then
                                            if not LineElement2.IsSwitch then
                                                LineElement2.MergeWith(LineElement1, TRUE){Series Merge}
                                    end;

                        end;

            LineElement1 := BranchList.GoForward;
        end;
    end;

end;

procedure DoRemoveBranches(var BranchList: TCktTree; FirstPDElement: TPDElement; KeepLoad: Boolean; const EditStr: String);
var
    PDElem: TPDElement;
    BusName: String;
    TotalkVA: Complex;
    pLoad: TLoadObj;
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
        DoSimpleMsg(Format('%s.%s Not Found (Remove Command).', [FirstPDElement.ParentClass.Name, FirstPDElement.Name]), 5432100);
    end
    else
    begin

     { If KeepLoad, create a new Load object at upstream bus (from bus).}
        if KeepLoad then
            with BranchList.PresentBranch do
            begin
                BusName := FirstPDElement.GetBus(FromTerminal);
                TotalkVA := CDivreal(PDelem.Power[FromTerminal, ActiveActor], 1000.0);
                NewLoadName := Format('Eq_%s_%s', [FirstPDElement.Name, StripExtension(BusName)]);
       {Pick up the kV Base for the From bus}
                LoadBus := ActiveCircuit[ActiveActor].Buses^[FromBusReference];
                if Loadbus.kVBase > 0.0 then
                    LoadBasekV := LoadBus.kVBase
                else
                begin    // Try to guess from the present voltage at the first node on the bus
                    ActiveCircuit[ActiveActor].Solution.UpdateVBus(ActiveActor);
                    LoadBasekV := Cabs(Loadbus.Vbus^[1]) * 0.001;
                end;
                if FirstPDElement.NPhases > 1 then
                    LoadBasekV := LoadBasekV * Sqrt3;
       {Load up parser with definition of equivalent load}
                Parser[ActiveActor].CmdString := Format(' phases=%d Bus1=%s kW=%g kvar=%g kV=%g %s', [FirstPDElement.NPhases, Busname, TotalkVA.re, TotalkVA.im, LoadBasekV, EditStr]);
                AddObject('load', NewLoadName); // Add new load to circuit
            end;

     {Disable all elements in the tree downline from the start element}

   //  {****} WriteDLLDebugFile(Format('StartLevel = %d ',[ StartLevel]));

        while PDElem <> NIL do
        begin
   // {****} WriteDLLDebugFile(Format('Disabling cktelement %d %s.%s',[ BranchList.Level, PDelem.ParentClass.Name, PDElem.Name ]));
            with BranchList.PresentBranch do
            begin

                pShunt := BranchList.PresentBranch.FirstShuntObject;
                while pShunt <> NIL do
                begin
                    pShunt.Enabled := FALSE;
   //  {****} WriteDLLDebugFile(Format('Disabling shunt element %s.%s',[ pShunt.ParentClass.Name, pShunt.Name ]));
                    pShunt := BranchList.PresentBranch.NextShuntObject;
                end;

            end;

            PDElem.Enabled := FALSE;
            PDElem := BranchList.GoForward;

         // Check to see if we are back where we started. If so, stop.
            if BranchList.Level <= StartLevel then
                PDElem := NIL;

        end;

    end;

    with ActiveCircuit[ActiveActor] do
    begin
        ReprocessBusDefs(ActiveActor);  // to get new load added and account for disabled devices
        DoResetMeterZones(ActiveActor);  // without eliminated devices
        Solution.SystemYChanged := TRUE; // force rebuild of Y
    end;
end;

procedure DoReduceLaterals(var Branchlist: TCktTree);
{Remove all 1-phase laterals in Branchlist and lump load back to main feeder}

var
    PDelem: TPDElement;
    BusName: String;
    TotalkVA: Complex;
    pLoad: TLoadObj;
    NewLoadName: String;
    pShunt: TDSSCktElement;
    LoadBus: TDSSBus;
    LoadBasekV: Double;
    StartLevel: Integer;
    pBus: TDSSBus;

begin
 {
  Just march down the feeder until we encounter a 1-phase PD element
 }
   // Position BranchList at "beginning"
    PDElem := BranchList.First;

    while PDElem <> NIL do
    begin

        if PDElem.nphases = 1 then   // ELIMINATE THIS LATERAL
        begin
        {Check to see if this is a 1-phase switch or other branch in the middle of a 3-phase branch and go on}
        {If the To bus has more than 1 phase, keep this branch else lump the load at the From node}
            pBus := ActiveCircuit[ActiveActor].Buses^[Branchlist.PresentBranch.ToBusReference];  //To Bus

            if pBus.NumNodesThisBus = 1 then // Eliminate the lateral starting with this branch
            begin

             { If KeepLoad (ReduceLateralsKeepLoad), create a new Load object at upstream bus (from bus).}

                if ActiveCircuit[ActiveActor].ReduceLateralsKeepLoad then
                    with BranchList.PresentBranch do
                    begin
                        BusName := PDElem.GetBus(FromTerminal);
                        TotalkVA := CDivreal(PDelem.Power[FromTerminal, ActiveActor], 1000.0);
                        NewLoadName := Format('Lateral_%s', [PDElem.Name]);
                 {Pick up the kV Base for the From bus}
                        LoadBus := ActiveCircuit[ActiveActor].Buses^[FromBusReference];
                        if (Loadbus.kVBase > 0.0) then
                            LoadBasekV := LoadBus.kVBase  // Use defined voltage base
                        else
                        begin    // Try to guess voltage base from the present voltage at the first node on the bus
                            ActiveCircuit[ActiveActor].Solution.UpdateVBus(ActiveActor);
                            LoadBasekV := Cabs(Loadbus.Vbus^[1]) * 0.001;
                        end;
                 {Load up parser with definition of equivalent load and create a new Load object on the main feeder
                  with the same bus definition as the From Bus of the 1-phase line}
                        Parser[ActiveActor].CmdString := Format(' phases=%d Bus1=%s kW=%g kvar=%g kV=%g', [PDElem.NPhases, Busname, TotalkVA.re, TotalkVA.im, LoadBasekV]);
                        AddObject('load', NewLoadName); // Add new load to circuit
                    end;

             {

               Disable all elements in the tree downline from the beginning of the 1-phase lateral

             }

                StartLevel := BranchList.level;   // record level of first 1-phase branch in this lateral
                while PDElem <> NIL do
                begin
           // {****} WriteDLLDebugFile(Format('Disabling cktelement %d %s.%s',[ BranchList.Level, PDelem.ParentClass.Name, PDElem.Name ]));
                 { Get rid of loads and other shunt elements connected to this branch }
                    with BranchList.PresentBranch do
                    begin

                        pShunt := BranchList.PresentBranch.FirstShuntObject;
                        while pShunt <> NIL do
                        begin
                            pShunt.Enabled := FALSE;
           //  {****} WriteDLLDebugFile(Format('Disabling shunt element %s.%s',[ pShunt.ParentClass.Name, pShunt.Name ]));
                            pShunt := BranchList.PresentBranch.NextShuntObject;
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


   // {****} If PDElem<>Nil then WriteDLLDebugFile(Format('Going on.. cktelement %d %s.%s phases=%d',[ BranchList.Level, PDelem.ParentClass.Name, PDElem.Name, PDelem.NPhases  ]));

    end;

    with ActiveCircuit[ActiveActor] do
    begin
        ReprocessBusDefs(ActiveActor);  // to get new load added and account for disabled devices
        DoResetMeterZones(ActiveActor);  // without eliminated devices
        Solution.SystemYChanged := TRUE; // force rebuild of Y
    end;


end;

end.
