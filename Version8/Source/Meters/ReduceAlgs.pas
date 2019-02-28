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

Uses CktTree, PDElement;

  procedure DoReduceDefault(Var BranchList:TCktTree);
  procedure DoReduceShortLines( Var BranchList:TCktTree );
  procedure DoReduceDangling(var BranchList:TCktTree);
  {{procedure DoReduceTapEnds(var BranchList:TCktTree);}
  procedure DoBreakLoops(var BranchList:TCktTree);
  procedure DoMergeParallelLines(var BranchList:TCktTree);
  procedure DoReduceSwitches(Var Branchlist:TCktTree);
  procedure DoRemoveAll_1ph_Laterals(Var Branchlist:TCktTree);

  procedure DoRemoveBranches(Var BranchList:TCktTree; FirstPDElement:TPDElement; KeepLoad:Boolean; Const EditStr:String);

  
implementation

Uses Line, Utilities, DSSGlobals, DSSClassDefs, Load, uComplex, ParserDel, CktElement, sysutils, ExecHelper, Bus;

Const
     SERIESMERGE:Boolean = True;
     PARALLELMERGE:Boolean = False;

procedure DoMergeParallelLines(var BranchList:TCktTree);
{Merge all lines in this zone that are marked in parallel}

Var
   LineElement:TLineObj;

begin
    IF BranchList <> NIL Then
    Begin
        BranchList.First;
        LineElement := BranchList.GoForward; // Always keep the first element
         WHILE LineElement <> NIL Do
          Begin
             If BranchList.PresentBranch.IsParallel Then
             Begin
               {There will always be two lines in parallel.  The first operation will disable the second}
               If LineElement.Enabled Then LineElement.MergeWith (TLineObj(BranchList.PresentBranch.LoopLineObj), PARALLELMERGE);  // Guaranteed to be a line
             End;
          LineElement := BranchList.GoForward;
          End;
    End;
end;

procedure DoBreakLoops(var BranchList:TCktTree);

{Break loops}
Var
   LineElement:TLineObj;

begin
    IF BranchList <> NIL Then
    Begin
        BranchList.First;
        LineElement := BranchList.GoForward; // Always keep the first element
         WHILE LineElement <> NIL Do
          Begin
             If BranchList.PresentBranch.IsLoopedHere Then
             Begin
               {There will always be two lines in the loop.  The first operation will disable the second}
               If LineElement.Enabled Then TLineObj(BranchList.PresentBranch.LoopLineObj).Enabled := FALSE; // Disable the other
             End;
          LineElement := BranchList.GoForward;
          End;
    End;
end;


{

procedure DoReduceTapEnds(var BranchList:TCktTree);
(*Var
   pLineElem1, pLineElem2:TLineObj;
   ToBusRef:Integer;
   AngleTest:Double;
   ParentNode:TCktTreeNode;
*)
begin

end;
}



procedure DoReduceDangling(var BranchList:TCktTree);
VAr
        pLineElem1:TDSSCktElement;
        ToBusRef:Integer;
begin
  If BranchList <> Nil Then
  Begin
     {Let's throw away all dangling end branches}
      BranchList.First;
      pLineElem1 := BranchList.GoForward; // Always keep the first element

     While pLineElem1 <> Nil Do
      Begin

         If IsLineElement(pLineElem1) Then
         With  BranchList.PresentBranch Do  Begin

             {If it is at the end of a section and has no load,cap, reactor,
             or coordinate, just throw it away}
             If IsDangling Then Begin
                 ToBusRef := ToBusReference;  // only access this property once!
                 If ToBusRef >0 Then
                   With ActiveCircuit[ActiveActor].Buses^[ToBusRef] Do
                     If Not (Keep) Then
                     pLineElem1.Enabled := False;
           End; {IF}
         End;  {If-With}
       pLineElem1 := BranchList.GoForward;
      End;
   End;

end;

Function IsShortLine(const Elem:TDSSCktElement):Boolean;
Var
   Ztest:Double;
   LineElement: TLineObj;

Begin
     LineElement :=  TLineObj(Elem);
     {Get Positive Sequence or equivalent from matrix}
     If LineElement.SymComponentsModel Then With LineElement Do Ztest := Cabs(Cmplx(R1, X1)) * Len
     Else {Get impedance from Z matrix}  {Zs - Zm ... approximates Z1}
         With LineElement Do Begin
            If NPhases>1 Then Ztest := Cabs(Csub(Z.Getelement(1,1), Z.GetElement(1,2))) * Len
            Else Ztest := Cabs(Z.Getelement(1,1)) * Len;
         End;

     If Ztest <= ActiveCircuit[ActiveActor].ReductionZmag Then Result := True Else Result := False;

End;

procedure DoReduceShortLines( Var BranchList:TCktTree );
{Eliminate short lines with impedance < Zmag and merge with lines on either side}
Var
   LineElement1, LineElement2 : TLineObj;
   ShuntElement : TDSSCktElement;
   ParentNode   : TCktTreeNode;
   MergeOK      : Boolean;

begin
    IF BranchList <> NIL Then  Begin  {eliminate really short lines}
      {First, flag all elements that need to be merged}
        LineElement1 := BranchList.First;
        LineElement1 := BranchList.GoForward; // Always keep the first element
         WHILE LineElement1 <> NIL Do  Begin
             If IsLineElement(LineElement1) Then  Begin
                 LineElement1.Flag := IsShortLine(LineElement1);    {Too small: Mark for merge with something}
             End; {IF}
          LineElement1 := BranchList.GoForward;  // traverse the whole meter zone  (circuit tree)
         End; {WHILE}

        LineElement1 := BranchList.First;
        LineElement1 := BranchList.GoForward; // Always keep the first element in the Tree
         WHILE LineElement1 <> NIL Do
          Begin
           if LineElement1.enabled then    // else skip

            If Not LineElement1.HasControl then
             If Not LineElement1.IsMonitored then   // Skip if controlled element or control is monitoring ,,,

             If LineElement1.Flag Then  // too short; Try to merge this element out
             Begin
               With BranchList Do
               Begin
     //   {****} WriteDLLDebugFile(Format('Processing Line.%s Bus1=%s Bus2=%s',[Uppercase(LineElement1.Name), LineElement1.GetBus(1), LineElement1.GetBus(2)]));
                 If (PresentBranch.NumChildBranches=0) and (PresentBranch.NumShuntObjects=0) Then  LineElement1.Enabled := False     // just discard it
                 Else
                  If (PresentBranch.NumChildBranches=0) {****OR (PresentBranch.NumChildBranches>1)**} then                    {Merge with Parent and move shunt elements to TO node on parent branch}
                     Begin
                        ParentNode := PresentBranch.ParentBranch;
                        If ParentNode <> Nil Then Begin
                            If ParentNode.NumChildBranches=1 Then   // only works for in-line
                              If Not ActiveCircuit[ActiveActor].Buses^[PresentBranch.ToBusReference].Keep Then Begin     // Check Keeplist
                               {Let's consider merging}
                                {First Check for any Capacitors. Skip if any}
                                MergeOK := TRUE;
                                If ParentNode.NumShuntObjects > 0 Then Begin
                                   ShuntElement :=  ParentNode.FirstShuntObject;
                                   While ShuntElement <> Nil Do Begin
                                     If ((ShuntElement.DSSObjType and CLASSMASK) = CAP_ELEMENT) OR
                                        ((ShuntElement.DSSObjType and CLASSMASK) = REACTOR_ELEMENT) Then Begin
                                        MergeOK := FALSE;
                                        Break;  // outta loop
                                     End;
                                     ShuntElement :=  PresentBranch.NextShuntObject;
                                   End;  {While}
                                End;

                                if MergeOK then  Begin
                                 LineElement2 := ParentNode.CktObject;
                                 If LineElement2.enabled Then  // Check to make sure it hasn't been merged out
                                 If IsLineElement(LineElement2) Then
                                   If LineElement2.MergeWith(LineElement1, SERIESMERGE) Then Begin {Move any loads to ToBus Reference of parent branch}
                          //    {****} WriteDLLDebugFile(Format('TOP Loop: Eliminating Line %s and merging into Line %s ',[Uppercase(LineElement1.Name), Uppercase(LineElement2.Name) ]));
                                      If ParentNode.NumShuntObjects>0 Then Begin
                                         {Redefine bus connection for PC elements hanging on the bus that is eliminated}
                                         ShuntElement :=  ParentNode.FirstShuntObject;
                                         While ShuntElement <> Nil Do  Begin
                                           Parser[ActiveActor].CmdString := 'bus1="' +ActiveCircuit[ActiveActor].BusList.Get(PresentBranch.ToBusReference) + GetNodeString(ShuntElement.GetBus(1) ) + '"';
                          //   {****} WriteDLLDebugFile(Format('Moving Shunt.%s from %s to %s ',[ShuntElement.Name, ShuntElement.GetBus(1), Parser.CmdString ]));
                                           ShuntElement.Edit(ActiveActor);
                                          ShuntElement :=  ParentNode.NextShuntObject;
                                         End;  {While}
                                      End; {IF}
                                   //+++ LineElement1 := BranchList.GoForward; // skip to next branch since we eliminated a bus
                                   End;
                                End;
                              End; {IF}
                        End; {IF ParentNode}
                     End

                  Else

                    If (PresentBranch.NumChildBranches=1) Then {Merge with child}
                     Begin
                      IF Not ActiveCircuit[ActiveActor].Buses^[PresentBranch.ToBusReference].Keep then    // check keeplist
                       Begin
                         {Let's consider merging}
                          {First Check for any Capacitors. Skip if any}
                          MergeOK := TRUE;
                          If PresentBranch.NumShuntObjects > 0 Then Begin
                             ShuntElement :=  PresentBranch.FirstShuntObject;
                             While ShuntElement <> Nil Do Begin
                               If ((ShuntElement.DSSObjType and CLASSMASK) = CAP_ELEMENT) OR
                                  ((ShuntElement.DSSObjType and CLASSMASK) = REACTOR_ELEMENT) Then Begin
                                  MergeOK := FALSE;
                                  Break;  // outta loop
                               End;
                               ShuntElement :=  PresentBranch.NextShuntObject;
                             End;  {While}
                          End;

                          if MergeOK then Begin
                            LineElement2 := PresentBranch.FirstChildBranch.CktObject; // child of PresentBranch
                            If LineElement2.enabled Then  // Check to make sure it hasn't been merged out
                            If IsLineElement(LineElement2) Then
                             If LineElement2.MergeWith(LineElement1, SERIESMERGE) Then Begin
                      //  {****} WriteDLLDebugFile(Format('BOT Loop: Eliminating Line %s and merging into Line %s ',[Uppercase(LineElement1.Name), Uppercase(LineElement2.Name)]));
                                  If PresentBranch.NumShuntObjects>0 Then Begin
                                   {Redefine bus connection to upline bus}
                                   ShuntElement :=  PresentBranch.FirstShuntObject;
                                   While ShuntElement <> Nil Do Begin
                                     Parser[ActiveActor].CmdString := 'bus1="' + ActiveCircuit[ActiveActor].BusList.Get(PresentBranch.FromBusReference) + GetNodeString(ShuntElement.GetBus(1) ) + '"';
                  //  {****} WriteDLLDebugFile(Format('Moving Shunt.%s from %s to %s ',[ShuntElement.Name, ShuntElement.GetBus(1), Parser.CmdString ]));
                                     ShuntElement.Edit(ActiveActor);
                                     ShuntElement :=  PresentBranch.NextShuntObject;
                                   End;  {While}
                               End; {IF}
                               LineElement1 := BranchList.GoForward; // skip to next branch since we eliminated a bus
                             End;
                          End;
                       End; {IF not}
                     End; {ELSE}
               End;
             End;
           LineElement1 := BranchList.GoForward;
          End;

          With ActiveCircuit[ActiveActor] Do Begin
             ReprocessBusDefs(ActiveActor);  // to get new load added and account for disabled devices
             DoResetMeterZones(ActiveActor);  // without eliminated devices
             Solution.SystemYChanged := True; // force rebuild of Y
          End;
    End;
end;

procedure DoReduceSwitches(Var Branchlist:TCktTree);

{Merge switches in with lines or delete if dangling}

Var
   LineElement1, LineElement2:TLineObj;
begin

   IF BranchList<>NIL Then
   Begin

     LineElement1 := BranchList.First;
     LineElement1 := BranchList.GoForward; // Always keep the first element
     WHILE LineElement1 <> NIL Do
     Begin

         If LineElement1.Enabled Then   // maybe we threw it away already
         If IsLineElement(LineElement1) Then
         If LineElement1.IsSwitch Then
         With BranchList.PresentBranch Do
             {see if eligble for merging}
             CASE  NumChildBranches of
              0: {Throw away if dangling}
                 IF NumShuntObjects = 0 Then LineElement1.Enabled := FALSE;

              1: IF NumShuntObjects = 0 Then
                 IF Not ActiveCircuit[ActiveActor].Buses^[ToBusReference].Keep then
                   Begin
                     {Let's consider merging}
                      LineElement2 := FirstChildBranch.CktObject;
                      If IsLineElement(LineElement2) Then
                      If Not LineElement2.IsSwitch then LineElement2.MergeWith(LineElement1, SERIESMERGE){Series Merge}
                   End;
             ELSE {Nada}
             END;

       LineElement1 := BranchList.GoForward;
     End;
   End;

end;

procedure DoReduceDefault(Var BranchList:TCktTree);

Var
   LineElement1, LineElement2:TLineObj;
begin
   IF BranchList<>NIL Then
   Begin

     {Now merge remaining lines}

     LineElement1 := BranchList.First;
     LineElement1 := BranchList.GoForward; // Always keep the first element
     WHILE LineElement1 <> NIL Do
       Begin

           If IsLineElement(LineElement1) Then
           If Not LineElement1.IsSwitch Then         // Exceptions
           if Not LineElement1.HasControl Then
           if Not LineElement1.IsMonitored Then
           If LineElement1.Enabled Then   // maybe we threw it away already
           With BranchList Do
             Begin
                 {see if eligble for merging}
                 IF PresentBranch.NumChildBranches = 1 Then
                 IF PresentBranch.NumShuntObjects = 0 Then
                 IF Not ActiveCircuit[ActiveActor].Buses^[PresentBranch.ToBusReference].Keep then
                   Begin
                     {Let's consider merging}
                      LineElement2 := PresentBranch.FirstChildBranch.CktObject;

                      If IsLineElement(LineElement2) Then
                      If Not LineElement2.IsSwitch then LineElement2.MergeWith(LineElement1, SERIESMERGE){Series Merge}
                   End;

             End;

         LineElement1 := BranchList.GoForward;
       End;
   End;

end;

procedure DoRemoveBranches(Var BranchList:TCktTree; FirstPDElement:TPDElement; KeepLoad:Boolean; Const EditStr:String);
Var
   PDElem :TPDElement;
   BusName: String;
   TotalkVA :Complex;
  // pLoad : TLoadObj;
   NewLoadName: String;
   pShunt: TDSSCktElement;
   LoadBus: TDSSBus;
   LoadBasekV: Double;
   StartLevel: Integer;

Begin

// Position BranchList at "FirstPDElement"
   PDElem := BranchList.First;
   while (PDElem <> FirstPDElement) and (PDElem <> nil) do
     PDElem := BranchList.GoForward;

   StartLevel := BranchList.level;

   if PDElem = nil then Begin
     DoSimpleMsg(Format('%s.%s Not Found (Remove Command).',[FirstPDElement.ParentClass.Name, FirstPDElement.Name ]), 5432100);
   End Else
   Begin

     { If KeepLoad, create a new Load object at upstream bus (from bus).}
     if KeepLoad then
     With BranchList.PresentBranch Do  Begin
       BusName := FirstPDElement.GetBus(FromTerminal);
       TotalkVA := CDivreal(PDelem.Power[FromTerminal, ActiveActor], 1000.0);
       NewLoadName := Format('Eq_%s_%s',[FirstPDElement.Name, StripExtension(BusName)]);
       {Pick up the kV Base for the From bus}
       LoadBus :=  ActiveCircuit[ActiveActor].Buses^[FromBusReference];
       if Loadbus.kVBase > 0.0 then LoadBasekV := LoadBus.kVBase
       Else
           Begin    // Try to guess from the present voltage at the first node on the bus
               ActiveCircuit[ActiveActor].Solution.UpdateVBus(ActiveActor);
               LoadBasekV := Cabs(Loadbus.Vbus^[1]) * 0.001;
           End;
       If FirstPDElement.NPhases > 1 Then LoadBasekV := LoadBasekV * Sqrt3;
       {Load up parser with definition of equivalent load}
       Parser[ActiveActor].CmdString := Format(' phases=%d Bus1=%s kW=%g kvar=%g kV=%g %s', [FirstPDElement.NPhases, Busname, TotalkVA.re, TotalkVA.im, LoadBasekV, EditStr ]);
       AddObject('load', NewLoadName); // Add new load to circuit
     End;

     {Disable all elements in the tree downline from the start element}

   //  {****} WriteDLLDebugFile(Format('StartLevel = %d ',[ StartLevel]));

     while PDElem <> Nil do
     Begin
   // {****} WriteDLLDebugFile(Format('Disabling cktelement %d %s.%s',[ BranchList.Level, PDelem.ParentClass.Name, PDElem.Name ]));
         With BranchList Do
         Begin
              pShunt := PresentBranch.FirstShuntObject;
              while pShunt <> Nil do
              Begin
                 pShunt.Enabled := False;
   //  {****} WriteDLLDebugFile(Format('Disabling shunt element %s.%s',[ pShunt.ParentClass.Name, pShunt.Name ]));
                 pShunt := PresentBranch.NextShuntObject;
              End;
         End;

         PDElem.Enabled := False;
         PDElem := BranchList.GoForward;

         // Check to see if we are back where we started. If so, stop.
         if BranchList.Level <= StartLevel then PDElem := Nil;

     End;

   End;

   With ActiveCircuit[ActiveActor] Do Begin
     ReprocessBusDefs(ActiveActor);  // to get new load added and account for disabled devices
     DoResetMeterZones(ActiveActor);  // without eliminated devices
     Solution.SystemYChanged := True; // force rebuild of Y
   End;
End;

procedure DoRemoveAll_1ph_Laterals(Var Branchlist:TCktTree);
{Remove all 1-phase laterals in Branchlist and lump total load back to main feeder}
{
  This removes all elements on all 1ph laterals and moves the net load back to the main feeder tap point
  Does not
}

Var
   PDelem:      TPDElement;
   BusName:     String;
   pShunt:      TDSSCktElement;
   HeadBus:     TDSSBus;
   HeadBasekV:  Double;
   StartLevel:  Integer;
   pBus:        TDSSBus;
   strNodes:    String;


Begin
 {
  Just march down the feeder until we encounter a 1-phase PD element
 }
   // Position BranchList at "beginning"
   PDElem := BranchList.First;

   While PDElem<> Nil Do
   Begin

     if PDElem.nphases=1 then   // ELIMINATE THIS LATERAL
     Begin
        {Check to see if this is a 1-phase switch or other branch in the middle of a 3-phase branch and go on}
        {If the To bus has more than 1 phase, keep this branch else lump the load at the From node}
         pBus := ActiveCircuit[ActiveActor].Buses^[Branchlist.PresentBranch.ToBusReference];  //To Bus

         if pBus.NumNodesThisBus = 1 then // Eliminate the lateral starting with this branch
         Begin

             { If KeepLoad (ReduceLateralsKeepLoad), create a new Load object at upstream bus (from bus).}

             if ActiveCircuit[ActiveActor].ReduceLateralsKeepLoad  then
             With BranchList Do  Begin
                 BusName     := PDElem.GetBus(PresentBranch.FromTerminal);
                 // Make sure there is a node reference .. default to 1
                 if Pos('.', BusName, 1) = 0 then BusName := BusName + '.1';

                 {Pick up the kV Base for the From bus}
                 HeadBus     :=  ActiveCircuit[ActiveActor].Buses^[PresentBranch.FromBusReference];
                 if (HeadBus.kVBase > 0.0 )
                 Then HeadBasekV := HeadBus.kVBase  // Use defined voltage base
                 Else
                     Begin    // Try to guess voltage base from the present voltage at the first node on the bus
                         ActiveCircuit[ActiveActor].Solution.UpdateVBus(ActiveActor);
                         HeadBasekV := Cabs(HeadBus.Vbus^[1]) * 0.001;
                     End;
             End;

             {

               Disable all PDelements in the tree downline from the beginning of the 1-phase lateral
               Move 1-phase shunts to Headbus

             }

             StartLevel := BranchList.level;   // record level of first 1-phase branch in this lateral
             while PDElem <> Nil do
             Begin
            // {****} WriteDLLDebugFile(Format('Disabling cktelement %d %s.%s',[ BranchList.Level, PDelem.ParentClass.Name, PDElem.Name ]));
                 { Get rid of loads and other shunt elements connected to this branch }
                 With BranchList Do
                 Begin
                      pShunt := PresentBranch.FirstShuntObject;
                      while pShunt <> Nil do
                      Begin
                         Parser[ActiveActor].CmdString := Format('Bus1=%s kV=%.6g ',[Busname, HeadBasekV ]);
                         pShunt.Edit(ActiveActor);
                         pShunt := PresentBranch.NextShuntObject;
                      End;
                 End;

                 PDElem.Enabled := False;
                 PDElem := BranchList.GoForward;

                 // Check to see if we are back where we started. If so, stop with this lateral and get on to the next.
                 If PDElem <> Nil then if (BranchList.Level <= StartLevel) then Break;
             End ;

         End
         ELSE PDElem := BranchList.GoForward;


     End
     ELSE PDElem := BranchList.GoForward;


   // {****} If PDElem<>Nil then WriteDLLDebugFile(Format('Going on.. cktelement %d %s.%s phases=%d',[ BranchList.Level, PDelem.ParentClass.Name, PDElem.Name, PDelem.NPhases  ]));

   End;

   With ActiveCircuit[ActiveActor] Do Begin
     ReprocessBusDefs(ActiveActor);  // to get new load added and account for disabled devices
     DoResetMeterZones(ActiveActor);  // without eliminated devices
     Solution.SystemYChanged := True; // force rebuild of Y
   End;


End;

end.
