unit ImplBus;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TBus = class(TAutoObject, IBus)
    protected
    function Get_Name: WideString; safecall;
    function Get_NumNodes: Integer; safecall;
    function Get_SeqVoltages: OleVariant; safecall;
    function Get_Voltages: OleVariant; safecall;
    function Get_Nodes: OleVariant; safecall;
    function Get_Isc: OleVariant; safecall;
    function Get_Voc: OleVariant; safecall;
    function Get_kVBase: Double; safecall;
    function Get_puVoltages: OleVariant; safecall;
    function Get_Zsc0: OleVariant; safecall;
    function Get_Zsc1: OleVariant; safecall;
    function Get_ZscMatrix: OleVariant; safecall;
    function ZscRefresh: WordBool; safecall;
    function Get_YscMatrix: OleVariant; safecall;
    function Get_Coorddefined: WordBool; safecall;
    function Get_x: Double; safecall;
    procedure Set_x(Value: Double); safecall;
    function Get_y: Double; safecall;
    procedure Set_y(Value: Double); safecall;
    function Get_Distance: Double; safecall;
    function GetUniqueNodeNumber(StartNumber: Integer): Integer; safecall;
    function Get_CplxSeqVoltages: OleVariant; safecall;
    function Get_Int_Duration: Double; safecall;
    function Get_Lambda: Double; safecall;
    function Get_Cust_Duration: Double; safecall;
    function Get_Cust_Interrupts: Double; safecall;
    function Get_N_Customers: Integer; safecall;
    function Get_N_interrupts: Double; safecall;
    function Get_puVLL: OleVariant; safecall;
    function Get_VLL: OleVariant; safecall;
    function Get_puVmagAngle: OleVariant; safecall;
    function Get_VMagAngle: OleVariant; safecall;
    function Get_TotalMiles: Double; safecall;
    function Get_SectionID: Integer; safecall;
  end;

implementation

uses ComServ, DSSGlobals, ImplGlobals, Circuit, Ucomplex, MathUtil, sysutils,
     ExecHelper, SolutionAlgs, Variants, Utilities, Bus;

function TBus.Get_Name: WideString;
begin
     Result := '';

     If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
     IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
              Result := BusList.Get(ActiveBusIndex);
end;

function TBus.Get_NumNodes: Integer;
begin
     Result := 0;
     If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
     IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Result := ActiveCircuit[ActiveActor].Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].NumNodesThisBus;

end;

function TBus.Get_SeqVoltages: OleVariant;

// Compute sequence voltages for Active Bus
// magnitude only
// returns a set of seq voltages (3)

VAR
  Nvalues,i, iV:Integer;
  VPh, V012 : Array[1..3] of Complex;

Begin
   IF ActiveCircuit[ActiveActor] = nil Then Begin
        Result := VarArrayCreate([0, 0], varDouble)
   End
   ELSE With ActiveCircuit[ActiveActor] Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      Nvalues := Buses^[ActiveBusIndex].NumNodesThisBus;
      If Nvalues >3 then Nvalues := 3;

      // Assume nodes 1, 2, and 3 are the 3 phases
      Result := VarArrayCreate( [0, 2], varDouble);
      IF Nvalues<>3 THEN
         For i := 1 to 3 DO Result[i-1] := -1.0  // Signify seq voltages n/A for less then 3 phases
      ELSE
      Begin

          iV := 0;
          FOR i := 1 to  3 DO
          Begin
            Vph[i]  := Solution.NodeV^[Buses^[ActiveBusIndex].Find(i)];
          End;

          Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components

          For i := 1 to 3 DO  // Stuff it in the result
          Begin
             Result[iV] := Cabs(V012[i]);
             Inc(iV);
          End;

      End;
   End
  ELSE Result := VarArrayCreate([0, 0], varDouble);


end;

function TBus.Get_Voltages: OleVariant;
// Return Complex for all nodes of voltages for Active Bus

VAR
  Nvalues,i,  iV, NodeIdx, jj : Integer;
  Volts : Complex;
  pBus : TDSSBus;

Begin
   IF ActiveCircuit[ActiveActor] = nil Then Begin
        Result := VarArrayCreate([0, 0], varDouble)
   End
   ELSE With ActiveCircuit[ActiveActor] Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      pBus    := Buses^[ActiveBusIndex];
      Nvalues := pBus.NumNodesThisBus;
      Result  := VarArrayCreate( [0, 2*NValues -1], varDouble);
      iV := 0;
      jj := 1;
      WITH pBus DO
      FOR i := 1 to  NValues DO
      Begin
            // this code so nodes come out in order from smallest to larges
            Repeat
                 NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                 inc(jj)
            Until NodeIdx>0;

            Volts      := Solution.NodeV^[GetRef(NodeIdx)];  // referenced to pBus
            Result[iV] := Volts.re;
            Inc(iV);
            Result[iV] := Volts.im;
            Inc(iV);
      End;
  End
  ELSE Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

function TBus.Get_Nodes: OleVariant;

// return array of node numbers corresponding to voltages

VAR
  Nvalues,i, iV, NodeIdx, jj:Integer;
  pBus : TDSSBus;

Begin
   IF ActiveCircuit[ActiveActor] = nil Then Begin
        Result := VarArrayCreate([0, 0], varInteger)
   End
   ELSE With ActiveCircuit[ActiveActor] Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      pBus := Buses^[ActiveBusIndex];
      With pBus Do
      Begin
          Nvalues := NumNodesThisBus;
          Result := VarArrayCreate( [0, NValues -1], varInteger);
          iV := 0;
          jj := 1;
          FOR i := 1 to  NValues DO
          Begin
                // this code so nodes come out in order from smallest to larges
              Repeat
                   NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                   inc(jj)
              Until NodeIdx>0;
             Result[iV] := Buses^[ActiveBusIndex].GetNum(NodeIdx);
             Inc(iV);
          End;
      End;
  End
  ELSE Result := VarArrayCreate([0, 0], varInteger);  // just return null array

end;

function TBus.Get_Isc: OleVariant;

// Return the short circuit current

Var
   Isc :Complex;
   i, iV, NValues :Integer;

begin

   IF ActiveCircuit[ActiveActor] = nil Then Begin
        Result := VarArrayCreate([0, 0], varDouble)
   End
   ELSE With ActiveCircuit[ActiveActor] Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      If Buses^[ActiveBusIndex].BusCurrent <> nil Then
      Begin
        NValues := Buses^[ActiveBusIndex].NumNodesThisBus;
        Result := VarArrayCreate( [0, 2*NValues -1], varDouble);
        iV := 0;
        FOR i := 1 to  NValues DO
        Begin
           Isc := Buses^[ActiveBusIndex].BusCurrent^[i];
           Result[iV] := Isc.Re;
           Inc(iV);
           Result[iV] := Isc.Im;
           Inc(iV);
        End;
      End
      Else
          Result := VarArrayCreate([0, 0], varDouble);
  End
  ELSE Result := VarArrayCreate([0, 0], varDouble) ;  // just return null array

end;

function TBus.Get_Voc: OleVariant;

// Return the Open circuit Voltage for this bus

Var
   Voc :Complex;
   i, iV, NValues :Integer;

begin

   IF ActiveCircuit[ActiveActor] = nil Then Begin
        Result := VarArrayCreate([0, 0], varDouble)
   End
   ELSE With ActiveCircuit[ActiveActor] Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      If Buses^[ActiveBusIndex].VBus <> nil Then
      Begin
        NValues := Buses^[ActiveBusIndex].NumNodesThisBus;
        Result := VarArrayCreate( [0, 2*NValues -1], varDouble);
        iV := 0;
        FOR i := 1 to  NValues DO
        Begin
           Voc := Buses^[ActiveBusIndex].VBus^[i];
           Result[iV] := Voc.Re;
           Inc(iV);
           Result[iV] := Voc.Im;
           Inc(iV);
        End;
      End
      Else
          Result := VarArrayCreate([0, 0], varDouble);
  End
  ELSE Result := VarArrayCreate([0, 0], varDouble) ;  // just return null array


end;

function TBus.Get_kVBase: Double;
begin
     Result := 0.0;
     If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
     IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Result := ActiveCircuit[ActiveActor].Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].kVBase ;

end;

function TBus.Get_puVoltages: OleVariant;

// Returns voltages at bus in per unit.  However, if kVBase=0, returns actual volts

VAR
  Nvalues,i, iV,  NodeIdx, jj :Integer;
  Volts:Complex;
  BaseFactor:Double;
  pBus : TDSSBus;

Begin
   IF ActiveCircuit[ActiveActor] = nil Then Begin
        Result := VarArrayCreate([0, 0], varDouble)
   End
   ELSE With ActiveCircuit[ActiveActor] Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      pBus    := Buses^[ActiveBusIndex];
      With pBus Do
      Begin
          Nvalues := NumNodesThisBus;
          Result := VarArrayCreate( [0, 2*NValues -1], varDouble);
          iV := 0;
          jj := 1;
          If kVBase>0.0 Then BaseFactor := 1000.0*kVBase
                        Else BaseFactor := 1.0;
          FOR i := 1 to  NValues DO
          Begin
                // this code so nodes come out in order from smallest to larges
                Repeat
                     NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                     inc(jj)
                Until NodeIdx>0;

                Volts      := Solution.NodeV^[GetRef(NodeIdx)];
                Result[iV] := Volts.re/BaseFactor;
                Inc(iV);
                Result[iV] := Volts.im/BaseFactor;
                Inc(iV);
          End;
      End;
  End
  ELSE Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

function TBus.Get_Zsc0: OleVariant;

Var  Z:Complex;

begin
    IF ActiveCircuit[ActiveActor] = nil Then Begin
            Result := VarArrayCreate([0, 0], varDouble)
    End
    ELSE With ActiveCircuit[ActiveActor] Do
    IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
    Begin
            Z := Buses^[ActiveBusIndex].Zsc0;
            Result := VarArrayCreate( [0, 1], varDouble);
            Result[0] := Z.Re;
            Result[1] := Z.Im;
    End
    ELSE Result := VarArrayCreate([0, 0], varDouble) ;  // just return null array
end;

function TBus.Get_Zsc1: OleVariant;

Var Z:Complex;

begin

    IF ActiveCircuit[ActiveActor] = nil Then Begin
            Result := VarArrayCreate([0, 0], varDouble)
    End
    ELSE With ActiveCircuit[ActiveActor] Do
    IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
    Begin
            Z := Buses^[ActiveBusIndex].Zsc1;
            Result := VarArrayCreate( [0, 1], varDouble);
            Result[0] := Z.Re;
            Result[1] := Z.Im;
    End
    ELSE Result := VarArrayCreate([0, 0], varDouble) ;  // just return null array

end;

function TBus.Get_ZscMatrix: OleVariant;

Var
    Nelements, iV, i, j : Integer;
    Z:Complex;

begin

    IF ActiveCircuit[ActiveActor] = nil Then Begin
            Result := VarArrayCreate([0, 0], varDouble)
    End
    ELSE

    Try

    With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        If Assigned(Buses^[ActiveBusIndex].Zsc) Then Begin
          Nelements := Buses^[ActiveBusIndex].Zsc.Order;
          Result := VarArrayCreate( [0, ((2*Nelements*Nelements)-1)], varDouble);
          iV := 0;
          With Buses^[ActiveBusIndex] Do
          For i := 1 to Nelements Do
            For j := 1 to Nelements Do  Begin
              Z := Zsc.GetElement(i,j);
              Result[iV] := Z.Re;
              Inc(iV);
              Result[iV] := Z.Im;
              Inc(iV);
            End;

        End
        Else  Result := VarArrayCreate([0, 0], varDouble) ;  // just return null array
      End
      ELSE Result := VarArrayCreate([0, 0], varDouble) ;  // just return null array

      Except
          On E:Exception Do DoSimpleMsg('ZscMatrix Error: ' + E.message + CRLF , 5016);
      End;

end;

function TBus.ZscRefresh: WordBool;


begin

   Result := False;   // Init in case of failure

   If ExecHelper.DoZscRefresh(ActiveActor) = 0 Then Result := TRUE;

end;

function TBus.Get_YscMatrix: OleVariant;

Var
    Nelements, iV, i, j : Integer;
    Y1:Complex;

begin

    IF ActiveCircuit[ActiveActor] = nil Then Begin
            Result := VarArrayCreate([0, 0], varDouble)
    End
    ELSE

    Try

    With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        If Assigned(Buses^[ActiveBusIndex].Ysc) Then Begin
          Nelements := Buses^[ActiveBusIndex].Ysc.Order;
          Result := VarArrayCreate( [0, ((2*Nelements*Nelements)-1)], varDouble);
          iV := 0;
          With Buses^[ActiveBusIndex] Do
          For i := 1 to Nelements Do
            For j := 1 to Nelements Do  Begin
              Y1 := Ysc.GetElement(i,j);
              Result[iV] := Y1.Re;
              Inc(iV);
              Result[iV] := Y1.Im;
              Inc(iV);
            End;

        End
        Else  Result := VarArrayCreate([0, 0], varDouble) ;  // just return null array
      End
      ELSE Result := VarArrayCreate([0, 0], varDouble) ;  // just return null array

      Except
          On E:Exception Do DoSimpleMsg('ZscMatrix Error: ' + E.message + CRLF, 5017 );
      End;


end;

function TBus.Get_Coorddefined: WordBool;
begin
     Result := FALSE;
     If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       IF (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) Then Result := TRUE;
end;

function TBus.Get_x: Double;
begin
    Result := 0.0;
     If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       IF (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) Then
         Result := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].x;
end;

procedure TBus.Set_x(Value: Double);
begin
     If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
         Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined := TRUE;
         Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].x := Value;
       End;
end;

function TBus.Get_y: Double;
begin
    Result := 0.0;
     If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       IF (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) Then
         Result := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].y;
end;

procedure TBus.Set_y(Value: Double);
begin
   If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
         Buses^[ActiveBusIndex].Coorddefined := TRUE;
         Buses^[ActiveBusIndex].y := Value;
       End;
end;

function TBus.Get_Distance: Double;
begin
    Result := 0.0;
    If (ActiveCircuit[ActiveActor] <> Nil) Then
     With ActiveCircuit[ActiveActor] Do
      IF ((ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses)) Then
        Result := Buses^[ActiveBusIndex].DistFromMeter;
end;

function TBus.GetUniqueNodeNumber(StartNumber: Integer): Integer;
begin
    if ActiveCircuit[ActiveActor] <> Nil then
    With ActiveCircuit[ActiveActor] Do

    if ActiveBusIndex > 0 then
      Result := Utilities.GetUniqueNodeNumber(BusList.Get(ActiveBusIndex), StartNumber);
end;

function TBus.Get_CplxSeqVoltages: OleVariant;

// Compute sequence voltages for Active Bus
// Complex values
// returns a set of seq voltages (3) in 0, 1, 2 order

VAR
  Nvalues,i, iV:Integer;
  VPh, V012 : Array[1..3] of Complex;

Begin
   IF ActiveCircuit[ActiveActor] = nil Then Begin
        Result := VarArrayCreate([0, 0], varDouble)
   End
   ELSE With ActiveCircuit[ActiveActor] Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      Nvalues := Buses^[ActiveBusIndex].NumNodesThisBus;
      If Nvalues > 3 then Nvalues := 3;

      // Assume nodes labelled 1, 2, and 3 are the 3 phases
      Result := VarArrayCreate( [0, 5], varDouble);
      IF Nvalues <> 3 THEN
          For i := 1 to 6 DO Result[i-1] := -1.0  // Signify seq voltages n/A for less then 3 phases
      ELSE
      Begin
          iV := 0;
          FOR i := 1 to 3 DO Vph[i] := Solution.NodeV^[Buses^[ActiveBusIndex].Find(i)];

          Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components

          For i := 1 to 3 DO  // Stuff it in the result
          Begin
             Result[iV] := V012[i].re;
             Inc(iV);
             Result[iV] := V012[i].im;
             Inc(iV);
          End;
      End;
   End
  ELSE Result := VarArrayCreate([0, 0], varDouble);

end;

function TBus.Get_Int_Duration: Double;
begin
  Result := 0.0;
  if ActiveCircuit[ActiveActor] <> Nil then
    With ActiveCircuit[ActiveActor] Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].Bus_Int_Duration;
end;

function TBus.Get_Lambda: Double;
begin
  Result := 0.0;
  if ActiveCircuit[ActiveActor] <> Nil then
    With ActiveCircuit[ActiveActor] Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].BusFltRate;
end;

function TBus.Get_Cust_Duration: Double;
begin
  Result := 0.0;
  if ActiveCircuit[ActiveActor] <> Nil then
    With ActiveCircuit[ActiveActor] Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].BusCustDurations ;
end;

function TBus.Get_Cust_Interrupts: Double;
begin
  Result := 0.0;
  if ActiveCircuit[ActiveActor] <> Nil then
    With ActiveCircuit[ActiveActor] Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].BusCustDurations ;
end;

function TBus.Get_N_Customers: Integer;
begin
  Result := 0;
  if ActiveCircuit[ActiveActor] <> Nil then
    With ActiveCircuit[ActiveActor] Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].BusTotalNumCustomers  ;
end;

function TBus.Get_N_interrupts: Double;
begin
  Result := 0.0;
  if ActiveCircuit[ActiveActor] <> Nil then
    With ActiveCircuit[ActiveActor] Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].Bus_Num_Interrupt ;
end;


function TBus.Get_puVLL: OleVariant;

VAR
  Nvalues,i,  iV, NodeIdxi, NodeIdxj, jj : Integer;
  Volts : Complex;
  pBus : TDSSBus;
  BaseFactor : double;

Begin
   IF ActiveCircuit[ActiveActor] = nil Then Begin
        Result := VarArrayCreate([0, 0], varDouble)
   End
   ELSE With ActiveCircuit[ActiveActor] Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      pBus    := Buses^[ActiveBusIndex];
      Nvalues := pBus.NumNodesThisBus;
      If Nvalues > 3 Then Nvalues := 3;

      If Nvalues > 1 Then
      Begin
          If Nvalues = 2 Then  Nvalues := 1;  // only one L-L voltage if 2 phase
          Result  := VarArrayCreate( [0, 2*NValues -1], varDouble);
          iV := 0;
          WITH pBus DO
          Begin
            If kVBase>0.0 Then BaseFactor := 1000.0*kVBase*sqrt3
                          Else BaseFactor := 1.0;
            FOR i := 1 to  NValues DO     // for 2- or 3-phases
            Begin
                  // this code assumes the nodes are ordered 1, 2, 3
                  NodeIdxi := FindIdx(i);  // Get the index of the Node that matches i
                  jj := i+1;
                  if jj>3 then jj := 1; // wrap around
                  NodeIdxj := FindIdx(jj);

                  With Solution Do Volts := Csub(NodeV^[GetRef(NodeIdxi)], NodeV^[GetRef(NodeIdxj)]);
                  Result[iV] := Volts.re / BaseFactor;
                  Inc(iV);
                  Result[iV] := Volts.im / BaseFactor;
                  Inc(iV);
            End;
          End;  {With pBus}
      End
      ELSE Begin  // for 1-phase buses, do not attempt to compute.
          Result := VarArrayCreate([0, 1], varDouble);  // just return -1's in array
          Result[0] := -99999.0;
          Result[1] := 0.0;
      End;
  End
  ELSE Result := VarArrayCreate([0, 0], varDouble);  // just return null array


end;

function TBus.Get_VLL: OleVariant;
VAR
  Nvalues,i,  iV, NodeIdxi, NodeIdxj, jj : Integer;
  Volts : Complex;
  pBus : TDSSBus;

Begin
   IF ActiveCircuit[ActiveActor] = nil Then Begin
        Result := VarArrayCreate([0, 0], varDouble)
   End
   ELSE With ActiveCircuit[ActiveActor] Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      pBus    := Buses^[ActiveBusIndex];
      Nvalues := pBus.NumNodesThisBus;
      If Nvalues > 3 Then Nvalues := 3;

      If Nvalues > 1 Then
      Begin
          If Nvalues = 2 Then  Nvalues := 1;  // only one L-L voltage if 2 phase
          Result  := VarArrayCreate( [0, 2*NValues -1], varDouble);
          iV := 0;
          WITH pBus DO
            FOR i := 1 to  NValues DO     // for 2- or 3-phases
            Begin

                  // this code assumes the nodes are ordered 1, 2, 3
                  // this code so nodes come out in order from smallest to largest
                  NodeIdxi := FindIdx(i);  // Get the index of the Node that matches i
                  jj := i+1;
                  if jj>3 then jj := 1; // wrap around
                  NodeIdxj := FindIdx(jj);

                  With Solution Do Volts := Csub(NodeV^[GetRef(NodeIdxi)], NodeV^[GetRef(NodeIdxj)]);
                  Result[iV] := Volts.re;
                  Inc(iV);
                  Result[iV] := Volts.im;
                  Inc(iV);
            End;
      End
      ELSE Begin  // for 1-phase buses, do not attempt to compute.
          Result := VarArrayCreate([0, 1], varDouble);  // just return -1's in array
          Result[0] := -99999.0;
          Result[1] := 0.0;
      End;
  End
  ELSE Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

function TBus.Get_puVmagAngle: OleVariant;
// Return mag/angle for all nodes of voltages for Active Bus

VAR
  Nvalues,i,  iV, NodeIdx, jj : Integer;
  Volts : polar;
  pBus : TDSSBus;
  Basefactor : Double;

Begin

   IF ActiveCircuit[ActiveActor] = nil Then Begin
        Result := VarArrayCreate([0, 0], varDouble)
   End
   ELSE With ActiveCircuit[ActiveActor] Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      pBus    := Buses^[ActiveBusIndex];
      Nvalues := pBus.NumNodesThisBus;
      Result  := VarArrayCreate( [0, 2*NValues -1], varDouble);
      iV := 0;
      jj := 1;
      WITH pBus DO Begin
          If kVBase>0.0 Then BaseFactor := 1000.0 * kVBase
                        Else BaseFactor := 1.0;

          FOR i := 1 to  NValues DO
          Begin
                // this code so nodes come out in order from smallest to larges
                Repeat
                      NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                      inc(jj)
                Until NodeIdx>0;

                Volts      := ctopolardeg(Solution.NodeV^[GetRef(NodeIdx)]);  // referenced to pBus
                Result[iV] := Volts.mag / BaseFactor;
                Inc(iV);
                Result[iV] := Volts.ang;
                Inc(iV);
          End;
      End;
  End
  ELSE Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

function TBus.Get_VMagAngle: OleVariant;
// Return mag/angle for all nodes of voltages for Active Bus

VAR
  Nvalues,i,  iV, NodeIdx, jj : Integer;
  Volts : polar;
  pBus : TDSSBus;

Begin

   IF ActiveCircuit[ActiveActor] = nil Then Begin
        Result := VarArrayCreate([0, 0], varDouble)
   End
   ELSE With ActiveCircuit[ActiveActor] Do
   IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
   Begin
      pBus    := Buses^[ActiveBusIndex];
      Nvalues := pBus.NumNodesThisBus;
      Result  := VarArrayCreate( [0, 2*NValues -1], varDouble);
      iV := 0;
      jj := 1;
      WITH pBus DO
      FOR i := 1 to  NValues DO
      Begin
            // this code so nodes come out in order from smallest to larges
            Repeat
                 NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                 inc(jj)
            Until NodeIdx>0;

            Volts      := ctopolardeg(Solution.NodeV^[GetRef(NodeIdx)]);  // referenced to pBus
            Result[iV] := Volts.mag;
            Inc(iV);
            Result[iV] := Volts.ang;
            Inc(iV);
      End;
  End
  ELSE Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

function TBus.Get_TotalMiles: Double;
begin
  Result := 0.0;
  if ActiveCircuit[ActiveActor] <> Nil then
    With ActiveCircuit[ActiveActor] Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].BusTotalMiles  ;
end;

function TBus.Get_SectionID: Integer;
begin
  Result := 0;
  if ActiveCircuit[ActiveActor] <> Nil then
    With ActiveCircuit[ActiveActor] Do
      if ActiveBusIndex > 0 then
         Result := Buses^[ActiveBusIndex].BusSectionID  ;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TBus, Class_Bus, ciInternal , tmApartment	);
end.
