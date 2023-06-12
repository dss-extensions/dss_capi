unit DBus;

interface

uses DSSGlobals, Circuit, Ucomplex, MathUtil, sysutils,
     ExecHelper, SolutionAlgs, Variants, Utilities, Bus, CktElement,
     Ucmatrix, Arraydef;

function BUSI(mode: longint; arg: longint): longint; cdecl;
function BUSF(mode: longint; arg: double): double; cdecl;
function BUSS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;
procedure BUSV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

var
  myStrArray  : Array of Byte;
  myDBLArray  : Array of double;
  myCmplxArray: Array of Complex;
  myPolarArray: Array of Polar;
  myIntArray  : Array of integer;

implementation

Function CheckBusReference(cktElem:TDSSCktElement; BusReference:Integer; Var TerminalIndex:integer):Boolean;

{Check all terminals of cktelement to see if bus connected to busreference}

Var  i:integer;
Begin
     Result := FALSE;
     With cktElem Do
     For i := 1 to NTerms Do Begin
         If Terminals^[i].BusRef = BusReference Then Begin
             TerminalIndex := i;
             Result := TRUE;
             Break;
         End;
     End;
End;

function BUSI(mode: longint; arg: longint): longint; cdecl;

begin
  Result := 0;  // Default return value
  case mode of
  0: begin                                           // Bus.NumNodes
     Result := 0;
     If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
     IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Result := ActiveCircuit[ActiveActor].Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].NumNodesThisBus;
  end;
  1: begin                                           // Bus.ZscRefresh
     Result := 0;   // Init in case of failure
     If ExecHelper.DoZscRefresh(ActiveActor) = 0 Then Result := 1;
  end;
  2: begin                                           // Bus.Coorddefined
     Result := 0;
     If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       IF (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) Then Result := 1;
  end;
  3: begin                                           // Bus.GetUniqueNodeNumber
      if ActiveCircuit[ActiveActor] <> Nil then
      With ActiveCircuit[ActiveActor] Do
      if ActiveBusIndex > 0 then
        Result := Utilities.GetUniqueNodeNumber(BusList.Get(ActiveBusIndex), arg);
  end;
  4: begin                                           // Bus.N_Customers
      Result := 0;
      if ActiveCircuit[ActiveActor] <> Nil then
        With ActiveCircuit[ActiveActor] Do
          if ActiveBusIndex > 0 then
             Result := Buses^[ActiveBusIndex].BusTotalNumCustomers  ;
  end;
  5: begin                                           // Bus.SectionID
     Result := 0;
      if ActiveCircuit[ActiveActor] <> Nil then
        With ActiveCircuit[ActiveActor] Do
          if ActiveBusIndex > 0 then
             Result := Buses^[ActiveBusIndex].BusSectionID  ;
  end
  else
      Result:=-1;
  end;
end;

//**************************floating point variables***************************
function BUSF(mode: longint; arg: double): double; cdecl;
begin
  Result := 0.0;  // Default return value
  case mode of
  0: begin                                           // Bus.kVBase
     Result := 0.0;
     If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
     IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Result := ActiveCircuit[ActiveActor].Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].kVBase ;
  end;
  1: begin                                           // Bus.X -read
    Result := 0.0;
     If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       IF (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) Then
         Result := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].x;
  end;
  2: begin                                           // Bus.X - Write
     If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       Begin
         Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined := TRUE;
         Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].x := arg;
       End;
       Result:=0.0;
  end;
  3: begin                                           // Bus.Y -read
    Result := 0.0;
     If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
       IF (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) Then
         Result := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].y;
  end;
  4: begin                                           // Bus.Y - Write
     If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
        Begin
          Buses^[ActiveBusIndex].Coorddefined := TRUE;
          Buses^[ActiveBusIndex].y := arg;
        End;
        Result := 0.0;
  end;
  5: begin                                           // Bus.Distance
     Result := 0.0;
     If (ActiveCircuit[ActiveActor] <> Nil) Then
      With ActiveCircuit[ActiveActor] Do
       IF ((ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses)) Then
         Result := Buses^[ActiveBusIndex].DistFromMeter;
  end;
  6: begin                                           // Bus.Lambda
      Result := 0.0;
      if ActiveCircuit[ActiveActor] <> Nil then
        With ActiveCircuit[ActiveActor] Do
          if ActiveBusIndex > 0 then
             Result := Buses^[ActiveBusIndex].BusFltRate;
  end;
  7: begin                                           // Bus.N_interrupts
      Result := 0.0;
      if ActiveCircuit[ActiveActor] <> Nil then
      With ActiveCircuit[ActiveActor] Do
         if ActiveBusIndex > 0 then
           Result := Buses^[ActiveBusIndex].Bus_Num_Interrupt ;
  end;
  8: begin                                           // Bus.int_duration
      Result := 0.0;
      if ActiveCircuit[ActiveActor] <> Nil then
        With ActiveCircuit[ActiveActor] Do
          if ActiveBusIndex > 0 then
             Result := Buses^[ActiveBusIndex].Bus_Int_Duration;
  end;
  9: begin                                           // Bus.Cust_interrupts
      Result := 0.0;
      if ActiveCircuit[ActiveActor] <> Nil then
        With ActiveCircuit[ActiveActor] Do
          if ActiveBusIndex > 0 then
             Result := Buses^[ActiveBusIndex].BusCustDurations ;
  end;
  10: begin                                          // Bus.Cust_duration
      Result := 0.0;
    if ActiveCircuit[ActiveActor] <> Nil then
       With ActiveCircuit[ActiveActor] Do
         if ActiveBusIndex > 0 then
            Result := Buses^[ActiveBusIndex].BusCustDurations ;
  end;
  11: begin                                          // Bus.Totalmiles
      Result := 0.0;
      if ActiveCircuit[ActiveActor] <> Nil then
        With ActiveCircuit[ActiveActor] Do
          if ActiveBusIndex > 0 then
             Result := Buses^[ActiveBusIndex].BusTotalMiles  ;
  end;
  12: begin                                          // Bus.latitude read
      Result := 0.0;
       If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
        IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
         IF (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) Then
           Result := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat;
  end;
  13: begin                                          // Bus.Latitude write
      If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
        IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
        Begin
           Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined := TRUE;
           Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat := arg;
        End;
  end;
  14: begin                                          // Bus.latitude read
      Result := 0.0;
       If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
        IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
         IF (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) Then
           Result := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long;
  end;
  15: begin                                          // Bus.Latitude write
      If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
        IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
        Begin
           Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined := TRUE;
           Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long := arg;
        End;
  end
  else
      Result:=-1.0;
  end;
end;

//*****************************String type properties*******************************
function BUSS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;
begin
  Result:=pAnsiChar(AnsiString('0')); //Default return value
  case mode of
  0: begin                                           // Bus.Name read
      Result :=pAnsiChar(AnsiString(''));

     If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
     IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
              Result := pAnsiChar(AnsiString(BusList.Get(ActiveBusIndex)));
  end
  else
      Result:=pAnsiChar(AnsiString('Error, Parameter non recognized'));
  end;
end;

procedure BUSV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

var
  BusReference,
  k,
  LoadCount,
  LineCount,
  Nvalues,
  Norder,
  i,
  iV,
  NodeIdx,
  jj,
  NodeIdxj,
  Nelements,
  j,
  NodeIdxi          : Integer;
  Volts,
  Voc,
  Z,
  Y1,
  Isc               : Complex;
  pBus              : TDSSBus;
  VPh,
  V012              : Array[1..3] of Complex;
  BaseFactor        : Double;
  voltsp            : polar;
  pElem             : TDSSCktElement;
  Zsc012Temp        : TCmatrix;
  pValues           : pDoubleArray;
  myPXEList         : DynStringArray;
  S                 : String;

begin
  case mode of
  0: begin                                           // Bus.Voltages
    myType    :=  3;                  // Complex
    IF ActiveCircuit[ActiveActor] = nil Then
    Begin
      setlength(myCmplxArray,1);
    End
    ELSE With ActiveCircuit[ActiveActor] Do
    Begin
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        setlength(myCmplxArray,0);
        pBus    := Buses^[ActiveBusIndex];
        Nvalues := pBus.NumNodesThisBus;
        jj := 1;
        WITH pBus DO
        FOR i := 1 to  NValues DO
        Begin
          // this code so nodes come out in order from smallest to larges
          Repeat
           NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
           inc(jj)
          Until NodeIdx>0;
          setlength(myCmplxArray,length(myCmplxArray) + 1);
          myCmplxArray[High(myCmplxArray)]  := Solution.NodeV^[GetRef(NodeIdx)];   // referenced to pBus
        End;
      End

    End;
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * length(myCmplxArray);
  end;

  1: begin                                           // Bus.SeqVoltages
    myType    :=  3;
    IF ActiveCircuit[ActiveActor] = nil Then
    Begin
      setlength(myDBLArray,1);
    End
    ELSE With ActiveCircuit[ActiveActor] Do
    Begin
      setlength(myDBLArray,3);
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        Nvalues := Buses^[ActiveBusIndex].NumNodesThisBus;
        IF Nvalues <> 3 THEN
           For i := 1 to 3 DO myDBLArray[ i - 1] := -1.0  // Signify seq voltages n/A for less then 3 phases
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
             myDBLArray[iV - 1] := Cabs(V012[i]);
             Inc(iV);
          End;
        End;
      End;
    End;
    myPointer :=  @(myDBLArray[0]);
    mySize    :=  SizeOf(myDBLArray[0]) * 3;
  end;
  2: begin                                           // Bus.Nodes
    myType  :=  1;
    IF ActiveCircuit[ActiveActor] = nil Then
    Begin
      setlength(myIntArray,1);
    End
    ELSE With ActiveCircuit[ActiveActor] Do
    IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
    Begin
      pBus := Buses^[ActiveBusIndex];
      With pBus Do
      Begin
        setlength(myIntArray,NumNodesThisBus);
        Nvalues := NumNodesThisBus;
        iV := 0;
        jj := 1;
        FOR i := 1 to  NValues DO
        Begin
              // this code so nodes come out in order from smallest to larges
            Repeat
                 NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                 inc(jj)
            Until NodeIdx>0;
           myIntArray[iV] := Buses^[ActiveBusIndex].GetNum(NodeIdx);
           Inc(iV);
        End;
      End;
    End;
    myPointer :=  @(myIntArray[0]);
    mySize    :=  SizeOf(myIntArray[0]) * Length(myIntArray);
  end;
  3: begin                                           // Bus.Voc
    myType  :=  3;
    setlength(myCmplxArray,1);
    IF ActiveCircuit[ActiveActor] <> nil Then
    Begin
      With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        If Buses^[ActiveBusIndex].VBus <> nil Then
        Begin
          NValues := Buses^[ActiveBusIndex].NumNodesThisBus;
          setlength(myCmplxArray, NValues);
          FOR i := 1 to  NValues DO
          Begin
             myCmplxArray[i - 1] := Buses^[ActiveBusIndex].VBus^[i];
          End;
        End;
      End;
    End;
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  4: begin // Bus.Isc
    myType  :=  3;
    setlength(myCmplxArray,1);
    IF ActiveCircuit[ActiveActor] <> nil Then
    Begin
      With ActiveCircuit[ActiveActor] Do
      Begin
        IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
        Begin
          If Buses^[ActiveBusIndex].BusCurrent <> nil Then
          Begin
            NValues := Buses^[ActiveBusIndex].NumNodesThisBus;
            setlength(myCmplxArray,NValues);
            FOR i := 1 to  NValues DO
            Begin
               myCmplxArray[i - 1] := Buses^[ActiveBusIndex].BusCurrent^[i];
            End;
          End;
        End;
      End;
    End;
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  5: begin  // Bus.PuVoltages
    myType  :=  3;
    setlength(myCmplxArray,1);
    IF ActiveCircuit[ActiveActor] <> nil Then
    Begin
      With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        pBus    := Buses^[ActiveBusIndex];
        With pBus Do
        Begin
          Nvalues := NumNodesThisBus;
          setlength(myCmplxArray,NValues);
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
            Until NodeIdx > 0;
            myCmplxArray[i - 1] := cdivreal(Solution.NodeV^[GetRef(NodeIdx)], BaseFactor);
          End;
        End;
      End;
    End;
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  End;
  6: begin  // Bus.ZscMatrix
    myType  :=  3;
    setlength(myCmplxArray,1);
    IF ActiveCircuit[ActiveActor] <> nil Then
    Begin
      Try
      With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        If Assigned(Buses^[ActiveBusIndex].Zsc) Then Begin
          Nelements := Buses^[ActiveBusIndex].Zsc.Order;
          setlength(myCmplxArray, (Nelements * Nelements));
          iV := 0;
          With Buses^[ActiveBusIndex] Do
          For i := 1 to Nelements Do
            For j := 1 to Nelements Do
            Begin
              myCmplxArray[iV] := Zsc.GetElement(i,j);
              inc(iV);
            End;
        End;
      End;
      Except
          On E:Exception Do DoSimpleMsg('ZscMatrix Error: ' + E.message + CRLF , 5016);
      End;
    End;
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  End;
  7: begin  // Bus.Zcs1
    myType  :=  3;
    setlength(myCmplxArray,1);
    IF ActiveCircuit[ActiveActor] <> nil Then
    Begin
      With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        myCmplxArray[0] := Buses^[ActiveBusIndex].Zsc1;
      End
    End;
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  8: begin  // Bus.Zsc0
    myType  :=  3;
    setlength(myCmplxArray,1);
    IF ActiveCircuit[ActiveActor] <> nil Then
    Begin
      With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        myCmplxArray[0] := Buses^[ActiveBusIndex].Zsc0;
      End;
    End;
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  9: begin   // Bus.YscMatrix
    myType  :=  3;
    setlength(myCmplxArray,1);
    IF ActiveCircuit[ActiveActor] <> nil Then
    Begin
      Try
      With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        If Assigned(Buses^[ActiveBusIndex].Ysc) Then Begin
          Nelements := Buses^[ActiveBusIndex].Ysc.Order;
          setlength(myCmplxArray, ( Nelements * Nelements));
          iV := 0;
          With Buses^[ActiveBusIndex] Do
          For i := 1 to Nelements Do
            For j := 1 to Nelements Do  Begin
              myCmplxArray[iV] := Ysc.GetElement(i,j);
              Inc(iV);
            End;
        End;
      End;
      Except
          On E:Exception Do DoSimpleMsg('ZscMatrix Error: ' + E.message + CRLF, 5017 );
      End;
    End;
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  10: begin  // Bus.CplxSeqVoltages
    myType  :=  3;
    setlength(myCmplxArray,1);
    IF ActiveCircuit[ActiveActor] <> nil Then
    Begin
      With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        Nvalues := Buses^[ActiveBusIndex].NumNodesThisBus;
        setlength(myCmplxArray,3);
        IF Nvalues <> 3 THEN
            For i := 0 to 2 DO myCmplxArray[i] := cmplx(-1.0,-1.0)  // Signify seq voltages n/A for less then 3 phases
        ELSE
        Begin
            iV := 0;
            FOR i := 1 to 3 DO Vph[i] := Solution.NodeV^[Buses^[ActiveBusIndex].Find(i)];
            Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components
            For i := 1 to 3 DO  // Stuff it in the result
            Begin
               myCmplxArray[iV] := V012[i];
               Inc(iV);
            End;
        End;
      End;
    End;
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  11: begin  // Bus.VLL
    myType  :=  3;
    setlength(myCmplxArray,1);
    IF ActiveCircuit[ActiveActor] <> nil Then
    Begin
      With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        pBus    := Buses^[ActiveBusIndex];
        Nvalues := pBus.NumNodesThisBus;
        jj      :=  1;
        If Nvalues > 3 Then Nvalues := 3;
        If Nvalues > 1 Then
        Begin
            If Nvalues = 2 Then  Nvalues := 1;  // only one L-L voltage if 2 phase
            setlength(myCmplxArray, ( 2 * NValues ) - 1);
            iV := 0;
            WITH pBus DO
            Begin
              If kVBase>0.0 Then BaseFactor := 1000.0*kVBase*sqrt3
                            Else BaseFactor := 1.0;
              FOR i := 1 to  NValues DO     // for 2- or 3-phases
              Begin
                    // this code assumes the nodes are ordered 1, 2, 3
      //------------------------------------------------------------------------------------------------
      // This section was added to prevent measuring using disconnected nodes, for example, if the
      // bus has 2 nodes but those are 1 and 3, that will bring a problem.
                    jj      :=  i;
                    Repeat
                      NodeIdxi := FindIdx(jj);  // Get the index of the Node that matches i
                      inc(jj);
                    Until NodeIdxi > 0;

                    Repeat
                      NodeIdxj := FindIdx(jj);  // Get the index of the Node that matches i
                      if jj > 3 then jj := 1
                      else inc(jj);
                    Until NodeIdxj > 0;
      //------------------------------------------------------------------------------------------------
      //                      if jj>3 then jj := 1; // wrap around
      //                      NodeIdxj := FindIdx(jj);
                    With Solution Do Volts := Csub(NodeV^[GetRef(NodeIdxi)], NodeV^[GetRef(NodeIdxj)]);
                    myCmplxArray[iV] := Volts;
                    Inc(iV);
              End;
            End;  {With pBus}
        End
        ELSE Begin  // for 1-phase buses, do not attempt to compute.
            setlength(myCmplxArray,1);  // just return -1's in array
            myCmplxArray[0] := cmplx(-99999.0, 0);
        End;
      End;
    End;
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  12: begin   // Bus. PuVLL
    myType  :=  3;
    setlength(myCmplxArray,1);
    IF ActiveCircuit[ActiveActor] <> nil Then
    Begin
      With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        pBus    := Buses^[ActiveBusIndex];
        Nvalues := pBus.NumNodesThisBus;
        If Nvalues > 3 Then Nvalues := 3;
        If Nvalues > 1 Then
        Begin
            If Nvalues = 2 Then  Nvalues := 1;  // only one L-L voltage if 2 phase
            setlength(myCmplxArray, ( 2 * NValues ) - 1);
            iV := 0;
            WITH pBus DO
            Begin
              If kVBase>0.0 Then BaseFactor := 1000.0*kVBase*sqrt3
                            Else BaseFactor := 1.0;
              FOR i := 1 to  NValues DO     // for 2- or 3-phases
              Begin
                    // this code assumes the nodes are ordered 1, 2, 3
      //------------------------------------------------------------------------------------------------
      // This section was added to prevent measuring using disconnected nodes, for example, if the
      // bus has 2 nodes but those are 1 and 3, that will bring a problem.
                    jj      :=  i;
                    Repeat
                      NodeIdxi := FindIdx(jj);  // Get the index of the Node that matches i
                      inc(jj);
                    Until NodeIdxi > 0;

                    Repeat
                      NodeIdxj := FindIdx(jj);  // Get the index of the Node that matches i
                      if jj > 3 then jj := 1
                      else inc(jj);
                    Until NodeIdxj > 0;
      //------------------------------------------------------------------------------------------------
      //                      if jj>3 then jj := 1; // wrap around
      //                      NodeIdxj := FindIdx(jj);
                    With Solution Do Volts := Csub(NodeV^[GetRef(NodeIdxi)], NodeV^[GetRef(NodeIdxj)]);
                    myCmplxArray[iV] := cdivreal( Volts, BaseFactor );
                    Inc(iV);
              End;
            End;  {With pBus}
        End
        ELSE Begin  // for 1-phase buses, do not attempt to compute.
          setlength(myCmplxArray, 1);  // just return -1's in array
          myCmplxArray[0] := cmplx( -99999.0, 0 );
        End;
      End;
    End;
    myPointer :=  @(myCmplxArray[0]);
    mySize    :=  SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
  end;
  13: begin  // Bus.VMagAngle
    myType  :=  3;
    setlength(myPolarArray,1);
    IF ActiveCircuit[ActiveActor] <> nil Then
    Begin
      With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        pBus    := Buses^[ActiveBusIndex];
        Nvalues := pBus.NumNodesThisBus;
        setlength(myPolarArray, ( 2 * NValues ) - 1);
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
              Voltsp      := ctopolardeg(Solution.NodeV^[GetRef(NodeIdx)]);  // referenced to pBus
              myPolarArray[iV] := Voltsp;
              Inc(iV);
        End;
      End
    End;
    myPointer :=  @(myPolarArray[0]);
    mySize    :=  SizeOf(myPolarArray[0]) * Length(myCmplxArray);
  end;
  14: begin   // Bus.PuVMagAngle
    myType  :=  3;
    setlength(myPolarArray,1);
    IF ActiveCircuit[ActiveActor] <> nil Then
    Begin
      With ActiveCircuit[ActiveActor] Do
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        pBus    := Buses^[ActiveBusIndex];
        Nvalues := pBus.NumNodesThisBus;
        setlength( myPolarArray, ( 2 * NValues ) - 1 );
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
            myPolarArray[iV]     := ctopolardeg(Solution.NodeV^[GetRef(NodeIdx)]);  // referenced to pBus
            myPolarArray[iV].mag := myPolarArray[iV].mag / BaseFactor;
            Inc(iV);
          End;
        End;
      End;
    End;
    myPointer :=  @(myPolarArray[0]);
    mySize    :=  SizeOf(myPolarArray[0]) * Length(myCmplxArray);
  end;
  15: begin   // Bus.LineList
    myType  :=  4;        // String
    setlength(myStrArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        BusReference := ActiveBusIndex;
       { Count number of Lines connected to this bus }
        LineCount := 0;
        pElem:=TDSSCktElement(Lines.First);
        while Assigned(pElem) do
        Begin
          if CheckBusReference(pElem, BusReference, j) then Inc(LineCount);
          pElem := TDSSCktElement(Lines.Next);
        End;
        if LineCount > 0 then
        Begin
       // Allocate Array
          setlength(myStrArray, 0);
          pElem:=TDSSCktElement(Lines.First);
          while Assigned(pElem) do
          Begin
            if CheckBusReference(pElem, BusReference, j) then
            Begin
             S :=  'LINE.' + pElem.name;
             for i := 1 to High(S) do
             Begin
               setlength(myStrArray, length(myStrArray) + 1);
               myStrArray[High(myStrArray)] :=  Byte(S[i]);
             End;
             setlength(myStrArray, length(myStrArray) + 1);
             myStrArray[High(myStrArray)] :=  Byte(0);
            End;
            pElem := TDSSCktElement(Lines.Next);
          End;
        End;
      End;
    End;
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  16: begin   // Bus.LoadList
    myType  :=  4;        // String
    setlength(myStrArray, 1);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        BusReference := ActiveBusIndex;
       { Count number of LOAD elements connected to this bus }
        LoadCount := 0;
        pElem:=TDSSCktElement(Loads.First);
        while Assigned(pElem) do
        Begin
           if CheckBusReference(pElem, BusReference, j) then Inc(LoadCount);
           pElem := TDSSCktElement(Loads.Next);
        End;

        if LoadCount > 0 then
        Begin
       // Allocate Array
          setlength(myStrArray, 0);
          pElem:=TDSSCktElement(Loads.First);
          While Assigned(pElem) do
          Begin
             if CheckBusReference(pElem, BusReference, j) then
             Begin
               S :=  'LOAD.' + pElem.name;
               for i := 1 to High(S) do
               Begin
                 setlength(myStrArray, length(myStrArray) + 1);
                 myStrArray[High(myStrArray)] :=  Byte(S[i]);
               End;
               setlength(myStrArray, length(myStrArray) + 1);
               myStrArray[High(myStrArray)] :=  Byte(0);
             End;
             pElem := TDSSCktElement(Loads.Next);
          End;
        End;
      End;
    End;
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  17: begin   // Bus.ZSC012Matrix
    myType := 2;          // Double
    setlength(myDBLArray, 1);
    IF ActiveCircuit[ActiveActor] <> nil Then
    Begin
      With ActiveCircuit[ActiveActor] Do
      Begin
       pBus    := Buses^[ActiveBusIndex];
       With pBus Do
        Begin
          If NumNodesThisBus = 3 Then
           Begin
            Nvalues := SQR(NumNodesThisBus) * 2;  // Should be 9 complex numbers
            setlength(myDBLArray, NValues);
            // Compute ZSC012 for 3-phase buses else leave it zeros
            // ZSC012 = Ap2s Zsc As2p
            Zsc012Temp:= Zsc.MtrxMult(As2p);  // temp for intermediate result
            if Assigned(ZSC012) then ZSC012.Free;
            ZSC012 := Ap2s.MtrxMult(Zsc012Temp);
            // Cleanup
            Zsc012Temp.Free;

            {Return all the elements of ZSC012}
            k := 0;
            pValues := pDoubleArray(ZSC012.GetValuesArrayPtr(Norder));
            For i := 1 to Nvalues do Begin
              myDBLArray[k] := pValues^[i];
              Inc(k);
            End;
           End
        End;
      End;
    End;
    myPointer :=  @(myDBLArray[0]);
    mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
  end;
  18: begin   // Bus.AllPCEatBus
    myType  :=  4;        // String
    setlength(myStrArray, 1);
    If (ActiveCircuit[ActiveActor] <> Nil) Then
    Begin
      With ActiveCircuit[ActiveActor] Do
      Begin
        IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
        Begin
          myPXEList   :=  getPCEatBus(BusList.Get(ActiveBusIndex));
          setlength(myStrArray, 0);
          for i := 0 to High(myPXEList) do
          Begin
            if myPXEList[i] <> '' then
            Begin
              for j := 1 to High(myPXEList[i]) do
              Begin
                setlength(myStrArray, length(myStrArray) + 1);
                myStrArray[High(myStrArray)]  :=  Byte(myPXEList[i][j]);
              End;
              setlength(myStrArray, length(myStrArray) + 1);
              myStrArray[High(myStrArray)]  :=  Byte(0);
            End;
          End;
        End;
      End;
    End;
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  19: begin   // Bus.AllPDEatBus
    myType  :=  4;        // String
    setlength(myStrArray, 1);
    If (ActiveCircuit[ActiveActor] <> Nil) Then
    Begin
      With ActiveCircuit[ActiveActor] Do
      Begin
        IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
        Begin
          myPXEList   :=  getPDEatBus(BusList.Get(ActiveBusIndex));
          setlength(myStrArray, 0);
          for i := 0 to High(myPXEList) do
          Begin
            if myPXEList[i] <> '' then
            Begin
              for j := 1 to High(myPXEList[i]) do
              Begin
                setlength(myStrArray, length(myStrArray) + 1);
                myStrArray[High(myStrArray)]  :=  Byte(myPXEList[i][j]);
              End;
              setlength(myStrArray, length(myStrArray) + 1);
              myStrArray[High(myStrArray)]  :=  Byte(0);
            End;
          End;
        End;
      End;
    End;
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  else
    myType  :=  4;        // String
    S       :=  'Command not recognized';
    setlength(myStrArray, 0);
    for j := 1 to High(myPXEList[i]) do
    Begin
      setlength(myStrArray, length(myStrArray) + 1);
      myStrArray[High(myStrArray)]  :=  Byte(S[j]);
    End;
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
end;

end.
