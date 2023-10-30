unit DFuses;

interface

function FusesI(mode:longint;arg:longint):longint;cdecl;
function FusesF(mode:longint;arg:double):double;cdecl;
function FusesS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure FusesV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses {$IFNDEF FPC_DLL}ComServ, {$ENDIF}Executive, Sysutils, ControlElem, Fuse, Pointerlist, DSSGlobals, Variants;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit[ActiveActor]) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('Fuse.%s.%s=%s', [TFuseObj(FuseClass.GetActiveObj).Name, parm, val]);
  DSSExecutive[ActiveActor].Command := cmd;
end;

function FusesI(mode:longint;arg:longint):longint;cdecl;

Var
   pElem : TFuseObj;
   elem: TFuseObj;
   i : Integer;
   pFuse:TFuseObj;

begin
  Result:=0;
  case mode of
  0: begin  // Fuses.Count
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
        Result := FuseClass.ElementList.ListSize;
  end;
  1: begin  // Fuses.First
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := FuseClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := FuseClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
  end;
  2: begin  // Fuses.Next
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := FuseClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := FuseClass.ElementList.ActiveIndex;
          End
          Else pElem := FuseClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
  end;
  3: begin  // Fuse.MonitoredTerm read
      Result := 0;
      elem := FuseClass.GetActiveObj  ;
      if elem <> nil then Result := elem.MonitoredElementTerminal ;
  end;
  4: begin  // Fuse.MonitoredTerm write
    elem := FuseClass.GetActiveObj ;
    if elem <> nil then Set_parameter('monitoredterm', IntToStr(arg));
  end;
  5: begin  // Fuse.SwitchedTerm read
      Result := 0;
      elem := FuseClass.GetActiveObj  ;
      if elem <> nil then Result := elem.ElementTerminal;
  end;
  6: begin  // Fuse.SwitchedTerm write
    elem := FuseClass.GetActiveObj ;
    if elem <> nil then Set_parameter('switchedterm', IntToStr(arg));
  end;
  7: begin  // Fuse.Open
    pFuse := FuseClass.GetActiveObj ;
    if pFuse <> nil then begin
      for i := 1 to pFuse.ControlledElement.NPhases do pFuse.States[i] := CTRL_OPEN // Open all phases
    end;
  end;
  8: begin  // Fuse.Close
    pFuse := FuseClass.GetActiveObj ;
    if pFuse <> nil then begin
      for i := 1 to pFuse.ControlledElement.NPhases do pFuse.States[i] := CTRL_CLOSE // Close all phases
    end;
  end;
  9: begin  // Fuse.IsBlown
      Result :=0;
      elem := FuseClass.GetActiveObj ;
      if elem <> nil then Begin
          for i := 1 to elem.nphases do
              If not elem.ControlledElement.Closed[i,ActiveActor] Then Result := 1;
      End;
  end;
  10: begin  // Fuse.Idx read
    if ActiveCircuit[ActiveActor] <> Nil then
       Result := FuseClass.ElementList.ActiveIndex
    else Result := 0;
  end;
  11: begin  // Fuse.Idx write
    if ActiveCircuit[ActiveActor] <> Nil then   Begin
        pFuse := FuseClass.Elementlist.Get(arg);
        If pFuse <> Nil Then ActiveCircuit[ActiveActor].ActiveCktElement := pFuse;
    End;
  end;
  12: begin  // Fuse.NumPhases
      Result := 0;
      if ActiveCircuit[ActiveActor] <> Nil then   Begin
          pFuse := FuseClass.GetActiveObj ;
          If pFuse <> Nil Then Result := pFuse.NPhases ;
      End;
  end;
  13: begin  // Fuse.Reset
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pFuse := FuseClass.GetActiveObj;
        If pFuse <> Nil Then pFuse.Reset(ActiveActor);
     End;
  end
  else
      Result:=-1;
  end;
end;

//******************************Floating point type properties********************
function FusesF(mode:longint;arg:double):double;cdecl;

Var
  elem: TFuseObj;

begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // Fuses.RatedCurrent read
    elem := FuseClass.GetActiveObj ;
    if elem <> nil then Result := elem.RatedCurrent
    else Result := -1.0;
  end;
  1: begin  // Fuses.RatedCurrent write
    elem := FuseClass.GetActiveObj ;
    if elem <> nil then Set_parameter('RatedCurrent', Format('%.8g ',[arg]));
  end ;
  2: begin  // Fuses.Delay read
    elem := FuseClass.GetActiveObj ;
    if elem <> nil then Result := elem.DelayTime
    else Result := -1.0;
  end;
  3: begin  // Fuses.Delay write
    elem := FuseClass.GetActiveObj ;
    if elem <> nil then Set_parameter('Delay', Format('%.8g ',[arg]));
  end
  else
      Result:=-1.0;
  end;
end;

//******************************String type properties********************
function FusesS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;

Var
  elem: TFuseObj;

begin
  Result := pAnsiChar(AnsiString('')); // Default return value
  case mode of
  0: begin  // Fuses.Name read
      Result := pAnsiChar(AnsiString(''));
      elem := FuseClass.GetActiveObj;
      If elem <> Nil Then Result := pAnsiChar(AnsiString(elem.Name));
  end;
  1: begin  // Fuses.Name write
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          If FuseClass.SetActive(string(arg)) Then
          Begin
               ActiveCircuit[ActiveActor].ActiveCktElement := FuseClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Fuse "'+ arg +'" Not Found in Active Circuit.', 77003);
          End;
     End;
  end;
  2: begin  // Fuses. MonitoredObj read
      Result := pAnsiChar(AnsiString(''));
      elem := FuseClass.GetActiveObj  ;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.MonitoredElementName));
  end;
  3: begin  // Fuses. MonitoredObj write
      elem := FuseClass.GetActiveObj ;
      if elem <> nil then Set_parameter('monitoredObj', string(arg));
  end;
  4: begin  // Fuses.SwitchedObj read
      Result := pAnsiChar(AnsiString(''));
      elem := FuseClass.ElementList.Active ;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.ElementName));
  end;
  5: begin  // Fuses.SwitchedObj write
      elem := FuseClass.GetActiveObj ;
      if elem <> nil then Set_parameter('SwitchedObj', string(arg));
  end;
  6: begin  // Fuses.TCCcurve read
      elem := FuseClass.GetActiveObj ;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.FuseCurve.Name))
      else Result := pAnsiChar(AnsiString('No Fuse Active!'));
  end;
  7: begin  // Fuses.TCCcurve write
      elem := FuseClass.GetActiveObj ;
      if elem <> nil then Set_parameter('FuseCurve', string(arg));
  end
  else
      Result:= pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//******************************Variant type properties********************
procedure FusesV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

Var
  elem        : TFuseObj;
  pList       : TPointerList;
  k,
  i           : Integer;
  S           : String;

begin
  case mode of
  0: begin  // Fuses.AllName
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      If FuseClass.ElementList.ListSize > 0 then
      Begin
        pList := FuseClass.ElementList;
        elem := pList.First;
        WHILE elem<>Nil DO Begin
            WriteStr2Array(elem.Name);
            WriteStr2Array(Char(0));
            elem := pList.next;
        End;
      End;
    End
    Else  WriteStr2Array('');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  1: begin  // Fuses.States read
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      Elem := FuseClass.GetActiveObj;
      If Elem <> nil Then
      Begin
        for i:= 1 to elem.ControlledElement.Nphases DO
        Begin
          if elem.States[i] = CTRL_CLOSE then
            WriteStr2Array('closed')
          else
            WriteStr2Array('open');
          WriteStr2Array(Char(0));
        End;
      End;
    End
    Else  WriteStr2Array('');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  2: begin  // Fuses.States write
    myType  :=  4;          // String
    k := 0;
    elem := FuseClass.GetActiveObj;
    If elem <> nil Then
    Begin

      for i := 1 to elem.ControlledElement.NPhases do
      Begin
        S := BArray2Str(myPointer, k);
        if S = '' then
          break
        else
        Begin
           case LowerCase(S)[1] of
            'o': elem.States[i] := CTRL_OPEN;
            'c': elem.States[i] := CTRL_CLOSE;
          end;
        End;
      End;
    End;
    mySize  :=  k;
  end;
  3: begin  // Fuses.NormalStates read
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      Elem := FuseClass.GetActiveObj;
      If Elem <> nil Then
      Begin
        for i:= 1 to elem.ControlledElement.Nphases DO Begin
          if elem.NormalStates[i] = CTRL_CLOSE then
            WriteStr2Array('closed')
          else
            WriteStr2Array('open');
          WriteStr2Array(Char(0));
        End;
      End;
    End
    Else  WriteStr2Array('');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  4: begin  // Fuses.NormalStates write
    elem := FuseClass.GetActiveObj;
    k := 0;
    If elem <> nil Then
    Begin
      // allocate space based on number of phases of controlled device
      for i := 1 to elem.ControlledElement.NPhases do
      Begin
        S := BArray2Str(myPointer, k);
        if S = '' then
          break
        else
        Begin
          case LowerCase(S)[1] of
          'o': elem.NormalStates[i] := CTRL_OPEN;
          'c': elem.NormalStates[i] := CTRL_CLOSE;
          end;
        End;
      End;
    End;
    mySize  :=  k;
  end
  else
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    WriteStr2Array('Error, parameter not recognized');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
end;

end.
