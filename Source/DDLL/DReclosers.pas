unit DReclosers;

interface

function ReclosersI(mode:longint;arg:longint):longint;cdecl;
function ReclosersF(mode:longint;arg:double):double;cdecl;
function ReclosersS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure ReclosersV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses Executive, Sysutils, ControlElem, Recloser, PointerList, Variants, DSSGlobals, DSSClassDefs;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit[ActiveActor]) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('recloser.%s.%s=%s', [TRecloserObj(RecloserClass.GetActiveObj).Name, parm, val]);
  DSSExecutive[ActiveActor].Command := cmd;
end;

function ReclosersI(mode:longint;arg:longint):longint;cdecl;

Var
   pElem : TRecloserObj;
   elem: TRecloserObj;
   pRecloser:TRecloserObj;

begin
  Result:=0; // Default return value
  case mode of
  0: begin  // Reclosers.Count
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
        Result := RecloserClass.ElementList.ListSize;
  end;
  1: begin  // Reclosers.First
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := RecloserClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := RecloserClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
  end;
  2: begin  // Reclosers.Next
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := RecloserClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := RecloserClass.ElementList.ActiveIndex;
          End
          Else pElem := RecloserClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
  end;
  3: begin  // Reclosers.MonitoredTerm read
      Result := 0;
      elem := RecloserClass.GetActiveObj  ;
      if elem <> nil then Result := elem.MonitoredElementTerminal ;
  end;
  4: begin  // Reclosers.MonitoredTerm write
      elem := RecloserClass.GetActiveObj ;
      if elem <> nil then Set_parameter('monitoredterm', IntToStr(arg));
  end;
  5: begin  // Reclosers.SwitchedTerm read
      Result := 0;
      elem := RecloserClass.GetActiveObj  ;
      if elem <> nil then Result := elem.ElementTerminal  ;
  end;
  6: begin  // Reclosers.SwitchedTerm write
      elem := RecloserClass.GetActiveObj ;
      if elem <> nil then Set_parameter('SwitchedTerm', IntToStr(arg));
  end;
  7: begin  // Reclosers.NumFast read
      Result := 0;
      elem := RecloserClass.ElementList.Active;  ;
      if elem <> nil then Result := elem.NumFast ;
  end;
  8: begin  // Reclosers.NumFast write
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then Set_parameter('numfast', IntToStr(arg));
  end;
  9: begin  // Reclosers.Shots read
      Result := 0;
      elem := RecloserClass.ElementList.Active;  ;
      if elem <> nil then Result := elem.NumReclose + 1;
  end;
  10: begin  // Reclosers.Shots write
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then Set_parameter('shots', IntToStr(arg));
  end;
  11: begin  // Recloser.Open                                     // TODO
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then elem.PresentState := CTRL_OPEN;
  end;
  12: begin  // Reclosers.Close                                  // TODO
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then elem.PresentState := CTRL_CLOSE;
  end;
  13: begin // Reclosers.Idx read
      if ActiveCircuit[ActiveActor] <> Nil then
         Result := RecloserClass.ElementList.ActiveIndex
      else Result := 0;
  end;
  14: begin // Reclosers.Idx write
      if ActiveCircuit[ActiveActor] <> Nil then   Begin
          pRecloser := RecloserClass.Elementlist.Get(arg);
          If pRecloser <> Nil Then ActiveCircuit[ActiveActor].ActiveCktElement := pRecloser;
      End;
  end;
  15: begin  // Reclosers.Reset                                  // TODO
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then elem.Reset(ActiveActor);
      end
  else
      Result:=-1;
  end;
end;

//********************Floating point type properties******************************
function ReclosersF(mode:longint;arg:double):double;cdecl;

var
  elem: TRecloserObj;

begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // Reclosers.PhaseTrip read
      Result := 0;
      elem := RecloserClass.ElementList.Active;
      if elem <> nil then Result := elem.PhaseTrip;
  end;
  1: begin  // Reclosers.PhaseTrip write
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then Set_parameter('PhaseTrip', Format('%.g',[arg]));
  end;
  2: begin  // Reclosers.PhaseInst read
      Result := 0;
      elem := RecloserClass.ElementList.Active;
      if elem <> nil then Result := elem.PhaseInst;
  end;
  3: begin  // Reclosers.PhaseInst write
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then Set_parameter('Phaseinst', Format('%.g',[arg]));
  end;
  4: begin  // Reclosers.GroundTrip read
      Result := 0;
      elem := RecloserClass.ElementList.Active;
      if elem <> nil then Result := elem.GroundTrip;
  end;
  5: begin  // Reclosers.GroundTrip write
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then Set_parameter('GroundTrip', Format('%.g',[arg]));
  end;
  6: begin  // Reclosers.GroundInst read
      Result := 0;
      elem := RecloserClass.ElementList.Active;
      if elem <> nil then Result := elem.GroundInst;
  end;
  7: begin  // Reclosers.GroundInst write
      elem := RecloserClass.ElementList.Active  ;
      if elem <> nil then Set_parameter('GroundInst', Format('%.g',[arg]));
  end
  else
      Result:=-1.0;
  end;
end;

//********************String type properties******************************
function ReclosersS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;

Var
  elem: TRecloserObj;

begin
  Result := pAnsiChar(AnsiString('')); // Default return value
  case mode of
  0: begin  // Reclosers.Name read
      Result := pAnsiChar(AnsiString(''));
      elem := RecloserClass.GetActiveObj;
      If elem <> Nil Then Result := pAnsiChar(AnsiString(elem.Name));
  end;
  1: begin  // Reclosers.Name write
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          If RecloserClass.SetActive(arg) Then
          Begin
               ActiveCircuit[ActiveActor].ActiveCktElement := RecloserClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Recloser "'+ arg +'" Not Found in Active Circuit.', 77003);
          End;
     End;
  end;
  2: begin  // Reclosers.MonitoredObj read
      Result := pAnsiChar(AnsiString(''));
      elem := RecloserClass.GetActiveObj  ;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.MonitoredElementName));
  end;
  3: begin  // Reclosers.MonitoredObj write
      elem := RecloserClass.GetActiveObj ;
      if elem <> nil then Set_parameter('monitoredObj', arg);
  end;
  4: begin  // Reclosers.SwitchedObj read
      Result := pAnsiChar(AnsiString(''));
      elem := RecloserClass.ElementList.Active ;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.ElementName));
  end;
  5: begin  // Reclosers.SwitchedObj write
      elem := RecloserClass.GetActiveObj ;
      if elem <> nil then Set_parameter('SwitchedObj', arg);
  end;
  6: begin  // Reclosers.State read                                          // TODO
      Result := pAnsiChar(AnsiString(''));
      elem := RecloserClass.GetActiveObj ;
      if elem <> nil then Begin
        if elem.PresentState = CTRL_CLOSE then Result := pAnsiChar(AnsiString('closed'))
        else  Result := pAnsiChar(AnsiString('open'));
      End;

  end;
  7: begin  // Reclosers.State write                                         // TODO
      elem := RecloserClass.GetActiveObj ;
      if elem <> nil then Begin
        if LowerCase(arg)[1] = 'c' then elem.PresentState := CTRL_CLOSE
        else elem.PresentState := CTRL_OPEN;
      End;
  end;
  8: begin  // Reclosers.Normal read                                         // TODO
      Result := pAnsiChar(AnsiString(''));
      elem := RecloserClass.GetActiveObj ;
      if elem <> nil then Begin
        if elem.NormalState = CTRL_CLOSE then Result := pAnsiChar(AnsiString('closed'))
        else  Result := pAnsiChar(AnsiString('open'));
      End;
  end;
  9: begin  // Reclosers.Normal write                                        // TODO
      elem := RecloserClass.GetActiveObj ;
      if elem <> nil then Begin
        if LowerCase(arg)[1] = 'c' then elem.NormalState := CTRL_CLOSE
        else elem.NormalState := CTRL_OPEN;
      End;
  end;
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//********************Variant type properties******************************
procedure ReclosersV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

Var
  elem: TRecloserObj;
  pList: TPointerList;
  k, i: Integer;

begin
  case mode of
  0:begin  // Reclosers.AllNames
      myType  :=  4;        // String
      setlength(myStrArray,0);
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        If RecloserClass.ElementList.ListSize > 0 then
        Begin
          pList := RecloserClass.ElementList;
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
  1:begin  // Reclosers.RecloseIntervals
      myType  :=  2;        // Double
      setlength(myDBLArray, 1);
      myDBLArray[0] := 0;
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        elem := RecloserClass.ElementList.Active;
        If elem <> Nil Then
        Begin
          setlength(myDBLArray, elem.NumReclose);
          k:=0;
          for i := 1 to elem.NumReclose  do
          Begin
              myDBLArray[k] := elem.RecloseIntervals ^[i];
              Inc(k);
          End;
        End;
      End;
      myPointer :=  @(myDBLArray[0]);
      mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    end
  else
    Begin
      myType  :=  4;        // String
      setlength(myStrArray, 0);
      WriteStr2Array('Error, parameter not recognized');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    End;
  end;
end;

end.
