unit DVSources;

interface

function VsourcesI(mode:longint;arg:longint):Longint;cdecl;
function VsourcesF(mode:longint;arg:double):double;cdecl;
function VsourcesS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure VsourcesV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses {$IFNDEF FPC_DLL}ComServ, {$ENDIF}Vsource, Variants, PointerList, DSSGlobals, CktElement;

function VsourcesI(mode:longint;arg:longint):Longint;cdecl;

Var
   pElem : TVsourceObj;
   elem: TVsourceObj;

begin
  Result:=0; // Default return value
  case mode of
  0: begin  // Vsource.Count
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
        Result := VsourceClass[ActiveActor].ElementList.ListSize;
  end;
  1: begin  // Vsource.First
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := VsourceClass[ActiveActor].ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := VsourceClass[ActiveActor].ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
  end;
  2: begin  // Vsource.Next
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := VsourceClass[ActiveActor].ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := VsourceClass[ActiveActor].ElementList.ActiveIndex;
          End
          Else pElem := VsourceClass[ActiveActor].ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
  end;
  3: begin   // Vsource.Phases read
      Result := 0;
      elem := VsourceClass[ActiveActor].ElementList.Active ;
      if elem <> nil then Result := elem.NPhases ;
  end;
  4: begin  // Vsource.Phases write
      elem := VsourceClass[ActiveActor].GetActiveObj ;
      if elem <> nil then elem.Nphases := arg;
  end
  else
      Result:=-1;
  end;
end;

//***************************Floating point type properties*******************************
function VsourcesF(mode:longint;arg:double):double;cdecl;

var
  elem: TVsourceObj;

begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // Vsources.basekV read
    Result := 0.0;
    elem := VsourceClass[ActiveActor].ElementList.Active ;
    if elem <> nil then Result := elem.kVBase ;
  end;
  1: begin  // Vsources.basekV write
    elem := VsourceClass[ActiveActor].GetActiveObj ;
    if elem <> nil then elem.kVBase := arg;
  end;
  2: begin  // Vsource.pu read
    Result := 0.0;
    elem := VsourceClass[ActiveActor].ElementList.Active ;
    if elem <> nil then Result := elem.perunit ;
  end;
  3: begin  // Vsource.pu write
      elem := VsourceClass[ActiveActor].GetActiveObj ;
      if elem <> nil then elem.PerUnit := arg;
  end;
  4: begin  // Vsource.Angledeg read
      Result := 0.0;
      elem := VsourceClass[ActiveActor].ElementList.Active ;
      if elem <> nil then Result := elem.angle ;
  end;
  5: begin  // Vsource.Angledeg write
      elem := VsourceClass[ActiveActor].GetActiveObj ;
      if elem <> nil then elem.Angle := arg;
  end;
  6: begin  // Vsource.Frequency read
      Result := 0.0;
      elem := VsourceClass[ActiveActor].ElementList.Active ;
      if elem <> nil then Result := elem.SrcFrequency  ;
  end;
  7: begin  // Vsource.Frequency write
      elem := VsourceClass[ActiveActor].GetActiveObj ;
      if elem <> nil then elem.SrcFrequency := arg;
  end
  else
      Result:=-1.0;
  end;
end;

//***************************String type properties*******************************
function VsourcesS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;

Var
   elem: TDSSCktElement;

begin
  Result := pAnsiChar(AnsiString(''));    // Default return value
  case mode of
  0: begin  // Vsources.Name read
    Result := pAnsiChar(AnsiString(''));
    elem := ActiveCircuit[ActiveActor].ActiveCktElement;
    If elem <> Nil Then Result := pAnsiChar(AnsiString(elem.Name));
  end;
  1: begin  // Vsources.Name write
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          If VsourceClass[ActiveActor].SetActive(string(arg)) Then
          Begin
               ActiveCircuit[ActiveActor].ActiveCktElement := VsourceClass[ActiveActor].ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Vsource "'+ arg +'" Not Found in Active Circuit.', 77003);
          End;
     End;
  end
  else
      Result:= pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//***************************Variant type properties*******************************
procedure VsourcesV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

Var
  elem  : TVsourceObj;
  pList : TPointerList;

begin
  case mode of
  0:begin  // VSources.AllNames
      myType  :=  4;        // String
      setlength(myStrArray,0);
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        If VsourceClass[ActiveActor].ElementList.ListSize > 0 then
        Begin
          pList := VsourceClass[ActiveActor].ElementList;
          elem := pList.First;
          WHILE elem<>Nil DO Begin
            WriteStr2Array(elem.Name);
            WriteStr2Array(Char(0));
            elem := pList.next ;
          End;
        End;
      End;
      if (length(myStrArray) = 0) then
        WriteStr2Array('None');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
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
