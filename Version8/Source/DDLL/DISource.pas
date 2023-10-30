unit DISource;

interface

function IsourceI(mode:longint;arg:longint):Longint;cdecl;
function IsourceF(mode:longint;arg:double):double;cdecl;
function IsourceS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure IsourceV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses {$IFNDEF FPC_DLL}ComServ, {$ENDIF}Variants, PointerList, Isource, DSSGlobals, CktElement;

function IsourceI(mode:longint;arg:longint):Longint;cdecl;

Var
   pElem : TIsourceObj;

begin
  Result:=0; // Default return value
  case mode of
  0: begin  // Isources.Count
     If ActiveCircuit[ActiveActor] <> Nil Then
        Result := IsourceClass[ActiveActor].ElementList.ListSize;
  end;
  1: begin  // Isources.First
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := IsourceClass[ActiveActor].ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := IsourceClass[ActiveActor].ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
  end;
  2: begin  // Isources.Next
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := IsourceClass[ActiveActor].ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := IsourceClass[ActiveActor].ElementList.ActiveIndex;
          End
          Else pElem := IsourceClass[ActiveActor].ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
  end
  else
      Result:=-1;
  end;
end;

//***************************Floating point type properties*******************************
function IsourceF(mode:longint;arg:double):double;cdecl;

Var
   elem: TIsourceObj;

begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // Isources.Amps read
      Result := 0.0;
      elem := IsourceClass[ActiveActor].ElementList.Active ;
      if elem <> nil then Result := elem.Amps   ;
  end;
  1: begin  // Isources.Amps write
      elem := IsourceClass[ActiveActor].GetActiveObj ;
      if elem <> nil then elem.Amps := arg;
  end;
  2: begin  // Isources.AngleDeg read
      Result := 0.0;
      elem := IsourceClass[ActiveActor].ElementList.Active ;
      if elem <> nil then Result := elem.Angle ;
  end;
  3: begin  // Isources.AngleDeg write
      elem := IsourceClass[ActiveActor].GetActiveObj ;
      if elem <> nil then elem.Angle := arg;
  end;
  4: begin  // Isources.Frequency read
      Result := 0.0;
      elem := IsourceClass[ActiveActor].ElementList.Active ;
      if elem <> nil then Result := elem.SrcFrequency  ;
  end;
  5: begin  // Isources.Frequency write
      elem := IsourceClass[ActiveActor].GetActiveObj ;
      if elem <> nil then elem.SrcFrequency := arg;
  end
  else
      Result:=-1.0;
  end;
end;

//***************************String type properties*******************************
function IsourceS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;

Var
   elem: TDSSCktElement;

begin
  Result := pAnsiChar(AnsiString('')); // Default return value
  case mode of
  0: begin  // Isources.Name read
        Result := pAnsiChar(AnsiString(''));
        elem := ActiveCircuit[ActiveActor].ActiveCktElement;
        If elem <> Nil Then Result := pAnsiChar(AnsiString(elem.Name));
  end;
  1: begin  // Isoruces.Name write
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          If IsourceClass[ActiveActor].SetActive(string(arg)) Then
          Begin
               ActiveCircuit[ActiveActor].ActiveCktElement := IsourceClass[ActiveActor].ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Isource "'+ arg +'" Not Found in Active Circuit.', 77003);
          End;
     End;
  end
  else
      Result:= pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//***************************Variant type properties*******************************
procedure IsourceV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

Var
  elem: TIsourceObj;
  pList: TPointerList;

begin
  case mode of
  0: begin                // Isources.AllNames
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      If IsourceClass[ActiveActor].ElementList.ListSize > 0 then
      Begin
        pList := IsourceClass[ActiveActor].ElementList;
        elem := pList.First;
        WHILE elem<>Nil DO
        Begin
          WriteStr2Array(elem.Name);
          WriteStr2Array(Char(0));
          elem := pList.next ;
        End;
      End;
    End
    Else  WriteStr2Array('');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
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
