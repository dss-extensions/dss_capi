unit DGICSources;

interface

function GICSourcesI(mode: longint; arg: longint):longint; cdecl;
function GICSourcesF(mode: longint; arg: double):double; cdecl;
function GICSourcesS(mode: longint; arg: pAnsiChar):pAnsiChar; cdecl;
procedure GICSourcesV(mode: longint; out arg: Variant); cdecl;

implementation

uses Variants, DSSGlobals, GICSource, PointerList, CktElement;

//*****************************Integer type properties************************
function GICSourcesI(mode: longint; arg: longint):longint; cdecl;
Var
   pElem : TGICsourceObj;
Begin
  Result:=0; // Default return value
  case mode of
  0: begin  // GICSources.Count
       If ActiveCircuit[ActiveActor] <> Nil Then
         Result := GICSourceClass.ElementList.ListSize;
  end;
  1: begin  // GICSources.First
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := GICSourceClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := GICSourceClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
  end;
  2: begin  // GICSources.Next
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := GICSourceClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := GICSourceClass.ElementList.ActiveIndex;
          End
          Else pElem := GICSourceClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
  end;
  3: begin  // GICSources.Phases read
    pElem := GICSourceClass.ElementList.Active ;
    if pElem <> nil then Begin
      Result := pElem.nphases;
    End;
  end;
  4: begin  // GICSources.Phases write
    pElem := GICSourceClass.ElementList.Active ;
    if pElem <> nil then Begin
      pElem.nphases := arg ;
      pElem.NConds := arg;  // Force reallocation of terminal info
    End;
  end
  else
      Result:=-1;
  end;
End;

//*****************************Floating point type properties************************
function GICSourcesF(mode: longint; arg: double):double; cdecl;
Var
   elem : TGICsourceObj;
Begin
  Result:=0.0; // Default return value
  case mode of
    0: begin  // GICSources.EN read
      elem := GICSourceClass.ElementList.Active ;
      if elem <> nil then Result := elem.ENorth;
    end;
    1: begin  // GICSources.EN write
      elem := GICSourceClass.ElementList.Active ;
      if elem <> nil then elem.ENorth := arg ;
    end;
    2: begin  // GICSources.EE read
      elem := GICSourceClass.ElementList.Active ;
      if elem <> nil then Result := elem.EEast;
    end;
    3: begin  // GICSources.EE write
      elem := GICSourceClass.ElementList.Active ;
      if elem <> nil then elem.EEast := arg ;
    end;
    4: begin  // GICSources.Lat1 read
      elem := GICSourceClass.ElementList.Active ;
      if elem <> nil then Result := elem.Lat1;
    end;
    5: begin  // GICSources.Lat1 write
      elem := GICSourceClass.ElementList.Active ;
      if elem <> nil then Begin
         elem.Lat1 := arg ;
         elem.VoltsSpecified := FALSE;
      End;
    end;
    6: begin  // GICSources.Lat2 read
      elem := GICSourceClass.ElementList.Active ;
      if elem <> nil then Result := elem.Lat2;
    end;
    7: begin  // GICSources.Lat2 write
      elem := GICSourceClass.ElementList.Active ;
      if elem <> nil then Begin
         elem.Lat2 := arg ;
         elem.VoltsSpecified := FALSE;
      End;
    end;
    8: begin  // GICSources.Lon1 read
      elem := GICSourceClass.ElementList.Active ;
      if elem <> nil then Result := elem.Lon1;
    end;
    9: begin  // GICSources.Lon1 write
      elem := GICSourceClass.ElementList.Active ;
      if elem <> nil then Begin
         elem.Lon1 := arg ;
         elem.VoltsSpecified := FALSE;
      End;
    end;
    10: begin  // GICSources.Lon2 read
      elem := GICSourceClass.ElementList.Active ;
      if elem <> nil then Result := elem.Lon2;
    end;
    11: begin  // GICSources.Lon2 write
      elem := GICSourceClass.ElementList.Active ;
      if elem <> nil then Begin
         elem.Lon2 := arg ;
         elem.VoltsSpecified := FALSE;
      End;
    end;
    12: begin  // GICSources.Volts read
      elem := GICSourceClass.ElementList.Active ;
      if elem <> nil then Result := elem.Volts;
    end;
    13: begin  // GICSources.Volts write
      elem := GICSourceClass.ElementList.Active ;
      if elem <> nil then Begin
         elem.Volts := arg ;
         elem.VoltsSpecified := FALSE;
      End;
    end
    else
      Result:=-1.0;
  end;
End;

//*******************************String type properties****************************
function GICSourcesS(mode: longint; arg: pAnsiChar):pAnsiChar; cdecl;
var
  S   : String;
Begin
  Result:=pAnsiChar(AnsiString('0'));   // Default return value
  case mode of
    0: begin  // GICSources.Bus1
      IF ActiveCircuit[ActiveActor] <> NIL
      THEN Begin
           Result := pAnsiChar(AnsiString(ActiveCircuit[ActiveActor].ActiveCktElement.GetBus(1)));
      End
    end;
    1: begin  // GICSources.Bus2
      IF ActiveCircuit[ActiveActor] <> NIL
      THEN Begin
           Result := pAnsiChar(AnsiString(ActiveCircuit[ActiveActor].ActiveCktElement.GetBus(2)));
      End
    end;
    2: begin  // GICSources.Name read
      IF ActiveCircuit[ActiveActor] <> NIL
      THEN Begin
           Result := pAnsiChar(AnsiString(ActiveCircuit[ActiveActor].ActiveCktElement.Name));
      End
    end;
    3: begin  // GICSources.Name write
      IF ActiveCircuit[ActiveActor] <> NIL
      THEN Begin
        S   :=  widestring(arg);
        If GICsourceClass.SetActive(S) Then
        Begin
             ActiveCircuit[ActiveActor].ActiveCktElement := GICsourceClass.ElementList.Active ;
        End
        Else Begin
            DoSimpleMsg('Vsource "'+ S +'" Not Found in Active Circuit.', 77003);
        End;
      End
    end
    else
      Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
End;

//*****************************Variant ype properties*****************************
procedure GICSourcesV(mode: longint; out arg: Variant); cdecl;
Var
  GICElem      :TGICSourceObj;
  ElementList  :Tpointerlist;
  k:Integer;
Begin
  case mode of
    0: begin  // GISource.AllNames
      arg := VarArrayCreate([0, 0], varOleStr);
      arg[0] := 'NONE';
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
         ElementList := GICsourceClass.ElementList;
         If ElementList.ListSize>0 Then
         Begin
           VarArrayRedim(arg, ElementList.ListSize-1);
           k:=0;
           GICElem := ElementList.First;
           WHILE GICElem<>Nil DO
           Begin
              arg[k] := GICElem.Name;
              Inc(k);
              GICElem := ElementList.Next;
           End;
         End;
      End;
    end
    else
      arg[0]:='Error, parameter not valid';
  end;
End;


end.
