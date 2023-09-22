unit DActiveClass;

interface

function ActiveClassI(mode:longint; arg: longint):longint; cdecl;
function ActiveClassS(mode:longint; arg: pAnsiChar):pAnsiChar; cdecl;
procedure ActiveClassV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;


implementation

uses DSSGlobals, DSSObject, Variants, CktElement, PCElement, DSSClass, PDClass, PCClass, MeterClass, ControlClass;

function ActiveClassI(mode:longint; arg: longint):longint; cdecl;
begin
  case mode of
  0: begin  // ActiveClass.First
       Result := 0;
       If (ActiveCircuit[ActiveActor] <> Nil) and Assigned(ActiveDSSClass[ActiveActor]) Then
       Begin
            Result := ActiveDSSClass[ActiveActor].First;  // sets active objects
       End;
  end;
  1: begin  // ActiveClass.Next
       Result := 0;
       If (ActiveCircuit[ActiveActor] <> Nil) and Assigned(ActiveDSSClass[ActiveActor]) Then
       Begin
            Result := ActiveDSSClass[ActiveActor].Next;  // sets active objects
       End;
  end;
  2: begin  //ActiveClass.NumElements
        if Assigned(ActiveDSSClass[ActiveActor]) then  Result := ActiveDSSCLass[ActiveActor].ElementCount
         Else Result := 0;
  end;
  3: begin  //ActiveClass.Count
         if Assigned(ActiveDSSClass[ActiveActor]) then  Result := ActiveDSSCLass[ActiveActor].ElementCount
         Else Result := 0;
  end
  else
      Result:=-1;
  end;
end;

//***************************String type properties*****************************
function ActiveClassS(mode:longint; arg: pAnsiChar):pAnsiChar; cdecl;

Var
  pelem:TDSSObject;

begin
  Result:=pAnsiChar(AnsiString('0'));
  case mode of
  0: begin  // ActiveClass.Name read
      if Assigned(ActiveDSSObject[ActiveActor]) then  Result := pAnsiChar(AnsiString(ActiveDSSObject[ActiveActor].Name))
      Else Result := pAnsiChar(AnsiString(''));
  end;
  1: begin  // ActiveClass.Name write
     If  Assigned(ActiveDSSClass[ActiveActor]) Then  Begin
         pelem := ActiveDSSClass[ActiveActor].Find(arg);
         if pelem <> Nil then Begin
            if pelem is TDSSCktElement then
             ActiveCircuit[ActiveActor].ActiveCktElement := TDSSCktElement(pelem)  // sets ActiveDSSobject
          Else
             ActiveDSSObject[ActiveActor] := pelem;
         End;
     End;
  end;
  2: begin  // ActiveClass.ActiveClassName
     if Assigned(ActiveDSSClass[ActiveActor]) then  Result := pAnsiChar(AnsiString(ActiveDSSCLass[ActiveActor].Name))
     Else Result := pAnsiChar(AnsiString(''));
  end;
  3: begin  // ActiveClass.ActiveClassParent
     if Assigned(ActiveDSSClass[ActiveActor]) then
     Begin
      Result  :=  pAnsiChar('Generic Object');

      if ActiveDSSClass[ActiveActor].ClassType.InheritsFrom(TPCClass) then
        Result  :=  pAnsiChar('TPCClas');
      if ActiveDSSClass[ActiveActor].ClassType.InheritsFrom(TPDClass) then
        Result  :=  pAnsiChar('TPDClass');
      if ActiveDSSClass[ActiveActor].ClassType.InheritsFrom(TMeterClass) then
        Result  :=  pAnsiChar('TMeterClass');
      if ActiveDSSClass[ActiveActor].ClassType.InheritsFrom(TControlClass) then
        Result  :=  pAnsiChar('TControlClass');
     End
     Else Result := pAnsiChar(AnsiString('Parent Class unknonwn'));
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not recognized'));
  end;
end;

//*****************************Variant type properties**************************
procedure ActiveClassV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

Var
  idx,
  i     : Integer;
  S     : String;

begin
  case mode of
  0: begin
    setlength(myStrArray,1);
    myStrArray[0] :=  0;
    mySize        :=  0;
    If (ActiveCircuit[ActiveActor] <> Nil) and Assigned(ActiveDSSClass[ActiveActor]) Then
    Begin
       WITH ActiveCircuit[ActiveActor] DO
       Begin
         setlength(myStrArray,0);
         idx := ActiveDSSClass[ActiveActor].First;
         WHILE idx > 0 DO  Begin
            S := ActiveDSSObject[ActiveActor].Name;
            for i := 1 to High(S) do
            Begin
              setlength(myStrArray,length(myStrArray) + 1);
              myStrArray[High(myStrArray)]  :=  Byte(S[i]);
            End;
            idx := ActiveDSSClass[ActiveActor].Next;
            if idx > 0 then
            Begin
              setlength(myStrArray,length(myStrArray) + 1);
              myStrArray[High(myStrArray)]  :=  Byte(0);
            End;
         End;
       End
    End;
    myType    :=  4;                  // String
    mySize    :=  length(myStrArray);
    myPointer :=  @(myStrArray[0]);
  end
  else
    setlength(myStrArray,1);
    myStrArray[0]   :=  0;
    myPointer       :=  @(myStrArray[0]);
    myType          :=  -1;                  // Error
  end;
end;

end.
