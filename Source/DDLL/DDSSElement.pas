unit DDSSElement;

interface

function DSSElementI(mode:longint; arg: longint):longint;cdecl;
function DSSElementS(mode:longint; arg: pAnsiChar):pAnsiChar;cdecl;
procedure DSSElementV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses DSSGlobals,
     Variants,
     Sysutils;

function DSSElementI(mode:longint; arg: longint):longint;cdecl;
begin
  case mode of
  0: begin  // DSSElement.NumProperties
    Result := 0;
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       If ActiveDSSObject<>Nil THEN
       WITH ActiveDSSObject[ActiveActor] DO
       Begin
            Result := ParentClass.NumProperties ;
       End
     End;
  end
  else
      Result:=-1;
  end;
end;

//*********************************String type properties**************************
function DSSElementS(mode:longint; arg: pAnsiChar):pAnsiChar;cdecl;
begin
  Result:=pAnsiChar(AnsiString(''));// Default return value
  case mode of
  0: begin
     If ActiveCircuit[ActiveActor] <> Nil Then
       if ActiveDSSObject[ActiveActor] <> Nil then
        WITH ActiveDSSObject[ActiveActor] DO
        Begin
          Result := pAnsiChar(AnsiString(ParentClass.Name + '.' + Name));
        End
     Else
        Result := pAnsiChar(AnsiString(''));
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not recognized'));
  end;
end;

//*****************************Variant type properties**************************
procedure DSSElementV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

VAR
   k:Integer;

begin
  case mode of
  0: begin  // DSSElement.AllPropertyNames
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    WITH ActiveCircuit[ActiveActor] DO
    Begin
      If ActiveDSSObject[ActiveActor]<>Nil THEN
      WITH ActiveDSSObject[ActiveActor] DO
      Begin
        WITH ParentClass Do
        Begin
          For k := 1 to NumProperties DO
          Begin
            WriteStr2Array(PropertyName^[k]);
            WriteStr2Array(Char(0));
          End;
        End;
      End
    End;
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
