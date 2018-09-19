unit DDSSElement;

interface

function DSSElementI(mode:longint; arg: longint):longint;cdecl;
function DSSElementS(mode:longint; arg: pAnsiChar):pAnsiChar;cdecl;
procedure DSSElementV(mode: longint; out arg: Variant);cdecl;

implementation

uses DSSGlobals,
     Variants,
     Sysutils;

function DSSElementI(mode:longint; arg: longint):longint;cdecl;
begin
  Result:=0; // Default return value
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
procedure DSSElementV(mode: longint; out arg: Variant);cdecl;

VAR
   k:Integer;

begin
  case mode of
  0: begin  // DSSElement.AllPropertyNames
    arg := VarArrayCreate([0, 0], varOleStr);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       If ActiveDSSObject[ActiveActor]<>Nil THEN
       WITH ActiveDSSObject[ActiveActor] DO
       Begin
            WITH ParentClass Do
            Begin
                arg := VarArrayCreate([0, NumProperties-1], varOleStr);
                For k := 1 to NumProperties DO Begin
                    arg[k-1] := PropertyName^[k];
                End;
            End;
       End
     End;
  end
  else
      arg[0]:='Error, parameter not recognized';
  end;
end;

end.
