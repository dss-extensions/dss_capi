unit DDSSElement;

interface

function DSSElementI(mode: Longint; arg: Longint): Longint; CDECL;
function DSSElementS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure DSSElementV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    DSSGlobals,
    Variants,
    Sysutils;

function DSSElementI(mode: Longint; arg: Longint): Longint; CDECL;
begin
    case mode of
        0:
        begin  // DSSElement.NumProperties
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    if ActiveDSSObject <> NIL then
                        with ActiveDSSObject[ActiveActor] do
                        begin
                            Result := ParentClass.NumProperties;
                        end
                end;
        end
    else
        Result := -1;
    end;
end;

//*********************************String type properties**************************
function DSSElementS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
begin
    Result := Pansichar(Ansistring(''));// Default return value
    case mode of
        0:
        begin
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveDSSObject[ActiveActor] <> NIL then
                    with ActiveDSSObject[ActiveActor] do
                    begin
                        Result := Pansichar(Ansistring(ParentClass.Name + '.' + Name));
                    end
                else
                    Result := Pansichar(Ansistring(''));
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not recognized'));
    end;
end;

//*****************************Variant type properties**************************
procedure DSSElementV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    k: Integer;

begin
    case mode of
        0:
        begin  // DSSElement.AllPropertyNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    if ActiveDSSObject[ActiveActor] <> NIL then
                        with ActiveDSSObject[ActiveActor] do
                        begin
                            with ParentClass do
                            begin
                                for k := 1 to NumProperties do
                                begin
                                    WriteStr2Array(PropertyName^[k]);
                                    WriteStr2Array(Char(0));
                                end;
                            end;
                        end
                end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end
    else
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
end;

end.
