unit DDSSElement;

interface

function DSSElementI(mode: Longint; arg: Longint): Longint; CDECL;
function DSSElementS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure DSSElementV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    DSSGlobals,
    Variants,
    Sysutils;

function DSSElementI(mode: Longint; arg: Longint): Longint; CDECL;
begin
    Result := 0; // Default return value
    case mode of
        0:
        begin  // DSSElement.NumProperties
            Result := 0;
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveDSSObject <> NIL then
                        with ActiveDSSObject do
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
function DSSElementS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
begin
    Result := pAnsiChar(Ansistring(''));// Default return value
    case mode of
        0:
        begin
            if ActiveCircuit <> NIL then
                if ActiveDSSObject <> NIL then
                    with ActiveDSSObject do
                    begin
                        Result := pAnsiChar(Ansistring(ParentClass.Name + '.' + Name));
                    end
                else
                    Result := pAnsiChar(Ansistring(''));
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not recognized'));
    end;
end;

//*****************************Variant type properties**************************
procedure DSSElementV(mode: Longint; out arg: Variant); CDECL;

var
    k: Integer;

begin
    case mode of
        0:
        begin  // DSSElement.AllPropertyNames
            arg := VarArrayCreate([0, 0], varOleStr);
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    if ActiveDSSObject <> NIL then
                        with ActiveDSSObject do
                        begin
                            with ParentClass do
                            begin
                                arg := VarArrayCreate([0, NumProperties - 1], varOleStr);
                                for k := 1 to NumProperties do
                                begin
                                    arg[k - 1] := PropertyName^[k];
                                end;
                            end;
                        end
                end;
        end
    else
        arg[0] := 'Error, parameter not recognized';
    end;
end;

end.
