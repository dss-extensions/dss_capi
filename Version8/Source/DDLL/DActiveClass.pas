unit DActiveClass;

interface

function ActiveClassI(mode: Longint; arg: Longint): Longint; CDECL;
function ActiveClassS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure ActiveClassV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    DSSGlobals,
    DSSObject,
    Variants,
    CktElement;

function ActiveClassI(mode: Longint; arg: Longint): Longint; CDECL;
begin
    case mode of
        0:
        begin  // ActiveClass.First
            Result := 0;
            if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass[ActiveActor]) then
            begin
                Result := ActiveDSSClass[ActiveActor].First;  // sets active objects
            end;
        end;
        1:
        begin  // ActiveClass.Next
            Result := 0;
            if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass[ActiveActor]) then
            begin
                Result := ActiveDSSClass[ActiveActor].Next;  // sets active objects
            end;
        end;
        2:
        begin  //ActiveClass.NumElements
            if Assigned(ActiveDSSClass[ActiveActor]) then
                Result := ActiveDSSCLass[ActiveActor].ElementCount
            else
                Result := 0;
        end;
        3:
        begin  //ActiveClass.Count
            if Assigned(ActiveDSSClass[ActiveActor]) then
                Result := ActiveDSSCLass[ActiveActor].ElementCount
            else
                Result := 0;
        end
    else
        Result := -1;
    end;
end;

//***************************String type properties*****************************
function ActiveClassS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    pelem: TDSSObject;

begin
    Result := pAnsiChar(Ansistring('0'));
    case mode of
        0:
        begin  // ActiveClass.Name read
            if Assigned(ActiveDSSObject[ActiveActor]) then
                Result := pAnsiChar(Ansistring(ActiveDSSObject[ActiveActor].Name))
            else
                Result := pAnsiChar(Ansistring(''));
        end;
        1:
        begin  // ActiveClass.Name write
            if Assigned(ActiveDSSClass[ActiveActor]) then
            begin
                pelem := ActiveDSSClass[ActiveActor].Find(Widestring(arg));
                if pelem <> NIL then
                begin
                    if pelem is TDSSCktElement then
                        ActiveCircuit[ActiveActor].ActiveCktElement := TDSSCktElement(pelem)  // sets ActiveDSSobject
                    else
                        ActiveDSSObject[ActiveActor] := pelem;
                end;
            end;
        end;
        2:
        begin  // ActiveClass.ActiveClassName
            if Assigned(ActiveDSSClass[ActiveActor]) then
                Result := pAnsiChar(Ansistring(ActiveDSSCLass[ActiveActor].Name))
            else
                Result := pAnsiChar(Ansistring(''));
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not recognized'));
    end;
end;

//*****************************Variant type properties**************************
procedure ActiveClassV(mode: Longint; out arg: Variant); CDECL;

var
    idx: Integer;
    k: Integer;

begin
    case mode of
        0:
        begin
            if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    arg := VarArrayCreate([0, ActiveDSSClass[ActiveActor].ElementCount - 1], varOleStr);
                    k := 0;
                    idx := ActiveDSSClass[ActiveActor].First;
                    while idx > 0 do
                    begin
                        arg[k] := ActiveDSSObject[ActiveActor].Name;
                        Inc(k);
                        idx := ActiveDSSClass[ActiveActor].Next;
                    end;
                end
            else
                arg := VarArrayCreate([0, 0], varOleStr);
        end
    else
        arg[0] := 'Error,parameter not recognized';
    end;
end;

end.
