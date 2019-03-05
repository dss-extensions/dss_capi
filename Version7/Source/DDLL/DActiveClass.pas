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
            if (ActiveCircuit <> NIL) and Assigned(ActiveDSSClass) then
            begin
                Result := ActiveDSSClass.First;  // sets active objects
            end;
        end;
        1:
        begin  // ActiveClass.Next
            Result := 0;
            if (ActiveCircuit <> NIL) and Assigned(ActiveDSSClass) then
            begin
                Result := ActiveDSSClass.Next;  // sets active objects
            end;
        end;
        2:
        begin  //ActiveClass.NumElements
            if Assigned(ActiveDSSClass) then
                Result := ActiveDSSCLass.ElementCount
            else
                Result := 0;
        end;
        3:
        begin  //ActiveClass.Count
            if Assigned(ActiveDSSClass) then
                Result := ActiveDSSCLass.ElementCount
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
            if Assigned(ActiveDSSObject) then
                Result := pAnsiChar(Ansistring(ActiveDSSObject.Name))
            else
                Result := pAnsiChar(Ansistring(''));
        end;
        1:
        begin  // ActiveClass.Name write
            if Assigned(ActiveDSSClass) then
            begin
                pelem := ActiveDSSClass.Find(String(arg));
                if pelem <> NIL then
                begin
                    if pelem is TDSSCktElement then
                        ActiveCircuit.ActiveCktElement := TDSSCktElement(pelem)  // sets ActiveDSSobject
                    else
                        ActiveDSSObject := pelem;
                end;
            end;
        end;
        2:
        begin  // ActiveClass.ActiveClassName
            if Assigned(ActiveDSSClass) then
                Result := pAnsiChar(Ansistring(ActiveDSSCLass.Name))
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
            if (ActiveCircuit <> NIL) and Assigned(ActiveDSSClass) then
                with ActiveCircuit do
                begin
                    arg := VarArrayCreate([0, ActiveDSSClass.ElementCount - 1], varOleStr);
                    k := 0;
                    idx := ActiveDSSClass.First;
                    while idx > 0 do
                    begin
                        arg[k] := ActiveDSSObject.Name;
                        Inc(k);
                        idx := ActiveDSSClass.Next;
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
