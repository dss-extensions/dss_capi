unit DVSources;

interface

function VsourcesI(mode: Longint; arg: Longint): Longint; CDECL;
function VsourcesF(mode: Longint; arg: Double): Double; CDECL;
function VsourcesS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure VsourcesV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    ComServ,
    Vsource,
    Variants,
    PointerList,
    DSSGlobals,
    CktElement;

function VsourcesI(mode: Longint; arg: Longint): Longint; CDECL;

var
    pElem: TVsourceObj;
    elem: TVsourceObj;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin  // Vsource.Count
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := VsourceClass.ElementList.ListSize;
        end;
        1:
        begin  // Vsource.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pElem := VsourceClass.ElementList.First;
                if pElem <> NIL then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := 1;
                        end
                        else
                            pElem := VsourceClass.ElementList.Next;
                    until (Result = 1) or (pElem = NIL);
            end;
        end;
        2:
        begin  // Vsource.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pElem := VsourceClass.ElementList.Next;
                if pElem <> NIL then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := VsourceClass.ElementList.ActiveIndex;
                        end
                        else
                            pElem := VsourceClass.ElementList.Next;
                    until (Result > 0) or (pElem = NIL);
            end;
        end;
        3:
        begin   // Vsource.Phases read
            Result := 0;
            elem := VsourceClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.NPhases;
        end;
        4:
        begin  // Vsource.Phases write
            elem := VsourceClass.GetActiveObj;
            if elem <> NIL then
                elem.Nphases := arg;
        end
    else
        Result := -1;
    end;
end;

//***************************Floating point type properties*******************************
function VsourcesF(mode: Longint; arg: Double): Double; CDECL;

var
    elem: TVsourceObj;

begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // Vsources.basekV read
            Result := 0.0;
            elem := VsourceClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.kVBase;
        end;
        1:
        begin  // Vsources.basekV write
            elem := VsourceClass.GetActiveObj;
            if elem <> NIL then
                elem.kVBase := arg;
        end;
        2:
        begin  // Vsource.pu read
            Result := 0.0;
            elem := VsourceClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.perunit;
        end;
        3:
        begin  // Vsource.pu write
            elem := VsourceClass.GetActiveObj;
            if elem <> NIL then
                elem.PerUnit := arg;
        end;
        4:
        begin  // Vsource.Angledeg read
            Result := 0.0;
            elem := VsourceClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.angle;
        end;
        5:
        begin  // Vsource.Angledeg write
            elem := VsourceClass.GetActiveObj;
            if elem <> NIL then
                elem.Angle := arg;
        end;
        6:
        begin  // Vsource.Frequency read
            Result := 0.0;
            elem := VsourceClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.SrcFrequency;
        end;
        7:
        begin  // Vsource.Frequency write
            elem := VsourceClass.GetActiveObj;
            if elem <> NIL then
                elem.SrcFrequency := arg;
        end
    else
        Result := -1.0;
    end;
end;

//***************************String type properties*******************************
function VsourcesS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    elem: TDSSCktElement;

begin
    Result := pAnsiChar(Ansistring(''));    // Default return value
    case mode of
        0:
        begin  // Vsources.Name read
            Result := pAnsiChar(Ansistring(''));
            elem := ActiveCircuit[ActiveActor].ActiveCktElement;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.Name));
        end;
        1:
        begin  // Vsources.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if VsourceClass.SetActive(Widestring(arg)) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := VsourceClass.ElementList.Active;
                end
                else
                begin
                    DoSimpleMsg('Vsource "' + Widestring(arg) + '" Not Found in Active Circuit.', 77003);
                end;
            end;
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not valid'));
    end;
end;

//***************************Variant type properties*******************************
procedure VsourcesV(mode: Longint; out arg: Variant); CDECL;

var
    elem: TVsourceObj;
    pList: TPointerList;
    k: Integer;

begin
    case mode of
        0:
        begin  // VSources.AllNames
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if VsourceClass.ElementList.ListSize > 0 then
                begin
                    pList := VsourceClass.ElementList;
                    VarArrayRedim(arg, pList.ListSize - 1);
                    k := 0;
                    elem := pList.First;
                    while elem <> NIL do
                    begin
                        arg[k] := elem.Name;
                        Inc(k);
                        elem := pList.next;
                    end;
                end;
            end;
        end
    else
        arg[0] := 'Error, parameter not valid';
    end;
end;

end.
