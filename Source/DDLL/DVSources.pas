unit DVSources;

interface

function VsourcesI(mode: Longint; arg: Longint): Longint; CDECL;
function VsourcesF(mode: Longint; arg: Double): Double; CDECL;
function VsourcesS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure VsourcesV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
{$IFNDEF FPC_DLL}
    ComServ,
{$ENDIF}
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
                Result := VsourceClass[ActiveActor].ElementList.ListSize;
        end;
        1:
        begin  // Vsource.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pElem := VsourceClass[ActiveActor].ElementList.First;
                if pElem <> NIL then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := 1;
                        end
                        else
                            pElem := VsourceClass[ActiveActor].ElementList.Next;
                    until (Result = 1) or (pElem = NIL);
            end;
        end;
        2:
        begin  // Vsource.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pElem := VsourceClass[ActiveActor].ElementList.Next;
                if pElem <> NIL then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := VsourceClass[ActiveActor].ElementList.ActiveIndex;
                        end
                        else
                            pElem := VsourceClass[ActiveActor].ElementList.Next;
                    until (Result > 0) or (pElem = NIL);
            end;
        end;
        3:
        begin   // Vsource.Phases read
            Result := 0;
            elem := VsourceClass[ActiveActor].ElementList.Active;
            if elem <> NIL then
                Result := elem.NPhases;
        end;
        4:
        begin  // Vsource.Phases write
            elem := VsourceClass[ActiveActor].GetActiveObj;
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
            elem := VsourceClass[ActiveActor].ElementList.Active;
            if elem <> NIL then
                Result := elem.kVBase;
        end;
        1:
        begin  // Vsources.basekV write
            elem := VsourceClass[ActiveActor].GetActiveObj;
            if elem <> NIL then
                elem.kVBase := arg;
        end;
        2:
        begin  // Vsource.pu read
            Result := 0.0;
            elem := VsourceClass[ActiveActor].ElementList.Active;
            if elem <> NIL then
                Result := elem.perunit;
        end;
        3:
        begin  // Vsource.pu write
            elem := VsourceClass[ActiveActor].GetActiveObj;
            if elem <> NIL then
                elem.PerUnit := arg;
        end;
        4:
        begin  // Vsource.Angledeg read
            Result := 0.0;
            elem := VsourceClass[ActiveActor].ElementList.Active;
            if elem <> NIL then
                Result := elem.angle;
        end;
        5:
        begin  // Vsource.Angledeg write
            elem := VsourceClass[ActiveActor].GetActiveObj;
            if elem <> NIL then
                elem.Angle := arg;
        end;
        6:
        begin  // Vsource.Frequency read
            Result := 0.0;
            elem := VsourceClass[ActiveActor].ElementList.Active;
            if elem <> NIL then
                Result := elem.SrcFrequency;
        end;
        7:
        begin  // Vsource.Frequency write
            elem := VsourceClass[ActiveActor].GetActiveObj;
            if elem <> NIL then
                elem.SrcFrequency := arg;
        end
    else
        Result := -1.0;
    end;
end;

//***************************String type properties*******************************
function VsourcesS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    elem: TDSSCktElement;

begin
    Result := Pansichar(Ansistring(''));    // Default return value
    case mode of
        0:
        begin  // Vsources.Name read
            Result := Pansichar(Ansistring(''));
            elem := ActiveCircuit[ActiveActor].ActiveCktElement;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.Name));
        end;
        1:
        begin  // Vsources.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if VsourceClass[ActiveActor].SetActive(String(arg)) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := VsourceClass[ActiveActor].ElementList.Active;
                end
                else
                begin
                    DoSimpleMsg('Vsource "' + arg + '" Not Found in Active Circuit.', 77003);
                end;
            end;
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//***************************Variant type properties*******************************
procedure VsourcesV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    elem: TVsourceObj;
    pList: TPointerList;

begin
    case mode of
        0:
        begin  // VSources.AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if VsourceClass[ActiveActor].ElementList.ListSize > 0 then
                begin
                    pList := VsourceClass[ActiveActor].ElementList;
                    elem := pList.First;
                    while elem <> NIL do
                    begin
                        WriteStr2Array(elem.Name);
                        WriteStr2Array(Char(0));
                        elem := pList.next;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end
    else
    begin
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
    end;
end;

end.
