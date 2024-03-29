unit DISource;

interface

function IsourceI(mode: Longint; arg: Longint): Longint; CDECL;
function IsourceF(mode: Longint; arg: Double): Double; CDECL;
function IsourceS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure IsourceV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
{$IFNDEF FPC_DLL}
    ComServ,
{$ENDIF}
    Variants,
    PointerList,
    Isource,
    DSSGlobals,
    CktElement;

function IsourceI(mode: Longint; arg: Longint): Longint; CDECL;

var
    pElem: TIsourceObj;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin  // Isources.Count
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := IsourceClass[ActiveActor].ElementList.ListSize;
        end;
        1:
        begin  // Isources.First
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pElem := IsourceClass[ActiveActor].ElementList.First;
                if pElem <> NIL then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := 1;
                        end
                        else
                            pElem := IsourceClass[ActiveActor].ElementList.Next;
                    until (Result = 1) or (pElem = NIL);
            end;
        end;
        2:
        begin  // Isources.Next
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pElem := IsourceClass[ActiveActor].ElementList.Next;
                if pElem <> NIL then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := IsourceClass[ActiveActor].ElementList.ActiveIndex;
                        end
                        else
                            pElem := IsourceClass[ActiveActor].ElementList.Next;
                    until (Result > 0) or (pElem = NIL);
            end;
        end
    else
        Result := -1;
    end;
end;

//***************************Floating point type properties*******************************
function IsourceF(mode: Longint; arg: Double): Double; CDECL;

var
    elem: TIsourceObj;

begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // Isources.Amps read
            Result := 0.0;
            elem := IsourceClass[ActiveActor].ElementList.Active;
            if elem <> NIL then
                Result := elem.Amps;
        end;
        1:
        begin  // Isources.Amps write
            elem := IsourceClass[ActiveActor].GetActiveObj;
            if elem <> NIL then
                elem.Amps := arg;
        end;
        2:
        begin  // Isources.AngleDeg read
            Result := 0.0;
            elem := IsourceClass[ActiveActor].ElementList.Active;
            if elem <> NIL then
                Result := elem.Angle;
        end;
        3:
        begin  // Isources.AngleDeg write
            elem := IsourceClass[ActiveActor].GetActiveObj;
            if elem <> NIL then
                elem.Angle := arg;
        end;
        4:
        begin  // Isources.Frequency read
            Result := 0.0;
            elem := IsourceClass[ActiveActor].ElementList.Active;
            if elem <> NIL then
                Result := elem.SrcFrequency;
        end;
        5:
        begin  // Isources.Frequency write
            elem := IsourceClass[ActiveActor].GetActiveObj;
            if elem <> NIL then
                elem.SrcFrequency := arg;
        end
    else
        Result := -1.0;
    end;
end;

//***************************String type properties*******************************
function IsourceS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    elem: TDSSCktElement;

begin
    Result := Pansichar(Ansistring('')); // Default return value
    case mode of
        0:
        begin  // Isources.Name read
            Result := Pansichar(Ansistring(''));
            elem := ActiveCircuit[ActiveActor].ActiveCktElement;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.Name));
        end;
        1:
        begin  // Isoruces.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if IsourceClass[ActiveActor].SetActive(String(arg)) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := IsourceClass[ActiveActor].ElementList.Active;
                end
                else
                begin
                    DoSimpleMsg('Isource "' + arg + '" Not Found in Active Circuit.', 77003);
                end;
            end;
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//***************************Variant type properties*******************************
procedure IsourceV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    elem: TIsourceObj;
    pList: TPointerList;

begin
    case mode of
        0:
        begin                // Isources.AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if IsourceClass[ActiveActor].ElementList.ListSize > 0 then
                begin
                    pList := IsourceClass[ActiveActor].ElementList;
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
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
end;

end.
