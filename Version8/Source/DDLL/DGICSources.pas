unit DGICSources;

interface

function GICSourcesI(mode: Longint; arg: Longint): Longint; CDECL;
function GICSourcesF(mode: Longint; arg: Double): Double; CDECL;
function GICSourcesS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure GICSourcesV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    Variants,
    DSSGlobals,
    GICSource,
    PointerList,
    CktElement;

//*****************************Integer type properties************************
function GICSourcesI(mode: Longint; arg: Longint): Longint; CDECL;
var
    pElem: TGICsourceObj;
begin
    Result := 0; // Default return value
    case mode of
        0:
        begin  // GICSources.Count
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := GICSourceClass.ElementList.ListSize;
        end;
        1:
        begin  // GICSources.First
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pElem := GICSourceClass.ElementList.First;
                if pElem <> NIL then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := 1;
                        end
                        else
                            pElem := GICSourceClass.ElementList.Next;
                    until (Result = 1) or (pElem = NIL);
            end;
        end;
        2:
        begin  // GICSources.Next
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pElem := GICSourceClass.ElementList.Next;
                if pElem <> NIL then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := GICSourceClass.ElementList.ActiveIndex;
                        end
                        else
                            pElem := GICSourceClass.ElementList.Next;
                    until (Result > 0) or (pElem = NIL);
            end;
        end;
        3:
        begin  // GICSources.Phases read
            pElem := GICSourceClass.ElementList.Active;
            if pElem <> NIL then
            begin
                Result := pElem.nphases;
            end;
        end;
        4:
        begin  // GICSources.Phases write
            pElem := GICSourceClass.ElementList.Active;
            if pElem <> NIL then
            begin
                pElem.nphases := arg;
                pElem.NConds := arg;  // Force reallocation of terminal info
            end;
        end
    else
        Result := -1;
    end;
end;

//*****************************Floating point type properties************************
function GICSourcesF(mode: Longint; arg: Double): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // GICSources.EN read
            elem := GICSourceClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.ENorth;
        end;
        1:
        begin  // GICSources.EN write
            elem := GICSourceClass.ElementList.Active;
            if elem <> NIL then
                elem.ENorth := arg;
        end;
        2:
        begin  // GICSources.EE read
            elem := GICSourceClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.EEast;
        end;
        3:
        begin  // GICSources.EE write
            elem := GICSourceClass.ElementList.Active;
            if elem <> NIL then
                elem.EEast := arg;
        end;
        4:
        begin  // GICSources.Lat1 read
            elem := GICSourceClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.Lat1;
        end;
        5:
        begin  // GICSources.Lat1 write
            elem := GICSourceClass.ElementList.Active;
            if elem <> NIL then
            begin
                elem.Lat1 := arg;
                elem.VoltsSpecified := FALSE;
            end;
        end;
        6:
        begin  // GICSources.Lat2 read
            elem := GICSourceClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.Lat2;
        end;
        7:
        begin  // GICSources.Lat2 write
            elem := GICSourceClass.ElementList.Active;
            if elem <> NIL then
            begin
                elem.Lat2 := arg;
                elem.VoltsSpecified := FALSE;
            end;
        end;
        8:
        begin  // GICSources.Lon1 read
            elem := GICSourceClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.Lon1;
        end;
        9:
        begin  // GICSources.Lon1 write
            elem := GICSourceClass.ElementList.Active;
            if elem <> NIL then
            begin
                elem.Lon1 := arg;
                elem.VoltsSpecified := FALSE;
            end;
        end;
        10:
        begin  // GICSources.Lon2 read
            elem := GICSourceClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.Lon2;
        end;
        11:
        begin  // GICSources.Lon2 write
            elem := GICSourceClass.ElementList.Active;
            if elem <> NIL then
            begin
                elem.Lon2 := arg;
                elem.VoltsSpecified := FALSE;
            end;
        end;
        12:
        begin  // GICSources.Volts read
            elem := GICSourceClass.ElementList.Active;
            if elem <> NIL then
                Result := elem.Volts;
        end;
        13:
        begin  // GICSources.Volts write
            elem := GICSourceClass.ElementList.Active;
            if elem <> NIL then
            begin
                elem.Volts := arg;
                elem.VoltsSpecified := FALSE;
            end;
        end
    else
        Result := -1.0;
    end;
end;

//*******************************String type properties****************************
function GICSourcesS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
var
    S: String;
begin
    Result := pAnsiChar(Ansistring('0'));   // Default return value
    case mode of
        0:
        begin  // GICSources.Bus1
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := pAnsiChar(Ansistring(ActiveCircuit[ActiveActor].ActiveCktElement.GetBus(1)));
            end
        end;
        1:
        begin  // GICSources.Bus2
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := pAnsiChar(Ansistring(ActiveCircuit[ActiveActor].ActiveCktElement.GetBus(2)));
            end
        end;
        2:
        begin  // GICSources.Name read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := pAnsiChar(Ansistring(ActiveCircuit[ActiveActor].ActiveCktElement.Name));
            end
        end;
        3:
        begin  // GICSources.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                S := Widestring(arg);
                if GICsourceClass.SetActive(S) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := GICsourceClass.ElementList.Active;
                end
                else
                begin
                    DoSimpleMsg('Vsource "' + S + '" Not Found in Active Circuit.', 77003);
                end;
            end
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not valid'));
    end;
end;

//*****************************Variant ype properties*****************************
procedure GICSourcesV(mode: Longint; out arg: Variant); CDECL;
var
    GICElem: TGICSourceObj;
    ElementList: Tpointerlist;
    k: Integer;
begin
    case mode of
        0:
        begin  // GISource.AllNames
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ElementList := GICsourceClass.ElementList;
                if ElementList.ListSize > 0 then
                begin
                    VarArrayRedim(arg, ElementList.ListSize - 1);
                    k := 0;
                    GICElem := ElementList.First;
                    while GICElem <> NIL do
                    begin
                        arg[k] := GICElem.Name;
                        Inc(k);
                        GICElem := ElementList.Next;
                    end;
                end;
            end;
        end
    else
        arg[0] := 'Error, parameter not valid';
    end;
end;


end.
