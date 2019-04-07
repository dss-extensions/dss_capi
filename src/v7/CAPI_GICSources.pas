unit CAPI_GICSources;

{$inline on}

interface

uses
    CAPI_Utils;

procedure GICSources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure GICSources_Get_AllNames_GR(); CDECL;
function GICSources_Get_Count(): Integer; CDECL;
function GICSources_Get_First(): Integer; CDECL;
function GICSources_Get_Next(): Integer; CDECL;
function GICSources_Get_Name(): PAnsiChar; CDECL;
procedure GICSources_Set_Name(const Value: PAnsiChar); CDECL;
function GICSources_Get_Phases(): Integer; CDECL;
procedure GICSources_Set_Phases(Value: Integer); CDECL;
function GICSources_Get_Bus1(): PAnsiChar; CDECL;
function GICSources_Get_Bus2(): PAnsiChar; CDECL;
function GICSources_Get_EN(): Double; CDECL;
procedure GICSources_Set_EN(Value: Double); CDECL;
function GICSources_Get_EE(): Double; CDECL;
procedure GICSources_Set_EE(Value: Double); CDECL;
function GICSources_Get_Lat1(): Double; CDECL;
procedure GICSources_Set_Lat1(Value: Double); CDECL;
function GICSources_Get_Lat2(): Double; CDECL;
procedure GICSources_Set_Lat2(Value: Double); CDECL;
function GICSources_Get_Lon1(): Double; CDECL;
procedure GICSources_Set_Lon1(Value: Double); CDECL;
function GICSources_Get_Lon2(): Double; CDECL;
procedure GICSources_Set_Lon2(Value: Double); CDECL;
function GICSources_Get_Volts(): Double; CDECL;
procedure GICSources_Set_Volts(Value: Double); CDECL;
function GICSources_Get_idx(): Integer; CDECL;
procedure GICSources_Set_idx(Value: Integer); CDECL;

implementation

uses
    CAPI_Constants,
    GICsource,
    PointerList,
    DSSGlobals,
    CktElement,
    SysUtils;
//------------------------------------------------------------------------------
procedure GICSources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit = NIL then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, GICsourceClass.ElementList, True);
end;

procedure GICSources_Get_AllNames_GR(); CDECL;
// Same as GICSources_Get_AllNames but uses global result (GR) pointers
begin
    GICSources_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;
//------------------------------------------------------------------------------
function GICSources_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := GICsourceClass.ElementList.ListSize;
end;
//------------------------------------------------------------------------------
function GICSources_Get_First(): Integer; CDECL;
var
    pElem: TGICSourceObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pElem := GICsourceClass.ElementList.First;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pElem;
                    Result := 1;
                end
                else
                    pElem := GICsourceClass.ElementList.Next;
            until (Result = 1) or (pElem = NIL);
    end;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Next(): Integer; CDECL;
var
    pElem: TGICSourceObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pElem := GICsourceClass.ElementList.Next;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pElem;
                    Result := GICsourceClass.ElementList.ActiveIndex;
                end
                else
                    pElem := GICsourceClass.ElementList.Next;
            until (Result > 0) or (pElem = NIL);
    end;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Name_AnsiString(): Ansistring; inline;
var
    elem: TDSSCktElement;
begin
    Result := '';
    if ActiveCircuit = NIL then
        Exit;
    elem := ActiveCircuit.ActiveCktElement;
    if elem <> NIL then
        Result := elem.Name;
end;

function GICSources_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(GICSources_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit = NIL then
        Exit;
    if GICsourceClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := GICsourceClass.ElementList.Active;
    end
    else
    begin
        DoSimpleMsg('GICSource "' + Value + '" Not Found in Active Circuit.', 77003);
    end;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Phases(): Integer; CDECL;
var
    elem: TGICSourceObj;
begin
    Result := 0;
    elem := GICsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.NPhases;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Phases(Value: Integer); CDECL;
var
    elem: TGICSourceObj;
begin
    elem := GICsourceClass.GetActiveObj;
    if elem <> NIL then
    begin
        elem.nphases := Value;
        Elem.NConds := Value;  // Force reallocation of terminal info
    end;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Bus1_AnsiString(): Ansistring; inline;
begin
    Result := '';
    if ActiveCircuit = NIL then
        Exit;
    Result := ActiveCircuit.ActiveCktElement.GetBus(1);
end;

function GICSources_Get_Bus1(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(GICSources_Get_Bus1_AnsiString());
end;
//------------------------------------------------------------------------------
function GICSources_Get_Bus2_AnsiString(): Ansistring; inline;
begin
    Result := '';
    if ActiveCircuit = NIL then
        Exit;
    Result := ActiveCircuit.ActiveCktElement.GetBus(2);
end;

function GICSources_Get_Bus2(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(GICSources_Get_Bus2_AnsiString());
end;
//------------------------------------------------------------------------------
function GICSources_Get_EN(): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.ENorth;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_EN(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        elem.ENorth := Value;
end;
//------------------------------------------------------------------------------
function GICSources_Get_EE(): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.EEast;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_EE(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        elem.EEast := Value;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Lat1(): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.Lat1;

end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lat1(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
    begin
        elem.Lat1 := Value;
        elem.VoltsSpecified := FALSE;
    end;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Lat2(): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.Lat2;

end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lat2(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
    begin
        elem.Lat2 := Value;
        elem.VoltsSpecified := FALSE;
    end;

end;
//------------------------------------------------------------------------------
function GICSources_Get_Lon1(): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.Lon1;

end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lon1(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
    begin
        elem.Lon1 := Value;
        elem.VoltsSpecified := FALSE;
    end;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Lon2(): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.Lon2;

end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lon2(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
    begin
        elem.Lon2 := Value;
        elem.VoltsSpecified := FALSE;
    end;

end;
//------------------------------------------------------------------------------
function GICSources_Get_Volts(): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.Volts;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Volts(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
    begin
        elem.Volts := Value;
        elem.VoltsSpecified := TRUE;
    end;
end;
//------------------------------------------------------------------------------
function GICSources_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := GICSourceClass.ElementList.ActiveIndex
    else
        Result := 0
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_idx(Value: Integer); CDECL;
var
    elem: TGICsourceObj;
begin
    if ActiveCircuit = NIL then
        Exit;

    elem := GICSourceClass.ElementList.Get(Value);
    if elem = NIL then
    begin
        DoSimpleMsg('Invalid GICSource index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := elem;
end;
//------------------------------------------------------------------------------
end.
