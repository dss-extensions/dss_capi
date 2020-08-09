unit CAPI_GICSources;

interface

uses
    CAPI_Utils;

procedure GICSources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
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
function _activeObj(out obj: TGICSourceObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
    
    obj := GICsourceClass.ElementList.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active GICSource object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure GICSources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, GICsourceClass.ElementList, True);
end;
//------------------------------------------------------------------------------
function GICSources_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := GICsourceClass.ElementList.ListSize;
end;
//------------------------------------------------------------------------------
function GICSources_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := Generic_CktElement_Get_First(GICsourceClass.ElementList);
end;
//------------------------------------------------------------------------------
function GICSources_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := Generic_CktElement_Get_Next(GICsourceClass.ElementList);
end;
//------------------------------------------------------------------------------
function GICSources_Get_Name(): PAnsiChar; CDECL;
var
    elem: TGICSourceObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(elem.Name);
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit then
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
    if not _activeObj(elem) then
        Exit;
    
    Result := elem.NPhases;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Phases(Value: Integer); CDECL;
var
    elem: TGICSourceObj;
begin
    if not _activeObj(elem) then
        Exit;

    elem.nphases := Value;
    Elem.NConds := Value;  // Force reallocation of terminal info
end;
//------------------------------------------------------------------------------
function GICSources_Get_Bus1(): PAnsiChar; CDECL;
var
    elem: TGICSourceObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(elem.GetBus(1));
end;
//------------------------------------------------------------------------------
function GICSources_Get_Bus2(): PAnsiChar; CDECL;
var
    elem: TGICSourceObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(elem.GetBus(2));
end;
//------------------------------------------------------------------------------
function GICSources_Get_EN(): Double; CDECL;
var
    elem: TGICSourceObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
        
    Result := elem.ENorth;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_EN(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    if not _activeObj(elem) then
        Exit;
    
    elem.ENorth := Value;
end;
//------------------------------------------------------------------------------
function GICSources_Get_EE(): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    
    Result := elem.EEast;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_EE(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    if not _activeObj(elem) then
        Exit;
    
    elem.EEast := Value;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Lat1(): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;

    Result := elem.Lat1;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lat1(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    if not _activeObj(elem) then
        Exit;

    elem.Lat1 := Value;
    elem.VoltsSpecified := FALSE;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Lat2(): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    
    Result := elem.Lat2;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lat2(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    if not _activeObj(elem) then
        Exit;

    elem.Lat2 := Value;
    elem.VoltsSpecified := FALSE;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Lon1(): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;

    Result := elem.Lon1;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lon1(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    if not _activeObj(elem) then
        Exit;

    elem.Lon1 := Value;
    elem.VoltsSpecified := FALSE;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Lon2(): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;

    Result := elem.Lon2;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lon2(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    if not _activeObj(elem) then
        Exit;

    elem.Lon2 := Value;
    elem.VoltsSpecified := FALSE;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Volts(): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;

    Result := elem.Volts;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Volts(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    if not _activeObj(elem) then
        Exit;

    elem.Volts := Value;
    elem.VoltsSpecified := TRUE;
end;
//------------------------------------------------------------------------------
function GICSources_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := GICSourceClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_idx(Value: Integer); CDECL;
var
    elem: TGICsourceObj;
begin
    if InvalidCircuit then
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
