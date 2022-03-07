unit CAPI_GICSources;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure GICSources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
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
    DSSPointerList,
    DSSGlobals,
    CktElement,
    SysUtils,
    DSSClass,
    DSSHelper;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TGICSourceObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.GICsourceClass.ElementList.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['GICSource'], 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure GICSources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.GICsourceClass.ElementList, True);
end;

procedure GICSources_Get_AllNames_GR(); CDECL;
// Same as GICSources_Get_AllNames but uses global result (GR) pointers
begin
    GICSources_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;
//------------------------------------------------------------------------------
function GICSources_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.GICsourceClass.ElementList.Count;
end;
//------------------------------------------------------------------------------
function GICSources_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.GICsourceClass.ElementList);
end;
//------------------------------------------------------------------------------
function GICSources_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.GICsourceClass.ElementList);
end;
//------------------------------------------------------------------------------
function GICSources_Get_Name(): PAnsiChar; CDECL;
var
    elem: TGICSourceObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.GICsourceClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.GICsourceClass.ElementList.Active;
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'GICSource "%s" not found in Active Circuit.', [Value], 77003);
    end;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Phases(): Integer; CDECL;
var
    elem: TGICSourceObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    
    Result := elem.NPhases;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Phases(Value: Integer); CDECL;
var
    elem: TGICSourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if Value < 1 then
    begin
        DoSimpleMsg(DSSPrime, '%s: Number of phases must be a positive integer!', [elem.FullName], 6568);
        Exit;
    end;
    elem.Fnphases := Value;
    Elem.NConds := Value;  // Force reallocation of terminal info
end;
//------------------------------------------------------------------------------
function GICSources_Get_Bus1(): PAnsiChar; CDECL;
var
    elem: TGICSourceObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.GetBus(1));
end;
//------------------------------------------------------------------------------
function GICSources_Get_Bus2(): PAnsiChar; CDECL;
var
    elem: TGICSourceObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.GetBus(2));
end;
//------------------------------------------------------------------------------
function GICSources_Get_EN(): Double; CDECL;
var
    elem: TGICSourceObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
        
    Result := elem.ENorth;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_EN(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    
    elem.ENorth := Value;
end;
//------------------------------------------------------------------------------
function GICSources_Get_EE(): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    
    Result := elem.EEast;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_EE(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    
    elem.EEast := Value;
end;
//------------------------------------------------------------------------------
function GICSources_Get_Lat1(): Double; CDECL;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.Lat1;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lat1(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
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
    if not _activeObj(DSSPrime, elem) then
        Exit;
    
    Result := elem.Lat2;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lat2(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
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
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.Lon1;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lon1(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
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
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.Lon2;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Lon2(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
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
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.Volts;
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_Volts(Value: Double); CDECL;
var
    elem: TGICsourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.Volts := Value;
    elem.VoltsSpecified := TRUE;
end;
//------------------------------------------------------------------------------
function GICSources_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.GICSourceClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure GICSources_Set_idx(Value: Integer); CDECL;
var
    elem: TGICsourceObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    elem := DSSPrime.GICSourceClass.ElementList.Get(Value);
    if elem = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['GICSource', Value], 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := elem;
end;
//------------------------------------------------------------------------------
end.
