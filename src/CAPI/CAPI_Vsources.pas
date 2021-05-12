unit CAPI_Vsources;

interface

uses
    CAPI_Utils;

procedure Vsources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Vsources_Get_AllNames_GR(); CDECL;
function Vsources_Get_Count(): Integer; CDECL;
function Vsources_Get_First(): Integer; CDECL;
function Vsources_Get_Next(): Integer; CDECL;
function Vsources_Get_Name(): PAnsiChar; CDECL;
procedure Vsources_Set_Name(const Value: PAnsiChar); CDECL;
function Vsources_Get_BasekV(): Double; CDECL;
function Vsources_Get_pu(): Double; CDECL;
procedure Vsources_Set_BasekV(Value: Double); CDECL;
procedure Vsources_Set_pu(Value: Double); CDECL;
function Vsources_Get_AngleDeg(): Double; CDECL;
function Vsources_Get_Frequency(): Double; CDECL;
function Vsources_Get_Phases(): Integer; CDECL;
procedure Vsources_Set_AngleDeg(Value: Double); CDECL;
procedure Vsources_Set_Frequency(Value: Double); CDECL;
procedure Vsources_Set_Phases(Value: Integer); CDECL;

function Vsources_Get_idx(): Integer; CDECL;
procedure Vsources_Set_idx(Value: Integer); CDECL;


implementation

uses
    CAPI_Constants,
    Vsource,
    DSSPointerList,
    DSSGlobals,
    CktElement,
    SysUtils;

//------------------------------------------------------------------------------
function _activeObj(out obj: TVsourceObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
    
    obj := VsourceClass.ElementList.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active Vsource object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Vsources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, VsourceClass.ElementList, False);
end;

procedure Vsources_Get_AllNames_GR(); CDECL;
// Same as Vsources_Get_AllNames but uses global result (GR) pointers
begin
    Vsources_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Vsources_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := VsourceClass.ElementList.Count;
end;
//------------------------------------------------------------------------------
function Vsources_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := Generic_CktElement_Get_First(VsourceClass.ElementList);
end;
//------------------------------------------------------------------------------
function Vsources_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := Generic_CktElement_Get_Next(VsourceClass.ElementList);
end;
//------------------------------------------------------------------------------
function Vsources_Get_Name(): PAnsiChar; CDECL;
var
    elem: TVsourceObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem.Name);
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name
begin
    if InvalidCircuit then
        Exit;
        
    if VsourceClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := VsourceClass.ElementList.Active;
    end
    else
    begin
        DoSimpleMsg('Vsource "' + Value + '" Not Found in Active Circuit.', 77003);
    end;
end;
//------------------------------------------------------------------------------
function Vsources_Get_BasekV(): Double; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.kVBase;
end;
//------------------------------------------------------------------------------
function Vsources_Get_pu(): Double; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.perunit;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_BasekV(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.kVBase := Value;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_pu(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.PerUnit := Value;
end;
//------------------------------------------------------------------------------
function Vsources_Get_AngleDeg(): Double; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.angle;
end;
//------------------------------------------------------------------------------
function Vsources_Get_Frequency(): Double; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.SrcFrequency;
end;
//------------------------------------------------------------------------------
function Vsources_Get_Phases(): Integer; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.NPhases;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_AngleDeg(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.Angle := Value;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_Frequency(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.SrcFrequency := Value;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_Phases(Value: Integer); CDECL;
var
    elem: TVsourceObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.Nphases := Value;
end;
//------------------------------------------------------------------------------
function Vsources_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := VsourceClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_idx(Value: Integer); CDECL;
var
    pVsource: TVsourceObj;
begin
    if InvalidCircuit then
        Exit;
    pVsource := VsourceClass.ElementList.Get(Value);
    if pVsource = NIL then
    begin
        DoSimpleMsg('Invalid VSource index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := pVsource;
end;
//------------------------------------------------------------------------------
end.
