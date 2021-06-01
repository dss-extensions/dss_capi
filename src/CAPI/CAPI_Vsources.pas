unit CAPI_Vsources;

interface

uses
    CAPI_Utils,
    CAPI_Types;

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
    SysUtils,
    DSSClass,
    DSSHelper;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TVsourceObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.VsourceClass.ElementList.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active Vsource object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Vsources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.VsourceClass.ElementList, False);
end;

procedure Vsources_Get_AllNames_GR(); CDECL;
// Same as Vsources_Get_AllNames but uses global result (GR) pointers
begin
    Vsources_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Vsources_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.VsourceClass.ElementList.Count;
end;
//------------------------------------------------------------------------------
function Vsources_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.VsourceClass.ElementList);
end;
//------------------------------------------------------------------------------
function Vsources_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.VsourceClass.ElementList);
end;
//------------------------------------------------------------------------------
function Vsources_Get_Name(): PAnsiChar; CDECL;
var
    elem: TVsourceObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
        
    if DSSPrime.VsourceClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.VsourceClass.ElementList.Active;
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Vsource "' + Value + '" Not Found in Active Circuit.', 77003);
    end;
end;
//------------------------------------------------------------------------------
function Vsources_Get_BasekV(): Double; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.kVBase;
end;
//------------------------------------------------------------------------------
function Vsources_Get_pu(): Double; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.perunit;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_BasekV(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.kVBase := Value;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_pu(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.PerUnit := Value;
end;
//------------------------------------------------------------------------------
function Vsources_Get_AngleDeg(): Double; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.angle;
end;
//------------------------------------------------------------------------------
function Vsources_Get_Frequency(): Double; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.SrcFrequency;
end;
//------------------------------------------------------------------------------
function Vsources_Get_Phases(): Integer; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.NPhases;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_AngleDeg(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.Angle := Value;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_Frequency(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.SrcFrequency := Value;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_Phases(Value: Integer); CDECL;
var
    elem: TVsourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.Nphases := Value;
end;
//------------------------------------------------------------------------------
function Vsources_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.VsourceClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_idx(Value: Integer); CDECL;
var
    pVsource: TVsourceObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    pVsource := DSSPrime.VsourceClass.ElementList.Get(Value);
    if pVsource = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid VSource index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pVsource;
end;
//------------------------------------------------------------------------------
end.
