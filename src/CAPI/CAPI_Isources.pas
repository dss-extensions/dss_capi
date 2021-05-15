unit CAPI_ISources;

interface

uses
    CAPI_Utils;

procedure ISources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure ISources_Get_AllNames_GR(); CDECL;
function ISources_Get_Count(): Integer; CDECL;
function ISources_Get_First(): Integer; CDECL;
function ISources_Get_Next(): Integer; CDECL;
function ISources_Get_Name(): PAnsiChar; CDECL;
procedure ISources_Set_Name(const Value: PAnsiChar); CDECL;
function ISources_Get_Amps(): Double; CDECL;
procedure ISources_Set_Amps(Value: Double); CDECL;
function ISources_Get_AngleDeg(): Double; CDECL;
function ISources_Get_Frequency(): Double; CDECL;
procedure ISources_Set_AngleDeg(Value: Double); CDECL;
procedure ISources_Set_Frequency(Value: Double); CDECL;

// API Extensions
function ISources_Get_idx(): Integer; CDECL;
procedure ISources_Set_idx(Value: Integer); CDECL;

implementation

uses
    CAPI_Constants,
    DSSPointerList,
    Isource,
    DSSGlobals,
    CktElement,
    SysUtils,
    DSSClass,
    DSSHelper;

//------------------------------------------------------------------------------
function _activeObj(DSSPrime: TDSSContext; out obj: TIsourceObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
    
    obj := DSSPrime.IsourceClass.GetActiveObj();
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, 'No active ISource object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure ISources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;

    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.IsourceClass.ElementList, True);
end;

procedure ISources_Get_AllNames_GR(); CDECL;
// Same as ISources_Get_AllNames but uses global result (GR) pointers
begin
    ISources_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function ISources_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;

    Result := DSSPrime.IsourceClass.ElementList.Count;
end;
//------------------------------------------------------------------------------
function ISources_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime.IsourceClass.ElementList);
end;
//------------------------------------------------------------------------------
function ISources_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime.IsourceClass.ElementList);
end;
//------------------------------------------------------------------------------
function ISources_Get_Name(): PAnsiChar; CDECL;
var
    elem: TIsourceObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem.Name);
end;
//------------------------------------------------------------------------------
procedure ISources_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name

begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    if DSSPrime.IsourceClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.IsourceClass.ElementList.Active;
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'ISource "' + Value + '" Not Found in Active Circuit.', 77003);
    end;
end;
//------------------------------------------------------------------------------
function ISources_Get_Amps(): Double; CDECL;
var
    elem: TIsourceObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Amps;
end;
//------------------------------------------------------------------------------
procedure ISources_Set_Amps(Value: Double); CDECL;
var
    elem: TIsourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.Amps := Value;
end;
//------------------------------------------------------------------------------
function ISources_Get_AngleDeg(): Double; CDECL;
var
    elem: TIsourceObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Angle;
end;
//------------------------------------------------------------------------------
function ISources_Get_Frequency(): Double; CDECL;
var
    elem: TIsourceObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.SrcFrequency;
end;
//------------------------------------------------------------------------------
procedure ISources_Set_AngleDeg(Value: Double); CDECL;
var
    elem: TIsourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.Angle := Value;
end;
//------------------------------------------------------------------------------
procedure ISources_Set_Frequency(Value: Double); CDECL;
var
    elem: TIsourceObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.SrcFrequency := Value;
end;
//------------------------------------------------------------------------------
function ISources_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ISourceClass.ElementList.ActiveIndex;
end;
//------------------------------------------------------------------------------
procedure ISources_Set_idx(Value: Integer); CDECL;
var
    pISource: TISourceObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    pISource := DSSPrime.ISourceClass.ElementList.Get(Value);
    if pISource = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid ISource index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pISource;
end;
//------------------------------------------------------------------------------
end.
