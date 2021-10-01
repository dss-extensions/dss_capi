unit CAPI_Storages;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure Storages_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
function Storages_Get_Count(): Integer; CDECL;
function Storages_Get_First(): Integer; CDECL;
function Storages_Get_Name(): PAnsiChar; CDECL;
function Storages_Get_Next(): Integer; CDECL;
function Storages_Get_idx(): Integer; CDECL;
procedure Storages_Set_idx(Value: Integer); CDECL;
procedure Storages_Set_Name(const Value: PAnsiChar); CDECL;
procedure Storages_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Storages_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Storages_Get_RegisterValues_GR(); CDECL;
function Storages_Get_puSOC(): Double; CDECL;
procedure Storages_Set_puSOC(Value: Double); CDECL;
function Storages_Get_State(): Integer; CDECL;
procedure Storages_Set_State(Value: Integer); CDECL;

implementation

uses
    CAPI_Constants,
    Executive,
    Sysutils,
    Storage2,
    DSSPointerlist,
    DSSGlobals,
    DSSClass,
    DSSHelper;


function OldModels(DSS: TDSSContext): Boolean;
begin
    Result := DSS_CAPI_LEGACY_MODELS;
    if DSS_CAPI_LEGACY_MODELS then
        DoSimpleMsg(DSS, 'The Storages API is not available in the legacy-models mode!', 18990);
end;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TStorage2Obj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) or OldModels(DSS) then
        Exit;
    
    obj := DSS.ActiveCircuit.StorageElements.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active Storage object found! Activate one and retry.', 18989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Storages_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) or OldModels(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.StorageElements, False);
end;
//------------------------------------------------------------------------------
function Storages_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) or OldModels(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.StorageElements.Count;
end;
//------------------------------------------------------------------------------
function Storages_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) or OldModels(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.StorageElements);
end;
//------------------------------------------------------------------------------
function Storages_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) or OldModels(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.StorageElements);
end;
//------------------------------------------------------------------------------
function Storages_Get_Name(): PAnsiChar; CDECL;
var
    elem: TStorage2Obj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
procedure Storages_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name
begin
    if InvalidCircuit(DSSPrime) or OldModels(DSSPrime) then
        Exit;
    if DSSPrime.Storage2Class.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.Storage2Class.ElementList.Active;
        DSSPrime.ActiveCircuit.StorageElements.Get(DSSPrime.Storage2Class.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Storage "' + Value + '" Not Found in Active Circuit.', 77003);
    end;
end;
//------------------------------------------------------------------------------
function Storages_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) or OldModels(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.StorageElements.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Storages_Set_idx(Value: Integer); CDECL;
var
    pStorage: TStorage2Obj;
begin
    if InvalidCircuit(DSSPrime) or OldModels(DSSPrime) then
        Exit;
    pStorage := DSSPrime.ActiveCircuit.StorageElements.Get(Value);
    if pStorage = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid Storage index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pStorage;
end;
//------------------------------------------------------------------------------
procedure Storages_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    k: Integer;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumStorage2Registers);
    for k := 0 to NumStorage2Registers - 1 do
    begin
        Result[k] := DSS_CopyStringAsPChar(DSSPrime.Storage2Class.RegisterNames[k + 1]);
    end;
end;
//------------------------------------------------------------------------------
procedure Storages_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    elem: TStorage2Obj;
    k: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NumStorage2Registers);
    for k := 0 to NumStorage2Registers - 1 do
    begin
        Result[k] := elem.Registers[k + 1];
    end;
end;

procedure Storages_Get_RegisterValues_GR(); CDECL;
// Same as Storages_Get_RegisterValues but uses global result (GR) pointers
begin
    Storages_Get_RegisterValues(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;
//------------------------------------------------------------------------------
function Storages_Get_puSOC(): Double; CDECL;
var
    elem: TStorage2Obj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Storagevars.kWhStored / elem.StorageVars.kWhRating;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_puSOC(Value: Double); CDECL;
var
    elem: TStorage2Obj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.Storagevars.kWhStored := elem.StorageVars.kWhRating * Value;
end;
//------------------------------------------------------------------------------
function Storages_Get_State(): Integer; CDECL;
var
    elem: TStorage2Obj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.StorageState;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_State(Value: Integer); CDECL;
var
    elem: TStorage2Obj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if (Value <> STORE_CHARGING) and 
       (Value <> STORE_IDLING) and
       (Value <> STORE_DISCHARGING) then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid Storage state: "' + IntToStr(Value) + '".', 656568);
    end;
    elem.StorageState := Value;
end;
//------------------------------------------------------------------------------
end.
