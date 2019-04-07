unit CAPI_Vsources;

{$inline on}

interface

uses
    CAPI_Utils;

procedure Vsources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
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
    PointerList,
    DSSGlobals,
    CktElement,
    SysUtils;

procedure Vsources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, VSourceClass[ActiveActor].ElementList, False);
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
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := VSourceClass[ActiveActor].ElementList.ListSize;
end;
//------------------------------------------------------------------------------
function Vsources_Get_First(): Integer; CDECL;
var
    pElem: TVsourceObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pElem := VSourceClass[ActiveActor].ElementList.First;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                    Result := 1;
                end
                else
                    pElem := VSourceClass[ActiveActor].ElementList.Next;
            until (Result = 1) or (pElem = NIL);
    end;
end;
//------------------------------------------------------------------------------
function Vsources_Get_Next(): Integer; CDECL;
var
    pElem: TVsourceObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pElem := VSourceClass[ActiveActor].ElementList.Next;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                    Result := VSourceClass[ActiveActor].ElementList.ActiveIndex;
                end
                else
                    pElem := VSourceClass[ActiveActor].ElementList.Next;
            until (Result > 0) or (pElem = NIL);
    end;
end;
//------------------------------------------------------------------------------
function Vsources_Get_Name_AnsiString(): Ansistring; inline;
var
    elem: TDSSCktElement;
begin
    Result := '';
    elem := ActiveCircuit[ActiveActor].ActiveCktElement;
    if elem <> NIL then
        Result := elem.Name;
end;

function Vsources_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Vsources_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name

begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        if VSourceClass[ActiveActor].SetActive(Value) then
        begin
            ActiveCircuit[ActiveActor].ActiveCktElement := VSourceClass[ActiveActor].ElementList.Active;
        end
        else
        begin
            DoSimpleMsg('Vsource "' + Value + '" Not Found in Active Circuit.', 77003);
        end;
    end;
end;
//------------------------------------------------------------------------------
function Vsources_Get_BasekV(): Double; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    elem := VSourceClass[ActiveActor].ElementList.Active;
    if elem <> NIL then
        Result := elem.kVBase;
end;
//------------------------------------------------------------------------------
function Vsources_Get_pu(): Double; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    elem := VSourceClass[ActiveActor].ElementList.Active;
    if elem <> NIL then
        Result := elem.perunit;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_BasekV(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    elem := VSourceClass[ActiveActor].GetActiveObj;
    if elem <> NIL then
        elem.kVBase := Value;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_pu(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    elem := VSourceClass[ActiveActor].GetActiveObj;
    if elem <> NIL then
        elem.PerUnit := Value;
end;
//------------------------------------------------------------------------------
function Vsources_Get_AngleDeg(): Double; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    elem := VSourceClass[ActiveActor].ElementList.Active;
    if elem <> NIL then
        Result := elem.angle;

end;
//------------------------------------------------------------------------------
function Vsources_Get_Frequency(): Double; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    elem := VSourceClass[ActiveActor].ElementList.Active;
    if elem <> NIL then
        Result := elem.SrcFrequency;

end;
//------------------------------------------------------------------------------
function Vsources_Get_Phases(): Integer; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0;
    elem := VSourceClass[ActiveActor].ElementList.Active;
    if elem <> NIL then
        Result := elem.NPhases;

end;
//------------------------------------------------------------------------------
procedure Vsources_Set_AngleDeg(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    elem := VSourceClass[ActiveActor].GetActiveObj;
    if elem <> NIL then
        elem.Angle := Value;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_Frequency(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    elem := VSourceClass[ActiveActor].GetActiveObj;
    if elem <> NIL then
        elem.SrcFrequency := Value;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_Phases(Value: Integer); CDECL;
var
    elem: TVsourceObj;
begin
    elem := VSourceClass[ActiveActor].GetActiveObj;
    if elem <> NIL then
        elem.Nphases := Value;
end;
//------------------------------------------------------------------------------
function Vsources_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := VSourceClass[ActiveActor].ElementList.ActiveIndex
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_idx(Value: Integer); CDECL;
var
    pVsource: TVsourceObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    pVsource := VSourceClass[ActiveActor].ElementList.Get(Value);
    if pVsource = NIL then
    begin
        DoSimpleMsg('Invalid VSource index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit[ActiveActor].ActiveCktElement := pVsource;
end;
//------------------------------------------------------------------------------
end.
