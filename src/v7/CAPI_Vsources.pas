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
    if ActiveCircuit = NIL then
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
    if ActiveCircuit <> NIL then
        Result := VsourceClass.ElementList.ListSize;
end;
//------------------------------------------------------------------------------
function Vsources_Get_First(): Integer; CDECL;
var
    pElem: TVsourceObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pElem := VsourceClass.ElementList.First;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pElem;
                    Result := 1;
                end
                else
                    pElem := VsourceClass.ElementList.Next;
            until (Result = 1) or (pElem = NIL);
    end;
end;
//------------------------------------------------------------------------------
function Vsources_Get_Next(): Integer; CDECL;
var
    pElem: TVsourceObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pElem := VsourceClass.ElementList.Next;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pElem;
                    Result := VsourceClass.ElementList.ActiveIndex;
                end
                else
                    pElem := VsourceClass.ElementList.Next;
            until (Result > 0) or (pElem = NIL);
    end;
end;
//------------------------------------------------------------------------------
function Vsources_Get_Name_AnsiString(): Ansistring; inline;
var
    elem: TDSSCktElement;
begin
    Result := '';
    if ActiveCircuit = NIL then Exit;
    elem := ActiveCircuit.ActiveCktElement;
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
    if ActiveCircuit <> NIL then
    begin
        if VsourceClass.SetActive(Value) then
        begin
            ActiveCircuit.ActiveCktElement := VsourceClass.ElementList.Active;
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
    elem := VsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.kVBase;
end;
//------------------------------------------------------------------------------
function Vsources_Get_pu(): Double; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    elem := VsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.perunit;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_BasekV(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    elem := VsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.kVBase := Value;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_pu(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    elem := VsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.PerUnit := Value;
end;
//------------------------------------------------------------------------------
function Vsources_Get_AngleDeg(): Double; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    elem := VsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.angle;

end;
//------------------------------------------------------------------------------
function Vsources_Get_Frequency(): Double; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    elem := VsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.SrcFrequency;

end;
//------------------------------------------------------------------------------
function Vsources_Get_Phases(): Integer; CDECL;
var
    elem: TVsourceObj;
begin
    Result := 0;
    elem := VsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.NPhases;

end;
//------------------------------------------------------------------------------
procedure Vsources_Set_AngleDeg(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    elem := VsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.Angle := Value;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_Frequency(Value: Double); CDECL;
var
    elem: TVsourceObj;
begin
    elem := VsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.SrcFrequency := Value;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_Phases(Value: Integer); CDECL;
var
    elem: TVsourceObj;
begin
    elem := VsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.Nphases := Value;
end;
//------------------------------------------------------------------------------
function Vsources_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := VsourceClass.ElementList.ActiveIndex
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Vsources_Set_idx(Value: Integer); CDECL;
var
    pVsource: TVsourceObj;
begin
    if ActiveCircuit = NIL then
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
