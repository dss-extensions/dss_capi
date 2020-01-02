unit CAPI_ISources;

{$inline on}

interface

uses
    CAPI_Utils;

procedure ISources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
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
    PointerList,
    Isource,
    DSSGlobals,
    CktElement,
    SysUtils,
    DSSClass,
    DSSHelper;

procedure ISources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.IsourceClass.ElementList, True);
end;
//------------------------------------------------------------------------------
function ISources_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.IsourceClass.ElementList.ListSize;
end;
//------------------------------------------------------------------------------
function ISources_Get_First(): Integer; CDECL;
var
    pElem: TIsourceObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pElem := DSSPrime.IsourceClass.ElementList.First;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    DSSPrime.ActiveCircuit.ActiveCktElement := pElem;
                    Result := 1;
                end
                else
                    pElem := DSSPrime.IsourceClass.ElementList.Next;
            until (Result = 1) or (pElem = NIL);
    end;
end;
//------------------------------------------------------------------------------
function ISources_Get_Next(): Integer; CDECL;
var
    pElem: TIsourceObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pElem := DSSPrime.IsourceClass.ElementList.Next;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    DSSPrime.ActiveCircuit.ActiveCktElement := pElem;
                    Result := DSSPrime.IsourceClass.ElementList.ActiveIndex;
                end
                else
                    pElem := DSSPrime.IsourceClass.ElementList.Next;
            until (Result > 0) or (pElem = NIL);
    end;
end;
//------------------------------------------------------------------------------
function ISources_Get_Name(): PAnsiChar; CDECL;
var
    elem: TDSSCktElement;
begin
    Result := nil;
    if DSSPrime.ActiveCircuit = NIL then Exit;
    elem := DSSPrime.ActiveCircuit.ActiveCktElement;
    if elem <> NIL then
        Result := DSS_GetAsPAnsiChar(elem.Name);
end;
//------------------------------------------------------------------------------
procedure ISources_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name

begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        if DSSPrime.IsourceClass.SetActive(Value) then
        begin
            DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.IsourceClass.ElementList.Active;
        end
        else
        begin
            DoSimpleMsg(DSSPrime, 'ISource "' + Value + '" Not Found in Active Circuit.', 77003);
        end;
    end;
end;
//------------------------------------------------------------------------------
function ISources_Get_Amps(): Double; CDECL;
var
    elem: TIsourceObj;
begin
    Result := 0.0;
    elem := DSSPrime.IsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.Amps;
end;
//------------------------------------------------------------------------------
procedure ISources_Set_Amps(Value: Double); CDECL;
var
    elem: TIsourceObj;
begin
    elem := DSSPrime.IsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.Amps := Value;
end;
//------------------------------------------------------------------------------
function ISources_Get_AngleDeg(): Double; CDECL;
var
    elem: TIsourceObj;
begin
    Result := 0.0;
    elem := DSSPrime.IsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.Angle;
end;
//------------------------------------------------------------------------------
function ISources_Get_Frequency(): Double; CDECL;
var
    elem: TIsourceObj;
begin
    Result := 0.0;
    elem := DSSPrime.IsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.SrcFrequency;
end;
//------------------------------------------------------------------------------
procedure ISources_Set_AngleDeg(Value: Double); CDECL;
var
    elem: TIsourceObj;
begin
    elem := DSSPrime.IsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.Angle := Value;
end;
//------------------------------------------------------------------------------
procedure ISources_Set_Frequency(Value: Double); CDECL;
var
    elem: TIsourceObj;
begin
    elem := DSSPrime.IsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.SrcFrequency := Value;
end;
//------------------------------------------------------------------------------
function ISources_Get_idx(): Integer; CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.IsourceClass.ElementList.ActiveIndex
    else
        Result := 0
end;
//------------------------------------------------------------------------------
procedure ISources_Set_idx(Value: Integer); CDECL;
var
    pISource: TISourceObj;
begin
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    pISource := DSSPrime.IsourceClass.ElementList.Get(Value);
    if pISource = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid ISource index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pISource;
end;
//------------------------------------------------------------------------------
end.
