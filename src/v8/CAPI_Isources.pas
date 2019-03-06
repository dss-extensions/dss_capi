unit CAPI_ISources;

{$inline on}

interface

uses
    CAPI_Utils;

procedure ISources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
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
    PointerList,
    Isource,
    DSSGlobals,
    CktElement,
    SysUtils;

procedure ISources_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    elem: TIsourceObj;
    pList: TPointerList;
    k: Integer;

begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        if ISourceClass[ActiveActor].ElementList.ListSize > 0 then
        begin
            pList := ISourceClass[ActiveActor].ElementList;
            DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (pList.ListSize - 1) + 1);
            k := 0;
            elem := pList.First;
            while elem <> NIL do
            begin
                Result[k] := DSS_CopyStringAsPChar(elem.Name);
                Inc(k);
                elem := pList.next;
            end;
        end;
    end;

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
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ISourceClass[ActiveActor].ElementList.ListSize;
end;
//------------------------------------------------------------------------------
function ISources_Get_First(): Integer; CDECL;
var
    pElem: TIsourceObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pElem := ISourceClass[ActiveActor].ElementList.First;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                    Result := 1;
                end
                else
                    pElem := ISourceClass[ActiveActor].ElementList.Next;
            until (Result = 1) or (pElem = NIL);
    end;
end;
//------------------------------------------------------------------------------
function ISources_Get_Next(): Integer; CDECL;
var
    pElem: TIsourceObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pElem := ISourceClass[ActiveActor].ElementList.Next;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                    Result := ISourceClass[ActiveActor].ElementList.ActiveIndex;
                end
                else
                    pElem := ISourceClass[ActiveActor].ElementList.Next;
            until (Result > 0) or (pElem = NIL);
    end;
end;
//------------------------------------------------------------------------------
function ISources_Get_Name_AnsiString(): Ansistring; inline;
var
    elem: TDSSCktElement;
begin
    Result := '';
    elem := ActiveCircuit[ActiveActor].ActiveCktElement;
    if elem <> NIL then
        Result := elem.Name;
end;

function ISources_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(ISources_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure ISources_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name

begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        if ISourceClass[ActiveActor].SetActive(Value) then
        begin
            ActiveCircuit[ActiveActor].ActiveCktElement := ISourceClass[ActiveActor].ElementList.Active;
        end
        else
        begin
            DoSimpleMsg('ISource "' + Value + '" Not Found in Active Circuit.', 77003);
        end;
    end;
end;
//------------------------------------------------------------------------------
function ISources_Get_Amps(): Double; CDECL;
var
    elem: TIsourceObj;
begin
    Result := 0.0;
    elem := ISourceClass[ActiveActor].ElementList.Active;
    if elem <> NIL then
        Result := elem.Amps;
end;
//------------------------------------------------------------------------------
procedure ISources_Set_Amps(Value: Double); CDECL;
var
    elem: TIsourceObj;
begin
    elem := ISourceClass[ActiveActor].GetActiveObj;
    if elem <> NIL then
        elem.Amps := Value;
end;
//------------------------------------------------------------------------------
function ISources_Get_AngleDeg(): Double; CDECL;
var
    elem: TIsourceObj;
begin
    Result := 0.0;
    elem := ISourceClass[ActiveActor].ElementList.Active;
    if elem <> NIL then
        Result := elem.Angle;
end;
//------------------------------------------------------------------------------
function ISources_Get_Frequency(): Double; CDECL;
var
    elem: TIsourceObj;
begin
    Result := 0.0;
    elem := ISourceClass[ActiveActor].ElementList.Active;
    if elem <> NIL then
        Result := elem.SrcFrequency;
end;
//------------------------------------------------------------------------------
procedure ISources_Set_AngleDeg(Value: Double); CDECL;
var
    elem: TIsourceObj;
begin
    elem := ISourceClass[ActiveActor].GetActiveObj;
    if elem <> NIL then
        elem.Angle := Value;
end;
//------------------------------------------------------------------------------
procedure ISources_Set_Frequency(Value: Double); CDECL;
var
    elem: TIsourceObj;
begin
    elem := ISourceClass[ActiveActor].GetActiveObj;
    if elem <> NIL then
        elem.SrcFrequency := Value;
end;
//------------------------------------------------------------------------------
function ISources_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ISourceClass[ActiveActor].ElementList.ActiveIndex
    else
        Result := 0
end;
//------------------------------------------------------------------------------
procedure ISources_Set_idx(Value: Integer); CDECL;
var
    pISource: TISourceObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    pISource := ISourceClass[ActiveActor].ElementList.Get(Value);
    if pISource = NIL then
    begin
        DoSimpleMsg('Invalid ISource index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit[ActiveActor].ActiveCktElement := pISource;
end;
//------------------------------------------------------------------------------
end.
