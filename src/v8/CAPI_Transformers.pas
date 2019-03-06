unit CAPI_Transformers;

{$inline on}

interface

uses
    CAPI_Utils;

procedure Transformers_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Transformers_Get_AllNames_GR(); CDECL;
function Transformers_Get_First(): Integer; CDECL;
function Transformers_Get_IsDelta(): Wordbool; CDECL;
function Transformers_Get_kV(): Double; CDECL;
function Transformers_Get_kVA(): Double; CDECL;
function Transformers_Get_MaxTap(): Double; CDECL;
function Transformers_Get_MinTap(): Double; CDECL;
function Transformers_Get_Name(): PAnsiChar; CDECL;
function Transformers_Get_Next(): Integer; CDECL;
function Transformers_Get_NumTaps(): Integer; CDECL;
function Transformers_Get_NumWindings(): Integer; CDECL;
function Transformers_Get_R(): Double; CDECL;
function Transformers_Get_Rneut(): Double; CDECL;
function Transformers_Get_Tap(): Double; CDECL;
function Transformers_Get_Wdg(): Integer; CDECL;
function Transformers_Get_XfmrCode(): PAnsiChar; CDECL;
function Transformers_Get_Xhl(): Double; CDECL;
function Transformers_Get_Xht(): Double; CDECL;
function Transformers_Get_Xlt(): Double; CDECL;
function Transformers_Get_Xneut(): Double; CDECL;
procedure Transformers_Set_IsDelta(Value: Wordbool); CDECL;
procedure Transformers_Set_kV(Value: Double); CDECL;
procedure Transformers_Set_kVA(Value: Double); CDECL;
procedure Transformers_Set_MaxTap(Value: Double); CDECL;
procedure Transformers_Set_MinTap(Value: Double); CDECL;
procedure Transformers_Set_Name(const Value: PAnsiChar); CDECL;
procedure Transformers_Set_NumTaps(Value: Integer); CDECL;
procedure Transformers_Set_NumWindings(Value: Integer); CDECL;
procedure Transformers_Set_R(Value: Double); CDECL;
procedure Transformers_Set_Rneut(Value: Double); CDECL;
procedure Transformers_Set_Tap(Value: Double); CDECL;
procedure Transformers_Set_Wdg(Value: Integer); CDECL;
procedure Transformers_Set_XfmrCode(const Value: PAnsiChar); CDECL;
procedure Transformers_Set_Xhl(Value: Double); CDECL;
procedure Transformers_Set_Xht(Value: Double); CDECL;
procedure Transformers_Set_Xlt(Value: Double); CDECL;
procedure Transformers_Set_Xneut(Value: Double); CDECL;
function Transformers_Get_Count(): Integer; CDECL;
procedure Transformers_Get_WdgVoltages(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Transformers_Get_WdgVoltages_GR(); CDECL;
procedure Transformers_Get_WdgCurrents(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Transformers_Get_WdgCurrents_GR(); CDECL;
function Transformers_Get_strWdgCurrents(): PAnsiChar; CDECL;
function Transformers_Get_CoreType(): Integer; CDECL;
procedure Transformers_Set_CoreType(Value: Integer); CDECL;
function Transformers_Get_RdcOhms(): Double; CDECL;
procedure Transformers_Set_RdcOhms(Value: Double); CDECL;

// API extensions
function Transformers_Get_idx(): Integer; CDECL;
procedure Transformers_Set_idx(Value: Integer); CDECL;


implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Executive,
    Transformer,
    SysUtils,
    PointerList,
    ucomplex;

function ActiveTransformer: TTransfObj;
begin
    Result := NIL;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Transformers.Active;
end;

// assuming the active winding has already been set
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('transformer.%s.%s=%s', [ActiveTransformer.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure Transformers_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    elem: TTransfObj;
    lst: TPointerList;
    k: Integer;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if Transformers.ListSize > 0 then
            begin
                lst := Transformers;
                DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (lst.ListSize - 1) + 1);
                k := 0;
                elem := lst.First;
                while elem <> NIL do
                begin
                    Result[k] := DSS_CopyStringAsPChar(elem.Name);
                    Inc(k);
                    elem := lst.Next;
                end;
            end;
end;

procedure Transformers_Get_AllNames_GR(); CDECL;
// Same as Transformers_Get_AllNames but uses global result (GR) pointers
begin
    Transformers_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Transformers_Get_First(): Integer; CDECL;
var
    elem: TTransfObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        lst := ActiveCircuit[ActiveActor].Transformers;
        elem := lst.First;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                    Result := 1;
                end
                else
                    elem := lst.Next;
            until (Result = 1) or (elem = NIL);
        end;
    end;
end;
//------------------------------------------------------------------------------
function Transformers_Get_IsDelta(): Wordbool; CDECL;
var
    elem: TTransfObj;
begin
    Result := FALSE;
    elem := ActiveTransformer;
    if elem <> NIL then
        if elem.WdgConnection[elem.ActiveWinding] > 0 then
            Result := TRUE;
end;
//------------------------------------------------------------------------------
function Transformers_Get_kV(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.Winding^[elem.ActiveWinding].kvll;
end;
//------------------------------------------------------------------------------
function Transformers_Get_kVA(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.WdgKVA[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_MaxTap(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.Maxtap[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_MinTap(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.Mintap[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_Name_AnsiString(): Ansistring; inline;
var
    elem: TTransfObj;
begin
    Result := '';
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        elem := ActiveCircuit[ActiveActor].Transformers.Active;
        if elem <> NIL then
            Result := elem.Name;
    end;
end;

function Transformers_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Transformers_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Transformers_Get_Next(): Integer; CDECL;
var
    elem: TTransfObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        lst := ActiveCircuit[ActiveActor].Transformers;
        elem := lst.Next;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                    Result := lst.ActiveIndex;
                end
                else
                    elem := lst.Next;
            until (Result > 0) or (elem = NIL);
        end
    end;
end;
//------------------------------------------------------------------------------
function Transformers_Get_NumTaps(): Integer; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.NumTaps[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_NumWindings(): Integer; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.NumberOfWindings;
end;
//------------------------------------------------------------------------------
function Transformers_Get_R(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.WdgResistance[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_Rneut(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.WdgRneutral[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_Tap(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.PresentTap[elem.ActiveWinding, ActiveActor];
end;
//------------------------------------------------------------------------------
function Transformers_Get_Wdg(): Integer; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.ActiveWinding;
end;
//------------------------------------------------------------------------------
function Transformers_Get_XfmrCode_AnsiString(): Ansistring; inline;
var
    elem: TTransfObj;
begin
    Result := '';
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.XfmrCode;
end;

function Transformers_Get_XfmrCode(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Transformers_Get_XfmrCode_AnsiString());
end;
//------------------------------------------------------------------------------
function Transformers_Get_Xhl(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.XhlVal;
end;
//------------------------------------------------------------------------------
function Transformers_Get_Xht(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.XhtVal;
end;
//------------------------------------------------------------------------------
function Transformers_Get_Xlt(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.XltVal;
end;
//------------------------------------------------------------------------------
function Transformers_Get_Xneut(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.WdgXneutral[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_IsDelta(Value: Wordbool); CDECL;
begin
    if Value = TRUE then
        Set_Parameter('Conn', 'Delta')
    else
        Set_Parameter('Conn', 'Wye')
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_kV(Value: Double); CDECL;
begin
    Set_Parameter('kv', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_kVA(Value: Double); CDECL;
begin
    Set_Parameter('kva', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_MaxTap(Value: Double); CDECL;
begin
    Set_Parameter('MaxTap', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_MinTap(Value: Double); CDECL;
begin
    Set_Parameter('MinTap', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    if TransformerClass[ActiveActor].SetActive(Value) then
    begin
        ActiveCircuit[ActiveActor].ActiveCktElement := TransformerClass[ActiveActor].ElementList.Active;
    end
    else
    begin
        DoSimpleMsg('Transformer "' + Value + '" Not Found in Active Circuit.', 5003);
    end;
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_NumTaps(Value: Integer); CDECL;
begin
    Set_Parameter('NumTaps', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_NumWindings(Value: Integer); CDECL;
var
    elem: TTransfObj;
begin
    elem := ActiveTransformer;
    if elem <> NIL then
        elem.SetNumWindings(Value);
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_R(Value: Double); CDECL;
begin
    Set_Parameter('%R', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Rneut(Value: Double); CDECL;
begin
    Set_Parameter('Rneut', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Tap(Value: Double); CDECL;
begin
    Set_Parameter('Tap', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Wdg(Value: Integer); CDECL;
var
    elem: TTransfObj;
begin
    elem := ActiveTransformer;
    if elem <> NIL then
        if (value > 0) and (value <= elem.NumberOfWindings) then
            elem.ActiveWinding := Value;
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_XfmrCode(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter('XfmrCode', Value);
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Xhl(Value: Double); CDECL;
begin
    Set_Parameter('Xhl', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Xht(Value: Double); CDECL;
begin
    Set_Parameter('Xht', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Xlt(Value: Double); CDECL;
begin
    Set_Parameter('Xlt', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_Xneut(Value: Double); CDECL;
begin
    Set_Parameter('Xneut', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
function Transformers_Get_Count(): Integer; CDECL;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ActiveCircuit[ActiveActor].Transformers.ListSize;
end;
//------------------------------------------------------------------------------
procedure Transformers_Get_WdgVoltages(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    elem: TTransfObj;
    TempVoltageBuffer: pComplexArray;
    i,
    iV: Integer;
begin
    elem := ActiveTransformer;
    if elem <> NIL then
    begin
        if (elem.ActiveWinding > 0) and (elem.ActiveWinding <= elem.NumberOfWindings) then
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2 * elem.nphases - 1) + 1);
            TempVoltageBuffer := AllocMem(Sizeof(Complex) * elem.nphases);
            elem.GetWindingVoltages(elem.ActiveWinding, TempVoltageBuffer, ActiveActor);
            iV := 0;
            for i := 1 to elem.Nphases do
            begin
                Result[iV] := TempVoltageBuffer^[i].re;
                Inc(iV);
                Result[iV] := TempVoltageBuffer^[i].im;
                Inc(iV);
            end;

            Reallocmem(TempVoltageBuffer, 0);
        end
        else
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
        ;

    end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

end;

procedure Transformers_Get_WdgVoltages_GR(); CDECL;
// Same as Transformers_Get_WdgVoltages but uses global result (GR) pointers
begin
    Transformers_Get_WdgVoltages(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Transformers_Get_WdgCurrents(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    elem: TTransfObj;
    TempCurrentBuffer: pComplexArray;
    NumCurrents,
    i,
    iV: Integer;
begin

    elem := ActiveTransformer;
    if elem <> NIL then
    begin
        NumCurrents := 2 * elem.NPhases * elem.NumberOfWindings; // 2 currents per winding
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (2 * NumCurrents - 1) + 1);
        TempCurrentBuffer := AllocMem(Sizeof(Complex) * NumCurrents);
        ;
        elem.GetAllWindingCurrents(TempCurrentBuffer, ActiveActor);
        iV := 0;
        for i := 1 to NumCurrents do
        begin
            Result[iV] := TempCurrentBuffer^[i].re;
            Inc(iV);
            Result[iV] := TempCurrentBuffer^[i].im;
            Inc(iV);
        end;

        Reallocmem(TempCurrentBuffer, 0);

    end
    else
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    ;
end;

procedure Transformers_Get_WdgCurrents_GR(); CDECL;
// Same as Transformers_Get_WdgCurrents but uses global result (GR) pointers
begin
    Transformers_Get_WdgCurrents(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Transformers_Get_strWdgCurrents_AnsiString(): Ansistring; CDECL;
var
    elem: TTransfObj;
begin
    elem := ActiveTransformer;
    if elem <> NIL then
    begin
        Result := elem.GetWindingCurrentsResult(ActiveActor);
    end;
end;

function Transformers_Get_strWdgCurrents(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Transformers_Get_strWdgCurrents_AnsiString());
end;
//------------------------------------------------------------------------------
function Transformers_Get_CoreType(): Integer; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0;  // default = shell
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.CoreType;

end;
//------------------------------------------------------------------------------
procedure Transformers_Set_CoreType(Value: Integer); CDECL;
var
    elem: TTransfObj;
begin
    elem := ActiveTransformer;
    if elem <> NIL then
    begin
        elem.CoreType := Value;
        case Value of
            1:
                elem.strCoreType := '1-phase';
            3:
                elem.strCoreType := '3-leg';
            5:
                elem.strCoreType := '5-leg';
        else
            elem.strCoreType := 'shell';
        end;
    end;

end;
//------------------------------------------------------------------------------
function Transformers_Get_RdcOhms(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.WdgRdc[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_RdcOhms(Value: Double); CDECL;
begin
    Set_Parameter('RdcOhms', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
function Transformers_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    Result := ActiveCircuit[ActiveActor].Transformers.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_idx(Value: Integer); CDECL;
var
    pTransformer: TTransfObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    pTransformer := ActiveCircuit[ActiveActor].Transformers.Get(Value);
    if pTransformer = NIL then
        Exit;
    ActiveCircuit[ActiveActor].ActiveCktElement := pTransformer;
end;
//------------------------------------------------------------------------------
end.
