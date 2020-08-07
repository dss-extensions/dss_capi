unit CAPI_Transformers;

interface

uses
    CAPI_Utils;

procedure Transformers_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
function Transformers_Get_First(): Integer; CDECL;
function Transformers_Get_IsDelta(): Boolean; CDECL;
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
procedure Transformers_Set_IsDelta(Value: Boolean); CDECL;
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
procedure Transformers_Get_WdgVoltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Transformers_Get_WdgVoltages_GR(); CDECL;
procedure Transformers_Get_WdgCurrents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Transformers_Get_WdgCurrents_GR(); CDECL;
function Transformers_Get_strWdgCurrents(): PAnsiChar; CDECL;
function Transformers_Get_CoreType(): Integer; CDECL;
procedure Transformers_Set_CoreType(Value: Integer); CDECL;
function Transformers_Get_RdcOhms(): Double; CDECL;
procedure Transformers_Set_RdcOhms(Value: Double); CDECL;

// API extensions
function Transformers_Get_idx(): Integer; CDECL;
procedure Transformers_Set_idx(Value: Integer); CDECL;
procedure Transformers_Get_LossesByType(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Transformers_Get_LossesByType_GR(); CDECL;
procedure Transformers_Get_AllLossesByType(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Transformers_Get_AllLossesByType_GR(); CDECL;


implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Executive,
    Transformer,
    SysUtils,
    PointerList,
    ucomplex;

//------------------------------------------------------------------------------
function _activeObj(out obj: TTransfObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
        
    obj := ActiveCircuit.Transformers.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active Transformer object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
        
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
    obj: TTransfObj;
begin
    if not _activeObj(obj) then
        Exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('transformer.%s.%s=%s', [obj.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure Transformers_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if InvalidCircuit then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.Transformers, False);
end;
//------------------------------------------------------------------------------
function Transformers_Get_First(): Integer; CDECL;
var
    elem: TTransfObj;
    lst: TPointerList;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
        
    lst := ActiveCircuit.Transformers;
    elem := lst.First;
    if elem = NIL then
        Exit;
        
    repeat
        if elem.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := elem;
            Result := 1;
        end
        else
            elem := lst.Next;
    until (Result = 1) or (elem = NIL);
end;
//------------------------------------------------------------------------------
function Transformers_Get_IsDelta(): Boolean; CDECL;
var
    elem: TTransfObj;
begin
    Result := FALSE;
    if not _activeObj(elem) then
        Exit;

    if (elem.ActiveWinding > 0) and 
       (elem.ActiveWinding <= elem.NumberOfWindings) and
       (elem.WdgConnection[elem.ActiveWinding] > 0) then
        Result := TRUE;
end;
//------------------------------------------------------------------------------
function Transformers_Get_kV(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;

    if (elem.ActiveWinding > 0) and 
       (elem.ActiveWinding <= elem.NumberOfWindings) then
        Result := elem.Winding^[elem.ActiveWinding].kvll;
end;
//------------------------------------------------------------------------------
function Transformers_Get_kVA(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;

    if (elem.ActiveWinding > 0) and 
       (elem.ActiveWinding <= elem.NumberOfWindings) then
        Result := elem.WdgKVA[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_MaxTap(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;

    if (elem.ActiveWinding > 0) and 
       (elem.ActiveWinding <= elem.NumberOfWindings) then
        Result := elem.Maxtap[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_MinTap(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;

    if (elem.ActiveWinding > 0) and 
       (elem.ActiveWinding <= elem.NumberOfWindings) then
        Result := elem.Mintap[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_Name(): PAnsiChar; CDECL;
var
    elem: TTransfObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;
    
    Result := DSS_GetAsPAnsiChar(elem.Name);
end;

//------------------------------------------------------------------------------
function Transformers_Get_Next(): Integer; CDECL;
var
    elem: TTransfObj;
    lst: TPointerList;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;

    lst := ActiveCircuit.Transformers;
    elem := lst.Next;
    if elem = NIL then
        Exit;

    repeat
        if elem.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := elem;
            Result := lst.ActiveIndex;
        end
        else
            elem := lst.Next;
    until (Result > 0) or (elem = NIL);
end;
//------------------------------------------------------------------------------
function Transformers_Get_NumTaps(): Integer; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;

    if (elem.ActiveWinding > 0) and 
       (elem.ActiveWinding <= elem.NumberOfWindings) then
        Result := elem.NumTaps[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_NumWindings(): Integer; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;

    Result := elem.NumberOfWindings;
end;
//------------------------------------------------------------------------------
function Transformers_Get_R(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;

    if (elem.ActiveWinding > 0) and 
       (elem.ActiveWinding <= elem.NumberOfWindings) then
        Result := elem.WdgResistance[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_Rneut(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;

    if (elem.ActiveWinding > 0) and 
       (elem.ActiveWinding <= elem.NumberOfWindings) then
        Result := elem.WdgRneutral[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_Tap(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;

    if (elem.ActiveWinding > 0) and 
       (elem.ActiveWinding <= elem.NumberOfWindings) then
        Result := elem.PresentTap[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
function Transformers_Get_Wdg(): Integer; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;

    Result := elem.ActiveWinding;
end;
//------------------------------------------------------------------------------
function Transformers_Get_XfmrCode(): PAnsiChar; CDECL;
var
    elem: TTransfObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(elem.XfmrCode);
end;
//------------------------------------------------------------------------------
function Transformers_Get_Xhl(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;

    Result := elem.XhlVal;
end;
//------------------------------------------------------------------------------
function Transformers_Get_Xht(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;

    Result := elem.XhtVal;
end;
//------------------------------------------------------------------------------
function Transformers_Get_Xlt(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;

    Result := elem.XltVal;
end;
//------------------------------------------------------------------------------
function Transformers_Get_Xneut(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;

    if (elem.ActiveWinding > 0) and 
       (elem.ActiveWinding <= elem.NumberOfWindings) then
        Result := elem.WdgXneutral[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_IsDelta(Value: Boolean); CDECL;
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
    if InvalidCircuit then
        Exit;

    if TransformerClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := TransformerClass.ElementList.Active;
        ActiveCircuit.Transformers.Get(TransformerClass.Active);
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
    if not _activeObj(elem) then
        Exit;

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
    if not _activeObj(elem) then
        Exit;
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
    Result := 0;
    if Assigned(ActiveCircuit) then
        Result := ActiveCircuit.Transformers.ListSize;
end;
//------------------------------------------------------------------------------
procedure Transformers_Get_WdgVoltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TTransfObj;
begin
    if not _activeObj(elem) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    
    if (elem.ActiveWinding > 0) and (elem.ActiveWinding <= elem.NumberOfWindings) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * elem.nphases);
        elem.GetWindingVoltages(elem.ActiveWinding, pComplexArray(ResultPtr));
        Exit;
    end;
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
end;

procedure Transformers_Get_WdgVoltages_GR(); CDECL;
// Same as Transformers_Get_WdgVoltages but uses global result (GR) pointers
begin
    Transformers_Get_WdgVoltages(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Transformers_Get_WdgCurrents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TTransfObj;
    NumCurrents: Integer;
begin
    if not _activeObj(elem) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;

    NumCurrents := 2 * elem.NPhases * elem.NumberOfWindings; // 2 currents per winding
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NumCurrents);
    elem.GetAllWindingCurrents(pComplexArray(ResultPtr));
end;

procedure Transformers_Get_WdgCurrents_GR(); CDECL;
// Same as Transformers_Get_WdgCurrents but uses global result (GR) pointers
begin
    Transformers_Get_WdgCurrents(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Transformers_Get_strWdgCurrents(): PAnsiChar; CDECL;
var
    elem: TTransfObj;
begin
    if not _activeObj(elem) then
    begin
        Result := NIL;
        Exit;
    end;
    
    Result := DSS_GetAsPAnsiChar(elem.GetWindingCurrentsResult);
end;
//------------------------------------------------------------------------------
function Transformers_Get_CoreType(): Integer; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0;  // default = shell
    if _activeObj(elem) then
        Result := elem.CoreType;
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_CoreType(Value: Integer); CDECL;
var
    elem: TTransfObj;
begin
    if not _activeObj(elem) then
        Exit;
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
//------------------------------------------------------------------------------
function Transformers_Get_RdcOhms(): Double; CDECL;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then    
        Exit;
    
    if (elem.ActiveWinding > 0) and 
       (elem.ActiveWinding <= elem.NumberOfWindings) then
        Result := elem.WdgRdc[elem.ActiveWinding];
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_RdcOhms(Value: Double); CDECL;
begin
    Set_Parameter('RdcOhms', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Transformers_Get_LossesByType(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Returns an array with (TotalLosses, LoadLosses, NoLoadLosses) for the current active transformer, in VA
var 
    CResult: PComplexArray; // this array is one-based, see Ucomplex
    elem: TTransfObj;
begin
    if not _activeObj(elem) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * 3);
    CResult := PComplexArray(ResultPtr);
    elem.GetLosses(CResult[1], CResult[2], CResult[3]);
    // Keep the results in VA (NOT kVA) for consistency with CktElement_Get_Losses
end;

procedure Transformers_Get_LossesByType_GR(); CDECL;
begin
    Transformers_Get_LossesByType(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Transformers_Get_AllLossesByType(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Returns an array with (TotalLosses, LoadLosses, NoLoadLosses) for all transformers, in VA
var
    Result: PDoubleArray;
    CResult: PComplexArray; // this array is one-based, see Ucomplex
    elem: TTransfObj;
    lst: TPointerList;
    k: Integer;
begin
    if (InvalidCircuit) or (ActiveCircuit.Transformers.ListSize <= 0) then
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Result[0] := 0;
        Exit;
    end;
    
    lst := ActiveCircuit.Transformers;
    DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, lst.ListSize * 2 * 3);
    CResult := PComplexArray(ResultPtr);
    k := 1;
    elem := lst.First;
    while elem <> NIL do
    begin
        elem.GetLosses(CResult[k], CResult[k + 1], CResult[k + 2]);
        elem := lst.Next;
        Inc(k, 3);
    end;
    
    // Keep the results in VA (NOT kVA) for consistency with CktElement_Get_Losses
end;

procedure Transformers_Get_AllLossesByType_GR(); CDECL;
begin
    Transformers_Get_AllLossesByType(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;
//------------------------------------------------------------------------------
function Transformers_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Transformers.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Transformers_Set_idx(Value: Integer); CDECL;
var
    pTransformer: TTransfObj;
begin
    if InvalidCircuit then
        Exit;
    pTransformer := ActiveCircuit.Transformers.Get(Value);
    if pTransformer = NIL then
    begin
        DoSimpleMsg('Invalid Transformer index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := pTransformer;
end;
//------------------------------------------------------------------------------
end.
