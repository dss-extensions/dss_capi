unit CAPI_Generators;

{$inline on}

interface

uses
    CAPI_Utils;

procedure Generators_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Generators_Get_AllNames_GR(); CDECL;
function Generators_Get_First(): Integer; CDECL;
function Generators_Get_Name(): PAnsiChar; CDECL;
function Generators_Get_Next(): Integer; CDECL;
procedure Generators_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Generators_Get_RegisterNames_GR(); CDECL;
procedure Generators_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Generators_Get_RegisterValues_GR(); CDECL;
function Generators_Get_ForcedON(): Wordbool; CDECL;
procedure Generators_Set_ForcedON(Value: Wordbool); CDECL;
procedure Generators_Set_Name(const Value: PAnsiChar); CDECL;
function Generators_Get_kV(): Double; CDECL;
function Generators_Get_kvar(): Double; CDECL;
function Generators_Get_kW(): Double; CDECL;
function Generators_Get_PF(): Double; CDECL;
function Generators_Get_Phases(): Integer; CDECL;
procedure Generators_Set_kV(Value: Double); CDECL;
procedure Generators_Set_kvar(Value: Double); CDECL;
procedure Generators_Set_kW(Value: Double); CDECL;
procedure Generators_Set_PF(Value: Double); CDECL;
procedure Generators_Set_Phases(Value: Integer); CDECL;
function Generators_Get_Count(): Integer; CDECL;
function Generators_Get_idx(): Integer; CDECL;
procedure Generators_Set_idx(Value: Integer); CDECL;
function Generators_Get_Model(): Integer; CDECL;
procedure Generators_Set_Model(Value: Integer); CDECL;
function Generators_Get_kVArated(): Double; CDECL;
procedure Generators_Set_kVArated(Value: Double); CDECL;
function Generators_Get_Vmaxpu(): Double; CDECL;
function Generators_Get_Vminpu(): Double; CDECL;
procedure Generators_Set_Vmaxpu(Value: Double); CDECL;
procedure Generators_Set_Vminpu(Value: Double); CDECL;

implementation

uses
    CAPI_Constants,
    DSSCLassDefs,
    DSSGlobals,
    Generator,
    CktElement,
    SysUtils;

//------------------------------------------------------------------------------
function _activeObj(out obj: TGeneratorObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
    
    obj := ActiveCircuit.Generators.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active Generator object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Generators_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if InvalidCircuit then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.Generators, False);
end;

procedure Generators_Get_AllNames_GR(); CDECL;
// Same as Generators_Get_AllNames but uses global result (GR) pointers
begin
    Generators_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Generators_Get_First(): Integer; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
        
    pGen := ActiveCircuit.Generators.First;
    if pGen = NIL then
        Exit;
        
    repeat
        if pGen.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := pGen;
            Result := 1;
        end
        else
            pGen := ActiveCircuit.Generators.Next;
    until (Result = 1) or (pGen = NIL);
end;
//------------------------------------------------------------------------------
function Generators_Get_Name(): PAnsiChar; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := NIL;
    if not _activeObj(pGen) then
        Exit;

    Result := DSS_GetAsPAnsiChar(pGen.Name);
end;
//------------------------------------------------------------------------------
function Generators_Get_Next(): Integer; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := 0;

    if InvalidCircuit then
        Exit;
    
    pGen := ActiveCircuit.Generators.Next;
    if pGen = NIL then
        Exit;

    repeat
        if pGen.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := pGen;
            Result := ActiveCircuit.Generators.ActiveIndex;
        end
        else
            pGen := ActiveCircuit.Generators.Next;
    until (Result > 0) or (pGen = NIL);
end;
//------------------------------------------------------------------------------
procedure Generators_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    GeneratorCls: TGenerator;
    k: Integer;
begin
    GeneratorCls := GeneratorClass;
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumGenRegisters - 1) + 1);
    for k := 0 to NumGenRegisters - 1 do
    begin
        Result[k] := DSS_CopyStringAsPChar(GeneratorCls.RegisterNames[k + 1]);
    end;
end;

procedure Generators_Get_RegisterNames_GR(); CDECL;
// Same as Generators_Get_RegisterNames but uses global result (GR) pointers
begin
    Generators_Get_RegisterNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
procedure Generators_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    Gen: TGeneratorObj;
    k: Integer;
begin
    if not _activeObj(Gen) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, numGenRegisters);
    for k := 0 to numGenRegisters - 1 do
    begin
        Result[k] := Gen.Registers[k + 1];
    end;
end;

procedure Generators_Get_RegisterValues_GR(); CDECL;
// Same as Generators_Get_RegisterValues but uses global result (GR) pointers
begin
    Generators_Get_RegisterValues(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Generators_Get_ForcedON(): Wordbool; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := FALSE;
    if not _activeObj(pGen) then
        Exit;

    Result := pGen.ForcedON;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_ForcedON(Value: Wordbool); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(pGen) then
        Exit;

    pGen.ForcedON := Value;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit then
        Exit;
    if GeneratorClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := GeneratorClass.ElementList.Active;
        ActiveCircuit.Generators.Get(GeneratorClass.Active);
    end
    else
    begin
        DoSimpleMsg('Generator "' + Value + '" Not Found in Active Circuit.', 5003);
    end;
end;
//------------------------------------------------------------------------------
function Generators_Get_kV(): Double; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := -1.0;  // not set
    if not _activeObj(pGen) then
        Exit;

    Result := pGen.GenVars.kVGeneratorBase;
end;
//------------------------------------------------------------------------------
function Generators_Get_kvar(): Double; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := 0.0;
    if not _activeObj(pGen) then
        Exit;

    Result := pGen.Presentkvar;
end;
//------------------------------------------------------------------------------
function Generators_Get_kW(): Double; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := 0.0;
    if not _activeObj(pGen) then
        Exit;

    Result := pGen.PresentkW;
end;
//------------------------------------------------------------------------------
function Generators_Get_PF(): Double; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := 0.0;
    if not _activeObj(pGen) then
        Exit;

    Result := pGen.PowerFactor;
end;
//------------------------------------------------------------------------------
function Generators_Get_Phases(): Integer; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := 0;  // not set
    if not _activeObj(pGen) then
        Exit;

    Result := pGen.nphases;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_kV(Value: Double); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(pGen) then
        Exit;

    pGen.PresentkV := Value;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_kvar(Value: Double); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(pGen) then
        Exit;

    pGen.Presentkvar := Value;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_kW(Value: Double); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(pGen) then
        Exit;

    pGen.PresentkW := Value;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_PF(Value: Double); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(pGen) then
        Exit;

    pGen.PowerFactor := Value;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Phases(Value: Integer); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(pGen) then
        Exit;

    pGen.Nphases := Value;
end;
//------------------------------------------------------------------------------
function Generators_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Generators.ListSize;
end;
//------------------------------------------------------------------------------
function Generators_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Generators.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Generators_Set_idx(Value: Integer); CDECL;
var
    pGen: TGeneratorObj;
begin
    if InvalidCircuit then
        Exit;
    pGen := ActiveCircuit.Generators.Get(Value);
    if pGen = NIL then
    begin
        DoSimpleMsg('Invalid Generator index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := pGen;
end;
//------------------------------------------------------------------------------
function Generators_Get_Model(): Integer; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := -1;
    if not _activeObj(pGen) then
        Exit;
    
    Result := pGen.GenModel;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Model(Value: Integer); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(pGen) then
        Exit;

    with pGen do
    begin
        GenModel := Value;
         // Handle side effect
        if GenModel = 3 then
            ActiveCircuit.Solution.SolutionInitialized := FALSE;
    end;
end;
//------------------------------------------------------------------------------
function Generators_Get_kVArated(): Double; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := -1.0;
    if not _activeObj(pGen) then
        Exit;

    Result := pGen.Genvars.kVArating;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_kVArated(Value: Double); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(pGen) then
        Exit;

    pGen.Genvars.kVArating := Value;
end;
//------------------------------------------------------------------------------
function Generators_Get_Vmaxpu(): Double; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := -1.0;
    if not _activeObj(pGen) then
        Exit;
    
    Result := pGen.Vmaxpu;
end;
//------------------------------------------------------------------------------
function Generators_Get_Vminpu(): Double; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := -1.0;
    if not _activeObj(pGen) then
        Exit;

    Result := pGen.Vminpu;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Vmaxpu(Value: Double); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(pGen) then
        Exit;

    pGen.VMaxPu := Value;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Vminpu(Value: Double); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(pGen) then
        Exit;

    pGen.VMinPu := Value;
end;
//------------------------------------------------------------------------------
end.
