unit CAPI_Generators;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure Generators_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Generators_Get_AllNames_GR(); CDECL;
function Generators_Get_First(): Integer; CDECL;
function Generators_Get_Name(): PAnsiChar; CDECL;
function Generators_Get_Next(): Integer; CDECL;
procedure Generators_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Generators_Get_RegisterNames_GR(); CDECL;
procedure Generators_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Generators_Get_RegisterValues_GR(); CDECL;
function Generators_Get_ForcedON(): TAPIBoolean; CDECL;
procedure Generators_Set_ForcedON(Value: TAPIBoolean); CDECL;
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
    SysUtils,
    DSSClass,
    DSSHelper;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TGeneratorObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.ActiveCircuit.Generators.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['Generator'], 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Generators_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.Generators, False);
end;

procedure Generators_Get_AllNames_GR(); CDECL;
// Same as Generators_Get_AllNames but uses global result (GR) pointers
begin
    Generators_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Generators_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.Generators);
end;
//------------------------------------------------------------------------------
function Generators_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.Generators);
end;
//------------------------------------------------------------------------------
function Generators_Get_Name(): PAnsiChar; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, pGen.Name);
end;
//------------------------------------------------------------------------------
procedure Generators_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    GeneratorCls: TGenerator;
    k: Integer;
begin
    GeneratorCls := DSSPrime.GeneratorClass;
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumGenRegisters);
    for k := 0 to NumGenRegisters - 1 do
    begin
        Result[k] := DSS_CopyStringAsPChar(GeneratorCls.RegisterNames[k + 1]);
    end;
end;

procedure Generators_Get_RegisterNames_GR(); CDECL;
// Same as Generators_Get_RegisterNames but uses global result (GR) pointers
begin
    Generators_Get_RegisterNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
procedure Generators_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    Gen: TGeneratorObj;
    k: Integer;
begin
    if not _activeObj(DSSPrime, Gen) then
    begin
        DefaultResult(ResultPtr, ResultCount);
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
    Generators_Get_RegisterValues(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function Generators_Get_ForcedON(): TAPIBoolean; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    Result := pGen.ForcedON;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_ForcedON(Value: TAPIBoolean); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    pGen.ForcedON := Value;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.GeneratorClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.GeneratorClass.ElementList.Active;
        DSSPrime.ActiveCircuit.Generators.Get(DSSPrime.GeneratorClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Generator "%s" not found in Active Circuit.', [Value], 5003);
    end;
end;
//------------------------------------------------------------------------------
function Generators_Get_kV(): Double; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := -1.0;  // not set
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    Result := pGen.GenVars.kVGeneratorBase;
end;
//------------------------------------------------------------------------------
function Generators_Get_kvar(): Double; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    Result := pGen.Presentkvar;
end;
//------------------------------------------------------------------------------
function Generators_Get_kW(): Double; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    Result := pGen.PresentkW;
end;
//------------------------------------------------------------------------------
function Generators_Get_PF(): Double; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    Result := pGen.PowerFactor;
end;
//------------------------------------------------------------------------------
function Generators_Get_Phases(): Integer; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := 0;  // not set
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    Result := pGen.nphases;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_kV(Value: Double); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    pGen.PresentkV := Value;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_kvar(Value: Double); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    pGen.kvarBase := Value;
    pGen.PropertySideEffects(ord(TGeneratorProp.kvar))
end;
//------------------------------------------------------------------------------
procedure Generators_Set_kW(Value: Double); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    pGen.PresentkW := Value;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_PF(Value: Double); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    pGen.PowerFactor := Value;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Phases(Value: Integer); CDECL;
var
    elem: TGeneratorObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if Value < 1 then
    begin
        DoSimpleMsg(DSSPrime, '%s: Number of phases must be a positive integer!', [elem.FullName], 6568);
        Exit;
    end;
    elem.FNphases := Value;
    //TODO: missing side-effects?
end;
//------------------------------------------------------------------------------
function Generators_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Generators.Count;
end;
//------------------------------------------------------------------------------
function Generators_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Generators.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Generators_Set_idx(Value: Integer); CDECL;
var
    pGen: TGeneratorObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    pGen := DSSPrime.ActiveCircuit.Generators.Get(Value);
    if pGen = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['Generator', Value], 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pGen;
end;
//------------------------------------------------------------------------------
function Generators_Get_Model(): Integer; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := -1;
    if not _activeObj(DSSPrime, pGen) then
        Exit;
    
    Result := pGen.GenModel;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Model(Value: Integer); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    with pGen do
    begin
        GenModel := Value;
         // Handle side effect
        if GenModel = 3 then
            DSSPrime.ActiveCircuit.Solution.SolutionInitialized := FALSE;
    end;
end;
//------------------------------------------------------------------------------
function Generators_Get_kVArated(): Double; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := -1.0;
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    Result := pGen.Genvars.kVArating;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_kVArated(Value: Double); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    pGen.Genvars.kVArating := Value;
end;
//------------------------------------------------------------------------------
function Generators_Get_Vmaxpu(): Double; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := -1.0;
    if not _activeObj(DSSPrime, pGen) then
        Exit;
    
    Result := pGen.Vmaxpu;
end;
//------------------------------------------------------------------------------
function Generators_Get_Vminpu(): Double; CDECL;
var
    pGen: TGeneratorObj;
begin
    Result := -1.0;
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    Result := pGen.Vminpu;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Vmaxpu(Value: Double); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    pGen.VMaxPu := Value;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Vminpu(Value: Double); CDECL;
var
    pGen: TGeneratorObj;
begin
    if not _activeObj(DSSPrime, pGen) then
        Exit;

    pGen.VMinPu := Value;
end;
//------------------------------------------------------------------------------
end.
