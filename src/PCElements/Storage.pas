unit Storage;

{
  ----------------------------------------------------------
  Copyright (c) 2009-2016, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
//  To Do:
//    Make connection to User model
//    Yprim for various modes
//    Define state vars and dynamics mode behavior
//    Complete Harmonics mode algorithm (generator mode is implemented)

//  The storage element is essentially a generator that can be dispatched
//  to either produce power or consume power commensurate with rating and
//  amount of stored energy.
//
//  The storage element can also produce or absorb vars within the kVA rating of the inverter.
//  That is, a StorageController object requests kvar and the storage element provides them if
//  it has any capacity left. The storage element can produce/absorb kvar while idling.

//  The Storage element is assumed balanced over the no. of phases defined

interface

uses
    Classes,
    StorageVars,
    StoreUserModel,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    UComplex, DSSUcomplex,
    LoadShape,
    Spectrum,
    ArrayDef,
    Dynamics;

type
{$SCOPEDENUMS ON}
    TStorageProp = (
        INVALID = 0,
        phases = 1,
        bus1 = 2,
        kv = 3, // propKV
        kW = 4, // propKW
        pf = 5, // propPF
        conn = 6, // propCONNECTION
        kvar = 7, // propKVAR
        kVA = 8, // propKVA
        kWrated = 9, // propKWRATED
        kWhrated = 10, // propKWHRATED
        kWhstored = 11, // propKWHSTORED
        pctstored = 12, // propPCTSTORED
        pctreserve = 13, // propPCTRESERVE
        State = 14, // propSTATE
        pctDischarge = 15, // propPCTKWOUT
        pctCharge = 16, // propPCTKWIN
        pctEffCharge = 17, // propCHARGEEFF
        pctEffDischarge = 18, // propDISCHARGEEFF
        pctIdlingkW = 19, // propIDLEKW
        pctIdlingkvar = 20, // propIDLEKVAR
        pctR = 21, // propPCTR
        pctX = 22, // propPCTX
        model = 23, // propMODEL
        Vminpu = 24, // propVMINPU
        Vmaxpu = 25, // propVMAXPU
        Balanced = 26, // propBalanced
        LimitCurrent = 27, // propLimited
        yearly = 28, // propYEARLY
        daily = 29, // propDAILY
        duty = 30, // propDUTY
        DispMode = 31, // propDISPMODE
        DischargeTrigger = 32, // propDISPOUTTRIG
        ChargeTrigger = 33, // propDISPINTRIG
        TimeChargeTrig = 34, // propCHARGETIME
        cls = 35, // propCLASS
        DynaDLL = 36, // propDynaDLL
        DynaData = 37, // propDynaData
        UserModel = 38, // propUSERMODEL
        UserData = 39, // propUSERDATA
        debugtrace = 40 // propDEBUGTRACE
    );
{$SCOPEDENUMS OFF}

const
    NumStorageRegisters = 6;    // Number of energy meter registers
    NumStorageVariables = 7;    // No state variables

//= = = = = = = = = = = = = = DEFINE STATES = = = = = = = = = = = = = = = = = = = = = = = = =

    STORE_CHARGING = -1;
    STORE_IDLING = 0;
    STORE_DISCHARGING = 1;
//= = = = = = = = = = = = = = DEFINE DISPATCH MODES = = = = = = = = = = = = = = = = = = = = = = = = =

    STORE_DEFAULT = 0;
    STORE_LOADMODE = 1;
    STORE_PRICEMODE = 2;
    STORE_EXTERNALMODE = 3;
    STORE_FOLLOW = 4;

type

    TStorage = class(TPCClass)
    PROTECTED
        cBuffer: TCBuffer24;  // Temp buffer for calcs  24-phase Storage element?

        procedure DefineProperties; override;
    PUBLIC
        RegisterNames: array[1..NumStorageRegisters] of String;

        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;

        procedure ResetRegistersAll;
        procedure SampleAll;
        procedure UpdateAll;
    end;

    TStorageObj = class(TPCElement)
    PRIVATE
        Yeq: Complex;   // at nominal
        Yeq95: Complex;   // at 95%
        Yeq105: Complex;   // at 105%
        YeqIdling: Complex;   // in shunt representing idle impedance
        YeqDischarge: Complex;   // equiv at rated power of storage element only
        PhaseCurrentLimit: Complex;
        MaxDynPhaseCurrent: Double;

        DebugTrace: Boolean;
        FState: Integer;
        FStateChanged: Boolean;
        FirstSampleAfterReset: Boolean;
        StorageSolutionCount: Integer;
        StorageFundamental: Double;  // Thevinen equivalent voltage mag and angle reference for Harmonic model
        StorageObjSwitchOpen: Boolean;

        ForceBalanced: Boolean;
        CurrentLimited: Boolean;

        kVANotSet: Boolean;
        kvar_out: Double;
        kW_out: Double;
        pctIdlekW: Double;
        pctIdlekvar: Double;
        pctChargeEff: Double;
        pctDischargeEff: Double;
        DischargeTrigger: Double;
        ChargeTrigger: Double;
        ChargeTime: Double;
        kWhBeforeUpdate: Double;

        pctR: Double;
        pctX: Double;

        OpenStorageSolutionCount: Integer;
        Pnominalperphase: Double;
        Qnominalperphase: Double;
        RandomMult: Double;

        Reg_Hours: Integer;
        Reg_kvarh: Integer;
        Reg_kWh: Integer;
        Reg_MaxkVA: Integer;
        Reg_MaxkW: Integer;
        Reg_Price: Integer;
        ShapeFactor: Complex;
        TraceFile: TFileStream;
        IsUserModel: Boolean;
        UserModel: TStoreUserModel;   // User-Written Models
        DynaModel: TStoreDynaModel;

        UserModelNameStr, UserModelEditStr: String;
        DynaModelNameStr, DynaModelEditStr: String;

        kvarBase: Double;  // Base vars per phase
        VBase: Double;  // Base volts suitable for computing currents
        VBase105: Double;
        VBase95: Double;
        Vmaxpu: Double;
        Vminpu: Double;
        YPrimOpenCond: TCmatrix;


        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);
        procedure CalcStorageModelContribution;
        procedure CalcInjCurrentArray;
        procedure CalcVTerminalPhase;
        procedure CalcYearlyMult(Hr: Double);
        procedure CalcYPrimMatrix(Ymatrix: TcMatrix);

        procedure DoConstantPQStorageObj;
        procedure DoConstantZStorageObj;
        procedure DoDynamicMode;
        procedure DoHarmonicMode;
        procedure DoUserModel;
        procedure DoDynaModel;

        procedure Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
        procedure SetDragHandRegister(Reg: Integer; const Value: Double);
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);

        procedure WriteTraceRecord(const s: String);

        procedure SyncUpPowerQuantities;
        procedure SetKWandKvarOut;
        procedure CheckStateTriggerLevel(Level: Double);
        procedure UpdateStorage;    // Update Storage elements based on present kW and IntervalHrs variable
        function NormalizeToTOD(h: Integer; sec: Double): Double;
        function Get_PresentkW: Double;
        function Get_Presentkvar: Double;
        function Get_PresentkV: Double;
        procedure Set_PresentkV(const Value: Double); //TODO: remove?
        procedure Set_Presentkvar(const Value: Double);
        procedure Set_PresentkW(const Value: Double);
        procedure Set_PowerFactor(const Value: Double);
        procedure Set_StorageState(const Value: Integer);
        procedure Set_pctkvarOut(const Value: Double);
        procedure Set_pctkWOut(const Value: Double);
        function Get_kWTotalLosses: Double;
        function Get_kWIdlingLosses: Double;

    PROTECTED
        procedure GetTerminalCurrents(Curr: pComplexArray); OVERRIDE;

    PUBLIC
        Connection: Integer;  // 0 = line-neutral; 1=Delta
        DailyShapeObj: TLoadShapeObj;  // Daily (24 HR) Storage element shape
        DutyShapeObj: TLoadShapeObj;  // Duty cycle load shape for changes typically less than one hour
        YearlyShapeObj: TLoadShapeObj;  // Shape for this Storage element
        StorageClass: Integer;
        VoltageModel: Integer;   // Variation with voltage
        PFNominal: Double;

        StorageVars: TStorageVars;

        FpctkWout: Double;   // percent of kW rated output currently dispatched
        Fpctkvarout: Double;
        pctkWin: Double;
        pctReserve: Double;
        DispatchMode: Integer;

        Registers, Derivatives: array[1..NumStorageRegisters] of Double;

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure Set_ConductorClosed(Index: Integer; Value: Boolean); OVERRIDE;
        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        function InjCurrents: Integer; OVERRIDE;
        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        procedure SetNominalStorageOuput;
        procedure Randomize(Opt: Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform

        procedure ResetRegisters;
        procedure TakeSample;

        // Support for Dynamics Mode
        procedure InitStateVars; OVERRIDE;
        procedure IntegrateStates; OVERRIDE;

        // Support for Harmonics Mode
        procedure InitHarmonics; OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        property PresentkW: Double READ Get_PresentkW WRITE Set_PresentkW;
        property Presentkvar: Double READ Get_Presentkvar WRITE Set_Presentkvar;
        property PresentkV: Double READ Get_PresentkV WRITE Set_PresentkV;
        property PowerFactor: Double READ PFNominal WRITE Set_PowerFactor;

        property StorageState: Integer READ FState WRITE Set_StorageState;
        property PctkWOut: Double READ FpctkWOut WRITE Set_pctkWOut;
        property PctkVarOut: Double READ FpctkvarOut WRITE Set_pctkvarOut;

        property kWTotalLosses: Double READ Get_kWTotalLosses;
        property kWIdlingLosses: Double READ Get_kWIdlingLosses;

        property MinModelVoltagePU: Double READ VminPu;
    end;

implementation

uses
    BufStream,
    Circuit,
    Sysutils,
    Command,
    Math,
    MathUtil,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TStorageObj;
    TProp = TStorageProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    
    DispatchModeEnum, StateEnum: TDSSEnum;

constructor TStorage.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        StateEnum := TDSSEnum.Create('LegacyStorage: State', True, 1, 1, 
            ['Charging', 'Idling', 'Discharging'], 
            [-1, 0, 1]);
        StateEnum.DefaultValue := 0;
        DispatchModeEnum := TDSSEnum.Create('LegacyStorage: Dispatch Mode', True, 1, 1, 
            ['Default', 'Loadshape', 'Price', 'External', 'Follow'], 
            [0, 1, 2, 3, 4]);
        DispatchModeEnum.DefaultValue := 0;
    end;

    inherited Create(dssContext, STORAGE_ELEMENT, 'Storage');

     // Set Register names
    RegisterNames[1] := 'kWh';
    RegisterNames[2] := 'kvarh';
    RegisterNames[3] := 'Max kW';
    RegisterNames[4] := 'Max kVA';
    RegisterNames[5] := 'Hours';
    RegisterNames[6] := 'Price($)';
end;

destructor TStorage.Destroy;
begin
    inherited Destroy;
end;

procedure Setkvar(obj: TObj; Value: Double);
begin
    obj.Presentkvar := Value;
end;

function Getkvar(obj: TObj): Double;
begin
    Result := obj.kvar_out;
end;

procedure SetPctStored(obj: TObj; Value: Double);
begin
    obj.StorageVars.kWhStored := Value * 0.01 * obj.StorageVars.kWhRating;
end;

function GetPctStored(obj: TObj): Double;
begin
    Result := obj.StorageVars.kWhStored / obj.StorageVars.kWhRating * 100.0;
end;

procedure TStorage.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // enum properties
    PropertyType[ord(TProp.conn)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.conn)] := ptruint(@obj.Connection);
    PropertyOffset2[ord(TProp.conn)] := PtrInt(DSS.ConnectionEnum);

    PropertyType[ord(TProp.State)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.State)] := ptruint(@obj.FState);
    PropertyOffset2[ord(TProp.State)] := PtrInt(StateEnum);

    PropertyType[ord(TProp.DispMode)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.DispMode)] := ptruint(@obj.DispatchMode);
    PropertyOffset2[ord(TProp.DispMode)] := PtrInt(DispatchModeEnum);

    // strings
    PropertyType[ord(TProp.UserModel)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.UserModel)] := ptruint(@obj.UserModelNameStr);
    PropertyType[ord(TProp.UserData)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.UserData)] := ptruint(@obj.UserModelEditStr);

    PropertyType[ord(TProp.DynaDLL)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.DynaDLL)] := ptruint(@obj.DynaModelNameStr);
    PropertyType[ord(TProp.DynaData)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.DynaData)] := ptruint(@obj.DynaModelEditStr);

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;

    // boolean properties
    PropertyType[ord(TProp.Balanced)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.LimitCurrent)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.debugtrace)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.LimitCurrent)] := ptruint(@obj.CurrentLimited);
    PropertyOffset[ord(TProp.Balanced)] := ptruint(@obj.ForceBalanced);
    PropertyOffset[ord(TProp.debugtrace)] := ptruint(@obj.DebugTrace);

    // integer properties                
    PropertyType[ord(TProp.cls)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.model)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.cls)] := ptruint(@obj.StorageClass);
    PropertyOffset[ord(TProp.model)] := ptruint(@obj.VoltageModel);
    
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    // object properties
    PropertyType[ord(TProp.yearly)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.daily)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.duty)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.yearly)] := ptruint(@obj.YearlyShapeObj);
    PropertyOffset[ord(TProp.daily)] := ptruint(@obj.DailyShapeObj);
    PropertyOffset[ord(TProp.duty)] := ptruint(@obj.DutyShapeObj);
    PropertyOffset2[ord(TProp.yearly)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.daily)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.duty)] := ptruint(DSS.LoadShapeClass);

    // doubles
    PropertyOffset[ord(TProp.pf)] := ptruint(@obj.PFNominal);
    PropertyOffset[ord(TProp.pctIdlingkvar)] := ptruint(@obj.pctIdlekvar);
    PropertyOffset[ord(TProp.pctR)] := ptruint(@obj.pctR);
    PropertyOffset[ord(TProp.pctX)] := ptruint(@obj.pctX);
    PropertyOffset[ord(TProp.pctIdlingkW)] := ptruint(@obj.pctIdlekW);
    PropertyOffset[ord(TProp.DischargeTrigger)] := ptruint(@obj.DischargeTrigger);
    PropertyOffset[ord(TProp.ChargeTrigger)] := ptruint(@obj.ChargeTrigger);
    PropertyOffset[ord(TProp.pctEffCharge)] := ptruint(@obj.pctChargeEff);
    PropertyOffset[ord(TProp.pctEffDischarge)] := ptruint(@obj.pctDischargeEff);
    PropertyOffset[ord(TProp.pctDischarge)] := ptruint(@obj.pctkWout);
    PropertyOffset[ord(TProp.Vminpu)] := ptruint(@obj.VMinPu);
    PropertyOffset[ord(TProp.Vmaxpu)] := ptruint(@obj.VMaxPu);
    PropertyOffset[ord(TProp.kVA)] := ptruint(@obj.StorageVars.kVArating);
    PropertyOffset[ord(TProp.kWrated)] := ptruint(@obj.StorageVars.kWrating);
    PropertyOffset[ord(TProp.kWhrated)] := ptruint(@obj.StorageVars.kWhrating);
    PropertyOffset[ord(TProp.kWhstored)] := ptruint(@obj.StorageVars.kWhstored);
    PropertyOffset[ord(TProp.pctreserve)] := ptruint(@obj.pctReserve);
    PropertyOffset[ord(TProp.pctCharge)] := ptruint(@obj.pctkWIn);
    PropertyOffset[ord(TProp.TimeChargeTrig)] := ptruint(@obj.ChargeTime);
    PropertyOffset[ord(TProp.kv)] := ptruint(@obj.StorageVars.kVStorageBase);
    PropertyOffset[ord(TProp.kW)] := ptruint(@obj.kW_Out);

    PropertyType[ord(TProp.kvar)] := TPropertyType.DoubleProperty;
    PropertyOffset[ord(TProp.kvar)] := 1;
    PropertyWriteFunction[ord(TProp.kvar)] := @Setkvar;
    PropertyReadFunction[ord(TProp.kvar)] := @Getkvar;
    PropertyFlags[ord(TProp.kvar)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction];

    PropertyType[ord(TProp.pctstored)] := TPropertyType.DoubleProperty;
    PropertyOffset[ord(TProp.pctstored)] := 1;
    PropertyWriteFunction[ord(TProp.pctstored)] := @SetPctStored;
    PropertyReadFunction[ord(TProp.pctstored)] := @GetPctStored;
    PropertyFlags[ord(TProp.pctstored)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction];


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TStorage.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure SetNcondsForConnection(Obj: TObj);
begin
    with Obj do
        case Connection of
            0:
                NConds := Fnphases + 1;
            1:
                case Fnphases of
                    1, 2:
                        NConds := Fnphases + 1; // L-L and Open-delta
                else
                    NConds := Fnphases;
                end;
        end;
end;

procedure TStorage.UpdateAll;
var
    i: Integer;
begin
    for i := 1 to ElementList.Count do
        with TStorageObj(ElementList.Get(i)) do
            if Enabled then
                UpdateStorage;
end;

procedure TStorageObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    i: Integer;
begin
    case Idx of
        ord(TProp.phases):
            SetNCondsForConnection(self);  // Force Reallocation of terminal info
        ord(TProp.kW):
        begin
            FpctkWOut := kW_Out / StorageVars.kWRating * 100.0;
            SyncUpPowerQuantities;   // keep kvar nominal up to date with kW and PF
        end;
        ord(TProp.pf):
            SyncUpPowerQuantities;   // keep kvar nominal up to date with kW and PF
        ord(TProp.kWrated):
            StorageVars.kVArating := StorageVars.kWrating;
        ord(TProp.kWhrated):
        begin
            StorageVars.kWhStored := StorageVars.kWhRating; // Assume fully charged
            kWhBeforeUpdate := StorageVars.kWhStored;
            StorageVars.kWhReserve := StorageVars.kWhRating * pctReserve * 0.01;
        end;

        ord(TProp.pctreserve):
            StorageVars.kWhReserve := StorageVars.kWhRating * pctReserve * 0.01;

        ord(TProp.conn):
        begin
            SetNCondsForConnection(self);

            // VBase is always L-N voltage unless 1-phase device or more than 3 phases

            case Fnphases of
                2, 3:
                    VBase := StorageVars.kVStorageBase * InvSQRT3x1000;    // L-N Volts
            else
                VBase := StorageVars.kVStorageBase * 1000.0;   // Just use what is supplied
            end;

            VBase95 := Vminpu * VBase;
            VBase105 := Vmaxpu * VBase;

            Yorder := Fnconds * Fnterms;
            YPrimInvalid := TRUE;
        end;
        ord(TProp.kv):
        begin
            case FNphases of
                2, 3:
                    VBase := StorageVars.kVStorageBase * InvSQRT3x1000;
            else
                VBase := StorageVars.kVStorageBase * 1000.0;
            end;
            VBase95 := Vminpu * VBase;
            VBase105 := Vmaxpu * VBase;
        end;
        ord(TProp.debugtrace):
            if DebugTrace then
            begin   // Init trace file
                FreeAndNil(TraceFile);
                TraceFile := TBufferedFileStream.Create(DSS.OutputDirectory + 'STOR_' + Name + '.csv', fmCreate);
                FSWrite(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, StorageModel,  Qnominalperphase, Pnominalperphase, CurrentType');
                for i := 1 to nphases do
                    FSWrite(Tracefile, ', |Iinj' + IntToStr(i) + '|');
                for i := 1 to nphases do
                    FSWrite(Tracefile, ', |Iterm' + IntToStr(i) + '|');
                for i := 1 to nphases do
                    FSWrite(Tracefile, ', |Vterm' + IntToStr(i) + '|');
                for i := 1 to NumVariables do
                    FSWrite(Tracefile, ', ' + VariableName(i));

                FSWrite(TraceFile, ',Vthev, Theta');
                FSWriteln(TraceFile);
                FSFlush(Tracefile);
            end
            else
            begin
                FreeAndNil(TraceFile);
            end;

        ord(TProp.UserModel):
        begin
            UserModel.Name := UserModelNameStr;
            IsUserModel := UserModel.Exists;
        end;
        ord(TProp.UserData):
            if UserModel.Exists then
                UserModel.Edit := UserModelEditStr;
        ord(TProp.DynaDLL):
        begin
            DynaModel.Name := DynaModelNameStr; 
            IsUserModel := DynaModel.Exists;
        end;
        ord(TProp.DynaData):
            if DynaModel.Exists then
                DynaModel.Edit := DynaModelEditStr;

        ord(TProp.kVA):
            kVANotSet := FALSE;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TStorage.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        RecalcElementData;
        YPrimInvalid := TRUE;
        Exclude(Flags, Flg.EditionActive);
    end;
    Result := True;
end;

procedure TStorageObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);

    Other := TObj(OtherPtr);
    if (Fnphases <> Other.Fnphases) then
    begin
        FNphases := Other.Fnphases;
        NConds := Fnphases;  // Forces reallocation of terminal stuff
        Yorder := Fnconds * Fnterms;
        YPrimInvalid := TRUE;
    end;

    StorageVars.kVStorageBase := Other.StorageVars.kVStorageBase;
    Vbase := Other.Vbase;
    Vminpu := Other.Vminpu;
    Vmaxpu := Other.Vmaxpu;
    Vbase95 := Other.Vbase95;
    Vbase105 := Other.Vbase105;
    kW_out := Other.kW_out;
    kvar_out := Other.kvar_out;
    Pnominalperphase := Other.Pnominalperphase;
    PFNominal := Other.PFNominal;
    Qnominalperphase := Other.Qnominalperphase;
    Connection := Other.Connection;
    YearlyShapeObj := Other.YearlyShapeObj;
    DailyShapeObj := Other.DailyShapeObj;
    DutyShapeObj := Other.DutyShapeObj;
    DispatchMode := Other.DispatchMode;
    StorageClass := Other.StorageClass;
    VoltageModel := Other.VoltageModel;

    Fstate := Other.Fstate;
    FstateChanged := Other.FstateChanged;
    kVANotSet := Other.kVANotSet;

    StorageVars.kVArating := Other.StorageVars.kVArating;
    StorageVars.kWRating := Other.StorageVars.kWRating;
    StorageVars.kWhRating := Other.StorageVars.kWhRating;
    StorageVars.kWhStored := Other.StorageVars.kWhStored;
    StorageVars.kWhReserve := Other.StorageVars.kWhReserve;
    kWhBeforeUpdate := Other.kWhBeforeUpdate;
    pctReserve := Other.pctReserve;
    DischargeTrigger := Other.DischargeTrigger;
    ChargeTrigger := Other.ChargeTrigger;
    pctChargeEff := Other.pctChargeEff;
    pctDischargeEff := Other.pctDischargeEff;
    pctkWout := Other.pctkWout;
    pctkWin := Other.pctkWin;
    pctIdlekW := Other.pctIdlekW;
    pctIdlekvar := Other.pctIdlekvar;
    ChargeTime := Other.ChargeTime;

    pctR := Other.pctR;
    pctX := Other.pctX;

    RandomMult := Other.RandomMult;

    UserModel.Name := Other.UserModel.Name;  // Connect to user written models
    DynaModel.Name := Other.DynaModel.Name;
    IsUserModel := Other.IsUserModel;
    ForceBalanced := Other.ForceBalanced;
    CurrentLimited := Other.CurrentLimited;
end;

procedure TStorage.ResetRegistersAll;  // Force all EnergyMeters in the circuit to reset
var
    idx: Integer;
begin
    idx := First;
    while idx > 0 do
    begin
        TStorageObj(GetActiveObj).ResetRegisters;
        idx := Next;
    end;
end;

procedure TStorage.SampleAll;  // Force all Storage elements in the circuit to take a sample
var
    i: Integer;
begin
    for i := 1 to ElementList.Count do
        with TStorageObj(ElementList.Get(i)) do
            if Enabled then
                TakeSample;
end;

constructor TStorageObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; // + STORAGE_ELEMENT;  // In both PCelement and Storageelement list
    TraceFile := nil;

    FNphases := 3;
    Fnconds := 4;  // defaults to wye
    Yorder := 0;  // To trigger an initial allocation
    Nterms := 1;  // forces allocations

    YearlyShapeObj := NIL;
    DailyShapeObj := NIL;
    DutyShapeObj := NIL;
    Connection := 0;    // Wye (star)
    VoltageModel := 1;  // Typical fixed kW negative load
    StorageClass := 1;

    StorageSolutionCount := -1;  // For keep track of the present solution in Injcurrent calcs
    OpenStorageSolutionCount := -1;
    YPrimOpenCond := NIL;

    StorageVars.kVStorageBase := 12.47;
    VBase := 7200.0;
    Vminpu := 0.90;
    Vmaxpu := 1.10;
    VBase95 := Vminpu * Vbase;
    VBase105 := Vmaxpu * Vbase;
    Yorder := Fnterms * Fnconds;
    RandomMult := 1.0;

    // Output rating stuff
    kW_out := 25.0;
    kvar_out := 0.0;
    kvarBase := kvar_out;     // initialize
    PFNominal := 1.0;
    with StorageVars do
    begin
        kWRating := 25.0;
        kVArating := kWRating * 1.0;
        kWhRating := 50;
        kWhStored := kWhRating;
        kWhBeforeUpdate := kWhRating;
        kWhReserve := kWhRating * pctReserve / 100.0;
    end;

    FState := STORE_IDLING;  // Idling and fully charged
    FStateChanged := TRUE;  // Force building of YPrim
    pctReserve := 20.0;  // per cent of kWhRating
    pctR := 0.0;
    pctX := 50.0;
    pctIdlekW := 1.0;
    pctIdlekvar := 0.0;

    DischargeTrigger := 0.0;
    ChargeTrigger := 0.0;
    pctChargeEff := 90.0;
    pctDischargeEff := 90.0;
    FpctkWout := 100.0;
    Fpctkvarout := 100.0;
    pctkWin := 100.0;

    ChargeTime := 2.0;   // 2 AM

    kVANotSet := TRUE;  // Flag to set the default value for kVA

    // Make the StorageVars struct as public
    PublicDataStruct := @StorageVars;
    PublicDataSize := SizeOf(TStorageVars);

    IsUserModel := FALSE;
    UserModel := TStoreUserModel.Create(DSS);
    DynaModel := TStoreDynaModel.Create(DSS);
    UserModelNameStr := '';
    UserModelEditStr := '';
    DynaModelNameStr := '';
    DynaModelEditStr := '';

    Reg_kWh := 1;
    Reg_kvarh := 2;
    Reg_MaxkW := 3;
    Reg_MaxkVA := 4;
    Reg_Hours := 5;
    Reg_Price := 6;

    DebugTrace := FALSE;
    StorageObjSwitchOpen := FALSE;
    SpectrumObj := NIL; // override base class

    ForceBalanced := FALSE;
    CurrentLimited := FALSE;

    RecalcElementData;
end;

destructor TStorageObj.Destroy;
begin
    YPrimOpenCond.Free;
    UserModel.Free;
    DynaModel.Free;
    FreeAndNil(TraceFile);
    inherited Destroy;
end;

procedure TStorageObj.Randomize(Opt: Integer);
begin
    case Opt of
        0:
            RandomMult := 1.0;
        GAUSSIAN:
            RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
        UNIfORM:
            RandomMult := Random;  // number between 0 and 1.0
        LOGNORMAL:
            RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
    end;
end;

procedure TStorageObj.CalcDailyMult(Hr: Double);
begin
    if (DailyShapeObj <> NIL) then
    begin
        ShapeFactor := DailyShapeObj.GetMultAtHour(Hr);
    end
    else
        ShapeFactor := CDOUBLEONE;  // Default to no  variation

    CheckStateTriggerLevel(ShapeFactor.re);   // last recourse
end;

procedure TStorageObj.CalcDutyMult(Hr: Double);
begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMultAtHour(Hr);
        CheckStateTriggerLevel(ShapeFactor.re);
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult If no duty curve specified
end;

procedure TStorageObj.CalcYearlyMult(Hr: Double);
begin
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMultAtHour(Hr);
        CheckStateTriggerLevel(ShapeFactor.re);
    end
    else
        CalcDailyMult(Hr);  // Defaults to Daily curve
end;

procedure TStorageObj.SetKWandKvarOut;
var
    OldState: Integer;
begin
    OldState := Fstate;
    with StorageVars do
        case FState of

            STORE_CHARGING:
            begin
                if kWhStored < kWhRating then
                    case DispatchMode of
                        STORE_FOLLOW:
                        begin
                            kW_out := kWRating * ShapeFactor.re;
                            kvar_out := kvarBase * ShapeFactor.im;    // ???
                        end
                    else
                        kW_out := -kWRating * pctkWin / 100.0;
                        if PFNominal = 1.0 then
                            kvar_out := 0.0
                        else
                            SyncUpPowerQuantities;  // computes kvar_out from PF
                    end
                else
                    Fstate := STORE_IDLING;   // all charged up
            end;


            STORE_DISCHARGING:
            begin
                if kWhStored > kWhReserve then
                    case DispatchMode of
                        STORE_FOLLOW:
                        begin
                            kW_out := kWRating * ShapeFactor.re;
                            kvar_out := kvarBase * ShapeFactor.im;
                        end
                    else
                        kW_out := kWRating * pctkWout / 100.0;
                        if PFNominal = 1.0 then
                            kvar_out := 0.0
                        else
                            SyncUpPowerQuantities; // computes kvar_out from PF
                    end
                else
                    Fstate := STORE_IDLING;  // not enough storage to discharge
            end;

        end;

    // If idling output is only losses

    if Fstate = STORE_IDLING then
    begin
        kW_out := 0.0;   // -kWIdlingLosses;     Just use YeqIdling
        kvar_out := 0.0;
    end;

    if OldState <> Fstate then
        FstateChanged := TRUE;
end;

procedure TStorageObj.SetNominalStorageOuput;
begin
    ShapeFactor := CDOUBLEONE;  // init here; changed by curve routine
    // Check to make sure the Storage element is ON
    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        if not (IsDynamicModel or IsHarmonicModel) then     // Leave Storage element in whatever state it was prior to entering Dynamic mode
        begin
          // Check dispatch to see what state the storage element should be in
            case DispatchMode of

                STORE_EXTERNALMODE: ;  // Do nothing
                STORE_LOADMODE:
                    CheckStateTriggerLevel(GeneratorDispatchReference);
                STORE_PRICEMODE:
                    CheckStateTriggerLevel(PriceSignal);

            else // dispatch off element's loadshapes, If any

                with Solution do
                    case Mode of
                        TSolveMode.SNAPSHOT: ; // Just solve for the present kW, kvar  // Don't check for state change
                        TSolveMode.DAILYMODE:
                            CalcDailyMult(DynaVars.dblHour); // Daily dispatch curve
                        TSolveMode.YEARLYMODE:
                            CalcYearlyMult(DynaVars.dblHour);
                        // MONTECARLO1,
                        // MONTEFAULT,
                        // FAULTSTUDY,
                        // DYNAMICMODE:   ; // // do nothing for these modes
                        TSolveMode.GENERALTIME:
                        begin
                         // This mode allows use of one class of load shape
                            case ActiveCircuit.ActiveLoadShapeClass of
                                USEDAILY:
                                    CalcDailyMult(DynaVars.dblHour);
                                USEYEARLY:
                                    CalcYearlyMult(DynaVars.dblHour);
                                USEDUTY:
                                    CalcDutyMult(DynaVars.dblHour);
                            else
                                ShapeFactor := CDOUBLEONE     // default to 1 + j1 if not known
                            end;
                        end;
                        // Assume Daily curve, If any, for the following
                        TSolveMode.MONTECARLO2,
                        TSolveMode.MONTECARLO3,
                        TSolveMode.LOADDURATION1,
                        TSolveMode.LOADDURATION2:
                            CalcDailyMult(DynaVars.dblHour);
                        TSolveMode.PEAKDAY:
                            CalcDailyMult(DynaVars.dblHour);

                        TSolveMode.DUTYCYCLE:
                            CalcDutyMult(DynaVars.dblHour);
                        // AUTOADDFLAG:  ;
                    end;
            end;

            SetKWandKvarOut;   // Based on State and amount of energy left in storage

            // Pnominalperphase is net at the terminal.  When discharging, the storage supplies the idling losses.
            // When charging, the idling losses are subtracting from the amount entering the storage element.

            Pnominalperphase := 1000.0 * kW_out / Fnphases;

            if Fstate = STORE_IDLING then
            begin
                if DispatchMode = STORE_EXTERNALMODE then   // Check for requested kvar
                    Qnominalperphase := StorageVars.kvarRequested / Fnphases * 1000.0
                else
                    Qnominalperphase := 0.0;
                Yeq := Cmplx(Pnominalperphase, -Qnominalperphase) / Sqr(Vbase);   // Vbase must be L-N for 3-phase
                Yeq95 := Yeq;
                Yeq105 := Yeq;
            end
            else
            begin
                Qnominalperphase := 1000.0 * kvar_out / Fnphases;

                case VoltageModel of
                    //****  Fix this when user model gets connected in
                    3: // Yeq := Cinv(cmplx(0.0, -StoreVARs.Xd))  ;  // Gets negated in CalcYPrim
                else
                    // Yeq no longer used for anything other than this calculation of Yeq95, Yeq105 and
                    // constant Z power flow model
                    Yeq := Cmplx(Pnominalperphase, -Qnominalperphase) / Sqr(Vbase);   // Vbase must be L-N for 3-phase
                    if (Vminpu <> 0.0) then
                        Yeq95 := Yeq / sqr(Vminpu)  // at 95% voltage
                    else
                        Yeq95 := Yeq; // Always a constant Z model

                    if (Vmaxpu <> 0.0) then
                        Yeq105 := Yeq / Sqr(Vmaxpu)   // at 105% voltage
                    else
                        Yeq105 := Yeq;
                end;
                // Like Model 7 generator, max current is based on amount of current to get out requested power at min voltage
                with StorageVars do
                begin
                    PhaseCurrentLimit := Cmplx(Pnominalperphase, Qnominalperphase) / VBase95;
                    MaxDynPhaseCurrent := Cabs(PhaseCurrentLimit);
                end;

            end;
              // When we leave here, all the Yeq's are in L-N values

        end;  // If  NOT (IsDynamicModel or IsHarmonicModel)
    end;  // With ActiveCircuit

    // If Storage element state changes, force re-calc of Y matrix
    if FStateChanged then
    begin
        YPrimInvalid := TRUE;
        FStateChanged := FALSE;  // reset the flag
    end;
end;

procedure TStorageObj.RecalcElementData;
begin
    VBase95 := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    // removed 5/8/17 kvarBase := kvar_out ;  // remember this for Follow Mode

    // values in ohms for thevenin equivalents
    StorageVars.RThev := pctR * 0.01 * SQR(PresentkV) / StorageVars.kVARating * 1000.0;
    StorageVars.XThev := pctX * 0.01 * SQR(PresentkV) / StorageVars.kVARating * 1000.0;

    // efficiencies
    StorageVars.ChargeEff := pctChargeEff * 0.01;
    StorageVars.DisChargeEff := pctDisChargeEff * 0.01;

    YeqIdling := Cmplx(pctIdlekW, pctIdlekvar) * (StorageVars.kWrating * 10.0 / SQR(vbase) / FNPhases);  // 10.0 = 1000/100 = kW->W/pct
    YeqDischarge := Cmplx((StorageVars.kWrating * 1000.0 / SQR(vbase) / FNPhases), 0.0);

    SetNominalStorageOuput;

    // Initialize to Zero - defaults to PQ Storage element
    // Solution object will reset after circuit modifications

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    // Update any user-written models
    if Usermodel.Exists then
        UserModel.FUpdateModel;  // Checks for existence and Selects
    if Dynamodel.Exists then
        Dynamodel.FUpdateModel;  // Checks for existence and Selects

end;

procedure TStorageObj.CalcYPrimMatrix(Ymatrix: TcMatrix);
var
    Y, Yij: Complex;
    i, j: Integer;
    FreqMultiplier: Double;
begin
    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

    with  ActiveCircuit.solution do
        if IsHarmonicModel then // IsDynamicModel or
        begin
            // Yeq is computed from %R and %X -- inverse of Rthev + j Xthev
            case Fstate of
                STORE_CHARGING:
                    Y := YeqDischarge + YeqIdling;
                STORE_IDLING:
                    Y := YeqIdling;
                STORE_DISCHARGING:
                    Y := -YeqDischarge + YeqIdling;
               // old way Y  := Yeq   // L-N value computed in initialization routines
            end;

            if Connection = 1 then
                Y := Y / 3.0; // Convert to delta impedance
            Y.im := Y.im / FreqMultiplier;
            Yij := -Y;
            for i := 1 to Fnphases do
            begin
                case Connection of
                    0:
                    begin
                        Ymatrix.SetElement(i, i, Y);
                        Ymatrix.AddElement(Fnconds, Fnconds, Y);
                        Ymatrix.SetElemsym(i, Fnconds, Yij);
                    end;
                    1:
                    begin // Delta connection
                        Ymatrix.SetElement(i, i, Y);
                        Ymatrix.AddElement(i, i, Y);  // put it in again
                        for j := 1 to i - 1 do
                            Ymatrix.SetElemsym(i, j, Yij);
                    end;
                end;
            end;
        end
        else
        begin  //  Regular power flow Storage element model
            // Yeq is always expected as the equivalent line-neutral admittance
            case Fstate of
                STORE_CHARGING:
                    Y := YeqDischarge + YeqIdling;
                STORE_IDLING:
                    Y := YeqIdling;
                STORE_DISCHARGING:
                    Y := -YeqDischarge + YeqIdling;
            end;

            // ****** Need to modify the base admittance for real harmonics calcs
            Y.im := Y.im / FreqMultiplier;

            case Connection of

                0:
                    with YMatrix do
                    begin // WYE
                        Yij := -Y;
                        for i := 1 to Fnphases do
                        begin
                            SetElement(i, i, Y);
                            AddElement(Fnconds, Fnconds, Y);
                            SetElemsym(i, Fnconds, Yij);
                        end;
                    end;

                1:
                    with YMatrix do
                    begin  // Delta  or L-L
                        Y := Y / 3.0; // Convert to delta impedance
                        Yij := -Y;
                        for i := 1 to Fnphases do
                        begin
                            j := i + 1;
                            if j > Fnconds then
                                j := 1;  // wrap around for closed connections
                            AddElement(i, i, Y);
                            AddElement(j, j, Y);
                            AddElemSym(i, j, Yij);
                        end;
                    end;

            end;
        end; // ELSE IF Solution.mode
end;

function TStorageObj.NormalizeToTOD(h: Integer; sec: Double): Double;
// Normalize time to a floating point number representing time of day If Hour > 24
// time should be 0 to 24.
var
    HourOfDay: Integer;
begin
    if h > 23 then
        HourOfDay := (h - (h div 24) * 24)
    else
        HourOfDay := h;

    Result := HourOfDay + sec / 3600.0;

    if Result > 24.0 then
        Result := Result - 24.0;   // Wrap around

end;

procedure TStorageObj.CheckStateTriggerLevel(Level: Double);
// This is where we set the state of the Storage element
var
    OldState: Integer;
begin
    FStateChanged := FALSE;

    OldState := Fstate;

    with StorageVars do
        if DispatchMode = STORE_FOLLOW then
        begin
            // set charge and discharge modes based on sign of loadshape
            if (Level > 0.0) and (kWhStored > kWhReserve) then
                StorageState := STORE_DISCHARGING
            else
            if (Level < 0.0) and (kWhStored < kWhRating) then
                StorageState := STORE_CHARGING
            else
                StorageState := STORE_IDLING;

        end
        else
        begin   // All other dispatch modes  Just compare to trigger value

            if (ChargeTrigger = 0.0) and (DischargeTrigger = 0.0) then
                Exit;

            // First see If we want to turn off Charging or Discharging State
            case Fstate of
                STORE_CHARGING:
                    if (ChargeTrigger <> 0.0) then
                        if (ChargeTrigger < Level) or (kWhStored >= kWHRating) then
                            Fstate := STORE_IDLING;
                STORE_DISCHARGING:
                    if (DischargeTrigger <> 0.0) then
                        if (DischargeTrigger > Level) or (kWhStored <= kWHReserve) then
                            Fstate := STORE_IDLING;
            end;

            // Now check to see If we want to turn on the opposite state
            case Fstate of
                STORE_IDLING:
                begin
                    if (DischargeTrigger <> 0.0) and (DischargeTrigger < Level) and (kWhStored > kWHReserve) then
                        FState := STORE_DISCHARGING
                    else
                    if (ChargeTrigger <> 0.0) and (ChargeTrigger > Level) and (kWhStored < kWHRating) then
                        Fstate := STORE_CHARGING;

                               // Check to see If it is time to turn the charge cycle on If it is not already on.
                    if not (Fstate = STORE_CHARGING) then
                        if ChargeTime > 0.0 then
                            with ActiveCircuit.Solution do
                            begin
                                if abs(NormalizeToTOD(DynaVars.intHour, DynaVARs.t) - ChargeTime) < DynaVARs.h / 3600.0 then
                                    Fstate := STORE_CHARGING;
                            end;
                end;
            end;
        end;

    if OldState <> Fstate then
    begin
        FstateChanged := TRUE;
        YPrimInvalid := TRUE;
    end;
end;

procedure TStorageObj.CalcYPrim;
var
    i: Integer;
begin
    // Build only shunt Yprim
    // Build a dummy Yprim Series so that CalcV Does not fail
    if (Yprim = NIL) OR (Yprim.order <> Yorder) OR (Yprim_Shunt = NIL) OR (Yprim_Series = NIL) then // YPrimInvalid
    begin
        if YPrim_Shunt <> NIL then
            YPrim_Shunt.Free;
        YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
        if YPrim_Series <> NIL then
            Yprim_Series.Free;
        YPrim_Series := TcMatrix.CreateMatrix(Yorder);
        if YPrim <> NIL then
            YPrim.Free;
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin
        YPrim_Shunt.Clear;
        YPrim_Series.Clear;
        YPrim.Clear;
    end;

    SetNominalStorageOuput;
    CalcYPrimMatrix(YPrim_Shunt);

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages Doesn't fail
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, Yprim_Shunt.Getelement(i, i) * 1.0e-10);

    YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
    inherited CalcYPrim;
end;

procedure TStorageObj.StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);
// Add the current into the proper location according to connection

// Reverse of similar routine in load  (Cnegates are switched)
var
    j: Integer;
begin
    case Connection of
        0:
        begin  //Wye
            TermArray^[i] += Curr;
            TermArray^[Fnconds] -= Curr; // Neutral
        end;
        1:
        begin //DELTA
            TermArray^[i] += Curr;
            j := i + 1;
            if j > Fnconds then
                j := 1;
            TermArray^[j] -= Curr;
        end;
    end;
end;

procedure TStorageObj.WriteTraceRecord(const s: String);
var
    i: Integer;
    sout: String;
begin
    try
        if (not DSS.InShowResults) then
        begin
            WriteStr(sout, Format('%-.g, %d, %-.g, ',
                [ActiveCircuit.Solution.DynaVars.dblHour,
                ActiveCircuit.Solution.Iteration,
                ActiveCircuit.LoadMultiplier]),
                DSS.SolveModeEnum.OrdinalToString(ord(DSS.ActiveCircuit.Solution.mode)), ', ',
                DSS.DefaultLoadModelEnum.OrdinalToString(DSS.ActiveCircuit.Solution.LoadModel), ', ',
                VoltageModel: 0, ', ',
                (Qnominalperphase * 3.0 / 1.0e6): 8: 2, ', ',
                (Pnominalperphase * 3.0 / 1.0e6): 8: 2, ', ',
                s, ', ');
            FSWrite(TraceFile, sout);
            
            for i := 1 to nphases do
            begin
                WriteStr(sout, (Cabs(InjCurrent^[i])): 8: 1, ', ');
                FSWrite(TraceFile, sout);
            end;
            for i := 1 to nphases do
            begin
                WriteStr(sout, (Cabs(ITerminal^[i])): 8: 1, ', ');
                FSWrite(TraceFile, sout);
            end;
            for i := 1 to nphases do
            begin
                WriteStr(sout, (Cabs(Vterminal^[i])): 8: 1, ', ');
                FSWrite(TraceFile, sout);
            end;
            for i := 1 to NumVariables do
                FSWrite(TraceFile, Format('%-.g, ', [Variable[i]]));

            FSWriteln(Tracefile);
            FSFlush(TraceFile);
        end;
    except
        On E: Exception do
        begin
        end;

    end;
end;

procedure TStorageObj.DoConstantPQStorageObj;
// Compute total terminal current for Constant PQ
var
    i: Integer;
    Curr,
    VLN, VLL: Complex;
   //---DEBUG--- S:Complex;
    VmagLN,
    VmagLL: Double;
    V012: array[0..2] of Complex;  // Sequence voltages

begin
     //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    ZeroITerminal;

    case FState of
        STORE_IDLING:  // YPrim current is only current
        begin
            for i := 1 to FNPhases do
            begin
                Curr := InjCurrent^[i];
                StickCurrInTerminalArray(ITerminal, Curr, i);  // Put YPrim contribution into Terminal array taking into account connection
                IterminalUpdated := TRUE;
                StickCurrInTerminalArray(InjCurrent, -Curr, i);    // Compensation current is zero since terminal current is same as Yprim contribution
            end;
        end;
    else   // For Charging and Discharging

        CalcVTerminalPhase; // get actual voltage across each phase of the load

        if ForceBalanced and (Fnphases = 3) then
        begin  // convert to pos-seq only
            Phase2SymComp(Vterminal, pComplexArray(@V012));
            V012[0] := CZERO; // Force zero-sequence voltage to zero
            V012[2] := CZERO; // Force negative-sequence voltage to zero
            SymComp2Phase(Vterminal, pComplexArray(@V012));  // Reconstitute Vterminal as balanced
        end;

        for i := 1 to Fnphases do
        begin
            case Connection of

                0:
                begin  // Wye
                    VLN := Vterminal^[i];
                    VMagLN := Cabs(VLN);
                    if VMagLN <= VBase95 then
                        Curr := Yeq95 * VLN  // Below 95% use an impedance model
                    else
                    if VMagLN > VBase105 then
                        Curr := Yeq105 * VLN  // above 105% use an impedance model
                    else
                        Curr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / VLN);  // Between 95% -105%, constant PQ
                    if CurrentLimited then
                        if Cabs(Curr) > MaxDynPhaseCurrent then
                            Curr := cong(PhaseCurrentLimit / (VLN / VMagLN));
                end;

                1:
                begin  // Delta
                    VLL := Vterminal^[i];
                    VMagLL := Cabs(VLL);
                    if Fnphases > 1 then
                        VMagLN := VMagLL / SQRT3
                    else
                        VMagLN := VmagLL;  // L-N magnitude
                    if VMagLN <= VBase95 then
                        Curr := (Yeq95 / 3.0) * VLL  // Below 95% use an impedance model
                    else
                    if VMagLN > VBase105 then
                        Curr := (Yeq105 / 3.0) * VLL  // above 105% use an impedance model
                    else
                        Curr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / VLL);  // Between 95% -105%, constant PQ
                    if CurrentLimited then
                        if Cabs(Curr) * SQRT3 > MaxDynPhaseCurrent then
                            Curr := cong(PhaseCurrentLimit / (VLL / VMagLN)); // Note VmagLN has sqrt3 factor in it
                end;

            end;
            StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
            IterminalUpdated := TRUE;
            StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
        end;
    end;
end;

procedure TStorageObj.DoConstantZStorageObj;
// constant Z model
var
    i: Integer;
    Curr,
    Yeq2: Complex;
    V012: array[0..2] of Complex;  // Sequence voltages
begin
    // Assume Yeq is kept up to date
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;
    if Connection = 0 then
        Yeq2 := Yeq
    else
        Yeq2 := Yeq / 3.0;

    if ForceBalanced and (Fnphases = 3) then
    begin  // convert to pos-seq only
        Phase2SymComp(Vterminal, pComplexArray(@V012));
        V012[0] := CZERO; // Force zero-sequence voltage to zero
        V012[2] := CZERO; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, pComplexArray(@V012));  // Reconstitute Vterminal as balanced
    end;

    for i := 1 to Fnphases do
    begin
        Curr := Yeq2 * Vterminal^[i];   // Yeq is always line to neutral
        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection

    end;
end;

procedure TStorageObj.DoUserModel;
// Compute total terminal Current from User-written model
var
    i: Integer;
begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

    if UserModel.Exists then    // Check automatically selects the usermodel If true
    begin
        UserModel.FCalc(Vterminal, Iterminal);
        IterminalUpdated := TRUE;
        with ActiveCircuit.Solution do
        begin          // Negate currents from user model for power flow Storage element model
            for i := 1 to FnConds do
                InjCurrent^[i] -= Iterminal^[i];
        end;
    end
    else
    begin
        DoSimpleMsg('%s model designated to use user-written model, but user-written model is not defined.', [FullName], 567);
    end;
end;

procedure TStorageObj.DoDynamicMode;
// Compute Total Current and add into InjTemp

//   For now, just assume the storage element Thevenin voltage is constant
//   for the duration of the dynamic simulation.
var
    i: Integer;
    V012,
    I012: array[0..2] of Complex;


    procedure CalcVthev_Dyn;
    begin
        with StorageVars do
            Vthev := pclx(VthevMag, Theta);   // keeps theta constant
    end;

begin
    // Test using DESS model
    // Compute Vterminal

    if DynaModel.Exists then
        DoDynaModel   // do user-written model

    else
    begin
        CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
        ZeroITerminal;

        // Simple Thevenin equivalent
        // compute terminal current (Iterminal) and take out the Yprim contribution

        with StorageVars do
            case Fnphases of
                1:
                begin
                    CalcVthev_Dyn;  // Update for latest phase angle
                    ITerminal^[1] := (VTerminal^[1] - Vthev - VTerminal^[2]) / Zthev;
                    if CurrentLimited then
                        if Cabs(Iterminal^[1]) > MaxDynPhaseCurrent then   // Limit the current but keep phase angle
                            ITerminal^[1] := ptocomplex(topolar(MaxDynPhaseCurrent, cang(Iterminal^[1])));
                    ITerminal^[2] := -ITerminal^[1];
                end;
                3:
                begin
                    Phase2SymComp(Vterminal, pComplexArray(@V012));

                    // Positive Sequence Contribution to Iterminal
                    CalcVthev_Dyn;  // Update for latest phase angle

                    // Positive Sequence Contribution to Iterminal
                    I012[1] := (V012[1] - Vthev) / Zthev;

                    if CurrentLimited and (Cabs(I012[1]) > MaxDynPhaseCurrent) then   // Limit the pos seq current but keep phase angle
                        I012[1] := ptocomplex(topolar(MaxDynPhaseCurrent, cang(I012[1])));

                    if ForceBalanced then
                    begin
                        I012[2] := CZERO;
                    end
                    else
                        I012[2] := V012[2] / Zthev;  // for inverter

                    I012[0] := CZERO;

                    SymComp2Phase(ITerminal, pComplexArray(@I012));  // Convert back to phase components

                end;
            else
                DoSimpleMsg('Dynamics mode is implemented only for 1- or 3-phase Storage Element. %s has %d phases.', [FullName, Fnphases], 5671);
                DSS.SolutionAbort := TRUE;
            end;

        // Add it into inj current array
        for i := 1 to FnConds do
            InjCurrent^[i] -= Iterminal^[i];
    end;
end;

procedure TStorageObj.DoDynaModel;
var
    DESSCurr: array[1..6] of Complex;  // Temporary biffer
    i: Integer;
begin
    // do user written dynamics model

    with ActiveCircuit.Solution do
    begin  // Just pass node voltages to ground and let dynamic model take care of it
        for i := 1 to FNconds do
            VTerminal^[i] := NodeV^[NodeRef^[i]];
        StorageVars.w_grid := TwoPi * Frequency;
    end;

    DynaModel.FCalc(Vterminal, pComplexArray(@DESSCurr));

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        StickCurrInTerminalArray(ITerminal, -DESSCurr[i], i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, DESSCurr[i], i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TStorageObj.DoHarmonicMode;
// Compute Injection Current Only when in harmonics mode

// Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built
// Vd is the fundamental frequency voltage behind Xd" for phase 1
var
    i: Integer;
    E: Complex;
    StorageHarmonic: Double;
    pBuffer: PCBuffer24;
begin
    pBuffer := @TStorage(ParentClass).cBuffer;
    ComputeVterminal;

    with ActiveCircuit.Solution do
    begin
        StorageHarmonic := Frequency / StorageFundamental;
        if SpectrumObj <> NIL then
            E := SpectrumObj.GetMult(StorageHarmonic) * StorageVars.VThevHarm // Get base harmonic magnitude
        else
            E := CZERO;

        RotatePhasorRad(E, StorageHarmonic, StorageVars.ThetaHarm);  // Time shift by fundamental frequency phase shift
        for i := 1 to Fnphases do
        begin
            pBuffer[i] := E;
            if i < Fnphases then
                RotatePhasorDeg(E, StorageHarmonic, -120.0);  // Assume 3-phase Storage element
        end;
    end;

    // Handle Wye Connection
    if Connection = 0 then
        pBuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

    // Inj currents = Yprim (E) 
    YPrim.MVMult(InjCurrent, pComplexArray(pBuffer));
end;

procedure TStorageObj.CalcVTerminalPhase;
var
    i, j: Integer;
begin
    // Establish phase voltages and stick in Vterminal
    case Connection of

        0:
        begin
            with ActiveCircuit.Solution do
                for i := 1 to Fnphases do
                    Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[Fnconds]);
        end;

        1:
        begin
            with ActiveCircuit.Solution do
                for i := 1 to Fnphases do
                begin
                    j := i + 1;
                    if j > Fnconds then
                        j := 1;
                    Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[j]);
                end;
        end;

    end;

    StorageSolutionCount := ActiveCircuit.Solution.SolutionCount;
end;

procedure TStorageObj.CalcStorageModelContribution;
// Calculates Storage element current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)
begin
    IterminalUpdated := FALSE;
    with  ActiveCircuit, ActiveCircuit.Solution do
    begin
        if IsDynamicModel then
            DoDynamicMode
        else
        if IsHarmonicModel and (Frequency <> Fundamental) then
            DoHarmonicMode
        else
        begin
            //  compute currents and put into InjTemp array;
            case VoltageModel of
                1:
                    DoConstantPQStorageObj;
                2:
                    DoConstantZStorageObj;
                3:
                    DoUserModel;
            else
                DoConstantPQStorageObj;  // for now, until we implement the other models.
            end;
        end;
    end;
   // When this is Done, ITerminal is up to date
end;

procedure TStorageObj.CalcInjCurrentArray;
// Difference between currents in YPrim and total current
begin
      // Now Get Injection Currents
    if StorageObjSwitchOpen then
        ZeroInjCurrent
    else
        CalcStorageModelContribution;
end;

procedure TStorageObj.GetTerminalCurrents(Curr: pComplexArray);
// Compute total Currents
begin
    with ActiveCircuit.Solution do
    begin
        if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
        begin     // recalc the contribution
            if not StorageObjSwitchOpen then
                CalcStorageModelContribution;  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr);
    end;

    if (DebugTrace) then
        WriteTraceRecord('TotalCurrent');
end;

function TStorageObj.InjCurrents: Integer;
begin
    with ActiveCircuit.Solution do
    begin
        if LoadsNeedUpdating then
            SetNominalStorageOuput; // Set the nominal kW, etc for the type of solution being Done

        CalcInjCurrentArray;          // Difference between currents in YPrim and total terminal current

        if (DebugTrace) then
            WriteTraceRecord('Injection');

         // Add into System Injection Current Array

        Result := inherited InjCurrents;
    end;
end;

procedure TStorageObj.ResetRegisters;
var
    i: Integer;
begin
    for i := 1 to NumStorageRegisters do
        Registers[i] := 0.0;
    for i := 1 to NumStorageRegisters do
        Derivatives[i] := 0.0;
    FirstSampleAfterReset := TRUE;  // initialize for trapezoidal integration
end;

procedure TStorageObj.Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
begin
    if ActiveCircuit.TrapezoidalIntegration then
    begin
        // Trapezoidal Rule Integration
        if not FirstSampleAfterReset then
            Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
    end
    else   // Plain Euler integration
        Registers[Reg] := Registers[Reg] + Interval * Deriv;

    Derivatives[Reg] := Deriv;
end;

procedure TStorageObj.TakeSample;
// Update Energy from metered zone
var
    S: Complex;
    Smag: Double;
    HourValue: Double;
begin
    // Compute energy in Storage element branch
    if Enabled then
    begin
        // Only tabulate discharge hours
        if FSTate = STORE_DISCHARGING then
        begin
            S := cmplx(Get_PresentkW, Get_Presentkvar);
            Smag := Cabs(S);
            HourValue := 1.0;
        end
        else
        begin
            S := CZERO;
            Smag := 0.0;
            HourValue := 0.0;
        end;

        if (FState = STORE_DISCHARGING) or ActiveCircuit.TrapezoidalIntegration then
            // Make sure we always integrate for Trapezoidal case
            // Don't need to for Gen Off and normal integration
            with ActiveCircuit.Solution do
            begin
                if ActiveCircuit.PositiveSequence then
                begin
                    S := S * 3;
                    Smag := 3.0 * Smag;
                end;
                Integrate(Reg_kWh, S.re, IntervalHrs);   // Accumulate the power
                Integrate(Reg_kvarh, S.im, IntervalHrs);
                SetDragHandRegister(Reg_MaxkW, abs(S.re));
                SetDragHandRegister(Reg_MaxkVA, Smag);
                Integrate(Reg_Hours, HourValue, IntervalHrs);  // Accumulate Hours in operation
                Integrate(Reg_Price, S.re * ActiveCircuit.PriceSignal * 0.001, IntervalHrs);  // Accumulate Hours in operation
                FirstSampleAfterReset := FALSE;
            end;
    end;
end;

procedure TStorageObj.UpdateStorage;
// Update Storage levels
begin
    with StorageVars do
    begin
        kWhBeforeUpdate := kWhStored;   // keep this for reporting change in storage as a variable

        // Assume User model will take care of updating storage in dynamics mode
        if ActiveCircuit.solution.IsDynamicModel and IsUserModel then
            Exit;


        with ActiveCircuit.Solution do
            case FState of

                STORE_DISCHARGING:
                begin
                    // Deplete storage by amount of Idling Power to achieve Present kW output
                    kWhStored := kWhStored - (PresentkW + kWIdlingLosses) * IntervalHrs / DischargeEff;
                    if kWhStored < kWhReserve then
                    begin
                        kWhStored := kWhReserve;
                        Fstate := STORE_IDLING;  // It's empty Turn it off
                        FstateChanged := TRUE;
                    end;
                end;

                STORE_CHARGING:
                begin
                    // kWIdlingLosses is always positive while PresentkW is negative for Charging
                    kWhStored := kWhStored - (PresentkW + kWIdlingLosses) * IntervalHrs * ChargeEff;
                    if kWhStored > kWhRating then
                    begin
                        kWhStored := kWhRating;
                        Fstate := STORE_IDLING;  // It's full Turn it off
                        FstateChanged := TRUE;
                    end;
                end;
            end;

    end;

    // the update is done at the end of a time step so have to force
    // a recalc of the Yprim for the next time step.  Else it will stay the same.
    if FstateChanged then
        YPrimInvalid := TRUE;
end;

function TStorageObj.Get_PresentkW: Double;
begin
    Result := kW_Out;  //Pnominalperphase * 0.001 * Fnphases;
end;

function TStorageObj.Get_kWTotalLosses: Double;
begin
    Result := 0.0;
    case StorageState of
        STORE_CHARGING:
            Result := abs(Power[1].re * (100.0 - pctChargeEff) / 100000.0) + pctChargeEff * kWIdlingLosses / 100.0; // kW
        STORE_IDLING:
            Result := kWIdlingLosses;
        STORE_DISCHARGING:
            Result := abs(Power[1].re * (100.0 / pctDisChargeEff - 1.0) / 1000.0) + (100.0 / pctDisChargeEff) * kWIdlingLosses;  // kW
    end;
end;

function TStorageObj.Get_kWIdlingLosses: Double;
var
    i: Integer;
begin
    ComputeVterminal;

    Result := 0.0;
    // Compute sum of SQR(V) at this device -- sum of VV*
    for i := 1 to FNphases do
        Result := Result + (Vterminal^[i] * cong(VTerminal^[i])).re;

    Result := Result * YeqIdling.re * 0.001;  // to kW

end;

function TStorageObj.Get_PresentkV: Double;
begin
    Result := StorageVars.kVStorageBase;
end;

function TStorageObj.Get_Presentkvar: Double;
begin
    Result := kvar_out;   // Qnominalperphase * 0.001 * Fnphases;
end;

procedure TStorageObj.InitHarmonics;
// This routine makes a thevenin equivalent behis the reactance spec'd in %R and %X
var
    E, Va: complex;
begin
    YPrimInvalid := TRUE;  // Force rebuild of YPrims
    StorageFundamental := ActiveCircuit.Solution.Frequency;  // Whatever the frequency is when we enter here.

    Yeq := Cinv(Cmplx(StorageVars.RThev, StorageVars.XThev));      // used for current calcs  Always L-N

    // Compute reference Thevinen voltage from phase 1 current

    if FState = STORE_DISCHARGING then
    begin
        ComputeIterminal;  // Get present value of current

        with ActiveCircuit.solution do
            case Connection of
                0:
                begin // wye - neutral is explicit
                    Va := NodeV^[NodeRef^[1]] - NodeV^[NodeRef^[Fnconds]];
                end;
                1:
                begin  // delta -- assume neutral is at zero
                    Va := NodeV^[NodeRef^[1]];
                end;
            end;

        E := Va - Iterminal^[1] * cmplx(StorageVars.Rthev, StorageVars.Xthev);
        StorageVars.Vthevharm := Cabs(E);   // establish base mag and angle
        StorageVars.ThetaHarm := Cang(E);
    end
    else
    begin
        StorageVars.Vthevharm := 0.0;
        StorageVars.ThetaHarm := 0.0;
    end;
end;

procedure TStorageObj.InitStateVars;
// for going into dynamics mode
var
    // VNeut: Complex;
    VThevPolar: Polar;
    i: Integer;
    V012,
    I012: array[0..2] of Complex;
    Vabc: array[1..3] of Complex;
begin
    YPrimInvalid := TRUE;  // Force rebuild of YPrims

    with StorageVars do
    begin
        ZThev := Cmplx(RThev, XThev);
        Yeq := Cinv(ZThev);  // used to init state vars
    end;


    if DynaModel.Exists then   // Checks existence and selects
    begin
        ComputeIterminal;
        ComputeVterminal;
        with StorageVars do
        begin
            NumPhases := Fnphases;
            NumConductors := Fnconds;
            w_grid := twopi * ActiveCircuit.Solution.Frequency;
        end;
        DynaModel.FInit(Vterminal, Iterminal);
    end
    else
    begin
        // Compute nominal Positive sequence voltage behind equivalent filter impedance
        if FState = STORE_DISCHARGING then
            with ActiveCircuit.Solution do
            begin
                ComputeIterminal;

                if FnPhases = 3 then
                begin
                    Phase2SymComp(ITerminal, pComplexArray(@I012));
                    // Voltage behind Xdp  (transient reactance), volts
                    // case Connection of
                    //     0:
                    //         Vneut := NodeV^[NodeRef^[Fnconds]]
                    // else
                    //     Vneut := CZERO;
                    // end;

                    for i := 1 to FNphases do
                        Vabc[i] := NodeV^[NodeRef^[i]];   // Wye Voltage

                    Phase2SymComp(pComplexArray(@Vabc), pComplexArray(@V012));
                    with StorageVars do
                    begin
                        Vthev := V012[1] - I012[1] * ZThev;    // Pos sequence
                        VThevPolar := cToPolar(VThev);
                        VThevMag := VThevPolar.mag;
                        Theta := VThevPolar.ang;  // Initial phase angle
                    end;
                end
                else
                begin   // Single-phase Element
                    for i := 1 to Fnconds do
                        Vabc[i] := NodeV^[NodeRef^[i]];
                    with StorageVars do
                    begin
                        Vthev := VDiff(NodeRef^[1], NodeRef^[2]) - ITerminal^[1] * ZThev;    // Pos sequence
                        VThevPolar := cToPolar(VThev);
                        VThevMag := VThevPolar.mag;
                        Theta := VThevPolar.ang;  // Initial phase angle
                    end;

                end;
            end;
    end;
end;

procedure TStorageObj.IntegrateStates;
// dynamics mode integration routine
begin
   // Compute Derivatives and Then integrate

    ComputeIterminal;

    if Dynamodel.Exists then   // Checks for existence and Selects

        DynaModel.Integrate

    else
        with ActiveCircuit.Solution, StorageVars do
        begin
            with StorageVars do
                if (Dynavars.IterationFlag = 0) then
                begin // First iteration of new time step
                end;

            // Compute shaft dynamics
            // TracePower := TerminalPowerIn(Vterminal, Iterminal, FnPhases);
            // Write Dynamics Trace Record
            if DebugTrace then
            begin
                FSWrite(TraceFile, Format('t=%-.5g ', [Dynavars.t]));
                FSWrite(TraceFile, Format(' Flag=%d ', [Dynavars.Iterationflag]));
                FSWriteln(TraceFile);
                FSFlush(TraceFile);
            end;

        end;
end;

function TStorageObj.Get_Variable(i: Integer): Double;
// Return variables one at a time
var
    N, k: Integer;
begin
    Result := -9999.99;  // error return value; no state fars
    if i < 1 then
        Exit;
    // for now, report kWhstored and mode
    with StorageVars do
        case i of
            1:
                Result := kWhStored;
            2:
                Result := FState;
            3:
                if not (FState = STORE_DISCHARGING) then
                    Result := 0.0
                else
                    Result := Power[1].re * 0.001; // kW_Out; // pctkWout;
            4:
                if not (FState = STORE_CHARGING) then
                    Result := 0.0
                else
                    Result := Power[1].re * 0.001; // kW_out; // pctkWin;
            5:
                Result := kWTotalLosses; // Present kW charge or discharge loss incl idle losses
            6:
                Result := kWIdlingLosses; // Present Idling Loss
            7:
                Result := kWhStored - kWhBeforeUpdate;
        else
        begin
            if UserModel.Exists then   // Checks for existence and Selects
            begin
                N := UserModel.FNumVars;
                k := (i - NumStorageVariables);
                if k <= N then
                begin
                    Result := UserModel.FGetVariable(k);
                    Exit;
                end;
            end;
            if DynaModel.Exists then  // Checks for existence and Selects
            begin
                N := DynaModel.FNumVars;
                k := (i - NumStorageVariables);
                if k <= N then
                begin
                    Result := DynaModel.FGetVariable(k);
                    Exit;
                end;
            end;
        end;
        end;
end;

procedure TStorageObj.Set_Variable(i: Integer; Value: Double);
var
    N, k: Integer;

begin
    if i < 1 then
        Exit;  // No variables to set

    with StorageVars do
        case i of
            1:
                kWhStored := Value;
            2:
                Fstate := Trunc(Value);
            3:
                pctkWout := Value;
            4:
                pctkWin := Value;
            5..7: ; // Do Nothing; read only
        else
        begin
            if UserModel.Exists then    // Checks for existence and Selects
            begin
                N := UserModel.FNumVars;
                k := (i - NumStorageVariables);
                if k <= N then
                begin
                    UserModel.FSetVariable(k, Value);
                    Exit;
                end;
            end;
            if DynaModel.Exists then     // Checks for existence and Selects
            begin
                N := DynaModel.FNumVars;
                k := (i - NumStorageVariables);
                if k <= N then
                begin
                    DynaModel.FSetVariable(k, Value);
                    Exit;
                end;
            end;
        end;
        end;
end;

procedure TStorageObj.GetAllVariables(States: pDoubleArray);
var
    i: Integer;
begin
    for i := 1 to NumStorageVariables do
        States^[i] := Variable[i];

    if UserModel.Exists then
    begin    // Checks for existence and Selects
        // N := UserModel.FNumVars;
        UserModel.FGetAllVars(pDoubleArray(@States^[NumStorageVariables + 1]));
    end;
    if DynaModel.Exists then
    begin    // Checks for existence and Selects
        // N := UserModel.FNumVars;
        DynaModel.FGetAllVars(pDoubleArray(@States^[NumStorageVariables + 1]));
    end;
end;

function TStorageObj.NumVariables: Integer;
begin
    Result := NumStorageVariables;

     // Exists does a check and then does a Select
    if UserModel.Exists then
        Result := Result + UserModel.FNumVars;
    if DynaModel.Exists then
        Result := Result + DynaModel.FNumVars;
end;

function TStorageObj.VariableName(i: Integer): String;

const
    BuffSize = 255;
var
    n,
    i2: Integer;
    Buff: array[0..BuffSize] of AnsiChar;
    pName: pAnsichar;

begin
    if i < 1 then
        Exit;  // Someone goofed

    case i of
        1:
            Result := 'kWh';
        2:
            Result := 'State';
        3:
            Result := 'kWOut';
        4:
            Result := 'kWIn';
        5:
            Result := 'Losses';
        6:
            Result := 'Idling';
        7:
            Result := 'kWh Chng';
    else
    begin
        if UserModel.Exists then    // Checks for existence and Selects
        begin
            pName := PAnsiChar(@Buff);
            n := UserModel.FNumVars;
            i2 := i - NumStorageVariables;
            if i2 <= n then
            begin
                UserModel.FGetVarName(i2, pName, BuffSize);
                Result := String(pName);
                Exit;
            end;
        end;
        if DynaModel.Exists then   // Checks for existence and Selects
        begin
            pName := PAnsiChar(@Buff);
            n := DynaModel.FNumVars;
            i2 := i - NumStorageVariables; // Relative index
            if i2 <= n then
            begin
                DynaModel.FGetVarName(i2, pName, BuffSize);
                Result := String(pName);
                Exit;
            end;
        end;
    end;
    end;
end;

procedure TStorageObj.MakePosSequence();
var
    newkW, newPF, V: Double;
    oldPhases, changes: Integer;
begin
    // Make sure voltage is line-neutral
    if (Fnphases > 1) or (connection <> 0) then
        V := StorageVars.kVStorageBase / SQRT3
    else
        V := StorageVars.kVStorageBase;

    oldPhases := FnPhases;
    changes := 3;
    if oldPhases > 1 then
    begin
        newkW := StorageVars.kWrating / Fnphases;
        newPF := PFNominal;
        changes := changes + 2;
    end;
    SetInteger(ord(TProp.Phases), 1);
    SetInteger(ord(TProp.conn), 0);
    SetDouble(ord(TProp.kV), V);
    if oldPhases > 1 then
    begin
        SetDouble(ord(TProp.kWrated), newkW);
        SetDouble(ord(TProp.PF), newPF);
    end;
    EndEdit(changes);

    inherited;   // write out other properties
end;

procedure TStorageObj.Set_ConductorClosed(Index: Integer; Value: Boolean);
begin
    inherited;

    // Just turn storage element on or off;

    if Value then
        StorageObjSwitchOpen := FALSE
    else
        StorageObjSwitchOpen := TRUE;
end;

procedure TStorageObj.Set_pctkvarOut(const Value: Double);
begin
    FpctkvarOut := Value;
    // Force recompute of target PF and requested kVAr
    Presentkvar := StorageVars.kWRating * sqrt(1.0 / SQR(PFNominal) - 1.0) * FpctkvarOut / 100.0;
end;

procedure TStorageObj.Set_pctkWOut(const Value: Double);
begin
    FpctkWOut := Value;
    kW_Out := FpctkWOut * StorageVars.kWRating / 100.0;
end;

procedure TStorageObj.Set_PowerFactor(const Value: Double);
begin
    PFNominal := Value;
    SyncUpPowerQuantities;
end;

procedure TStorageObj.Set_PresentkV(const Value: Double);
begin
    StorageVars.kVStorageBase := Value;
    case FNphases of
        2, 3:
            VBase := StorageVars.kVStorageBase * InvSQRT3x1000;
    else
        VBase := StorageVars.kVStorageBase * 1000.0;
    end;
end;

procedure TStorageObj.Set_Presentkvar(const Value: Double);
// set the kvar to requested value within rating of inverter
var
    kVA_Gen: Double;
begin
    kvar_out := Value;
    StorageVars.kvarRequested := Value;
    // Requested kVA output
    kVA_Gen := Sqrt(Sqr(kW_out) + Sqr(kvar_out));
    with StorageVars do
        if kVA_Gen > kVArating then
            kVA_Gen := kVARating;  // Limit kVA to rated value
    if kVA_Gen <> 0.0 then
        PFNominal := abs(kW_out / kVA_Gen)
    else
        PFNominal := 1.0;
    if (kW_out * kvar_out) < 0.0 then
        PFNominal := -PFNominal;
end;

procedure TStorageObj.Set_PresentkW(const Value: Double);
begin
    FpctkWOut := Value / StorageVars.kWRating * 100.0;
    kW_Out := Value;
     //SyncUpPowerQuantities;
end;

procedure TStorageObj.Set_StorageState(const Value: Integer);
var
    SavedState: Integer;
begin
    SavedState := Fstate;

     // Decline if storage is at its limits ; set to idling instead

    with StorageVars do
        case Value of

            STORE_CHARGING:
            begin
                if kWhStored < kWhRating then
                    Fstate := Value
                else
                    Fstate := STORE_IDLING;   // all charged up
            end;

            STORE_DISCHARGING:
            begin
                if kWhStored > kWhReserve then
                    Fstate := Value
                else
                    Fstate := STORE_IDLING;  // not enough storage to discharge
            end;
        else
            Fstate := STORE_IDLING;
        end;

    if SavedState <> Fstate then
        FStateChanged := TRUE;

     //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, ---State Set To %s', [ActiveCircuit.Solution.dblHour, StateToStr ]));
end;

procedure TStorageObj.SyncUpPowerQuantities;
begin
    if kVANotSet then
        StorageVars.kVARating := StorageVars.kWrating;
    kvar_out := 0.0;
     // keep kvar nominal up to date with kW and PF
    if (PFNominal <> 0.0) then
    begin
        kvar_out := kW_out * sqrt(1.0 / Sqr(PFNominal) - 1.0);
        if PFNominal < 0.0 then
            kvar_out := -kvar_out;
    end;

     // 5-8-2017  moved this from recalcElementdata
    kvarbase := kvar_out;   // remember for follow mode; synch up here
end;

procedure TStorageObj.SetDragHandRegister(Reg: Integer; const Value: Double);
begin
    if Value > Registers[reg] then
        Registers[Reg] := Value;
end;

finalization    DispatchModeEnum.Free;
    StateEnum.Free;
end.
