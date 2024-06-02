unit WindGen;

// Copyright (c) 2024, DSS-Extensions contributors
// Copyright (c) 2024, Electric Power Research Institute, Inc.
// All rights reserved.

interface

uses
    WindGenVars,
    WindGenUserModel,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    ucomplex,
    LoadShape,
    GrowthShape,
    Spectrum,
    ArrayDef,
    DynEqPCE
    Dynamics,
    WTG3_Model,
    XYCurve;

const
    NumWGenRegisters = 6; // Number of energy meter registers

type
{$SCOPEDENUMS ON}
    TWindGenPropLegacy = (
        INVALID = 0,
        phases = 1,
        bus1 = 2,
        kv = 3,
        kW = 4,
        PF = 5,
        model = 6,
        yearly = 7,
        daily = 8,
        duty = 9,
        conn = 10,
        kvar = 11,
        cls = 12,
        debugtrace = 13,
        Vminpu = 14,
        Vmaxpu = 15,
        kVA = 16,
        MVA = 17,
        UserModel = 18,
        UserData = 19,
        DutyStart = 20,
        DynamicEq = 21,
        DynOut = 22,
        Rthev = 23,
        Xthev = 24,
        Vss = 25,
        Pss = 26,
        Qss = 27,
        vwind = 28,
        QMode = 29,
        SimMechFlg = 30,
        APCFlg = 31,
        QFlg = 32,
        delt0 = 33,
        N_WTG = 34,
        VV_Curve = 35,
        Ag = 36,
        Cp = 37,
        Lamda = 38,
        P = 39,
        pd = 40,
        PLoss = 41,
        Rad = 42,
        VCutIn = 43,
        VCutOut = 44
    );

    TWindGenProp = (
        INVALID = 0,
        Phases = 1,
        Bus1 = 2,
        kV = 3,
        kW = 4,
        PF = 5,
        Model = 6,
        Yearly = 7,
        Daily = 8,
        Duty = 9,
        Conn = 10,
        kvar = 11,
        cls = 12,
        DebugTrace = 13,
        Vminpu = 14,
        Vmaxpu = 15,
        kVA = 16,
        MVA = 17,
        UserModel = 18,
        UserData = 19,
        DutyStart = 20,
        DynamicEq = 21,
        DynOut = 22,
        Rthev = 23,
        Xthev = 24,
        Vss = 25,
        Pss = 26,
        Qss = 27,
        VWind = 28,
        QMode = 29,
        SimMechFlg = 30,
        APCFlg = 31,
        QFlg = 32,
        delt0 = 33,
        N_WTG = 34,
        VV_Curve = 35,
        Ag = 36,
        Cp = 37,
        Lamda = 38,
        P = 39,
        pd = 40,
        PLoss = 41,
        Rad = 42,
        VCutIn = 43,
        VCutOut = 44
    );

    TWindGenVariable = (
        INVALID = 0,
        userTrip = 1,
        wtgTrip = 2,
        Pcurtail = 3,
        Pcmd = 4,
        Pgen = 5,
        Qcmd = 6,
        Qgen = 7,
        Vref = 8,
        Vmag = 9,
        vwind = 10,
        WtRef = 11,
        WtAct = 12,
        dOmg = 13,
        dFrqPuTest = 14,
        QMode = 15,
        Qref = 16,
        PFref = 17,
        thetaPitch = 18,
        Pg = 19,
        Ps = 20,
        Pr = 21,
        s = 22
    );    
{$SCOPEDENUMS OFF}

    TWindGen = class(TDynEqPCEClass)
    PROTECTED
        cBuffer: TCBuffer24; // Temp buffer for calcs  24-phase WindGen?
        
        procedure DefineProperties(); override;
    PUBLIC
        RegisterNames: ArrayOfString;
        varNames: ArrayOfString;

        constructor Create;
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;

        procedure ResetRegistersAll();
        procedure SampleAll();
        function GetRegisterNames(obj: TDSSObject): ArrayOfString; override;
        function GetRegisterValues(obj: TDSSObject; var numRegisters: Integer): pDoubleArray; override;
    end;

    TWindGenObj = class(TDynEqPCE)
    PRIVATE
        Yeq: Complex; // at nominal
        Yeq95: Complex; // at 95%
        Yeq105: Complex; // at 105%

        Edp: Complex;
        PhaseCurrentLimit: Complex;
        Model7MaxPhaseCurr: Double;
        Model7LastAngle: Double;
        DeltaQMax: Double; // Max allowable var change on Model=3 per iteration

        DQDV: Double;
        DQDVSaved: Double;
        FForcedON: Boolean;
        FirstSampleAfterReset: Boolean;
        // IsFixed: Boolean; // if Fixed, always at base value
        WindGenSolutionCount: Integer;
        GenFundamental: Double; // Thevinen equivalent voltage mag and angle reference for Harmonic model
        GenON: Boolean; // Indicates whether WindGen is currently on
        GenSwitchOpen: Boolean;
        kVANotSet: Boolean;
        LastGrowthFactor: Double;
        LastYear: Integer; // added for speedup so we don't have to search for growth factor a lot
        OpenWindGenSolutionCount: Integer;
        PVFactor: Double; // deceleration Factor for computing vars for PV WindGens
        Reg_Hours: Integer;
        Reg_kvarh: Integer;
        Reg_kWh: Integer;
        Reg_MaxkVA: Integer;
        Reg_MaxkW: Integer;
        Reg_Price: Integer;
        ShapeFactor: Complex;
        TraceFile: TFileStream;
        UserModel: TWindGenUserModel; // User-Written Models
        UserModelNameStr, UserModelEditStr: String;
        V_Avg: Double;
        varBase: Double; // Base vars per phase
        varMax: Double;
        varMin: Double;
        VBase: Double; // Base volts suitable for computing currents
        VBase105: Double;
        VBase95: Double;
        Vthev: Complex; // Thevinen equivalent voltage (complex) for dynamic model
        YPrimOpenCond: TCmatrix; // To handle cases where one conductor of load is open ; We revert to admittance for inj currents
        YQFixed: Double; // Fixed value of y for type 7 load
        ShapeIsActual: Boolean;

        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double); // now incorporates DutyStart offset
        procedure CalcGenModelContribution();
        procedure CalcInjCurrentArray();
        procedure CalcVterminal();
        procedure CalcVTerminalPhase();
        procedure CalcVthev_Dyn; // 3-phase Voltage behind transient reactance
        procedure CalcVthev_Dyn_Mod7(const V: Complex);
        procedure CalcYearlyMult(Hr: Double);
        procedure CalcYPrimMatrix(Ymatrix: TcMatrix);

        procedure DoConstantPQGen();
        procedure DoConstantZGen();
        procedure DoDynamicMode();
        procedure DoFixedQGen();
        procedure DoFixedQZGen();
        procedure DoHarmonicMode();
        procedure DoPVTypeGen();
        procedure DoUserModel();

        procedure Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
        procedure SetDragHandRegister(Reg: Integer; const Value: Double);
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);

        procedure WriteTraceRecord(const s: String);

        procedure SyncUpPowerQuantities;

        procedure SetkWkvar(const PkW, Qkvar: Double);

    PROTECTED
        procedure Set_ConductorClosed(Index: Integer; Value: Boolean); OVERRIDE;
        procedure GetTerminalCurrents(Curr: pComplexArray); OVERRIDE;

    PUBLIC

        WindModelDyn: TGE_WTG3_Model;
        Connection: Integer; // 0 = line-neutral; 1=Delta
        DailyDispShapeObj: TLoadShapeObj; // Daily (24 HR) WindGen shape
        DutyShapeObj: TLoadShapeObj; // Duty cycle load shape for changes typically less than one hour
        DutyStart: Double; // starting time offset into the DutyShape [hrs] for this WindGen
        GenClass: Integer;
        GenModel: Integer; // Variation with voltage
        WindGenVars: TWindGenVars; // State Variables
        kvarBase: Double;
        kvarMax: Double;
        kvarMin: Double;
        kWBase: Double;
        PFNominal: Double;
        
        TODO: not set by props
        Vpu: Double; // per unit Target voltage for WindGen with voltage control
        
        Vmaxpu: Double;
        Vminpu: Double;
        VV_CurveObj: TXYcurveObj;
        Loss_CurveObj: TXYcurveObj;
        GenActive: Boolean;

        YearlyShapeObj: TLoadShapeObj; // Shape for this WindGen

        Registers, Derivatives: array[1..NumWGenregisters] of Double;

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;
        procedure DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;

        procedure RecalcElementData(); OVERRIDE;
        procedure CalcYPrim(); OVERRIDE;

        function InjCurrents(): Integer; OVERRIDE;
        function NumVariables(): Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        procedure SetNominalGeneration();

        procedure ResetRegisters;
        procedure TakeSample();

        // Procedures for setting the DQDV used by the Solution Object
        procedure InitDQDVCalc();
        procedure CalcDQDV();
        procedure ResetStartPoint();

        // Support for Dynamics Mode
        procedure InitStateVars(); OVERRIDE;
        procedure IntegrateStates(); OVERRIDE;

        // Support for Harmonics Mode
        procedure InitHarmonics(); OVERRIDE;

        procedure MakePosSequence(); OVERRIDE; // Make a positive Sequence Model
    end;

implementation

uses
    ParserDel,
    Circuit,
    Sysutils,
    Command,
    Math,
    MathUtil,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    Classes;

type
    TObj = TWindGenObj;
    TProp = TWindGenProp;
    TPropLegacy = TWindGenPropLegacy;
    TVar = TWindGenVariable;
const
    NumPropsThisClass = Ord(High(TProp));
    NumWGenVariables = ord(High(TVar));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;
    WindGenModelEnum: TDSSEnum;

constructor TWindGen.Create; // Creates superstructure for all objects
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
        VarInfo := TypeInfo(TVar);

        WindGenModelEnum := TDSSEnum.Create('Generator: Model', True, 0, 0, [
            'Constant PQ', 'Constant Z', 'Constant P|V|', 'Constant P, fixed Q', 
            'Constant P, fixed X', 'User model'],
            [1, 2, 3, 4, 5, 6],
            ['ConstantPQ', 'ConstantZ', 'ConstantPV', 'ConstantP_FixedQ', 'ConstantP_FixedX', 'UserModel']);
        WindGenModelEnum.JSONUseNumbers := true;
    end;

    SetLength(varNames, NumWGenVariables);
    for i := 1 to NumWGenVariables do
        varNames[i - 1] := GetEnumName(VarInfo, i);

    inherited Create(dssContext, WINDGEN_ELEMENT, 'WindGen');

    // Set Register names
    RegisterNames := ArrayOfString.Create(
        'kWh',
        'kvarh',
        'Max kW',
        'Max kVA',
        'Hours',
        '$'
    );
end;

destructor TWindGen.Destroy;
begin
    inherited Destroy;
end;

procedure ObjSetDynOutputNames(obj: TObj; variables: TStringList);
begin
    obj.SetDynOutputNames(variables);
    variables.Free();
end;

function ObjGetDynOutputNames(obj: TObj): TStringList;
begin
    Result := obj.GetDynOutputNames();
end;

procedure TWindGen.DefineProperties();
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    NumProperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    // enum properties
    PropertyType[ord(TProp.conn)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.conn)] := ptruint(@obj.Connection);
    PropertyOffset2[ord(TProp.conn)] := PtrInt(DSS.ConnectionEnum);

    // string properties
    PropertyType[ord(TProp.UserModel)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.UserModel)] := ptruint(@obj.UserModelNameStr);
    PropertyFlags[ord(TProp.UserModel)] := [TPropertyFlag.IsFilename];

    PropertyType[ord(TProp.UserData)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.UserData)] := ptruint(@obj.UserModelEditStr);

    PropertyType[ord(TProp.DynOut)] := TPropertyType.StringListProperty;
    PropertyOffset[ord(TProp.DynOut)] := 1; // dummy
    PropertyWriteFunction[ord(TProp.DynOut)] := @ObjSetDynOutputNames;
    PropertyReadFunction[ord(TProp.DynOut)] := @ObjGetDynOutputNames;
    PropertyFlags[ord(TProp.DynOut)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction];

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;
    PropertyFlags[ord(TProp.bus1)] := [TPropertyFlag.Required];

    // boolean properties
    PropertyType[ord(TProp.debugtrace)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.debugtrace)] := ptruint(@obj.WindModelDyn.DebugTrace);

    // integer properties
    PropertyType[ord(TProp.cls)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.cls)] := ptruint(@obj.GenClass);

    PropertyType[ord(TProp.model)] := TPropertyType.MappedIntEnumProperty;
    PropertyOffset[ord(TProp.model)] := ptruint(@obj.GenModel);
    PropertyOffset2[ord(TProp.model)] := PtrInt(WindGenModelEnum);
    // PropertyFlags[ord(TProp.model)] := [TPropertyFlag.Required];

    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    PropertyType[ord(TProp.QMode)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.QMode)] := ptruint(@obj.WindModelDyn.QMode);

    PropertyType[ord(TProp.SimMechFlg)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.SimMechFlg)] := ptruint(@obj.WindModelDyn.SimMechFlg);

    PropertyType[ord(TProp.APCFlg)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.APCFlg)] := ptruint(@obj.WindModelDyn.APCFLG);

    PropertyType[ord(TProp.QFlg)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.QFlg)] := ptruint(@obj.WindModelDyn.QFlg);

    PropertyType[ord(TProp.N_WTG)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.N_WTG)] := ptruint(@obj.WindModelDyn.N_WTG);


    // object properties
    PropertyType[ord(TProp.yearly)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.daily)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.duty)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.DynamicEq)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.VV_Curve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.PLoss)] := TPropertyType.DSSObjectReferenceProperty;
    
    PropertyOffset[ord(TProp.yearly)] := ptruint(@obj.YearlyShapeObj);
    PropertyOffset[ord(TProp.daily)] := ptruint(@obj.DailyDispShapeObj);
    PropertyOffset[ord(TProp.duty)] := ptruint(@obj.DutyShapeObj);
    PropertyOffset[ord(TProp.DynamicEq)] := ptruint(@obj.DynamicEqObj);
    PropertyOffset[ord(TProp.VV_Curve)] := ptruint(@obj.VV_CurveObj);
    PropertyOffset[ord(TProp.PLoss)] := ptruint(@obj.Loss_CurveObj);

    PropertyOffset2[ord(TProp.yearly)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.daily)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.duty)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.DynamicEq)] := ptruint(DSS.DynamicExpClass);
    PropertyOffset2[ord(TProp.VV_Curve)] := ptruint(DSS.XYCurveClass);
    PropertyOffset2[ord(TProp.PLoss)] := ptruint(DSS.XYCurveClass);
    

    // double properties (default type)
    PropertyOffset[ord(TProp.kW)] := ptruint(@obj.kWBase);
    PropertyOffset[ord(TProp.pf)] := ptruint(@obj.PFNominal);

    PropertyOffset[ord(TProp.Rthev)] := ptruint(@obj.WindModelDyn.Zthev.re);
    PropertyOffset[ord(TProp.Xthev)] := ptruint(@obj.WindModelDyn.Zthev.im);

    PropertyOffset[ord(TProp.Vminpu)] := ptruint(@obj.VMinPu);
    PropertyOffset[ord(TProp.Vmaxpu)] := ptruint(@obj.VMaxPu);

    PropertyOffset[ord(TProp.DutyStart)] := ptruint(@obj.DutyStart);
    PropertyFlags[ord(TProp.DutyStart)] := [TPropertyFlag.Units_hour];

    PropertyOffset[ord(TProp.kVA)] := ptruint(@obj.WindGenvars.kVArating);
    PropertyFlags[ord(TProp.kVA)] := [TPropertyFlag.DynamicDefault];

    PropertyOffset[ord(TProp.delt0)] := ptruint(@obj.WindModelDyn.delt0);
    PropertyOffset[ord(TProp.Vss)] := ptruint(@obj.WindModelDyn.Vss);
    PropertyOffset[ord(TProp.Pss)] := ptruint(@obj.WindModelDyn.Pss);
    PropertyOffset[ord(TProp.Qss)] := ptruint(@obj.WindModelDyn.Qss);
    PropertyOffset[ord(TProp.VWind)] := ptruint(@obj.WindModelDyn.vwind);
    PropertyOffset[ord(TProp.Ag)] := ptruint(@obj.WindgenVars.ag);
    PropertyOffset[ord(TProp.Cp)] := ptruint(@obj.WindgenVars.Cp);
    PropertyOffset[ord(TProp.Lamda)] := ptruint(@obj.WindgenVars.Lamda);
    PropertyOffset[ord(TProp.P)] := ptruint(@obj.WindgenVars.Poles);
    PropertyOffset[ord(TProp.pd)] := ptruint(@obj.WindgenVars.pd);
    PropertyOffset[ord(TProp.Rad)] := ptruint(@obj.WindgenVars.Rad);
    PropertyOffset[ord(TProp.VCutIn)] := ptruint(@obj.WindgenVars.VCutin);
    PropertyOffset[ord(TProp.VCutOut)] := ptruint(@obj.WindgenVars.VCutout);

    ord(TProp.kV):PresentkV := Parser.DblValue;
    ord(TProp.kvar):Presentkvar := Parser.DblValue;

    PropertyOffset[ord(TProp.kV)] := ptruint(@obj.WindGenvars.kVGeneratorBase);
    PropertyFlags[ord(TProp.kV)] := [TPropertyFlag.Required, TPropertyFlag.Units_kV, TPropertyFlag.NonNegative];

    PropertyOffset[ord(TProp.kvar)] := ptruint(@obj.kvarBase);
    PropertyFlags[ord(TProp.kvar)] := [TPropertyFlag.NoDefault, TPropertyFlag.RequiredInSpecSet];

    PropertyFlags[ord(TProp.kW)] := [TPropertyFlag.RequiredInSpecSet];
    PropertyFlags[ord(TProp.PF)] := [TPropertyFlag.RequiredInSpecSet, TPropertyFlag.PowerFactorLimits];

    // adv doubles
    PropertyOffset[ord(TProp.MVA)] := ptruint(@obj.WindGenvars.kVArating);
    PropertyScale[ord(TProp.MVA)] := 1000.0;
    PropertyFlags[ord(TProp.MVA)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.MVA)] := ord(TProp.kVA);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TWindGen.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure SetNcondsForConnection(obj: TObj);
begin
    case obj.Connection of
        0:
            obj.NConds := obj.Fnphases + 1;
        1:
            case obj.Fnphases of
                1, 2:
                    obj.NConds := obj.Fnphases + 1; // L-L and Open-delta
            else
                obj.NConds := obj.Fnphases;
            end;
    end;
end;

function TWindGen.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
var
    obj: TObj;
begin
    obj:= TObj(ptr);
    obj.RecalcElementData();
    obj.YPrimInvalid := TRUE;
    // if Flg.NeedsYprim in obj.Flags then
    // begin
    //     obj.YPrimInvalid := TRUE;
    //     Exclude(obj.Flags, Flg.NeedsYprim);
    // end;
    Exclude(obj.Flags, Flg.EditingActive);
    Result := True;
end;

procedure TWindGenObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
var
    i: Integer;
    kVA_Gen: Double;
    addedNeedsYprim: Boolean = false;
begin
    // if not (Flg.NeedsYprim in Flags) then
    // begin
    //     addedNeedsYprim := true;
    //     Include(Flags, Flg.NeedsYprim);
    // end;

    if (Idx > 0) and (Idx <= NumPropsThisClass) then
    begin
        // if addedNeedsYprim and 
        //     (TSetterFlag.AvoidFullRecalc in setterFlags) and 
        //     (TProp(idx) in [TProp.kvar, TProp.kW, TProp.model, TProp.pf, TProp.status, TProp.bus1,
        //                     TProp.Daily, TProp.Yearly, TProp.Duty, TProp.cls,
        //                     TProp.Vminpu, TProp.Vmaxpu]) then
        // begin
        //     Exclude(Flags, Flg.NeedsYprim);
        // end;
        case TProp(Idx) of
            TProp.Conn:
            begin
                SetNCondsForConnection(self);
                // VBase is always L-N voltage unless 1-phase device or more than 3 phases
                with WindGenVars do 
                    case Fnphases of
                        2, 3:
                            VBase := kVGeneratorBase * InvSQRT3x1000; // L-N Volts
                    else
                        VBase := kVGeneratorBase * 1000.0; // Just use what is supplied
                    end;

                Yorder := Fnconds * Fnterms;
                YPrimInvalid := TRUE;
            end;
            TProp.kV:
                with WindGenvars do
                    case FNphases of
                        2, 3:
                            VBase := kVGeneratorBase * InvSQRT3x1000;
                    else
                        VBase := kVGeneratorBase * 1000.0;
                    end;

            TProp.kvar:
            begin
                WindGenvars.Qnominalperphase := 1000.0 * kvarBase / Fnphases; // init to something reasonable
                kVA_Gen := Sqrt(Sqr(kWBase) + Sqr(kvarBase));
                if kVA_Gen <> 0.0 then
                    PFNominal := kWBase / kVA_Gen
                else
                    PFNominal := 1.0;
                if (kWBase * kvarBase) < 0.0 then
                    PFNominal := -PFNominal;

                kvarMax := 2.0 * kvarBase;
                kvarMin := -kvarMax;

                if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.NoPropertyTracking)) = 0 then
                begin
                    PrpSequence[ord(TProp.PF)] := 0;
                end;
            end;
            TProp.phases:
                SetNCondsForConnection(self); // Force Reallocation of terminal info

            // keep kvar nominal up to date with kW and PF
            TProp.kW, TProp.PF:
            begin
                SyncUpPowerQuantities;
                if TProp(idx) = TProp.PF then
                begin
                    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.NoPropertyTracking)) = 0 then
                    begin
                        PrpSequence[ord(TProp.kvar)] := 0;
                    end;
                end;
            end;
            TProp.UserModel:
                UserModel.Name := UserModelNameStr; // Connect to user written models
            TProp.UserData:
                if UserModel.Exists then
                    UserModel.Edit(UserModelEditStr); // Send edit string to user model

            // if a model 3 generator added, force calc of dQdV
            TProp.model:
                if GenModel = 3 then
                    ActiveCircuit.Solution.SolutionInitialized := FALSE;

            TODO: doesn`t it use Wind Speed instead of kW/kvar somewhere else?
            // Sets the kW and kvar properties to match the peak kW demand from the Loadshape
            TProp.yearly:
                if (YearlyShapeObj <> NIL) and YearlyShapeObj.UseActual then
                    SetkWkvar(YearlyShapeObj.MaxP, YearlyShapeObj.MaxQ);
            TProp.daily:
                if (DailyDispShapeObj <> NIL) and DailyDispShapeObj.UseActual then
                    SetkWkvar(DailyDispShapeObj.MaxP, DailyDispShapeObj.MaxQ);
            TProp.duty:
                if (DutyShapeObj <> NIL) and DutyShapeObj.UseActual then
                    SetkWkvar(DutyShapeObj.MaxP, DutyShapeObj.MaxQ);

            TProp.debugtrace:
                if DebugTrace then
                begin
                    FreeAndNil(TraceFile);
                    TraceFile := TBufferedFileStream.Create(DSS.OutputDirectory + 'GEN_' + Name + '.csv', fmCreate);
                    FSWrite(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, GenModel, dQdV, Avg_Vpu, Vdiff, MQnominalperphase, MPnominalperphase, CurrentType');
                    for i := 1 to fnphases do
                        FSWrite(Tracefile, ', |Iinj' + IntToStr(i) + '|');
                    for i := 1 to fnphases do
                        FSWrite(Tracefile, ', |Iterm' + IntToStr(i) + '|');
                    for i := 1 to fnphases do
                        FSWrite(Tracefile, ', |Vterm' + IntToStr(i) + '|');
                    FSWrite(TraceFile, ',Vthev, Theta');
                    FSWriteln(TraceFile);
                    FSFlush(Tracefile);
                end
                else
                begin
                    FreeAndNil(TraceFile);
                end;
                
            TProp.kVA, TProp.MVA:
            begin
                WindModelDyn.ratedKVA := WindGenVars.kVArating;
                kVANotSet := FALSE;
            end;

            TProp.DynamicEq:
                if DynamicEqObj <> NIL then
                    SetLength(DynamicEqVals, DynamicEqObj.NVariables);

            TProp.DebugTrace:
                if WindModelDyn.DebugTrace then
                begin
                    FreeAndNil(TraceFile);
                    TraceFile := TBufferedFileStream.Create(DSS.OutputDirectory + 'WINDGEN_' + Name + '.csv', fmCreate);
                    FSWrite(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, GenModel, dQdV, Avg_Vpu, Vdiff, MQnominalperphase, MPnominalperphase, CurrentType');
                    for i := 1 to nphases do
                        FSWrite(Tracefile, ', |Iinj' + IntToStr(i) + '|');
                    for i := 1 to nphases do
                        FSWrite(Tracefile, ', |Iterm' + IntToStr(i) + '|');
                    for i := 1 to nphases do
                        FSWrite(Tracefile, ', |Vterm' + IntToStr(i) + '|');
                    FSWrite(TraceFile, ',Vthev, Theta');
                    FSWriteln(TraceFile);
                    FSFlush(Tracefile);
                    // WindModelDyn.InitTraceFile(); -- done in RecalcElementData
                end
                begin
                    FreeAndNil(TraceFile);
                end;

            TProp.VV_Curve: //  the Volt-var control curve
                if VV_CurveObj <> NIL then
                begin
                    WindModelDyn.V1_VoltVar := VV_CurveObj.XValue_pt[1];
                    WindModelDyn.V2_VoltVar := VV_CurveObj.XValue_pt[2];
                    WindModelDyn.V3_VoltVar := VV_CurveObj.XValue_pt[3];
                    WindModelDyn.V4_VoltVar := VV_CurveObj.XValue_pt[4];
                    WindModelDyn.Q1_VoltVar := VV_CurveObj.YValue_pt[1];
                    WindModelDyn.Q2_VoltVar := VV_CurveObj.YValue_pt[2];
                    WindModelDyn.Q3_VoltVar := VV_CurveObj.YValue_pt[3];
                    WindModelDyn.Q4_VoltVar := VV_CurveObj.YValue_pt[4];
                    // WindModelDyn.ReCalcElementData(); -- already done in our ReCalcElementData
                end;

            TProp.PLoss:
                if Loss_CurveObj <> NIL then
                    WindgenVars.PLoss := Loss_CurveObj.Name; TODO: PLoss shouldn`t be in WindgenVars, especially since it doesn`t need to exist
        end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;


procedure Set_PresentkV(const Value: Double);
procedure Set_Presentkvar(const Value: Double);
procedure Set_PresentkW(const Value: Double);
procedure Set_PowerFactor(const Value: Double);
property PresentkW: Double WRITE Set_PresentkW;
property Presentkvar: Double WRITE Set_Presentkvar;
property PresentkV: Double WRITE Set_PresentkV;
property PowerFactor: Double READ PFNominal WRITE Set_PowerFactor;

procedure TWindGenObj.Set_PowerFactor(const Value: Double);
begin
    PFNominal := Value;
    SyncUpPowerQuantities();
end;

procedure TWindGenObj.Set_PresentkV(const Value: Double);
begin
    with WindGenvars do
    begin
        kVWindGenBase := Value;
        case FNphases of
            2, 3:
                VBase := kVWindGenBase * InvSQRT3x1000;
        else
            VBase := kVWindGenBase * 1000.0;
        end;
    end;
end;

procedure TWindGenObj.Set_Presentkvar(const Value: Double);
var
    kVA_Gen: Double;
begin
    kvarBase := Value;
    WindGenvars.Qnominalperphase := 1000.0 * kvarBase / Fnphases; // init to something reasonable
    kVA_Gen := Sqrt(Sqr(kWBase) + Sqr(kvarBase));
    if kVA_Gen <> 0.0 then
        PFNominal := kWBase / kVA_Gen
    else
        PFNominal := 1.0;
    if (kWBase * kvarBase) < 0.0 then
        PFNominal := -PFNominal;

    kvarMax := 2.0 * kvarBase;
    kvarMin := -kvarMax;
end;

procedure TWindGenObj.Set_PresentkW(const Value: Double);
begin
    kWBase := Value;
    SyncUpPowerQuantities;
end;


procedure TWindGenObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);

    Other := TObj(OtherPtr);
    if (Fnphases <> Other.Fnphases) then
    begin
        Nphases := Other.Fnphases;
        NConds := Fnphases; // Forces reallocation of terminal stuff

        Yorder := Fnconds * Fnterms;
        YprimInvalid := true;
    end;

    WindGenVars.kVWindGenBase := Other.WindGenVars.kVWindGenBase;
    Vbase := Other.Vbase;
    Vminpu := Other.Vminpu;
    Vmaxpu := Other.Vmaxpu;
    Vbase95 := Other.Vbase95;
    Vbase105 := Other.Vbase105;
    kWBase := Other.kWBase;
    kvarBase := Other.kvarBase;
    WindGenVars.Pnominalperphase := Other.WindGenVars.Pnominalperphase;
    PFNominal := Other.PFNominal;
    WindGenVars.Qnominalperphase := Other.WindGenVars.Qnominalperphase;
    varMin := Other.varMin;
    varMax := Other.varMax;
    Connection := Other.Connection;
    YearlyShape := Other.YearlyShape;
    YearlyShapeObj := Other.YearlyShapeObj;
    DailyDispShape := Other.DailyDispShape;
    DailyDispShapeObj := Other.DailyDispShapeObj;
    DutyShape := Other.DutyShape;
    DutyShapeObj := Other.DutyShapeObj;
    DutyStart := Other.DutyStart;
    GenClass := Other.GenClass;
    GenModel := Other.GenModel;
    // IsFixed := Other.IsFixed;
    WindGenVars.VTarget := Other.WindGenvars.VTarget;
    Vpu := Other.Vpu;
    kvarMax := Other.kvarMax;
    kvarMin := Other.kvarMin;
    FForcedON := Other.FForcedON;
    kVANotSet := Other.kVANotSet;

    WindGenVars.kVArating := Other.WindGenVars.kVArating;
    WindGenVars.puXd := Other.WindGenVars.puXd;
    WindGenVars.puXdp := Other.WindGenVars.puXdp;
    WindGenVars.puXdpp := Other.WindGenVars.puXdpp;
    WindGenVars.Hmass := Other.WindGenVars.Hmass;
    WindGenVars.Theta := Other.WindGenVars.Theta;
    WindGenVars.Speed := Other.WindGenVars.Speed;
    WindGenVars.w0 := Other.WindGenVars.w0;
    WindGenVars.dSpeed := Other.WindGenVars.dSpeed;
    WindGenVars.D := Other.WindGenVars.D;
    WindGenVars.Dpu := Other.WindGenVars.Dpu;
    WindGenVars.XRdp := Other.WindGenVars.Xrdp;

    UserModel.Name := Other.UserModel.Name; // Connect to user written models
end;

procedure TWindGen.ResetRegistersAll(); // Force all EnergyMeters in the circuit to reset
var
    pGen: TObj;
begin
    for pGen in ActiveCircuit.WindGens do
    begin
        pGen.ResetRegisters();
    end;
end;

procedure TWindGen.SampleAll(); // Force all EnergyMeters in the circuit to take a sample
var
    pGen: TObj;
begin
    for pGen in ActiveCircuit.WindGens do
    begin
        if pGen.enabled then
            pGen.TakeSample();
    end;
end;

constructor TWindGenObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; // + WINDGEN_ELEMENT; // In both PCelement and Genelement list

    Nphases := 3;
    Fnconds := 4; // defaults to wye
    Yorder := 0; // To trigger an initial allocation
    Nterms := 1; // forces allocations
    kWBase := 1000.0;
    kvarBase := 60.0;

    kvarMax := kvarBase * 2.0;
    kvarMin := -kvarmax;
    PFNominal := 0.88;
    YearlyShapeObj := nil; // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
    DailyDispShapeObj := nil; // if DaillyShapeobj = nil then the load alway stays nominal * global multipliers
    DutyShapeObj := nil; // if DutyShapeobj = nil then the load alway stays nominal * global multipliers
    DutyStart := 0.0;
    Connection := 0; // Wye (star)
    GenModel := 1; // Typical fixed kW negative load
    GenClass := 1;
    LastYear := 0;
    LastGrowthFactor := 1.0;

    DQDVSaved := 0.0; // Initialize this here.  Allows WindGens to be turned off and on

    WindGenSolutionCount := -1; // For keep track of the present solution in Injcurrent calcs
    OpenWindGenSolutionCount := -1;
    YPrimOpenCond := nil;

    WindGenVars.kVWindGenBase := 12.47;
    Vpu := 1.0;
    WindGenVars.VTarget := 1000.0 * Vpu * WindGenVars.kVWindGenBase / SQRT3; // Line-to-Neutral target
    VBase := 7200.0;
    Vminpu := 0.90;
    Vmaxpu := 1.10;
    VBase95 := Vminpu * Vbase;
    VBase105 := Vmaxpu * Vbase;
    Yorder := Fnterms * Fnconds;
    // IsFixed := false;

    // Machine rating stuff
    WindGenVars.kVArating := kWBase * 1.2;
    kVANotSet := true; // Flag for default value for kVA

    with WindGenVars do
    begin
        // These are inherited from the generator object, it is uncertain if needed
        puXd := 1.0;
        puXdp := 0.28;
        puXdpp := 0.20;
        Xd := puXd * SQR(kVWindGenBase) * 1000.0 / kVARating;
        Xdp := puXdp * SQR(kVWindGenBase) * 1000.0 / kVARating;
        Xdpp := puXdpp * SQR(kVWindGenBase) * 1000.0 / kVARating;
        Hmass := 1.0; //  W-sec/VA rating
        Theta := 0.0;
        w0 := TwoPi * Basefrequency;
        Speed := 0.0;
        dSpeed := 0.0;
        D := 1.0;
        XRdp := 20.0;
        // Added for the wind generator specifically
        PLoss := '';
        ag := 1 / 90;
        Cp := 0.41;
        Lamda := 7.95;
        Poles := 2;
        pd := 1.225;
        Rad := 40;
        VCutin := 5;
        VCutout := 23;
        Pm := 0;
        Ps := 0;
        Pr := 0;
        Pg := 0;
        s := 0;
    end;

    // Advertise WindGenvars struct as public

    PublicDataStruct := pointer(@WindGenVars);
    PublicDataSize := SizeOf(TWindGenVars);

    UserModel := TWindGenUserModel.Create(@WindGenVars);

    // Register values inherited from Generator model
    Reg_kWh := 1;
    Reg_kvarh := 2;
    Reg_MaxkW := 3;
    Reg_MaxkVA := 4;
    Reg_Hours := 5;
    Reg_Price := 6;

    PVFactor := 0.1;
    DebugTrace := false;
    FForcedON := false;
    GenSwitchOpen := false;
    ShapeIsActual := false;

    SpectrumObj := DSS.SpectrumClass.DefaultGen; // override base class

    GenActive := true; // variable to use if needed

    // Creates the Dynamic model for the Wind Turbine
    WindModelDyn := TGE_WTG3_Model.Initialize(DSS);
    WindModelDyn.vwind := 12;
    WindModelDyn.QMode := 0;

    RecalcElementData();
end;

destructor TWindGenObj.Destroy;
begin
    YPrimOpenCond.Free;
    UserModel.Free;
    inherited Destroy;
end;

procedure TWindGenObj.CalcDailyMult(Hr: Double);
begin
    if (DailyDispShapeObj <> nil) then
    begin
        ShapeFactor := DailyDispShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := DailyDispShapeObj.UseActual;
    end
    else
        ShapeFactor := cmplx(WindModelDyn.vwind, 0); // Default to no daily variation
end;

procedure TWindGenObj.CalcDutyMult(Hr: Double);
begin
    if DutyShapeObj <> nil then
    begin
        ShapeFactor := DutyShapeObj.GetMultAtHour(Hr + DutyStart);
        ShapeIsActual := DutyShapeObj.UseActual;
    end
    else
        CalcDailyMult(Hr); // Default to Daily Mult if no duty curve specified
end;

procedure TWindGenObj.CalcYearlyMult(Hr: Double);
begin
    // Yearly curve is assumed to be hourly only
    if YearlyShapeObj <> nil then
    begin
        ShapeFactor := YearlyShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := YearlyShapeObj.UseActual;
    end
    else
        TODO: check this shapefactor
        ShapeFactor := cmplx(WindModelDyn.vwind, 0); // Defaults to no variation
end;

procedure TWindGenObj.SetNominalGeneration();
var
    myV: complex;
    VMag,
    VMagTmp,
    LeadLag,
    kVATmp,
    kvarCalc,
    myLosses,
    Factor: Double;
    GenOn_Saved: Boolean;
    i: Integer;
    mode: TSolveMode;
    dblHour: Double;
begin
    mode := ActiveCircuit.Solution.Mode;
    dblHour := ActiveCircuit.Solution.DynaVars.dblHour;

    VMag := 0.0;
    VMagTmp := 0.0;
    myV := CZero;
    GenOn_Saved := GenON;
    ShapeFactor := cmplx(WindModelDyn.vwind, 0);
    
    // Check to make sure the generation is ON
    kvarCalc := 0.0;
    GenON := true; // The first assumption is that the generator is ON

    case Mode of
        SNAPSHOT:
            Factor := GenMultiplier;
        DAILYMODE:
        begin
            Factor := GenMultiplier;
            CalcDailyMult(dblHour) // Daily dispatch curve
        end;
        YEARLYMODE:
        begin
            Factor := GenMultiplier;
            CalcYearlyMult(dblHour);
        end;
        DUTYCYCLE:
        begin
            Factor := GenMultiplier;
            CalcDutyMult(dblHour);
        end;
        GENERALTIME,   // General sequential time simulation
        DYNAMICMODE:
        begin
            Factor := GenMultiplier;
            // This mode allows use of one class of load shape
            case ActiveLoadShapeClass of
                USEDAILY:
                    CalcDailyMult(dblHour);
                USEYEARLY:
                    CalcYearlyMult(dblHour);
                USEDUTY:
                    CalcDutyMult(dblHour);
            else
                ShapeFactor := cmplx(WindModelDyn.vwind, 0); // default to the wind speed set by default
            end;
        end;
        MONTECARLO1,
        MONTEFAULT,
        FAULTSTUDY:
            Factor := GenMultiplier * 1.0;
        MONTECARLO2,
        MONTECARLO3,
        LOADDURATION1,
        LOADDURATION2:
        begin
            Factor := GenMultiplier;
            CalcDailyMult(dblHour);
        end;
        PEAKDAY:
        begin
            Factor := GenMultiplier;
            CalcDailyMult(dblHour);
        end;
        AUTOADDFLAG:
            Factor := 1.0;
    else
        Factor := GenMultiplier;
    end;

    WindModelDyn.vwind := ShapeFactor.re;
    if (ShapeFactor.re > WindgenVars.VCutout) or (ShapeFactor.re < WindgenVars.VCutin) then
    begin
        WindGenvars.Pnominalperphase := 0.001 * kWBase;
        WindGenvars.Qnominalperphase := 0.0;
        WindGenvars.Pm := 0.0;
        WindGenvars.Pg := 0.0;
        WindGenvars.Ps := 0.0;
        WindGenvars.Pr := 0.0;
        WindGenvars.s := 0.0;
    end
    else
    begin
        if not (ActiveCircuit.Solution.IsDynamicModel or ActiveCircuit.Solution.IsHarmonicModel) then
        begin
            // start by getting the losses from the provided curve (if any)
            if Assigned(Loss_CurveObj) then
                myLosses := Loss_CurveObj.GetYValue(WindModelDyn.vwind)
            else
                myLosses := 0.0; // no losses given that the curve was not provided

            LeadLag := 1;
            with WindgenVars do
            begin
                Pm := 0.5 * pd * PI * math.Power(Rad, 2) * math.Power(Shapefactor.re, 3) * Cp;
                myLosses := Pm * myLosses / 100;
                Pg := (Pm - myLosses) / 1e3; // in kW
                if Pg > kWBase then
                    Pg := kWBase; // Generation limits
                s := 1 - ((Poles * Shapefactor.re * Lamda) / (w0 * ag * Rad));
                Ps := Pg / (1 - s);
                Pr := Ps * s;

                Pnominalperphase := (1e3 * Factor * Pg) / Fnphases;
                // Now check for Q depending on QMode
                case WindModelDyn.QMode of
                    1: // PF
                    begin
                        kvarCalc := math.Power(Pg / Abs(PFNominal), 2) - math.Power(Pg, 2);
                        kvarCalc := sqrt(kvarCalc);
                        kVATmp := sqrt(math.Power(Pg, 2) + math.Power(kvarCalc, 2));

                        if kVATmp > KVARating then        // Check saturation
                            kvarCalc := kvarBase;

                        if PFNominal < 0 then
                            LeadLag := -1.0;
                    end;
                    2: // Volt-var ctrl
                    begin
                        if NodeRef <> NIL then
                        begin
                            // get the highest voltage done locally given with whatever is on memory
                            for i := 1 to NumPhases do
                            begin
                                myV := NodeV[NodeRef[i]];
                                VMagTmp := ctopolar(myV).mag;
                                if VMagTmp > VMag then
                                    VMag := VmagTmp;
                            end;
                            Vmag := Vmag / VBase; // in pu

                            // start by getting the losses from the provided curve (if any)
                            if VV_CurveObj <> NIL then
                                VmagTmp := VV_CurveObj.GetYValue(Vmag)
                            else
                                VmagTmp := 0.0; // no losses given that the curve was not provided
                        end
                        else
                            VmagTmp := 0.0;

                        // Calculates Q based on the
                        kvarCalc := kvarBase * VmagTmp;
                        if Abs(kvarCalc) > kvarBase then
                        begin
                            kvarCalc := kvarBase;
                            if VmagTmp < 0 then
                                LeadLag := -1.0;
                        end;
                    end
                else
                    kvarCalc := 0;
                end;

                Qnominalperphase := 1e3 * kvarCalc * LeadLag * Factor / Fnphases;
            end;
        end;
    end;

    if not (ActiveCircuit.Solution.IsDynamicModel or ActiveCircuit.Solution.IsHarmonicModel) then
    begin
        case GenModel of
            6:
                Yeq := Cinv(cmplx(0.0, -WindGenvars.Xd)); // Gets negated in CalcYPrim
        else
            with WindGenvars do
                Yeq := Cmplx(Pnominalperphase, -Qnominalperphase) / Sqr(Vbase); // Vbase must be L-N for 3-phase
            if (Vminpu <> 0.0) then
                Yeq95 := Yeq / sqr(Vminpu)  // at 95% voltage
            else
                Yeq95 := Yeq; // Always a constant Z model

            if (Vmaxpu <> 0.0) then
                Yeq105 := Yeq / Sqr(Vmaxpu)   // at 105% voltage
            else
                Yeq105 := Yeq;
        end;
    end;

    // If WindGen state changes, force re-calc of Y matrix
    if GenON <> GenON_Saved then
        YprimInvalid := true;
end;

procedure TWindGenObj.RecalcElementData();
begin
    VBase95 := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    varBase := 1000.0 * kvarBase / Fnphases;
    varMin := 1000.0 * kvarMin / Fnphases;
    varMax := 1000.0 * kvarMax / Fnphases;

    // Populate data structures used for interchange with user-written models.
    with WindGenvars do
    begin
        Xd := puXd * 1000.0 * SQR(kVWindGenBase) / kVARating;
        Xdp := puXdp * 1000.0 * SQR(kVWindGenBase) / kVArating;
        Xdpp := puXdpp * 1000.0 * SQR(kVWindGenBase) / kVArating;
        Conn := connection;
        NumPhases := Fnphases;
        NumConductors := Fnconds;

        if not (kVANotSet) then
        begin
            kWBase := (kVArating * Abs(PFNominal));
            kvarbase := sqrt(sqr(kVArating) - sqr(kWBase));
        end
        else
        begin
            kVArating := kWBase / Abs(PFNominal);
            WindModelDyn.ratedKVA := kVArating;
        end;
    end;

    SetNominalGeneration();

    YQFixed := -varBase / Sqr(VBase); //10-17-02  Fixed negative sign
    WindGenvars.Vtarget := Vpu * 1000.0 * WindGenvars.kVWindGenBase;

    if Fnphases > 1 then
        WindGenvars.VTarget := WindGenvars.VTarget / SQRT3;

    // Initialize to Zero - defaults to PQ WindGen
    // Solution object will reset after circuit modifications
    DQDV := DQDVSaved; // for Model = 3
    DeltaQMax := (varMax - varMin) * 0.10; // Limit to 10% of range

    Reallocmem(InjCurrent, SizeOf(Complex) * Yorder);

    // Update any user-written models
    if Usermodel.Exists then
        UserModel.FUpdateModel();

    WindModelDyn.ReCalcElementData();
end;

procedure TWindGenObj.CalcYPrimMatrix(Ymatrix: TcMatrix);
var
    Y, Yij: Complex;
    i, j: Integer;
    FreqMultiplier: Double;
    WTGZLV: Double;
begin
    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

    if ActiveCircuit.Solution.IsDynamicModel or ActiveCircuit.Solution.IsHarmonicModel then
    begin
        if GenON then
        begin
            with WindModelDyn do
            begin
                WTGZLV := sqr(WindGenvars.kVWindGenBase) * 1e3 / WindGenVars.kVArating;
                Y := Cmplx(EPSILON, -N_WTG / (WindModelDyn.Zthev.im * WTGZLV)) //Yeq  // L-N value computed in initial condition routines
            end;
        end
        else
            Y := EPSILON;

        if Connection = 1 then
            Y := Y / 3.0; // Convert to delta impedance
        Y.im := Y.im / FreqMultiplier;
        Yij := -Y;
        for i := 1 to Fnphases do
        begin
            case Connection of
                0:
                begin
                    Ymatrix[i, i] := Y;
                    Ymatrix.AddElement(Fnconds, Fnconds, Y);
                    Ymatrix[i, Fnconds] := Yij;
                    Ymatrix[Fnconds, i] := Yij;
                end;
                1:
                begin   // Delta connection}
                    Ymatrix[i, i] := Y;
                    Ymatrix.AddElement(i, i, Y); // put it in again
                    for j := 1 to i - 1 do
                    begin
                        Ymatrix[i, j] := Yij;
                        Ymatrix[j, i] := Yij;
                    end;
                end;
            end;
        end;
    end
    else
    begin  //  Regular power flow WindGen model
        // Yeq is always expected as the equivalent line-neutral admittance
        Y := -Yeq; // negate for generation    Yeq is L-N quantity
        // ****** Need to modify the base admittance for real harmonics calcs
        Y.im := Y.im / FreqMultiplier;

        case Connection of
            0:
                begin // WYE
                    Yij := -Y;
                    for i := 1 to Fnphases do
                    begin
                        YMatrix[i, i] := Y;
                        YMatrix.AddElement(Fnconds, Fnconds, Y);
                        YMatrix[i, Fnconds] := Yij;
                        YMatrix[Fnconds, i] := Yij;
                    end;
                end;
            1:
                begin  // Delta  or L-L
                    Y := Y / 3.0; // Convert to delta impedance
                    Yij := -Y;
                    for i := 1 to Fnphases do
                    begin
                        j := i + 1;
                        if j > Fnconds then
                            j := 1; // wrap around for closed connections
                        YMatrix.AddElement(i, i, Y);
                        YMatrix.AddElement(j, j, Y);
                        YMatrix.AddElemSym(i, j, Yij);
                    end;
                end;
        end;
    end;
end;

procedure TWindGenObj.CalcYPrim();
var
    i: Integer;
begin
     // Build only shunt Yprim
     // Build a dummy Yprim Series so that CalcV does not fail
    if YprimInvalid then
    begin
        if YPrim_Shunt <> nil then
            YPrim_Shunt.Free;
        YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
        if YPrim_Series <> nil then
            Yprim_Series.Free;
        YPrim_Series := TcMatrix.CreateMatrix(Yorder);
        if YPrim <> nil then
            YPrim.Free;
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin
        YPrim_Shunt.Clear;
        YPrim_Series.Clear;
        YPrim.Clear;
    end;

    if ActiveCircuit.Solution.LoadModel = POWERFLOW then
    begin
        // 12-7-99 we'll start with Yeq in system matrix
        SetNominalGeneration();
        CalcYPrimMatrix(YPrim_Shunt);

    end
    else
    begin
         // ADMITTANCE model wanted
        SetNominalGeneration();
        CalcYPrimMatrix(YPrim_Shunt);
    end;

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));

    YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
    inherited CalcYPrim();
end;

procedure TWindGenObj.StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);
// Add the current into the proper location according to connection
// 
// Reverse of similar routine in load  (Cnegates are switched)
var
    j: Integer;
begin
    case Connection of
        0:
        begin  //Wye
            TermArray[i] += Curr;
            TermArray[Fnconds] -= Curr; // Neutral
        end;
        1:
        begin //DELTA
            TermArray[i] += Curr;
            j := i + 1;
            if j > Fnconds then
                j := 1;
            TermArray[j] -= Curr;
        end;
    end;
end;

procedure TWindGenObj.WriteTraceRecord(const s: String);
var
    i: Integer;
begin
    if DSS.InShowResults then
        Exit;

    try
        TODO
        WriteStr(sout, Format('%-.g, %d, %-.g, ',
            [ActiveCircuit.Solution.DynaVars.t + ActiveCircuit.Solution.Dynavars.IntHour * 3600.0,
            ActiveCircuit.Solution.Iteration,
            ActiveCircuit.LoadMultiplier]),
            DSS.SolveModeEnum.OrdinalToString(ord(DSS.ActiveCircuit.Solution.mode)), ', ',
            DSS.DefaultLoadModelEnum.OrdinalToString(DSS.ActiveCircuit.Solution.LoadModel), ', ',
            GenModel: 0, ', ',
            DQDV: 10: 4, ', ',
            (V_Avg * 0.001732 / WindGenvars.kVWindGenBase): 10: 5, ', ',
            (GenVars.Vtarget - V_Avg): 9: 1, ', ',
            (Genvars.Qnominalperphase * 3.0 / 1.0e6): 8: 3, ', ',
            (Genvars.Pnominalperphase * 3.0 / 1.0e6): 8: 3, ', ',
            s, ', '
        );
        FSWrite(TraceFile, sout);
        for i := 1 to fnphases do
        begin
            WriteStr(sout, (Cabs(InjCurrent[i])): 8: 1, ', ');
            FSWrite(TraceFile, sout);
        end;
        for i := 1 to fnphases do
        begin
            WriteStr(sout, (Cabs(ITerminal[i])): 8: 1, ', ');
            FSWrite(TraceFile, sout);
        end;
        for i := 1 to fnphases do
        begin
            WriteStr(sout, (Cabs(Vterminal[i])): 8: 1, ', ');
            FSWrite(TraceFile, sout);
        end;
        WriteStr(sout, GenVars.VThevMag: 8: 1, ', ', Genvars.Theta * 180.0 / PI);
        FSWrite(TraceFile, sout);
        FSWriteln(Tracefile);
        FSFlush(TraceFile);
    except
        On E: Exception do
        begin
        end;
    end;
end;

procedure TWindGenObj.DoConstantPQGen();
// Compute total terminal current for Constant PQ
var
    i: Integer;
    Curr,
    V: Complex;
    Vmag: Double;
begin
     //Treat this just like the Load model
    
    TODO: check call to CalcYPrimContribution followed by =zero
    CalcYPrimContribution(InjCurrent); // Init InjCurrent Array
    for i := 1 to FnConds do
        InjCurrent[i] := 0;

    ZeroITerminal();

    CalcVTerminalPhase(); // get actual voltage across each phase of the load

    for i := 1 to Fnphases do
    begin
        V := Vterminal[i];
        VMag := Cabs(V);

        case Connection of
            0:
            begin  //Wye
                if VMag <= VBase95 then
                    Curr := Yeq95 * V  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Yeq105 * V  // above 105% use an impedance model
                else
                    with WindGenvars do
                        Curr := cong(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V)); // Between 95% -105%, constant PQ
            end;
            1:
            begin  //Delta
                case Fnphases of
                    2, 3:
                        VMag := VMag / SQRT3; // L-N magnitude
                else
                    //leave Vmag as is
                end;

                if VMag <= VBase95 then
                    Curr := (Yeq95 / 3.0) * V  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := (Yeq105 / 3.0) * V  // above 105% use an impedance model
                else
                    with WindGenvars do
                        Curr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / V); // Between 95% -105%, constant PQ
            end;
        end;

        StickCurrInTerminalArray(ITerminal, -Curr, i); // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i); // Put into Terminal array taking into account connection
    end;
end;

procedure TWindGenObj.DoConstantZGen();
var
    i: Integer;
    Curr,
    Yeq2: Complex;
begin
// Assume Yeq is kept up to date
    CalcYPrimContribution(InjCurrent); // Init InjCurrent Array
    CalcVTerminalPhase(); // get actual voltage across each phase of the load
    ZeroITerminal();
    if Connection = 0 then
        Yeq2 := Yeq
    else
        Yeq2 := Yeq / 3.0;

    for i := 1 to Fnphases do
    begin
        Curr := Yeq2 * Vterminal[i]; // Yeq is always line to neutral

        StickCurrInTerminalArray(ITerminal, -Curr, i); // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i); // Put into Terminal array taking into account connection
    end;
end;

procedure TWindGenObj.DoPVTypeGen();
// Compute total terminal current for Constant P,|V|
var
    i: Integer;
    DQ: Double;
    Curr: Complex;
begin
    CalcYPrimContribution(InjCurrent); // Init InjCurrent Array
    CalcVTerminalPhase(); // get actual voltage across each phase of the WindGen
    ZeroITerminal();

    // Guess at a new var output value
    V_Avg := 0.0;
    for i := 1 to Fnphases do
        V_Avg := V_Avg + Cabs(Vterminal[i]);

    if Connection = 1 then
        V_Avg := V_Avg / (SQRT3 * Fnphases)
    else
        V_Avg := V_Avg / Fnphases;

    DQ := PVFactor * DQDV * (WindGenvars.Vtarget - V_Avg); // Vtarget is L-N
    if (Abs(DQ) > DeltaQMax) then
        if (DQ < 0.0) then
            DQ := -DeltaQMax
        else
            DQ := DeltaQMax;
    with WindGenvars do
        Qnominalperphase := Qnominalperphase + DQ;

    // Test Limits
    with WindGenvars do
    begin
        if (Qnominalperphase > varMax) then
            Qnominalperphase := varMax
        else
        if (Qnominalperphase < varMin) then
            Qnominalperphase := varMin;

        // Compute injection currents using W and var values
        // Do not use comstant Z models outside normal range
        // Presumably the var source will take care of the voltage problems
        for i := 1 to Fnphases do
        begin
            Curr := cong(Cmplx(Pnominalperphase, Qnominalperphase) / Vterminal[i]);

            StickCurrInTerminalArray(ITerminal, -Curr, i); // Put into Terminal array taking into account connection
            ITerminalUpdated := true;
            StickCurrInTerminalArray(InjCurrent, Curr, i); // Put into Terminal array taking into account connection
        end;
    end;
end;

procedure TWindGenObj.DoFixedQGen();
// Compute total terminal current for Fixed Q
// Constant P, Fixed Q  Q is always kvarBase
var
    i: Integer;
    Curr,
    V: Complex;
    Vmag: Double;
begin
    CalcYPrimContribution(InjCurrent); // Init InjCurrent Array
    CalcVTerminalPhase(); // get actual voltage across each phase of the load
    ZeroITerminal();

    for i := 1 to Fnphases do
    begin
        V := Vterminal[i];
        VMag := Cabs(V);

        case Connection of
            0:
            begin
                if VMag <= VBase95 then
                    Curr := Cmplx(Yeq95.re, YQfixed) * V  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmplx(Yeq105.re, YQfixed) * V  // above 105% use an impedance model
                else
                    Curr := cong(Cmplx(WindGenvars.Pnominalperphase, varBase) / V);
            end;
            1:
            begin
                case Fnphases of
                    2, 3:
                        VMag := VMag / SQRT3; // L-N magnitude
                else
                    // leave Vmag as is
                end;
                if VMag <= VBase95 then
                    Curr := Cmplx(Yeq95.re / 3.0, YQfixed / 3.0) * V  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmplx(Yeq105.re / 3.0, YQfixed / 3.0) * V  // above 105% use an impedance model
                else
                    Curr := cong(Cmplx(WindGenvars.Pnominalperphase, varBase) / V);
            end;
        end;

        StickCurrInTerminalArray(ITerminal, -Curr, i); // Put into Terminal array taking into account connection
        set_ITerminalUpdated(true);
        StickCurrInTerminalArray(InjCurrent, Curr, i); // Put into Terminal array taking into account connection
    end;
end;

procedure TWindGenObj.DoFixedQZGen();
// Compute total terminal current for
// Constant P, Fixed Q  Q is always a fixed Z derived from kvarBase
var
    i: Integer;
    Curr,
    V: Complex;
    Vmag: Double;
begin
    CalcYPrimContribution(InjCurrent); // Init InjCurrent Array
    CalcVTerminalPhase(); // get actual voltage across each phase of the load
    ZeroITerminal();

    for i := 1 to Fnphases do
    begin
        V := Vterminal[i];
        Vmag := Cabs(V);

        case Connection of
            0:
            begin
                if Vmag <= VBase95 then
                    Curr := Cmplx(Yeq95.re, YQfixed) * V  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmplx(Yeq105.re, YQfixed) * V
                else
                begin
                    Curr := cong(WindGenvars.Pnominalperphase / V); // P component of current
                    Curr += Cmplx(0.0, YQFixed) * V; // add in Q component of current
                end;
            end;
            1:
            begin
                case Fnphases of
                    2, 3:
                        VMag := VMag / SQRT3; // L-N magnitude
                else
                    // leave Vmag as is
                end;
                if Vmag <= VBase95 then
                    Curr := Cmplx(Yeq95.re / 3.0, YQfixed / 3.0) * V  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmplx(Yeq105.re / 3.0, YQfixed / 3.0) * V
                else
                begin
                    Curr := cong(WindGenvars.Pnominalperphase / V); // P component of current
                    Curr += Cmplx(0.0, YQFixed / 3.0) * V; // add in Q component of current
                end;
            end;
        end;


        StickCurrInTerminalArray(ITerminal, -Curr, i); // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i); // Put into Terminal array taking into account connection
    end;
end;

procedure TWindGenObj.DoUserModel();
// Compute total terminal Current from User-written model
var
    i: Integer;
begin
    CalcYPrimContribution(InjCurrent); // Init InjCurrent Array

    if UserModel.Exists then    // Check automatically selects the usermodel if true
    begin
         //AppendToEventLog('Wnominal=', Format('%-.5g',[Pnominalperphase]));
        UserModel.FCalc(Vterminal, Iterminal);
        IterminalUpdated := TRUE;
        with ActiveCircuit.Solution do
        begin          // Negate currents from user model for power flow WindGen model
            for i := 1 to FnConds do
                InjCurrent[i] -= Iterminal[i];
        end;
    end
    else
    begin
        DoSimpleMsg('%s model designated to use user-written model, but user-written model is not defined.', [FullName], 567);
    end;
end;

procedure TWindGenObj.DoDynamicMode();
// Compute Total Current and add into InjTemp
var
    i: Integer;
    V012,
    I012: array[0..2] of Complex;
begin
    //CalcYPrimContribution(InjCurrent); // Init InjCurrent Array  and computes VTerminal L-N
    ComputeVTerminal();
    for i := 1 to FnConds do
        InjCurrent[i] := 0;

    // Inj = -Itotal (in) - Yprim*Vtemp

    case GenModel of
        6:
            if UserModel.Exists then       // auto selects model
            begin // We have total currents in Iterminal
                UserModel.FCalc(Vterminal, Iterminal); // returns terminal currents in Iterminal
            end
            else
            begin
                DoSimpleMsg(Format('Dynamics model missing for %s ', [FullName]), 5671);
                DSS.SolutionAbort := true;
            end;
    else
        WindModelDyn.CalcDynamic(Vterminal, Iterminal);
    end;

    IterminalUpdated := TRUE;

    // Add it into inj current array
    for i := 1 to FnConds do
        InjCurrent[i] -= Iterminal[i];
end;

procedure TWindGenObj.DoHarmonicMode();
// Compute Injection Current Only when in harmonics mode
// 
// Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built
// Vd is the fundamental frequency voltage behind Xd" for phase 1
var
    i: Integer;
    E: Complex;
    GenHarmonic: Double;
    pBuffer: PCBuffer24;
begin
    pBuffer := @TWindGen(ParentClass).cBuffer;
    ComputeVterminal();

    GenHarmonic := ActiveCircuit.Solution.Frequency / GenFundamental;
    E := SpectrumObj.GetMult(GenHarmonic) * WindGenvars.VThevHarm; // Get base harmonic magnitude
    RotatePhasorRad(E, GenHarmonic, WindGenvars.ThetaHarm); // Time shift by fundamental frequency phase shift
    for i := 1 to Fnphases do
    begin
        pBuffer[i] := E;
        if i < Fnphases then
            RotatePhasorDeg(E, GenHarmonic, -120.0); // Assume 3-phase WindGen
    end;

    // Handle Wye Connection
    if Connection = 0 then
        pBuffer[Fnconds] := Vterminal[Fnconds]; // assume no neutral injection voltage

    // Inj currents = Yprim (E)
    YPrim.MVMult(InjCurrent, pComplexArray(pBuffer));
end;

procedure TWindGenObj.CalcVTerminalPhase();
var
    i, j: Integer;
begin
    // Establish phase voltages and stick in Vterminal
    case Connection of
        0:
        begin
            for i := 1 to Fnphases do
                Vterminal[i] := ActiveCircuit.Solution.VDiff(NodeRef[i], NodeRef[Fnconds]);
        end;
        1:
        begin
            for i := 1 to Fnphases do
            begin
                j := i + 1;
                if j > Fnconds then
                    j := 1;
                Vterminal[i] := ActiveCircuit.Solution.VDiff(NodeRef[i], NodeRef[j]);
            end;
        end;
    end;

    WindGenSolutionCount := ActiveCircuit.Solution.SolutionCount;
end;

procedure TWindGenObj.CalcVTerminal();
// Put terminal voltages in an array
begin
    ComputeVTerminal();
    WindGenSolutionCount := ActiveCircuit.Solution.SolutionCount;
end;

procedure TWindGenObj.CalcGenModelContribution();
// Calculates WindGen current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)
begin
    IterminalUpdated := FALSE;
    if ActiveCircuit.Solution.IsDynamicModel then
    begin
        DoDynamicMode();
        Exit;
    end;

    if ActiveCircuit.Solution.IsHarmonicModel and (ActiveCircuit.Solution.Frequency <> ActiveCircuit.Fundamental) then
    begin
        DoHarmonicMode();
        Exit;
    end;

    // compute currents and put into InjTemp array;
    case GenModel of
        1:
            DoConstantPQGen();
        2:
            DoConstantZGen();
        3:
            DoPVTypeGen(); // Constant P, |V|
        4:
            DoFixedQGen();
        5:
            DoFixedQZGen();
        6:
            DoUserModel();
    else
        DoConstantPQGen(); // for now, until we implement the other models.
    end;
    // When this is done, ITerminal is up to date
end;

procedure TWindGenObj.CalcInjCurrentArray();
// Difference between currents in YPrim and total current
begin
    // Now Get Injection Currents
    if GenSwitchOpen then
        ZeroInjCurrent()
    else
        CalcGenModelContribution();
end;

procedure TWindGenObj.GetTerminalCurrents(Curr: pComplexArray);
// Compute total Currents
var
    i: Integer;
begin
    if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
    begin     // recalc the contribution
        if not GenSwitchOpen then
            CalcGenModelContribution(); // Adds totals in Iterminal as a side effect
    end
    else TODO: check this else in Generator
        inherited GetTerminalCurrents(Curr);

    if (WindModelDyn.DebugTrace) then
        WriteTraceRecord('TotalCurrent');
end;

function TWindGenObj.InjCurrents(): Integer;
begin
    if ActiveCircuit.Solution.LoadsNeedUpdating then
        SetNominalGeneration(); // Set the nominal kW, etc for the type of solution being done

    CalcInjCurrentArray(); // Difference between currents in YPrim and total terminal current
    if (WindModelDyn.DebugTrace) then
        WriteTraceRecord('Injection');

    // Add into System Injection Current Array
    Result := inherited InjCurrents();
end;

procedure TWindGenObj.ResetRegisters();
var
    i: Integer;
begin
    for i := 1 to NumWGenregisters do
        Registers[i] := 0.0;
    for i := 1 to NumWGenregisters do
        Derivatives[i] := 0.0;
    FirstSampleAfterReset := true; // initialize for trapezoidal integration
end;

procedure TWindGenObj.Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
begin
    if ActiveCircuit.TrapezoidalIntegration then
    begin
        // Trapezoidal Rule Integration
        if not FirstSampleAfterReset then
            Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
    end
    else 
        // Plain Euler integration
        Registers[Reg] := Registers[Reg] + Interval * Deriv;

    Derivatives[Reg] := Deriv;
end;

procedure TWindGenObj.TakeSample();
// Update Energy from metered zone
var
    S: Complex;
    Smag: Double;
    HourValue: Double;
    IntervalHrs: Double;
begin
    // Compute energy in WindGen branch
    if not Enabled then
        Exit;

    IntervalHrs := ActiveCircuit.Solution.IntervalHrs;
    if GenON then
    begin
        S := cmplx(WindGenvars.Pnominalperphase * 0.001 * Fnphases, WindGenvars.Qnominalperphase * 0.001 * Fnphases);
        Smag := Cabs(S);
        HourValue := 1.0;
    end
    else
    begin
        S := 0;
        Smag := 0.0;
        HourValue := 0.0;
    end;

    if GenON or ActiveCircuit.TrapezoidalIntegration then
    // Make sure we always integrate for Trapezoidal case
    // Don't need to for Gen Off and normal integration
    begin
        if ActiveCircuit.PositiveSequence then
        begin
            S := S * 3.0;
            Smag := 3.0 * Smag;
        end;
        Integrate(Reg_kWh, S.re, IntervalHrs); // Accumulate the power
        Integrate(Reg_kvarh, S.im, IntervalHrs);
        SetDragHandRegister(Reg_MaxkW, abs(S.re));
        SetDragHandRegister(Reg_MaxkVA, Smag);
        Integrate(Reg_Hours, HourValue, IntervalHrs); // Accumulate Hours in operation
        Integrate(Reg_Price, S.re * ActiveCircuit.PriceSignal * 0.001, IntervalHrs); // Accumulate Hours in operation
        FirstSampleAfterReset := false;
    end;
end;

procedure TWindGenObj.InitDQDVCalc();
begin
    DQDV := 0.0;
    WindGenvars.Qnominalperphase := 0.5 * (varmax + varmin); // avg of the limits
end;

procedure TWindGenObj.CalcDQDV();
var
    i: Integer;
    cYii: Complex;
begin
    // use 1st node element of Y matrix For DQDV
    i := NodeRef[1];
    KLUSolve.GetMatrixElement(ActiveCircuit.Solution.hYsystem, i, i, @cYii);
    Yii := Cabs(cYii);
    // DQDV := Yii; // Save in DQDV for now
    DQDV := 2.0 * Yii * Vbase * vpu; // Save in DQDV for now

    DQDVSaved := DQDV; //Save for next time  Allows generator to be enabled/disabled during simulation
end;

procedure TWindGenObj.ResetStartPoint();
begin
    WindGenvars.Qnominalperphase := 1000.0 * kvarBase / Fnphases;
end;

procedure TWindGenObj.DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean);
var
    i: Integer;
begin
    inherited DumpProperties(F, Complete);

    FSWriteLn(F, Format('!DQDV=%10.2g', DQDV));

    for i := 1 to ParentClass.NumProperties do
        FSWriteLn(F, '~ ' + ParentClass.PropertyName[i] + '=' + GetPropertyValue(i));

    FSWriteLn(F);
end;

procedure TWindGenObj.InitHarmonics();
var
    E, Va: complex;
    NodeV: pNodeVarray;
begin
    YprimInvalid := true; // Force rebuild of YPrims
    GenFundamental := ActiveCircuit.Solution.Frequency; // Whatever the frequency is when we enter here.

    with WindGenvars do
    begin
        Yeq := Cinv(Cmplx(0.0, Xdpp)); // used for current calcs  Always L-N

        // Compute reference Thevinen voltage from phase 1 current
        if not GenON then
        begin
            Vthevharm := 0.0;
            ThetaHarm := 0.0;
            Exit;
        end;

        ComputeIterminal(); // Get present value of current
        NodeV := ActiveCircuit.Solution.NodeV;
        case Connection of
            0:// wye - neutral is explicit
                Va := NodeV[NodeRef[1]] - NodeV[NodeRef[Fnconds]];
            1:// delta -- assume neutral is at zero
                Va := NodeV[NodeRef[1]];
        end;

        E := Va - Iterminal[1] * cmplx(0.0, Xdpp);
        Vthevharm := Cabs(E); // establish base mag and angle
        ThetaHarm := Cang(E);
    end;
end;

procedure TWindGenObj.InitStateVars();
var
    i, NumData: Integer;
    V012,
    I012: array[0..2] of Complex;
    Vabc: array[1..3] of Complex;
    NodeV: pNodeVarray;
begin
    YprimInvalid := true; // Force rebuild of YPrims
    with WindGenvars do
    begin
        Zthev := Cmplx(Xdp / XRdp, Xdp);
        Yeq := Cinv(Zthev);

        // Compute nominal Positive sequence voltage behind transient reactance
        if not GenON then
        begin
            Vthev := 0;
            Theta := 0.0;
            dTheta := 0.0;
            w0 := 0;
            Speed := 0.0;
            dSpeed := 0.0;
            Exit;
        end;

        NodeV := ActiveCircuit.Solution.NodeV;

        ComputeIterminal();

        case Fnphases of

            1:
            begin
                Edp := NodeV[NodeRef[1]] - NodeV[NodeRef[2]] - ITerminal[1] * Zthev;
                VThevMag := Cabs(Edp);
            end;

            3:
            begin
                // Calculate Edp based on Pos Seq only
                Phase2SymComp(ITerminal, @I012);
                // Voltage behind Xdp  (transient reactance), volts

                for i := 1 to FNphases do
                    Vabc[i] := NodeV[NodeRef[i]] // Wye Voltage

                Phase2SymComp(@Vabc, @V012);
                Edp := V012[1] - I012[1] * Zthev; // Pos sequence
                VThevMag := Cabs(Edp);
            end;
        else
            DoSimpleMsg('Dynamics mode is implemented only for 1- or 3-phase WindGens. %s has %d phases.', [FullName, Fnphases], 5672);
            SolutionAbort := true;
        end;

        if DynamicEqObj = nil then
        begin
            // Shaft variables
            // Theta is angle on Vthev[1] relative to system reference
            Theta := Cang(Edp);
            if GenModel = 7 then
                Model7LastAngle := Theta;

            dTheta := 0.0;
            w0 := Twopi * ActiveCircuit.Solution.Frequency;
            // recalc Mmass and D in case the frequency has changed
            with WindGenvars do
            begin
                WindGenvars.Mmass := 2.0 * WindGenvars.Hmass * WindGenvars.kVArating * 1000.0 / (w0); // M = W-sec
                D := Dpu * kVArating * 1000.0 / (w0);
            end;
            Pshaft := -Power[1].re; // Initialize Pshaft to present power Output

            Speed := 0.0; // relative to synch speed
            dSpeed := 0.0;

            // Init User-written models
            if GenModel = 6 then
            begin
                if UserModel.Exists then
                    UserModel.FInit(Vterminal, Iterminal);

                Exit;
            end;

            WindModelDyn.Init(Vterminal, Iterminal);
            Exit;
        end;

        //
        // if DynamicEqObj <> nil then...
        //


        // Initializes the memory values for the dynamic equation
        for i := 0 to High(DynamicEqVals) do
            DynamicEqVals[i][1] := 0.0;

        // Check for initial conditions using calculated values (P0, Q0)
        NumData := (length(DynamicEqPair) div 2) - 1;
        for i := 0 to NumData do
        begin
            if DynamicEqObj.IsInitVal(DynamicEqPair[(i * 2) + 1]) then
            begin
                if DynamicEqPair[(i * 2) + 1] = 9 then
                begin
                    DynamicEqVals[DynamicEqPair[i * 2]][0] := Cang(Edp);
                    if GenModel = 7 then
                        Model7LastAngle := DynamicEqVals[DynamicEqPair[i * 2]][0];
                end
                else
                    DynamicEqVals[DynamicEqPair[i * 2]][0] := PCEValue[1, DynamicEqPair[(i * 2) + 1]];
            end;
        end;
    end;
end;

procedure TWindGenObj.IntegrateStates();
var
    TracePower: Complex;
    i, Numdata: Integer;
    h: Double;
begin
    // Compute Derivatives and then integrate

    ComputeIterminal();

    // Check for user-written exciter model.

    h := ActiveCircuit.Solution.DynaVars.h;

    with WindGenvars do
        if DynamicEqObj = nil then
        begin
            // Dynamics using the internal equation
            if (ActiveCircuit.Solution.DynaVars.IterationFlag = 0) then
            begin // First iteration of new time step
                ThetaHistory := Theta + 0.5 * h * dTheta;
                SpeedHistory := Speed + 0.5 * h * dSpeed;
            end;

            // Compute shaft dynamics
            TracePower := TerminalPowerIn(Vterminal, Iterminal, FnPhases);
            dSpeed := (Pshaft + TracePower.re - D * Speed) / Mmass;
            dTheta := Speed;

            // Trapezoidal method
            Speed := SpeedHistory + 0.5 * h * dSpeed;
            Theta := ThetaHistory + 0.5 * h * dTheta;

            // Write Dynamics Trace Record
            if WindModelDyn.DebugTrace then
            begin
                FSWrite(TraceFile, Format('t=%-.5g ', [ActiveCircuit.Solution.Dynavars.t]));
                FSWrite(TraceFile, Format(' Flag=%d ', [ActiveCircuit.Solution.Dynavars.Iterationflag]));
                FSWrite(TraceFile, Format(' Speed=%-.5g ', [Speed]));
                FSWrite(TraceFile, Format(' dSpeed=%-.5g ', [dSpeed]));
                FSWrite(TraceFile, Format(' Pshaft=%-.5g ', [PShaft]));
                FSWrite(TraceFile, Format(' P=%-.5g Q= %-.5g', [TracePower.Re, TracePower.im]));
                FSWrite(TraceFile, Format(' M=%-.5g ', [Mmass]));
                FSWriteln(TraceFile);
                FSFlush(TraceFile);
            end;

            if GenModel = 6 then
            begin
                if UserModel.Exists then
                    UserModel.Integrate();
            end
            else
            begin
                WindModelDyn.Integrate();
            end;
            Exit;
        end;
    
    with WindGenvars do
    begin
        // Dynamics using an external equation
        if (ActiveCircuit.Solution.Dynavars.IterationFlag = 0) then
        begin 
            // First iteration of new time step
            SpeedHistory := DynamicEqVals[DynOut[0]][0] + 0.5 * h * DynamicEqVals[DynOut[0]][1]; // first speed
            ThetaHistory := DynamicEqVals[DynOut[1]][0] + 0.5 * h * DynamicEqVals[DynOut[1]][1]; // then angle
        end;

        // Check for initial conditions using calculated values (P, Q, VMag, VAng, IMag, IAng)
        NumData := (length(DynamicEqPair) div 2) - 1;
        for i := 0 to NumData do
        begin
            if not DynamicEqObj.IsInitVal(DynamicEqPair[(i * 2) + 1]) then // it's not intialization
            begin
                case DynamicEqPair[(i * 2) + 1] of
                    0:
                        DynamicEqVals[DynamicEqPair[i * 2]][0] := -TerminalPowerIn(Vterminal, Iterminal, FnPhases).re;
                    1:
                        DynamicEqVals[DynamicEqPair[i * 2]][0] := -TerminalPowerIn(Vterminal, Iterminal, FnPhases).im;
                else
                    DynamicEqVals[DynamicEqPair[i * 2]][0] := PCEValue[1, DynamicEqPair[(i * 2) + 1]];
                end;
            end;
        end;

        // solves the differential equation using the given values
        DynamicEqObj.SolveEq(DynamicEqVals);
        
        // Trapezoidal method - Places the calues in the same vars to keep the code consistent
        Speed := SpeedHistory + 0.5 * h * DynamicEqVals[DynOut[0]][1];
        Theta := ThetaHistory + 0.5 * h * DynamicEqVals[DynOut[1]][1];

        // saves the new integration values in memoryspace
        DynamicEqVals[DynOut[0]][0] := Speed;
        DynamicEqVals[DynOut[1]][0] := Theta;
    end;
end;

function TWindGenObj.Get_Variable(i: Integer): Double;
// Return variables one at a time
var
    N, k: Integer;
begin
    N := 0;
    Result := -9999.99; // error return value
    if i < 1 then
    begin
        DoSimpleMsg('%s: invalid variable index %d.', [FullName, i], 565);
        Exit;
    end;
    if DynamicEqObj <> NIL then
    begin
        if i <= DynamicEqObj.NVariables * Length(DynamicEqVals[0]) then
            Result := DynamicEqObj.Get_DynamicEqVal(i - 1, DynamicEqVals)
        else
            DoSimpleMsg('%s: invalid variable index %d.', [FullName, i], 565);
        Exit;
    end;

    if i <= NumWGenVariables then
    begin
        case i of
            ord(TVar.userTrip):
                Result := WindModelDyn.userTrip;
            ord(TVar.wtgTrip):
                Result := WindModelDyn.wtgTrip;
            ord(TVar.Pcurtail):
                Result := WindModelDyn.Pcurtail;
            ord(TVar.Pcmd):
                Result := WindModelDyn.Pcmd;
            ord(TVar.Pgen):
                Result := WindModelDyn.Pgen;
            ord(TVar.Qcmd):
                Result := WindModelDyn.Qcmd;
            ord(TVar.Qgen):
                Result := WindModelDyn.Qgen;
            ord(TVar.Vref):
                Result := WindModelDyn.Vref;
            ord(TVar.Vmag):
                Result := WindModelDyn.Vmag;
            ord(TVar.vwind):
                Result := WindModelDyn.vwind;
            ord(TVar.WtRef):
                Result := WindModelDyn.WtRef;
            ord(TVar.WtAct):
                Result := WindModelDyn.Wt;
            ord(TVar.dOmg):
                Result := WindModelDyn.dOmg;
            ord(TVar.dFrqPuTest):
                Result := WindModelDyn.dFrqPuTest;
            ord(TVar.QMode):
                Result := WindModelDyn.QMode;
            ord(TVar.Qref):
                Result := WindModelDyn.Qref;
            ord(TVar.PFref):
                Result := WindModelDyn.PFref;
            ord(TVar.thetaPitch):
                Result := WindModelDyn.thetaPitch;
            ord(TVar.Pg):
                Result := WindGenVars.Pg;
            ord(TVar.Ps):
                Result := WindGenVars.Ps;
            ord(TVar.Pr):
                Result := WindGenVars.Pr;
            ord(TVar.s):
                Result := WindGenVars.s;
        end;
        Exit;
    end;
    
    if UserModel.Exists then
    begin
        N := UserModel.FNumVars;
        k := (i - NumWGenVariables);
        if k <= N then
        begin
            Result := UserModel.FGetVariable(k);
            Exit;
        end;
    end;
end;

procedure TWindGenObj.Set_Variable(i: Integer; Value: Double);
var
    N: Integer;
begin
    N := 0;
    if i < 1 then
    begin
        DoSimpleMsg('%s: invalid variable index %d.', [FullName, i], 565);
        Exit; // No variables to set
    end;
    if DynamicEqObj <> NIL then
    begin
        DoSimpleMsg('%s: cannot set state variable when using DynamicEq.', [FullName], 566);
        Exit;
    end;

    case i of
        ord(TVar.userTrip):
            WindModelDyn.userTrip := round(Value);
        ord(TVar.Pcurtail):
            WindModelDyn.Pcurtail := Value;
        ord(TVar.vwind):
            WindModelDyn.vwind := Value;
        ord(TVar.dFrqPuTest):
            WindModelDyn.dFrqPuTest := Value;
        ord(TVar.QMode):
            WindModelDyn.QMode := round(Value);
        ord(TVar.Qref):
            WindModelDyn.Qref := Value;
        ord(TVar.PFref):
            WindModelDyn.PFref := Value;
        ord(TVar.Pg):
            WindGenVars.Pg := Value;
        ord(TVar.Ps):
            WindGenVars.Ps := Value;
        ord(TVar.Pr):
            WindGenVars.Pr := Value;
        ord(TVar.s):
            WindGenVars.s := Value;
    end;

    if i <= NumWGenVariables then
        Exit;

    if UserModel.Exists then
    begin
        N := UserModel.FNumVars;
        i -= NumWGenVariables;
        if i <= N then
        begin
            UserModel.FSetVariable(i, Value);
            Exit;
        end;
    end;
end;

procedure TWindGenObj.GetAllVariables(States: pDoubleArray);
var
    i, N: Integer;
begin
    N := 0;
    if DynamicEqObj <> NIL then
    begin
        for i := 1 to DynamicEqObj.NVariables * Length(DynamicEqVals[0]) do
            States[i - 1] := DynamicEqObj.Get_DynamicEqVal(i - 1, DynamicEqVals);

        Exit;
    end;

    for i := 1 to NumWGenVariables do
        States[i - 1] := Variable[i];

    if UserModel.Exists then
    begin
        N := UserModel.FNumVars;
        UserModel.FGetAllVars(pDoubleArray(@States[NumWGenVariables]));
    end;
end;

function TWindGenObj.NumVariables(): Integer;
begin
    // Try DynamicExp first
    Result := inherited NumVariables();
    if Result <> 0 then 
        Exit;

    // Fallback to the classic
    Result := NumWGenVariables;
    if UserModel.Exists then
        Result := Result + UserModel.FNumVars;
end;

function TWindGenObj.VariableName(i: Integer): String;
const
    BuffSize = 255;
var
    n,
    i2: Integer;
    Buff: array[0..BuffSize] of Ansichar;
    pName: Pansichar;
begin
    Result := 'ERROR';
    if i < 1 then
        Exit; // Someone goofed

    // Try DynamicExp first
    Result := inherited VariableName(i);
    if Length(Result) <> 0 then
        Exit;

    // Fallback to the classic
    n := 0;
    if i > 0 and i < NumWGenVariables then
    begin
        Result := TWindGen(ParentClass).varNames[i - 1];
        Exit;
    end;

    if UserModel.Exists then  // Checks for existence and Selects
    begin
        pName := PAnsiChar(@Buff);
        n := UserModel.FNumVars;
        i2 := i - NumWGenVariables;
        if i2 <= n then
        begin
            UserModel.FGetVarName(i2, pName, BuffSize);
            Result := String(pName);
            Exit;
        end;
    end;
end;

procedure TWindGenObj.MakePosSequence();
var
    V: Double;
    had_kVA, had_MVA, had_kvars: Boolean;
    kW_new, PF_new, new_kVA, new_MVA, new_minkvar, new_maxkvar: Double;
    oldPhases, changes: Integer;
begin
    // Make sure voltage is line-neutral
    if (Fnphases > 1) or (connection <> 0) then
        V := WindGenvars.kVWindGenBase / SQRT3
    else
        V := WindGenvars.kVWindGenBase;

    // Divide the load by no. phases
    changes := 3;
    oldPhases := Fnphases;
    if Fnphases > 1 then
    begin
        had_kVA := PrpSequence[ord(TProp.kVA)] <> 0;
        had_MVA := PrpSequence[ord(TProp.MVA)] <> 0;
        had_kvars := (PrpSequence[ord(TProp.Maxkvar)] <> 0) or (PrpSequence[ord(TProp.Minkvar)] <> 0);
        kW_new := kWbase / Fnphases;
        PF_new := PFNominal;
        if had_kvars then
        begin
            new_minkvar := kvarmin / Fnphases;
            new_maxkvar := kvarmax / Fnphases;
            Inc(changes);
        end;
        if had_kVA then
        begin
            new_kVA := WindGenvars.kvarating / Fnphases;
            Inc(changes);
        end;
        if had_MVA then
        begin
            new_MVA := WindGenvars.kvarating / 1000.0 / Fnphases;
            Inc(changes);
        end;
    end;

    BeginEdit(True);
    SetInteger(ord(TProp.Phases), 1, []);
    SetInteger(ord(TProp.conn), 0, []);
    SetDouble(ord(TProp.kV), V, []);
    if oldPhases > 1 then
    begin
        SetDouble(ord(TProp.kW), kW_new, []);
        SetDouble(ord(TProp.PF), PF_new, []);
        if had_kvars then
        begin
            SetDouble(ord(TProp.minkvar), new_minkvar, []);
            SetDouble(ord(TProp.maxkvar), new_maxkvar, []);
        end;
        if had_kVA then
            SetDouble(ord(TProp.kVA), new_kVA, []);
        if had_MVA then
            SetDouble(ord(TProp.MVA), new_MVA, []);
    end;
    EndEdit(changes);

    inherited;
end;

procedure TWindGenObj.Set_ConductorClosed(Index: Integer; Value: Boolean);
begin
    inherited;
    GenSwitchOpen := not Value; // Just turn WindGen on or off;
end;

procedure TWindGenObj.SyncUpPowerQuantities;
begin
    // keep kvar nominal up to date with kW and PF
    if (PFNominal <> 0.0) then
    begin
        kvarBase := kWBase * sqrt(1.0 / Sqr(PFNominal) - 1.0);
        WindGenvars.Qnominalperphase := 1000.0 * kvarBase / Fnphases;
        kvarMax := 2.0 * kvarBase;
        kvarMin := -kvarMax;
        if PFNominal < 0.0 then
            kvarBase := -kvarBase;

        if kVANotSet then
            WindGenvars.kVARating := kWBase * 1.2;
    end;
end;

procedure TWindGenObj.SetDragHandRegister(Reg: Integer;
    const Value: Double);
begin
    if Value > Registers[reg] then
        Registers[Reg] := Value;
end;

procedure TWindGenObj.SetkWkvar(const PkW, Qkvar: Double);
begin
    kWBase := PkW;
    Presentkvar := Qkvar;
end;

procedure TWindGenObj.CalcVthev_Dyn;
begin
    if GenSwitchOpen then
        WindGenvars.VThevMag := 0.0;
    Vthev := pclx(WindGenvars.VthevMag, WindGenvars.Theta);
end;

procedure TWindGenObj.CalcVthev_Dyn_Mod7(const V: Complex);
// Adjust VThev to be in phase with V, if possible
// 
// If the voltage magnitude drops below 15% or so, the accuracy of determining the
// phase angle gets flaky. This algorithm approximates the action of a PLL that will
// hold the last phase angle until the voltage recovers.
var
    Model7angle: Double;
begin
    if GenSwitchOpen then
        WindGenvars.VThevMag := 0.0;
    // For Phases=1, Vbase is voltage across the terminals.
    // Else it is LN voltage.
    if Cabs(V) > 0.2 * Vbase then
        Model7angle := Cang(V)
    else
        Model7Angle := Model7LastAngle;

    Vthev := pclx(WindGenvars.VthevMag, Model7angle);
    Model7Lastangle := Model7angle;
end;

finalization
    WindGenModelEnum.Free;
end.
