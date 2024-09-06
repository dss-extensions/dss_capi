unit WindGen;

// Copyright (c) 2024, DSS-Extensions contributors
// Copyright (c) 2024, Electric Power Research Institute, Inc.
// All rights reserved.

interface

uses
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    ucomplex, DSSUComplex,
    LoadShape,
    GrowthShape,
    Spectrum,
    ArrayDef,
    DynEqPCE,
    Dynamics,
    WTG3_Model,
    XYCurve,
    DSSObject,
    Classes;

const
    NumWGenRegisters = 6; // Number of energy meter registers

type
{$SCOPEDENUMS ON}
    TWindGenPropLegacy = (
        INVALID = 0,
        phases, // 1
        bus1, // 2
        kv, // 3
        kW, // 4
        PF, // 5
        model, // 6
        yearly, // 7
        daily, // 8
        duty, // 9
        conn, // 10
        kvar, // 11
        cls, // 12
        debugtrace, // 13
        Vminpu, // 14
        Vmaxpu, // 15
        kVA, // 16
        MVA, // 17
        // UserModel, // 18
        // UserData, // 19
        DutyStart, // 20
        DynamicEq, // 21
        DynOut, // 22
        Rthev, // 23
        Xthev, // 24
        Vss, // 25
        Pss, // 26
        Qss, // 27
        vwind, // 28
        QMode, // 29
        SimMechFlg, // 30
        APCFlg, // 31
        QFlg, // 32
        delt0, // 33
        N_WTG, // 34
        VV_Curve, // 35
        Ag, // 36
        Cp, // 37
        Lamda, // 38
        P, // 39
        pd, // 40
        PLoss, // 41
        Rad, // 42
        VCutIn, // 43
        VCutOut // 44
    );

    TWindGenProp = (
        INVALID = 0,
        Phases,
        Bus1,
        kV,
        kW,
        PF,
        Model,
        Yearly,
        Daily,
        Duty,
        Conn,
        kvar,
        cls,
        DebugTrace,
        Vminpu,
        Vmaxpu,
        kVA,
        MVA,
        DutyStart,
        DynamicEq,
        DynOut,
        RThev,
        XThev,
        VSS,
        PSS,
        QSS,
        VWind,
        QMode,
        SimMechFlg,
        APCFlg,
        QFlg,
        delt0,
        N_WTG,
        VV_Curve,
        Ag,
        Cp,
        Lamda,
        P,
        pd,
        PLoss,
        Rad,
        VCutIn,
        VCutOut
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

    // WindGen public data/state variable structure
    TWindGenVars = {$IFNDEF DSS_CAPI_NO_PACKED_RECORDS}packed{$ENDIF} record

        Theta, // Direct-Axis voltage magnitude & angle
        Pshaft,
        Speed,
        w0, // present Shaft Power and relative Speed, rad/sec, difference from Synchronous speed, w0
        // actual speed = Speed + w0
        Hmass, // Per unit mass constant
        Mmass, // Mass constant actual values (Joule-sec/rad
        D, Dpu, // Actual and per unit damping factors
        kVArating,
        kVWindGenBase,
        // Xdp, 
        // Xdpp, // machine Reactances, ohms
        // puXdp, 
        // puXdpp, // machine Reactances, per unit
        dTheta,
        dSpeed, // Derivatives of Theta and Speed
        ThetaHistory,
        SpeedHistory, // history variables for integration
        Pnominalperphase,
        Qnominalperphase: Double; // Target P and Q for power flow solution, watts, vars}: Double;    { All Doubles 

        // 32-bit integers
        NumPhases, // Number of phases
        NumConductors, // Total Number of conductors (wye-connected will have 4)
        Conn: Integer;   // 0 = wye; 1 = Delta

        // Revisions (additions) to structure ...
        // Later additions are appended to end of the structure so that
        // previously compiled DLLs do not break

        VthevMag: Double; // Thevinen equivalent voltage for dynamic model
        // VThevHarm: Double; // Thevinen equivalent voltage mag reference for Harmonic model
        // ThetaHarm: Double; // Thevinen equivalent voltage angle reference for Harmonic model
        // VTarget: Double;   // Target voltage for WindGen with voltage control
        // Zthev: Complex;
        XRdp: Double;  // Assumed X/R for Xd'

        PLoss: String;     // Name of the XY curve describing the active power losses for the turbine
        ag, // Garbox ratio
        Cp, // Turbine performance coefficient
        Lamda, // Tip speed ratio
        Poles, // Number of poles of the induction generator
        pd, // Air density
        Rad, // Rotor radius
        VCutin, // Cut-in speed for the wind generator
        VCutout, // Cut-out speed for the wind generator
        Pm, // mechanical power (steady-state)
        Ps, // Stator active power
        Pr, // Rotor active power
        Pg, // Total power output
        s: Double;     // generator pitch
    end;

    TWindGen = class(TDynEqPCEClass)
    PROTECTED
        cBuffer: TCBuffer24; // Temp buffer for calcs  24-phase WindGen?
        
        procedure DefineProperties(); override;
    PUBLIC
        RegisterNames: ArrayOfString;
        varNames: ArrayOfString;

        constructor Create(dssContext: TDSSContext);
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
        // PhaseCurrentLimit: Complex;

        FForcedON: Boolean;
        FirstSampleAfterReset: Boolean;
        // GenFundamental: Double; // Thevinen equivalent voltage mag and angle reference for Harmonic model
        GenON: Boolean; // Indicates whether WindGen is currently on
        GenSwitchOpen: Boolean;
        kVANotSet: Boolean;
        PVFactor: Double; // deceleration Factor for computing vars for PV WindGens
        TraceFile: TFileStream;
        V_Avg: Double;
        varBase: Double; // Base vars per phase
        VBase: Double; // Base volts suitable for computing currents
        VBase105: Double;
        VBase95: Double;
        Vthev: Complex; // Thevinen equivalent voltage (complex) for dynamic model
        YQFixed: Double; // Fixed value of y for type 7 load
        ShapeIsActual: Boolean;

        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double); // now incorporates DutyStart offset
        procedure CalcYearlyMult(Hr: Double);

        procedure CalcGenModelContribution();
        procedure CalcYPrimMatrix(Ymatrix: TcMatrix);

        procedure DoConstantPQGen();
        procedure DoConstantZGen();
        procedure DoDynamicMode();
        procedure DoFixedQGen();
        procedure DoFixedQZGen();
        procedure DoHarmonicMode();

        procedure Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
        procedure SetDragHandRegister(Reg: Integer; const Value: Double);

        procedure WriteTraceRecord(const s: String);

    PROTECTED
        procedure GetTerminalCurrents(Curr: pComplexArray); OVERRIDE;

    PUBLIC
        ShapeFactor: Complex;

        WindModelDyn: TGE_WTG3_Model;
        DailyDispShapeObj: TLoadShapeObj; // Daily (24 HR) WindGen shape
        DutyShapeObj: TLoadShapeObj; // Duty cycle load shape for changes typically less than one hour
        DutyStart: Double; // starting time offset into the DutyShape [hrs] for this WindGen
        GenClass: Integer;
        GenModel: Integer; // Variation with voltage
        GenVars: TWindGenVars; // State Variables
        kvarBase: Double;
        kWBase: Double;
        PFNominal: Double;
        
        // Vpu: Double; // per unit Target voltage for WindGen with voltage control
        
        Vmaxpu: Double;
        Vminpu: Double;
        VV_CurveObj: TXYcurveObj;
        Loss_CurveObj: TXYcurveObj;
        GenActive: Boolean;

        YearlyShapeObj: TLoadShapeObj; // Shape for this WindGen

        Registers, Derivatives: array[1..NumWGenregisters] of Double;

        constructor Create(ParClass: TDSSClass; const genName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData(); OVERRIDE;
        procedure CalcYPrim(); OVERRIDE;
        procedure SetConductorClosed(Index: Integer; Value: Boolean); OVERRIDE;

        function InjCurrents(): Integer; OVERRIDE;
        function NumVariables(): Integer; OVERRIDE;
        procedure GetAllVariables(var States: ArrayOfDouble); OVERRIDE;
        function GetVariable(i: Integer): Double; OVERRIDE;
        procedure SetVariable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;
        procedure SyncUpPowerQuantities();


        procedure SetNominalGeneration();

        procedure ResetRegisters();
        procedure TakeSample();

        // Support for Dynamics Mode
        procedure InitStateVars(); OVERRIDE;
        procedure IntegrateStates(); OVERRIDE;
       
        procedure InitHarmonics(); OVERRIDE; // Support for Harmonics Mode
        procedure MakePosSequence(); OVERRIDE;
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
    Solution,
    Utilities,
    BufStream,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TWindGenObj;
    TProp = TWindGenProp;
    TPropLegacy = TWindGenPropLegacy;
    TVar = TWindGenVariable;
const
    NumPropsThisClass = Ord(High(TProp));
    NumWGenVariables = ord(High(TVar));
    // Register values inherited from Generator model
    Reg_kWh = 1;
    Reg_kvarh = 2;
    Reg_MaxkW = 3;
    Reg_MaxkVA = 4;
    Reg_Hours = 5;
    Reg_Price = 6;
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;
    VarInfo: Pointer = NIL;
    WindGenQModeEnum, WindGenModelEnum: TDSSEnum;

constructor TWindGen.Create(dssContext: TDSSContext); // Creates superstructure for all objects
var
    i: Integer;
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
        VarInfo := TypeInfo(TVar);

        WindGenQModeEnum := TDSSEnum.Create('WindGen: Q Mode', True, 0, 0, [
            'Q', 'PF', 'VoltVar'],
            [0, 1, 2],
            ['Q', 'PF', 'VoltVar']);
        WindGenQModeEnum.JSONUseNumbers := true;

        WindGenModelEnum := TDSSEnum.Create('WindGen: Model', True, 0, 0, [
            'Constant PQ', 'Constant Z', 'Constant P, fixed Q', 'Constant P, fixed X'],
            [1, 2, 4, 5],
            ['ConstantPQ', 'ConstantZ', 'ConstantP_FixedQ', 'ConstantP_FixedX']);
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

    PropertyType[ord(TProp.QMode)] := TPropertyType.MappedIntEnumProperty;
    PropertyOffset[ord(TProp.QMode)] := ptruint(@obj.WindModelDyn.QMode);
    PropertyOffset2[ord(TProp.QMode)] := PtrInt(WindGenQModeEnum);

    PropertyType[ord(TProp.SimMechFlg)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.SimMechFlg)] := ptruint(@obj.WindModelDyn.SimMechFlg);

    PropertyType[ord(TProp.APCFlg)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.APCFlg)] := ptruint(@obj.WindModelDyn.APCFLG);

    //TODO: QFlg should be a boolean; left as int for compatibility
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

    PropertyOffset[ord(TProp.RThev)] := ptruint(@obj.WindModelDyn.ZThev.re);
    PropertyOffset[ord(TProp.XThev)] := ptruint(@obj.WindModelDyn.ZThev.im);

    PropertyOffset[ord(TProp.Vminpu)] := ptruint(@obj.VMinPu);
    PropertyOffset[ord(TProp.Vmaxpu)] := ptruint(@obj.VMaxPu);

    PropertyOffset[ord(TProp.DutyStart)] := ptruint(@obj.DutyStart);
    PropertyFlags[ord(TProp.DutyStart)] := [TPropertyFlag.Units_hour];

    PropertyOffset[ord(TProp.kVA)] := ptruint(@obj.GenVars.kVArating);
    PropertyFlags[ord(TProp.kVA)] := [TPropertyFlag.DynamicDefault];

    PropertyOffset[ord(TProp.delt0)] := ptruint(@obj.WindModelDyn.delt0);
    
    PropertyOffset[ord(TProp.VSS)] := ptruint(@obj.WindModelDyn.VSS);
    PropertyFlags[ord(TProp.VSS)] := [TPropertyFlag.Units_pu_Voltage, TPropertyFlag.NonNegative];

    PropertyOffset[ord(TProp.PSS)] := ptruint(@obj.WindModelDyn.PSS);
    PropertyOffset[ord(TProp.QSS)] := ptruint(@obj.WindModelDyn.QSS);
    
    PropertyOffset[ord(TProp.VWind)] := ptruint(@obj.WindModelDyn.vwind);
    PropertyFlags[ord(TProp.VWind)] := [TPropertyFlag.NonNegative, TPropertyFlag.Units_m_per_s];
    
    PropertyOffset[ord(TProp.Ag)] := ptruint(@obj.GenVars.ag);
    PropertyOffset[ord(TProp.Cp)] := ptruint(@obj.GenVars.Cp);
    PropertyOffset[ord(TProp.Lamda)] := ptruint(@obj.GenVars.Lamda);
    PropertyOffset[ord(TProp.P)] := ptruint(@obj.GenVars.Poles);
    
    PropertyOffset[ord(TProp.pd)] := ptruint(@obj.GenVars.pd);
    PropertyFlags[ord(TProp.pd)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.Units_kg_m3];
    
    
    PropertyOffset[ord(TProp.Rad)] := ptruint(@obj.GenVars.Rad);
    PropertyFlags[ord(TProp.Rad)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.Units_m];

    PropertyOffset[ord(TProp.VCutIn)] := ptruint(@obj.GenVars.VCutin);
    PropertyFlags[ord(TProp.VCutIn)] := [TPropertyFlag.Units_m_per_s];
    
    PropertyOffset[ord(TProp.VCutOut)] := ptruint(@obj.GenVars.VCutout);
    PropertyFlags[ord(TProp.VCutOut)] := [TPropertyFlag.Units_m_per_s];

    PropertyOffset[ord(TProp.kV)] := ptruint(@obj.GenVars.kVWindGenBase);
    PropertyFlags[ord(TProp.kV)] := [TPropertyFlag.Required, TPropertyFlag.Units_kV, TPropertyFlag.NonNegative];

    PropertyOffset[ord(TProp.kvar)] := ptruint(@obj.kvarBase);
    PropertyFlags[ord(TProp.kvar)] := [TPropertyFlag.NoDefault, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_kvar];

    PropertyFlags[ord(TProp.kW)] := [TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_kW];
    PropertyFlags[ord(TProp.PF)] := [TPropertyFlag.RequiredInSpecSet, TPropertyFlag.PowerFactorLimits];

    // adv doubles
    PropertyOffset[ord(TProp.MVA)] := ptruint(@obj.GenVars.kVArating);
    PropertyScale[ord(TProp.MVA)] := 1000.0;
    PropertyFlags[ord(TProp.MVA)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.MVA)] := ord(TProp.kVA);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties();
end;

function TWindGen.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.SetActiveCktElement(Obj);
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure SetNcondsForConnection(obj: TObj);
begin
    case obj.Connection of
        TGeneralConnection.Wye:
            obj.SetNConds(obj.Fnphases + 1);
        TGeneralConnection.Delta:
            case obj.Fnphases of
                1, 2:
                    obj.SetNConds(obj.Fnphases + 1); // L-L and Open-delta
            else
                obj.SetNConds(obj.Fnphases);
            end;
    end;
end;

function TWindGen.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
var
    obj: TObj;
begin
    obj:= TObj(ptr);
    obj.RecalcElementData();
    obj.SetYprimInvalid(true);
    // if Flg.NeedsYprim in obj.Flags then
    // begin
    //     obj.SetYprimInvalid(true);
    //     Exclude(obj.Flags, Flg.NeedsYprim);
    // end;
    Exclude(obj.Flags, Flg.EditingActive);
    Result := True;
end;

procedure TWindGenObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
var
    i: Integer;
    kVA_Gen: Double;
    // addedNeedsYprim: Boolean = false;
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
                with GenVars do 
                    case Fnphases of
                        2, 3:
                            VBase := kVWindGenBase * InvSQRT3x1000; // L-N Volts
                    else
                        VBase := kVWindGenBase * 1000.0; // Just use what is supplied
                    end;

                Yorder := FNConds * Fnterms;
                SetYprimInvalid(true);
            end;
            TProp.kV:
                with GenVars do
                    case FNphases of
                        2, 3:
                            VBase := kVWindGenBase * InvSQRT3x1000;
                    else
                        VBase := kVWindGenBase * 1000.0;
                    end;

            TProp.kvar:
            begin
                GenVars.Qnominalperphase := 1000.0 * kvarBase / Fnphases; // init to something reasonable
                kVA_Gen := Sqrt(Sqr(kWBase) + Sqr(kvarBase));
                if kVA_Gen <> 0.0 then
                    PFNominal := kWBase / kVA_Gen
                else
                    PFNominal := 1.0;
                if (kWBase * kvarBase) < 0.0 then
                    PFNominal := -PFNominal;

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
                SyncUpPowerQuantities();
                if TProp(idx) = TProp.PF then
                begin
                    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.NoPropertyTracking)) = 0 then
                    begin
                        PrpSequence[ord(TProp.kvar)] := 0;
                    end;
                end;
            end;

            TProp.DebugTrace:
                if WindModelDyn.DebugTrace then
                begin
                    FreeAndNil(TraceFile);
                    TraceFile := TBufferedFileStream.Create(DSS.OutputDirectory + 'WINDGEN_' + Name + '.csv', fmCreate);
                    FSWrite(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, GenModel, dQdV, Avg_Vpu, Vdiff, MQnominalperphase, MPnominalperphase, CurrentType');
                    for i := 1 to FNPhases do
                        FSWrite(Tracefile, ', |Iinj' + IntToStr(i) + '|');
                    for i := 1 to FNPhases do
                        FSWrite(Tracefile, ', |Iterm' + IntToStr(i) + '|');
                    for i := 1 to FNPhases do
                        FSWrite(Tracefile, ', |Vterm' + IntToStr(i) + '|');
                    FSWrite(TraceFile, ',Vthev, Theta');
                    FSWriteln(TraceFile);
                    FSFlush(Tracefile);
                    // WindModelDyn.InitTraceFile(); -- done in RecalcElementData
                end
                else
                begin
                    FreeAndNil(TraceFile);
                end;
                
            TProp.kVA, TProp.MVA:
            begin
                WindModelDyn.ratedKVA := GenVars.kVArating;
                kVANotSet := FALSE;
            end;

            TProp.DynamicEq:
                if DynamicEqObj <> NIL then
                    SetLength(DynamicEqVals, DynamicEqObj.NVariables);

            TProp.VV_Curve: //  the Volt-var control curve
                if VV_CurveObj <> NIL then
                begin
                    WindModelDyn.V1_VoltVar := VV_CurveObj.XValue_pt(1);
                    WindModelDyn.V2_VoltVar := VV_CurveObj.XValue_pt(2);
                    WindModelDyn.V3_VoltVar := VV_CurveObj.XValue_pt(3);
                    WindModelDyn.V4_VoltVar := VV_CurveObj.XValue_pt(4);
                    WindModelDyn.Q1_VoltVar := VV_CurveObj.YValue_pt(1);
                    WindModelDyn.Q2_VoltVar := VV_CurveObj.YValue_pt(2);
                    WindModelDyn.Q3_VoltVar := VV_CurveObj.YValue_pt(3);
                    WindModelDyn.Q4_VoltVar := VV_CurveObj.YValue_pt(4);
                    // WindModelDyn.ReCalcElementData(); -- already done in our ReCalcElementData
                end;

            TProp.PLoss:
                if Loss_CurveObj <> NIL then
                    GenVars.PLoss := Loss_CurveObj.Name();

            TProp.VWind:
            begin
                //TODO: save a copy so it doesn't get affected by the loadshapes
            end;
        end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

procedure TWindGenObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);

    Other := TObj(OtherPtr);
    if (Fnphases <> Other.Fnphases) then
    begin
        FNphases := Other.Fnphases;
        SetNConds(Fnphases); // Forces reallocation of terminal stuff

        Yorder := FNConds * Fnterms;
        SetYprimInvalid(true);
    end;

    GenVars.kVWindGenBase := Other.GenVars.kVWindGenBase;
    Vbase := Other.Vbase;
    Vminpu := Other.Vminpu;
    Vmaxpu := Other.Vmaxpu;
    Vbase95 := Other.Vbase95;
    Vbase105 := Other.Vbase105;
    kWBase := Other.kWBase;
    kvarBase := Other.kvarBase;
    GenVars.Pnominalperphase := Other.GenVars.Pnominalperphase;
    PFNominal := Other.PFNominal;
    GenVars.Qnominalperphase := Other.GenVars.Qnominalperphase;
    Connection := Other.Connection;
    YearlyShapeObj := Other.YearlyShapeObj;
    DailyDispShapeObj := Other.DailyDispShapeObj;
    DutyShapeObj := Other.DutyShapeObj;
    DutyStart := Other.DutyStart;
    GenClass := Other.GenClass;
    GenModel := Other.GenModel;
    // IsFixed := Other.IsFixed;
    // GenVars.VTarget := Other.GenVars.VTarget;
    FForcedON := Other.FForcedON;
    kVANotSet := Other.kVANotSet;

    GenVars.kVArating := Other.GenVars.kVArating;
    // GenVars.puXdp := Other.GenVars.puXdp;
    // GenVars.puXdpp := Other.GenVars.puXdpp;
    GenVars.Hmass := Other.GenVars.Hmass;
    GenVars.Theta := Other.GenVars.Theta;
    GenVars.Speed := Other.GenVars.Speed;
    GenVars.w0 := Other.GenVars.w0;
    GenVars.dSpeed := Other.GenVars.dSpeed;
    GenVars.D := Other.GenVars.D;
    GenVars.Dpu := Other.GenVars.Dpu;
    GenVars.XRdp := Other.GenVars.Xrdp;

    SetNCondsForConnection(self);
    RecalcElementData();
end;

procedure TWindGen.ResetRegistersAll(); // Force all EnergyMeters in the circuit to reset
var
    pGen: TObj;
begin
    for pGen in ElementList do
    begin
        pGen.ResetRegisters();
    end;
end;

procedure TWindGen.SampleAll(); // Force all EnergyMeters in the circuit to take a sample
var
    pGen: TObj;
begin
    for pGen in ElementList do
    begin
        if pGen.Enabled() then
            pGen.TakeSample();
    end;
end;

function TWindGen.GetRegisterNames(obj: TDSSObject): ArrayOfString;
begin
    Result := RegisterNames;
end;

function TWindGen.GetRegisterValues(obj: TDSSObject; var numRegisters: Integer): pDoubleArray;
begin
    if not (obj is TObj) then
    begin
        Result := NIL;
        numRegisters := 0;
        Exit;
    end;
    numRegisters := NumWGenRegisters;
    Result := pDoubleArray(@TObj(obj).Registers[1]);
end;

constructor TWindGenObj.Create(ParClass: TDSSClass; const genName: String);
begin
    inherited create(ParClass, genName);
    DSSObjType := ParClass.DSSClassType; // + WINDGEN_ELEMENT; // In both PCelement and Genelement list

    FNphases := 3; //TODO: check if we need any side-effect for this
    FNConds := 4; // defaults to wye
    Yorder := 0; // To trigger an initial allocation
    SetNTerms(1); // forces allocations
    kWBase := 1000.0;
    kvarBase := 60.0;

    PFNominal := 0.88;
    YearlyShapeObj := nil; // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
    DailyDispShapeObj := nil; // if DaillyShapeobj = nil then the load alway stays nominal * global multipliers
    DutyShapeObj := nil; // if DutyShapeobj = nil then the load alway stays nominal * global multipliers
    DutyStart := 0.0;
    // Connection := 0; // Wye (star) -- now done in PCE
    GenModel := 1; // Typical fixed kW negative load
    GenClass := 1;

    GenVars.kVWindGenBase := 12.47;
    // Vpu := 1.0;
    // GenVars.VTarget := 1000.0 * Vpu * GenVars.kVWindGenBase / SQRT3; // Line-to-Neutral target
    // GenVars.VTarget := 1000.0 * GenVars.kVWindGenBase / SQRT3; // Line-to-Neutral target
    VBase := 7200.0;
    Vminpu := 0.90;
    Vmaxpu := 1.10;
    VBase95 := Vminpu * Vbase;
    VBase105 := Vmaxpu * Vbase;
    Yorder := Fnterms * FNConds;
    // IsFixed := false;

    // Machine rating stuff
    GenVars.kVArating := kWBase * 1.2;
    kVANotSet := true; // Flag for default value for kVA

    with GenVars do
    begin
        // These are inherited from the generator object, it is uncertain if needed
        // puXdp := 0.28;
        // puXdpp := 0.20;
        // Xdp := puXdp * SQR(kVWindGenBase) * 1000.0 / kVARating;
        // Xdpp := puXdpp * SQR(kVWindGenBase) * 1000.0 / kVARating;
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

    // Advertise GenVars struct as public

    PublicDataStruct := pointer(@GenVars);
    PublicDataSize := SizeOf(TWindGenVars);

    PVFactor := 0.1;
    WindModelDyn.DebugTrace := false;
    FForcedON := false;
    GenSwitchOpen := false;
    ShapeIsActual := false;

    SpectrumObj := DSS.SpectrumClass.DefaultGen; // override base class

    GenActive := true; // variable to use if needed

    // Creates the Dynamic model for the Wind Turbine
    WindModelDyn.Initialize(DSS);
    WindModelDyn.VWind := 12;
    WindModelDyn.QMode := 0;

    RecalcElementData();
end;

destructor TWindGenObj.Destroy;
begin
    inherited Destroy;
end;

procedure TWindGenObj.CalcDailyMult(Hr: Double);
begin
    if (DailyDispShapeObj <> nil) then
    begin
        ShapeFactor := DailyDispShapeObj.MultAtHour(Hr);
        ShapeIsActual := DailyDispShapeObj.UseActual;
    end
    else
        ShapeFactor := cmplx(WindModelDyn.VWind, 0); // Default to no daily variation
end;

procedure TWindGenObj.CalcDutyMult(Hr: Double);
begin
    if DutyShapeObj <> nil then
    begin
        ShapeFactor := DutyShapeObj.MultAtHour(Hr + DutyStart);
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
        ShapeFactor := YearlyShapeObj.MultAtHour(Hr);
        ShapeIsActual := YearlyShapeObj.UseActual;
    end
    else
        ShapeFactor := cmplx(WindModelDyn.VWind, 0); // Defaults to no variation
end;

procedure TWindGenObj.SetNominalGeneration();
var
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
    mode := ActiveCircuit.Solution.Mode();
    dblHour := ActiveCircuit.Solution.DynaVars.dblHour;

    VMag := 0.0;
    VMagTmp := 0.0;
    GenOn_Saved := GenON;
    ShapeFactor := cmplx(WindModelDyn.VWind, 0);
    
    // Check to make sure the generation is ON
    kvarCalc := 0.0;
    GenON := true; // The first assumption is that the generator is ON

    case Mode of
        TSolveMode.SNAPSHOT:
            Factor := ActiveCircuit.GenMultiplier;
        TSolveMode.DAILYMODE:
        begin
            Factor := ActiveCircuit.GenMultiplier;
            CalcDailyMult(dblHour) // Daily dispatch curve
        end;
        TSolveMode.YEARLYMODE:
        begin
            Factor := ActiveCircuit.GenMultiplier;
            CalcYearlyMult(dblHour);
        end;
        TSolveMode.DUTYCYCLE:
        begin
            Factor := ActiveCircuit.GenMultiplier;
            CalcDutyMult(dblHour);
        end;
        TSolveMode.GENERALTIME,   // General sequential time simulation
        TSolveMode.DYNAMICMODE:
        begin
            Factor := ActiveCircuit.GenMultiplier;
            // This mode allows use of one class of load shape
            case ActiveCircuit.ActiveLoadShapeClass of
                USEDAILY:
                    CalcDailyMult(dblHour);
                USEYEARLY:
                    CalcYearlyMult(dblHour);
                USEDUTY:
                    CalcDutyMult(dblHour);
            else
                ShapeFactor := cmplx(WindModelDyn.VWind, 0); // default to the wind speed set by default
            end;
        end;
        TSolveMode.MONTECARLO1,
        TSolveMode.MONTEFAULT,
        TSolveMode.FAULTSTUDY:
            Factor := ActiveCircuit.GenMultiplier * 1.0;
        TSolveMode.MONTECARLO2,
        TSolveMode.MONTECARLO3,
        TSolveMode.LOADDURATION1,
        TSolveMode.LOADDURATION2:
        begin
            Factor := ActiveCircuit.GenMultiplier;
            CalcDailyMult(dblHour);
        end;
        TSolveMode.PEAKDAY:
        begin
            Factor := ActiveCircuit.GenMultiplier;
            CalcDailyMult(dblHour);
        end;
        TSolveMode.AUTOADDFLAG:
            Factor := 1.0;
    else
        Factor := ActiveCircuit.GenMultiplier;
    end;

    WindModelDyn.VWind := ShapeFactor.re;
    if (ShapeFactor.re > GenVars.VCutout) or (ShapeFactor.re < GenVars.VCutin) then
    begin
        GenVars.Pnominalperphase := 0.001 * kWBase;
        GenVars.Qnominalperphase := 0.0;
        GenVars.Pm := 0.0;
        GenVars.Pg := 0.0;
        GenVars.Ps := 0.0;
        GenVars.Pr := 0.0;
        GenVars.s := 0.0;
    end
    else
    begin
        if not (ActiveCircuit.Solution.IsDynamicModel or ActiveCircuit.Solution.IsHarmonicModel) then
        begin
            // start by getting the losses from the provided curve (if any)
            if Loss_CurveObj <> NIL then
                myLosses := Loss_CurveObj.GetYValue(WindModelDyn.vwind)
            else
                myLosses := 0.0; // no losses given that the curve was not provided

            LeadLag := 1;
            with GenVars do
            begin
                Pm := 0.5 * pd * PI * SQR(Rad) * math.Power(Shapefactor.re, 3) * Cp;
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
                        kvarCalc := sqrt(SQR(Pg / Abs(PFNominal)) - SQR(Pg));
                        kVATmp := sqrt(SQR(Pg) + SQR(kvarCalc));

                        if kVATmp > KVARating then // Check saturation
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
                                VMagTmp := cabs(ActiveCircuit.Solution.NodeV[NodeRef[i]]);
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
        Yeq := Cmplx(GenVars.Pnominalperphase, -GenVars.Qnominalperphase) / Sqr(Vbase); // Vbase must be L-N for 3-phase
        if (Vminpu <> 0.0) then
            Yeq95 := Yeq / sqr(Vminpu)  // at 95% voltage
        else
            Yeq95 := Yeq; // Always a constant Z model

        if (Vmaxpu <> 0.0) then
            Yeq105 := Yeq / Sqr(Vmaxpu)   // at 105% voltage
        else
            Yeq105 := Yeq;
    end;

    // If WindGen state changes, force re-calc of Y matrix
    if GenON <> GenON_Saved then
        SetYprimInvalid(true);
end;

procedure TWindGenObj.RecalcElementData();
begin
    VBase95 := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    varBase := 1000.0 * kvarBase / Fnphases;

    // Populate data structures used for interchange with user-written models.
    with GenVars do
    begin
        // Xdp := puXdp * 1000.0 * SQR(kVWindGenBase) / kVArating;
        // Xdpp := puXdpp * 1000.0 * SQR(kVWindGenBase) / kVArating;
        Conn := ord(connection);
        NumPhases := Fnphases;
        NumConductors := FNConds;

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
    // GenVars.Vtarget := Vpu * 1000.0 * GenVars.kVWindGenBase;
    // GenVars.Vtarget := 1000.0 * GenVars.kVWindGenBase;

    // if Fnphases > 1 then
    //     GenVars.VTarget := GenVars.VTarget / SQRT3;

    // Initialize to Zero - defaults to PQ WindGen
    // Solution object will reset after circuit modifications

    Reallocmem(InjCurrent, SizeOf(Complex) * Yorder);

    WindModelDyn.ReCalcElementData();
end;

procedure TWindGenObj.CalcYPrimMatrix(Ymatrix: TcMatrix);
var
    Y, Yij: Complex;
    i, j: Integer;
    FreqMultiplier: Double;
    WTGZLV: Double;
begin
    FYprimFreq := ActiveCircuit.Solution.Frequency();
    FreqMultiplier := FYprimFreq / BaseFrequency;

    if ActiveCircuit.Solution.IsDynamicModel or ActiveCircuit.Solution.IsHarmonicModel then
    begin
        if GenON then
        begin
            WTGZLV := sqr(GenVars.kVWindGenBase) * 1e3 / GenVars.kVArating;
            Y := Cmplx(EPSILON, -WindModelDyn.N_WTG / (WindModelDyn.ZThev.im * WTGZLV)) //Yeq  // L-N value computed in initial condition routines
        end
        else
            Y := EPSILON;

        if Connection = TGeneralConnection.Delta then
            Y := Y / 3.0; // Convert to delta impedance
        Y.im := Y.im / FreqMultiplier;
        Yij := -Y;
        for i := 1 to Fnphases do
        begin
            case Connection of
                TGeneralConnection.Wye:
                begin
                    Ymatrix[i, i] := Y;
                    Ymatrix.AddElement(FNConds, FNConds, Y);
                    Ymatrix[i, FNConds] := Yij;
                    Ymatrix[FNConds, i] := Yij;
                end;
                TGeneralConnection.Delta:
                begin
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

        Exit;
    end;

    //  Regular power flow WindGen model
    // Yeq is always expected as the equivalent line-neutral admittance
    Y := -Yeq; // negate for generation    Yeq is L-N quantity
    // ****** Need to modify the base admittance for real harmonics calcs
    Y.im := Y.im / FreqMultiplier;

    case Connection of
        TGeneralConnection.Wye:
            begin
                Yij := -Y;
                for i := 1 to Fnphases do
                begin
                    YMatrix[i, i] := Y;
                    YMatrix.AddElement(FNConds, FNConds, Y);
                    YMatrix[i, FNConds] := Yij;
                    YMatrix[FNConds, i] := Yij;
                end;
            end;
        TGeneralConnection.Delta:
            begin
                Y := Y / 3.0; // Convert to delta impedance
                Yij := -Y;
                for i := 1 to Fnphases do
                begin
                    j := i + 1;
                    if j > FNConds then
                        j := 1; // wrap around for closed connections
                    YMatrix.AddElement(i, i, Y);
                    YMatrix.AddElement(j, j, Y);
                    YMatrix.AddElemSym(i, j, Yij);
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
    if YprimInvalid() then
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
        Yprim_Series.SetElement(i, i, Yprim_Shunt.GetElement(i, i) * 1.0e-10);

    YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
    inherited CalcYPrim();
end;

procedure TWindGenObj.WriteTraceRecord(const s: String);
var
    i: Integer;
    sout: String;
begin
    if DSS.InShowResults then
        Exit;

    try
        WriteStr(sout, Format('%-.g, %d, %-.g, ',
            [ActiveCircuit.Solution.DynaVars.t + ActiveCircuit.Solution.Dynavars.IntHour * 3600.0,
            ActiveCircuit.Solution.Iteration,
            ActiveCircuit.LoadMultiplier()]),
            DSS.SolveModeEnum.OrdinalToString(ord(DSS.ActiveCircuit.Solution.Mode())), ', ',
            DSS.DefaultLoadModelEnum.OrdinalToString(DSS.ActiveCircuit.Solution.LoadModel), ', ',
            GenModel: 0, ', ',
            0.0: 10: 4, ', ',
            (V_Avg * 0.001732 / GenVars.kVWindGenBase): 10: 5, ', ',
            ((1000.0 * 1.0 * GenVars.kVWindGenBase / SQRT3) - V_Avg): 9: 1, ', ', // first term was Vtarget
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
        WriteStr(sout, GenVars.VThevMag: 8: 1, ', ', GenVars.Theta * 180.0 / PI);
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
    CalcYPrimContribution(InjCurrent); // Init InjCurrent Array
    // for i := 1 to FNConds do
    //     InjCurrent[i] := 0;

    ZeroITerminal();

    CalcVTerminalPhase(); // get actual voltage across each phase of the load

    for i := 1 to Fnphases do
    begin
        V := Vterminal[i];
        VMag := Cabs(V);

        case Connection of
            TGeneralConnection.Wye:
            begin
                if VMag <= VBase95 then
                    Curr := Yeq95 * V  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Yeq105 * V  // above 105% use an impedance model
                else
                    Curr := cong(Cmplx(GenVars.Pnominalperphase, GenVars.Qnominalperphase) / V); // Between 95% -105%, constant PQ
            end;
            TGeneralConnection.Delta:
            begin
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
                    Curr := cong(Cmplx(GenVars.Pnominalperphase, GenVars.Qnominalperphase) / V); // Between 95% -105%, constant PQ
            end;
        end;

        StickCurrInTerminalArray(ITerminal, -Curr, i); // Put into Terminal array taking into account connection
        SetITerminalUpdated(TRUE);
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
    if Connection = TGeneralConnection.Wye then
        Yeq2 := Yeq
    else
        Yeq2 := Yeq / 3.0;

    for i := 1 to Fnphases do
    begin
        Curr := Yeq2 * Vterminal[i]; // Yeq is always line to neutral

        StickCurrInTerminalArray(ITerminal, -Curr, i); // Put into Terminal array taking into account connection
        SetITerminalUpdated(TRUE);
        StickCurrInTerminalArray(InjCurrent, Curr, i); // Put into Terminal array taking into account connection
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
            TGeneralConnection.Wye:
            begin
                if VMag <= VBase95 then
                    Curr := Cmplx(Yeq95.re, YQfixed) * V  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmplx(Yeq105.re, YQfixed) * V  // above 105% use an impedance model
                else
                    Curr := cong(Cmplx(GenVars.Pnominalperphase, varBase) / V);
            end;
            TGeneralConnection.Delta:
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
                    Curr := cong(Cmplx(GenVars.Pnominalperphase, varBase) / V);
            end;
        end;

        StickCurrInTerminalArray(ITerminal, -Curr, i); // Put into Terminal array taking into account connection
        SetITerminalUpdated(true);
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
            TGeneralConnection.Wye:
            begin
                if Vmag <= VBase95 then
                    Curr := Cmplx(Yeq95.re, YQfixed) * V  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmplx(Yeq105.re, YQfixed) * V
                else
                begin
                    Curr := cong(GenVars.Pnominalperphase / V); // P component of current
                    Curr += Cmplx(0.0, YQFixed) * V; // add in Q component of current
                end;
            end;
            TGeneralConnection.Delta:
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
                    Curr := cong(GenVars.Pnominalperphase / V); // P component of current
                    Curr += Cmplx(0.0, YQFixed / 3.0) * V; // add in Q component of current
                end;
            end;
        end;


        StickCurrInTerminalArray(ITerminal, -Curr, i); // Put into Terminal array taking into account connection
        SetITerminalUpdated(TRUE);
        StickCurrInTerminalArray(InjCurrent, Curr, i); // Put into Terminal array taking into account connection
    end;
end;

procedure TWindGenObj.DoDynamicMode();
// Compute Total Current and add into InjTemp
var
    i: Integer;
begin
    //CalcYPrimContribution(InjCurrent); // Init InjCurrent Array  and computes VTerminal L-N
    ComputeVTerminal();
    // for i := 1 to FNConds do
    //     InjCurrent[i] := 0;

    // NOTE: while Generator does:
    //
    // Inj = -Itotal (in) - Yprim*Vtemp
    //
    // WindGen does only:
    //
    // Inj = -Itotal (in)
    //
    // As such, no need to clear previous values since InjCurrent elements are 
    // replaced (no sum or subtraction involved).

    WindModelDyn.CalcDynamic(Vterminal, Iterminal);

    SetITerminalUpdated(TRUE);

    // Add it into inj current array
    for i := 1 to FNConds do
        InjCurrent[i] := -Iterminal[i];
end;

procedure TWindGenObj.DoHarmonicMode();
// Compute Injection Current Only when in harmonics mode
// 
// Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built
// Vd is the fundamental frequency voltage behind Xd" for phase 1
// var
//     i: Integer;
//     E: Complex;
//     GenHarmonic: Double;
//     pBuffer: PCBuffer24;
begin
    DSS.SetSolutionAbort(true);
    DoSimpleMsg('%s: WindGen harmonics model is not fully implemented. Please use the Generator model instead.', [FullName()], 5674);

    // pBuffer := @TWindGen(ParentClass).cBuffer;
    // ComputeVterminal();

    // GenHarmonic := ActiveCircuit.Solution.Frequency() / GenFundamental;
    // E := SpectrumObj.GetMult(GenHarmonic) * GenVars.VThevHarm; // Get base harmonic magnitude
    // RotatePhasorRad(E, GenHarmonic, GenVars.ThetaHarm); // Time shift by fundamental frequency phase shift
    // for i := 1 to Fnphases do
    // begin
    //     pBuffer[i] := E;
    //     if i < Fnphases then
    //         RotatePhasorDeg(E, GenHarmonic, -120.0); // Assume 3-phase WindGen
    // end;

    // // Handle Wye Connection
    // if Connection = TGeneralConnection.Wye then
    //     pBuffer[FNConds] := Vterminal[FNConds]; // assume no neutral injection voltage

    // // Inj currents = Yprim (E)
    // YPrim.MVMult(InjCurrent, pComplexArray(pBuffer));
end;

procedure TWindGenObj.CalcGenModelContribution();
// Calculates WindGen current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)
begin
    SetITerminalUpdated(FALSE);
    if ActiveCircuit.Solution.IsDynamicModel then
    begin
        DoDynamicMode();
        Exit;
    end;

    if ActiveCircuit.Solution.IsHarmonicModel and (ActiveCircuit.Solution.Frequency() <> ActiveCircuit.Fundamental) then
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
        4:
            DoFixedQGen();
        5:
            DoFixedQZGen();
    else
        DoConstantPQGen(); // for now, until we implement the other models.
    end;
    // When this is done, ITerminal is up to date
end;

procedure TWindGenObj.GetTerminalCurrents(Curr: pComplexArray);
// Compute total Currents
begin
    if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
    begin     // recalc the contribution
        if not GenSwitchOpen then
            CalcGenModelContribution(); // Adds totals in Iterminal as a side effect
    end
    else // TODO: BUG: there is no else in Generator, Load, PVsystem
        inherited GetTerminalCurrents(Curr);

    if (WindModelDyn.DebugTrace) then
        WriteTraceRecord('TotalCurrent');
end;

function TWindGenObj.InjCurrents(): Integer;
begin
    if ActiveCircuit.Solution.LoadsNeedUpdating then
        SetNominalGeneration(); // Set the nominal kW, etc for the type of solution being done

    // Difference between currents in YPrim and total terminal current
    if GenSwitchOpen then
        ZeroInjCurrent()
    else
        CalcGenModelContribution();

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
            Registers[Reg] += 0.5 * Interval * (Deriv + Derivatives[Reg]);
    end
    else 
        // Plain Euler integration
        Registers[Reg] += Interval * Deriv;

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
    if not FEnabled then
        Exit;

    IntervalHrs := ActiveCircuit.Solution.IntervalHrs;
    if GenON then
    begin
        S := cmplx(GenVars.Pnominalperphase * 0.001 * Fnphases, GenVars.Qnominalperphase * 0.001 * Fnphases);
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

procedure TWindGenObj.InitHarmonics();
// var
//     E, Va: complex;
//     NodeV: pNodeVarray;
begin
    DSS.SetSolutionAbort(true);
    DoSimpleMsg('%s: WindGen harmonics model is not fully implemented. Please use the Generator model instead.', [FullName()], 5673);

    // SetYprimInvalid(true); // Force rebuild of YPrims
    // GenFundamental := ActiveCircuit.Solution.Frequency(); // Whatever the frequency is when we enter here.

    // with GenVars do
    // begin
    //     Yeq := Cinv(Cmplx(0.0, Xdpp)); // used for current calcs  Always L-N

    //     // Compute reference Thevinen voltage from phase 1 current
    //     if not GenON then
    //     begin
    //         Vthevharm := 0.0;
    //         ThetaHarm := 0.0;
    //         Exit;
    //     end;

    //     ComputeIterminal(); // Get present value of current
    //     NodeV := ActiveCircuit.Solution.NodeV;
    //     case Connection of
    //         TGeneralConnection.Wye:// wye - neutral is explicit
    //             Va := NodeV[NodeRef[1]] - NodeV[NodeRef[FNConds]];
    //         TGeneralConnection.Delta:// delta -- assume neutral is at zero
    //             Va := NodeV[NodeRef[1]];
    //     end;

    //     E := Va - Iterminal[1] * cmplx(0.0, Xdpp);
    //     Vthevharm := Cabs(E); // establish base mag and angle
    //     ThetaHarm := Cang(E);
    // end;

end;

procedure TWindGenObj.InitStateVars();
var
    i, NumData: Integer;
    V012,
    I012: array[0..2] of Complex;
    Vabc: array[1..3] of Complex;
    NodeV: pNodeVarray;
begin
    NodeV := ActiveCircuit.Solution.NodeV;

    SetYprimInvalid(true); // Force rebuild of YPrims
    with GenVars do
    begin
        Yeq := Cinv(WindModelDyn.Zthev);

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

        ComputeIterminal();

        case Fnphases of
            1:
            begin
                Edp := NodeV[NodeRef[1]] - NodeV[NodeRef[2]] - ITerminal[1] * WindModelDyn.Zthev;
                VThevMag := Cabs(Edp);
            end;

            3:
            begin
                // Calculate Edp based on Pos Seq only
                Phase2SymComp(ITerminal, pComplexArray(@I012));
                // Voltage behind Xdp  (transient reactance), volts

                for i := 1 to FNphases do
                    Vabc[i] := NodeV[NodeRef[i]]; // Wye Voltage

                Phase2SymComp(pComplexArray(@Vabc), pComplexArray(@V012));
                Edp := V012[1] - I012[1] * WindModelDyn.Zthev; // Pos sequence
                VThevMag := Cabs(Edp);
            end;
        else
            DoSimpleMsg('Dynamics mode is implemented only for 1- or 3-phase WindGens. %s has %d phases.', [FullName(), Fnphases], 5672);
            DSS.SetSolutionAbort(true);
        end;

        if DynamicEqObj = nil then
        begin
            // Shaft variables
            // Theta is angle on Vthev[1] relative to system reference
            Theta := Cang(Edp);
            dTheta := 0.0;
            w0 := Twopi * ActiveCircuit.Solution.Frequency();
            // recalc Mmass and D in case the frequency has changed
            GenVars.Mmass := 2.0 * GenVars.Hmass * GenVars.kVArating * 1000.0 / w0; // M = W-sec
            GenVars.D := GenVars.Dpu * GenVars.kVArating * 1000.0 / w0;
            Pshaft := -Power(1).re; // Initialize Pshaft to present power Output

            Speed := 0.0; // relative to synch speed
            dSpeed := 0.0;

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
                end
                else
                    DynamicEqVals[DynamicEqPair[i * 2]][0] := PCEValue(1, DynamicEqPair[(i * 2) + 1]);
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

    with GenVars do
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

            WindModelDyn.Integrate();
            Exit;
        end;
    
    with GenVars do
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
                    DynamicEqVals[DynamicEqPair[i * 2]][0] := PCEValue(1, DynamicEqPair[(i * 2) + 1]);
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

function TWindGenObj.GetVariable(i: Integer): Double;
// Return variables one at a time
begin
    Result := -9999.99; // error return value
    if i < 1 then
    begin
        DoSimpleMsg('%s: invalid variable index %d.', [FullName(), i], 565);
        Exit;
    end;
    if DynamicEqObj <> NIL then
    begin
        if i <= DynamicEqObj.NVariables * Length(DynamicEqVals[0]) then
            Result := DynamicEqObj.Get_DynamicEqVal(i - 1, DynamicEqVals)
        else
            DoSimpleMsg('%s: invalid variable index %d.', [FullName(), i], 565);
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
                Result := GenVars.Pg;
            ord(TVar.Ps):
                Result := GenVars.Ps;
            ord(TVar.Pr):
                Result := GenVars.Pr;
            ord(TVar.s):
                Result := GenVars.s;
        end;
        Exit;
    end;
end;

procedure TWindGenObj.SetVariable(i: Integer; Value: Double);
begin
    if i < 1 then
    begin
        DoSimpleMsg('%s: invalid variable index %d.', [FullName(), i], 565);
        Exit; // No variables to set
    end;
    if DynamicEqObj <> NIL then
    begin
        DoSimpleMsg('%s: cannot set state variable when using DynamicEq.', [FullName()], 566);
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
            GenVars.Pg := Value;
        ord(TVar.Ps):
            GenVars.Ps := Value;
        ord(TVar.Pr):
            GenVars.Pr := Value;
        ord(TVar.s):
            GenVars.s := Value;
    end;

    if i <= NumWGenVariables then
        Exit;
end;

procedure TWindGenObj.GetAllVariables(var States: ArrayOfDouble);
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
        States[i - 1] := GetVariable(i);
end;

function TWindGenObj.NumVariables(): Integer;
begin
    // Try DynamicExp first
    Result := inherited NumVariables();
    if Result <> 0 then 
        Exit;

    // Fallback to the classic
    Result := NumWGenVariables;
end;

function TWindGenObj.VariableName(i: Integer): String;
begin
    Result := 'ERROR';
    if i < 1 then
        Exit; // Someone goofed

    // Try DynamicExp first
    Result := inherited VariableName(i);
    if Length(Result) <> 0 then
        Exit;

    // Fallback to the classic
    if (i > 0) and (i <= NumWGenVariables) then
    begin
        Result := TWindGen(ParentClass).varNames[i - 1];
        Exit;
    end;
end;

procedure TWindGenObj.MakePosSequence();
var
    V: Double;
    had_kVA, had_MVA: Boolean;
    kW_new, PF_new, new_kVA, new_MVA: Double;
    oldPhases, changes: Integer;
begin
    // Make sure voltage is line-neutral
    if (Fnphases > 1) or (connection <> TGeneralConnection.Wye) then
        V := GenVars.kVWindGenBase / SQRT3
    else
        V := GenVars.kVWindGenBase;

    // Divide the load by no. phases
    changes := 3;
    oldPhases := Fnphases;
    if Fnphases > 1 then
    begin
        had_kVA := PrpSequence[ord(TProp.kVA)] <> 0;
        had_MVA := PrpSequence[ord(TProp.MVA)] <> 0;
        kW_new := kWbase / Fnphases;
        PF_new := PFNominal;
        if had_kVA then
        begin
            new_kVA := GenVars.kvarating / Fnphases;
            Inc(changes);
        end;
        if had_MVA then
        begin
            new_MVA := GenVars.kvarating / 1000.0 / Fnphases;
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
        // if had_kvars then
        // begin
        //     SetDouble(ord(TProp.minkvar), new_minkvar, []);
        //     SetDouble(ord(TProp.maxkvar), new_maxkvar, []);
        // end;
        if had_kVA then
            SetDouble(ord(TProp.kVA), new_kVA, []);
        if had_MVA then
            SetDouble(ord(TProp.MVA), new_MVA, []);
    end;
    EndEdit(changes);

    inherited;
end;

procedure TWindGenObj.SetConductorClosed(Index: Integer; Value: Boolean);
begin
    inherited;
    GenSwitchOpen := not Value; // Just turn WindGen on or off;
end;

procedure TWindGenObj.SyncUpPowerQuantities();
begin
    // keep kvar nominal up to date with kW and PF
    if (PFNominal <> 0.0) then
    begin
        kvarBase := kWBase * sqrt(1.0 / Sqr(PFNominal) - 1.0);
        GenVars.Qnominalperphase := 1000.0 * kvarBase / Fnphases;
        if PFNominal < 0.0 then
            kvarBase := -kvarBase;

        if kVANotSet then
            GenVars.kVARating := kWBase * 1.2;
    end;
end;

procedure TWindGenObj.SetDragHandRegister(Reg: Integer;
    const Value: Double);
begin
    if Value > Registers[reg] then
        Registers[Reg] := Value;
end;

finalization
    WindGenModelEnum.Free;
    WindGenQModeEnum.Free;
end.
