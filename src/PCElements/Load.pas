unit Load;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

//  The load is assumed balanced over the no. of phases defined
// To model unbalanced loads, define separate single-phase loads

// IF you do not specify load shapes defaults are:
//    Yearly:  Defaults to No variation or Daily when Daily is defined
//    Daily:   Defaults to No variation  (i.e. multiplier = 1.0 always)
//    Dutycycle: Defaults to Daily shape
//    Growth: Circuit default growth factor

// Change Log
// 4/1/14 Added Vlowpu property to make solution converge better at very low voltages
// 1/7/15 Added puXHarm and XRHarm properties to help model motor load for harmonic studies
// 3/16/16 Added PFSpecified to account for problems when UseActual is specified and no Qmult specified
// 1/10/18 Celso/Paulo mods for low-voltage transition for Model 5

interface

uses
    Classes,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    UComplex, DSSUcomplex,
    LoadShape,
    GrowthShape,
    Spectrum,
    ArrayDef;

type
{$SCOPEDENUMS ON}
{$PUSH}
{$Z4} // keep enums as int32 values
    TLoadModel = (
        // INVALID = 0,
        ConstPQ = 1, // Constant kVA (P,Q always in same ratio)
        ConstZ = 2, // Constant impedance
        Motor = 3, // Constant P, Quadratic Q (Mostly motor)
        CVR = 4, // Linear P, Quadratic Q  (Mixed motor/resistive Use this for CVR studies
        ConstI = 5, // Constant |I|
        ConstPFixedQ = 6, // Constant P (Variable); Q is fixed value (not variable)
        ConstPFixedX = 7, // Constant P (Variable); Q is fixed Z (not variable)
        ZIPV = 8 // ZIPV (3 real power coefficients, 3 reactive, Vcutoff)
    );

    TLoadStatus = (
        Variable = 0,
        Fixed,
        Exempt
    );
{$SCOPEDENUMS OFF}
{$POP}
    TLoadConnection = TGeneralConnection;
        
    TLoadSpec = (
        kW_PF = 0,
        kW_kvar = 1,
        kVA_PF = 2,
        ConnectedkVA_PF = 3,
        kWh_PF = 4
    );

    TLoadProp = (
        INVALID = 0,
        phases = 1,
        bus1 = 2,
        kV = 3,
        kW = 4,
        pf = 5,
        model = 6,
        yearly = 7,
        daily = 8,
        duty = 9, 
        growth = 10, 
        conn = 11, 
        kvar = 12, 
        Rneut = 13, // IF entered -, assume open
        Xneut = 14, 
        status = 15, // fixed or variable
        cls = 16, // integer
        Vminpu = 17, // Min pu voltage for which model applies
        Vmaxpu = 18, // Max pu voltage for which model applies
        Vminnorm = 19, // Min pu voltage normal load
        Vminemerg = 20, // Min pu voltage emergency rating
        xfkVA = 21, // Service transformer rated kVA
        allocationfactor = 22, // allocation factor  for xfkVA
        kVA = 23, // specify load in kVA and PF
        pctmean = 24, // per cent default mean
        pctstddev = 25, // per cent default standard deviation
        CVRwatts = 26, // Percent watts reduction per 1% reduction in voltage from nominal
        CVRvars = 27, // Percent vars reduction per 1% reduction in voltage from nominal
        kwh = 28, // kwh billing
        kwhdays = 29, // kwh billing period (24-hr days)
        Cfactor = 30, // multiplier from kWh avg to peak kW
        CVRcurve = 31, // name of curve to use for yearly CVR simulations
        NumCust = 32, // Number of customers, this load
        ZIPV = 33, // array of 7 coefficients
        pctSeriesRL = 34, // pct of Load that is series R-L
        RelWeight = 35, // Weighting factor for reliability
        Vlowpu = 36, // Below this value resort to constant Z model = Yeq
        puXharm = 37, // pu Reactance for Harmonics, if specifies
        XRharm = 38 // X/R at fundamental for series R-L model for hamonics
    );
{$SCOPEDENUMS OFF}
    TLoad = class(TPCClass)
    PROTECTED
        procedure DefineProperties; override;  // Add Properties of this class to propName
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TLoadObj = class(TPCElement)
    PUBLIC
        FAllocationFactor: Double;   // For all types of allocation
        FkVAAllocationFactor: Double;   // for connected kVA specification
        FConnectedkVA: Double;
        FkWh: Double;
        FkWhDays: Double;
        FCFactor: Double;   // For kWh billed spec
        FAvgkW: Double;
        FPhaseCurr: pComplexArray; // this is the intermediate current computed in each power flow mode.
        HarmAng: pDoubleArray;  // References for Harmonics mode
        HarmMag: pDoubleArray;
        LastGrowthFactor: Double;
        LastYear: Integer;   // added FOR speedup so we don't have to search FOR growth factor a lot
        LoadFundamental: Double;
        LoadSolutionCount: Integer;
        OpenLoadSolutionCount: Integer;
        RandomMult: Double;
        ShapeFactor: Complex;
        varBase: Double;  // Base vars per phase
        varNominal: Double;
        VBase: Double;  // Base volts suitable for computing currents
        VBase105: Double;
        VBase95: Double;
        VBaseLow: Double;
        WNominal: Double;  // Nominal Watts per phase
        Yeq: Complex;   // at nominal
        Yeq105: Complex;
        Yeq105I: Complex; // ***Added by Celso & Paulo
        Yeq95: Complex;
        Yneut: Complex;
        YPrimOpenCond: TCmatrix;  // To handle cases where one conductor of load is open
        YQFixed: Double;   // Fixed value of y FOR type 7 load
        FpuXHarm: Double;   // puX for harmonics solution.
        FXRHarmRatio: Double;   // X/R at fundamental

        // formerly private, now read-only properties for COM access
        FpuMean: Double;
        FpuStdDev: Double;
        FCVRwattFactor: Double;
        FCVRvarFactor: Double;
        Vmaxpu: Double;
        VminEmerg: Double;  // overrides system settings IF <> 0.0
        VminNormal: Double;
        Vminpu: Double;
        VLowpu: Double; // below this voltage, resorts to linear @ Yeq

        // For interpolating currents between VbaseLow and Vbase95
        ILow: Complex;
        I95: Complex;
        IBase: Complex; // at nominal  ***Added by Celso & Paulo
        M95: Complex; // complex slope of line between Low and 95
        M95I: Complex; // complex slope of line between Low and 95 for Constant I  **Added by Celso & Paulo

        status: TLoadStatus;
        ShapeIsActual: Boolean;
        PFSpecified: Boolean;  // Added 3-16-16 to fix problem with UseActual
        PFChanged: Boolean;

        function AllTerminalsClosed: Boolean;
        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);
        procedure CalcInjCurrentArray;
        procedure CalcLoadModelContribution;
        procedure CalcVTerminalPhase;
        procedure CalcYearlyMult(Hr: Double);
        procedure CalcCVRMult(Hr: Double);
        procedure CalcYPrimMatrix(Ymatrix: TcMatrix);
        procedure DoConstantILoad;
        procedure DoConstantPQLoad;
        procedure DoConstantZLoad;
        procedure DoFixedQ;
        procedure DoFixedQZ;
        procedure DoHarmonicMode;
        procedure DoCVRModel;
        procedure DoZIPVModel;
        procedure DoMotorTypeLoad;
        function GrowthFactor(Year: Integer): Double;
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer); inline;
        function InterpolateY95_YLow(const Vmag: Double): Complex; inline;
        function InterpolateY95I_YLow(const Vmag: Double): Complex; inline; // ***Added by Celso & Paulo
        function Get_Unserved: Boolean;
        function Get_ExceedsNormal: Boolean;
        procedure Set_kVAAllocationFactor(const Value: Double);
        procedure ComputeAllocatedLoad;
        // Set kWh properties ...
        procedure Set_CFactor(const Value: Double);
        procedure Set_AllocationFactor(const Value: Double);
        procedure SetkWkvar(const PkW, Qkvar: Double);


    PROTECTED
        procedure GetTerminalCurrents(Curr: pComplexArray); OVERRIDE;

    PUBLIC

        Connection: TLoadConnection; 
        DailyShapeObj: TLoadShapeObj;  // Daily load Shape FOR this load
        DutyShapeObj: TLoadShapeObj;  // Shape for this load
        EEN_Factor: Double;         // is overloaded  Factor is the amount of overload
        GrowthShapeObj: TGrowthShapeObj;  // Shape for this Growth  Curve
        HasBeenAllocated: Boolean;
        kWBase: Double;
        kVABase: Double;
        kWref: Double;
        kVARref: Double;
        kvarBase: Double;
        kVLoadBase: Double;
        LoadClass: Integer;
        NumCustomers: Integer;
        LoadSpecType: TLoadSpec;  // 0=kW, PF;  1= kw, kvar;  2=kva, PF
        PFNominal: Double;
        Rneut: Double;
        UE_Factor: Double;  // These are set to > 0 IF a line in the critical path
        Xneut: Double;  // Neutral impedance
        YearlyShapeObj: TLoadShapeObj;  // Shape for this load
        CVRShapeObj: TLoadShapeObj;
        ZIPV: Array[1..7] of Double;  // Made public 5-20-2013
        ZIPVset: Boolean;
        puSeriesRL: Double;
        RelWeighting: Double;

        FLoadModel: TLoadModel;   // Variation with voltage

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;
        function InjCurrents: Integer; OVERRIDE;
        procedure InitHarmonics; OVERRIDE;
        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        procedure SetNominalLoad;
        procedure Randomize(Opt: Integer);
                  // 0 = reset to 1.0
                  // 1 = Gaussian around mean and std Dev
                  // 2 = uniform

        property Unserved: Boolean READ Get_Unserved;
        property ExceedsNormal: Boolean READ Get_ExceedsNormal;

        // AllocationFactor adjusts either connected kVA allocation factor or kWh CFactor
        property AllocationFactor: Double READ FAllocationFactor WRITE Set_AllocationFactor;

        // Allocate load from connected kva or kWh billing
        
        //TODO: remove these properties, use plain DSS properties 
        // instead to ease the code transition?
        property kVAAllocationFactor: Double READ FkVAAllocationFactor WRITE Set_kVAAllocationFactor;
        property ConnectedkVA: Double READ FConnectedkVA;
        property kWh: Double READ FkWh;// WRITE Set_kWh;
        property kWhDays: Double READ FkWhDays;
        property CFactor: Double READ FCFactor WRITE Set_CFactor;

        property puMean: Double READ FpuMean;
        property puStdDev: Double READ FpuStdDev;
        property CVRwatts: Double READ FCVRwattFactor;
        property CVRvars: Double READ FCVRvarFactor;
        property MaxPU: Double READ Vmaxpu;
        property MinEmerg: Double READ VminEmerg;
        property MinNormal: Double READ VminNormal;
        property MinPU: Double READ Vminpu;
        Property IsPFSpecified: Boolean read PFSpecified;
    const
        nZIPV = 7;
    end;

implementation

uses
    Circuit,
    DSSClassDefs,
    DSSGlobals,
    Dynamics,
    Sysutils,
    Command,
    Math,
    MathUtil,
    Utilities,
    TypInfo,
    DSSHelper,
    DSSObjectHelper;

type
    TObj = TLoadObj;
    TProp = TLoadProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    
    LoadStatusEnum, LoadModelEnum: TDSSEnum;

constructor TLoad.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        LoadModelEnum := TDSSEnum.Create('Load: Model', True, 0, 0, [
            'Constant PQ', 'Constant Z', 'Motor (constant P, quadratic Q)', 'CVR (linear P, quadratic Q)', 
            'Constant I', 'Constant P, fixed Q', 'Constant P, fixed X', 'ZIPV'], 
            [1, 2, 3, 4, 5, 6, 7, 8]);
        LoadStatusEnum := TDSSEnum.Create('Load: Status', True, 1, 1, 
            ['Variable', 'Fixed', 'Exempt'], [0, 1, 2]);
        LoadStatusEnum.DefaultValue := 0;
    end;

    inherited Create(dssContext, LOAD_ELEMENT, 'Load');
end;

destructor TLoad.Destroy;
begin
    inherited Destroy;
end;

procedure TLoad.DefineProperties;
type
    P = TProp;
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

    PropertyType[ord(TProp.model)] := TPropertyType.MappedIntEnumProperty;
    PropertyOffset[ord(TProp.model)] := ptruint(@obj.FLoadModel);
    PropertyOffset2[ord(TProp.model)] := PtrInt(LoadModelEnum);

    PropertyType[ord(TProp.status)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.status)] := ptruint(@obj.status);
    PropertyOffset2[ord(TProp.status)] := PtrInt(LoadStatusEnum);

    // array property    
    PropertyType[ord(TProp.ZIPV)] := TPropertyType.DoubleFArrayProperty;
    PropertyOffset[ord(TProp.ZIPV)] := ptruint(@obj.ZIPV[1]);
    PropertyOffset2[ord(TProp.ZIPV)] := 7;

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;

    // pct properties
    PropertyScale[ord(TProp.pctmean)] := 0.01;
    PropertyScale[ord(TProp.pctstddev)] := 0.01;
    PropertyScale[ord(TProp.pctSeriesRL)] := 0.01;
    PropertyOffset[ord(TProp.pctmean)] := ptruint(@obj.FpuMean);
    PropertyOffset[ord(TProp.pctstddev)] := ptruint(@obj.FpuStdDev);
    PropertyOffset[ord(TProp.pctSeriesRL)] := ptruint(@obj.puSeriesRL);

    // integer properties
    PropertyOffset[ord(TProp.cls)] := ptruint(@obj.LoadClass);
    PropertyOffset[ord(TProp.NumCust)] := ptruint(@obj.NumCustomers);
    PropertyType[ord(TProp.cls)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.NumCust)] := TPropertyType.IntegerProperty;

    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    // object properties
    AddProperties_Object(
        [ord(P.yearly), ord(P.daily), ord(P.duty), ord(P.CVRcurve), ord(P.growth)],
        [@obj.YearlyShapeObj, @obj.DailyShapeObj, @obj.DutyShapeObj, @obj.CVRShapeObj, @obj.GrowthShapeObj],
        [DSS.LoadShapeClass, DSS.LoadShapeClass, DSS.LoadShapeClass, DSS.LoadShapeClass, DSS.GrowthShapeClass]
    );
    
    // double properties (default type)
    AddProperties_Double(
        [ord(P.Rneut), ord(P.Xneut), ord(P.kV), ord(P.kW), ord(P.pf), ord(P.kvar), ord(P.kVA), 
        ord(P.RelWeight), ord(P.Vlowpu), ord(P.puXharm), ord(P.XRharm), ord(P.Vminpu), ord(P.VMaxPu), ord(P.Vminnorm), 
        ord(P.Vminemerg), ord(P.CVRwatts), ord(P.CVRvars), ord(P.kwh), ord(P.xfkVA), ord(P.kwhdays), 
        ord(P.Cfactor), ord(P.allocationfactor)],
        [@obj.Rneut, @obj.Xneut, @obj.kVLoadBase, @obj.kwBase, @obj.PFNominal, @obj.kvarBase, @obj.kVABase, 
        @obj.RelWeighting, @obj.VLowpu, @obj.FpuXHarm, @obj.FXRHarmRatio, @obj.VMinPu, @obj.VMaxPu, @obj.VminNormal,
        @obj.VminEmerg, @obj.FCVRwattFactor, @obj.FCVRvarFactor, @obj.kWh, @obj.FConnectedkVA, @obj.kWhdays,
        @obj.FCFactor, @obj.FkVAAllocationFactor]
    );

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TLoad.NewObject(const ObjName: String; Activate: Boolean): Pointer;
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
            TLoadConnection.Wye:
                NConds := Fnphases + 1;
            TLoadConnection.Delta:
                case Fnphases of
                    1, 2:
                        NConds := Fnphases + 1; // L-L and Open-delta
                else
                    NConds := Fnphases;
                end;
        end;
end;

procedure TLoadObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of
        ord(TProp.conn):
        begin
            SetNCondsForConnection(self);

            case Connection of
                TLoadConnection.Delta:
                    VBase := kVLoadBase * 1000.0;
            else
                case Fnphases of
                    2, 3:
                        VBase := kVLoadBase * InvSQRT3x1000;
                else
                    VBase := kVLoadBase * 1000.0;
                end;
            end;
            VBase95 := Vminpu * VBase;
            VBase105 := Vmaxpu * VBase;
            VBaseLow := VLowpu * VBase;

            Yorder := Fnconds * Fnterms;
            Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);
            YPrimInvalid := TRUE;
        end;

        ord(TProp.kV), ord(TProp.phases):
        begin
            if Idx = ord(TProp.phases) then
            begin
                Reallocmem(FPhaseCurr, SizeOf(FPhaseCurr^[1]) * FNphases);
                SetNCondsForConnection(self);  // Force Reallocation of terminal info
            end;

            case Connection of
                TLoadConnection.Delta:
                    VBase := kVLoadBase * 1000.0;
            else  // wye
                case Fnphases of
                    2, 3:
                        VBase := kVLoadBase * InvSQRT3x1000;
                else
                    VBase := kVLoadBase * 1000.0; // 1-phase or unknown
                end;
            end;
        end;
        
        ord(TProp.kW):
        begin
            LoadSpecType := TLoadSpec.kW_PF;
            kWRef := kWBase;
        end;
        ord(TProp.pf):
        begin
            PFChanged := TRUE;
            PFSpecified := TRUE;
        end;

        ord(TProp.allocationfactor):
        begin
            FAllocationFactor := FkVAAllocationFactor;
            LoadSpecType := TLoadSpec.ConnectedkVA_PF;
            ComputeAllocatedLoad;
            HasBeenAllocated := TRUE;
        end;

        ord(TProp.Cfactor):
        begin
            FAllocationFactor := FCFactor;
            LoadSpecType := TLoadSpec.kWh_PF;
            ComputeAllocatedLoad;
            HasBeenAllocated := TRUE;
        end;

        // Set shape objects;  returns nil if not valid
        // Sets the kW and kvar properties to match the peak kW demand from the Loadshape
        ord(TProp.Yearly):
        begin
            if Assigned(YearlyShapeObj) then
                with YearlyShapeObj do
                    if UseActual then
                    begin
                        kWref := kWBase;
                        kVARref := kVARbase;
                        SetkWkvar(MaxP, MaxQ);
                    end;
        end;
        ord(TProp.daily):
        begin
            if Assigned(DailyShapeObj) then
                with DailyShapeObj do
                    if UseActual then
                        SetkWkvar(MaxP, MaxQ);
            // If Yearly load shape is not yet defined, make it the same as Daily
            if YearlyShapeObj = NIL then
                YearlyShapeObj := DailyShapeObj;
        end;
        ord(TProp.duty):
        begin
            if Assigned(DutyShapeObj) then
                with DutyShapeObj do
                    if UseActual then
                        SetkWkvar(MaxP, MaxQ);
        end;
        ord(TProp.kwh):
        begin
            LoadSpecType := TLoadSpec.kWh_PF;
            FAllocationFactor := FCFactor;
            ComputeAllocatedLoad;
        end;
        ord(TProp.kvar):
        begin
            LoadSpecType := TLoadSpec.kW_kvar;
            PFSpecified := FALSE;
            kVARref := kVARbase;
        end;// kW, kvar
        {*** see set_xfkva, etc           21, 22: LoadSpectype := 3;  // XFKVA*AllocationFactor, PF  }
        ord(TProp.xfkVA):
        begin
            LoadSpecType := TLoadSpec.ConnectedkVA_PF;
            FAllocationFactor := FkVAAllocationFactor;
            ComputeAllocatedLoad;
        end;
        ord(TProp.kwhdays):
        begin
            LoadSpecType := TLoadSpec.kWh_PF;
            ComputeAllocatedLoad;
        end;
        ord(TProp.kVA):
            LoadSpecType := TLoadSpec.kVA_PF;  // kVA, PF
            {*** see set_kwh, etc           28..30: LoadSpecType := 4;  // kWh, days, cfactor, PF }
        ord(TProp.ZIPV):
            ZIPVset := True;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TLoad.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        RecalcElementData;
        YPrimInvalid := TRUE;
        Exclude(Flags, Flg.EditionActive);
    end;
    Result := True;
end;

procedure TLoadObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr); // Take care of inherited class properties

    Other := TObj(OtherPtr);
    Connection := Other.Connection;

    if Fnphases <> Other.Fnphases then
    begin
        FNphases := Other.Fnphases;
        SetNCondsForConnection(self); // Forces reallocation of terminal stuff
        Yorder := Fnconds * Fnterms;
        YPrimInvalid := TRUE;
    end;

    kVLoadBase := Other.kVLoadBase;
    Vbase := Other.Vbase;
    VLowpu := Other.VLowpu;
    Vminpu := Other.Vminpu;
    Vmaxpu := Other.Vmaxpu;
    VBaseLow := Other.VBaseLow;
    Vbase95 := Other.Vbase95;
    Vbase105 := Other.Vbase105;
    kWBase := Other.kWBase;
    kVAbase := Other.kVABase;
    kvarBase := Other.kvarBase;
    LoadSpecType := Other.LoadSpecType;
    WNominal := Other.WNominal;
    PFNominal := Other.PFNominal;
    varNominal := Other.varNominal;
    Rneut := Other.Rneut;
    Xneut := Other.Xneut;
    CVRshapeObj := Other.CVRshapeObj;
    DailyShapeObj := Other.DailyShapeObj;
    DutyShapeObj := Other.DutyShapeObj;
    YearlyShapeObj := Other.YearlyShapeObj;
    GrowthShapeObj := Other.GrowthShapeObj;
    LoadClass := Other.LoadClass;
    NumCustomers := Other.NumCustomers;
    FLoadModel := Other.FLoadModel;
    status := Other.status;
    FkVAAllocationFactor := Other.FkVAAllocationFactor;
    FConnectedkVA := Other.FConnectedkVA;
    FCVRwattFactor := Other.FCVRwattFactor;
    FCVRvarFactor := Other.FCVRvarFactor;
    ShapeIsActual := Other.ShapeIsActual;
    puSeriesRL := Other.puSeriesRL;
    RelWeighting := Other.RelWeighting;

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);
    Reallocmem(FPhaseCurr, SizeOf(FPhaseCurr^[1]) * FNphases);

    ZIPVset := Other.ZIPVset;
    if ZIPVset then
        for i := 1 to nZIPV do
            ZIPV[i] := Other.ZIPV[i];
end;

constructor TLoadObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    if ParClass = nil then Exit;

    inherited create(ParClass);
    Name := AnsiLowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType;

    Fnphases := 3;
    Fnconds := 4;  // defaults to wye  so it has a 4th conductor
    Yorder := 0;  // To trigger an initial allocation
    Nterms := 1;  // forces allocations
    kWBase := 10.0;
    kvarBase := 5.0;
    PFNominal := 0.88;
    kVABase := kWBase / PFNominal;
    LoadSpecType := TLoadSpec.kW_PF;
    Rneut := -1.0;  // signify neutral is open
    Xneut := 0.0;

    YearlyShapeObj := NIL;  // IF YearlyShapeobj = nil THEN the load alway stays nominal * global multipliers
    DailyShapeObj := NIL;  // IF DaillyShapeobj = nil THEN the load alway stays nominal * global multipliers
    DutyShapeObj := NIL;  // IF DutyShapeobj = nil THEN the load alway stays nominal * global multipliers
    GrowthShapeObj := NIL;  // IF grwothshapeobj = nil THEN the load alway stays nominal * global multipliers
    CVRShapeObj := NIL;
    Connection := TLoadConnection.Wye;    // Wye (star)
    FLoadModel := TLoadModel.ConstPQ;  // changed from 2 RCD {easiest to solve}
    LoadClass := 1;
    NumCustomers := 1;
    LastYear := 0;
    FCVRwattFactor := 1.0;
    FCVRvarFactor := 2.0;
    RelWeighting := 1.0;

    LastGrowthFactor := 1.0;
    FkVAAllocationFactor := 0.5;
    FAllocationFactor := FkVAAllocationFactor;
    HasBeenAllocated := FALSE;
    PFChanged := FALSE;
    ShapeIsActual := FALSE;
    PFSpecified := FALSE;  // default to not specified by PF property

    LoadSolutionCount := -1;  // for keeping track of the present solution in Injcurrent calcs
    OpenLoadSolutionCount := -1;
    YPrimOpenCond := NIL;

    FConnectedkVA := 0.0;  // Loadspectype=3
    FkWh := 0.0;  // Loadspectype=4
    FCfactor := 4.0;
    FkWhDays := 30.0;
    VminNormal := 0.0;    // indicates for program to use Circuit quantities
    VminEmerg := 0.0;
    kVLoadBase := 12.47;
    VBase := 7200.0;
    VLowpu := 0.50;
    VminPu := 0.95;
    VMaxPU := 1.05;
    VBaseLow := VLowpu * Vbase;
    VBase95 := VminPu * Vbase;
    VBase105 := VMaxPU * Vbase;
    Yorder := Fnterms * Fnconds;

    RandomMult := 1.0;
    status := TLoadStatus.Variable;
    FpuXHarm := 0.0;  // zero signifies not specified.
    FXRHarmRatio := 6.0;


    FpuMean := 0.5;
    FpuStdDev := 0.1;
    UE_Factor := 0.0;
    EEN_Factor := 0.0;
    SpectrumObj := DSS.SpectrumClass.DefaultLoad; // override base class definition
    HarmMag := NIL;
    HarmAng := NIL;
    puSeriesRL := 0.50;
    FPhaseCurr := NIL;  // storage for intermediate current computation
                        // allocated in Recalcelementdata

    ZIPVset := False;

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);
    Reallocmem(FPhaseCurr, SizeOf(FPhaseCurr^[1]) * FNphases);
    RecalcElementData;
end;

destructor TLoadObj.Destroy;
begin
    if ParentClass = nil then Exit;

    YPrimOpenCond.Free;
    ReallocMem(HarmMag, 0);
    ReallocMem(HarmAng, 0);
    Reallocmem(FPhaseCurr, 0);

    inherited Destroy;
end;

procedure TLoadObj.Randomize(Opt: Integer);
begin
    case Opt of
        0:
            RandomMult := 1.0;
        GAUSSIAN:
            if Assigned(YearlyShapeObj) then
                RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev)
            else
                RandomMult := Gauss(FpuMean, FpuStdDev);
        UNIFORM:
            RandomMult := Random;  // number between 0 and 1.0
        LOGNORMAL:
            if Assigned(YearlyShapeObj) then
                RandomMult := QuasiLognormal(YearlyShapeObj.Mean)
            else
                RandomMult := QuasiLognormal(FpuMean);
    end;
end;

procedure TLoadObj.CalcDailyMult(Hr: Double);
begin
    if DailyShapeObj <> NIL then
    begin
        ShapeFactor := DailyShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := DailyShapeObj.UseActual;
    end
    else
        ShapeFactor := Cmplx(1.0, 1.0);  // Default to no daily variation
end;

procedure TLoadObj.CalcDutyMult(Hr: Double);
begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := DutyShapeObj.UseActual;
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult IF no duty curve specified
end;

procedure TLoadObj.CalcYearlyMult(Hr: Double);
begin
    // Yearly curve is assumed to be hourly only
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := YearlyShapeObj.UseActual;
    end
    else
        // Defaults to no variation
        ShapeFactor := Cmplx(1.0, 1.0);
end;

procedure TLoadObj.CalcCVRMult(Hr: Double);
var
    CVRFactor: Complex;

begin
    // CVR curve is assumed to be used in a yearly simulation
    if CVRShapeObj <> NIL then
    begin
        CVRFactor := CVRShapeObj.GetMultAtHour(Hr);    {Complex}
        FCVRWattFactor := CVRFactor.re;
        FCVRvarFactor := CVRFactor.im;
    end;
    // Else FCVRWattFactor, etc. remain unchanged
end;

function TLoadObj.GrowthFactor(Year: Integer): Double;
begin
    if Year = 0 then
        LastGrowthFactor := 1.0  // default all to 1 in year 0 ; use base values
    else
    begin
        if GrowthShapeObj = NIL then
            LastGrowthFactor := Activecircuit.DefaultGrowthFactor
        else
        if Year <> LastYear then    // Search growthcurve
            LastGrowthFactor := GrowthShapeObj.GetMult(Year);
    end;

    Result := LastGrowthFactor;  // for Now
end;

procedure TLoadObj.SetkWkvar(const PkW, Qkvar: Double);
begin
    kWBase := PkW;
    kvarbase := Qkvar;
    if PFSpecified then
        LoadSpecType := TLoadSpec.kW_PF
    else
        LoadSpecType := TLoadSpec.kW_kvar;
end;

procedure TLoadObj.SetNominalLoad;
var
    Factor: Double;
begin
    ShapeFactor := CDOUBLEONE;
    ShapeIsActual := FALSE;
    with ActiveCircuit.Solution do
        if status = TLoadStatus.Fixed then
        begin
            Factor := GrowthFactor(Year);   // For fixed loads, consider only growth factor
        end
        else
            case Mode of
                TSolveMode.SNAPSHOT,
                TSolveMode.HARMONICMODE:
                    if status = TLoadStatus.Exempt then
                        Factor := GrowthFactor(Year)
                    else
                        Factor := ActiveCircuit.LoadMultiplier * GrowthFactor(Year);
                TSolveMode.DAILYMODE:
                begin
                    Factor := GrowthFactor(Year);
                    if status <> TLoadStatus.Exempt then
                        Factor := Factor * ActiveCircuit.LoadMultiplier;
                    CalcDailyMult(DynaVars.dblHour);
                end;
                TSolveMode.YEARLYMODE:
                begin
                    Factor := ActiveCircuit.LoadMultiplier * GrowthFactor(Year);
                    CalcYearlyMult(DynaVars.dblHour);
                    if FLoadModel = TLoadModel.CVR then
                        CalcCVRMult(DynaVars.dblHour);
                end;
                TSolveMode.DUTYCYCLE:
                begin
                    Factor := GrowthFactor(Year);
                    if status <> TLoadStatus.Exempt then
                        Factor := Factor * ActiveCircuit.LoadMultiplier;
                    CalcDutyMult(DynaVars.dblHour);
                end;
                TSolveMode.GENERALTIME,
                TSolveMode.DYNAMICMODE:
                begin
                    Factor := GrowthFactor(Year);
                    if status <> TLoadStatus.Exempt then
                        Factor := Factor * ActiveCircuit.LoadMultiplier;
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
                TSolveMode.MONTECARLO1:
                begin
                    Randomize(RandomType);
                    Factor := RandomMult * GrowthFactor(Year);
                    if status <> TLoadStatus.Exempt then
                        Factor := Factor * ActiveCircuit.LoadMultiplier;
                end;

                TSolveMode.MONTECARLO2,
                TSolveMode.MONTECARLO3,
                TSolveMode.LOADDURATION1,
                TSolveMode.LOADDURATION2:
                begin
                    Factor := GrowthFactor(Year);
                    CalcDailyMult(DynaVars.dblHour);
                    if status <> TLoadStatus.Exempt then
                        Factor := Factor * ActiveCircuit.LoadMultiplier;
                end;
                TSolveMode.PEAKDAY:
                begin
                    Factor := GrowthFactor(Year);
                    CalcDailyMult(DynaVars.dblHour);
                end;
                TSolveMode.AUTOADDFLAG:
                    Factor := GrowthFactor(Year);  // Loadmult = 1.0 by default
            else
                Factor := GrowthFactor(Year)    // defaults to Base kW * growth
            end;

    if ShapeIsActual then
    begin
        WNominal := 1000.0 * ShapeFactor.re / Fnphases;
        varNominal := 0.0; // initialize  for unity PF  and check for change
        if ShapeFactor.im <> 0.0 then   // Qmult was specified
            varNominal := 1000.0 * ShapeFactor.im / Fnphases
        else
        if PFSpecified and (PFNominal <> 1.0) then  // Qmult not specified but PF was
        begin  // user specified the PF for this load
            varNominal := WNominal * SQRT((1.0 / SQR(PFNominal) - 1));
            if PFNominal < 0.0 then // watts and vare are in opposite directions
                varNominal := -varNominal;
        end;
    end
    else
    begin
        WNominal := 1000.0 * kWBase * Factor * ShapeFactor.re / Fnphases;
        varNominal := 1000.0 * kvarBase * Factor * ShapeFactor.im / Fnphases;
    end;

    Yeq := Cmplx(WNominal, -VarNominal) / Sqr(Vbase);
    if (Vminpu <> 0.0) then
        Yeq95 := Yeq / sqr(Vminpu)   // at 95% voltage
    else
        Yeq95 := CZERO;

    if (Vmaxpu <> 0.0) then
        Yeq105 := Yeq / sqr(Vmaxpu)   // at 105% voltage
    else
        Yeq105 := Yeq;

    if (Vmaxpu <> 0.0) then
        Yeq105I := Yeq / Vmaxpu   // at 105% voltage for Constant I ***Added by Celso & Paulo
    else
        Yeq105I := Yeq;                    // **Added by Celso & Paulo

    // New code to help with convergence at low voltages
    ILow := (Yeq * VbaseLow);
    I95 := (Yeq95 * Vbase95);
    M95 := (I95 - ILow) / (VBase95 - VBaseLow); // (I95 - ILow)/(Vbase95 - VbaseLow);    ***Added by Celso & Paulo
    IBase := (Yeq * VBase);                          // ***Added by Celso & Paulo
    M95I := (IBase - ILow) / (VBase95 - VBaseLow); // (IBase - ILow)/(Vbase95 - VbaseLow);    ***Added by Celso & Paulo
end;

procedure TLoadObj.RecalcElementData;
begin
    VBaseLow := VLowpu * VBase;
    VBase95 := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    // Set kW and kvar from root values of kVA and PF

    case LoadSpecType of
        TLoadSpec.kW_PF:
        begin
            kvarBase := kWBase * SQRT(1.0 / SQR(PFNominal) - 1.0);
            if PFNominal < 0.0 then
                kvarBase := -kvarBase;
            kVABase := SQRT(SQR(kWbase) + SQR(kvarBase));
        end;
        TLoadSpec.kW_kvar:
        begin  // need to set PFNominal
            kVABase := SQRT(SQR(kWbase) + SQR(kvarBase));
            if kVABase > 0.0 then
            begin
                PFNominal := kWBase / kVABase;
                // If kW and kvar are different signs, PF is negative
                if kvarbase <> 0.0 then
                    PFNominal := PFNominal * Sign(kWbase * kvarbase);
            end;
            // Else leave it as it is
        end;
        TLoadSpec.kVA_PF:
        begin
            kWbase := kVABase * Abs(PFNominal);
            kWref := kWBase;
            kvarBase := kWBase * SQRT(1.0 / SQR(PFNominal) - 1.0);
            kvarref := kvarbase;
            if PFNominal < 0.0 then
                kvarBase := -kvarBase;
        end;
        TLoadSpec.ConnectedkVA_PF, TLoadSpec.kWh_PF:
            if PFChanged then
            begin  // Recompute kvarBase
                kvarBase := kWBase * SQRT(1.0 / SQR(PFNominal) - 1.0);
                if PFNominal < 0.0 then
                    kvarBase := -kvarBase;
                kVABase := SQRT(SQR(kWref) + SQR(kVARref));
            end;
         // done automagically in Property set...      3, 4: ComputeAllocatedLoad;
    else
    end;

    SetNominalLoad;

    if Rneut < 0.0 then  // flag FOR open neutral
        YNeut := Cmplx(0.0, 0.0)
    else
    if (Rneut = 0.0) and (Xneut = 0.0) then // Solidly Grounded
        YNeut := Cmplx(1.0e6, 0.0)  // 1 microohm resistor
    else
        YNeut := Cinv(Cmplx(Rneut, XNeut));

    varBase := 1000.0 * kvarBase / Fnphases;
    YQFixed := -varBase / SQR(VBase);

    PFChanged := FALSE;
end;

procedure TLoadObj.CalcYPrimMatrix(Ymatrix: TcMatrix);
var
    Y, Yij,
    ZSeries: Complex;
    i, j: Integer;
    FreqMultiplier: Double;
    XseriesOhms: Double;
begin
    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

    with ActiveCircuit.Solution do
        if IsHarmonicModel and (Frequency <> ActiveCircuit.Fundamental) then
        begin     // Harmonic Mode  and other than fundamental frequency
            if ActiveCircuit.NeglectLoadY then
            begin
                // Just a small value so things don't die and we get the actual injection current out the terminal
                Y := cmplx(Epsilon, 0.0)
            end
            else // compute equivalent Y assuming some of the load is series R-L and the rest is parallel R-L
            begin
                // Parallel R-L part of the Load model for harmonics mode
                // Based in equivalent Y at 100% voltage
                Y := Yeq * (1.0 - puSeriesRL);
                Y.im := Y.im / FreqMultiplier;  {Correct reactive part for frequency}

                // Series-connected R-L part
                if puSeriesRL <> 0.0 then
                begin
                    if FpuXharm > 0.0 then
                    begin   // compute Zseries from special harmonic reactance for representing motors.
                            // the series branch is assumed to represent the motor
                        XseriesOhms := SQR(kVLoadBase) * 1000.0 / (kVABase * puSeriesRL) * FpuXharm;
                        Zseries := cmplx(XseriesOhms / FXRharmRatio, XSeriesOhms);
                    end
                    else    // Compute Zseries from nominal load value
                        Zseries := Cinv(Yeq * puSeriesRL);

                    Zseries.im := Zseries.im * FreqMultiplier;  // Correct reactive part for frequency
                    Y := Cinv(ZSeries) + Y; // convert to admittance and add into Y
                end;

            end;
        end
        else
        begin   // not Harmonic mode
            Y := Yeq;
            Y.im := Y.im / FreqMultiplier;  // Correct reactive part for frequency
        end;


    Yij := -Y;

    case Connection of
        TLoadConnection.Wye:
        begin // WYE
            for i := 1 to Fnphases do
            begin
                Ymatrix.SetElement(i, i, Y);
                Ymatrix.AddElement(Fnconds, Fnconds, Y);
                Ymatrix.SetElemsym(i, Fnconds, Yij);
            end;
            Ymatrix.AddElement(Fnconds, Fnconds, YNeut);  // Neutral

            // If neutral is floating, make sure there is some small
            // connection to ground  by increasing the last diagonal slightly
            if Rneut < 0.0 then
                Ymatrix.SetElement(Fnconds, Fnconds, Ymatrix.GetElement(Fnconds, Fnconds) * 1.000001)
        end;
        TLoadConnection.Delta:
        begin  // Delta  or L-L
            for i := 1 to Fnphases do
            begin
                j := i + 1;
                if j > Fnconds then
                    j := 1;  // wrap around for closed connections
                Ymatrix.AddElement(i, i, Y);
                Ymatrix.AddElement(j, j, Y);
                Ymatrix.AddElemSym(i, j, Yij);   // get both off-diagonal elements
            end;
        end;
    end;
end;

procedure TLoadObj.CalcYPrim;
// If doing an analysis that requires the load to be modeled as an impedance
// then put all in.
var
    i: Integer;
begin
    // Build only YPrim Shunt for a Load  then Copy to YPrim
    // Build a dummy Yprim Series so that CalcV does not fail
    if YPrimInvalid then
    begin
        if YPrim_Shunt <> NIL then
            Yprim_Shunt.Free;
        if YPrim_Series <> NIL then
            Yprim_Series.Free;
        if YPrim <> NIL then
            Yprim.Free;

        YPrim_Series := TcMatrix.CreateMatrix(Yorder);
        YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
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
        SetNominalLoad;         // same as admittance model
        CalcYPrimMatrix(YPrim_Shunt);

    end
    else
    begin   // ADMITTANCE model wanted

        SetNominalLoad;
        CalcYPrimMatrix(YPrim_Shunt);

    end;

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, Yprim_Shunt.Getelement(i, i) * 1.0e-10);

    YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
    inherited CalcYPrim;
end;

procedure TLoadObj.StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer); inline;
// Put the current into the proper location according to connection
var
    j: Integer;
begin
    case Connection of

        TLoadConnection.Wye:
        begin  //Wye
            TermArray^[i] -= Curr;
            TermArray^[Fnconds] += Curr; // Neutral
        end;

        TLoadConnection.Delta:
        begin //DELTA
            TermArray^[i] -= Curr;
            j := i + 1;
            if j > Fnconds then
                j := 1;  // rotate the phases
            TermArray^[j] += Curr;
        end;
    end;
end;

procedure TLoadObj.DoConstantPQLoad;
var
    i: Integer;
    Curr: Complex;
    V: Complex;
    Vmag: Double;
begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        VMag := Cabs(V);

        if VMag <= VBaseLow then
            Curr := Yeq * V  // Below VbaseZ resort to linear equal to Yprim contribution
        else
        if VMag <= VBase95 then
            Curr := InterpolateY95_YLow(Vmag) * V   //  Voltage between Vminpu and Vlow
        else
        if VMag > VBase105 then
            Curr := Yeq105 * V  // above 105% use an impedance model
        else
            Curr := cong(Cmplx(WNominal, varNominal) / V);  // Above 95%, constant PQ

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
        FPhaseCurr^[i] := Curr;

        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TLoadObj.DoConstantZLoad;
var
    i: Integer;
    Curr: Complex;
begin
    // Assume Yeq is kept up to date

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        Curr := Yeq * Vterminal^[i];

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
        FPhaseCurr^[i] := Curr;

        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TLoadObj.DoMotorTypeLoad;
// Constant P, quadratic Q
var
    i: Integer;
    Curr: Complex;
    V: Complex;
    VMag: Double;
begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        VMag := Cabs(V);
        if VMag <= VBaseLow then
            Curr := Yeq * V  // Below VbaseZ resort to linear equal to Yprim contribution
        else
        if VMag <= VBase95 then
            Curr := InterpolateY95_YLow(Vmag) * V   //  Voltage between Vminpu and Vlow
        else
        if VMag > VBase105 then
            Curr := Yeq105 * V  // above 105% use an impedance model
        else
        begin
            Curr := cong(Cmplx(WNominal, 0.0) / V);  // Above 95%, constant P
            Curr += Cmplx(0.0, Yeq.im) * V;  // add in Q component of current
        end;

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
        FPhaseCurr^[i] := Curr;

        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TLoadObj.DoConstantILoad;
// Constant Current Load
var
    i: Integer;
    V: Complex;
    Vmag: Double;
    Curr: Complex;
begin
    // Computes the current assuming the voltage mag is Vbase
    // Just uses the phase angle off the voltage

    // Injection = [s/v]* = [ (P+jQ)/(Vbase * V/|V|)]*
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];

        Vmag := Cabs(V);
        if VMag <= VBaseLow then
            Curr := Yeq * V  // Below VbaseZ resort to linear equal to Yprim contribution
        else if VMag <= VBase95 then
            Curr := InterpolateY95I_YLow(Vmag) * V   //  Voltage between Vminpu and Vlow    ***Added by Celso & Paulo
        else if VMag > VBase105 then
            Curr := Yeq105I * V  // above 105% use an impedance model
        else
            Curr := cong(Cmplx(WNominal, varNominal) / ((V / Vmag) * Vbase));

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
        FPhaseCurr^[i] := Curr;

        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;


{$IFDEF UNIX}
//TODO: test in all platforms
function exp(x: double): double; cdecl; external;
function pow(x, y: double): double; cdecl; external;
{$ELSE}
{$DEFINE pow:=Math.Power}
{$ENDIF}

procedure TLoadObj.DoZIPVModel;
var
    i: Integer;
    Curr: Complex;
    CurrZ: Complex;
    CurrI: Complex;
    CurrP: Complex;
    V, Vaux: Complex;
    Vmag: Double;
    vx, evx, yv: Double;
begin
    if not ZIPVset then
    begin
        DoSimpleMsg(_('ZIPV is not set. Aborting...'), 1366);
        DSS.SolutionAbort := True;
        Exit;
    end;

{$IFDEF NO_ZIPV_MANUAL_OPT}
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
{$ELSE}
    //->CalcYPrimContribution
    ComputeVTerminal;

    // Apply these voltages to Yprim
    YPrim.MVMult(InjCurrent, Vterminal);
    //<-CalcYPrimContribution
    
    //-> CalcVTerminalPhase
    // Establish phase voltages and stick in Vtemp
    case Connection of
        TLoadConnection.Wye:
        begin
            Vaux := Vterminal[Fnconds];
            for i := 1 to Fnphases do
                Vterminal[i] -= Vaux;
        end;
        TLoadConnection.Delta:
        begin
            Vaux := Vterminal[1];
            for i := 1 to Fnphases do
            begin
                if i >= Fnconds then
                    Vterminal[i] -= Vaux
                else
                    Vterminal[i] -= Vterminal[i + 1]; // VDiff(NodeRef^[i], NodeRef^[j]);
            end;
        end;
    end;
    LoadSolutionCount := ActiveCircuit.Solution.SolutionCount;
    //<- CalcVTerminalPhase
{$ENDIF}

    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        VMag := Cabs(V);

        // May 31, 2016 changed to linear model below VbaseLow -- converges better for low voltage
        if VMag <= VBaseLow then
            Curr := Yeq * V  // Below VbaseZ resort to linear equal to Yprim contribution
        else
        begin
            CurrZ := CZERO;
            CurrI := CZERO;
            CurrP := CZERO;

            if VMag <= VBase95 then
            begin
                if (ZIPV[1] <> 0) or (ZIPV[4] <> 0) then
                    CurrZ := Cmplx(Yeq.re * ZIPV[1], Yeq.im * ZIPV[4]);
                
                if (ZIPV[3] <> 0) or (ZIPV[6] <> 0) then
                    CurrP := Cmplx(InterpolateY95_YLow(Vmag).re * ZIPV[3], InterpolateY95_YLow(Vmag).im * ZIPV[6]);
                
                if (ZIPV[2] <> 0) or (ZIPV[5] <> 0) then
                    CurrI := Cmplx(InterpolateY95I_YLow(Vmag).re * ZIPV[2], InterpolateY95I_YLow(Vmag).im * ZIPV[5]);
                    
                Curr := (CurrZ + CurrI + CurrP) * V;
            end
            else
            if VMag > VBase105 then
            begin
                if (ZIPV[1] <> 0) or (ZIPV[4] <> 0) then
                    CurrZ := Cmplx(Yeq.re * ZIPV[1], Yeq.im * ZIPV[4]);
                
                if (ZIPV[3] <> 0) or (ZIPV[6] <> 0) then
                    CurrP := Cmplx(Yeq105.re * ZIPV[3], Yeq105.im * ZIPV[6]);
                
                if (ZIPV[2] <> 0) or (ZIPV[5] <> 0) then
                    CurrI := Cmplx(Yeq105I.re * ZIPV[2], Yeq105I.im * ZIPV[5]);
                    
                Curr := (CurrZ + CurrI + CurrP) * V;
            end
            else
            begin
                if (ZIPV[1] <> 0) or (ZIPV[4] <> 0) then
                    CurrZ := Cmplx(Yeq.re * ZIPV[1], Yeq.im * ZIPV[4]) * V;
                
                if (ZIPV[2] <> 0) or (ZIPV[5] <> 0) then
                    CurrI := cong(Cmplx(WNominal * ZIPV[2], varNominal * ZIPV[5]) / ((V / Cabs(V)) * Vbase));
                
                if (ZIPV[3] <> 0) or (ZIPV[6] <> 0) then
                    CurrP := cong(Cmplx(WNominal * ZIPV[3], varNominal * ZIPV[6]) / V);
                    
                Curr := CurrZ + CurrI + CurrP;
            end;

            // low-voltage drop-out
            if ZIPV[7] > 0.0 then
            begin
                vx := 500.0 * (Vmag / Vbase - ZIPV[7]);
{$IFNDEF NO_ZIPV_MANUAL_OPT}
                if vx < 20 then // if >= 20, yv is 1 for a float64
                begin
{$ENDIF}                
                    evx := exp(2 * vx);
                    yv := 0.5 * (1 + (evx - 1) / (evx + 1));
                    Curr := Curr * yv;
{$IFNDEF NO_ZIPV_MANUAL_OPT}
                end;
{$ENDIF}
            end;
        end;

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
        FPhaseCurr^[i] := Curr;

        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TLoadObj.DoCVRModel;
// Linear P, quadratic Q
var
    i: Integer;
    V: Complex;
    Curr: Complex;
    Cvar: Complex;  // var current
    WattFactor: Double;
    VarFactor: Double;
    Vmag: Double;
    VRatio: Double;
begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;
    try
        for i := 1 to Fnphases do
        begin
            V := Vterminal^[i];
            Vmag := Cabs(V);

            if VMag <= VBaseLow then
                Curr := Yeq * V  // Below VbaseZ resort to linear equal to Yprim contribution
            else
            if VMag <= VBase95 then
                Curr := InterpolateY95_YLow(Vmag) * V   //  Voltage between Vminpu and Vlow
            else
            if VMag > VBase105 then
                Curr := Yeq105 * V  // above 105% use an impedance model
            else
            begin
                VRatio := Vmag / VBase;    // vbase is l-n FOR wye and l-l FOR delta

                // Linear factor adjustment does not converge for some reason while power adjust does easily
                 // WattFactor := (1.0 + FCVRwattFactor*(Vmag/VBase - 1.0));
                if FCVRWattFactor <> 1.0 then
                    WattFactor := pow(VRatio, FCVRWattFactor)
                else
                    WattFactor := Vratio;  // old value (in error): 1.0;
                if WattFactor > 0.0 then
                    Curr := cong(Cmplx(WNominal * WattFactor, 0.0) / V)
                else
                    Curr := CZERO; // P component of current

                if Vmag = 0.0 then
                    Cvar := CZERO    // Trap divide by zero error
                //Compute Q component of current
                else
                if FCVRvarFactor = 2.0 then
                begin // Check for easy, quick ones first
                    Cvar := Cmplx(0.0, Yeq.im) * V; // 2 is same as Constant impedance
                end
                else
                if FCVRvarFactor = 3.0 then
                begin
                    VarFactor := VRatio*VRatio*VRatio;
                    Cvar := cong(Cmplx(0.0, VarNominal * VarFactor) / V);
                end
                else
                begin
                    // Other Var factor code here if not squared or cubed
                    VarFactor := pow(VRatio, FCVRvarFactor);
                    Cvar := cong(Cmplx(0.0, VarNominal * VarFactor) / V);
                end;
                Curr += Cvar;  // add in Q component of current
            end;

            // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
            FPhaseCurr^[i] := Curr;

            StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
            IterminalUpdated := TRUE;
            StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
        end;

    except
        On E: Exception do
        begin
            DoSimpleMsg('Error in %s: %s ', [FullName, E.Message], 5871);
            raise;
        end;

    end;
end;

procedure TLoadObj.DoFixedQ;
// Constant P, Fixed Q  Q is always kvarBase
var
    i: Integer;
    Curr,
    V: Complex;
    Vmag: Double;
begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        VMag := Cabs(V);
        if VMag <= VBaseLow then
            Curr := Yeq * V  // Below VbaseZ resort to linear equal to Yprim contribution
        else
        if VMag <= VBase95 then
            Curr := Cmplx(Yeq95.re, YQfixed) * V  // Below 95% use an impedance model
        else
        if VMag > VBase105 then
            Curr := Cmplx(Yeq105.re, YQfixed) * V  // above 105% use an impedance model
        else
        begin
            Curr := cong(Cmplx(WNominal, varBase) / V);
        end;

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
        FPhaseCurr^[i] := Curr;

        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TLoadObj.DoFixedQZ;
// Constant P, Fixed Q  Q is always a fixed Z derived from kvarBase
var
    i: Integer;
    Curr,
    V: Complex;
    Vmag: Double;
begin
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        Vmag := Cabs(V);
        if VMag <= VBaseLow then
            Curr := Yeq * V  // Below VbaseZ resort to linear equal to Yprim contribution
        else
        if Vmag <= VBase95 then
            Curr := Cmplx(Yeq95.re, YQfixed) * V  // Below 95% use an impedance model
        else
        if VMag > VBase105 then
            Curr := Cmplx(Yeq105.re, YQfixed) * V
        else
        begin
            Curr := cong(Cmplx(WNominal, 0.0) / V); // P component of current
            Curr += Cmplx(0.0, YQFixed) * V;  // add in Q component of current
        end;

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
        FPhaseCurr^[i] := Curr;

        StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TLoadObj.DoHarmonicMode;
// Compute Injection Current Only when in harmonics mode
// Assumes spectrum is an ideal current source based on the fundamental current and spectrum
var
    i: Integer;
    Curr, Mult: Complex;
    LoadHarmonic: Double;

begin
    // Don't calc Vterminal here because it could be undefined!
    ZeroInjCurrent;
    ZeroIterminal;
    with ActiveCircuit.Solution do
    begin
        LoadHarmonic := Frequency / LoadFundamental;    // Loadfundamental = frequency of solution when Harmonic mode entered
        Mult := SpectrumObj.GetMult(LoadHarmonic);
        for i := 1 to FNphases do
        begin
            Curr := Mult * HarmMag^[i]; // Get base harmonic magnitude
            RotatePhasorDeg(Curr, LoadHarmonic, HarmAng^[i]);   // Time shift by fundamental
            // don't need to save Curr here like we do in Power Flow modes
            StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into InjCurrent array taking into account connection
            StickCurrInTerminalArray(ITerminal, -Curr, i);  // Put into Terminal array taking into account connection
            // NOTE: This is the value of ITerminal a Monitor will capture in Harmonics mode .. it captures the harmonic injection
            IterminalUpdated := TRUE;
        end;

    end;
end;

function TLoadObj.AllTerminalsClosed: Boolean;
var
    i, j: Integer;
begin
    Result := TRUE;
    for i := 1 to Nterms do
        for j := 1 to NConds do
            if not Terminals[i - 1].ConductorsClosed[j - 1] then
            begin
                Result := FALSE;
                Exit;
            end;
end;

procedure TLoadObj.CalcVTerminalPhase;
var
    i, j: Integer;
begin
    // Establish phase voltages and stick in Vtemp
    case Connection of

        TLoadConnection.Wye:
        begin
            with ActiveCircuit.Solution do
                for i := 1 to Fnphases do
                    Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[Fnconds]);
        end;

        TLoadConnection.Delta:
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

    LoadSolutionCount := ActiveCircuit.Solution.SolutionCount;
end;

procedure TLoadObj.CalcLoadModelContribution;
// Calculates total load current and adds it properly into the InjCurrent array

// Need to implement DynamicMode sometime ...
begin
    IterminalUpdated := FALSE;
    with ActiveCircuit, ActiveCircuit.Solution do
    begin
          {IF      IsDynamicModel THEN  DoDynamicMode
          ELSE} if IsHarmonicModel and (Frequency <> Fundamental) then
            DoHarmonicMode
        else
           //  compute total Load currents and Add into InjCurrent array;
            case FLoadModel of

                TLoadModel.ConstPQ:
                    DoConstantPQLoad; // normal load-flow type load
                TLoadModel.ConstZ:
                    DoConstantZLoad;
                TLoadModel.Motor:
                    DoMotorTypeLoad;  // Constant P, Quadratic Q;
                TLoadModel.CVR:
                    DoCVRModel;       // mixed motor/resistive load   with CVR factors
                TLoadModel.ConstI:
                    DoConstantILoad;
                TLoadModel.ConstPFixedQ:
                    DoFixedQ;         // Fixed Q
                TLoadModel.ConstPFixedX:
                    DoFixedQZ;        // Fixed, constant Z Q
                TLoadModel.ZIPV:
                    DoZIPVModel;
            else
                DoConstantZLoad;     // FOR now, until we implement the other models.
            end;
    end;
end;

procedure TLoadObj.CalcInjCurrentArray;
// Fill InjCurrent array with the current values to use for injections.
var
    i, j, k: Integer;
begin
    // IF a terminal is open, THEN standard load models don't apply, so check it out first
    if (not DSS_CAPI_LOADS_TERMINAL_CHECK) OR AllTerminalsClosed then
    begin
        // Now Get Injection Currents
        CalcLoadModelContribution
    end
    else
    begin
        /// THIS MAY NOT WORK !!! WATCH FOR BAD RESULTS
        // some terminals not closed  use admittance model FOR injection
        if OpenLoadSolutionCount <> ActiveCircuit.Solution.SolutionCount then
        begin
            // Rebuild the Yprimopencond IF a new solution because values may have changed.

            // only reallocate when necessary
            if YPrimOpenCond = NIL then
                YPrimOpenCond := TcMatrix.CreateMatrix(Yorder)
            else
                YPrimOpenCond.Clear;
            if YPrimOpenCond.Order <> Yorder then
            begin
                YPrimOpenCond.Free;
                YPrimOpenCond := TcMatrix.CreateMatrix(Yorder);
            end;
            CalcYPrimMatrix(YPrimOpenCond);

            // Now Account FOR the Open Conductors
            // For any conductor that is open, zero out row and column
            with YPrimOpenCond do
            begin
                k := 0;
                for i := 1 to Fnterms do
                begin
                    for j := 1 to Fnconds do
                    begin
                        if not Terminals[i - 1].ConductorsClosed[j - 1] then
                        begin
                            ZeroRow(j + k);
                            ZeroCol(j + k);
                            SetElement(j + k, j + k, Cmplx(1.0e-12, 0.0));  // In case node gets isolated
                        end;
                    end;
                    k := k + Fnconds;
                end;
            end;
            OpenLoadSolutionCount := ActiveCircuit.Solution.SolutionCount;

        end;

        ComputeVTerminal;
        YPrimOpenCond.MVmult(ComplexBuffer, Vterminal);
        for i := 1 to Yorder do
            ComplexBuffer^[i] := -ComplexBuffer^[i];
    end;
end;

procedure TLoadObj.GetTerminalCurrents(Curr: pComplexArray);
// Always return total terminal currents in the Curr array
begin
    with ActiveCircuit.Solution do
    begin
        if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
        begin     // recalc the contribution
            CalcLoadModelContribution;  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr);
    end;
end;

function TLoadObj.InjCurrents: Integer;
// Get the injection currents and add them directly into the Currents array
begin
    Result := 0;
    if Enabled then
        with ActiveCircuit.Solution do
        begin
            if LoadsNeedUpdating then
                SetNominalLoad; // Set the nominal kW, etc. for the type of solution being done
            CalcInjCurrentArray;
            Result := inherited Injcurrents;  // Add into Global Currents Array
        end;
end;

function TLoadObj.InterpolateY95_YLow(const Vmag: Double): Complex;
// For Vmag between V95 and Vlow, interpolate for equivalent Y
begin
    Result := (ILow + M95 * (Vmag - VbaseLow)) / Vmag;   //(Ilow + M95 * (Vmag - VBaseLow))/Vmag)
end;

function TLoadObj.InterpolateY95I_YLow(const Vmag: Double): Complex;      // ***Added by Celso & Paulo
// For Vmag between V95 and Vlow, interpolate for equivalent Y
begin
    Result := (ILow + M95I * (Vmag - VbaseLow)) / Vmag;   //(Ilow + M95I * (Vmag - VBaseLow))/Vmag)   // ***Changed by Celso & Paulo
end;

function TLoadObj.Get_Unserved: Boolean;
var
    i: Integer;
    Vpu,
    Vmag: Double;
    NormMinCriteria,
    EmergMinCriteria: Double;
    // Line overload takes precedence.
    // Assumes that low voltage is due to overloaded line.
    // IF voltage is below Emergency minumum, it is counted as  unserved.
begin
    Result := FALSE;
    if UE_Factor > 0.0 then
    begin
        Result := TRUE;
        Exit;
    end;

     {ELSE Check Voltages}
    if LoadSolutionCount <> ActiveCircuit.Solution.SolutionCount then
        CalcVTerminalPhase;

     // Get the lowest of the Phase voltages
    Vpu := Vbase;
    for i := 1 to Fnphases do
    begin
        Vmag := Cabs(Vterminal^[i]);
        if (Vmag < Vpu) then
            Vpu := Vmag;
    end;
    Vpu := Vpu / Vbase;

    if VminNormal <> 0.0 then
        NormMinCriteria := VMinNormal
    else
        NormMinCriteria := ActiveCircuit.NormalMinVolts;

    if VminEmerg <> 0.0 then
        EmergMinCriteria := VMinEmerg
    else
        EmergMinCriteria := ActiveCircuit.EmergMinVolts;

    if Vpu < EmergMinCriteria then
    begin
        Result := TRUE;
         //UE_Factor := 1.0;
         // 9-19-00 RCD  let UE_Factor start small and grow linearly at same slope
         // as EEN_Factor
        UE_Factor := (EmergMinCriteria - Vpu) / (NormMinCriteria - EmergMinCriteria);
        Exit;
    end;
end;

function TLoadObj.Get_ExceedsNormal: Boolean;
var
    i: Integer;
    Vpu,
    Vmag: Double;

    // Line overload takes precedence.
    // Assumes that low voltage is due to overloaded line.
    // IF voltage is below Normal minumum, it is counted as unserved in proportion
    // to the difference between the normal and emergency voltage limits.

    NormMinCriteria,
    EmergMinCriteria: Double;

begin
    Result := FALSE;
    if EEN_Factor > 0.0 then
    begin
        Result := TRUE;
        Exit;
    end;   // Check line overload

    if LoadSolutionCount <> ActiveCircuit.Solution.SolutionCount then
        CalcVTerminalPhase;

     // Get the lowest of the Phase voltages
    Vpu := Vbase;
    for i := 1 to Fnphases do
    begin
        Vmag := Cabs(Vterminal^[i]);
        if (Vmag < Vpu) then
            Vpu := Vmag;
    end;
    Vpu := Vpu / Vbase;

    if VminNormal <> 0.0 then
        NormMinCriteria := VMinNormal
    else
        NormMinCriteria := ActiveCircuit.NormalMinVolts;

    if VminEmerg <> 0.0 then
        EmergMinCriteria := VMinEmerg
    else
        EmergMinCriteria := ActiveCircuit.EmergMinVolts;


    if Vpu < NormMinCriteria then
    begin
        EEN_Factor := (NormMinCriteria - Vpu) / (NormMinCriteria - EmergMinCriteria);
       // 9-19-00 RCD  Let EEN factor grow linearly at same slope
       // IF EEN_Factor > 1.0 THEN EEN_Factor := 1.0;
        Result := TRUE;
        Exit;
    end;
end;

procedure TLoadObj.Set_kVAAllocationFactor(const Value: Double);
begin
    FkVAAllocationFactor := Value;
    FAllocationFactor := Value;
    LoadSpecType := TLoadSpec.ConnectedkVA_PF;
    ComputeAllocatedLoad;
    HasBeenAllocated := TRUE;
end;

procedure TLoadObj.Set_AllocationFactor(const Value: Double);
{This procedure is used by the energymeter allocateload function to adjust load allocation factors}
begin
    FAllocationFactor := Value;
    case LoadSpecType of
        TLoadSpec.ConnectedkVA_PF:
            FkVAAllocationFactor := Value;
        TLoadSpec.kWh_PF:
            FCFactor := Value;
    end;
    ComputeAllocatedLoad;  // update kWbase
    HasBeenAllocated := TRUE;
end;

procedure TLoadObj.Set_CFactor(const Value: Double);
begin
    FCFactor := Value;
    FAllocationFactor := Value;
    LoadSpecType := TLoadSpec.kWh_PF;
    ComputeAllocatedLoad;
    HasBeenAllocated := TRUE;
end;

procedure TLoadObj.ComputeAllocatedLoad;
begin
{Fixed loads defined by kW, kvar or kW, pf are ignored}

    case LoadSpecType of

        TLoadSpec.ConnectedkVA_PF:
            if FConnectedkVA > 0.0 then
            begin
                kWBase := FConnectedkVA * FkVAAllocationFactor * Abs(PFNominal);
                kvarBase := kWBase * SQRT(1.0 / SQR(PFNominal) - 1.0);
                if PFNominal < 0.0 then
                    kvarBase := -kvarBase;
            end;

        TLoadSpec.kWh_PF:
        begin
            FavgkW := FkWh / (FkWhDays * 24);
            kWBase := FavgkW * FCfactor;
            kvarBase := kWBase * SQRT(1.0 / SQR(PFNominal) - 1.0);
            if PFNominal < 0.0 then
                kvarBase := -kvarBase;
        end;
    end;
end;


procedure TLoadObj.InitHarmonics;
// Get the present terminal currents and store for harmonics base reference;
var
    i: Integer;
begin
    // Make Sure there's enuff memory
    ReallocMem(HarmMag, Sizeof(HarmMag^[1]) * FNphases);
    ReallocMem(HarmAng, Sizeof(HarmAng^[1]) * FNphases);

     // Currents := AllocMem(Sizeof(Currents^[1])*Yorder);   // to hold currents

    LoadFundamental := ActiveCircuit.Solution.Frequency;

    // GetCurrents(Currents); // Use FPhaseCurr from most recent pflow solution
    // Store the currents at fundamental frequency. The spectrum is applied to these.
     
    for i := 1 to Fnphases do
    begin
        HarmMag^[i] := Cabs(FPhaseCurr^[i]);
        HarmAng^[i] := Cdang(FPhaseCurr^[i]);
    end;

     // ReallocMem(Currents, 0);  // get rid of temp space
end;

procedure TLoadObj.MakePosSequence();
var
    V, newkW, newkvar, newkva: Double;
    changes: Integer;
begin
    // Make sure voltage is line-neutral
    if (Fnphases > 1) or (connection <> TLoadConnection.Wye) then
        V := kVLoadBase / SQRT3
    else
        V := kVLoadBase;

    // New Method: Assume load is distributed equally among the 3 phases -- works better
    //1-5-2016 RCD

    newkw := kWbase / 3.0;
    newkvar := kvarbase / 3.0;
    newkva := FConnectedKVA / 3.0;

    BeginEdit(True);
    SetInteger(ord(TProp.Phases), 1);
    SetInteger(ord(TProp.conn), 0);
    SetDouble(ord(TProp.kV), V);
    SetDouble(ord(TProp.kW), newkW);
    SetDouble(ord(TProp.kvar), newkvar);
    changes := 5;
    if newkva > 0.0 then
    begin
        SetDouble(ord(TProp.xfkVA), newkva);
        changes := changes + 1;
    end;
    EndEdit(changes);

    inherited;
end;

finalization    LoadStatusEnum.Free;
    LoadModelEnum.Free;
end.
