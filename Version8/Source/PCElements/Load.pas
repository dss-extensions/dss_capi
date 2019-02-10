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

{   Change Log
    10/7/99  RCD  Tightened up default load shape code and corrected comments
    11-22-99  Fixed bug in CalcYPrimMatrix
    12-5-99  Changed PQ load limits to 95% to 105%
    1-8-99 Made PQ load limits a variable (added vminpu, vmaxpu properties)
    2-1-00 Added normal and emergency voltage ratings to override system settings when <> 0
    4-17-00 Added XFKVA and AllocationFactor properties and associated code.
    8-26-00 Added exemption from LoadMult code (exemptfromLDcurve)
    9-19-00 Changed the way UE and EEN computed for low voltage
    10-25-00  Added Spectrum
    10-27-00 Implemented Harmonic current  and Harmonic Mode stuff
    3-27-01 Added check to prevent divide by zero on calculation of PFNominal
    5-17-01 Moved Spectrum definition back to PCElement
    2-18-03 Changed Rneut default to -1
            Created a Y_Series with small conductances on the diagonal so that calcV doesn't fail
    9-23-08 Added CVR Factors
    10-14-08 Added kWh and Cfactor. Modified behavior of AllocationFactor to simplify State Estimation
    4/1/14 Added Vlowpu property to make solution converge better at very low voltages
    1/7/15 Added puXHarm and XRHarm properties to help model motor load for harmonic studies
    3/16/16 Added PFSpecified to account for problems when UseActual is specified and no Qmult specified
    1/10/18 Celso/Paulo mods for low-voltage transition for Model 5
}

interface

uses
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    ucomplex,
    LoadShape,
    GrowthShape,
    Spectrum,
    ArrayDef;

type
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TLoad = class(TPCClass)
    PRIVATE

        procedure InterpretConnection(const S: String);
        procedure SetNcondsForConnection;
    PROTECTED
        function MakeLike(const OtherLoadName: String): Integer; OVERRIDE;
        procedure DefineProperties;  // Add Properties of this class to propName
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;
    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TLoadObj = class(TPCElement)
    PRIVATE
        PFChanged: Boolean;
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

        ExemptFromLDCurve: Boolean;
        Fixed: Boolean;   // IF Fixed, always at base value
        ShapeIsActual: Boolean;
        PFSpecified: Boolean;  // Added 3-16-16 to fix problem with UseActual
        FnZIPV: Integer;

        function AllTerminalsClosed: Boolean;
        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);
        procedure CalcInjCurrentArray(ActorID: Integer);
        procedure CalcLoadModelContribution(ActorID: Integer);
        procedure CalcVTerminalPhase(ActorID: Integer);
        procedure CalcYearlyMult(Hr: Double);
        procedure CalcCVRMult(Hr: Double);
        procedure CalcYPrimMatrix(Ymatrix: TcMatrix; ActorID: Integer);
        procedure DoConstantILoad(ActorID: Integer);
        procedure DoConstantPQLoad(ActorID: Integer);
        procedure DoConstantZLoad(ActorID: Integer);
        procedure DoFixedQ(ActorID: Integer);
        procedure DoFixedQZ(ActorID: Integer);
        procedure DoHarmonicMode(ActorID: Integer);
        procedure DoCVRModel(ActorID: Integer);
        procedure DoZIPVModel(ActorID: Integer);
        procedure SetZIPVSize(n: Integer);
        procedure DoMotorTypeLoad(ActorID: Integer);
        function GrowthFactor(Year: Integer; ActorID: Integer): Double;
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);
        function InterpolateY95_YLow(const Vmag: Double): Complex; inline;
        function InterpolateY95I_YLow(const Vmag: Double): Complex; inline; // ***Added by Celso & Paulo
        function Get_Unserved(ActorID: Integer): Boolean;

        procedure Set_kVAAllocationFactor(const Value: Double);
        procedure Set_ConnectedkVA(const Value: Double);
        procedure ComputeAllocatedLoad;
        {Set kWh properties ...}
        procedure Set_CFactor(const Value: Double);
        procedure Set_kWh(const Value: Double);
        procedure Set_kWhDays(const Value: Double);
        procedure Set_AllocationFactor(const Value: Double);
        procedure SetkWkvar(const PkW, Qkvar: Double);
        procedure set_nZIPV(const Value: Integer);


    PROTECTED
        procedure GetTerminalCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;

    PUBLIC

        Connection: Integer;  {     0 = line-neutral; 1=Delta}
        DailyShape: String;         // Daily (24 HR) load shape
        DailyShapeObj: TLoadShapeObj;  // Daily load Shape FOR this load
        DutyShape: String;         // Duty cycle load shape FOR changes typically less than one hour
        DutyShapeObj: TLoadShapeObj;  // Shape for this load
        EEN_Factor: Double;         // is overloaded  Factor is the amount of overload
        GrowthShape: String;         // (year, Multiplier from previous year)
        GrowthShapeObj: TGrowthShapeObj;  // Shape for this Growth  Curve
        HasBeenAllocated: Boolean;
        kWBase: Double;
        kVABase: Double;
        kvarBase: Double;
        kVLoadBase: Double;
        LoadClass: Integer;
        NumCustomers: Integer;
        LoadSpecType: Integer;  // 0=kW, PF;  1= kw, kvar;  2=kva, PF
        PFNominal: Double;
        Rneut: Double;
        UE_Factor: Double;  // These are set to > 0 IF a line in the critical path
        Xneut: Double;  // Neutral impedance
        YearlyShape: String;  // ='fixed' means no variation  exempt from variation
        YearlyShapeObj: TLoadShapeObj;  // Shape for this load
        CVRshape: String;
        CVRShapeObj: TLoadShapeObj;
        ZIPV: pDoubleArray;  // Made public 5-20-2013
        puSeriesRL: Double;
        RelWeighting: Double;

        FLoadModel: Integer;   // Variation with voltage
          {  1 = Constant kVA (P,Q always in same ratio)
             2 = Constant impedance
             3 = Constant P, Quadratic Q (Mostly motor)
             4 = Linear P, Quadratic Q  (Mixed motor/resistive Use this for CVR studies
             5 = Constant |I|
             6 = Constant P (Variable); Q is fixed value (not variable)
             7 = Constant P (Variable); Q is fixed Z (not variable)
             8 = ZIPV (3 real power coefficients, 3 reactive, Vcutoff)
          }

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;
        function Get_ExceedsNormal(ActorID: Integer): Boolean;
        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;
        function InjCurrents(ActorID: Integer): Integer; OVERRIDE;
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;
        procedure InitHarmonics(ActorID: Integer); OVERRIDE;
        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model
        procedure SetNominalLoad(ActorID: Integer);
        procedure Randomize(Opt: Integer);
                  // 0 = reset to 1.0
                  // 1 = Gaussian around mean and std Dev
                  // 2 = uniform

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

        procedure UpdateVoltageBases;

        property Unserved[ActorID: Integer]: Boolean READ Get_Unserved;
//        Property ExceedsNormal[ActorID:Integer] :Boolean Read Get_ExceedsNormal(ActorID:Integer);

        {AllocationFactor adjusts either connected kVA allocation factor or kWh CFactor}
        property AllocationFactor: Double READ FAllocationFactor WRITE Set_AllocationFactor;

        {Allocate load from connected kva or kWh billing}
        property kVAAllocationFactor: Double READ FkVAAllocationFactor WRITE Set_kVAAllocationFactor;
        property ConnectedkVA: Double READ FConnectedkVA WRITE Set_ConnectedkVA;
        property kWh: Double READ FkWh WRITE Set_kWh;
        property kWhDays: Double READ FkWhDays WRITE Set_kWhDays;
        property CFactor: Double READ FCFactor WRITE Set_CFactor;
        property puMean: Double READ FpuMean;
        property puStdDev: Double READ FpuStdDev;
        property CVRwatts: Double READ FCVRwattFactor;
        property CVRvars: Double READ FCVRvarFactor;
        property MaxPU: Double READ Vmaxpu;
        property MinEmerg: Double READ VminEmerg;
        property MinNormal: Double READ VminNormal;
        property MinPU: Double READ Vminpu;
        property ExemptLoad: Boolean READ ExemptFromLDCurve;
        property FixedLoad: Boolean READ Fixed;
        property nZIPV: Integer READ FnZIPV WRITE set_nZIPV;
    end;

var
    ActiveLoadObj: TLoadObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


uses
    ParserDel,
    Circuit,
    DSSClassDefs,
    DSSGlobals,
    Dynamics,
    Sysutils,
    Command,
    Math,
    MathUtil,
    Utilities;

const
    NumPropsThisClass = 38;

var
    CDOUBLEONE: Complex;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TLoad.Create;  // Creates superstructure FOR all Line objects
begin
    inherited Create;
    Class_Name := 'Load';
    DSSClassType := DSSClassType + LOAD_ELEMENT;

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLoad.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoad.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names

    PropertyName[1] := 'phases';
    PropertyName[2] := 'bus1';
    PropertyName[3] := 'kV';  //
    PropertyName[4] := 'kW';
    PropertyName[5] := 'pf';
    PropertyName[6] := 'model';
    PropertyName[7] := 'yearly';
    PropertyName[8] := 'daily';
    PropertyName[9] := 'duty';
    PropertyName[10] := 'growth';
    PropertyName[11] := 'conn';
    PropertyName[12] := 'kvar';
    PropertyName[13] := 'Rneut'; // IF entered -, assume open
    PropertyName[14] := 'Xneut';
    PropertyName[15] := 'status';  // fixed or variable
    PropertyName[16] := 'class';  // integer
    PropertyName[17] := 'Vminpu';  // Min pu voltage for which model applies
    PropertyName[18] := 'Vmaxpu';  // Max pu voltage for which model applies
    PropertyName[19] := 'Vminnorm';  // Min pu voltage normal load
    PropertyName[20] := 'Vminemerg';  // Min pu voltage emergency rating
    PropertyName[21] := 'xfkVA';  // Service transformer rated kVA
    PropertyName[22] := 'allocationfactor';  // allocation factor  for xfkVA
    PropertyName[23] := 'kVA';  // specify load in kVA and PF
    PropertyName[24] := '%mean';  // per cent default mean
    PropertyName[25] := '%stddev';  // per cent default standard deviation
    PropertyName[26] := 'CVRwatts';  // Percent watts reduction per 1% reduction in voltage from nominal
    PropertyName[27] := 'CVRvars';  // Percent vars reduction per 1% reduction in voltage from nominal
    PropertyName[28] := 'kwh';   // kwh billing
    PropertyName[29] := 'kwhdays';   // kwh billing period (24-hr days)
    PropertyName[30] := 'Cfactor';   // multiplier from kWh avg to peak kW
    PropertyName[31] := 'CVRcurve';   // name of curve to use for yearly CVR simulations
    PropertyName[32] := 'NumCust';   // Number of customers, this load
    PropertyName[33] := 'ZIPV';      // array of 7 coefficients
    PropertyName[34] := '%SeriesRL';      // pct of Load that is series R-L
    PropertyName[35] := 'RelWeight';      // Weighting factor for reliability
    PropertyName[36] := 'Vlowpu';      // Below this value resort to constant Z model = Yeq
    PropertyName[37] := 'puXharm';      // pu Reactance for Harmonics, if specifies
    PropertyName[38] := 'XRharm';      // X/R at fundamental for series R-L model for hamonics
(*
  Typical Motor Parameters for motor
      Xpu = 0.20
      X/r typically 3-6 for normal motors; higher for high-eff motors
*)

     // define Property help values
    PropertyHelp[1] := 'Number of Phases, this load.  Load is evenly divided among phases.';
    PropertyHelp[2] := 'Bus to which the load is connected.  May include specific node specification.';
    PropertyHelp[3] := 'Nominal rated (1.0 per unit) voltage, kV, for load. For 2- and 3-phase loads, specify phase-phase kV. ' +
        'Otherwise, specify actual kV across each branch of the load. ' +
        'If wye (star), specify phase-neutral kV. ' +
        'If delta or phase-phase connected, specify phase-phase kV.';  // line-neutral voltage
    PropertyHelp[4] := 'Total base kW for the load.  Normally, you would enter the maximum kW for the load for the first year ' +
        'and allow it to be adjusted by the load shapes, growth shapes, and global load multiplier.' + CRLF + CRLF +
        'Legal ways to define base load:' + CRLF +
        'kW, PF' + CRLF +
        'kW, kvar' + CRLF +
        'kVA, PF' + CRLF +
        'XFKVA * Allocationfactor, PF' + CRLF +
        'kWh/(kWhdays*24) * Cfactor, PF';
    PropertyHelp[5] := 'Load power factor.  Enter negative for leading powerfactor (when kW and kvar have opposite signs.)';
    PropertyHelp[6] := 'Integer code for the model to use for load variation with voltage. ' +
        'Valid values are:' + CRLF + CRLF +
        '1:Standard constant P+jQ load. (Default)' + CRLF +
        '2:Constant impedance load. ' + CRLF +
        '3:Const P, Quadratic Q (like a motor).' + CRLF +
        '4:Nominal Linear P, Quadratic Q (feeder mix). Use this with CVRfactor.' + CRLF +
        '5:Constant Current Magnitude' + CRLF +
        '6:Const P, Fixed Q' + CRLF +
        '7:Const P, Fixed Impedance Q' + CRLF +
        '8:ZIPV (7 values)' + CRLF + CRLF +
        'For Types 6 and 7, only the P is modified by load multipliers.';
    PropertyHelp[7] := 'LOADSHAPE object to use for yearly simulations.  Must be previously defined ' +
        'as a Loadshape object. Is set to the Daily load shape ' +
        ' when Daily is defined.  The daily load shape is repeated in this case. ' +
        'Set Status=Fixed to ignore Loadshape designation. ' +
        'Set to NONE to reset to no loadahape. ' +
        'The default is no variation.';
    PropertyHelp[8] := 'LOADSHAPE object to use for daily simulations.  Must be previously defined ' +
        'as a Loadshape object of 24 hrs, typically. ' +
        'Set Status=Fixed to ignore Loadshape designation. ' +
        'Set to NONE to reset to no loadahape. ' +
        'Default is no variation (constant) if not defined. ' +
        'Side effect: Sets Yearly load shape if not already defined.';
    PropertyHelp[9] := 'LOADSHAPE object to use for duty cycle simulations.  Must be previously defined ' +
        'as a Loadshape object.  Typically would have time intervals less than 1 hr. ' +
        'Designate the number of points to solve using the Set Number=xxxx command. ' +
        'If there are fewer points in the actual shape, the shape is assumed to repeat.' +
        'Set to NONE to reset to no loadahape. ' +
        'Set Status=Fixed to ignore Loadshape designation. ' +
        ' Defaults to Daily curve If not specified.';
    PropertyHelp[10] := 'Characteristic  to use for growth factors by years.  Must be previously defined ' +
        'as a Growthshape object. Defaults to circuit default growth factor (see Set Growth command).';
    PropertyHelp[11] := '={wye or LN | delta or LL}.  Default is wye.';
    PropertyHelp[12] := 'Specify the base kvar for specifying load as kW & kvar.  Assumes kW has been already defined.  Alternative to specifying the power factor.  Side effect: ' +
        ' the power factor and kVA is altered to agree.';
    PropertyHelp[13] := 'Default is -1. Neutral resistance of wye (star)-connected load in actual ohms. ' +
        'If entered as a negative value, the neutral can be open, or floating, or it can be connected to ' +
        'node 0 (ground), which is the usual default. ' +
        'If >=0 be sure to explicitly specify the node connection for the neutral, or last, conductor. ' +
        'Otherwise, the neutral impedance will be shorted to ground.';
    PropertyHelp[14] := 'Neutral reactance of wye(star)-connected load in actual ohms.  May be + or -.';
    PropertyHelp[15] := '={Variable | Fixed | Exempt}.  Default is variable. If Fixed, no load multipliers apply;  however, growth ' +
        'multipliers do apply.  All multipliers apply to Variable loads.  Exempt loads are not ' +
        'modified by the global load multiplier, such as in load duration curves, etc.  Daily multipliers ' +
        'do apply, so setting this property to Exempt is a good way to represent industrial load that stays the same' +
        ' day-after-day for the period study.';  // fixed or variable
    PropertyHelp[16] := 'An arbitrary integer number representing the class of load so that load values may ' +
        'be segregated by load value. Default is 1; not used internally.';
    PropertyHelp[17] := 'Default = 0.95.  Minimum per unit voltage for which the MODEL is assumed to apply. Lower end of normal voltage range.' +
        'Below this value, the load model reverts to a constant impedance model that matches the model at the transition voltage. ' +
        'See also "Vlowpu" which causes the model to match Model=2 below the transition voltage.';
    PropertyHelp[18] := 'Default = 1.05.  Maximum per unit voltage for which the MODEL is assumed to apply. ' +
        'Above this value, the load model reverts to a constant impedance model.';
    PropertyHelp[19] := 'Minimum per unit voltage for load EEN evaluations, Normal limit.  Default = 0, which defaults to system "vminnorm" ' +
        'property (see Set Command under Executive).  If this property is specified, it ALWAYS ' +
        'overrides the system specification. This allows you to have different criteria for different loads. ' +
        'Set to zero to revert to the default system value.';
    PropertyHelp[20] := 'Minimum per unit voltage for load UE evaluations, Emergency limit.  Default = 0, which defaults to system "vminemerg" ' +
        'property (see Set Command under Executive).  If this property is specified, it ALWAYS ' +
        'overrides the system specification. This allows you to have different criteria for different loads. ' +
        'Set to zero to revert to the default system value.';
    PropertyHelp[21] := 'Default = 0.0.  Rated kVA of service transformer for allocating loads based on connected kVA ' +
        'at a bus. Side effect:  kW, PF, and kvar are modified. See help on kVA.';
    PropertyHelp[22] := 'Default = 0.5.  Allocation factor for allocating loads based on connected kVA ' +
        'at a bus. Side effect:  kW, PF, and kvar are modified by multiplying this factor times the XFKVA (if > 0).';
    PropertyHelp[23] := 'Specify base Load in kVA (and power factor)' + CRLF + CRLF +
        'Legal ways to define base load:' + CRLF +
        'kW, PF' + CRLF +
        'kW, kvar' + CRLF +
        'kVA, PF' + CRLF +
        'XFKVA * Allocationfactor, PF' + CRLF +
        'kWh/(kWhdays*24) * Cfactor, PF';
    PropertyHelp[24] := 'Percent mean value for load to use for monte carlo studies if no loadshape is assigned to this load. Default is 50.';
    PropertyHelp[25] := 'Percent Std deviation value for load to use for monte carlo studies if no loadshape is assigned to this load. Default is 10.';
    PropertyHelp[26] := 'Percent reduction in active power (watts) per 1% reduction in voltage from 100% rated. Default=1. ' + CRLF +
        ' Typical values range from 0.4 to 0.8. Applies to Model=4 only.' + CRLF +
        ' Intended to represent conservation voltage reduction or voltage optimization measures.';
    PropertyHelp[27] := 'Percent reduction in reactive power (vars) per 1% reduction in voltage from 100% rated. Default=2. ' + CRLF +
        ' Typical values range from 2 to 3. Applies to Model=4 only.' + CRLF +
        ' Intended to represent conservation voltage reduction or voltage optimization measures.';
    PropertyHelp[28] := 'kWh billed for this period. Default is 0. See help on kVA and Cfactor and kWhDays.';
    PropertyHelp[29] := 'Length of kWh billing period in days (24 hr days). Default is 30. Average demand is computed using this value.';   // kwh billing period (24-hr days)
    PropertyHelp[30] := 'Factor relating average kW to peak kW. Default is 4.0. See kWh and kWhdays. See kVA.';   // multiplier from kWh avg to peak kW
    PropertyHelp[31] := 'Default is NONE. Curve describing both watt and var factors as a function of time. ' +
        'Refers to a LoadShape object with both Mult and Qmult defined. ' +
        'Define a Loadshape to agree with yearly or daily curve according to the type of analysis being done. ' +
        'If NONE, the CVRwatts and CVRvars factors are used and assumed constant.';
    PropertyHelp[32] := 'Number of customers, this load. Default is 1.';
    PropertyHelp[33] := 'Array of 7 coefficients:' + CRLF + CRLF +
        ' First 3 are ZIP weighting factors for real power (should sum to 1)' + CRLF +
        ' Next 3 are ZIP weighting factors for reactive power (should sum to 1)' + CRLF +
        ' Last 1 is cut-off voltage in p.u. of base kV; load is 0 below this cut-off' + CRLF +
        ' No defaults; all coefficients must be specified if using model=8.';
    PropertyHelp[34] := 'Percent of load that is series R-L for Harmonic studies. Default is 50. Remainder is assumed to be parallel R and L. ' +
        'This can have a significant impact on the amount of damping observed in Harmonics solutions.';
    PropertyHelp[35] := 'Relative weighting factor for reliability calcs. Default = 1. Used to designate high priority loads such as hospitals, etc. ' + CRLF + CRLF +
        'Is multiplied by number of customers and load kW during reliability calcs.';
    PropertyHelp[36] := 'Default = 0.50.  Per unit voltage at which the model switches to same as constant Z model (model=2). ' +
        'This allows more consistent convergence at very low voltaes due to opening switches or solving for fault situations.';
    PropertyHelp[37] := 'Special reactance, pu (based on kVA, kV properties), for the series impedance branch in the load model for HARMONICS analysis. ' +
        'Generally used to represent motor load blocked rotor reactance. ' +
        'If not specified (that is, set =0, the default value), the series branch is computed from the percentage of the ' +
        'nominal load at fundamental frequency specified by the %SERIESRL property. ' + CRLF + CRLF +
        'Applies to load model in HARMONICS mode only.' + CRLF + CRLF +
        'A typical value would be approximately 0.20 pu based on kVA * %SeriesRL / 100.0.';
    PropertyHelp[38] := 'X/R ratio of the special harmonics mode reactance specified by the puXHARM property at fundamental frequency. Default is 6. ';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

    PropertyHelp[NumPropsThisClass + 1] := 'Name of harmonic current spectrum for this load.  Default is "defaultload", which is defined when the DSS starts.';

end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLoad.NewObject(const ObjName: String): Integer;
begin
    // Make a new load object and add it to Load class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TLoadObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoad.SetNcondsForConnection;

begin
    with ActiveLoadObj do
    begin
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
        else  {nada}
        end;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoad.InterpretConnection(const S: String);

// Accepts     (checks only min number of chars required}
//    delta or LL           (Case insensitive)
//    Y, wye, or LN
var
    TestS: String;

begin
    with ActiveLoadObj do
    begin
        TestS := lowercase(S);
        case TestS[1] of
            'y', 'w':
                Connection := 0;  {Wye}
            'd':
                Connection := 1;  {Delta or line-Line}
            'l':
                case Tests[2] of
                    'n':
                        Connection := 0;
                    'l':
                        Connection := 1;
                end;
        end;

        SetNCondsForConnection;

        case Connection of
            1:
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
        YprimInvalid[ActiveActor] := TRUE;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLoad.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin
  // continue parsing WITH contents of Parser
    ActiveLoadObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveLoadObj;

    Result := 0;

    with ActiveLoadObj do
    begin
        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;
        Param := Parser[ActorID].StrValue;
        while (Length(Param) > 0) do
        begin
            if (Length(ParamName) = 0) then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[ParamPointer] := Param;

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 580);
                1:
                    Nphases := Parser[ActorID].Intvalue; // num phases
                2:
                    SetBus(1, param);
                3:
                    kVLoadBase := Parser[ActorID].DblValue;
                4:
                    kWBase := Parser[ActorID].DblValue;
                5:
                    PFNominal := Parser[ActorID].DblValue;
                6:
                    FLoadModel := Parser[ActorID].IntValue;
                7:
                    YearlyShape := Param;
                8:
                    DailyShape := Param;
                9:
                    DutyShape := Param;
                10:
                    GrowthShape := Param;
                11:
                    InterpretConnection(Param);
                12:
                    kvarBase := Parser[ActorID].DblValue;
                13:
                    Rneut := Parser[ActorID].DblValue;
                14:
                    Xneut := Parser[ActorID].DblValue;
                15:
                    case lowercase(Param)[1] of
                        'f':
                        begin
                            Fixed := TRUE;
                            ExemptFromLDCurve := FALSE;
                        end;
                        'e':
                        begin
                            Fixed := FALSE;
                            ExemptFromLDCurve := TRUE;
                        end;
                    else
                        Fixed := FALSE;
                        ExemptFromLDCurve := FALSE;
                    end;
                16:
                    LoadClass := Parser[ActorID].IntValue;
                17:
                    VMinPu := Parser[ActorID].DblValue;
                18:
                    VMaxPu := Parser[ActorID].DblValue;
                19:
                    VminNormal := Parser[ActorID].DblValue;
                20:
                    VminEmerg := Parser[ActorID].DblValue;
                21:
                    ConnectedkVA := Parser[ActorID].DblValue;
                22:
                    kVAAllocationFactor := Parser[ActorID].DblValue;
                23:
                    kVABase := Parser[ActorID].DblValue;
                24:
                    FpuMean := Parser[ActorID].DblValue / 100.0;
                25:
                    FpuStdDev := Parser[ActorID].DblValue / 100.0;
                26:
                    FCVRwattFactor := Parser[ActorID].DblValue;
                27:
                    FCVRvarFactor := Parser[ActorID].DblValue;
                28:
                    kWh := Parser[ActorID].DblValue;
                29:
                    kWhdays := Parser[ActorID].DblValue;
                30:
                    Cfactor := Parser[ActorID].DblValue;
                31:
                    CVRShape := Param;
                32:
                    NumCustomers := Parser[ActorID].IntValue;
                33:
                begin
                    SetZIPVSize(7);
                    Parser[ActorID].ParseAsVector(7, ZIPV);
                end;
                34:
                    puSeriesRL := Parser[ActorID].DblValue / 100.0;
                35:
                    RelWeighting := Parser[ActorID].DblValue;
                36:
                    VLowpu := Parser[ActorID].DblValue;
                37:
                    FpuXHarm := Parser[ActorID].DblValue;  // 0 means not set
                38:
                    FXRharmRatio := Parser[ActorID].DblValue;

            else
           // Inherited edits
                ClassEdit(ActiveLoadObj, paramPointer - NumPropsThisClass)
            end;

         // << SIDE EFFECTS >>
         // keep kvar nominal up to date WITH kW and PF
            case ParamPointer of
                1:
                begin
                    SetNcondsForConnection;  // Force Reallocation of terminal info
                    UpdateVoltageBases;
                end;
                3:
                    UpdateVoltageBases;

                4:
                    LoadSpecType := 0;
                5:
                begin
                    PFChanged := TRUE;
                    PFSpecified := TRUE;
                end;
    {Set shape objects;  returns nil if not valid}
    {Sets the kW and kvar properties to match the peak kW demand from the Loadshape}
                7:
                begin
                    YearlyShapeObj := LoadShapeClass[ActorID].Find(YearlyShape);
                    if Assigned(YearlyShapeObj) then
                        with YearlyShapeObj do
                            if UseActual then
                                SetkWkvar(MaxP, MaxQ);
                end;
                8:
                begin
                    DailyShapeObj := LoadShapeClass[ActorID].Find(DailyShape);
                    if Assigned(DailyShapeObj) then
                        with DailyShapeObj do
                            if UseActual then
                                SetkWkvar(MaxP, MaxQ);
                {If Yearly load shape is not yet defined, make it the same as Daily}
                    if YearlyShapeObj = NIL then
                        YearlyShapeObj := DailyShapeObj;
                end;
                9:
                begin
                    DutyShapeObj := LoadShapeClass[ActorID].Find(DutyShape);
                    if Assigned(DutyShapeObj) then
                        with DutyShapeObj do
                            if UseActual then
                                SetkWkvar(MaxP, MaxQ);
                end;
                10:
                    GrowthShapeObj := GrowthShapeClass[ActorID].Find(GrowthShape);

                12:
                begin
                    LoadSpecType := 1;
                    PFSpecified := FALSE;
                end;// kW, kvar
 {*** see set_xfkva, etc           21, 22: LoadSpectype := 3;  // XFKVA*AllocationFactor, PF  }
                23:
                    LoadSpecType := 2;  // kVA, PF
 {*** see set_kwh, etc           28..30: LoadSpecType := 4;  // kWh, days, cfactor, PF }
                31:
                    CVRShapeObj := LoadShapeClass[ActorID].Find(CVRshape);
            end;

            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

        RecalcElementData(ActorID);
        YprimInvalid[ActorID] := TRUE;
    end;

end;

//----------------------------------------------------------------------------
function TLoad.MakeLike(const OtherLoadName: String): Integer;
var
    OtherLoad: TLoadObj;
    i: Integer;
begin
    Result := 0;
   {See IF we can find this line name in the present collection}
    OtherLoad := Find(OtherLoadName);
    if OtherLoad <> NIL then
        with ActiveLoadObj do
        begin

            Connection := OtherLoad.Connection;

            if Fnphases <> OtherLoad.Fnphases then
            begin
                Nphases := OtherLoad.Fnphases;
                SetNcondsForConnection; // Forces reallocation of terminal stuff
                Yorder := Fnconds * Fnterms;
                YprimInvalid[ActiveActor] := TRUE;
            end;

            kVLoadBase := OtherLoad.kVLoadBase;
            Vbase := OtherLoad.Vbase;
            VLowpu := OtherLoad.VLowpu;
            Vminpu := OtherLoad.Vminpu;
            Vmaxpu := OtherLoad.Vmaxpu;
            VBaseLow := OtherLoad.VBaseLow;
            Vbase95 := OtherLoad.Vbase95;
            Vbase105 := OtherLoad.Vbase105;
            kWBase := OtherLoad.kWBase;
            kVAbase := OtherLoad.kVABase;
            kvarBase := OtherLoad.kvarBase;
            LoadSpecType := OtherLoad.LoadSpecType;
            WNominal := OtherLoad.WNominal;
            PFNominal := OtherLoad.PFNominal;
            varNominal := OtherLoad.varNominal;
            Rneut := OtherLoad.Rneut;
            Xneut := OtherLoad.Xneut;
            CVRshape := OtherLoad.CVRshape;
            CVRshapeObj := OtherLoad.CVRshapeObj;
            DailyShape := OtherLoad.DailyShape;
            DailyShapeObj := OtherLoad.DailyShapeObj;
            DutyShape := OtherLoad.DutyShape;
            DutyShapeObj := OtherLoad.DutyShapeObj;
            YearlyShape := OtherLoad.YearlyShape;
            YearlyShapeObj := OtherLoad.YearlyShapeObj;
            GrowthShape := OtherLoad.GrowthShape;
            GrowthShapeObj := OtherLoad.GrowthShapeObj;
//        Spectrum       := OtherLoad.Spectrum;       in base class now
//       SpectrumObj    := OtherLoad.SpectrumObj;
            LoadClass := OtherLoad.LoadClass;
            NumCustomers := OtherLoad.NumCustomers;
            FLoadModel := OtherLoad.FLoadModel;
            Fixed := OtherLoad.Fixed;
            ExemptFromLDCurve := OtherLoad.ExemptFromLDCurve;
            FkVAAllocationFactor := OtherLoad.FkVAAllocationFactor;
            FConnectedkVA := OtherLoad.FConnectedkVA;
            FCVRwattFactor := OtherLoad.FCVRwattFactor;
            FCVRvarFactor := OtherLoad.FCVRvarFactor;
            ShapeIsActual := OtherLoad.ShapeIsActual;
            puSeriesRL := OtherLoad.puSeriesRL;
            RelWeighting := OtherLoad.RelWeighting;

            SetZIPVSize(OtherLoad.nZIPV);
            for i := 1 to FnZIPV do
                ZIPV^[i] := OtherLoad.ZIPV^[i];

            ClassMakeLike(OtherLoad);  // Take care of inherited class properties


            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherLoad.PropertyValue[i];

            Result := 1;
        end
    else
        DoSimpleMsg('Error in Load MakeLike: "' + OtherLoadName + '" Not Found.', 581);

end;

//----------------------------------------------------------------------------
function TLoad.Init(Handle: Integer; ActorID: Integer): Integer;
var
    p: TLoadObj;

begin

    if Handle = 0 then
    begin  // init all load objects
        p := elementList.First;
        while p <> NIL do
        begin
            p.Randomize(0);
            p := elementlist.Next;
        end;
    end
    else
    begin
        Active := Handle;
        p := GetActiveObj;
        p.Randomize(0);
    end;

    DoSimpleMsg('Need to finish implementation TLoad.Init', -1);
    Result := 0;
end;

//----------------------------------------------------------------------------
constructor TLoadObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType;

    Fnphases := 3;
    Fnconds := 4;  // defaults to wye  so it has a 4th conductor
    Yorder := 0;  // To trigger an initial allocation
    Nterms := 1;  // forces allocations
    kWBase := 10.0;
    kvarBase := 5.0;
    PFNominal := 0.88;
    kVABase := kWBase / PFNominal;
    LoadSpecType := 0;
    Rneut := -1.0;  // signify neutral is open
    Xneut := 0.0;

    YearlyShape := '';
    YearlyShapeObj := NIL;  // IF YearlyShapeobj = nil THEN the load alway stays nominal * global multipliers
    DailyShape := '';
    DailyShapeObj := NIL;  // IF DaillyShapeobj = nil THEN the load alway stays nominal * global multipliers
    DutyShape := '';
    DutyShapeObj := NIL;  // IF DutyShapeobj = nil THEN the load alway stays nominal * global multipliers
    Growthshape := '';
    GrowthShapeObj := NIL;  // IF grwothshapeobj = nil THEN the load alway stays nominal * global multipliers
    CVRShape := '';
    CVRShapeObj := NIL;
    Connection := 0;    // Wye (star)
    FLoadModel := 1;  // changed from 2 RCD {easiest to solve}
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
    Fixed := FALSE;
    ExemptFromLDCurve := FALSE;

    FpuXHarm := 0.0;  // zero signifies not specified.
    FXRHarmRatio := 6.0;


    FpuMean := 0.5;
    FpuStdDev := 0.1;
    UE_Factor := 0.0;
    EEN_Factor := 0.0;
    Spectrum := 'defaultload';  // override base class definition
    HarmMag := NIL;
    HarmAng := NIL;
    puSeriesRL := 0.50;
    ZIPV := NIL;
    SetZIPVSize(0);
    FPhaseCurr := NIL;  // storage for intermediate current computation
                          // allocated in Recalcelementdata

    InitPropertyValues(0);

    RecalcElementData(ActiveActor);

end;


//----------------------------------------------------------------------------
destructor TLoadObj.Destroy;
begin
    YPrimOpenCond.Free;
    ReallocMem(HarmMag, 0);
    ReallocMem(HarmAng, 0);
    ReallocMem(ZIPV, 0);
    Reallocmem(FPhaseCurr, 0);

    inherited Destroy;
end;

procedure TLoadObj.SetZIPVSize(n: Integer);
begin
    FnZIPV := n;
    ReAllocMem(ZIPV, Sizeof(ZIPV^[1]) * FnZIPV);
end;

//----------------------------------------------------------------------------
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
    else
       {nada}
    end;
end;

//----------------------------------------------------------------------------
procedure TLoadObj.CalcDailyMult(Hr: Double);

begin
    if DailyShapeObj <> NIL then
    begin
        ShapeFactor := DailyShapeObj.GetMult(Hr);
        ShapeIsActual := DailyShapeObj.UseActual;
    end
    else
        ShapeFactor := Cmplx(1.0, 1.0);  // Default to no daily variation
end;


//----------------------------------------------------------------------------
procedure TLoadObj.CalcDutyMult(Hr: Double);

begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMult(Hr);
        ShapeIsActual := DutyShapeObj.UseActual;
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult IF no duty curve specified
end;

//----------------------------------------------------------------------------
procedure TLoadObj.CalcYearlyMult(Hr: Double);

begin
{Yearly curve is assumed to be hourly only}
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMult(Hr);
        ShapeIsActual := YearlyShapeObj.UseActual;
    end
    else
        ShapeFactor := Cmplx(1.0, 1.0);
                          // Defaults to no variation
end;

//----------------------------------------------------------------------------
procedure TLoadObj.CalcCVRMult(Hr: Double);

var
    CVRFactor: Complex;

begin
  {CVR curve is assumed to be used in a yearly simulation}
    if CVRShapeObj <> NIL then
    begin
        CVRFactor := CVRShapeObj.GetMult(Hr);    {Complex}
        FCVRWattFactor := CVRFactor.re;
        FCVRvarFactor := CVRFactor.im;
    end;
   {Else FCVRWattFactor, etc. remain unchanged}
end;

//----------------------------------------------------------------------------
function TLoadObj.GrowthFactor(Year: Integer; ActorID: Integer): Double;

begin
    if Year = 0 then
        LastGrowthFactor := 1.0  // default all to 1 in year 0 ; use base values
    else
    begin
        if GrowthShapeObj = NIL then
            LastGrowthFactor := ActiveCircuit[ActorID].DefaultGrowthFactor
        else
        if Year <> LastYear then    // Search growthcurve
            LastGrowthFactor := GrowthShapeObj.GetMult(Year);
    end;

    Result := LastGrowthFactor;  // for Now
end;


//----------------------------------------------------------------------------
procedure TLoadObj.SetkWkvar(const PkW, Qkvar: Double);
begin
    kWBase := PkW;
    kvarbase := Qkvar;
    if PFSpecified then
        LoadSpecType := 0
    else
        LoadSpecType := 1;
end;

procedure TLoadObj.SetNominalLoad(ActorID: Integer);
var
    Factor: Double;


begin
    ShapeFactor := CDOUBLEONE;
    ShapeIsActual := FALSE;
    with ActiveCircuit[ActorID].Solution do
        if Fixed then
        begin
            Factor := GrowthFactor(Year, ActorID);   // For fixed loads, consider only growth factor
        end
        else
            case Mode of
                SNAPSHOT,
                HARMONICMODE:
                    if ExemptFromLDCurve then
                        Factor := GrowthFactor(Year, ActorID)
                    else
                        Factor := ActiveCircuit[ActorID].LoadMultiplier * GrowthFactor(Year, ActorID);
                DAILYMODE:
                begin
                    Factor := GrowthFactor(Year, ActorID);
                    if not ExemptFromLDCurve then
                        Factor := Factor * ActiveCircuit[ActorID].LoadMultiplier;
                    CalcDailyMult(DynaVars.dblHour);
                end;
                YEARLYMODE:
                begin
                    Factor := ActiveCircuit[ActorID].LoadMultiplier * GrowthFactor(Year, ActorID);
                    CalcYearlyMult(DynaVars.dblHour);
                    if FLoadModel = 4 then
                        CalcCVRMult(DynaVars.dblHour);
                end;
                DUTYCYCLE:
                begin
                    Factor := GrowthFactor(Year, ActorID);
                    if not ExemptFromLDCurve then
                        Factor := Factor * ActiveCircuit[ActorID].LoadMultiplier;
                    CalcDutyMult(DynaVars.dblHour);
                end;
                GENERALTIME,
                DYNAMICMODE:
                begin
                    Factor := GrowthFactor(Year, ActorID);
                    if not ExemptFromLDCurve then
                        Factor := Factor * ActiveCircuit[ActorID].LoadMultiplier;
                           // This mode allows use of one class of load shape
                    case ActiveCircuit[ActorID].ActiveLoadShapeClass of
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
                MONTECARLO1:
                begin
                    Randomize(RandomType);
                    Factor := RandomMult * GrowthFactor(Year, ActorID);
                    if not ExemptFromLDCurve then
                        Factor := Factor * ActiveCircuit[ActorID].LoadMultiplier;
                end;

                MONTECARLO2,
                MONTECARLO3,
                LOADDURATION1,
                LOADDURATION2:
                begin
                    Factor := GrowthFactor(Year, ActorID);
                    CalcDailyMult(DynaVars.dblHour);
                    if not ExemptFromLDCurve then
                        Factor := Factor * ActiveCircuit[ActorID].LoadMultiplier;
                end;
                PEAKDAY:
                begin
                    Factor := GrowthFactor(Year, ActorID);
                    CalcDailyMult(DynaVars.dblHour);
                end;
                AUTOADDFLAG:
                    Factor := GrowthFactor(Year, ActorID);  // Loadmult = 1.0 by default
            else
                Factor := GrowthFactor(Year, ActorID)    // defaults to Base kW * growth
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

    Yeq := CDivReal(Cmplx(WNominal, -VarNominal), Sqr(Vbase));
    if (Vminpu <> 0.0) then
        Yeq95 := CDivReal(Yeq, sqr(Vminpu))   // at 95% voltage
    else
        Yeq95 := CZERO;

    if (Vmaxpu <> 0.0) then
        Yeq105 := CDivReal(Yeq, sqr(Vmaxpu))   // at 105% voltage
    else
        Yeq105 := Yeq;

    if (Vmaxpu <> 0.0) then
        Yeq105I := CDivReal(Yeq, Vmaxpu)   // at 105% voltage for Constant I ***Added by Celso & Paulo
    else
        Yeq105I := Yeq;                    // **Added by Celso & Paulo

    {New code to help with convergence at low voltages}
    ILow := (CmulReal(Yeq, VbaseLow));
    I95 := (CmulReal(Yeq95, Vbase95));
    M95 := CDivReal(Csub(I95, ILow), (VBase95 - VBaseLow)); // (I95 - ILow)/(Vbase95 - VbaseLow);
    M95 := CDivReal(Csub(I95, ILow), (VBase95 - VBaseLow)); // (I95 - ILow)/(Vbase95 - VbaseLow);    ***Added by Celso & Paulo
    IBase := (CmulReal(Yeq, VBase));                          // ***Added by Celso & Paulo
    M95I := CDivReal(Csub(IBase, ILow), (VBase95 - VBaseLow)); // (IBase - ILow)/(Vbase95 - VbaseLow);    ***Added by Celso & Paulo

end;

//----------------------------------------------------------------------------
procedure TLoadObj.RecalcElementData(ActorID: Integer);


begin

    VBaseLow := VLowpu * VBase;
    VBase95 := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;


    {Set kW and kvar from root values of kVA and PF}

    case LoadSpecType of
        0:
        begin  {kW, PF}
            kvarBase := kWBase * SQRT(1.0 / SQR(PFNominal) - 1.0);
            if PFNominal < 0.0 then
                kvarBase := -kvarBase;
            kVABase := SQRT(SQR(kWbase) + SQR(kvarBase));
        end;
        1:
        begin  {kW, kvar -- need to set PFNominal}
            kVABase := SQRT(SQR(kWbase) + SQR(kvarBase));
            if kVABase > 0.0 then
            begin
                PFNominal := kWBase / kVABase;
               {If kW and kvar are different signs, PF is negative}
                if kvarbase <> 0.0 then
                    PFNominal := PFNominal * Sign(kWbase * kvarbase);
            end;
          {Else leave it as it is}
        end;
        2:
        begin  {kVA, PF}
            kWbase := kVABase * Abs(PFNominal);
            kvarBase := kWBase * SQRT(1.0 / SQR(PFNominal) - 1.0);
            if PFNominal < 0.0 then
                kvarBase := -kvarBase;
        end;
        3, 4:
            if PFChanged then
            begin  // Recompute kvarBase
                kvarBase := kWBase * SQRT(1.0 / SQR(PFNominal) - 1.0);
                if PFNominal < 0.0 then
                    kvarBase := -kvarBase;
                kVABase := SQRT(SQR(kWbase) + SQR(kvarBase));
            end;


{ done automagically in Property set...      3, 4: ComputeAllocatedLoad;    }
    else
    end;

    SetNominalLoad(ActorID);

    {Now check for errors.  IF any of these came out nil and the string was not nil, give warning}
    if CompareText(YearlyShape, 'none') = 0 then
        YearlyShape := '';
    if CompareText(DailyShape, 'none') = 0 then
        DailyShape := '';
    if CompareText(DutyShape, 'none') = 0 then
        DutyShape := '';

    if YearlyShapeObj = NIL then
        if Length(YearlyShape) > 0 then
            DoSimpleMsg('WARNING! Yearly load shape: "' + YearlyShape + '" Not Found.', 583);
    if DailyShapeObj = NIL then
        if Length(DailyShape) > 0 then
            DoSimpleMsg('WARNING! Daily load shape: "' + DailyShape + '" Not Found.', 584);
    if DutyShapeObj = NIL then
        if Length(DutyShape) > 0 then
            DoSimpleMsg('WARNING! Duty load shape: "' + DutyShape + '" Not Found.', 585);
    if GrowthShapeObj = NIL then
        if Length(GrowthShape) > 0 then
            DoSimpleMsg('WARNING! Yearly Growth shape: "' + GrowthShape + '" Not Found.', 586);
    if CVRShapeObj = NIL then
        if Length(CVRShape) > 0 then
            DoSimpleMsg('WARNING! CVR Shape shape: "' + CVRShape + '" Not Found.', 586);

    SpectrumObj := SpectrumClass[ActorID].Find(Spectrum);
    if SpectrumObj = NIL then
        DoSimpleMsg('ERROR! Spectrum "' + Spectrum + '" Not Found.', 587);

    if Rneut < 0.0 then  // flag FOR open neutral
        YNeut := Cmplx(0.0, 0.0)
    else
    if (Rneut = 0.0) and (Xneut = 0.0) then // Solidly Grounded
        YNeut := Cmplx(1.0e6, 0.0)  // 1 microohm resistor
    else
        YNeut := Cinv(Cmplx(Rneut, XNeut));

    varBase := 1000.0 * kvarBase / Fnphases;
    YQFixed := -varBase / SQR(VBase);

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);
    Reallocmem(FPhaseCurr, SizeOf(FPhaseCurr^[1]) * FNphases);

    PFChanged := FALSE;

end;

//----------------------------------------------------------------------------
procedure TLoadObj.CalcYPrimMatrix(Ymatrix: TcMatrix; ActorID: Integer);

var
    Y, Yij,
    ZSeries: Complex;
    i, j: Integer;
    FreqMultiplier: Double;
    XseriesOhms: Double;

begin

    FYprimFreq := ActiveCircuit[ActorID].Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

    with ActiveCircuit[ActorID].Solution do
        if IsHarmonicModel and (Frequency <> ActiveCircuit[ActorID].Fundamental) then
        begin     // Harmonic Mode  and other than fundamental frequency
            if ActiveCircuit[ActorID].NeglectLoadY then
            begin
                     {Just a small value so things don't die and we get the actual injection current out the terminal}
                Y := cmplx(Epsilon, 0.0)
            end
            else
                // compute equivalent Y assuming some of the load is series R-L and the rest is parallel R-L
            begin
                   // Parallel R-L part of the Load model for harmonics mode
                   // Based in equivalent Y at 100% voltage
                Y := CmulReal(Yeq, (1.0 - puSeriesRL));
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
                        Zseries := Cinv(CmulReal(Yeq, puSeriesRL));

                    Zseries.im := Zseries.im * FreqMultiplier;  {Correct reactive part for frequency}
                    Y := Cadd(Cinv(ZSeries), Y); // convert to admittance and add into Y
                end;

            end;
        end
        else
        begin   // not Harmonic mode
            Y := Yeq;
            Y.im := Y.im / FreqMultiplier;  {Correct reactive part for frequency}
        end;


    Yij := Cnegate(Y);

    case Connection of

        0:
        begin // WYE
            for i := 1 to Fnphases do
            begin
                Ymatrix.SetElement(i, i, Y);
                Ymatrix.AddElement(Fnconds, Fnconds, Y);
                Ymatrix.SetElemsym(i, Fnconds, Yij);
            end;
            Ymatrix.AddElement(Fnconds, Fnconds, YNeut);  // Neutral

               { If neutral is floating, make sure there is some small
                 connection to ground  by increasing the last diagonal slightly }
            if Rneut < 0.0 then
                Ymatrix.SetElement(Fnconds, Fnconds, Cmulreal(Ymatrix.GetElement(Fnconds, Fnconds), 1.000001));
        end;
        1:
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


//----------------------------------------------------------------------------
procedure TLoadObj.CalcYPrim(ActorID: Integer);


// If doing an analysis that requires the load to be modeled as an impedance
// then put all in.

var
    i: Integer;

begin

// Build only YPrim Shunt for a Load  then Copy to YPrim
// Build a dummy Yprim Series so that CalcV does not fail
    if YprimInvalid[ActorID] then
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

    if ActiveCircuit[ActorID].Solution.LoadModel = POWERFLOW then
    begin

        SetNominalLoad(ActorID);         // same as admittance model
        CalcYPrimMatrix(YPrim_Shunt, ActorID);

    end
    else
    begin   // ADMITTANCE model wanted

        SetNominalLoad(ActorID);
        CalcYPrimMatrix(YPrim_Shunt, ActorID);

    end;

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));

    YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
    inherited CalcYPrim(ActorID);

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TLoadObj.StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);
 {Put the current into the proper location according to connection}

var
    j: Integer;

begin
    case Connection of

        0:
        begin  //Wye
            Caccum(TermArray^[i], Cnegate(Curr));
            Caccum(TermArray^[Fnconds], Curr); // Neutral
        end;

        1:
        begin //DELTA
            Caccum(TermArray^[i], Cnegate(Curr));
            j := i + 1;
            if j > Fnconds then
                j := 1;  // rotate the phases
            Caccum(TermArray^[j], Curr);
        end;
    end;
end;

procedure TLoadObj.UpdateVoltageBases;
begin
    with ActiveLoadObj do
        case Connection of
            1:
                VBase := kVLoadBase * 1000.0;
        else  {wye}
            case Fnphases of
                2, 3:
                    VBase := kVLoadBase * InvSQRT3x1000;
            else
                VBase := kVLoadBase * 1000.0; {1-phase or unknown}
            end;
        end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TLoadObj.DoConstantPQLoad(ActorID: Integer);

var
    i: Integer;
    Curr: Complex;
    V: Complex;
    Vmag: Double;

begin

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        VMag := Cabs(V);

        if VMag <= VBaseLow then
            Curr := Cmul(Yeq, V)  // Below VbaseZ resort to linear equal to Yprim contribution
        else
        if VMag <= VBase95 then
            Curr := Cmul(InterpolateY95_YLow(Vmag), V)   //  Voltage between Vminpu and Vlow
        else
        if VMag > VBase105 then
            Curr := Cmul(Yeq105, V)  // above 105% use an impedance model
        else
            Curr := Conjg(Cdiv(Cmplx(WNominal, varNominal), V));  // Above 95%, constant PQ

      // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
        FPhaseCurr^[i] := Curr;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE, ActorID);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TLoadObj.DoConstantZLoad(ActorID: Integer);
var
    i: Integer;
    Curr: Complex;

begin

// Assume Yeq is kept up to date

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        Curr := Cmul(Yeq, Vterminal^[i]);

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
        FPhaseCurr^[i] := Curr;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE, ActorID);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TLoadObj.DoMotorTypeLoad(ActorID: Integer);
// Constant P, quadratic Q
var
    i: Integer;
    Curr: Complex;
    V: Complex;
    VMag: Double;

begin

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        VMag := Cabs(V);
        if VMag <= VBaseLow then
            Curr := Cmul(Yeq, V)  // Below VbaseZ resort to linear equal to Yprim contribution
        else
        if VMag <= VBase95 then
            Curr := Cmul(InterpolateY95_YLow(Vmag), V)   //  Voltage between Vminpu and Vlow
        else
        if VMag > VBase105 then
            Curr := Cmul(Yeq105, V)  // above 105% use an impedance model
        else
        begin
            Curr := Conjg(Cdiv(Cmplx(WNominal, 0.0), V));  // Above 95%, constant P
            Caccum(Curr, Cmul(Cmplx(0.0, Yeq.im), V));  // add in Q component of current
        end;

      // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
        FPhaseCurr^[i] := Curr;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE, ActorID);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

procedure TLoadObj.DoConstantILoad(ActorID: Integer);

// Constant Current Load

var
    i: Integer;
    V: Complex;
    Vmag: Double;
    Curr: Complex;

begin

// Computes the current assuming the voltage mag is Vbase
// Just uses the phase angle off the voltage

{
   Injection = [s/v]* = [ (P+jQ)/(Vbase * V/|V|)]*
}


    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];

        Vmag := Cabs(V);
        if VMag <= VBaseLow then
            Curr := Cmul(Yeq, V)  // Below VbaseZ resort to linear equal to Yprim contribution
        else
        if VMag <= VBase95 then
            Curr := Cmul(InterpolateY95I_YLow(Vmag), V)   //  Voltage between Vminpu and Vlow    ***Added by Celso & Paulo
        else
        if VMag > VBase105 then
            Curr := Cmul(Yeq105I, V)  // above 105% use an impedance model
        else
        begin
            Curr := Conjg(Cdiv(Cmplx(WNominal, varNominal), CMulReal(CDivReal(V, Vmag), Vbase)));
        end;

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
        FPhaseCurr^[i] := Curr;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE, ActorID);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;

end;

procedure TLoadObj.DoZIPVModel(ActorID: Integer);
var
    i: Integer;
    Curr: Complex;
    CurrZ: Complex;
    CurrI: Complex;
    CurrP: Complex;
    V: Complex;
    Vmag: Double;
    vx, evx, yv: Double;
begin
    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        VMag := Cabs(V);

    { May 31, 2016 changed to linear model below VbaseLow -- converges better for low voltage}
        if VMag <= VBaseLow then
            Curr := Cmul(Yeq, V)  // Below VbaseZ resort to linear equal to Yprim contribution
        else
        begin
            if VMag <= VBase95 then
            begin
                CurrZ := Cmul(Cmplx(Yeq.re * ZIPV^[1], Yeq.im * ZIPV^[4]), V);    // ***Changed by Celso & Paulow
                CurrP := Cmul(Cmplx(InterpolateY95_YLow(Vmag).re * ZIPV^[3], InterpolateY95_YLow(Vmag).im * ZIPV^[6]), V);   // ***Changed by Celso & Paulo
                CurrI := Cmul(Cmplx(InterpolateY95I_YLow(Vmag).re * ZIPV^[2], InterpolateY95I_YLow(Vmag).im * ZIPV^[5]), V);  // ***Changed by Celso & Paulo
                Curr := CAdd(CurrZ, CAdd(CurrI, CurrP));   // ***Changed by Celso & Paulo
            end
            else
            if VMag > VBase105 then
            begin
                CurrZ := Cmul(Cmplx(Yeq.re * ZIPV^[1], Yeq.im * ZIPV^[4]), V);   // ***Changed by Celso & Paulo
                CurrP := Cmul(Cmplx(Yeq105.re * ZIPV^[3], Yeq105.im * ZIPV^[6]), V);         // ***Changed by Celso & Paulo
                CurrI := Cmul(Cmplx(Yeq105I.re * ZIPV^[2], Yeq105I.im * ZIPV^[5]), V);       // ***Changed by Celso & Paulo
                Curr := CAdd(CurrZ, CAdd(CurrI, CurrP));
            end
            else
            begin
                CurrZ := Cmul(Cmplx(Yeq.re * ZIPV^[1], Yeq.im * ZIPV^[4]), V);
                CurrI := Conjg(Cdiv(Cmplx(WNominal * ZIPV^[2], varNominal * ZIPV^[5]), CMulReal(CDivReal(V, Cabs(V)), Vbase)));
                CurrP := Conjg(Cdiv(Cmplx(WNominal * ZIPV^[3], varNominal * ZIPV^[6]), V));
                Curr := CAdd(CurrZ, CAdd(CurrI, CurrP));
            end;

      // low-voltage drop-out
            if ZIPV^[7] > 0.0 then
            begin
                vx := 500.0 * (Vmag / Vbase - ZIPV^[7]);
                evx := exp(2 * vx);
                yv := 0.5 * (1 + (evx - 1) / (evx + 1));
                Curr := CMulReal(Curr, yv);
            end;
        end;

    // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
        FPhaseCurr^[i] := Curr;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE, ActorID);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TLoadObj.DoCVRModel(ActorID: Integer);
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

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;

    try

        for i := 1 to Fnphases do
        begin
            V := Vterminal^[i];
            Vmag := Cabs(V);

            if VMag <= VBaseLow then
                Curr := Cmul(Yeq, V)  // Below VbaseZ resort to linear equal to Yprim contribution
            else
            if VMag <= VBase95 then
                Curr := Cmul(InterpolateY95_YLow(Vmag), V)   //  Voltage between Vminpu and Vlow
            else
            if VMag > VBase105 then
                Curr := Cmul(Yeq105, V)  // above 105% use an impedance model
            else
            begin
                VRatio := Vmag / VBase;    // vbase is l-n FOR wye and l-l FOR delta


              // Linear factor adjustment does not converge for some reason while power adjust does easily
                 // WattFactor := (1.0 + FCVRwattFactor*(Vmag/VBase - 1.0));
                if FCVRWattFactor <> 1.0 then
                    WattFactor := math.power(VRatio, FCVRWattFactor)
                else
                    WattFactor := Vratio;  // old value (in error): 1.0;
                if WattFactor > 0.0 then
                    Curr := Conjg(Cdiv(Cmplx(WNominal * WattFactor, 0.0), V))
                else
                    Curr := CZERO; // P component of current

                if Vmag = 0.0 then
                    Cvar := CZERO    // Trap divide by zero error
              {Compute Q component of current}
                else
                if FCVRvarFactor = 2.0 then
                begin  {Check for easy, quick ones first}
                    Cvar := Cmul(Cmplx(0.0, Yeq.im), V); // 2 is same as Constant impedance
                end
                else
                if FCVRvarFactor = 3.0 then
                begin
                    VarFactor := math.intpower(VRatio, 3);
                    Cvar := Conjg(Cdiv(Cmplx(0.0, VarNominal * VarFactor), V));
                end
                else
                begin
                  {Other Var factor code here if not squared or cubed}
                    VarFactor := math.power(VRatio, FCVRvarFactor);
                    Cvar := Conjg(Cdiv(Cmplx(0.0, VarNominal * VarFactor), V));
                end;
                Caccum(Curr, Cvar);  // add in Q component of current
            end;

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
            FPhaseCurr^[i] := Curr;

            StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
            set_ITerminalUpdated(TRUE, ActorID);
            StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
        end;
    except
        On E: Exception do
        begin
            DoSimpleMsg(Format('Error in Load.%s: %s ', [Name, E.Message]), 5871);
            raise;
        end;
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TLoadObj.DoFixedQ(ActorID: Integer);
// Constant P, Fixed Q  Q is always kvarBase
var
    i: Integer;
    Curr,
    V: Complex;
    Vmag: Double;

begin


    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        VMag := Cabs(V);
        if VMag <= VBaseLow then
            Curr := Cmul(Yeq, V)  // Below VbaseZ resort to linear equal to Yprim contribution
        else
        if VMag <= VBase95 then
            Curr := Cmul(Cmplx(Yeq95.re, YQfixed), V)  // Below 95% use an impedance model
        else
        if VMag > VBase105 then
            Curr := Cmul(Cmplx(Yeq105.re, YQfixed), V)  // above 105% use an impedance model
        else
        begin
            Curr := Conjg(Cdiv(Cmplx(WNominal, varBase), V));
        end;

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
        FPhaseCurr^[i] := Curr;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE, ActorID);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TLoadObj.DoFixedQZ(ActorID: Integer);
// Constant P, Fixed Q  Q is always a fixed Z derived from kvarBase
var
    i: Integer;
    Curr,
    V: Complex;
    Vmag: Double;

begin

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        Vmag := Cabs(V);
        if VMag <= VBaseLow then
            Curr := Cmul(Yeq, V)  // Below VbaseZ resort to linear equal to Yprim contribution
        else
        if Vmag <= VBase95 then
            Curr := Cmul(Cmplx(Yeq95.re, YQfixed), V)  // Below 95% use an impedance model
        else
        if VMag > VBase105 then
            Curr := Cmul(Cmplx(Yeq105.re, YQfixed), V)
        else
        begin
            Curr := Conjg(Cdiv(Cmplx(WNominal, 0.0), V)); // P component of current
            Caccum(Curr, Cmul(Cmplx(0.0, YQFixed), V));  // add in Q component of current
        end;

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
        FPhaseCurr^[i] := Curr;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE, ActorID);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TLoadObj.DoHarmonicMode;
{Compute Injection Current Only when in harmonics mode}
{Assumes spectrum is an ideal current source based on the fundamental current and spectrum}

var
    i: Integer;
    Curr, Mult: Complex;
    LoadHarmonic: Double;

begin

   {Don't calc Vterminal here because it could be undefined!}
    ZeroInjCurrent;
    ZeroIterminal;
    with ActiveCircuit[ActorID].Solution do
    begin
        LoadHarmonic := Frequency / LoadFundamental;    // Loadfundamental = frequency of solution when Harmonic mode entered
        Mult := SpectrumObj.GetMult(LoadHarmonic);
        for i := 1 to FNphases do
        begin
            Curr := CmulReal(Mult, HarmMag^[i]); // Get base harmonic magnitude
            RotatePhasorDeg(Curr, LoadHarmonic, HarmAng^[i]);   // Time shift by fundamental
          // don't need to save Curr here like we do in Power Flow modes
            StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into InjCurrent array taking into account connection
            StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
          // NOTE: This is the value of ITerminal a Monitor will capture in Harmonics mode .. it captures the harmonic injection
            set_ITerminalUpdated(TRUE, ActorID);
        end;

    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TLoadObj.AllTerminalsClosed: Boolean;

var
    i, j: Integer;

begin
    Result := TRUE;
    for i := 1 to Nterms do
        for j := 1 to NConds do
            if not Terminals^[i].Conductors^[j].Closed then
            begin
                Result := FALSE;
                Exit;
            end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TLoadObj.CalcVTerminalPhase(ActorID: Integer);

var
    i, j: Integer;

begin

{ Establish phase voltages and stick in Vtemp}
    case Connection of

        0:
        begin
            with ActiveCircuit[ActorID].Solution do
                for i := 1 to Fnphases do
                    Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[Fnconds]);
        end;

        1:
        begin
            with ActiveCircuit[ActorID].Solution do
                for i := 1 to Fnphases do
                begin
                    j := i + 1;
                    if j > Fnconds then
                        j := 1;
                    Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[j]);
                end;
        end;

    end;

    LoadSolutionCount := ActiveCircuit[ActorID].Solution.SolutionCount;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TLoadObj.CalcLoadModelContribution(ActorID: Integer);
// Calculates total load current and adds it properly into the InjCurrent array

// Need to implement DynamicMode sometime ...

begin
    set_ITerminalUpdated(FALSE, ActorID);
    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
          {IF      IsDynamicModel THEN  DoDynamicMode
          ELSE} if IsHarmonicModel and (Frequency <> Fundamental) then
            DoHarmonicMode(ActorID)
        else
           //  compute total Load currents and Add into InjCurrent array;
            case FLoadModel of

                1:
                    DoConstantPQLoad(ActorID); // normal load-flow type load
                2:
                    DoConstantZLoad(ActorID);
                3:
                    DoMotorTypeLoad(ActorID);  // Constant P, Quadratic Q;
                4:
                    DoCVRModel(ActorID);       // mixed motor/resistive load   with CVR factors
                5:
                    DoConstantILoad(ActorID);
                6:
                    DoFixedQ(ActorID);         // Fixed Q
                7:
                    DoFixedQZ(ActorID);        // Fixed, constant Z Q
                8:
                    DoZIPVModel(ActorID);
            else
                DoConstantZLoad(ActorID);     // FOR now, until we implement the other models.
            end;

    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TLoadObj.CalcInjCurrentArray(ActorID: Integer);

// Fill InjCurrent array with the current values to use for injections.

var
    i, j, k: Integer;

begin

// IF a terminal is open, THEN standard load models don't apply, so check it out first

    if AllTerminalsClosed then
    begin

// Now Get Injection Currents

        CalcLoadModelContribution(ActorID)

    end

    else
    begin

   /// THIS MAY NOT WORK !!! WATCH FOR BAD RESULTS

   // some terminals not closed  use admittance model FOR injection
        if OpenLoadSolutionCount <> ActiveCircuit[ActorID].Solution.SolutionCount then
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
            CalcYPrimMatrix(YPrimOpenCond, ActorID);

        {Now Account FOR the Open Conductors}
        {For any conductor that is open, zero out row and column}
            with YPrimOpenCond do
            begin
                k := 0;
                for i := 1 to Fnterms do
                begin
                    for j := 1 to Fnconds do
                    begin
                        if not Terminals^[i].Conductors^[j].Closed then
                        begin
                            ZeroRow(j + k);
                            ZeroCol(j + k);
                            SetElement(j + k, j + k, Cmplx(1.0e-12, 0.0));  // In case node gets isolated
                        end;
                    end;
                    k := k + Fnconds;
                end;
            end;
            OpenLoadSolutionCount := ActiveCircuit[ActorID].Solution.SolutionCount;

        end;

        ComputeVTerminal(ActorID);
        YPrimOpenCond.MVmult(ComplexBuffer, Vterminal);
        for i := 1 to Yorder do
            ComplexBuffer^[i] := Cnegate(ComplexBuffer^[i]);
    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TLoadObj.GetTerminalCurrents(Curr: pComplexArray; ActorID: Integer);

// Always return total terminal currents in the Curr array

begin

    with ActiveCircuit[ActorID].Solution do
    begin
        if IterminalSolutionCount[ActorID] <> ActiveCircuit[ActorID].Solution.SolutionCount then
        begin     // recalc the contribution
            CalcLoadModelContribution(ActorID);  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr, ActorID);
    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TLoadObj.InjCurrents(ActorID: Integer): Integer;

// Get the injection currents and add them directly into the Currents array

begin

    Result := 0;
    if Enabled then
        with ActiveCircuit[ActorID].Solution do
        begin
            if LoadsNeedUpdating then
                SetNominalLoad(ActorID); // Set the nominal kW, etc. for the type of solution being done
            CalcInjCurrentArray(ActorID);
            Result := inherited Injcurrents(ActorID);  // Add into Global Currents Array
        end;

end;

function TLoadObj.InterpolateY95_YLow(const Vmag: Double): Complex;
{
  For Vmag between V95 and Vlow, interpolate for equivalent  Y
}
begin

    Result := CDivReal(Cadd(ILow, CmulReal(M95, Vmag - VbaseLow)), Vmag);   //(Ilow + M95 * (Vmag - VBaseLow))/Vmag)

{****
    WriteDLLDebugFile(Format('Iter=%d, Name="%s", Vmag=%.6g, Yeq=%.6g +j %.6g',
             [ActiveCircuit.Solution.iteration, Name, Vmag, Result.re, Result.im]));
 }
end;

function TLoadObj.InterpolateY95I_YLow(const Vmag: Double): Complex;      // ***Added by Celso & Paulo
{
  For Vmag between V95 and Vlow, interpolate for equivalent  Y
}
begin

    Result := CDivReal(Cadd(ILow, CmulReal(M95I, Vmag - VbaseLow)), Vmag);   //(Ilow + M95I * (Vmag - VBaseLow))/Vmag)   // ***Changed by Celso & Paulo

{****
    WriteDLLDebugFile(Format('Iter=%d, Name="%s", Vmag=%.6g, Yeq=%.6g +j %.6g',
             [ActiveCircuit[ActiveActor].Solution.iteration, Name, Vmag, Result.re, Result.im]));
 }
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TLoadObj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);

// Gets the injection  currents for the last solution performed
// Do not call SetNominalLoad, as that may change the load values

var
    i: Integer;

begin

    try
        if Enabled then
        begin
            CalcInjCurrentArray(ActorID);
       // Copy into buffer array
            for i := 1 to Yorder do
                Curr^[i] := InjCurrent^[i];
        end
        else
            for i := 1 to Yorder do
                Curr^[i] := cZero;
    except
        ON E: Exception do
            DoErrorMsg('Load Object: "' + Name + '" in GetInjCurrents FUNCTION.',
                E.Message,
                'Current buffer may not big enough.', 588);
    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TLoadObj.Get_Unserved(ActorID: Integer): Boolean;
var
    i: Integer;
    Vpu,
    Vmag: Double;
    NormMinCriteria,
    EmergMinCriteria: Double;
  {  Line overload takes precedence.
     Assumes that low voltage is due to overloaded line.
     IF voltage is below Emergency minumum, it is counted as  unserved.
  }

begin
    Result := FALSE;
    if UE_Factor > 0.0 then
    begin
        Result := TRUE;
        Exit;
    end;

     {ELSE Check Voltages}
    if LoadSolutionCount <> ActiveCircuit[ActorID].Solution.SolutionCount then
        CalcVTerminalPhase(ActorID);

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
        NormMinCriteria := ActiveCircuit[ActorID].NormalMinVolts;

    if VminEmerg <> 0.0 then
        EmergMinCriteria := VMinEmerg
    else
        EmergMinCriteria := ActiveCircuit[ActorID].EmergMinVolts;

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

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TLoadObj.Get_ExceedsNormal(ActorID: Integer): Boolean;
var
    i: Integer;
    Vpu,
    Vmag: Double;

  {  Line overload takes precedence.
     Assumes that low voltage is due to overloaded line.
     IF voltage is below Normal minumum, it is counted as unserved in proportion
     to the difference between the normal and emergency voltage limits.
  }

    NormMinCriteria,
    EmergMinCriteria: Double;

begin

{ 1-4-00  Added Vpu}

    Result := FALSE;
    if EEN_Factor > 0.0 then
    begin
        Result := TRUE;
        Exit;
    end;   // Check line overload

    if LoadSolutionCount <> ActiveCircuit[ActorID].Solution.SolutionCount then
        CalcVTerminalPhase(ActorID);

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
        NormMinCriteria := ActiveCircuit[ActorID].NormalMinVolts;

    if VminEmerg <> 0.0 then
        EmergMinCriteria := VMinEmerg
    else
        EmergMinCriteria := ActiveCircuit[ActorID].EmergMinVolts;


    if Vpu < NormMinCriteria then
    begin
        EEN_Factor := (NormMinCriteria - Vpu) / (NormMinCriteria - EmergMinCriteria);
       // 9-19-00 RCD  Let EEN factor grow linearly at same slope
       // IF EEN_Factor > 1.0 THEN EEN_Factor := 1.0;
        Result := TRUE;
        Exit;
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TLoadObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i, j: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            case i of
                4:
                    Writeln(F, '~ ', PropertyName^[i], '=', kWBase: 8: 1);
                5:
                    Writeln(F, '~ ', PropertyName^[i], '=', PFNominal: 5: 3);
                12:
                    Writeln(F, '~ ', PropertyName^[i], '=', kvarBase: 8: 1);
                22:
                    Writeln(F, '~ ', PropertyName^[i], '=', FkVAAllocationFactor: 5: 3);
                23:
                    Writeln(F, '~ ', PropertyName^[i], '=', kVABase: 8: 1);
                33:
                begin
                    Write(F, '~ ', PropertyName^[i], '=');
                    for j := 1 to nZIPV do
                        Write(F, ZIPV^[j]: 0: 2, ' ');
                    Writeln(F, '"');
                end;
                34:
                    Writeln(F, '~ ', PropertyName^[i], '=', (puSeriesRL * 100.0): 8: 1);
            else
                Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
            end;
        end;

end;


procedure TLoadObj.Set_kVAAllocationFactor(const Value: Double);
begin
    FkVAAllocationFactor := Value;
    FAllocationFactor := Value;
    LoadSpecType := 3;
    ComputeAllocatedLoad;
    HasBeenAllocated := TRUE;
end;

procedure TLoadObj.Set_AllocationFactor(const Value: Double);
{This procedure is used by the energymeter allocateload function to adjust load allocation factors}
begin
    FAllocationFactor := Value;
    case LoadSpecType of
        3:
            FkVAAllocationFactor := Value;
        4:
            FCFactor := Value;
    end;
    ComputeAllocatedLoad;  // update kWbase
    HasBeenAllocated := TRUE;
end;

procedure TLoadObj.Set_CFactor(const Value: Double);
begin
    FCFactor := Value;
    FAllocationFactor := Value;
    LoadSpecType := 4;
    ComputeAllocatedLoad;
    HasBeenAllocated := TRUE;
end;

procedure TLoadObj.Set_ConnectedkVA(const Value: Double);
begin
    FConnectedkVA := Value;
    LoadSpecType := 3;
    FAllocationFactor := FkVAAllocationFactor;
    ComputeAllocatedLoad;
end;

procedure TLoadObj.Set_kWh(const Value: Double);
begin
    FkWh := Value;
    LoadSpecType := 4;
    FAllocationFactor := FCFactor;
    ComputeAllocatedLoad;
end;

procedure TLoadObj.Set_kWhDays(const Value: Double);
begin
    FkWhDays := Value;
    LoadSpecType := 4;
    ComputeAllocatedLoad;
end;

procedure TLoadObj.set_nZIPV(const Value: Integer);
begin
    SetZIPVSize(Value);
end;

procedure TLoadObj.ComputeAllocatedLoad;
begin
{Fixed loads defined by kW, kvar or kW, pf are ignored}

    case LoadSpecType of

        3:
            if FConnectedkVA > 0.0 then
            begin
                kWBase := FConnectedkVA * FkVAAllocationFactor * Abs(PFNominal);
                kvarBase := kWBase * SQRT(1.0 / SQR(PFNominal) - 1.0);
                if PFNominal < 0.0 then
                    kvarBase := -kvarBase;
            end;

        4:
        begin
            FavgkW := FkWh / (FkWhDays * 24);
            kWBase := FavgkW * FCfactor;
            kvarBase := kWBase * SQRT(1.0 / SQR(PFNominal) - 1.0);
            if PFNominal < 0.0 then
                kvarBase := -kvarBase;
        end;
    end;

end;


procedure TLoadObj.InitHarmonics(ActorID: Integer);
{
   Get the present terminal currents and store for harmonics base reference;
}
var
     {Currents:pComplexArray;}
    i: Integer;
begin
     {Make Sure there's enuff memory}
    ReallocMem(HarmMag, Sizeof(HarmMag^[1]) * FNphases);
    ReallocMem(HarmAng, Sizeof(HarmAng^[1]) * FNphases);

     // Currents := AllocMem(Sizeof(Currents^[1])*Yorder);   // to hold currents

    LoadFundamental := ActiveCircuit[ActorID].Solution.Frequency;

     // GetCurrents(Currents); // Use FPhaseCurr from most recent pflow solution
     {Store the currents at fundamental frequency.
      The spectrum is applied to these.
     }

    for i := 1 to Fnphases do
    begin
        HarmMag^[i] := Cabs(FPhaseCurr^[i]);
        HarmAng^[i] := Cdang(FPhaseCurr^[i]);
    end;

     // ReallocMem(Currents, 0);  // get rid of temp space
end;


procedure TLoadObj.InitPropertyValues(ArrayOffset: Integer);

begin

    PropertyValue[1] := '3';              //'phases';
    PropertyValue[2] := Getbus(1);         //'bus1';
    PropertyValue[3] := '12.47';
    PropertyValue[4] := '10';
    PropertyValue[5] := '.88';
    PropertyValue[6] := '1';
    PropertyValue[7] := '';
    PropertyValue[8] := '';
    PropertyValue[9] := '';
    PropertyValue[10] := '';
    PropertyValue[11] := 'wye';
    PropertyValue[12] := '5';
    PropertyValue[13] := '-1'; // 'rneut'; // IF entered -, assume open or user defined
    PropertyValue[14] := '0';  //'xneut';
    PropertyValue[15] := 'variable'; //'status';  // fixed or variable
    PropertyValue[16] := '1'; //class
    PropertyValue[17] := '0.95';
    PropertyValue[18] := '1.05';
    PropertyValue[19] := '0.0';
    PropertyValue[20] := '0.0';
    PropertyValue[21] := '0.0';
    PropertyValue[22] := '0.5';  // Allocation Factor
    PropertyValue[23] := '11.3636';
    PropertyValue[24] := '50';
    PropertyValue[25] := '10';
    PropertyValue[26] := '1';  // CVR watt factor
    PropertyValue[27] := '2';  // CVR var factor
    PropertyValue[28] := '0';  // kwh bulling
    PropertyValue[29] := '30';  // kwhdays
    PropertyValue[30] := '4';  // Cfactor
    PropertyValue[31] := '';  // CVRCurve
    PropertyValue[32] := '1';  // NumCust
    PropertyValue[33] := '';  // ZIPV coefficient array
    PropertyValue[34] := '50';  // %SeriesRL
    PropertyValue[35] := '1';  // RelWeighting
    PropertyValue[36] := '0.5';  // VZpu
    PropertyValue[37] := '0.0';  // puXharm
    PropertyValue[38] := '6.0';  // XRHarm


    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TLoadObj.MakePosSequence(ActorID: Integer);
var
    S: String;
    V: Double;

begin

    S := 'Phases=1 conn=wye';

  // Make sure voltage is line-neutral
    if (Fnphases > 1) or (connection <> 0) then
        V := kVLoadBase / SQRT3
    else
        V := kVLoadBase;

    S := S + Format(' kV=%-.5g', [V]);

(* OLD Method
  // Divide the load by no. phases
  If Fnphases>1 Then
  Begin
      S := S + Format(' kW=%-.5g  kvar=%-.5g',[kWbase/Fnphases, kvarbase/Fnphases]);
      If FConnectedKVA>0.0 Then
         S := S + Format(' xfkVA=%-.5g  ',[FConnectedkVA/Fnphases]);
  End;
*)

// New Method: Assume load is distributed equally among the 3 phases -- works better
//1-5-2016 RCD

    S := S + Format(' kW=%-.5g  kvar=%-.5g', [kWbase / 3.0, kvarbase / 3.0]);
    if FConnectedKVA > 0.0 then
        S := S + Format(' xfkVA=%-.5g  ', [FConnectedkVA / 3.0]);


    Parser[ActorID].CmdString := S;
    Edit(ActorID);

    inherited;
end;

function TLoadObj.GetPropertyValue(Index: Integer): String;
var
    i: Integer;
begin
    case Index of
        2:
            Result := GetBus(1);
        3:
            Result := Format('%-g', [kVLoadBase]);
        4:
            Result := Format('%-g', [kwBase]);
        5:
            Result := Format('%-.4g', [PFNominal]);
        7:
            Result := Yearlyshape;
        8:
            Result := Dailyshape;
        9:
            Result := Dutyshape;
        12:
            Result := Format('%-g', [kvarbase]);
        22:
            Result := Format('%-g', [FkVAAllocationFactor]);
        23:
            Result := Format('%-g', [kVABase]);
        30:
            Result := Format('%-.4g', [FCFactor]);
        33:
        begin
            Result := '';
            for i := 1 to nZIPV do
                Result := Result + Format(' %-g', [ZIPV^[i]]);
        end;
        34:
            Result := Format('%-g', [puSeriesRL * 100.0]);
        35:
            Result := Format('%-g', [RelWeighting]);
        36:
            Result := Format('%-g', [VLowpu]);
        37:
            Result := Format('%-g', [FpuXHarm]);
        38:
            Result := Format('%-g', [FXRHarmRatio]);
    else
        Result := inherited GetPropertyValue(index);
    end;
end;


initialization

    CDOUBLEONE := CMplx(1.0, 1.0);

end.
