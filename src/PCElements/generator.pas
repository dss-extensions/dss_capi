unit generator;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   Change Log

    11/30/99 Added new properties to support conventional load flow
              Vset, Qmin, Qmax
    12/1/99 Split out ComputeYsc(ibus)
            Added Code to estimate DQDV
    12/2/99 Fixed bug in CalcYPrimMatrix - same bug as was in Load
    12/6/99 revised 95% - 105% limits - same as Load
    1-8-00 made voltage limites variable just like the Load.  Added vminpu
           and vmaxpu properties and modified YEq95, etc.
    2-2-00 Trapezoidal integration option
    2-28-00 Corrected Errors in Take Sample function
    8-23-00 Added FixedQ models; Added Price register and related dispatchmode
    8-24-00 Fixed Pnominalperphase so that it is never 0.0 to avoid divide by zero error
    9-20-00 Added InitStateVars  Function for Dynamics mode
    10-6-00 Fixed error in TakeSample for positive sequence model
    10-25-00 Added Spectrum   and code for Harmonic mode analysis
    10-27-00 Deleted GetCurrents Override;
    3-7-01 Fixed bug related to setting kvar=  (Index wrong)
    3-27-01 Added check to prevent divide by zero on calculation of PFNominal
    5-17-01 moved spectrum editing back to base class
    7-2-01 Corrected TakeSample to integrate only when GenON instead of S>0
           Also corrected kVA Max for Positive Seq only
    8-14-01 Added price signal integration, which had been omitted
            Fixed TakeSample so it would integrate on Trapezoidal when not GenON
    1-17/02 Fixed sign error for Type 5 model.
    7/11/02 Added code to change Yprim when generator changes ON/OFF state
    7/30/02 Fixed problem with propertyvalues and maxkvar
    11/08/02  Added Dynamics model
    11/11/02 Add user-written exciter and Shaft Models
    3/6/03   Revised user-written dll interface.
             added control terminal code for PCELement override.
    3-17-03  Revised user-written models and harmonic models
    5-11-09  Added properties to support kW, kvar, PV, and kV  through COM
    8-28-13 Forced re-initializing solution if Model 3 generator added.
    7-??-18 Corrected Generator Model 7 1-phase Model
}
{
  The generator is essentially a negative load that can be dispatched.

  If the dispatch value (DispValue) is 0, the generator always follows the
  appropriate dispatch curve, which are simply load curves. If DispValue>0 then
  the generator only comes on when the global circuit load multiplier exceeds
  DispValue.  When the generator is on, it always follows the dispatch curve
  appropriate for the type of solution being performed.

  If you want to model a generator that is fully on whenever it is dispatched on,
  simply designate "Status=Fixed".  The default is "Status=Variable" (i.e., it follows
  a dispatch curve.  You could also define a dispatch curve that is always 1.0.

  Generators have their own energy meters that record:
  1. Total kwh
  2. Total kvarh
  3. Max kW
  4. Max kVA
  5. Hours in operation
  6. Price * kwH

  Generator meters reset with the circuit energy meters and take a sample with
  the circuit energy meters as well. The Energy meters also used trapezoidal integration
  so that they are compatible with Load-Duration simulations.

  Generator models are:
  1. Constant P, Q  (* dispatch curve, if appropriate).
  2. Constant Z  (For simple solution)
  3. Constant P, |V|  like a standard power flow
  4. Constant P, Fixed Q  (vars)
  5. Constant P, Fixed Q  (reactance)
  6. User model
  7. Approximate Inverter model

  Most of the time you will use #1 for planning studies.

}

//  The Generator is assumed balanced over the no. of phases defined

// If you do not specify load shapes defaults are:
//    Yearly:  Defaults to No variation (i.e. multiplier = 1.0 always)
//    Daily:   Defaults to No variation
//    Dutycycle: Defaults to Daily shape

interface

uses
    GeneratorVars,
    GenUserModel,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    ucomplex,
    LoadShape,
    GrowthShape,
    Spectrum,
    ArrayDef,
    Dynamics;

const
    NumGenRegisters = 6;    // Number of energy meter registers
    NumGenVariables = 6;

type

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TGenerator = class(TPCClass)
    PRIVATE

        procedure InterpretConnection(const S: String);
        procedure SetNcondsForConnection;
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const OtherGeneratorName: String): Integer; OVERRIDE;
    PUBLIC
        RegisterNames: array[1..NumGenregisters] of String;

        constructor Create(dss: TDSSContext);
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;
        function Init(Handle: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

        procedure ResetRegistersAll;
        procedure SampleAll;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TGeneratorObj = class(TPCElement)
    PRIVATE
// Moved to GeneratorVars        Zthev           :Complex;
        Yeq: Complex;   // at nominal
        Yeq95: Complex;   // at 95%
        Yeq105: Complex;   // at 105%

        Edp: Complex;
        PhaseCurrentLimit: Complex;
        Model7MaxPhaseCurr: Double;
        Model7LastAngle: Double;
        DebugTrace: Boolean;
        DeltaQMax: Double;  // Max allowable var change on Model=3 per iteration
        DispatchMode: Integer;
        DispatchValue: Double;
        DQDV: Double;
        DQDVSaved: Double;
        FForcedON: Boolean;
        FirstSampleAfterReset: Boolean;
        IsFixed: Boolean;   // if Fixed, always at base value
        GeneratorSolutionCount: Integer;
        GenFundamental: Double;  {Thevinen equivalent voltage mag and angle reference for Harmonic model}
        GenON: Boolean;           {Indicates whether generator is currently on}
        GenSwitchOpen: Boolean;
        kVANotSet: Boolean;
        LastGrowthFactor: Double;
        LastYear: Integer;   // added for speedup so we don't have to search for growth factor a lot
        OpenGeneratorSolutionCount: Integer;
        PVFactor: Double;  // deceleration Factor for computing vars for PV generators
        RandomMult: Double;
        Reg_Hours: Integer;
        Reg_kvarh: Integer;
        Reg_kWh: Integer;
        Reg_MaxkVA: Integer;
        Reg_MaxkW: Integer;
        Reg_Price: Integer;
        ShapeFactor: Complex;
// moved to GeneratorVars        Thetaharm       :Double;  {Thevinen equivalent voltage angle reference for Harmonic model}
        Tracefile: TextFile;
        UserModel, ShaftModel: TGenUserModel;   {User-Written Models}
        V_Avg: Double;
        V_Remembered: Double;
        var_Remembered: Double;
        varBase: Double; // Base vars per phase
        varMax: Double;
        varMin: Double;
        VBase: Double;  // Base volts suitable for computing currents
        VBase105: Double;
        VBase95: Double;
        Vthev: Complex;  {Thevinen equivalent voltage (complex) for dynamic model}
// moved to GeneratorVars        Vthevharm       :Double;  {Thevinen equivalent voltage mag reference for Harmonic model}
// moved to GeneratorVars        VthevMag        :Double;    {Thevinen equivalent voltage for dynamic model}
        YPrimOpenCond: TCmatrix;  // To handle cases where one conductor of load is open ; We revert to admittance for inj currents
        YQFixed: Double;  // Fixed value of y for type 7 load
        ShapeIsActual: Boolean;
        ForceBalanced: Boolean;

        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);  // now incorporates DutyStart offset
        procedure CalcGenModelContribution;
        procedure CalcInjCurrentArray;
        procedure CalcVterminal;
        procedure CalcVTerminalPhase;
        procedure CalcVthev_Dyn;      // 3-phase Voltage behind transient reactance
        procedure CalcVthev_Dyn_Mod7(const V: Complex);
        procedure CalcYearlyMult(Hr: Double);
        procedure CalcYPrimMatrix(Ymatrix: TcMatrix);

        procedure DoConstantPQGen;
        procedure DoConstantZGen;
        procedure DoCurrentLimitedPQ;
        procedure DoDynamicMode;
        procedure DoFixedQGen;
        procedure DoFixedQZGen;
        procedure DoHarmonicMode;
        procedure DoPVTypeGen;
        procedure DoUserModel;

        procedure Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
        procedure SetDragHandRegister(Reg: Integer; const Value: Double);
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);

        procedure WriteTraceRecord(const s: String);

        procedure SyncUpPowerQuantities;


        function Get_PresentkW: Double;
        function Get_Presentkvar: Double;
        function Get_PresentkV: Double;
        procedure Set_PresentkV(const Value: Double);
        procedure Set_Presentkvar(const Value: Double);
        procedure Set_PresentkW(const Value: Double);
        procedure Set_PowerFactor(const Value: Double);

        procedure SetkWkvar(const PkW, Qkvar: Double);

    PROTECTED
        procedure Set_ConductorClosed(Index: Integer; Value: Boolean); OVERRIDE;
        procedure GetTerminalCurrents(Curr: pComplexArray); OVERRIDE;

    PUBLIC

        Connection: Integer;  {0 = line-neutral; 1=Delta}
        DailyDispShape: String;  // Daily (24 HR) Generator shape
        DailyDispShapeObj: TLoadShapeObj;  // Daily Generator Shape for this load
        DutyShape: String;  // Duty cycle load shape for changes typically less than one hour
        DutyShapeObj: TLoadShapeObj;  // Shape for this generator
        DutyStart: Double; // starting time offset into the DutyShape [hrs] for this generator
        GenClass: Integer;
        GenModel: Integer;   // Variation with voltage
        GenVars: TGeneratorVars; {State Variables}
        kvarBase: Double;
        kvarMax: Double;
        kvarMin: Double;
        kWBase: Double;
        PFNominal: Double;
        Vpu: Double;   // per unit Target voltage for generator with voltage control
        Vmaxpu: Double;
        Vminpu: Double;

// moved to GeneratorVars        VTarget         :Double;  // Target voltage for generator with voltage control
        YearlyShape: String;  // ='fixed' means no variation  on all the time
        YearlyShapeObj: TLoadShapeObj;  // Shape for this Generator

        Registers, Derivatives: array[1..NumGenregisters] of Double;

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        function InjCurrents: Integer; OVERRIDE;
        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        procedure SetNominalGeneration;
        procedure Randomize(Opt: Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform

        procedure ResetRegisters;
        procedure TakeSample;

        // Procedures for setting the DQDV used by the Solution Object
        procedure InitDQDVCalc;
        procedure BumpUpQ;
        procedure RememberQV;
        procedure CalcDQDV;
        procedure ResetStartPoint;

        // Support for Dynamics Mode
        procedure InitStateVars; OVERRIDE;
        procedure IntegrateStates; OVERRIDE;

        // Support for Harmonics Mode
        procedure InitHarmonics; OVERRIDE;

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;

        property PresentkW: Double READ Get_PresentkW WRITE Set_PresentkW;
        property Presentkvar: Double READ Get_Presentkvar WRITE Set_Presentkvar;
        property ForcedON: Boolean READ FForcedON WRITE FForcedON;
        property PresentkV: Double READ Get_PresentkV WRITE Set_PresentkV;
        property PowerFactor: Double READ PFNominal WRITE Set_PowerFactor;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
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
    DSSHelper;

const
    NumPropsThisClass = 39;
  // Dispatch modes
    DEFAULT = 0;
    LOADMODE = 1;
    PRICEMODE = 2;

var
    cBuffer: array[1..24] of Complex;  // Temp buffer for calcs  24-phase generator?
    CDOUBLEONE: Complex;
//    TwoPI3:Double;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TGenerator.Create(dss: TDSSContext);  // Creates superstructure for all Line objects
begin
    inherited Create(dss);
    Class_Name := 'Generator';
    DSSClassType := DSSClassType + GEN_ELEMENT;  // In both PCelement and Genelement list

    ActiveElement := 0;

     // Set Register names
    RegisterNames[1] := 'kWh';
    RegisterNames[2] := 'kvarh';
    RegisterNames[3] := 'Max kW';
    RegisterNames[4] := 'Max kVA';
    RegisterNames[5] := 'Hours';
    RegisterNames[6] := '$';

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TGenerator.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGenerator.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;   {see DSSClass}

     // Define Property names
    AddProperty('phases', 1, 'Number of Phases, this Generator.  Power is evenly divided among phases.');
    AddProperty('bus1', 2, 'Bus to which the Generator is connected.  May include specific node specification.');
    AddProperty('kv', 3, 'Nominal rated (1.0 per unit) voltage, kV, for Generator. For 2- and 3-phase Generators, specify phase-phase kV. ' +
        'Otherwise, for phases=1 or phases>3, specify actual kV across each branch of the Generator. ' +
        'If wye (star), specify phase-neutral kV. ' +
        'If delta or phase-phase connected, specify phase-phase kV.');  // line-neutral voltage//  base voltage
    AddProperty('kW', 4, 'Total base kW for the Generator.  A positive value denotes power coming OUT of the element, ' + CRLF +
        'which is the opposite of a load. This value is modified depending on the dispatch mode. ' +
        'Unaffected by the global load multiplier and growth curves. ' +
        'If you want there to be more generation, you must add more generators or change this value.');
    AddProperty('pf', 5, 'Generator power factor. Default is 0.80. Enter negative for leading powerfactor ' +
        '(when kW and kvar have opposite signs.)' + CRLF +
        'A positive power factor for a generator signifies that the generator produces vars ' + CRLF +
        'as is typical for a synchronous generator.  Induction machines would be ' + CRLF +
        'specified with a negative power factor.');
    AddProperty('kvar', 13, 'Specify the base kvar.  Alternative to specifying the power factor.  Side effect: ' +
        ' the power factor value is altered to agree based on present value of kW.');
    AddProperty('model', 6, 'Integer code for the model to use for generation variation with voltage. ' +
        'Valid values are:' + CRLF + CRLF +
        '1:Generator injects a constant kW at specified power factor.' + CRLF +
        '2:Generator is modeled as a constant admittance.' + CRLF +
        '3:Const kW, constant kV.  Somewhat like a conventional transmission power flow P-V generator.' + CRLF +
        '4:Const kW, Fixed Q (Q never varies)' + CRLF +
        '5:Const kW, Fixed Q(as a constant reactance)' + CRLF +
        '6:Compute load injection from User-written Model.(see usage of Xd, Xdp)' + CRLF +
        '7:Constant kW, kvar, but current-limited below Vminpu. Approximates a simple inverter. See also Balanced.');
    AddProperty('Vminpu', 23, 'Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. ' +
        'Below this value, the load model reverts to a constant impedance model. For model 7, the current is ' +
        'limited to the value computed for constant power at Vminpu.');
    AddProperty('Vmaxpu', 24, 'Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. ' +
        'Above this value, the load model reverts to a constant impedance model.');
    AddProperty('yearly', 7, 'Dispatch shape to use for yearly simulations.  Must be previously defined ' +
        'as a Loadshape object. If this is not specified, a constant value is assumed (no variation). ' +
        'If the generator is assumed to be ON continuously, specify Status=FIXED, or ' +
        'designate a curve that is 1.0 per unit at all times. ' +
        'Set to NONE to reset to no loadahape. ' +
        'Nominally for 8760 simulations.  If there are fewer points in the designated shape than ' +
        'the number of points in the solution, the curve is repeated.');
    AddProperty('daily', 8, 'Dispatch shape to use for daily simulations.  Must be previously defined ' +
        'as a Loadshape object of 24 hrs, typically.  If generator is assumed to be ' +
        'ON continuously, specify Status=FIXED, or designate a Loadshape object' +
        'that is 1.0 perunit for all hours. ' +
        'Set to NONE to reset to no loadahape. '); // daily dispatch (hourly)
    AddProperty('duty', 9, 'Load shape to use for duty cycle dispatch simulations such as for wind generation. ' +
        'Must be previously defined as a Loadshape object. ' +
        'Typically would have time intervals less than 1 hr -- perhaps, in seconds. ' +
        'Set Status=Fixed to ignore Loadshape designation. ' +
        'Set to NONE to reset to no loadahape. ' +
        'Designate the number of points to solve using the Set Number=xxxx command. ' +
        'If there are fewer points in the actual shape, the shape is assumed to repeat.');  // as for wind generation
    AddProperty('dispmode', 10, '{Default* | Loadlevel | Price } Default = Default. Dispatch mode. ' +
        'In default mode, gen is either always on or follows dispatch curve as specified. ' +
        'Otherwise, the gen comes on when either the global default load level (Loadshape "default") or the price level ' +
        'exceeds the dispatch value.'); // = 0 | >0
    AddProperty('dispvalue', 11, 'Dispatch value. ' + CRLF +
        'If = 0.0 (default) then Generator follow dispatch curves, if any. ' + CRLF +
        'If > 0  then Generator is ON only when either the price signal (in Price dispatch mode) ' +
        'exceeds this value or the active circuit load multiplier * "default" loadshape value * the default yearly growth factor ' +
        'exceeds this value.  Then the generator follows dispatch curves (duty, daily, or yearly), if any (see also Status).');  // = 0 | >0
    AddProperty('conn', 12, '={wye|LN|delta|LL}.  Default is wye.');
    AddProperty('Rneut', 14, 'Removed due to causing confusion - Add neutral impedance externally.');
    AddProperty('Xneut', 15, 'Removed due to causing confusion - Add neutral impedance externally.');
    AddProperty('status', 16, '={Fixed | Variable*}.  If Fixed, then dispatch multipliers do not apply. ' +
        'The generator is alway at full power when it is ON. ' +
        ' Default is Variable  (follows curves).');  // fixed or variable
    AddProperty('class', 17, 'An arbitrary integer number representing the class of Generator so that Generator values may ' +
        'be segregated by class.'); // integer
    AddProperty('Vpu', 18, 'Per Unit voltage set point for Model = 3  (typical power flow model).  Default is 1.0. '); // per unit set point voltage for power flow model
    AddProperty('maxkvar', 19, 'Maximum kvar limit for Model = 3.  Defaults to twice the specified load kvar.  ' +
        'Always reset this if you change PF or kvar properties.');
    AddProperty('minkvar', 20, 'Minimum kvar limit for Model = 3. Enter a negative number if generator can absorb vars.' +
        ' Defaults to negative of Maxkvar.  Always reset this if you change PF or kvar properties.');
    AddProperty('pvfactor', 21, 'Deceleration factor for P-V generator model (Model=3).  Default is 0.1. ' +
        'If the circuit converges easily, you may want to use a higher number such as 1.0. ' +
        'Use a lower number if solution diverges. Use Debugtrace=yes to create a file that will ' +
        'trace the convergence of a generator model.');
    AddProperty('forceon', 25, '{Yes | No}  Forces generator ON despite requirements of other dispatch modes. ' +
        'Stays ON until this property is set to NO, or an internal algorithm cancels the forced ON state.');
    AddProperty('kVA', 26, 'kVA rating of electrical machine. Defaults to 1.2* kW if not specified. Applied to machine or inverter definition for Dynamics mode solutions. ');
    AddProperty('MVA', 27, 'MVA rating of electrical machine.  Alternative to using kVA=.');
    AddProperty('Xd', 28, 'Per unit synchronous reactance of machine. Presently used only for Thevinen impedance for power flow calcs of user models (model=6). ' +
        'Typically use a value 0.4 to 1.0. Default is 1.0');
    AddProperty('Xdp', 29, 'Per unit transient reactance of the machine.  Used for Dynamics mode and Fault studies.  Default is 0.27.' +
        'For user models, this value is used for the Thevinen/Norton impedance for Dynamics Mode.');
    AddProperty('Xdpp', 30, 'Per unit subtransient reactance of the machine.  Used for Harmonics. Default is 0.20.');
    AddProperty('H', 31, 'Per unit mass constant of the machine.  MW-sec/MVA.  Default is 1.0.');
    AddProperty('D', 32, 'Damping constant.  Usual range is 0 to 4. Default is 1.0.  Adjust to get damping');
    AddProperty('UserModel', 33, 'Name of DLL containing user-written model, which computes the terminal currents for Dynamics studies, ' +
        'overriding the default model.  Set to "none" to negate previous setting.');
    AddProperty('UserData', 34, 'String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model.');
    AddProperty('ShaftModel', 35, 'Name of user-written DLL containing a Shaft model, which models the prime mover and determines the power on the shaft for Dynamics studies. ' +
        'Models additional mass elements other than the single-mass model in the DSS default model. Set to "none" to negate previous setting.');
    AddProperty('ShaftData', 36, 'String (in quotes or parentheses) that gets passed to user-written shaft dynamic model for defining the data for that model.');
    AddProperty('DutyStart', 37, 'Starting time offset [hours] into the duty cycle shape for this generator, defaults to 0');
    AddProperty('debugtrace', 22, '{Yes | No }  Default is no.  Turn this on to capture the progress of the generator model ' +
        'for each iteration.  Creates a separate file for each generator named "GEN_name.CSV".');
    AddProperty('Balanced', 38, '{Yes | No*} Default is No.  For Model=7, force balanced current only for 3-phase generators. Force zero- and negative-sequence to zero.');
    AddProperty('XRdp', 39, 'Default is 20. X/R ratio for Xdp property for FaultStudy and Dynamic modes.');


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override default help string
    PropertyHelp[NumPropsThisClass + 1] := 'Name of harmonic voltage or current spectrum for this generator. ' +
        'Voltage behind Xd" for machine - default. Current injection for inverter. ' +
        'Default value is "default", which is defined when the DSS starts.';

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TGenerator.NewObject(const ObjName: String): Integer;
begin
    // Make a new Generator and add it to Generator class list
    with DSS.ActiveCircuit do
    begin
        ActiveCktElement := TGeneratorObj.Create(Self, ObjName);
        Result := AddObjectToList(DSS.ActiveDSSObject);
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGenerator.SetNcondsForConnection;

begin
    with DSS.ActiveGeneratorObj do
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
        end;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGenerator.InterpretConnection(const S: String);

// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN
var
    TestS: String;

begin
    with DSS.ActiveGeneratorObj do
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

            {VBase is always L-N voltage unless 1-phase device or more than 3 phases}

        with GenVars do {CASE Connection OF
              1: VBase := kVGeneratorBase * 1000.0 ;
              Else}
            case Fnphases of
                2, 3:
                    VBase := kVGeneratorBase * InvSQRT3x1000;    // L-N Volts
            else
                VBase := kVGeneratorBase * 1000.0;   // Just use what is supplied
            end;
            {End;}
        VBase95 := Vminpu * VBase;
        VBase105 := Vmaxpu * VBase;

        Yorder := Fnconds * Fnterms;
        YPrimInvalid := TRUE;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function InterpretDispMode(const S: String): Integer;
begin

    case lowercase(S)[1] of
        'l':
            Result := LOADMODE;
        'p':
            Result := PRICEMODE;
    else
        Result := DEFAULT;
    end;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TGenerator.Edit: Integer;
var
    i,
    ParamPointer: Integer;
    ParamName: String;
    Param: String;


begin
  // continue parsing with contents of Parser
    DSS.ActiveGeneratorObj := ElementList.Active;
    DSS.ActiveCircuit.ActiveCktElement := DSS.ActiveGeneratorObj;

    Result := 0;

    with DSS.ActiveGeneratorObj do
    begin

        ParamPointer := 0;
        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
        while Length(Param) > 0 do
        begin
            if (Length(ParamName) = 0) then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[PropertyIdxMap[ParamPointer]] := Param
            else
                DoSimpleMsg(DSS, 'Unknown parameter "' + ParamName + '" for Generator "' + Name + '"', 560);

            if ParamPointer > 0 then
                case PropertyIdxMap[ParamPointer] of
                    0:
                        DoSimpleMsg(DSS, 'Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 561);
                    1:
                        NPhases := DSS.Parser.Intvalue; // num phases
                    2:
                        SetBus(1, param);
                    3:
                        PresentkV := DSS.Parser.DblValue;
                    4:
                        kWBase := DSS.Parser.DblValue;
                    5:
                        PFNominal := DSS.Parser.DblValue;
                    6:
                        GenModel := DSS.Parser.IntValue;
                    7:
                        YearlyShape := Param;
                    8:
                        DailyDispShape := Param;
                    9:
                        DutyShape := Param;
                    10:
                        DispatchMode := InterpretDispMode(Param);
                    11:
                        DispatchValue := DSS.Parser.DblValue;
                    12:
                        InterpretConnection(Param);
                    13:
                        Presentkvar := DSS.Parser.DblValue;
                    14:
                        DoSimpleMsg(DSS, 'Rneut property has been deleted. Use external impedance.', 5611);
                    15:
                        DoSimpleMsg(DSS, 'Xneut property has been deleted. Use external impedance.', 5612);
                    16:
                        if lowercase(Param[1]) = 'f' then
                            IsFixed := TRUE
                        else
                            IsFixed := FALSE;
                    17:
                        GenClass := DSS.Parser.IntValue;
                    18:
                        Vpu := DSS.Parser.DblValue;
                    19:
                        kvarMax := DSS.Parser.DblValue;
                    20:
                        kvarMin := DSS.Parser.DblValue;
                    21:
                        PVFactor := DSS.Parser.DblValue;  //decelaration factor
                    22:
                        DebugTrace := InterpretYesNo(Param);
                    23:
                        VMinPu := DSS.Parser.DblValue;
                    24:
                        VMaxPu := DSS.Parser.DblValue;
                    25:
                        FForcedON := InterpretYesNo(Param);
                    26:
                        GenVars.kVArating := DSS.Parser.DblValue;
                    27:
                        GenVars.kVArating := DSS.Parser.DblValue * 1000.0;  // 'MVA';
                    28:
                        GenVars.puXd := DSS.Parser.DblValue;
                    29:
                        GenVars.puXdp := DSS.Parser.DblValue;
                    30:
                        GenVars.puXdpp := DSS.Parser.DblValue;
                    31:
                        GenVars.Hmass := DSS.Parser.DblValue;
                    32:
                        GenVars.Dpu := DSS.Parser.DblValue;
                    33:
                        UserModel.Name := DSS.Parser.StrValue;  // Connect to user written models
                    34:
                        if UserModel.Exists then
                            UserModel.Edit := DSS.Parser.StrValue;  // Send edit string to user model
                    35:
                        ShaftModel.Name := DSS.Parser.StrValue;
                    36:
                        ShaftModel.Edit := DSS.Parser.StrValue;
                    37:
                        DutyStart := DSS.Parser.DblValue;
                    38:
                        ForceBalanced := InterpretYesNo(Param);
                    39:
                        Genvars.XRdp := DSS.Parser.DblValue;  // X/R for dynamics model

                else
           // Inherited parameters
                    ClassEdit(DSS.ActiveGeneratorObj, ParamPointer - NumPropsThisClass)
                end;

            if ParamPointer > 0 then
                case PropertyIdxMap[ParamPointer] of
                    1:
                        SetNcondsForConnection;  // Force Reallocation of terminal info

            // keep kvar nominal up to date with kW and PF
                    4, 5:
                        SyncUpPowerQuantities;

            // if a model 3 generator added, force calc of dQdV
                    6:
                        if GenModel = 3 then
                            DSS.ActiveCircuit.Solution.SolutionInitialized := FALSE;

    {Set shape objects;  returns nil if not valid}
     {Sets the kW and kvar properties to match the peak kW demand from the Loadshape}
                    7:
                    begin
                        YearlyShapeObj := DSS.LoadShapeClass.Find(YearlyShape);
                        if Assigned(YearlyShapeObj) then
                            with YearlyShapeObj do
                                if UseActual then
                                    SetkWkvar(MaxP, MaxQ);
                    end;
                    8:
                    begin
                        DailyDispShapeObj := DSS.LoadShapeClass.Find(DailyDispShape);
                        if Assigned(DailyDispShapeObj) then
                            with DailyDispShapeObj do
                                if UseActual then
                                    SetkWkvar(MaxP, MaxQ);
                    end;
                    9:
                    begin
                        DutyShapeObj := DSS.LoadShapeClass.Find(DutyShape);
                        if Assigned(DutyShapeObj) then
                            with DutyShapeObj do
                                if UseActual then
                                    SetkWkvar(MaxP, MaxQ);
                    end;

                    22:
                        if DebugTrace then
                        begin
                            AssignFile(TraceFile, DSS.OutputDirectory + 'GEN_' + Name + '.CSV');
                            ReWrite(TraceFile);
                            Write(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, GenModel, dQdV, Avg_Vpu, Vdiff, MQnominalperphase, MPnominalperphase, CurrentType');
                            for i := 1 to nphases do
                                Write(Tracefile, ', |Iinj' + IntToStr(i) + '|');
                            for i := 1 to nphases do
                                Write(Tracefile, ', |Iterm' + IntToStr(i) + '|');
                            for i := 1 to nphases do
                                Write(Tracefile, ', |Vterm' + IntToStr(i) + '|');
                            Write(TraceFile, ',Vthev, Theta');
                            Writeln(TraceFile);
                            CloseFile(Tracefile);
                        end;
                    26, 27:
                        kVANotSet := FALSE;
                end;

            ParamName := DSS.Parser.NextParam;
            Param := DSS.Parser.StrValue;
        end;

        RecalcElementData;
        YPrimInvalid := TRUE;
    end;

end;

//----------------------------------------------------------------------------
function TGenerator.MakeLike(const OtherGeneratorName: String): Integer;
var
    OtherGenerator: TGeneratorObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this line name in the present collection}
    OtherGenerator := Find(OtherGeneratorName);
    if (OtherGenerator <> NIL) then
        with DSS.ActiveGeneratorObj do
        begin

            if (Fnphases <> OtherGenerator.Fnphases) then
            begin
                Nphases := OtherGenerator.Fnphases;
                NConds := Fnphases;  // Forces reallocation of terminal stuff

                Yorder := Fnconds * Fnterms;
                YPrimInvalid := TRUE;
            end;

            GenVars.kVGeneratorBase := OtherGenerator.GenVars.kVGeneratorBase;
            Vbase := OtherGenerator.Vbase;
            Vminpu := OtherGenerator.Vminpu;
            Vmaxpu := OtherGenerator.Vmaxpu;
            Vbase95 := OtherGenerator.Vbase95;
            Vbase105 := OtherGenerator.Vbase105;
            kWBase := OtherGenerator.kWBase;
            kvarBase := OtherGenerator.kvarBase;
            Genvars.Pnominalperphase := OtherGenerator.Genvars.Pnominalperphase;
            PFNominal := OtherGenerator.PFNominal;
            Genvars.Qnominalperphase := OtherGenerator.Genvars.Qnominalperphase;
            varMin := OtherGenerator.varMin;
            varMax := OtherGenerator.varMax;
            Connection := OtherGenerator.Connection;
     //  Rneut          := OtherGenerator.Rneut;
      // Xneut          := OtherGenerator.Xneut;
            YearlyShape := OtherGenerator.YearlyShape;
            YearlyShapeObj := OtherGenerator.YearlyShapeObj;
            DailyDispShape := OtherGenerator.DailyDispShape;
            DailyDispShapeObj := OtherGenerator.DailyDispShapeObj;
            DutyShape := OtherGenerator.DutyShape;
            DutyShapeObj := OtherGenerator.DutyShapeObj;
            DutyStart := OtherGenerator.DutyStart;
            DispatchMode := OtherGenerator.DispatchMode;
            DispatchValue := OtherGenerator.DispatchValue;
            GenClass := OtherGenerator.GenClass;
            GenModel := OtherGenerator.GenModel;
            IsFixed := OtherGenerator.IsFixed;
            GenVars.VTarget := OtherGenerator.Genvars.VTarget;
            Vpu := OtherGenerator.Vpu;
            kvarMax := OtherGenerator.kvarMax;
            kvarMin := OtherGenerator.kvarMin;
            FForcedON := OtherGenerator.FForcedON;
            kVANotSet := OtherGenerator.kVANotSet;

            GenVars.kVArating := OtherGenerator.GenVars.kVArating;
            GenVars.puXd := OtherGenerator.GenVars.puXd;
            GenVars.puXdp := OtherGenerator.GenVars.puXdp;
            GenVars.puXdpp := OtherGenerator.GenVars.puXdpp;
            GenVars.Hmass := OtherGenerator.GenVars.Hmass;
            GenVars.Theta := OtherGenerator.GenVars.Theta;
            GenVars.Speed := OtherGenerator.GenVars.Speed;
            GenVars.w0 := OtherGenerator.GenVars.w0;
            GenVars.dSpeed := OtherGenerator.GenVars.dSpeed;
            GenVars.D := OtherGenerator.GenVars.D;
            GenVars.Dpu := OtherGenerator.GenVars.Dpu;
            GenVars.XRdp := OtherGenerator.GenVars.Xrdp;

            UserModel.Name := OtherGenerator.UserModel.Name;  // Connect to user written models
            ShaftModel.Name := OtherGenerator.ShaftModel.Name;

            ClassMakeLike(OtherGenerator);

            for i := 1 to ParentClass.NumProperties do
                FPropertyValue^[i] := OtherGenerator.FPropertyValue^[i];

            Result := 1;
        end
    else
        DoSimpleMsg(DSS, 'Error in Load MakeLike: "' + OtherGeneratorName + '" Not Found.', 562);

end;

//----------------------------------------------------------------------------
function TGenerator.Init(Handle: Integer): Integer;
var
    p: TGeneratorObj;

begin

    if (Handle = 0) then
    begin  // init all
        p := elementList.First;
        while (p <> NIL) do
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

    DoSimpleMsg(DSS, 'Need to implement TGenerator.Init', -1);
    Result := 0;

end;

{--------------------------------------------------------------------------}
procedure TGenerator.ResetRegistersAll;  // Force all EnergyMeters in the circuit to reset

var
    pGen: TGeneratorObj;

begin
    pGen := DSS.ActiveCircuit.Generators.First;
    while (pGen <> NIL) do
    begin
        pGen.ResetRegisters;
        pGen := DSS.ActiveCircuit.Generators.Next;
    end;

end;

{--------------------------------------------------------------------------}
procedure TGenerator.SampleAll;  // Force all EnergyMeters in the circuit to take a sample

var
    pGen: TGeneratorObj;

begin
    pGen := DSS.ActiveCircuit.Generators.First;
    while pGen <> NIL do
    begin
        if pGen.enabled then
            pGen.TakeSample;
        pGen := DSS.ActiveCircuit.Generators.Next;
    end;
end;

//----------------------------------------------------------------------------
constructor TGeneratorObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; // + GEN_ELEMENT;  // In both PCelement and Genelement list

    Nphases := 3;
    Fnconds := 4;  // defaults to wye
    Yorder := 0;  // To trigger an initial allocation
    Nterms := 1;  // forces allocations
    kWBase := 1000.0;
    kvarBase := 60.0;


    kvarMax := kvarBase * 2.0;
    kvarMin := -kvarmax;
    PFNominal := 0.88;
  //   Rneut        := 0.0;
  //   Xneut        := 0.0;
    YearlyShape := '';
    YearlyShapeObj := NIL;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
    DailyDispShape := '';
    DailyDispShapeObj := NIL;  // if DaillyShapeobj = nil then the load alway stays nominal * global multipliers
    DutyShape := '';
    DutyShapeObj := NIL;  // if DutyShapeobj = nil then the load alway stays nominal * global multipliers
    DutyStart := 0.0;
    Connection := 0;    // Wye (star)
    GenModel := 1;  {Typical fixed kW negative load}
    GenClass := 1;
    LastYear := 0;
    LastGrowthFactor := 1.0;

    DQDVSaved := 0.0;  // Initialize this here.  Allows generators to be turned off and on


    GeneratorSolutionCount := -1;  // For keep track of the present solution in Injcurrent calcs
    OpenGeneratorSolutionCount := -1;
    YPrimOpenCond := NIL;

    GenVars.kVGeneratorBase := 12.47;
    Vpu := 1.0;
    GenVars.VTarget := 1000.0 * Vpu * GenVars.kVGeneratorBase / SQRT3;  {Line-to-Neutral target}
    VBase := 7200.0;
    Vminpu := 0.90;
    Vmaxpu := 1.10;
    VBase95 := Vminpu * Vbase;
    VBase105 := Vmaxpu * Vbase;
    Yorder := Fnterms * Fnconds;
    RandomMult := 1.0;
    IsFixed := FALSE;

     {Machine rating stuff}
    GenVars.kVArating := kWBase * 1.2;
    kVANotSet := TRUE;  // Flag for default value for kVA

     //GenVars.Vd         := 7200.0;


    with GenVars do
    begin
        puXd := 1.0;
        puXdp := 0.28;
        puXdpp := 0.20;
        Xd := puXd * SQR(kVGeneratorBase) * 1000.0 / kVARating;
        Xdp := puXdp * SQR(kVGeneratorBase) * 1000.0 / kVARating;
        Xdpp := puXdpp * SQR(kVGeneratorBase) * 1000.0 / kVARating;
        Hmass := 1.0;       //  W-sec/VA rating
        Theta := 0.0;
        w0 := TwoPi * Basefrequency;
        Speed := 0.0;
        dSpeed := 0.0;
        D := 1.0;
        XRdp := 20.0;
    end;

     {Advertise Genvars struct as public}

    PublicDataStruct := pointer(@Genvars);
    PublicDataSize := SizeOf(TGeneratorVars);

    UserModel := TGenUserModel.Create(DSS, @Genvars);
    ShaftModel := TGenUserModel.Create(DSS, @Genvars);

    DispatchValue := 0.0;   // Follow curves

    Reg_kWh := 1;
    Reg_kvarh := 2;
    Reg_MaxkW := 3;
    Reg_MaxkVA := 4;
    Reg_Hours := 5;
    Reg_Price := 6;

    PVFactor := 0.1;
    DebugTrace := FALSE;
    FForcedON := FALSE;
    GenSwitchOpen := FALSE;
    ShapeIsActual := FALSE;
    ForceBalanced := FALSE;

    Spectrum := 'defaultgen';  // override base class


    InitPropertyValues(0);

    RecalcElementData;

end;


//----------------------------------------------------------------------------
destructor TGeneratorObj.Destroy;
begin
    YPrimOpenCond.Free;
    UserModel.Free;
    ShaftModel.Free;
    inherited Destroy;
end;

//----------------------------------------------------------------------------
procedure TGeneratorObj.Randomize(Opt: Integer);
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

//----------------------------------------------------------------------------
procedure TGeneratorObj.CalcDailyMult(Hr: Double);

begin
    if (DailyDispShapeObj <> NIL) then
    begin
        ShapeFactor := DailyDispShapeObj.GetMult(Hr);
        ShapeIsActual := DailyDispShapeObj.UseActual;
    end
    else
        ShapeFactor := CDOUBLEONE;  // Default to no daily variation
end;


//----------------------------------------------------------------------------
procedure TGeneratorObj.CalcDutyMult(Hr: Double);

begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMult(Hr + DutyStart);
        ShapeIsActual := DutyShapeObj.UseActual;
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult if no duty curve specified
end;

//----------------------------------------------------------------------------
procedure TGeneratorObj.CalcYearlyMult(Hr: Double);

begin
{Yearly curve is assumed to be hourly only}
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMult(Hr);
        ShapeIsActual := YearlyShapeObj.UseActual;
    end
    else
        ShapeFactor := CDOUBLEONE;  // Defaults to no variation

end;


//----------------------------------------------------------------------------
procedure TGeneratorObj.SetNominalGeneration;
var
    Factor: Double;
    GenOn_Saved: Boolean;

begin
    GenOn_Saved := GenON;
    ShapeFactor := CDOUBLEONE;
    // Check to make sure the generation is ON
    with DSS.ActiveCircuit, DSS.ActiveCircuit.Solution do
    begin
        if not (IsDynamicModel or IsHarmonicModel) then     // Leave generator in whatever state it was prior to entering Dynamic mode
        begin
            GenON := TRUE;   // Init to on then check if it should be off
            if not FForcedON then
                case DispatchMode of
                    LOADMODE:
                        if (DispatchValue > 0.0) and (GeneratorDispatchReference < DispatchValue) then
                            GenON := FALSE;
                    PRICEMODE:
                        if (DispatchValue > 0.0) and (PriceSignal < DispatchValue) then
                            GenON := FALSE;
                end;
        end;


        if not GenON then
        begin
         // If Generator is OFF enter as tiny resistive load (.0001 pu) so we don't get divide by zero in matrix
            Genvars.Pnominalperphase := -0.1 * kWBase / Fnphases;
          // Pnominalperphase   := 0.0;
            Genvars.Qnominalperphase := 0.0;
        end
        else
        begin    // Generator is on, compute it's nominal watts and vars
            with Solution do
                if IsFixed then
                begin
                    Factor := 1.0;   // for fixed generators, set constant
                end
                else
                begin
                    case Mode of
                        TSolveMode.SNAPSHOT:
                            Factor := DSS.ActiveCircuit.GenMultiplier * 1.0;
                        TSolveMode.DAILYMODE:
                        begin
                            Factor := DSS.ActiveCircuit.GenMultiplier;
                            CalcDailyMult(DynaVars.dblHour) // Daily dispatch curve
                        end;
                        TSolveMode.YEARLYMODE:
                        begin
                            Factor := DSS.ActiveCircuit.GenMultiplier;
                            CalcYearlyMult(DynaVars.dblHour);
                        end;
                        TSolveMode.DUTYCYCLE:
                        begin
                            Factor := DSS.ActiveCircuit.GenMultiplier;
                            CalcDutyMult(DynaVars.dblHour);
                        end;
                        TSolveMode.GENERALTIME,   // General sequential time simulation
                        TSolveMode.DYNAMICMODE:
                        begin
                            Factor := DSS.ActiveCircuit.GenMultiplier;
                                       // This mode allows use of one class of load shape
                            case DSS.ActiveCircuit.ActiveLoadShapeClass of
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
                        TSolveMode.MONTECARLO1,
                        TSolveMode.MONTEFAULT,
                        TSolveMode.FAULTSTUDY:
                            Factor := DSS.ActiveCircuit.GenMultiplier * 1.0;
                        TSolveMode.MONTECARLO2,
                        TSolveMode.MONTECARLO3,
                        TSolveMode.LOADDURATION1,
                        TSolveMode.LOADDURATION2:
                        begin
                            Factor := DSS.ActiveCircuit.GenMultiplier;
                            CalcDailyMult(DynaVars.dblHour);
                        end;
                        TSolveMode.PEAKDAY:
                        begin
                            Factor := DSS.ActiveCircuit.GenMultiplier;
                            CalcDailyMult(DynaVars.dblHour);
                        end;
                        TSolveMode.AUTOADDFLAG:
                            Factor := 1.0;
                    else
                        Factor := 1.0
                    end;
                end;

            if not (IsDynamicModel or IsHarmonicModel) then         //******
            begin
                if ShapeIsActual then
                    Genvars.Pnominalperphase := 1000.0 * ShapeFactor.re / Fnphases
                else
                    Genvars.Pnominalperphase := 1000.0 * kWBase * Factor * ShapeFactor.re / Fnphases;

                with Genvars do
                    if GenModel = 3 then
                    begin   { Just make sure present value is reasonable}
                        if Qnominalperphase > varMax then
                            Qnominalperphase := varMax
                        else
                        if Qnominalperphase < varMin then
                            Qnominalperphase := varMin;
                    end
                    else
                    begin
                   { for other generator models}
                        if ShapeIsActual then
                            Qnominalperphase := 1000.0 * ShapeFactor.im / Fnphases
                        else
                            Qnominalperphase := 1000.0 * kvarBase * Factor * ShapeFactor.im / Fnphases;
                    end;
            end;
        end; {ELSE GenON}

        if not (IsDynamicModel or IsHarmonicModel) then
        begin       //******

            case GenModel of
                6:
                    Yeq := Cinv(cmplx(0.0, -Genvars.Xd));  // Gets negated in CalcYPrim
            else
                with Genvars do
                    Yeq := CDivReal(Cmplx(Pnominalperphase, -Qnominalperphase), Sqr(Vbase));   // Vbase must be L-N for 3-phase
                if (Vminpu <> 0.0) then
                    Yeq95 := CDivReal(Yeq, sqr(Vminpu))  // at 95% voltage
                else
                    Yeq95 := Yeq; // Always a constant Z model

                if (Vmaxpu <> 0.0) then
                    Yeq105 := CDivReal(Yeq, Sqr(Vmaxpu))   // at 105% voltage
                else
                    Yeq105 := Yeq;
            end;

          { When we leave here, all the Yeq's are in L-N values}

            if GenModel = 7 then
                with Genvars do
                begin
                    PhaseCurrentLimit := Cdivreal(Cmplx(Pnominalperphase, -Qnominalperphase), VBase95);
                    Model7MaxPhaseCurr := Cabs(PhaseCurrentLimit);
                end;

        end;
    end;  {With ActiveCircuit}

   // If generator state changes, force re-calc of Y matrix
    if GenON <> GenON_Saved then
        YPrimInvalid := TRUE;

end;

//----------------------------------------------------------------------------
procedure TGeneratorObj.RecalcElementData;

begin

    VBase95 := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    varBase := 1000.0 * kvarBase / Fnphases;
    varMin := 1000.0 * kvarMin / Fnphases;
    varMax := 1000.0 * kvarMax / Fnphases;

    {Populate data structures used for interchange with user-written models.}
    with GenVars do
    begin
        Xd := puXd * 1000.0 * SQR(kVGeneratorBase) / kVARating;
        Xdp := puXdp * 1000.0 * SQR(kVGeneratorBase) / kVArating;
        Xdpp := puXdpp * 1000.0 * SQR(kVGeneratorBase) / kVArating;
        Conn := connection;
        NumPhases := Fnphases;
        NumConductors := Fnconds;
    end;

    SetNominalGeneration;

    {Now check for errors.  If any of these came out nil and the string was not nil, give warning}
    if CompareText(YearlyShape, 'none') = 0 then
        YearlyShape := '';
    if CompareText(DailyDispShape, 'none') = 0 then
        DailyDispShape := '';
    if CompareText(DutyShape, 'none') = 0 then
        DutyShape := '';

    if YearlyShapeObj = NIL then
        if Length(YearlyShape) > 0 then
            DoSimpleMsg(DSS, 'WARNING! Yearly load shape: "' + YearlyShape + '" Not Found.', 563);
    if DailyDispShapeObj = NIL then
        if Length(DailyDispShape) > 0 then
            DoSimpleMsg(DSS, 'WARNING! Daily load shape: "' + DailyDispShape + '" Not Found.', 564);
    if DutyShapeObj = NIL then
        if Length(DutyShape) > 0 then
            DoSimpleMsg(DSS, 'WARNING! Duty load shape: "' + DutyShape + '" Not Found.', 565);

    SpectrumObj := DSS.SpectrumClass.Find(Spectrum);
    if SpectrumObj = NIL then
        DoSimpleMsg(DSS, 'ERROR! Spectrum "' + Spectrum + '" Not Found.', 566);


    YQFixed := -varBase / Sqr(VBase);   //10-17-02  Fixed negative sign
    GenVars.Vtarget := Vpu * 1000.0 * GenVars.kVGeneratorBase;

    if Fnphases > 1 then
        GenVars.VTarget := GenVars.VTarget / SQRT3;

    // Initialize to Zero - defaults to PQ generator
    // Solution object will reset after circuit modifications
    DQDV := DQDVSaved;         // for Model = 3
    DeltaQMax := (varMax - varMin) * 0.10;  // Limit to 10% of range

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    {Update any user-written models}
    if Usermodel.Exists then
        UserModel.FUpdateModel;
    if Shaftmodel.Exists then
        Shaftmodel.FUpdateModel;

end;

//----------------------------------------------------------------------------
procedure TGeneratorObj.CalcYPrimMatrix(Ymatrix: TcMatrix);

var
    Y, Yij: Complex;
    i, j: Integer;
    FreqMultiplier: Double;

begin

    FYprimFreq := DSS.ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

    with DSS.ActiveCircuit.solution do
        if IsDynamicModel or IsHarmonicModel then
        begin
            if GenON then
                Y := Yeq   // L-N value computed in initialization routines
            else
                Y := Cmplx(EPSILON, 0.0);

            if Connection = 1 then
                Y := CDivReal(Y, 3.0); // Convert to delta impedance
            Y.im := Y.im / FreqMultiplier;
            Yij := Cnegate(Y);
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
                    begin   {Delta connection}
                        Ymatrix.SetElement(i, i, Y);
                        Ymatrix.AddElement(i, i, Y);  // put it in again
                        for j := 1 to i - 1 do
                            Ymatrix.SetElemsym(i, j, Yij);
                    end;
                end;
            end;

      (**** Removed Neutral / Neutral may float

       IF Connection = 0 Then   With Ymatrix Do  // Take care of neutral issues
         Begin
           AddElement(Fnconds, Fnconds, YNeut);  // Add in user specified Neutral Z, if any
           // Bump up neutral-ground in case neutral ends up floating
           SetElement(Fnconds, Fnconds, CmulReal(GetElement(Fnconds, Fnconds), 1.000001));
         End;

      *)
        end

        else
        begin  //  Regular power flow generator model

       {Yeq is always expected as the equivalent line-neutral admittance}

            Y := cnegate(Yeq);  // negate for generation    Yeq is L-N quantity

       // ****** Need to modify the base admittance for real harmonics calcs
            Y.im := Y.im / FreqMultiplier;

            case Connection of

                0:
                    with YMatrix do
                    begin // WYE
                        Yij := Cnegate(Y);
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
                        Y := CDivReal(Y, 3.0); // Convert to delta impedance
                        Yij := Cnegate(Y);
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
        end;  {ELSE IF Solution.mode}

end;


//----------------------------------------------------------------------------
procedure TGeneratorObj.CalcYPrim;

var
    i: Integer;

begin

     // Build only shunt Yprim
     // Build a dummy Yprim Series so that CalcV does not fail
    if (Yprim = NIL) OR (Yprim.order <> Yorder) OR (Yprim_Shunt = NIL) OR (Yprim_Series = NIL) {YPrimInvalid} then
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

    if DSS.ActiveCircuit.Solution.LoadModel = POWERFLOW then
    begin

        // 12-7-99 we'll start with Yeq in system matrix
        SetNominalGeneration;
        CalcYPrimMatrix(YPrim_Shunt);

    end
    else
    begin

         // ADMITTANCE model wanted

        SetNominalGeneration;
        CalcYPrimMatrix(YPrim_Shunt);

    end;

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));

    YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
    inherited CalcYPrim;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);
 {Add the current into the proper location according to connection}

 {Reverse of similar routine in load  (Cnegates are switched)}

var
    j: Integer;

begin
    case Connection of

        0:
        begin  //Wye
            Caccum(TermArray^[i], Curr);
            Caccum(TermArray^[Fnconds], Cnegate(Curr)); // Neutral
        end;

        1:
        begin //DELTA
            Caccum(TermArray^[i], Curr);
            j := i + 1;
            if j > Fnconds then
                j := 1;
            Caccum(TermArray^[j], Cnegate(Curr));
        end;
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.WriteTraceRecord(const s: String);

var
    i: Integer;

begin

    try
        if (not DSS.InShowResults) then

        begin
            Append(TraceFile);
            Write(TraceFile, Format('%-.g, %d, %-.g, ',
                [DSS.ActiveCircuit.Solution.DynaVars.t + DSS.ActiveCircuit.Solution.Dynavars.IntHour * 3600.0,
                DSS.ActiveCircuit.Solution.Iteration,
                DSS.ActiveCircuit.LoadMultiplier]),
                GetSolutionModeID(DSS), ', ',
                GetLoadModel(DSS), ', ',
                GenModel: 0, ', ',
                DQDV: 8: 0, ', ',
                (V_Avg * 0.001732 / GenVars.kVgeneratorbase): 8: 3, ', ',
                (GenVars.Vtarget - V_Avg): 9: 1, ', ',
                (Genvars.Qnominalperphase * 3.0 / 1.0e6): 8: 2, ', ',
                (Genvars.Pnominalperphase * 3.0 / 1.0e6): 8: 2, ', ',
                s, ', ');
            for i := 1 to nphases do
                Write(TraceFile, (Cabs(InjCurrent^[i])): 8: 1, ', ');
            for i := 1 to nphases do
                Write(TraceFile, (Cabs(ITerminal^[i])): 8: 1, ', ');
            for i := 1 to nphases do
                Write(TraceFile, (Cabs(Vterminal^[i])): 8: 1, ', ');
            Write(TraceFile, GenVars.VThevMag: 8: 1, ', ', Genvars.Theta * 180.0 / PI);
            Writeln(TRacefile);
            CloseFile(TraceFile);
        end;
    except
        On E: Exception do
        begin
        end;

    end;
end;
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.DoConstantPQGen;

{Compute total terminal current for Constant PQ}

var
    i: Integer;
    Curr, V: Complex;
    Vmag: Double;
//   V012,I012 :Array[0..2] of Complex;
//   Iabc :Array[1..3] of Complex;
begin
     //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    ZeroITerminal;

    (*****   Tried this but couldn't get it to work
    CASE Fnphases of

    3:With Genvars Do Begin     // Use Symmetrical Components
          Phase2SymComp(Vterminal, @V012);   // Vterminal is L-N voltages here
                         // Phase2SymComp(InjCurrent, @I012);   // Vterminal is L-G voltages here
          V := V012[1]; // Positive sequence L-N voltage
          Vmag := Cabs(V012[1]);

           { IF   VMag <= VBase95
            THEN Curr := Cnegate(Cmul(Yeq95, V))  // Below 95% (Vminpu) use an impedance model
            ELSE If VMag > VBase105
            THEN Curr := Cnegate(Cmul(Yeq105, V))  // above 105% (Vmaxpu) use an impedance model
            }
            IF   (VMag <= VBase95) or (VMag > VBase105) THEN    Curr := Conjg( Cdiv( CurrentLimit, CDivReal(V, -Vmag)) )
            ELSE With Genvars Do Curr := Conjg(Cdiv(Cmplx(-Pnominalperphase, -Qnominalperphase), V));    // Current INTO pos seq model

         I012[1] := Curr;  // Pos sequence current into the terminal

          If Connection=1 Then I012[0] := CZERO  Else I012[0] := Cdiv(V012[0], cmplx(0.0, xdpp));
          I012[2] := Cdiv(V012[2], cmplx(0.0, xdpp));

          // Negative and Zero Sequence Contributions
         SymComp2Phase(@Iabc, @I012);    // Iabc now desired terminal current
         IF DebugTrace Then Begin
             Append(TraceFile);
             Write(TraceFile,Format('V1=%-.5g, /_%-.5g, ',[Cabs(V), CDang(V)]));
             Write(TraceFile,Format('I1=%-.5g, /_%-.5g, ',[Cabs(Curr), CDang(Curr)]));
             Write(TraceFile,'Iabc=');
             For i := 1 to 3 Do Write(TraceFile,Format('%-.5g, /_%-.5g, ',[ Cabs(Iabc[i]), CDang(Iabc[i])]));
             Writeln(TraceFile);
             CloseFile(TraceFile);
         End;

          For i := 1 to 3 Do Begin
            ITerminal^[i] := Iabc[i];  // Put into Terminal array directly because we have computed line current above
            Caccum(InjCurrent^[i], Cnegate(Iabc[i]));  // subtract in
            If Connection=0 Then Begin
               Caccum(Iterminal^[Fnconds], Cnegate(Iabc[i]));  // Neutral
               Caccum(InjCurrent^[Fnconds], Iabc[i]);  // Neutral
            End;
          End;
          IterminalUpdated := TRUE;  // so that we con't have to recompute for a report
      End
    ELSE
    ****)


    CalcVTerminalPhase; // get actual voltage across each phase of the load
    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        VMag := Cabs(V);

        case Connection of
            0:
            begin  {Wye}
                if VMag <= VBase95 then
                    Curr := Cmul(Yeq95, V)  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmul(Yeq105, V)  // above 105% use an impedance model
                else
                    with Genvars do
                        Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Between 95% -105%, constant PQ
            end;
            1:
            begin  {Delta}
                case Fnphases of
                    2, 3:
                        VMag := VMag / SQRT3;  // L-N magnitude
                else
                        {leave Vmag as is}
                end;

                if VMag <= VBase95 then
                    Curr := Cmul(CdivReal(Yeq95, 3.0), V)  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmul(CdivReal(Yeq105, 3.0), V)  // above 105% use an impedance model
                else
                    with Genvars do
                        Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Between 95% -105%, constant PQ
            end;
        end;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
    {END;}
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.DoConstantZGen;
var
    i: Integer;
    Curr,
    Yeq2: Complex;

begin

// Assume Yeq is kept up to date
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;
    if Connection = 0 then
        Yeq2 := Yeq
    else
        Yeq2 := CdivReal(Yeq, 3.0);

    for i := 1 to Fnphases do
    begin
        Curr := Cmul(Yeq2, Vterminal^[i]);   // Yeq is always line to neutral
        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.DoPVTypeGen;
{Compute total terminal current for Constant P,|V|}

// Constant P, constant |V|

var

    i: Integer;
    DQ: Double;
    Curr: Complex;

begin

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the generator
    ZeroITerminal;

    // Guess at a new var output value
    V_Avg := 0.0;
    for i := 1 to Fnphases do
        V_Avg := V_Avg + Cabs(Vterminal^[i]);

    if Connection = 1 then
        V_Avg := V_Avg / (SQRT3 * Fnphases)
    else
        V_Avg := V_Avg / Fnphases;

   // 12-9-99 added empirical 0.7 factor to improve iteration
   // 12-17-99 changed to 0.1 because first guess was consistently too high
    DQ := PVFactor * DQDV * (GenVars.Vtarget - V_Avg);   // Vtarget is L-N
    if (Abs(DQ) > DeltaQMax) then
        if (DQ < 0.0) then
            DQ := -DeltaQMax
        else
            DQ := DeltaQMax;
    with Genvars do
        Qnominalperphase := Qnominalperphase + DQ;

   { Test Limits}
    with Genvars do
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
            Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), Vterminal^[i]));
            StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
            IterminalUpdated := TRUE;
            StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
        end;
    end; {With}
end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.DoFixedQGen;

{Compute total terminal current for Fixed Q}
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

        case Connection of
            0:
            begin
                if VMag <= VBase95 then
                    Curr := Cmul(Cmplx(Yeq95.re, YQfixed), V)  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmul(Cmplx(Yeq105.re, YQfixed), V)  // above 105% use an impedance model
                else
                    Curr := Conjg(Cdiv(Cmplx(Genvars.Pnominalperphase, varBase), V));
            end;
            1:
            begin
                case Fnphases of
                    2, 3:
                        VMag := VMag / SQRT3;  // L-N magnitude
                else
                    {leave Vmag as is}
                end;
                if VMag <= VBase95 then
                    Curr := Cmul(Cmplx(Yeq95.re / 3.0, YQfixed / 3.0), V)  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmul(Cmplx(Yeq105.re / 3.0, YQfixed / 3.0), V)  // above 105% use an impedance model
                else
                    Curr := Conjg(Cdiv(Cmplx(Genvars.Pnominalperphase, varBase), V));
            end;
        end;
        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.DoFixedQZGen;

{Compute total terminal current for }
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

        case Connection of
            0:
            begin
                if Vmag <= VBase95 then
                    Curr := Cmul(Cmplx(Yeq95.re, YQfixed), V)  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmul(Cmplx(Yeq105.re, YQfixed), V)
                else
                begin
                    Curr := Conjg(Cdiv(Cmplx(Genvars.Pnominalperphase, 0.0), V)); // P component of current
                    Caccum(Curr, Cmul(Cmplx(0.0, YQFixed), V));  // add in Q component of current
                end;
            end;
            1:
            begin
                case Fnphases of
                    2, 3:
                        VMag := VMag / SQRT3;  // L-N magnitude
                else
                      {leave Vmag as is}
                end;
                if Vmag <= VBase95 then
                    Curr := Cmul(Cmplx(Yeq95.re / 3.0, YQfixed / 3.0), V)  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmul(Cmplx(Yeq105.re / 3.0, YQfixed / 3.0), V)
                else
                begin
                    Curr := Conjg(Cdiv(Cmplx(Genvars.Pnominalperphase, 0.0), V)); // P component of current
                    Caccum(Curr, Cmul(Cmplx(0.0, YQFixed / 3.0), V));  // add in Q component of current
                end;
            end;
        end;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end; {FOR}
end;
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.DoUserModel;
{Compute total terminal Current from User-written model}
var
    i: Integer;

begin

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

    if UserModel.Exists then    // Check automatically selects the usermodel if true
    begin
         //AppendToEventLog('Wnominal=', Format('%-.5g',[Pnominalperphase]));
        UserModel.FCalc(Vterminal, Iterminal);
        IterminalUpdated := TRUE;
        with DSS.ActiveCircuit.Solution do
        begin          // Negate currents from user model for power flow generator model
            for i := 1 to FnConds do
                Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));
        end;
    end
    else
    begin
        DoSimpleMsg(DSS, 'Generator.' + name + ' model designated to use user-written model, but user-written model is not defined.', 567);
    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.DoCurrentLimitedPQ;
{Compute total terminal current for Constant PQ, but limit to max current below
 Vminpu}


var
    i: Integer;
    PhaseCurr, DeltaCurr, VLN, VLL: Complex;
    VMagLN, VMagLL: Double;
    V012: array[0..2] of Complex;  // Sequence voltages

begin
     //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load

    if ForceBalanced and (Fnphases = 3) then
    begin    // convert to pos-seq only
        Phase2SymComp(Vterminal, @V012);
        V012[0] := CZERO; // Force zero-sequence voltage to zero
        V012[2] := CZERO; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, @V012);  // Reconstitute Vterminal as balanced
    end;

    ZeroITerminal;

    for i := 1 to Fnphases do
    begin

        case Connection of
            0:
            begin
                VLN := Vterminal^[i];   // VTerminal is LN for this connection
                VMagLN := Cabs(VLN);
                with Genvars do
                    PhaseCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLN));
                if Cabs(PhaseCurr) > Model7MaxPhaseCurr then
                    PhaseCurr := Conjg(Cdiv(PhaseCurrentLimit, CDivReal(VLN, VMagLN)));

                StickCurrInTerminalArray(ITerminal, Cnegate(PhaseCurr), i);  // Put into Terminal array taking into account connection
                ITerminalUpdated := TRUE;
                StickCurrInTerminalArray(InjCurrent, PhaseCurr, i);  // Put into Terminal array taking into account connection
            end;
            1:
            begin
                VLL := Vterminal^[i];     // VTerminal is LL for this connection
                VMagLL := Cabs(VLL);
                case Fnphases of
                    2, 3:   // 2 or 3 phase generator model 7
                    begin
                        with Genvars do
                            DeltaCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLL));
                        if Cabs(DeltaCurr) * SQRT3 > Model7MaxPhaseCurr then
                            DeltaCurr := Conjg(Cdiv(PhaseCurrentLimit, CDivReal(VLL, VMagLL / SQRT3)));
                    end
                else  // 1-phase generator model 7
                    with Genvars do
                        DeltaCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLL));
                    if Cabs(DeltaCurr) > Model7MaxPhaseCurr then
                        DeltaCurr := Conjg(Cdiv(PhaseCurrentLimit, CDivReal(VLL, VMagLL)));
                end;

                StickCurrInTerminalArray(ITerminal, Cnegate(DeltaCurr), i);  // Put into Terminal array taking into account connection
                ITerminalUpdated := TRUE;
                StickCurrInTerminalArray(InjCurrent, DeltaCurr, i);  // Put into Terminal array taking into account connection
            end;
        end;

    end;

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.DoDynamicMode;

{Compute Total Current and add into InjTemp}

var
    i: Integer;
    V012,
    I012: array[0..2] of Complex;

begin

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array  and computes VTerminal L-N

   {Inj = -Itotal (in) - Yprim*Vtemp}

    case GenModel of

        6:
            if UserModel.Exists then       // auto selects model
            begin   {We have total currents in Iterminal}
                UserModel.FCalc(Vterminal, Iterminal);  // returns terminal currents in Iterminal
            end
            else
            begin
                DoSimpleMsg(DSS, Format('Dynamics model missing for Generator.%s ', [Name]), 5671);
                DSS.SolutionAbort := TRUE;
            end;
    else

        case Fnphases of  {No user model, use default Thevinen equivalent for standard Generator model}
            1:
                with Genvars do
                begin
                   // 1-phase generators have 2 conductors
                    case Genmodel of
                        7:
                        begin  // simple inverter model
                                  // Assume inverter stays in phase with terminal voltage
                            CalcVthev_Dyn_Mod7(CSub(VTerminal^[1], VTerminal^[2]));
                        end;
                    else
                        CalcVthev_Dyn;  // Update for latest phase angle
                    end;


                    ITerminal^[1] := CDiv(CSub(Csub(VTerminal^[1], Vthev), VTerminal^[2]), Zthev);  // ZThev is based on Xd'
                    if Genmodel = 7 then
                    begin
                        if Cabs(Iterminal^[1]) > Model7MaxPhaseCurr then   // Limit the current but keep phase angle
                            ITerminal^[1] := ptocomplex(topolar(Model7MaxPhaseCurr, cang(Iterminal^[1])));
                    end;

                    ITerminal^[2] := Cnegate(ITerminal^[1]);
                end;

            3:
                with Genvars do
                begin
                    Phase2SymComp(Vterminal, @V012);

                    case GenModel of
                        7:
                        begin  // simple inverter model
                                // Positive Sequence Contribution to Iterminal
                                // Assume inverter stays in phase with pos seq voltage
                                // and pos seq current is limited
                            CalcVthev_Dyn_Mod7(V012[1]);

                                // Positive Sequence Contribution to Iterminal
                                // Ref Frame here is all L-N

                            I012[1] := CDiv(Csub(V012[1], Vthev), Zthev); // ZThev is based on Xd'
                            if Cabs(I012[1]) > Model7MaxPhaseCurr  // Limit the current but keep phase angle
                            then
                                I012[1] := ptocomplex(topolar(Model7MaxPhaseCurr, cang(I012[1])));
                            if ForceBalanced  // set the negative sequence current
                            then
                                I012[2] := CZERO
                            else
                                I012[2] := Cdiv(V012[2], Zthev);  // for inverter ZThev is  (Xd' + j0)

                        end
                    else
                            // Positive Sequence Contribution to Iterminal
                        CalcVthev_Dyn;  // Update for latest phase angle

                            // Positive Sequence Contribution to Iterminal
                        I012[1] := CDiv(Csub(V012[1], Vthev), Zthev);  // ZThev is based on Xd'
                        I012[2] := Cdiv(V012[2], Cmplx(0.0, Xdpp));  // machine use Xd"
                    end;

                      {Adjust for generator connection}
                    if (Connection = 1) or ForceBalanced then
                        I012[0] := CZERO
                    else
                        I012[0] := Cdiv(V012[0], Cmplx(0.0, Xdpp));

                    SymComp2Phase(ITerminal, @I012);  // Convert back to phase components

                      // Neutral current
                    if Connection = 0 then
                        ITerminal^[FnConds] := Cnegate(CmulReal(I012[0], 3.0));
                end;
        else
            DoSimpleMsg(DSS, Format('Dynamics mode is implemented only for 1- or 3-phase Generators. Generator.%s has %d phases.', [name, Fnphases]), 5671);
            DSS.SolutionAbort := TRUE;
        end;

    end;

    IterminalUpdated := TRUE;

    {Add it into inj current array}
    for i := 1 to FnConds do
        Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));

   {Take Care of any shaft model calcs}
    if (GenModel = 6) and ShaftModel.Exists then      // auto selects model
    begin           // Compute Mech Power to shaft
        ShaftModel.FCalc(Vterminal, Iterminal);     // Returns pshaft at least
    end;
end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.DoHarmonicMode;

{Compute Injection Current Only when in harmonics mode}

{Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built}
{Vd is the fundamental frequency voltage behind Xd" for phase 1}

var
    i: Integer;
    E: Complex;
    GenHarmonic: Double;

begin

    ComputeVterminal;

    with DSS.ActiveCircuit.Solution do
    begin
        GenHarmonic := Frequency / GenFundamental;
        E := CmulReal(SpectrumObj.GetMult(GenHarmonic), GenVars.VThevHarm); // Get base harmonic magnitude
        RotatePhasorRad(E, GenHarmonic, GenVars.ThetaHarm);  // Time shift by fundamental frequency phase shift
        for i := 1 to Fnphases do
        begin
            cBuffer[i] := E;
            if i < Fnphases then
                RotatePhasorDeg(E, GenHarmonic, -120.0);  // Assume 3-phase generator
        end;
    end;

   {Handle Wye Connection}
    if Connection = 0 then
        cbuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

   {Inj currents = Yprim (E) }
    YPrim.MVMult(InjCurrent, @cBuffer);

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.CalcVTerminalPhase;

var
    i, j: Integer;

begin

{ Establish phase voltages and stick in Vterminal}
    case Connection of

        0:
        begin
            with DSS.ActiveCircuit.Solution do
                for i := 1 to Fnphases do
                    Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[Fnconds]);
        end;

        1:
        begin
            with DSS.ActiveCircuit.Solution do
                for i := 1 to Fnphases do
                begin
                    j := i + 1;
                    if j > Fnconds then
                        j := 1;
                    Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[j]);
                end;
        end;

    end;

    GeneratorSolutionCount := DSS.ActiveCircuit.Solution.SolutionCount;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.CalcVTerminal;

{Put terminal voltages in an array}


begin

    ComputeVTerminal;

    GeneratorSolutionCount := DSS.ActiveCircuit.Solution.SolutionCount;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.CalcGenModelContribution;
// Calculates generator current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

begin
    IterminalUpdated := FALSE;
    with DSS.ActiveCircuit, Solution do
    begin
        if IsDynamicModel then
            DoDynamicMode
        else
        if IsHarmonicModel and (Frequency <> Fundamental) then
            DoHarmonicMode
        else
        begin
           //  compute currents and put into InjTemp array;
            case GenModel of
                1:
                    DoConstantPQGen;
                2:
                    DoConstantZGen;
                3:
                    DoPVTypeGen;  // Constant P, |V|
                4:
                    DoFixedQGen;
                5:
                    DoFixedQZGen;
                6:
                    DoUserModel;
                7:
                    DoCurrentLimitedPQ;
            else
                DoConstantPQGen;  // for now, until we implement the other models.
            end;
        end; {ELSE}
    end; {WITH}

   {When this is done, ITerminal is up to date}

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.CalcInjCurrentArray;


// Difference between currents in YPrim and total current


begin


// Now Get Injection Currents
    if GenSwitchOpen then
        ZeroInjCurrent
    else
        CalcGenModelContribution;

(*  We're not going to mess with this logic here -- too complicated: Use an open line in series
    to look at open phase conditions.

  ELSE Begin

   // some terminals not closed  use admittance model for injection
      If OpenGeneratorSolutionCount <> ActiveCircuit.Solution.SolutionCount Then Begin

      // Rebuild the Yprimopencond if a new solution because values may have changed.

        // only reallocate when necessary
        If YPrimOpenCond=nil Then YPrimOpenCond := TcMatrix.CreateMatrix(Yorder)
        ELSE YPrimOpenCond.Clear;
        If YPrimOpenCond.Order <> Yorder Then Begin
           YPrimOpenCond.Free;
           YPrimOpenCond := TcMatrix.CreateMatrix(Yorder);
        End;
        CalcYPrimMatrix(YPrimOpenCond);

        {Now Account for the Open Conductors}
        {For any conductor that is open, zero out row and column}
         With YPrimOpenCond Do Begin
           k := 0;
           FOR i := 1 TO Fnterms Do Begin
             FOR j := 1 TO Fnconds Do Begin
                 If Not Terminals^[i].Conductors^[j].Closed Then Begin
                    ZeroRow(j+k);
                    ZeroCol(j+k);
                    SetElement(j+k, j+k, Cmplx(1.0e-12,0.0));  // In case node gets isolated
                 End;
             End;
             k := k+Fnconds;
           End;
         End;
         OpenGeneratorSolutionCount := ActiveCircuit.Solution.SolutionCount;
         
      End;

      With ActiveCircuit.Solution Do
      FOR i := 1 TO Yorder Do Begin
          Ref := NodeRef^[i];
          If Ref=0 Then Vterminal^[i] := cZero
          ELSE  Vterminal^[i] := V^[ref];
      End;
      YPrimOpenCond.MVmult(InjTemp, Vterminal);
      For i := 1 to Yorder Do InjTemp^[i] := Cnegate(InjTemp^[i]);
   End;
 *)
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.GetTerminalCurrents(Curr: pComplexArray);

// Compute total Currents


begin
    with DSS.ActiveCircuit.Solution do
    begin
        if IterminalSolutionCount <> DSS.ActiveCircuit.Solution.SolutionCount then
        begin     // recalc the contribution
            if not GenSwitchOpen then
                CalcGenModelContribution;  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr);
    end;

    if (DebugTrace) then
        WriteTraceRecord('TotalCurrent');

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TGeneratorObj.InjCurrents: Integer;


begin

    with DSS.ActiveCircuit.Solution do
    begin
        if LoadsNeedUpdating then
            SetNominalGeneration; // Set the nominal kW, etc for the type of solution being done

        CalcInjCurrentArray;          // Difference between currents in YPrim and total terminal current

        if (DebugTrace) then
            WriteTraceRecord('Injection');

       // Add into System Injection Current Array

        Result := inherited InjCurrents;

    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.ResetRegisters;

var
    i: Integer;

begin
    for i := 1 to NumGenregisters do
        Registers[i] := 0.0;
    for i := 1 to NumGenregisters do
        Derivatives[i] := 0.0;
    FirstSampleAfterReset := TRUE;  // initialize for trapezoidal integration
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);

begin
    if DSS.ActiveCircuit.TrapezoidalIntegration then
    begin
        {Trapezoidal Rule Integration}
        if not FirstSampleAfterReset then
            Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
    end
    else   {Plain Euler integration}
        Registers[Reg] := Registers[Reg] + Interval * Deriv;

    Derivatives[Reg] := Deriv;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.TakeSample;
// Update Energy from metered zone

var
    S: Complex;
    Smag: Double;
    HourValue: Double;

begin

// Compute energy in Generator branch
    if Enabled then
    begin

        if GenON then
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

        if GenON or DSS.ActiveCircuit.TrapezoidalIntegration then
      {Make sure we always integrate for Trapezoidal case
       Don't need to for Gen Off and normal integration}
            with DSS.ActiveCircuit.Solution do
            begin
                if DSS.ActiveCircuit.PositiveSequence then
                begin
                    S := CmulReal(S, 3.0);
                    Smag := 3.0 * Smag;
                end;
                Integrate(Reg_kWh, S.re, IntervalHrs);   // Accumulate the power
                Integrate(Reg_kvarh, S.im, IntervalHrs);
                SetDragHandRegister(Reg_MaxkW, abs(S.re));
                SetDragHandRegister(Reg_MaxkVA, Smag);
                Integrate(Reg_Hours, HourValue, IntervalHrs);  // Accumulate Hours in operation
                Integrate(Reg_Price, S.re * DSS.ActiveCircuit.PriceSignal * 0.001, IntervalHrs);  // Accumulate Hours in operation
                FirstSampleAfterReset := FALSE;
            end;
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TGeneratorObj.Get_PresentkW: Double;
begin
    Result := Genvars.Pnominalperphase * 0.001 * Fnphases;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TGeneratorObj.Get_PresentkV: Double;
begin
    Result := Genvars.kVGeneratorBase;
end;

function TGeneratorObj.Get_Presentkvar: Double;
begin
    Result := Genvars.Qnominalperphase * 0.001 * Fnphases;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.InitDQDVCalc;

begin
    DQDV := 0.0;
    Genvars.Qnominalperphase := 0.5 * (varmax + varmin);   // avg of the limits
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.BumpUpQ;
{Bump up vars by 10% of range for next calc}
begin
    with Genvars do
        Qnominalperphase := Qnominalperphase + 0.1 * (varmax - varmin);
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.RememberQV;

var
    i: Integer;

begin
    var_Remembered := Genvars.Qnominalperphase;
    CalcVTerminal;
    V_Avg := 0.0;
    for i := 1 to Fnphases do
        V_Avg := V_Avg + Cabs(Vterminal^[i]);
    V_Avg := V_Avg / Fnphases;
    V_Remembered := V_Avg;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.CalcDQDV;
var
    Vdiff: Double;
    i: Integer;
begin

    CalcVTerminal;
    V_Avg := 0.0;
    for i := 1 to Fnphases do
        V_Avg := V_Avg + Cabs(Vterminal^[i]);
    V_Avg := V_Avg / Fnphases;

    Vdiff := V_Avg - V_Remembered;
    if (Vdiff <> 0.0) then
        DQDV := (Genvars.Qnominalperphase - var_Remembered) / Vdiff
    else
        DQDV := 0.0;  // Something strange has occured
                       // this will force a de facto P,Q model
    DQDVSaved := DQDV;  //Save for next time  Allows generator to be enabled/disabled during simulation
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.ResetStartPoint;

begin
    Genvars.Qnominalperphase := 1000.0 * kvarBase / Fnphases;
end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TGeneratorObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i, idx: Integer;

begin
    inherited DumpProperties(F, Complete);

    Writeln(F, '!DQDV=', DQDV: 10: 2);


    with ParentClass do
        for i := 1 to NumProperties do
        begin
            idx := PropertyIdxMap[i];
            case idx of
                34, 36:
                    Writeln(F, '~ ', PropertyName^[i], '=(', PropertyValue[idx], ')')
            else
                Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[idx]);
            end;
        end;

    Writeln(F);

end;


procedure TGeneratorObj.InitHarmonics;
var
    E, Va: complex;
begin

    YPrimInvalid := TRUE;  // Force rebuild of YPrims
    GenFundamental := DSS.ActiveCircuit.Solution.Frequency;  // Whatever the frequency is when we enter here.

    with GenVars do
    begin

        Yeq := Cinv(Cmplx(0.0, Xdpp));      // used for current calcs  Always L-N

         {Compute reference Thevinen voltage from phase 1 current}

        if GenON then
        begin

            ComputeIterminal;  // Get present value of current

            with DSS.ActiveCircuit.solution do
                case Connection of
                    0:
                    begin {wye - neutral is explicit}
                        Va := Csub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[Fnconds]]);
                    end;
                    1:
                    begin  {delta -- assume neutral is at zero}
                        Va := NodeV^[NodeRef^[1]];
                    end;
                end;

            E := Csub(Va, Cmul(Iterminal^[1], cmplx(0.0, Xdpp)));
            Vthevharm := Cabs(E);   // establish base mag and angle
            ThetaHarm := Cang(E);
        end
        else
        begin
            Vthevharm := 0.0;
            ThetaHarm := 0.0;
        end;
    end;

end;

procedure TGeneratorObj.InitPropertyValues(ArrayOffset: Integer);

begin

    PropertyValue[1] := '3';     //'phases';
    PropertyValue[2] := Getbus(1);         //'bus1';
    PropertyValue[3] := '12.47';
    PropertyValue[4] := '100';
    PropertyValue[5] := '.80';
    PropertyValue[6] := '1';
    PropertyValue[7] := '';
    PropertyValue[8] := '';
    PropertyValue[9] := '';
    PropertyValue[10] := 'Default';
    PropertyValue[11] := '0.0';
    PropertyValue[12] := 'wye';
    PropertyValue[13] := '60';
    PropertyValue[14] := '0'; // 'rneut'; // if entered -, assume open
    PropertyValue[15] := '0';  //'xneut';
    PropertyValue[16] := 'variable'; //'status'  fixed or variable
    PropertyValue[17] := '1'; //'class'
    PropertyValue[18] := '1.0';
    PropertyValue[19] := Str_Real(kvarMax, 3);
    PropertyValue[20] := Str_Real(kvarMin, 3);
    PropertyValue[21] := '0.1';
    PropertyValue[22] := 'no';
    PropertyValue[23] := '0.90';
    PropertyValue[24] := '1.10';
    PropertyValue[25] := 'No';
    PropertyValue[26] := Format('%-g', [GenVars.kVARating]);
    PropertyValue[27] := Format('%-g', [GenVars.kVARating * 0.001]);
    PropertyValue[28] := Format('%-g', [GenVars.puXd]);
    PropertyValue[29] := Format('%-g', [GenVars.puXdp]);
    PropertyValue[30] := Format('%-g', [GenVars.puXdpp]);
    PropertyValue[31] := Format('%-g', [GenVars.Hmass]);
    PropertyValue[32] := Format('%-g', [GenVars.Dpu]);
    PropertyValue[33] := '';
    PropertyValue[34] := '';
    PropertyValue[35] := '';
    PropertyValue[36] := '';
    PropertyValue[37] := '0';
    PropertyValue[38] := 'No';
    PropertyValue[39] := '20';

    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TGeneratorObj.InitStateVars;
var
    {VNeut,}

    i: Integer;
    V012,
    I012: array[0..2] of Complex;
    Vabc: array[1..3] of Complex;

begin
    YPrimInvalid := TRUE;  // Force rebuild of YPrims

    with GenVars do
    begin

        case Genmodel of
            7:
                Zthev := Cmplx(Xdp, 0.0); // use Xd' as an equivalent R for the inverter
        else
            Zthev := Cmplx(Xdp / XRdp, Xdp);
        end;

        Yeq := Cinv(Zthev);

     {Compute nominal Positive sequence voltage behind transient reactance}

        if GenON then
            with DSS.ActiveCircuit.Solution do
            begin

                ComputeIterminal;

                case Fnphases of

                    1:
                    begin
                        Edp := Csub(CSub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[2]]), Cmul(ITerminal^[1], Zthev));
                        VThevMag := Cabs(Edp);
                    end;

                    3:
                    begin
                 // Calculate Edp based on Pos Seq only
                        Phase2SymComp(ITerminal, @I012);
                     // Voltage behind Xdp  (transient reactance), volts

                        for i := 1 to FNphases do
                            Vabc[i] := NodeV^[NodeRef^[i]];   // Wye Voltage
                        Phase2SymComp(@Vabc, @V012);
                        Edp := Csub(V012[1], Cmul(I012[1], Zthev));    // Pos sequence
                        VThevMag := Cabs(Edp);
                    end;
                else
                    DoSimpleMsg(DSS, Format('Dynamics mode is implemented only for 1- or 3-phase Generators. Generator.' + name + ' has %d phases.', [Fnphases]), 5672);
                    DSS.SolutionAbort := TRUE;
                end;


         // Shaft variables
         // Theta is angle on Vthev[1] relative to system reference
         //Theta  := Cang(Vthev^[1]);   // Assume source at 0
                Theta := Cang(Edp);
                if GenModel = 7 then
                    Model7LastAngle := Theta;

                dTheta := 0.0;
                w0 := Twopi * DSS.ActiveCircuit.Solution.Frequency;
         // recalc Mmass and D in case the frequency has changed
                with GenVars do
                begin
                    GenVars.Mmass := 2.0 * GenVars.Hmass * GenVars.kVArating * 1000.0 / (w0);   // M = W-sec
                    D := Dpu * kVArating * 1000.0 / (w0);
                end;
                Pshaft := -Power[1].re; // Initialize Pshaft to present power Output

                Speed := 0.0;    // relative to synch speed
                dSpeed := 0.0;

         // Init User-written models
         //Ncond:Integer; V, I:pComplexArray; const X,Pshaft,Theta,Speed,dt,time:Double
                with DSS.ActiveCircuit.Solution do
                    if GenModel = 6 then
                    begin
                        if UserModel.Exists then
                            UserModel.FInit(Vterminal, Iterminal);
                        if ShaftModel.Exists then
                            ShaftModel.Finit(Vterminal, Iterminal);
                    end;

            end
        else
        begin
            Vthev := cZERO;
            Theta := 0.0;
            dTheta := 0.0;
            w0 := 0;
            Speed := 0.0;
            dSpeed := 0.0;
        end;
    end;  {With}
end;

procedure TGeneratorObj.IntegrateStates;

var
    TracePower: Complex;


begin
   // Compute Derivatives and then integrate

    ComputeIterminal;

// Check for user-written exciter model.
    //Function(V, I:pComplexArray; const Pshaft,Theta,Speed,dt,time:Double)

    with DSS.ActiveCircuit.Solution, GenVars do
    begin

        with DynaVars do
            if (IterationFlag = 0) then
            begin {First iteration of new time step}
                ThetaHistory := Theta + 0.5 * h * dTheta;
                SpeedHistory := Speed + 0.5 * h * dSpeed;
            end;

      // Compute shaft dynamics
        TracePower := TerminalPowerIn(Vterminal, Iterminal, FnPhases);
        dSpeed := (Pshaft + TracePower.re - D * Speed) / Mmass;
//      dSpeed := (Torque + TerminalPowerIn(Vtemp,Itemp,FnPhases).re/Speed) / (Mmass);
        dTheta := Speed;

     // Trapezoidal method
        with DynaVars do
        begin
            Speed := SpeedHistory + 0.5 * h * dSpeed;
            Theta := ThetaHistory + 0.5 * h * dTheta;
        end;

      // Write Dynamics Trace Record
        if DebugTrace then
        begin
            Append(TraceFile);
            Write(TraceFile, Format('t=%-.5g ', [Dynavars.t]));
            Write(TraceFile, Format(' Flag=%d ', [Dynavars.Iterationflag]));
            Write(TraceFile, Format(' Speed=%-.5g ', [Speed]));
            Write(TraceFile, Format(' dSpeed=%-.5g ', [dSpeed]));
            Write(TraceFile, Format(' Pshaft=%-.5g ', [PShaft]));
            Write(TraceFile, Format(' P=%-.5g Q= %-.5g', [TracePower.Re, TracePower.im]));
            Write(TraceFile, Format(' M=%-.5g ', [Mmass]));
            Writeln(TraceFile);
            CloseFile(TraceFile);
        end;

        if GenModel = 6 then
        begin
            if UserModel.Exists then
                UserModel.Integrate;
            if ShaftModel.Exists then
                ShaftModel.Integrate;
        end;


    end;
end;

function TGeneratorObj.Get_Variable(i: Integer): Double;
{Return variables one at a time}

var
    N, k: Integer;

begin
    N := 0;
    Result := -9999.99;  // error return value
    if i < 1 then
        Exit;  // Someone goofed

    with GenVars do
        case i of
            1:
                Result := (w0 + Speed) / TwoPi;  // Frequency, Hz
            2:
                Result := (Theta) * RadiansToDegrees;  // Report in Deg
            3:
                Result := Cabs(Vthev) / vbase;      // Report in pu
            4:
                Result := Pshaft;
            5:
                Result := dSpeed * RadiansToDegrees; // Report in Deg      57.29577951
            6:
                Result := dTheta;
        else
        begin
            if UserModel.Exists then
            begin
                N := UserModel.FNumVars;
                k := (i - NumGenVariables);
                if k <= N then
                begin
                    Result := UserModel.FGetVariable(k);
                    Exit;
                end;
            end;

           {If we get here, must be in the Shaft Model if anywhere}
            if ShaftModel.Exists then
            begin
                k := i - (NumGenVariables + N);
                if k > 0 then
                    Result := ShaftModel.FGetVariable(k);
            end;
        end;
        end;

end;

procedure TGeneratorObj.Set_Variable(i: Integer; Value: Double);
var
    N, k: Integer;

begin
    N := 0;
    if i < 1 then
        Exit;  // Someone goofed
    with GenVars do
        case i of
            1:
                Speed := (Value - w0) * TwoPi;
            2:
                Theta := Value / RadiansToDegrees; // deg to rad
            3: ;// meaningless to set Vd := Value * vbase; // pu to volts
            4:
                Pshaft := Value;
            5:
                dSpeed := Value / RadiansToDegrees;
            6:
                dTheta := Value;
        else
        begin
            if UserModel.Exists then
            begin
                N := UserModel.FNumVars;
                k := (i - NumGenVariables);
                if k <= N then
                begin
                    UserModel.FSetVariable(k, Value);
                    Exit;
                end;
            end;
         // If we get here, must be in the shaft model
            if ShaftModel.Exists then
            begin
                k := (i - (NumGenVariables + N));
                if k > 0 then
                    ShaftModel.FSetVariable(k, Value);
            end;
        end;
        end;
end;

procedure TGeneratorObj.GetAllVariables(States: pDoubleArray);

var
    i, N: Integer;
begin
    N := 0;
    for i := 1 to NumGenVariables do
        States^[i] := Variable[i];

    if UserModel.Exists then
    begin
        N := UserModel.FNumVars;
        UserModel.FGetAllVars(@States^[NumGenVariables + 1]);
    end;

    if ShaftModel.Exists then
    begin
        ShaftModel.FGetAllVars(@States^[NumGenVariables + 1 + N]);
    end;
end;

function TGeneratorObj.NumVariables: Integer;
begin
    Result := NumGenVariables;
    if UserModel.Exists then
        Result := Result + UserModel.FNumVars;
    if ShaftModel.Exists then
        Result := Result + ShaftModel.FNumVars;
end;

function TGeneratorObj.VariableName(i: Integer): String;
const
    BuffSize = 255;
var
    n,
    i2: Integer;
    Buff: array[0..BuffSize] of AnsiChar;
    pName: pAnsichar;

begin
    n := 0;
    if i < 1 then
        Exit;  // Someone goofed
    case i of
        1:
            Result := 'Frequency';
        2:
            Result := 'Theta (Deg)';
        3:
            Result := 'Vd';
        4:
            Result := 'PShaft';
        5:
            Result := 'dSpeed (Deg/sec)';
        6:
            Result := 'dTheta (Deg)';
    else
    begin
        if UserModel.Exists then  // Checks for existence and Selects
        begin
            pName := @Buff;
            n := UserModel.FNumVars;
            i2 := i - NumGenVariables;
            if i2 <= n then
            begin
                 // DLL functions require AnsiString type
                UserModel.FGetVarName(i2, pName, BuffSize);
                Result := String(pName);
                Exit;
            end;
        end;

        if ShaftModel.Exists then
        begin
            pName := @Buff;
            i2 := i - NumGenVariables - n;
            if i2 > 0 then
                UserModel.FGetVarName(i2, pName, BuffSize);
            Result := String(pName);
        end;
    end;
    end;

end;

function TGeneratorObj.GetPropertyValue(Index: Integer): String;

begin
    Result := '';
    case Index of
        3:
            Result := Format('%.6g', [Genvars.kVGeneratorBase]);
        4:
            Result := Format('%.6g', [kWBase]);
        5:
            Result := Format('%.6g', [PFNominal]);
        7:
            Result := Yearlyshape;
        8:
            Result := Dailydispshape;
        9:
            Result := DutyShape;
        13:
            Result := Format('%.6g', [kvarBase]);
        19:
            Result := Format('%.6g', [kvarMax]);
        20:
            Result := Format('%.6g', [kvarMin]);
        26:
            Result := Format('%.6g', [Genvars.kVArating]);
        27:
            Result := Format('%.6g', [Genvars.kVArating * 0.001]);
        34, 36:
        begin
            Result := '(' + inherited GetPropertyValue(index) + ')';
        end;
        37:
            Result := Format('%.6g', [DutyStart]);
        38:
            if ForceBalanced then
                Result := 'Yes'
            else
                Result := 'No';

    else
        Result := inherited GetPropertyValue(index);
    end;
end;

procedure TGeneratorObj.MakePosSequence;

var
    S: String;
    V: Double;

begin

    S := 'Phases=1 conn=wye';

  // Make sure voltage is line-neutral
    if (Fnphases > 1) or (connection <> 0) then
        V := GenVars.kVGeneratorBase / SQRT3
    else
        V := GenVars.kVGeneratorBase;

    S := S + Format(' kV=%-.5g', [V]);

  // Divide the load by no. phases
    if Fnphases > 1 then
    begin
        S := S + Format(' kW=%-.5g  PF=%-.5g', [kWbase / Fnphases, PFNominal]);
        if (PrpSequence^[19] <> 0) or (PrpSequence^[20] <> 0) then
            S := S + Format(' maxkvar=%-.5g  minkvar=%-.5g', [kvarmax / Fnphases, kvarmin / Fnphases]);
        if PrpSequence^[26] > 0 then
            S := S + Format(' kva=%-.5g  ', [genvars.kvarating / Fnphases]);
        if PrpSequence^[27] > 0 then
            S := S + Format(' MVA=%-.5g  ', [genvars.kvarating / 1000.0 / Fnphases]);
    end;

    DSS.Parser.CmdString := S;
    Edit;

    inherited;
end;

procedure TGeneratorObj.Set_ConductorClosed(Index: Integer;
    Value: Boolean);
begin
    inherited;

 // Just turn generator on or off;

    if Value then
        GenSwitchOpen := FALSE
    else
        GenSwitchOpen := TRUE;

end;


procedure TGeneratorObj.Set_PowerFactor(const Value: Double);
begin
    PFNominal := Value;
    SyncUpPowerQuantities;
end;

procedure TGeneratorObj.Set_PresentkV(const Value: Double);
begin
    with Genvars do
    begin
        kVGeneratorBase := Value;
        case FNphases of
            2, 3:
                VBase := kVGeneratorBase * InvSQRT3x1000;
        else
            VBase := kVGeneratorBase * 1000.0;
        end;
    end;
end;

procedure TGeneratorObj.Set_Presentkvar(const Value: Double);
var
    kVA_Gen: Double;

begin
    kvarBase := Value;
    Genvars.Qnominalperphase := 1000.0 * kvarBase / Fnphases; // init to something reasonable
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

procedure TGeneratorObj.Set_PresentkW(const Value: Double);
begin

    kWBase := Value;
    SyncUpPowerQuantities;

end;

procedure TGeneratorObj.SyncUpPowerQuantities;
begin

   // keep kvar nominal up to date with kW and PF
    if (PFNominal <> 0.0) then
    begin
        kvarBase := kWBase * sqrt(1.0 / Sqr(PFNominal) - 1.0);
        Genvars.Qnominalperphase := 1000.0 * kvarBase / Fnphases;
        kvarMax := 2.0 * kvarBase;
        kvarMin := -kvarMax;
        if PFNominal < 0.0 then
            kvarBase := -kvarBase;

        if kVANotSet then
            GenVars.kVARating := kWBase * 1.2;

    end;

end;

procedure TGeneratorObj.SetDragHandRegister(Reg: Integer;
    const Value: Double);
begin
    if Value > Registers[reg] then
        Registers[Reg] := Value;
end;

procedure TGeneratorObj.SetkWkvar(const PkW, Qkvar: Double);
begin

    kWBase := PkW;
    Presentkvar := Qkvar;

end;

procedure TGeneratorObj.CalcVthev_Dyn;
begin
    if GenSwitchOpen then
        GenVars.VThevMag := 0.0;
    Vthev := pclx(GenVars.VthevMag, Genvars.Theta);
end;

procedure TGeneratorObj.CalcVthev_Dyn_Mod7(const V: Complex);
{Adjust VThev to be in phase with V, if possible}
{
 If the voltage magnitude drops below 15% or so, the accuracy of determining the
 phase angle gets flaky. This algorithm approximates the action of a PLL that will
 hold the last phase angle until the voltage recovers.
}
var
    Model7angle: Double;
begin
    if GenSwitchOpen then
        GenVars.VThevMag := 0.0;
   {
      For Phases=1, Vbase is voltage across the terminals.
      Else it is LN voltage.
   }
    if Cabs(V) > 0.2 * Vbase then
        Model7angle := Cang(V)
    else
        Model7Angle := Model7LastAngle;

    Vthev := pclx(GenVars.VthevMag, Model7angle);
    Model7Lastangle := Model7angle;

end;

initialization

    CDOUBLEONE := CMPLX(1.0, 1.0);
//   TWOPI3     := twopi/3.0;

end.
