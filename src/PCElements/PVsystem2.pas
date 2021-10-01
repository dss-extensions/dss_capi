unit PVsystem2;

{
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   Change Log

    1/28/2011 Created from Storage Model


  To Do:
    Make connection to User model
    Yprim for various modes
    Define state vars and dynamics mode behavior
    Complete Harmonics mode algorithm (generator mode is implemented)
}
{
  The PVsystem element is essentially a generator that consists of a PV panel and an inverter.

  The PVsystem element can also produce or absorb vars within the kVA rating of the inverter.
  // WGS: Updated 9/24/2015 to allow for simultaneous modes and additional functionality in the InvControl.
}

//  The PVsystem element is assumed balanced over the no. of phases defined


interface

uses
    Classes,
    PVsystemUserModel,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    ucomplex,
    LoadShape,
    TempShape,
    XYCurve,
    Spectrum,
    ArrayDef,
    Dynamics;

const
    NumPVSystem2Registers = 6;    // Number of energy meter registers
    NumPVSystem2Variables = 13;    // No state variables that need integrating.
    VARMODEPF = 0;
    VARMODEKVAR = 1;

type
  {Struct to pass basic data to user-written DLLs}
    TPVSystem2Vars = packed record

        FkVArating: Double;
        kVPVSystemBase: Double;
        RThev: Double;
        XThev: Double;
        Vthevharm: Double;  {Thevinen equivalent voltage mag  for Harmonic model}
        VthevmagDyn: Double;  {Thevinen equivalent voltage mag  reference for Dynamics model}
        Thetaharm: Double;  {Thevinen equivalent  angle reference for Harmonic model}
        ThetaDyn: Double;  {Thevinen equivalent  angle reference for Dynamics model}
        InitialVAngle: Double;  {initial terminal voltage angle when entering dynamics mode}
        EffFactor: Double;
        TempFactor: Double;
        PanelkW: Double; //computed
        FTemperature: Double;
        FPmpp: Double;
        FpuPmpp: Double;
        FIrradiance: Double;
        MaxDynPhaseCurrent: Double;
        Fkvarlimit: Double; //maximum kvar output of the PVSystem (unsigned)
        Fkvarlimitneg: Double;

    // Variables set from InvControl. They are results of monitor in mode 3
        Vreg: Double; // will be set from InvControl or ExpControl
        Vavg: Double;
        VVOperation: Double;
        VWOperation: Double;
        DRCOperation: Double;
        VVDRCOperation: Double;
        WPOperation: Double;
        WVOperation: Double;
    //        kW_out_desired   :Double;

    {32-bit integers}
        NumPhases: Integer;   {Number of phases}
        NumConductors: Integer; {Total Number of conductors (wye-connected will have 4)}
        Conn: Integer;   // 0 = wye; 1 = Delta
        P_Priority: Boolean;  // default False // added 10/30/2018
        PF_Priority: Boolean;  // default False // added 1/29/2019
    end;

    TPVSystem2 = class(TPCClass)
    PRIVATE
        procedure InterpretConnection(const S: String);
        procedure SetNcondsForConnection;

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const OtherPVsystemObjName: String): Integer; OVERRIDE;

    PUBLIC
        RegisterNames: array[1..NumPVSystem2Registers] of String;

        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function Edit(): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

        procedure ResetRegistersAll;
        procedure SampleAll();
        procedure UpdateAll;
    end;

    TPVsystem2Obj = class(TPCElement)
    PRIVATE
        YEQ: Complex;   // at nominal
        YEQ_Min: Complex;   // at Vmin
        YEQ_Max: Complex;   // at VMax
        PhaseCurrentLimit: Complex;
        Zthev: Complex;

        LastThevAngle: Double;

        DebugTrace: Boolean;
        PVSystemSolutionCount: Integer;
        PVSystemFundamental: Double;  {Thevinen equivalent voltage mag and angle reference for Harmonic model}
        PVsystemObjSwitchOpen: Boolean;
        FirstSampleAfterReset: Boolean;

      //PFSpecified             :Boolean;
      //kvarSpecified           :Boolean;

        ForceBalanced: Boolean;
        CurrentLimited: Boolean;

        kvar_out: Double;
        kW_out: Double;
        kvarRequested: Double;
        Fpf_wp_nominal: Double;
        kWRequested: Double;
        FvarMode: Integer;

        FpctCutIn: Double;
        FpctCutOut: Double;
        FVarFollowInverter: Boolean;
        CutInkW: Double;
        CutOutkW: Double;
        FInverterON: Boolean;
        FpctPminNoVars: Double;
        FpctPminkvarLimit: Double;
        PminNoVars: Double;
        PminkvarLimit: Double;

        pctR: Double;
        pctX: Double;

        OpenPVSystemSolutionCount: Integer;

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
        TShapeValue: Double;

        TraceFile: TFileStream;
        UserModel: TPVsystemUserModel;   {User-Written Models}

        varBase: Double; // Base vars per phase
        VBaseMax: Double;
        VBaseMin: Double;
        Vmaxpu: Double;
        Vminpu: Double;
        YPrimOpenCond: TCmatrix;

        FVWMode: Boolean; //boolean indicating if under volt-watt control mode from InvControl (not ExpControl)
        FVVMode: Boolean; //boolean indicating if under volt-var mode from InvControl
        FWVMode: Boolean; //boolean indicating if under watt-var mode from InvControl
        FWPMode: Boolean; //boolean indicating if under watt-pf mode from InvControl
        FDRCMode: Boolean; //boolean indicating if under DRC mode from InvControl

        procedure CalcDailyMult(Hr: Double);  // now incorporates DutyStart offset
        procedure CalcDutyMult(Hr: Double);
        procedure CalcYearlyMult(Hr: Double);  // now incorporates DutyStart offset

        procedure CalcDailyTemperature(Hr: Double);
        procedure CalcDutyTemperature(Hr: Double);
        procedure CalcYearlyTemperature(Hr: Double);

        procedure ComputePanelPower;
        procedure ComputeInverterPower;
        procedure ComputekWkvar;

        procedure CalcPVSystemModelContribution();   // This is where the power gets computed
        procedure CalcInjCurrentArray();
      (*PROCEDURE CalcVterminal;*)
        procedure CalcVTerminalPhase();

        procedure CalcYPrimMatrix(Ymatrix: TcMatrix);

        procedure DoConstantPQPVsystemObj();
        procedure DoConstantZPVsystemObj();
        procedure DoDynamicMode();
        procedure DoHarmonicMode();
        procedure DoUserModel();

        procedure Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
        procedure SetDragHandRegister(Reg: Integer; const Value: Double);
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);

        procedure WriteTraceRecord(const s: String);

      // PROCEDURE SetKWandKvarOut;
        procedure UpdatePVSystem;    // Update PVSystem elements based on present kW and IntervalHrs variable

        function Get_PresentkW: Double;
        function Get_Presentkvar: Double;
        function Get_PresentIrradiance: Double;

        procedure Set_PresentkV(const Value: Double);
        procedure Set_PowerFactor(const Value: Double);
        procedure Set_pf_wp_nominal(const Value: Double);

        procedure Set_kVARating(const Value: Double);
        procedure Set_Pmpp(const Value: Double);

        procedure kWOut_Calc;

    PROTECTED
        procedure GetTerminalCurrents(Curr: pComplexArray); OVERRIDE;

    PUBLIC
        PVSystemVars: TPVSystem2Vars;

        VBase: Double;  // Base volts suitable for computing currents
        CurrentkvarLimit: Double;
        CurrentkvarLimitNeg: Double;

        AVRMode: Boolean;

        Connection: Integer;  {0 = line-neutral; 1=Delta}
        DailyShape: String;  // Daily (24 HR) PVSystem element irradiance shape
        DailyShapeObj: TLoadShapeObj;  // Daily PVSystem element irradianceShape for this load
        DutyShape: String;  // Duty cycle irradiance shape for changes typically less than one hour
        DutyShapeObj: TLoadShapeObj;  // irradiance Shape for this PVSystem element
        DutyStart: Double; // starting time offset into the DutyShape [hrs] for this PVsystem
        YearlyShape: String;  //
        YearlyShapeObj: TLoadShapeObj;  // Yearly irradiance Shape for this PVSystem element

        DailyTShape: String;
        DailyTShapeObj: TTShapeObj;
        DutyTShape: String;
        DutyTShapeObj: TTShapeObj;
        YearlyTShape: String;
        YearlyTShapeObj: TTShapeObj;

        InverterCurve: String;
        InverterCurveObj: TXYCurveObj;
        Power_TempCurve: String;
        Power_TempCurveObj: TXYCurveObj;

        kvarLimitSet: Boolean;
        kvarLimitNegSet: Boolean;

        FClass: Integer;
        VoltageModel: Integer;   // Variation with voltage
        PFnominal: Double;

        Registers: array[1..NumPVSystem2Registers] of Double;
        Derivatives: array[1..NumPVSystem2Registers] of Double;

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;

        procedure Set_ConductorClosed(Index: Integer; Value: Boolean); OVERRIDE;
        procedure RecalcElementData(); OVERRIDE;
        procedure CalcYPrim(); OVERRIDE;

        function InjCurrents(): Integer; OVERRIDE;
        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        procedure Set_Maxkvar(const Value: Double);
        procedure Set_Maxkvarneg(const Value: Double);

        procedure SetNominalPVSystemOuput();
        procedure Randomize(Opt: Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform


        procedure ResetRegisters;
        procedure TakeSample();

      // Support for Dynamics Mode
        procedure InitStateVars(); OVERRIDE;
        procedure IntegrateStates(); OVERRIDE;

      // Support for Harmonics Mode
        procedure InitHarmonics(); OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(F: TFileStream; Complete: Boolean); OVERRIDE;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;

      {Porperties}
        property PresentIrradiance: Double READ Get_PresentIrradiance WRITE PVSystemVars.FIrradiance;
        property PresentkW: Double READ Get_PresentkW WRITE kWRequested;
        property Presentkvar: Double READ Get_Presentkvar WRITE kvarRequested;
        property PresentkV: Double READ PVSystemVars.kVPVSystemBase WRITE Set_PresentkV;
        property PowerFactor: Double READ PFnominal WRITE Set_PowerFactor;
        property kVARating: Double READ PVSystemVars.FkVARating WRITE Set_kVARating;
        property Pmpp: Double READ PVSystemVars.FPmpp WRITE Set_pmpp;
        property puPmpp: Double READ PVSystemVars.FpuPmpp WRITE PVSystemVars.FpuPmpp;
        property Varmode: Integer READ FvarMode WRITE FvarMode;  // 0=constant PF; 1=kvar specified
        property VWmode: Boolean READ FVWmode WRITE FVWmode;
        property VVmode: Boolean READ FVVmode WRITE FVVmode;
        property WPmode: Boolean READ FWPmode WRITE FWPmode;
        property WVmode: Boolean READ FWVmode WRITE FWVmode;
        property DRCmode: Boolean READ FDRCmode WRITE FDRCmode;
        property InverterON: Boolean READ FInverterON WRITE FInverterON;
        property VarFollowInverter: Boolean READ FVarFollowInverter WRITE FVarFollowInverter;
        property kvarLimit: Double READ PVSystemVars.Fkvarlimit WRITE Set_Maxkvar;
        property kvarLimitneg: Double READ PVSystemVars.Fkvarlimitneg WRITE Set_Maxkvarneg;
        property MinModelVoltagePU: Double READ VminPu;
        property pf_wp_nominal: Double WRITE Set_pf_wp_nominal;
        property IrradianceNow: Double READ ShapeFactor.re;
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
    DSSHelper,
    DSSObjectHelper;

const

// ===========================================================================================
{
   To add a property,
    1) add a property constant to this list
    2) add a handler to the CASE statement in the Edit FUNCTION
    3) add a statement(s) to InitPropertyValues FUNCTION to initialize the string value
    4) add any special handlers to DumpProperties and GetPropertyValue, If needed
}
// ===========================================================================================

    propKV = 3;
    propIrradiance = 4;
    propPF = 5;
    propMODEL = 6;
    propYEARLY = 7;
    propDAILY = 8;
    propDUTY = 9;
    propTYEARLY = 10;
    propTDAILY = 11;
    propTDUTY = 12;
    propCONNECTION = 13;
    propKVAR = 14;
    propPCTR = 15;
    propPCTX = 16;
    propCLASS = 17;
    propInvEffCurve = 18;
    propTemp = 19;
    propPmpp = 20;
    propP_T_Curve = 21;
    propCutin = 22;
    propCutout = 23;
    propVMINPU = 24;
    propVMAXPU = 25;
    propKVA = 26;
    propUSERMODEL = 27;
    propUSERDATA = 28;
    propDEBUGTRACE = 29;
    proppctPmpp = 30;
    propBalanced = 31;
    propLimited = 32;
    propVarFollowInverter = 33;
    propkvarLimit = 34;
    propDutyStart = 35;
    propPpriority = 36;
    propPFpriority = 37;
    propPminNoVars = 38;
    propPminkvarLimit = 39;
    propkvarLimitneg = 40;

    NumPropsThisClass = 40; // Make this agree with the last property constant

var
    cBuffer: array[1..24] of Complex;  // Temp buffer for calcs  24-phase PVSystem element?
    CDOUBLEONE: Complex;

constructor TPVsystem2.Create(dssContext: TDSSContext);  // Creates superstructure for all PVSystem elements
begin
    inherited Create(dssContext);
    Class_Name := 'PVSystem';
    DSSClassType := DSSClassType + PVSYSTEM_ELEMENT;  // In both PCelement and PVSystem element list

    ActiveElement := 0;

    // Set Register names
    RegisterNames[1] := 'kWh';
    RegisterNames[2] := 'kvarh';
    RegisterNames[3] := 'Max kW';
    RegisterNames[4] := 'Max kVA';
    RegisterNames[5] := 'Hours';
    RegisterNames[6] := 'Price($)';

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

destructor TPVsystem2.Destroy;
begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;

procedure TPVsystem2.DefineProperties;
begin
    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;   {see DSSClass}

    // Define Property names
    {
    Using the AddProperty FUNCTION, you can list the properties here in the order you want
    them to appear when properties are accessed sequentially without tags.   Syntax:

    AddProperty( <name of property>, <index in the EDIT Case statement>, <help text>);

    }
    AddProperty('phases', 1,
        'Number of Phases, this PVSystem element.  Power is evenly divided among phases.');
    AddProperty('bus1', 2,
        'Bus to which the PVSystem element is connected.  May include specific node specification.');
    AddProperty('kv', propKV,
        'Nominal rated (1.0 per unit) voltage, kV, for PVSystem element. For 2- and 3-phase PVSystem elements, specify phase-phase kV. ' +
        'Otherwise, specify actual kV across each branch of the PVSystem element. ' +
        'If 1-phase wye (star or LN), specify phase-neutral kV. ' +
        'If 1-phase delta or phase-phase connected, specify phase-phase kV.');  // line-neutral voltage//  base voltage
    AddProperty('irradiance', propIrradiance,
        'Get/set the present irradiance value in kW/sq-m. Used as base value for shape multipliers. ' +
        'Generally entered as peak value for the time period of interest and the yearly, daily, and duty load shape ' +
        'objects are defined as per unit multipliers (just like Loads/Generators).');
    AddProperty('Pmpp', propPmpp,
        'Get/set the rated max power of the PV array for 1.0 kW/sq-m irradiance and a user-selected array temperature. ' +
        'The P-TCurve should be defined relative to the selected array temperature.');
    AddProperty('%Pmpp', proppctPmpp,
        'Upper limit on active power as a percentage of Pmpp.');
    AddProperty('Temperature', propTemp,
        'Get/set the present Temperature. Used as fixed value corresponding to PTCurve property. ' +
        'A multiplier is obtained from the Pmpp-Temp curve and applied to the nominal Pmpp from the irradiance ' +
        'to determine the net array output.');
    AddProperty('pf', propPF,
        'Nominally, the power factor for the output power. Default is 1.0. ' +
        'Setting this property will cause the inverter to operate in constant power factor mode.' +
        'Enter negative when kW and kvar have opposite signs.' + CRLF +
        'A positive power factor signifies that the PVSystem element produces vars ' + CRLF +
        'as is typical for a generator.  ');
    AddProperty('conn', propCONNECTION,
        '={wye|LN|delta|LL}.  Default is wye.');
    AddProperty('kvar', propKVAR,
        'Get/set the present kvar value.  Setting this property forces the inverter to operate in constant kvar mode.');
    AddProperty('kVA', propKVA,
        'kVA rating of inverter. Used as the base for Dynamics mode and Harmonics mode values.');
    AddProperty('%Cutin', propCutin,
        '% cut-in power -- % of kVA rating of inverter. ' +
        'When the inverter is OFF, the power from the array must be greater than this for the inverter to turn on.');
    AddProperty('%Cutout', propCutout,
        '% cut-out power -- % of kVA rating of inverter. ' +
        'When the inverter is ON, the inverter turns OFF when the power from the array drops below this value.');

    AddProperty('EffCurve', propInvEffCurve,
        'An XYCurve object, previously defined, that describes the PER UNIT efficiency vs PER UNIT of rated kVA for the inverter. ' +
        'Inverter output power is discounted by the multiplier obtained from this curve.');

    AddProperty('P-TCurve', propP_T_Curve,
        'An XYCurve object, previously defined, that describes the PV array PER UNIT Pmpp vs Temperature curve. ' +
        'Temperature units must agree with the Temperature property and the Temperature shapes used for simulations. ' +
        'The Pmpp values are specified in per unit of the Pmpp value for 1 kW/sq-m irradiance. ' +
        'The value for the temperature at which Pmpp is defined should be 1.0. ' +
        'The net array power is determined by the irradiance * Pmpp * f(Temperature)');
    AddProperty('%R', propPCTR,
        'Equivalent percent internal resistance, ohms. Default is 50%. Placed in series with internal voltage source' +
        ' for harmonics and dynamics modes. (Limits fault current to about 2 pu if not current limited -- see LimitCurrent) ');
    AddProperty('%X', propPCTX,
        'Equivalent percent internal reactance, ohms. Default is 0%. Placed in series with internal voltage source' +
        ' for harmonics and dynamics modes. ');
    AddProperty('model', propMODEL,
        'Integer code (default=1) for the model to use for power output variation with voltage. ' +
        'Valid values are:' + CRLF + CRLF +
        '1:PVSystem element injects a CONSTANT kW at specified power factor.' + CRLF +
        '2:PVSystem element is modeled as a CONSTANT ADMITTANCE.' + CRLF +
        '3:Compute load injection from User-written Model.');

    AddProperty('Vminpu', propVMINPU,
        'Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. ' +
        'Below this value, the load model reverts to a constant impedance model except for Dynamics model. ' +
        'In Dynamics mode, the current magnitude is limited to the value the power flow would compute for this voltage.');
    AddProperty('Vmaxpu', propVMAXPU,
        'Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. ' +
        'Above this value, the load model reverts to a constant impedance model.');
    AddProperty('Balanced', propBalanced,
        '{Yes | No*} Default is No.  Force balanced current only for 3-phase PVSystems. Forces zero- and negative-sequence to zero. ');
    AddProperty('LimitCurrent', propLimited,
        'Limits current magnitude to Vminpu value for both 1-phase and 3-phase PVSystems similar to Generator Model 7. For 3-phase, ' +
        'limits the positive-sequence current but not the negative-sequence.');
    AddProperty('yearly', propYEARLY,
        'Dispatch shape to use for yearly simulations.  Must be previously defined ' +
        'as a Loadshape object. If this is not specified, the Daily dispatch shape, if any, is repeated ' +
        'during Yearly solution modes. In the default dispatch mode, ' +
        'the PVSystem element uses this loadshape to trigger State changes.');
    AddProperty('daily', propDAILY,
        'Dispatch shape to use for daily simulations.  Must be previously defined ' +
        'as a Loadshape object of 24 hrs, typically.  In the default dispatch mode, ' +
        'the PVSystem element uses this loadshape to trigger State changes.'); // daily dispatch (hourly)
    AddProperty('duty', propDUTY,
        'Load shape to use for duty cycle dispatch simulations such as for solar ramp rate studies. ' +
        'Must be previously defined as a Loadshape object. ' +
        'Typically would have time intervals of 1-5 seconds. ' +
        'Designate the number of points to solve using the Set Number=xxxx command. ' +
        'If there are fewer points in the actual shape, the shape is assumed to repeat.');  // as for wind generation

    AddProperty('Tyearly', propTYEARLY,
        'Temperature shape to use for yearly simulations.  Must be previously defined ' +
        'as a TShape object. If this is not specified, the Daily dispatch shape, if any, is repeated ' +
        'during Yearly solution modes. ' +
        'The PVSystem element uses this TShape to determine the Pmpp from the Pmpp vs T curve. ' +
        'Units must agree with the Pmpp vs T curve.');
    AddProperty('Tdaily', propTDAILY,
        'Temperature shape to use for daily simulations.  Must be previously defined ' +
        'as a TShape object of 24 hrs, typically.  ' +
        'The PVSystem element uses this TShape to determine the Pmpp from the Pmpp vs T curve. ' +
        'Units must agree with the Pmpp vs T curve.'); // daily dispatch (hourly)
    AddProperty('Tduty', propTDUTY,
        'Temperature shape to use for duty cycle dispatch simulations such as for solar ramp rate studies. ' +
        'Must be previously defined as a TShape object. ' +
        'Typically would have time intervals of 1-5 seconds. ' +
        'Designate the number of points to solve using the Set Number=xxxx command. ' +
        'If there are fewer points in the actual shape, the shape is assumed to repeat. ' +
        'The PVSystem model uses this TShape to determine the Pmpp from the Pmpp vs T curve. ' +
        'Units must agree with the Pmpp vs T curve.');  // Cloud transient simulation
    AddProperty('class', propCLASS,
        'An arbitrary integer number representing the class of PVSystem element so that PVSystem values may ' +
        'be segregated by class.'); // integer

    AddProperty('UserModel', propUSERMODEL,
        'Name of DLL containing user-written model, which computes the terminal currents for Dynamics studies, ' +
        'overriding the default model.  Set to "none" to negate previous setting.');
    AddProperty('UserData', propUSERDATA,
        'String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model.');
    AddProperty('debugtrace', propDEBUGTRACE,
        '{Yes | No }  Default is no.  Turn this on to capture the progress of the PVSystem model ' +
        'for each iteration.  Creates a separate file for each PVSystem element named "PVSystem_name.CSV".');
    AddProperty('VarFollowInverter', propVarFollowInverter,
        'Boolean variable (Yes|No) or (True|False). Defaults to False which indicates that the reactive power generation/absorption does not respect the inverter status.' +
        'When set to True, the PVSystem reactive power generation/absorption will cease when the inverter status is off, due to panel kW dropping below %Cutout.  The reactive power ' +
        'generation/absorption will begin again when the panel kW is above %Cutin.  When set to False, the PVSystem will generate/absorb reactive power regardless of the status of the inverter.');


    AddProperty('DutyStart', propDutyStart,
        'Starting time offset [hours] into the duty cycle shape for this PVSystem, defaults to 0');
    AddProperty('WattPriority', propPPriority,
        '{Yes/No*/True/False} Set inverter to watt priority instead of the default var priority');
    AddProperty('PFPriority', propPFPriority,
        '{Yes/No*/True/False} Set inverter to operate with PF priority when in constant PF mode. If "Yes", value assigned to "WattPriority"' +
        ' is neglected. If controlled by an InvControl with either Volt-Var or DRC or both functions activated, PF priority is neglected and "WattPriority" is considered. Default = No.');

    AddProperty('%PminNoVars', propPminNoVars,
        'Minimum active power as percentage of Pmpp under which there is no vars production/absorption.');

    AddProperty('%PminkvarMax', propPminkvarLimit,
        'Minimum active power as percentage of Pmpp that allows the inverter to produce/absorb reactive power up to its kvarMax or kvarMaxAbs.');


    AddProperty('kvarMax', propkvarLimit,
        'Indicates the maximum reactive power GENERATION (un-signed numerical variable in kvar) for the inverter (as an un-signed value). Defaults to kVA rating of the inverter.');

    AddProperty('kvarMaxAbs', propkvarLimitneg,
        'Indicates the maximum reactive power ABSORPTION (un-signed numerical variable in kvar) for the inverter (as an un-signed value). Defaults to kVA rating of the inverter.');


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

    // Override default help string
    PropertyHelp[NumPropsThisClass + 1] := 'Name of harmonic voltage or current spectrum for this PVSystem element. ' +
        'A harmonic voltage source is assumed for the inverter. ' +
        'Default value is "default", which is defined when the DSS starts.';

end;

function TPVsystem2.NewObject(const ObjName: String): Integer;
begin
    // Make a new PVSystem element and add it to PVSystem class list
    with ActiveCircuit do
    begin
        ActiveCktElement := TPVsystem2Obj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

procedure TPVsystem2.SetNcondsForConnection;
begin
    with DSS.ActivePVsystem2Obj do
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

procedure TPVsystem2.UpdateAll;
var
    i: Integer;
begin
    for i := 1 to ElementList.Count do
        with TPVsystem2Obj(ElementList.Get(i)) do
            if Enabled then
                UpdatePVSystem;
end;

procedure TPVsystem2.InterpretConnection(const S: String);
  // Accepts
  //    delta or LL           (Case insensitive)
  //    Y, wye, or LN
var
    TestS: String;

begin
    with DSS.ActivePVsystem2Obj do
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

        with PVSystemVars do
            case Fnphases of
                2, 3:
                    VBase := kVPVSystemBase * InvSQRT3x1000;    // L-N Volts
            else
                VBase := kVPVSystemBase * 1000.0;   // Just use what is supplied
            end;

        VBaseMin := Vminpu * VBase;
        VBaseMax := Vmaxpu * VBase;

        Yorder := Fnconds * Fnterms;
        YprimInvalid := TRUE;
    end;
end;


//- - - - - - - - - - - - - - -MAIN EDIT FUNCTION - - - - - - - - - - - - - - -

function TPVsystem2.Edit(): Integer;

var
    i, iCase, ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin

  // continue parsing with contents of Parser
    DSS.ActivePVsystem2Obj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := DSS.ActivePVsystem2Obj;

    Result := 0;

    with DSS.ActivePVsystem2Obj do
    begin
        ParamPointer := 0;
        ParamName := Parser.NextParam;  // Parse next property off the command line
        Param := Parser.StrValue;   // Put the string value of the property value in local memory for faster access
        while Length(Param) > 0 do
        begin

            if (Length(ParamName) = 0) then
                Inc(ParamPointer)       // If it is not a named property, assume the next property
            else
                ParamPointer := CommandList.GetCommand(ParamName);  // Look up the name in the list for this class

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[PropertyIdxMap[ParamPointer]] := Param   // Update the string value of the property
            else
                DoSimpleMsg('Unknown parameter "' + ParamName + '" for PVSystem "' + Name + '"', 560);

            if (ParamPointer > 0) then
            begin
                iCase := PropertyIdxMap[ParamPointer];
                case iCASE of
                    0:
                        DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 561);
                    1:
                        NPhases := Parser.Intvalue; // num phases
                    2:
                        SetBus(1, param);
                    propKV:
                        PresentkV := Parser.DblValue;
                    propIrradiance:
                        PVSystemVars.FIrradiance := Parser.DblValue;
                    propPF:
                    begin
                        varMode := VARMODEPF;
                        PFnominal := Parser.DblValue;
                    end;
                    propMODEL:
                        VoltageModel := Parser.IntValue;
                    propYEARLY:
                        YearlyShape := Param;
                    propDAILY:
                        DailyShape := Param;
                    propDUTY:
                        DutyShape := Param;
                    propTYEARLY:
                        YearlyTShape := Param;
                    propTDAILY:
                        DailyTShape := Param;
                    propTDUTY:
                        DutyTShape := Param;
                    propCONNECTION:
                        InterpretConnection(Param);
                    propKVAR:
                    begin
                        varMode := VARMODEKVAR;
                        Presentkvar := Parser.DblValue;
                    end;
                    propPCTR:
                        pctR := Parser.DblValue;
                    propPCTX:
                        pctX := Parser.DblValue;
                    propCLASS:
                        FClass := Parser.IntValue;
                    propInvEffCurve:
                        InverterCurve := Param;
                    propTemp:
                        PVSystemVars.FTemperature := Parser.DblValue;
                    propPmpp:
                        PVSystemVars.FPmpp := Parser.DblValue;
                    propP_T_Curve:
                        Power_TempCurve := Param;
                    propCutin:
                        FpctCutIn := Parser.DblValue;
                    propCutout:
                        FpctCutOut := Parser.DblValue;
                    propVMINPU:
                        VMinPu := Parser.DblValue;
                    propVMAXPU:
                        VMaxPu := Parser.DblValue;
                    propKVA:
                        with PVSystemVars do
                        begin
                            FkVArating := Parser.DblValue;
                            if not kvarLimitSet then
                                PVSystemVars.Fkvarlimit := FkVArating;
                            if not kvarLimitSet and not kvarLimitNegSet then
                                PVSystemVars.Fkvarlimitneg := FkVArating;
                        end;
                    propUSERMODEL:
                        UserModel.Name := Parser.StrValue;  // Connect to user written models
                    propUSERDATA:
                        UserModel.Edit := Parser.StrValue;  // Send edit string to user model
                    propDEBUGTRACE:
                        DebugTrace := InterpretYesNo(Param);
                    proppctPmpp:
                        PVSystemVars.FpuPmpp := Parser.DblValue / 100.0;  // convert to pu
                    propBalanced:
                        ForceBalanced := InterpretYesNo(Param);
                    propLimited:
                        CurrentLimited := InterpretYesNo(Param);
                    propVarFollowInverter:
                        FVarFollowInverter := InterpretYesNo(Param);
                    propkvarLimit:
                    begin
                        PVSystemVars.Fkvarlimit := Abs(Parser.DblValue);
                        kvarLimitSet := TRUE;
                        if not kvarLimitNegSet then
                            PVSystemVars.Fkvarlimitneg := Abs(PVSystemVars.Fkvarlimit);

                    end;
                    propDutyStart:
                        DutyStart := Parser.DblValue;
                    propPPriority:
                        PVSystemVars.P_priority := InterpretYesNo(Param);  // set watt priority flag
                    propPFPriority:
                        PVSystemVars.PF_priority := InterpretYesNo(Param);

                    propPminNoVars:
                        FpctPminNoVars := abs(Parser.DblValue);
                    propPminkvarLimit:
                        FpctPminkvarLimit := abs(Parser.DblValue);

                    propkvarLimitneg:
                    begin
                        PVSystemVars.Fkvarlimitneg := Abs(Parser.DblValue);
                        kvarLimitNegSet := TRUE;
                    end;


                else
                  // Inherited parameters
                    ClassEdit(DSS.ActivePVsystem2Obj, ParamPointer - NumPropsThisClass)
                end;

                case iCase of
                    1:
                        SetNcondsForConnection;  // Force Reallocation of terminal info

                {Set loadshape objects;  returns nil If not valid}
                    propYEARLY:
                        YearlyShapeObj := DSS.LoadShapeClass.Find(YearlyShape);
                    propDAILY:
                        DailyShapeObj := DSS.LoadShapeClass.Find(DailyShape);
                    propDUTY:
                        DutyShapeObj := DSS.LoadShapeClass.Find(DutyShape);

                    propTYEARLY:
                        YearlyTShapeObj := DSS.TShapeClass.Find(YearlyTShape);
                    propTDAILY:
                        DailyTShapeObj := DSS.TShapeClass.Find(DailyTShape);
                    propTDUTY:
                        DutyTShapeObj := DSS.TShapeClass.Find(DutyTShape);

                    propInvEffCurve:
                        InverterCurveObj := DSS.XYCurveClass.Find(InverterCurve);
                    propP_T_Curve:
                        Power_TempCurveObj := DSS.XYCurveClass.Find(Power_TempCurve);

                    propDEBUGTRACE:
                        if DebugTrace then
                        begin   // Init trace file
                            FreeAndNil(TraceFile);
                            TraceFile := TFileStream.Create(DSS.OutputDirectory + 'STOR_' + Name + '.CSV', fmCreate);
                            FSWrite(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, PVSystemModel,  Qnominalperphase, Pnominalperphase, CurrentType');
                            for i := 1 to nphases do
                                FSWrite(Tracefile, ', |Iinj' + IntToStr(i) + '|');
                            for i := 1 to nphases do
                                FSWrite(Tracefile, ', |Iterm' + IntToStr(i) + '|');
                            for i := 1 to nphases do
                                FSWrite(Tracefile, ', |Vterm' + IntToStr(i) + '|');
                            FSWrite(TraceFile, ',Vthev, Theta');
                            FSWriteln(TraceFile);
                            FSFlush(Tracefile);
                        end
                        else
                        begin
                            FreeAndNil(TraceFile);
                        end;

                end;
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        RecalcElementData();
        YprimInvalid := TRUE;
    end;

end;

function TPVsystem2.MakeLike(const OtherPVsystemObjName: String): Integer;

// Copy over essential properties from other object

var
    OtherPVsystemObj: TPVsystem2Obj;
    i: Integer;

begin
    Result := 0;
    {See If we can find this line name in the present collection}
    OtherPVsystemObj := Find(OtherPVsystemObjName);
    if (OtherPVsystemObj <> NIL) then
    begin
        with DSS.ActivePVsystem2Obj do
        begin
            if (Fnphases <> OtherPVsystemObj.Fnphases) then
            begin
                Nphases := OtherPVsystemObj.Fnphases;
                NConds := Fnphases;  // Forces reallocation of terminal stuff
                Yorder := Fnconds * Fnterms;
                YprimInvalid := TRUE;
            end;

            PVSystemVars.kVPVSystemBase := OtherPVsystemObj.PVSystemVars.kVPVSystemBase;
            Vbase := OtherPVsystemObj.Vbase;
            Vminpu := OtherPVsystemObj.Vminpu;
            Vmaxpu := OtherPVsystemObj.Vmaxpu;
            VBaseMin := OtherPVsystemObj.VBaseMin;
            VBaseMax := OtherPVsystemObj.VBaseMax;
            kW_out := OtherPVsystemObj.kW_out;
            kvar_out := OtherPVsystemObj.kvar_out;
            Pnominalperphase := OtherPVsystemObj.Pnominalperphase;
            PFnominal := OtherPVsystemObj.PFnominal;
            Qnominalperphase := OtherPVsystemObj.Qnominalperphase;
            Connection := OtherPVsystemObj.Connection;
            YearlyShape := OtherPVsystemObj.YearlyShape;
            YearlyShapeObj := OtherPVsystemObj.YearlyShapeObj;
            DailyShape := OtherPVsystemObj.DailyShape;
            DailyShapeObj := OtherPVsystemObj.DailyShapeObj;
            DutyShape := OtherPVsystemObj.DutyShape;
            DutyShapeObj := OtherPVsystemObj.DutyShapeObj;
            DutyStart := OtherPVsystemObj.DutyStart;
            YearlyTShape := OtherPVsystemObj.YearlyTShape;
            YearlyTShapeObj := OtherPVsystemObj.YearlyTShapeObj;
            DailyTShape := OtherPVsystemObj.DailyTShape;
            DailyTShapeObj := OtherPVsystemObj.DailyTShapeObj;
            DutyTShape := OtherPVsystemObj.DutyTShape;
            DutyTShapeObj := OtherPVsystemObj.DutyTShapeObj;
            InverterCurve := OtherPVsystemObj.InverterCurve;
            InverterCurveObj := OtherPVsystemObj.InverterCurveObj;
            Power_TempCurve := OtherPVsystemObj.Power_TempCurve;
            Power_TempCurveObj := OtherPVsystemObj.Power_TempCurveObj;
            FClass := OtherPVsystemObj.FClass;
            VoltageModel := OtherPVsystemObj.VoltageModel;

            PVSystemVars.FTemperature := OtherPVsystemObj.PVSystemVars.FTemperature;
            PVSystemVars.FPmpp := OtherPVsystemObj.PVSystemVars.FPmpp;
            FpctCutin := OtherPVsystemObj.FpctCutin;
            FpctCutout := OtherPVsystemObj.FpctCutout;
            FVarFollowInverter := OtherPVsystemObj.FVarFollowInverter;
            PVSystemVars.Fkvarlimit := OtherPVsystemObj.PVSystemVars.Fkvarlimit;
            PVSystemVars.Fkvarlimitneg := OtherPVsystemObj.PVSystemVars.Fkvarlimitneg;
            FpctPminNoVars := OtherPVsystemObj.FpctPminNoVars;
            FpctPminkvarLimit := OtherPVsystemObj.FpctPminkvarLimit;
            kvarLimitSet := OtherPVsystemObj.kvarLimitSet;
            kvarLimitNegSet := OtherPVsystemObj.kvarLimitNegSet;


            PVSystemVars.FIrradiance := OtherPVsystemObj.PVSystemVars.FIrradiance;

            PVSystemVars.FkVArating := OtherPVsystemObj.PVSystemVars.FkVArating;

            pctR := OtherPVsystemObj.pctR;
            pctX := OtherPVsystemObj.pctX;

            RandomMult := OtherPVsystemObj.RandomMult;
            FVWMode := OtherPVsystemObj.FVWMode;
            FVVMode := OtherPVsystemObj.FVVMode;
            FWPMode := OtherPVsystemObj.FWPMode;
            FWVMode := OtherPVsystemObj.FWPMode;
            FDRCMode := OtherPVsystemObj.FDRCMode;
            AVRMode := OtherPVsystemObj.AVRMode;
            UserModel.Name := OtherPVsystemObj.UserModel.Name;  // Connect to user written models

            ForceBalanced := OtherPVsystemObj.ForceBalanced;
            CurrentLimited := OtherPVsystemObj.CurrentLimited;

            ClassMakeLike(OtherPVsystemObj);

            for i := 1 to ParentClass.NumProperties do
                FPropertyValue^[i] := OtherPVsystemObj.FPropertyValue^[i];

            Result := 1;
        end;
    end
    else
        DoSimpleMsg('Error in PVSystem MakeLike: "' + OtherPVsystemObjName + '" Not Found.', 562);
end;

procedure TPVsystem2.ResetRegistersAll;  // Force all EnergyMeters in the circuit to reset

var
    idx: Integer;

begin
    idx := First;
    while (idx > 0) do
    begin
        TPVsystem2Obj(GetActiveObj).ResetRegisters;
        idx := Next;
    end;
end;

procedure TPVsystem2.SampleAll();  // Force all active PV System energy meters  to take a sample

var
    i: Integer;
begin
    for i := 1 to ElementList.Count do
        with TPVsystem2Obj(ElementList.Get(i)) do
            if Enabled then
                TakeSample();
end;

constructor TPVsystem2Obj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; // + PVSystem_ELEMENT;  // In both PCelement and PVSystemelement list
    TraceFile := nil;

    Nphases := 3;
    Fnconds := 4;  // defaults to wye
    Yorder := 0;  // To trigger an initial allocation
    Nterms := 1;  // forces allocations

    YearlyShape := '';
    YearlyShapeObj := NIL;  // If YearlyShapeobj = nil Then the Irradiance alway stays nominal
    DailyShape := '';
    DailyShapeObj := NIL;  // If DaillyShapeobj = nil Then the Irradiance alway stays nominal
    DutyShape := '';
    DutyShapeObj := NIL;  // If DutyShapeobj = nil Then the Irradiance alway stays nominal
    DutyStart := 0.0;

    YearlyTShape := '';
    YearlyTShapeObj := NIL;  // If YearlyShapeobj = nil Then the Temperature always stays nominal
    DailyTShape := '';
    DailyTShapeObj := NIL;  // If DaillyShapeobj = nil Then the Temperature always stays nominal
    DutyTShape := '';
    DutyTShapeObj := NIL;  // If DutyShapeobj = nil Then the Temperature always stays nominal

    InverterCurveObj := NIL;
    Power_TempCurveObj := NIL;
    InverterCurve := '';
    Power_TempCurve := '';

    Connection := 0;    // Wye (star, L-N)
    VoltageModel := 1;  {Typical fixed kW negative load}
    FClass := 1;

    PVSystemSolutionCount := -1;  // For keep track of the present solution in Injcurrent calcs
    OpenPVSystemSolutionCount := -1;
    YPrimOpenCond := NIL;

    PVSystemVars.kVPVSystemBase := 12.47;
    VBase := 7200.0;
    Vminpu := 0.90;
    Vmaxpu := 1.10;
    VBaseMin := Vminpu * Vbase;
    VBaseMax := Vmaxpu * Vbase;
    Yorder := Fnterms * Fnconds;
    RandomMult := 1.0;

    varMode := VARMODEPF;
    FInverterON := TRUE; // start with inverterON
    FVarFollowInverter := FALSE;
    ForceBalanced := FALSE;
    CurrentLimited := FALSE;

    with PVSystemVars do
    begin
        FTemperature := 25.0;
        FIrradiance := 1.0;  // kW/sq-m
        FkVArating := 500.0;
        FPmpp := 500.0;
        FpuPmpp := 1.0;    // full on
        Vreg := 9999;
        Vavg := 9999;
        VVOperation := 9999;
        VWOperation := 9999;
        DRCOperation := 9999;
        VVDRCOperation := 9999;
        WPOperation := 9999;
        WVOperation := 9999;
        //         kW_out_desired  :=9999;
        Fkvarlimit := FkVArating;
        Fkvarlimitneg := FkVArating;
        P_Priority := FALSE;    // This is a change from older versions
        PF_Priority := FALSE;
    end;

    FpctCutIn := 20.0;
    FpctCutOut := 20.0;

    FpctPminNoVars := -1.0;
    FpctPminkvarLimit := -1.0;

    Fpf_wp_nominal := 1.0;

    {Output rating stuff}
    kW_out := 500.0;
    kvar_out := 0.0;
    PFnominal := 1.0;

    pctR := 50.0;
    ;
    pctX := 0.0;

    PublicDataStruct := @PVSystemVars;
    PublicDataSize := SizeOf(TPVSystem2Vars);

    kvarLimitSet := FALSE;
    kvarLimitNegSet := FALSE;


    UserModel := TPVsystemUserModel.Create(DSS);

    Reg_kWh := 1;
    Reg_kvarh := 2;
    Reg_MaxkW := 3;
    Reg_MaxkVA := 4;
    Reg_Hours := 5;
    Reg_Price := 6;

    DebugTrace := FALSE;
    PVsystemObjSwitchOpen := FALSE;
    Spectrum := '';  // override base class
    SpectrumObj := NIL;
    FVWMode := FALSE;
    FVVMode := FALSE;
    FWVMode := FALSE;
    FWPMode := FALSE;
    FDRCMode := FALSE;
    AVRMode := FALSE;
    InitPropertyValues(0);
    RecalcElementData();

end;

procedure TPVsystem2Obj.InitPropertyValues(ArrayOffset: Integer);

// Define default values for the properties
begin

    with PVSystemVars do
    begin
        PropertyValue[1] := '3';         //'phases';
        PropertyValue[2] := Getbus(1);   //'bus1';

        PropertyValue[propKV] := Format('%-g', [kVPVSystemBase]);
        PropertyValue[propIrradiance] := Format('%-g', [FIrradiance]);
        PropertyValue[propPF] := Format('%-g', [PFnominal]);
        PropertyValue[propMODEL] := '1';
        PropertyValue[propYEARLY] := '';
        PropertyValue[propDAILY] := '';
        PropertyValue[propDUTY] := '';
        PropertyValue[propTYEARLY] := '';
        PropertyValue[propTDAILY] := '';
        PropertyValue[propTDUTY] := '';
        PropertyValue[propCONNECTION] := 'wye';
        PropertyValue[propKVAR] := Format('%-g', [Presentkvar]);

        PropertyValue[propPCTR] := Format('%-g', [pctR]);
        PropertyValue[propPCTX] := Format('%-g', [pctX]);

        PropertyValue[propCLASS] := '1'; //'class'

        PropertyValue[propInvEffCurve] := '';
        PropertyValue[propTemp] := Format('%-g', [FTemperature]);
        PropertyValue[propPmpp] := Format('%-g', [FPmpp]);
        PropertyValue[propP_T_Curve] := '';
        PropertyValue[propCutin] := '20';
        PropertyValue[propCutout] := '20';
        PropertyValue[propVarFollowInverter] := 'NO';

        PropertyValue[propVMINPU] := '0.90';
        PropertyValue[propVMAXPU] := '1.10';
        PropertyValue[propKVA] := Format('%-g', [FkVArating]);

        PropertyValue[propUSERMODEL] := '';  // Usermodel
        PropertyValue[propUSERDATA] := '';  // Userdata
        PropertyValue[propDEBUGTRACE] := 'NO';
        PropertyValue[proppctPmpp] := '100';
        PropertyValue[propBalanced] := 'NO';
        PropertyValue[propLimited] := 'NO';
        PropertyValue[propkvarLimit] := Format('%-g', [Fkvarlimit]);
        PropertyValue[propkvarLimitneg] := Format('%-g', [Fkvarlimitneg]);
        PropertyValue[propPpriority] := 'NO';
        PropertyValue[propPFpriority] := 'NO';
    end;

    inherited  InitPropertyValues(NumPropsThisClass);

end;

function TPVsystem2Obj.GetPropertyValue(Index: Integer): String;

begin
    Result := '';
    with PVSystemVars do
        case Index of
            propKV:
                Result := Format('%.6g', [kVPVSystemBase]);
            propIrradiance:
                Result := Format('%.6g', [FIrradiance]);
            propPF:
                Result := Format('%.6g', [PFnominal]);
            propMODEL:
                Result := Format('%d', [VoltageModel]);
            propYEARLY:
                Result := YearlyShape;
            propDAILY:
                Result := DailyShape;
            propDUTY:
                Result := DutyShape;

            propTYEARLY:
                Result := YearlyTShape;
            propTDAILY:
                Result := DailyTShape;
            propTDUTY:
                Result := DutyTShape;

        {propCONNECTION :;}
            propKVAR:
                Result := Format('%.6g', [kvar_out]);
            propPCTR:
                Result := Format('%.6g', [pctR]);
            propPCTX:
                Result := Format('%.6g', [pctX]);
        {propCLASS      = 17;}
            propInvEffCurve:
                Result := InverterCurve;
            propTemp:
                Result := Format('%.6g', [FTemperature]);
            propPmpp:
                Result := Format('%.6g', [FPmpp]);
            propP_T_Curve:
                Result := Power_TempCurve;
            propCutin:
                Result := Format('%.6g', [FpctCutin]);
            propCutOut:
                Result := Format('%.6g', [FpctCutOut]);
            propVarFollowInverter:
                Result := StrYorN(FVarFollowInverter);
            propPminNoVars:
                Result := Format('%.6g', [FpctPminNoVars]);
            propPminkvarLimit:
                Result := Format('%.6g', [FpctPminkvarLimit]);

            propVMINPU:
                Result := Format('%.6g', [VMinPu]);
            propVMAXPU:
                Result := Format('%.6g', [VMaxPu]);
            propKVA:
                Result := Format('%.6g', [FkVArating]);

            propUSERMODEL:
                Result := UserModel.Name;
            propUSERDATA:
                Result := '(' + inherited GetPropertyValue(index) + ')';
            proppctPmpp:
                Result := Format('%.6g', [FpuPmpp * 100.0]);
            propBalanced:
                Result := StrYorN(ForceBalanced);
            propLimited:
                Result := StrYorN(CurrentLimited);
            propkvarLimit:
                Result := Format('%.6g', [Fkvarlimit]);
            propkvarLimitneg:
                Result := Format('%.6g', [Fkvarlimitneg]);
            propDutyStart:
                Result := Format('%.6g', [DutyStart]);


        {propDEBUGTRACE = 33;}
        else  // take the generic handler
            Result := inherited GetPropertyValue(index);
        end;
end;

destructor TPVsystem2Obj.Destroy;
begin
    YPrimOpenCond.Free;
    UserModel.Free;
    FreeAndNil(TraceFile);
    inherited Destroy;
end;

procedure TPVsystem2Obj.Randomize(Opt: Integer);
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

procedure TPVsystem2Obj.CalcDailyMult(Hr: Double);
begin
    if (DailyShapeObj <> NIL) then
    begin
        ShapeFactor := DailyShapeObj.GetMultAtHour(Hr);
    end
    else
        ShapeFactor := CDOUBLEONE;  // Default to no  variation
end;

procedure TPVsystem2Obj.CalcDailyTemperature(Hr: Double);
begin
    if (DailyTShapeObj <> NIL) then
    begin
        TShapeValue := DailyTShapeObj.GetTemperature(Hr);
    end
    else
        TShapeValue := PVSystemVars.FTemperature;
    ;  // Default to no  variation
end;

procedure TPVsystem2Obj.CalcDutyMult(Hr: Double);
begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMultAtHour(Hr + DutyStart);
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult If no duty curve specified
end;

procedure TPVsystem2Obj.CalcDutyTemperature(Hr: Double);
begin
    if DutyTShapeObj <> NIL then
    begin
        TShapeValue := DutyTShapeObj.GetTemperature(Hr);
    end
    else
        CalcDailyTemperature(Hr);  // Default to Daily Mult If no duty curve specified
end;

procedure TPVsystem2Obj.CalcYearlyMult(Hr: Double);
begin
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMultAtHour(Hr + DutyStart);
    end
    else
        CalcDailyMult(Hr);  // Defaults to Daily curve
end;

procedure TPVsystem2Obj.CalcYearlyTemperature(Hr: Double);
begin
    if YearlyTShapeObj <> NIL then
    begin
        TShapeValue := YearlyTShapeObj.GetTemperature(Hr);
    end
    else
        CalcDailyTemperature(Hr);  // Defaults to Daily curve
end;

procedure TPVsystem2Obj.RecalcElementData();
begin
    VBaseMin := VMinPu * VBase;
    VBaseMax := VMaxPu * VBase;

    varBase := 1000.0 * kvar_out / Fnphases;

    with PVSystemVars do
    begin
        // values in ohms for thevenin equivalents
        RThev := pctR * 0.01 * SQR(PresentkV) / FkVArating * 1000.0;
        XThev := pctX * 0.01 * SQR(PresentkV) / FkVArating * 1000.0;

        CutInkW := FpctCutin * FkVArating / 100.0;
        CutOutkW := FpctCutOut * FkVArating / 100.0;

        if FpctPminNoVars <= 0 then
            PminNoVars := -1
        else
            PminNoVars := FpctPminNoVars * FPmpp / 100.0;

        if FpctPminkvarLimit <= 0 then
            PminkvarLimit := -1
        else
            PminkvarLimit := FpctPminkvarLimit * FPmpp / 100.0;

    end;

    SetNominalPVSystemOuput();

    {Now check for errors.  If any of these came out nil and the string was not nil, give warning}
    if YearlyShapeObj = NIL then
        if Length(YearlyShape) > 0 then
            DoSimpleMsg('WARNING! Yearly load shape: "' + YearlyShape + '" Not Found.', 563);
    if DailyShapeObj = NIL then
        if Length(DailyShape) > 0 then
            DoSimpleMsg('WARNING! Daily load shape: "' + DailyShape + '" Not Found.', 564);
    if DutyShapeObj = NIL then
        if Length(DutyShape) > 0 then
            DoSimpleMsg('WARNING! Duty load shape: "' + DutyShape + '" Not Found.', 565);
    if YearlyTShapeObj = NIL then
        if Length(YearlyTShape) > 0 then
            DoSimpleMsg('WARNING! Yearly temperature shape: "' + YearlyTShape + '" Not Found.', 5631);
    if DailyTShapeObj = NIL then
        if Length(DailyTShape) > 0 then
            DoSimpleMsg('WARNING! Daily temperature shape: "' + DailyTShape + '" Not Found.', 5641);
    if DutyTShapeObj = NIL then
        if Length(DutyTShape) > 0 then
            DoSimpleMsg('WARNING! Duty temperature shape: "' + DutyTShape + '" Not Found.', 5651);

    if Length(Spectrum) > 0 then
    begin
        SpectrumObj := DSS.SpectrumClass.Find(Spectrum);
        if SpectrumObj = NIL then
            DoSimpleMsg('ERROR! Spectrum "' + Spectrum + '" Not Found.', 566);
    end
    else
        SpectrumObj := NIL;

    // Initialize to Zero - defaults to PQ PVSystem element
    // Solution object will reset after circuit modifications

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    {Update any user-written models}
    if Usermodel.Exists then
        UserModel.FUpdateModel;

end;

procedure TPVsystem2Obj.SetNominalPVSystemOuput();
begin

    ShapeFactor := CDOUBLEONE;  // init here; changed by curve routine
    TShapeValue := PVSystemVars.FTemperature; // init here; changed by curve routine

    // Check to make sure the PVSystem element is ON
    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        if not (IsDynamicModel or IsHarmonicModel) then     // Leave PVSystem element in whatever state it was prior to entering Dynamic mode
        begin
            // Check dispatch to see what state the PVSystem element should be in
            with Solution do
                case Mode of
                    TSolveMode.SNAPSHOT: ; {Just solve for the present kW, kvar}  // Don't check for state change
                    TSolveMode.DAILYMODE:
                    begin
                        CalcDailyMult(DynaVars.dblHour);
                        CalcDailyTemperature(DynaVars.dblHour);
                    end;
                    TSolveMode.YEARLYMODE:
                    begin
                        CalcYearlyMult(DynaVars.dblHour);
                        CalcYearlyTemperature(DynaVars.dblHour);
                    end;
             (*
                MONTECARLO1,
                MONTEFAULT,
                FAULTSTUDY,
                DYNAMICMODE:   ; // {do nothing yet}
             *)
                    TSolveMode.GENERALTIME:
                    begin
                         // This mode allows use of one class of load shape
                        case ActiveCircuit.ActiveLoadShapeClass of
                            USEDAILY:
                            begin
                                CalcDailyMult(DynaVars.dblHour);
                                CalcDailyTemperature(DynaVars.dblHour);
                            end;
                            USEYEARLY:
                            begin
                                CalcYearlyMult(DynaVars.dblHour);
                                CalcYearlyTemperature(DynaVars.dblHour);
                            end;
                            USEDUTY:
                            begin
                                CalcDutyMult(DynaVars.dblHour);
                                CalcDutyTemperature(DynaVars.dblHour);
                            end;
                        else
                            ShapeFactor := CDOUBLEONE     // default to 1 + j1 if not known
                        end;
                    end;

                // Assume Daily curve, If any, for the following
                    TSolveMode.MONTECARLO2,
                    TSolveMode.MONTECARLO3,
                    TSolveMode.LOADDURATION1,
                    TSolveMode.LOADDURATION2:
                    begin
                        CalcDailyMult(DynaVars.dblHour);
                        CalcDailyTemperature(DynaVars.dblHour);
                    end;
                    TSolveMode.PEAKDAY:
                    begin
                        CalcDailyMult(DynaVars.dblHour);
                        CalcDailyTemperature(DynaVars.dblHour);
                    end;

                    TSolveMode.DUTYCYCLE:
                    begin
                        CalcDutyMult(DynaVars.dblHour);
                        CalcDutyTemperature(DynaVars.dblHour);
                    end;
                {AUTOADDFLAG:  ; }
                end;

            ComputekWkvar;
            Pnominalperphase := 1000.0 * kW_out / Fnphases;
            Qnominalperphase := 1000.0 * kvar_out / Fnphases;

            case VoltageModel of

              //****  Fix this when user model gets connected in
                3: // YEQ := Cinv(cmplx(0.0, -StoreVARs.Xd))  ;  // Gets negated in CalcYPrim

            else

                YEQ := CDivReal(Cmplx(Pnominalperphase, -Qnominalperphase), Sqr(Vbase));   // Vbase must be L-N for 3-phase

                if (Vminpu <> 0.0) then
                    YEQ_Min := CDivReal(YEQ, SQR(Vminpu))  // at 95% voltage
                else
                    YEQ_Min := YEQ; // Always a constant Z model

                if (Vmaxpu <> 0.0) then
                    YEQ_Max := CDivReal(YEQ, SQR(Vmaxpu))   // at 105% voltage
                else
                    YEQ_Max := YEQ;

            { Like Model 7 generator, max current is based on amount of current to get out requested power at min voltage
            }
                with PVSystemvars do
                begin
                    PhaseCurrentLimit := Cdivreal(Cmplx(Pnominalperphase, Qnominalperphase), VBaseMin);
                    MaxDynPhaseCurrent := Cabs(PhaseCurrentLimit);
                end;


            end;
           { When we leave here, all the YEQ's are in L-N values}

        end;  {If  NOT (IsDynamicModel or IsHarmonicModel)}
    end;  {With ActiveCircuit}


end;


// ===========================================================================================
procedure TPVsystem2Obj.CalcYPrimMatrix(Ymatrix: TcMatrix);

var
    Y, Yij: Complex;
    i, j: Integer;
    FreqMultiplier: Double;

begin
    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

    with  ActiveCircuit.solution do
        if IsDynamicModel or IsHarmonicModel then
        begin
          {YEQ is computed from %R and %X -- inverse of Rthev + j Xthev}
            Y := YEQ;   // L-N value computed in initialization routines

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
        end

        else
        begin  //  Regular power flow PVSystem element model

          {YEQ is always expected as the equivalent line-neutral admittance}

            Y := cnegate(YEQ);   // negate for generation    YEQ is L-N quantity

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

procedure TPVsystem2Obj.ComputeInverterPower;

var
    kVA_Gen: Double;
    Qramp_limit: Double;
    TempPF: Double;
    CutOutkWAC: Double;
    // CutInkWAC: Double;

begin
    // Reset CurrentkvarLimit to kvarLimit
    CurrentkvarLimit := PVSystemVars.Fkvarlimit;
    CurrentkvarLimitNeg := PVSystemVars.Fkvarlimitneg;

    with PVSystemVars do
    begin
        EffFactor := 1.0;
        kW_Out := 0.0;

        if Assigned(InverterCurveObj) then
        begin
            CutOutkWAC := CutOutkW * InverterCurveObj.GetYValue(abs(CutOutkW) / FkVArating);
            // CutInkWAC := CutInkW * InverterCurveObj.GetYValue(abs(CutInkW) / FkVArating);
        end
        else  // Assume Ideal Inverter
        begin
            CutOutkWAC := CutOutkW;
            // CutInkWAC := CutInkW;
        end;


        // Determine state of the inverter
        if FInverterON then
        begin
            if Panelkw < CutOutkW then
            begin
                FInverterON := FALSE;
            end;
        end
        else
        begin
            if Panelkw >= CutInkW then
            begin
                FInverterON := TRUE;
            end;
        end;

        // set inverter output. Defaults to 100% of the panelkW if no efficiency curve spec'd
        if FInverterON then
        begin
            if Assigned(InverterCurveObj) then
                EffFactor := InverterCurveObj.GetYValue(PanelkW / FkVArating);  // pu eff vs pu power
            kWOut_Calc;
        end
        else
        begin
            kW_Out := 0.0;
        end;

        if (abs(kW_Out) < PminNoVars) then
        begin
            kvar_out := 0.0;  // Check minimum P for Q gen/absorption. if PminNoVars is disabled (-1), this will always be false

            CurrentkvarLimit := 0;
            CurrentkvarLimitNeg := 0.0;  // Set current limit to be used by InvControl's Check_Qlimits procedure.
        end
        else
        if varMode = VARMODEPF then
        begin
            if PFnominal = 1.0 then
                kvar_out := 0.0
            else
            begin
                kvar_out := kW_out * sqrt(1.0 / SQR(PFnominal) - 1.0) * sign(PFnominal);

                // Check limits
                if abs(kW_out) < PminkvarLimit then // straight line limit check. if PminkvarLimit is disabled (-1), this will always be false.
                begin
                    // straight line starts at max(PminNoVars, CutOutkWAC)
                    // if CutOut differs from CutIn, take cutout since it is assumed that CutOut <= CutIn always.
                    if abs(kW_out) >= max(PminNoVars, CutOutkWAC) then
                    begin
                        if (kvar_Out > 0.0) then
                        begin
                            Qramp_limit := Fkvarlimit / PminkvarLimit * abs(kW_out);   // generation limit
                            CurrentkvarLimit := Qramp_limit;  // For use in InvControl
                        end
                        else
                        if (kvar_Out < 0.0) then
                        begin
                            Qramp_limit := Fkvarlimitneg / PminkvarLimit * abs(kW_out);   // absorption limit
                            CurrentkvarLimitNeg := Qramp_limit;  // For use in InvControl
                        end;

                        if abs(kvar_Out) > Qramp_limit then
                            kvar_out := Qramp_limit * sign(kW_out) * sign(PFnominal);
                    end;
                end
                else
                if (abs(kvar_Out) > Fkvarlimit) or (abs(kvar_Out) > Fkvarlimitneg) then  // Other cases, check normal kvarLimit and kvarLimitNeg
                begin
                    if (kvar_Out > 0.0) then
                        kvar_out := Fkvarlimit * sign(kW_out) * sign(PFnominal)
                    else
                        kvar_out := Fkvarlimitneg * sign(kW_out) * sign(PFnominal);

                    if PF_Priority then // Forces constant power factor when kvar limit is exceeded and PF Priority is true.
                    begin
                        kW_out := kvar_out * sqrt(1.0 / (1.0 - Sqr(PFnominal)) - 1.0) * sign(PFnominal);
                    end;
                end;
            end;
        end
        else     // kvar is specified
        begin
            // Check limits
            if abs(kW_out) < PminkvarLimit then // straight line limit check. if PminkvarLimit is disabled (-1), this will always be false.
            begin
                  // straight line starts at max(PminNoVars, CutOutkWAC)
                  // if CutOut differs from CutIn, take cutout since it is assumed that CutOut <= CutIn always.
                if abs(kW_out) >= max(PminNoVars, CutOutkWAC) then
                begin
                    if (kvarRequested > 0.0) then
                    begin
                        Qramp_limit := Fkvarlimit / PminkvarLimit * abs(kW_out);   // generation limit
                        CurrentkvarLimit := Qramp_limit;   // For use in InvControl
                    end
                    else
                    if (kvarRequested < 0.0) then
                    begin
                        Qramp_limit := Fkvarlimitneg / PminkvarLimit * abs(kW_out);   // absorption limit
                        CurrentkvarLimitNeg := Qramp_limit;   // For use in InvControl
                    end;

                    if abs(kvarRequested) > Qramp_limit then
                        kvar_out := Qramp_limit * sign(kvarRequested)
                    else
                        kvar_out := kvarRequested;
                end;
            end
            else
            if ((kvarRequested > 0.0) and (abs(kvarRequested) >= Fkvarlimit)) or ((kvarRequested < 0.0) and (abs(kvarRequested) >= Fkvarlimitneg)) then
            begin
                if (kvarRequested > 0.0) then
                    kvar_Out := Fkvarlimit * sign(kvarRequested)
                else
                    kvar_Out := Fkvarlimitneg * sign(kvarRequested);

                if (varMode = VARMODEKVAR) and PF_Priority and FWPMode then
                begin
                    kW_out := abs(kvar_out) * sqrt(1.0 / (1.0 - Sqr(Fpf_wp_nominal)) - 1.0) * sign(kW_out);
                end
                // Forces constant power factor when kvar limit is exceeded and PF Priority is true. Temp PF is calculated based on kvarRequested
                else
                if PF_Priority and (not FVVMode or not FDRCMode or not FWVmode or not AVRMode) then
                begin
                    if abs(kvarRequested) > 0.0 then
                    begin
                        TempPF := cos(arctan(abs(kvarRequested / kW_out)));
                        kW_out := abs(kvar_out) * sqrt(1.0 / (1.0 - Sqr(TempPF)) - 1.0) * sign(kW_out);
                    end;
                end;
            end
            else
                kvar_Out := kvarRequested;
        end;

        if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then
            kvar_out := 0.0;

        // Limit kvar and kW so that kVA of inverter is not exceeded
        kVA_Gen := Sqrt(Sqr(kW_out) + Sqr(kvar_out));

        if kVA_Gen > FkVArating then
        begin
            if (varMode = VARMODEPF) and PF_Priority then
              // Operates under constant power factor when kVA rating is exceeded. PF must be specified and PFPriority must be TRUE
            begin
                kW_out := FkVArating * abs(PFnominal);
                kvar_out := FkVArating * sqrt(1 - Sqr(PFnominal)) * sign(PFnominal);
            end
            else
            if (varMode = VARMODEKVAR) and PF_Priority and FWPMode then
            begin
                kW_out := FkVArating * abs(Fpf_wp_nominal) * sign(kW_out);
                kvar_out := FkVArating * abs(sin(ArcCos(Fpf_wp_nominal))) * sign(kvarRequested)
            end
            else
            if (varMode = VARMODEKVAR) and PF_Priority and (not FVVMode or not FDRCMode or not FWVmode or not AVRMode) then
              // Operates under constant power factor (PF implicitly calculated based on kw and kvar)
            begin
                if abs(kvar_out) = Fkvarlimit then
                begin   // for handling cases when kvar limit and inverter's kVA limit are exceeded
                    kW_out := FkVArating * abs(TempPF) * sign(kW_out);
                end
                else
                begin
                    kW_out := FkVArating * abs(cos(ArcTan(kvarRequested / kW_out))) * sign(kW_out);
                end;
                kvar_out := FkVArating * abs(sin(ArcCos(kW_out / FkVArating))) * sign(kvarRequested);
            end
            else
            begin
                if P_Priority then
                begin  // back off the kvar
                    if kW_out > FkVArating then
                    begin
                        kW_out := FkVArating;
                        kvar_out := 0.0;
                    end
                    else
                        kvar_Out := Sqrt(SQR(FkVArating) - SQR(kW_Out)) * sign(kvar_Out);
                end
                else
                    kW_Out := Sqrt(SQR(FkVArating) - SQR(kvar_Out)) * sign(kW_Out);
            end;
        end;
        if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then
            kvar_out := 0.0;
    end;  {With PVSystemvars}
end;


procedure TPVsystem2Obj.ComputekWkvar;
begin
    ComputePanelPower;   // apply irradiance
    ComputeInverterPower; // apply inverter eff after checking for cutin/cutout
end;

// ===========================================================================================
procedure TPVsystem2Obj.ComputePanelPower;
begin
    with PVSystemVars do
    begin
        TempFactor := 1.0;
        if Assigned(Power_TempCurveObj) then
        begin
            TempFactor := Power_TempCurveObj.GetYValue(TshapeValue);  // pu Pmpp vs T (actual)
        end;

        PanelkW := FIrradiance * ShapeFactor.re * FPmpp * TempFactor;
    end;
end;

procedure TPVsystem2Obj.CalcYPrim();

var
    i: Integer;

begin
    // Build only shunt Yprim
    // Build a dummy Yprim Series so that CalcV Does not fail
    if YprimInvalid then
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

    SetNominalPVSystemOuput();
    CalcYPrimMatrix(YPrim_Shunt);

    // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages Doesn't fail
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));

    YPrim.CopyFrom(YPrim_Shunt);

    // Account for Open Conductors
    inherited CalcYPrim();

end;

// ===========================================================================================
procedure TPVsystem2Obj.StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);
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

// ===========================================================================================
procedure TPVsystem2Obj.WriteTraceRecord(const s: String);

var
    i: Integer;
    sout: String;
begin
    try
        if (not DSS.InshowResults) then
        begin
            WriteStr(sout, Format('%-.g, %d, %-.g, ',
                [ActiveCircuit.Solution.DynaVARs.t,
                ActiveCircuit.Solution.Iteration,
                ActiveCircuit.LoadMultiplier]),
                GetSolutionModeID(DSS), ', ',
                GetLoadModel(DSS), ', ',
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

            FSWriteln(Tracefile);
            FSFlush(TraceFile);
        end;
    except
        On E: Exception do
        begin

        end;

    end;
end;


// ===========================================================================================
procedure TPVsystem2Obj.DoConstantPQPVsystemObj();

{Compute total terminal current for Constant PQ}

var
    i: Integer;
    PhaseCurr,
    DeltaCurr,
    VLN, VLL: Complex;
    VmagLN,
    VmagLL: Double;
    V012: array[0..2] of Complex;  // Sequence voltages

begin
    //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    ZeroITerminal;

    CalcVTerminalPhase(); // get actual voltage across each phase of the load

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
            begin  {Wye}
                VLN := Vterminal^[i];
                VMagLN := Cabs(VLN);

                if CurrentLimited then
                begin
                    {Current-Limited Model}
                    PhaseCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLN));
                    if Cabs(PhaseCurr) > PVSystemVars.MaxDynPhaseCurrent then
                        PhaseCurr := Conjg(Cdiv(PhaseCurrentLimit, CDivReal(VLN, VMagLN)));
                end
                else
                begin
                   {The usual model}
                    if (VMagLN <= VBaseMin) then
                        PhaseCurr := Cmul(YEQ_Min, VLN)  // Below Vminpu use an impedance model
                    else
                    if (VMagLN > VBaseMax) then
                        PhaseCurr := Cmul(YEQ_Max, VLN)  // above Vmaxpu use an impedance model
                    else
                        PhaseCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLN));  // Between Vminpu and Vmaxpu, constant PQ
                end;

                StickCurrInTerminalArray(ITerminal, Cnegate(PhaseCurr), i);  // Put into Terminal array taking into account connection
                set_ITerminalUpdated(TRUE);
                StickCurrInTerminalArray(InjCurrent, PhaseCurr, i);  // Put into Terminal array taking into account connection
            end;

            1:
            begin  {Delta}
                VLL := Vterminal^[i];
                VMagLL := Cabs(VLL);

                if CurrentLimited then
                begin
                    {Current-Limited Model}
                    DeltaCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLL));
                    if Cabs(DeltaCurr) * SQRT3 > PVSystemVars.MaxDynPhaseCurrent then
                        DeltaCurr := Conjg(Cdiv(PhaseCurrentLimit, CDivReal(VLL, VMagLL / SQRT3)));
                end
                else
                begin
                   {The usual model}
                    case Fnphases of
                        2, 3:
                            VMagLN := VmagLL / SQRT3;
                    else
                        VMagLN := VmagLL;
                    end;

                    if VMagLN <= VBaseMin then
                        DeltaCurr := Cmul(CdivReal(YEQ_Min, 3.0), VLL)  // Below 95% use an impedance model
                    else
                    if VMagLN > VBaseMax then
                        DeltaCurr := Cmul(CdivReal(YEQ_Max, 3.0), VLL)  // above 105% use an impedance model
                    else
                        DeltaCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLL));  // Between 95% -105%, constant PQ
                end;

                StickCurrInTerminalArray(ITerminal, Cnegate(DeltaCurr), i);  // Put into Terminal array taking into account connection
                set_ITerminalUpdated(TRUE);
                StickCurrInTerminalArray(InjCurrent, DeltaCurr, i);  // Put into Terminal array taking into account connection
            end;

        end;

    end;

end;

// ===========================================================================================
procedure TPVsystem2Obj.DoConstantZPVsystemObj;
{constant Z model}
var
    i: Integer;
    Curr,
    YEQ2: Complex;
    V012: array[0..2] of Complex;  // Sequence voltages

begin

    // Assume YEQ is kept up to date
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase(); // get actual voltage across each phase of the load

    if ForceBalanced and (Fnphases = 3) then
    begin  // convert to pos-seq only
        Phase2SymComp(Vterminal, pComplexArray(@V012));
        V012[0] := CZERO; // Force zero-sequence voltage to zero
        V012[2] := CZERO; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, pComplexArray(@V012));  // Reconstitute Vterminal as balanced
    end;

    ZeroITerminal;

    if (Connection = 0) then
        YEQ2 := YEQ        // YEQ is always line to neutral
    else
        YEQ2 := CdivReal(YEQ, 3.0);          // YEQ for delta connection

    for i := 1 to Fnphases do
    begin
        Curr := Cmul(YEQ2, Vterminal^[i]);
        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;

end;


// =================================================================DOUSERMODEL==========================
procedure TPVsystem2Obj.DoUserModel;
{Compute total terminal Current from User-written model}
var
    i: Integer;

begin

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

    if UserModel.Exists then     // Check automatically selects the usermodel If true
    begin
        UserModel.FCalc(Vterminal, Iterminal);
        set_ITerminalUpdated(TRUE);
        with ActiveCircuit.Solution do
        begin          // Negate currents from user model for power flow PVSystem element model
            for i := 1 to FnConds do
                Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));
        end;
    end
    else
        DoSimpleMsg('PVSystem.' + name + ' model designated to use user-written model, but user-written model is not defined.', 567);

end;

// ===============================================================DoDynamicMode============================
procedure TPVsystem2Obj.DoDynamicMode;
{Compute Total Current and add into InjTemp}
var
    i: Integer;
    V012,
    I012: array[0..2] of Complex;
    Vthev: Complex;
    Theta: Double; // phase angle of thevinen source

    {-------------- Internal Proc -----------------------}
    procedure CalcVthev_Dyn(const V: Complex);
    {
       If the voltage magnitude drops below 15% or so, the accuracy of determining the
       phase angle gets flaky. This algorithm approximates the action of a PLL that will
       hold the last phase angle until the voltage recovers.
    }
    begin
        {Try to keep in phase with terminal voltage}

        with PVSystemVars do
        begin

            if Cabs(V) > 0.20 * Vbase then
                Theta := ThetaDyn + (Cang(V) - InitialVangle)
            else
                Theta := LastThevAngle;

            Vthev := pclx(VthevMagDyn, Theta);
            LastThevAngle := Theta;     // remember this for angle persistence
        end;
    end;

begin

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array  and computes VTerminal

    {Inj = -Itotal (in) - Yprim*Vtemp}

    case VoltageModel of

        3:
            if UserModel.Exists then       // auto selects model
            begin   {We have total currents in Iterminal}
                UserModel.FCalc(Vterminal, Iterminal);  // returns terminal currents in Iterminal
            end
            else
            begin
                DoSimpleMsg(Format('Dynamics model missing for PVSystem.%s ', [Name]), 5671);
                DSS.SolutionAbort := TRUE;
            end;
    else  {All other models -- current-limited like Generator Model 7}
      {
        This is a simple model that is basically a thevinen equivalent without inertia
      }

        case Fnphases of  {No user model, use default Thevinen equivalent for standard Generator model}
            1:
                with PVSystemVars do
                begin
                    // 1-phase generators have 2 conductors
                    // Assume inverter stays in phase with terminal voltage
                    CalcVthev_Dyn(CSub(VTerminal^[1], VTerminal^[2]));  // see internal proc above


                    ITerminal^[1] := CDiv(CSub(Csub(VTerminal^[1], Vthev), VTerminal^[2]), Zthev);

                    if CurrentLimited then
                        if Cabs(Iterminal^[1]) > MaxDynPhaseCurrent then   // Limit the current but keep phase angle
                            ITerminal^[1] := ptocomplex(topolar(MaxDynPhaseCurrent, cang(Iterminal^[1])));

                    ITerminal^[2] := Cnegate(ITerminal^[1]);
                end;

            3:
                with PVSystemVars do
                begin
                    Phase2SymComp(Vterminal, pComplexArray(@V012));  // convert Vabc to V012

                    //Begin  // simple inverter model     //HERE
                    // Positive Sequence Contribution to Iterminal
                    // Assume inverter stays in phase with pos seq voltage
                    CalcVthev_Dyn(V012[1]);

                    // Positive Sequence Contribution to Iterminal
                    I012[1] := CDiv(Csub(V012[1], Vthev), Zthev);

                    if CurrentLimited and (Cabs(I012[1]) > MaxDynPhaseCurrent) then   // Limit the pos seq current but keep phase angle
                        I012[1] := ptocomplex(topolar(MaxDynPhaseCurrent, cang(I012[1])));

                    if ForceBalanced then
                    begin
                        I012[2] := CZERO;
                    end
                    else
                        I012[2] := Cdiv(V012[2], Zthev);  // for inverter

                    //End;          //HERE

                    {Adjust for generator connection}
                    if (Connection = 1) or ForceBalanced then
                        I012[0] := CZERO
                    else
                        I012[0] := Cdiv(V012[0], Zthev);

                    SymComp2Phase(ITerminal, pComplexArray(@I012));  // Convert back to phase components

                    // Neutral current
                    if Connection = 0 then
                        ITerminal^[FnConds] := Cnegate(CmulReal(I012[0], 3.0));
                end;

        else
            DoSimpleMsg(Format('Dynamics mode is implemented only for 1- or 3-phase Generators. Generator.%s has %d phases.', [name, Fnphases]), 5671);
            DSS.SolutionAbort := TRUE;
        end;

    end;

    set_ITerminalUpdated(TRUE);

    {Add it into inj current array}
    for i := 1 to FnConds do
        Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));
end;

// ====================================================================DoHarmonicMode=======================
procedure TPVsystem2Obj.DoHarmonicMode();

{Compute Injection Current Only when in harmonics mode}

{Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built}
{Vd is the fundamental frequency voltage behind Xd" for phase 1}

var
    i: Integer;
    E: Complex;
    PVSystemHarmonic: Double;

begin
    ComputeVterminal();

    with ActiveCircuit.Solution, PVSystemVars do
    begin
        PVSystemHarmonic := Frequency / PVSystemFundamental;
        if SpectrumObj <> NIL then
            E := CmulReal(SpectrumObj.GetMult(PVSystemHarmonic), VThevHarm) // Get base harmonic magnitude
        else
            E := CZERO;

        RotatePhasorRad(E, PVSystemHarmonic, ThetaHarm);  // Time shift by fundamental frequency phase shift
        for i := 1 to Fnphases do
        begin
            cBuffer[i] := E;
            if i < Fnphases then
                RotatePhasorDeg(E, PVSystemHarmonic, -120.0);  // Assume 3-phase PVSystem element
        end;
    end;

    {Handle Wye Connection}
    if Connection = 0 then
        cbuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

    {Inj currents = Yprim (E) }
    YPrim.MVMult(InjCurrent, pComplexArray(@cBuffer));
end;

// ===========================================================================================
procedure TPVsystem2Obj.CalcVTerminalPhase();

var
    i, j: Integer;

begin

    { Establish phase voltages and stick in Vterminal}
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

    PVSystemSolutionCount := ActiveCircuit.Solution.SolutionCount;

end;

// ===========================================================================================
(*
PROCEDURE TPVsystem2Obj.CalcVTerminal;
{Put terminal voltages in an array}
Begin
   ComputeVTerminal;
   PVSystemSolutionCount := ActiveCircuit.Solution.SolutionCount;
End;
*)


// ============================================CalcPVSystemModelContribution===============================================
procedure TPVsystem2Obj.CalcPVSystemModelContribution();

// Calculates PVSystem element current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

begin
    set_ITerminalUpdated(FALSE);
    with  ActiveCircuit, ActiveCircuit.Solution do
    begin
        if IsDynamicModel then
            DoDynamicMode()
        else
        if IsHarmonicModel and (Frequency <> Fundamental) then
            DoHarmonicMode()
        else
        begin
            //  compute currents and put into InjTemp array;
            case VoltageModel of
                1:
                    DoConstantPQPVsystemObj();
                2:
                    DoConstantZPVsystemObj();
                3:
                    DoUserModel();

            else
                DoConstantPQPVsystemObj();  // for now, until we implement the other models.
            end;
        end; {ELSE}
    end; {WITH}

    {When this is Done, ITerminal is up to date}
end;

// ==========================================CalcInjCurrentArray=================================================
procedure TPVsystem2Obj.CalcInjCurrentArray();
  // Difference between currents in YPrim and total current
begin
    // Now Get Injection Currents
    if PVsystemObjSwitchOpen then
        ZeroInjCurrent
    else
        CalcPVSystemModelContribution();
end;

// =========================================GetTerminalCurrents==================================================
procedure TPVsystem2Obj.GetTerminalCurrents(Curr: pComplexArray);
// Compute total Currents

begin
    with ActiveCircuit.Solution do
    begin
        if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
        begin     // recalc the contribution
            if not PVsystemObjSwitchOpen then
                CalcPVSystemModelContribution();  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr);
    end;

    if (DebugTrace) then
        WriteTraceRecord('TotalCurrent');

end;

// ===========================================INJCURRENTS================================================
function TPVsystem2Obj.InjCurrents(): Integer;

begin
    with ActiveCircuit.Solution do
    begin
        if LoadsNeedUpdating then
            SetNominalPVSystemOuput(); // Set the nominal kW, etc for the type of solution being Done

        CalcInjCurrentArray();          // Difference between currents in YPrim and total terminal current

        if (DebugTrace) then
            WriteTraceRecord('Injection');

        // Add into System Injection Current Array

        Result := inherited InjCurrents();
    end;
end;

// ===========================================================================================
procedure TPVsystem2Obj.ResetRegisters;

var
    i: Integer;

begin
    for i := 1 to NumPVSystem2Registers do
        Registers[i] := 0.0;
    for i := 1 to NumPVSystem2Registers do
        Derivatives[i] := 0.0;
    FirstSampleAfterReset := TRUE;  // initialize for trapezoidal integration
end;

// ===========================================================================================
procedure TPVsystem2Obj.Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);

begin
    if ActiveCircuit.TrapezoidalIntegration then
    begin
        {Trapezoidal Rule Integration}
        if not FirstSampleAfterReset then
            Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
    end
    else   {Plain Euler integration}
        Registers[Reg] := Registers[Reg] + Interval * Deriv;
    Derivatives[Reg] := Deriv;
end;

// ===========================================================================================
procedure TPVsystem2Obj.TakeSample();
// Update Energy from metered zone

var
    S: Complex;
    Smag: Double;
    HourValue: Double;

begin

    // Compute energy in PVSystem element branch
    if Enabled then
    begin
        S := cmplx(Get_PresentkW, Get_Presentkvar);
        Smag := Cabs(S);
        HourValue := 1.0;


        with ActiveCircuit.Solution do
        begin
            if ActiveCircuit.PositiveSequence then
            begin
                S := CmulReal(S, 3.0);
                Smag := 3.0 * Smag;
            end;
            Integrate(Reg_kWh, S.re, IntervalHrs);   // Accumulate the power
            Integrate(Reg_kvarh, S.im, IntervalHrs);
            SetDragHandRegister(Reg_MaxkW, abs(S.re));
            SetDragHandRegister(Reg_MaxkVA, Smag);
            Integrate(Reg_Hours, HourValue, IntervalHrs);  // Accumulate Hours in operation
            Integrate(Reg_Price, S.re * ActiveCircuit.PriceSignal * 0.001, IntervalHrs);  //
            FirstSampleAfterReset := FALSE;
        end;
    end;
end;

procedure TPVsystem2Obj.UpdatePVSystem;
{Update PVSystem levels}
begin

    { Do Nothing}

end;

function TPVsystem2Obj.Get_PresentkW: Double;
begin
    Result := Pnominalperphase * 0.001 * Fnphases;
end;

function TPVsystem2Obj.Get_PresentIrradiance: Double;
begin
    Result := PVSystemVars.FIrradiance * ShapeFactor.re;
end;

function TPVsystem2Obj.Get_Presentkvar: Double;
begin
    Result := Qnominalperphase * 0.001 * Fnphases;
end;

procedure TPVsystem2Obj.DumpProperties(F: TFileStream; Complete: Boolean);

var
    i, idx: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
    begin                              // HERE
        for i := 1 to NumProperties do
        begin
            idx := PropertyIdxMap[i];
            case idx of
                propUSERDATA:
                    FSWriteln(F, '~ ' + PropertyName^[i] + '=(' + PropertyValue[idx] + ')')
            else
                FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[idx]);
            end;
        end;
    end;

    FSWriteln(F);
end;


// ============================================================InitHarmonics===============================
procedure TPVsystem2Obj.InitHarmonics();
// This routine makes a thevenin equivalent behis the reactance spec'd in %R and %X
var
    E, Va: complex;

begin
    YprimInvalid := TRUE;  // Force rebuild of YPrims
    PVSystemFundamental := ActiveCircuit.Solution.Frequency;  // Whatever the frequency is when we enter here.

    {Compute reference Thevinen voltage from phase 1 current}

    ComputeIterminal();  // Get present value of current

    with ActiveCircuit.solution do
    begin
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
    end;

    with PVSystemVars do
    begin
        YEQ := Cinv(Cmplx(RThev, XThev));           // used for current calcs  Always L-N
        E := Csub(Va, Cmul(Iterminal^[1], cmplx(Rthev, Xthev)));
        Vthevharm := Cabs(E);   // establish base mag and angle
        ThetaHarm := Cang(E);
    end;

end;


// ===============================================================InitStateVars============================
procedure TPVsystem2Obj.InitStateVars();

// for going into dynamics mode
var
//    VNeut,
    Edp: Complex;
    V12: Complex;
    i: Integer;
    V012,
    I012: array[0..2] of Complex;
    Vabc: array[1..3] of Complex;

begin
    YprimInvalid := TRUE;  // Force rebuild of YPrims

    with PVSystemVars do
    begin
        NumPhases := Fnphases;     // set Publicdata vars
        NumConductors := Fnconds;
        Conn := Connection;

        Zthev := Cmplx(RThev, XThev);
        YEQ := Cinv(Zthev);      // used for current calcs  Always L-N

        ComputeIterminal();

        with ActiveCircuit.Solution do
            case Fnphases of

                1:
                begin
                    V12 := CSub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[2]]);
                    InitialVAngle := Cang(V12);
                    Edp := Csub(V12, Cmul(ITerminal^[1], Zthev));
                    VthevmagDyn := Cabs(Edp);
                    ThetaDyn := Cang(Edp); // initial thev equivalent phase angle
                end;

                3:
                begin
                    // Calculate Edp based on Pos Seq only
                    Phase2SymComp(ITerminal, pComplexArray(@I012));
                    // Voltage behind Xdp  (transient reactance), volts

                    for i := 1 to FNphases do
                        Vabc[i] := NodeV^[NodeRef^[i]];   // Wye Voltage
                    Phase2SymComp(pComplexArray(@Vabc), pComplexArray(@V012));
                    InitialVAngle := Cang(V012[1]);
                    Edp := Csub(V012[1], Cmul(I012[1], Zthev));    // Pos sequence
                    VthevmagDyn := Cabs(Edp);
                    ThetaDyn := Cang(Edp); // initial thev equivalent phase angle
                end;
            else
                DoSimpleMsg(Format('Dynamics mode is implemented only for 1- or 3-phase Generators. PVSystem.' + name + ' has %d phases.', [Fnphases]), 5673);
                DSS.SolutionAbort := TRUE;
            end;

        LastThevAngle := ThetaDyn;
    end;
end;

// ===========================================================================================
procedure TPVsystem2Obj.IntegrateStates();

// dynamics mode integration routine

// VAR
//    TracePower:Complex;

begin
    // Compute Derivatives and Then integrate
    ComputeIterminal();

    if Usermodel.Exists then
        Usermodel.Integrate   // Checks for existence and Selects
    else
        with ActiveCircuit.Solution {, StorageVars} do
        begin
        end;
end;


// ===========================================================Get_Variable================================
function TPVsystem2Obj.Get_Variable(i: Integer): Double;
{Return variables one at a time}

var
    N, k: Integer;

begin
    Result := -9999.99;  // error return value; no state fars

    if i < 1 then
        Exit;
    // for now, report kWhstored and mode

    with PVSystemVars do
        case i of
            1:
                Result := PresentIrradiance;
            2:
                Result := PanelkW;
            3:
                Result := TempFactor;
            4:
                Result := EffFactor;
            5:
                Result := Vreg;
            6:
                Result := Vavg;
            7:
                Result := VVOperation;
            8:
                Result := VWOperation;
            9:
                Result := DRCOperation;
            10:
                Result := VVDRCOperation;
            11:
                Result := WPOperation;
            12:
                Result := WVOperation;
            13:
                Result := PanelkW * EffFactor;

        else
        begin
            if UserModel.Exists then
            begin
                N := UserModel.FNumVars;
                k := (i - NumPVSystem2Variables);
                if k <= N then
                begin
                    Result := UserModel.FGetVariable(k);
                    Exit;
                end;
            end;
        end;
        end;
end;

// ============================================================kWOut_Calc===============================
procedure TPVsystem2Obj.kWOut_Calc;

var
    Pac: Double;
    PpctLimit: Double;

begin
    with PVSystemVars do
    begin
        Pac := PanelkW * EffFactor;

        if VWmode or WVmode then
        begin
            if (Pac > kwrequested) then
                kW_Out := kwrequested
            else
                kW_Out := Pac
        end
        else
        begin
            PpctLimit := FPmpp * FpuPmpp;
            if (Pac > PpctLimit) then
                kW_Out := PpctLimit
            else
                kW_Out := Pac;
        end;
    end;
end;

// ============================================================Set_Variable===============================
procedure TPVsystem2Obj.Set_Variable(i: Integer; Value: Double);

var
    N, k: Integer;

begin
    if i < 1 then
        Exit;  // No variables to set

    with PVSystemVars do
        case i of
            1:
                FIrradiance := Value;
            2: ; // Setting this has no effect Read only
            3: ; // Setting this has no effect Read only
            4: ; // Setting this has no effect Read only
            5:
                Vreg := Value; // the InvControl or ExpControl will do this
            6:
                Vavg := Value;
            7:
                VVOperation := Value;
            8:
                VWOperation := Value;
            9:
                DRCOperation := Value;
            10:
                VVDRCOperation := Value;
            11:
                WPOperation := Value;
            12:
                WVOperation := Value;
            13: ; //ReadOnly //kW_out_desired := Value;

        else
        begin
            if UserModel.Exists then
            begin
                N := UserModel.FNumVars;
                k := (i - NumPVSystem2Variables);
                if k <= N then
                begin
                    UserModel.FSetVariable(k, Value);
                    Exit;
                end;
            end;
        end;
        end;

end;

// ===========================================================================================
procedure TPVsystem2Obj.GetAllVariables(States: pDoubleArray);

var
    i{, N}: Integer;
begin
    for i := 1 to NumPVSystem2Variables do
        States^[i] := Variable[i];

    if UserModel.Exists then
        UserModel.FGetAllVars(pDoubleArray(@States^[NumPVSystem2Variables + 1]));
end;

// ===========================================================================================
function TPVsystem2Obj.NumVariables: Integer;
begin
    Result := NumPVSystem2Variables;
    if UserModel.Exists then
        Result := Result + UserModel.FNumVars;
end;

// ===========================================================================================
function TPVsystem2Obj.VariableName(i: Integer): String;

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
            Result := 'Irradiance';
        2:
            Result := 'PanelkW';
        3:
            Result := 'P_TFactor';
        4:
            Result := 'Efficiency';
        5:
            Result := 'Vreg';
        6:
            Result := 'Vavg (DRC)';
        7:
            Result := 'volt-var';
        8:
            Result := 'volt-watt';
        9:
            Result := 'DRC';
        10:
            Result := 'VV_DRC';
        11:
            Result := 'watt-pf';
        12:
            Result := 'watt-var';
        13:
            Result := 'kW_out_desired'

    else
    begin
        if UserModel.Exists then
        begin
            pName := PAnsiChar(@Buff);
            n := UserModel.FNumVars;
            i2 := i - NumPVSystem2Variables;
            if (i2 <= n) then
            begin
                UserModel.FGetVarName(i2, pName, BuffSize);
                Result := String(pName);
                Exit;
            end;
        end;
    end;
    end;
end;

// ===========================================================================================
procedure TPVsystem2Obj.MakePosSequence();

var
    S: String;
    V: Double;

begin

    S := 'Phases=1 conn=wye';

    with PVSystemVars do
    begin
        // Make sure voltage is line-neutral
        if (Fnphases > 1) or (connection <> 0) then
            V := kVPVSystemBase / SQRT3
        else
            V := kVPVSystemBase;

        S := S + Format(' kV=%-.5g', [V]);

        if (Fnphases > 1) then
            S := S + Format(' kva=%-.5g  PF=%-.5g', [FkVArating / Fnphases, PFnominal]);

        Parser.CmdString := S;
        Edit();
    end;

    inherited;   // write out other properties
end;

// ===========================================================================================
procedure TPVsystem2Obj.Set_ConductorClosed(Index: Integer; Value: Boolean);
begin
    inherited;

    // Just turn PVSystem element on or off;
    PVsystemObjSwitchOpen := not Value;
end;

procedure TPVsystem2Obj.Set_Maxkvar(const Value: Double);
begin
    PVSystemVars.Fkvarlimit := Value;
    PropertyValue[propkvarLimit] := Format('%-g', [PVSystemVars.Fkvarlimit]);
end;

procedure TPVsystem2Obj.Set_Maxkvarneg(const Value: Double);
begin
    PVSystemVars.Fkvarlimitneg := Value;
    PropertyValue[propkvarLimitneg] := Format('%-g', [PVSystemVars.Fkvarlimitneg]);
end;

procedure TPVsystem2Obj.Set_kVARating(const Value: Double);
begin
    PVSystemVars.FkVARating := Value;
    PropertyValue[propKVA] := Format('%-g', [PVSystemVars.FkVArating]);
end;

// ===========================================================================================
procedure TPVsystem2Obj.Set_Pmpp(const Value: Double);
begin
    PVSystemVars.FPmpp := Value;
    PropertyValue[propPmpp] := Format('%-g', [PVSystemVars.FkVArating]);
end;

procedure TPVsystem2Obj.Set_PowerFactor(const Value: Double);
begin
    PFnominal := Value;
    varMode := VARMODEPF;
end;

procedure TPVsystem2Obj.Set_PresentkV(const Value: Double);
begin
    with PVSystemVars do
    begin
        kVPVSystemBase := Value;
        case FNphases of
            2, 3:
                VBase := kVPVSystemBase * InvSQRT3x1000;
        else
            VBase := kVPVSystemBase * 1000.0;
        end;
    end;
end;

procedure TPVsystem2Obj.Set_pf_wp_nominal(const Value: Double);
begin
    Fpf_wp_nominal := Value;
end;

procedure TPVsystem2Obj.SetDragHandRegister(Reg: Integer; const Value: Double);
begin
    if (Value > Registers[reg]) then
        Registers[Reg] := Value;
end;

initialization

    CDOUBLEONE := Cmplx(1.0, 1.0);

end.
