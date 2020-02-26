unit Storage2;

{
  ----------------------------------------------------------
  Copyright (c) 2009-2016, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   Change Log

    10/04/2009 Created from Generator Model


  To Do:
    Make connection to User model
    Yprim for various modes
    Define state vars and dynamics mode behavior
    Complete Harmonics mode algorithm (generator mode is implemented)
}
{
  The Storage2 element is essentially a generator that can be dispatched
  to either produce power or consume power commensurate with rating and
  amount of stored energy.

  The Storage2 element can also produce or absorb vars within the kVA rating of the inverter.
  That is, a StorageController2 object requests kvar and the Storage2 element provides them if
  it has any capacity left. The Storage2 element can produce/absorb kvar while idling.
}

//  The Storage2 element is assumed balanced over the no. of phases defined


interface

uses
    Storage2Vars,
    StoreUserModel,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    ucomplex,
    LoadShape,
    Spectrum,
    ArrayDef,
    Dynamics,
    XYCurve;

const
    NumStorage2Registers = 6;    // Number of energy meter registers
    NumStorage2Variables = 25;    // No state variables
    VARMODEPF = 0;
    VARMODEKVAR = 1;
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


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TStorage2 = class(TPCClass)
    PRIVATE

        procedure InterpretConnection(const S: String);
        procedure SetNcondsForConnection;
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const OtherStorage2ObjName: String): Integer; OVERRIDE;
    PUBLIC
        RegisterNames: array[1..NumStorage2Registers] of String;

        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

        procedure ResetRegistersAll;
        procedure SampleAll(ActorID: Integer);
        procedure UpdateAll(ActorID: Integer);

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TStorage2Obj = class(TPCElement)
    PRIVATE
        Yeq: Complex;   // at nominal
        Yeq95: Complex;   // at 95%
        Yeq105: Complex;   // at 105%
        PIdling: Double;
        YeqDischarge: Complex;   // equiv at rated power of Storage2 element only
        PhaseCurrentLimit: Complex;
        MaxDynPhaseCurrent: Double;

        DebugTrace: Boolean;
        FState: Integer;
        FStateChanged: Boolean;
        FirstSampleAfterReset: Boolean;
        Storage2SolutionCount: Integer;
        Storage2Fundamental: Double;  {Thevenin equivalent voltage mag and angle reference for Harmonic model}
        Storage2ObjSwitchOpen: Boolean;


        ForceBalanced: Boolean;
        CurrentLimited: Boolean;

//        LoadSpecType     :Integer;    // 0=kW, PF;  1= kw, kvar;
        kvar_out: Double;
        kW_out: Double;
        FkvarRequested: Double;
        FkWRequested: Double;
        FvarMode: Integer;
        FDCkW: Double;
        Fpf_wp_nominal: Double;

        // Variables for Inverter functionalities
        FpctCutIn: Double;
        FpctCutOut: Double;
        FVarFollowInverter: Boolean;
        CutInkW: Double;
        CutOutkW: Double;

        FCutOutkWAC: Double;  // CutInkW  reflected to the AC side of the inverter
        FCutInkWAC: Double;   // CutOutkW reflected to the AC side of the inverter

        FStateDesired: Integer;  // Stores desired state (before any change due to kWh limits or %CutIn/%CutOut

        FInverterON: Boolean;
        FpctPminNoVars: Double;
        FpctPminkvarLimit: Double;
        PminNoVars: Double;
        PminkvarLimit: Double;
        kVA_exceeded: Boolean;


        kvarLimitSet: Boolean;
        kvarLimitNegSet: Boolean;
        kVASet: Boolean;

        pctR: Double;
        pctX: Double;

        OpenStorage2SolutionCount: Integer;
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

        Tracefile: TextFile;
        IsUserModel: Boolean;
        UserModel: TStoreUserModel;   {User-Written Models}
        DynaModel: TStoreDynaModel;

//        VBase           :Double;  // Base volts suitable for computing currents  made public
        VBase105: Double;
        VBase95: Double;
        Vmaxpu: Double;
        Vminpu: Double;
        YPrimOpenCond: TCmatrix;

        // Variables for InvControl's Volt-Watt function
        FVWMode: Boolean; //boolean indicating if under volt-watt control mode from InvControl (not ExpControl)
        FVVMode: Boolean; //boolean indicating if under volt-var mode from InvControl
        FDRCMode: Boolean; //boolean indicating if under DRC mode from InvControl
        FWPMode: Boolean; //boolean indicating if under watt-pf mode from InvControl
        FWVMode: Boolean; //boolean indicating if under watt-var mode from InvControl


        procedure CalcDailyMult(Hr: Double; ActorID: Integer);
        procedure CalcDutyMult(Hr: Double; ActorID: Integer);
        procedure CalcYearlyMult(Hr: Double; ActorID: Integer);

        procedure ComputePresentkW; // Included
        procedure ComputeInverterPower; // Included

        procedure ComputekWkvar;        // Included
        procedure ComputeDCkW; // For Storage2 Update
        procedure CalcStorage2ModelContribution(ActorID: Integer);
        procedure CalcInjCurrentArray(ActorID: Integer);
        (*PROCEDURE CalcVterminal;*)
        procedure CalcVTerminalPhase(ActorID: Integer);

        procedure CalcYPrimMatrix(Ymatrix: TcMatrix; ActorID: Integer);

        procedure DoConstantPQStorage2Obj(ActorID: Integer);
        procedure DoConstantZStorage2Obj(ActorID: Integer);
        procedure DoDynamicMode(ActorID: Integer);
        procedure DoHarmonicMode(ActorID: Integer);
        procedure DoUserModel(ActorID: Integer);
        procedure DoDynaModel(ActorID: Integer);

        procedure Integrate(Reg: Integer; const Deriv: Double; const Interval: Double; ActorID: Integer);
        procedure SetDragHandRegister(Reg: Integer; const Value: Double);
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);

        procedure WriteTraceRecord(const s: String; ActorID: Integer);

        procedure CheckStateTriggerLevel(Level: Double; ActorID: Integer);
        procedure UpdateStorage2(ActorID: Integer);    // Update Storage2 elements based on present kW and IntervalHrs variable
        function NormalizeToTOD(h: Integer; sec: Double): Double;

        function InterpretState(const S: String): Integer;
//        FUNCTION  StateToStr:String;
        function DecodeState: String;

        function Get_PresentkW: Double;
        function Get_Presentkvar: Double;
        function Get_PresentkV: Double;
        function Get_kvarRequested: Double;
        function Get_kWRequested: Double;

        procedure Set_kW(const Value: Double);
        function Get_kW: Double;
        procedure Set_PresentkV(const Value: Double);

        procedure Set_PowerFactor(const Value: Double);
        procedure Set_kWRequested(const Value: Double);
        procedure Set_kvarRequested(const Value: Double);
        procedure Set_pf_wp_nominal(const Value: Double);


        procedure Set_Storage2State(const Value: Integer);
        procedure Set_pctkWOut(const Value: Double);
        procedure Set_pctkWIn(const Value: Double);

        function Get_DCkW: Double;
        function Get_kWTotalLosses: Double;
        function Get_InverterLosses: Double;
        function Get_kWIdlingLosses: Double;
        function Get_kWChDchLosses: Double;
        procedure Update_EfficiencyFactor;

        procedure Set_StateDesired(i: Integer);
        function Get_kWDesired: Double;

        // Procedures and functions for inverter functionalities
        procedure Set_kVARating(const Value: Double);
        procedure Set_pctkWrated(const Value: Double);
        function Get_Varmode: Integer;

        procedure Set_Varmode(const Value: Integer);
        function Get_VWmode: Boolean;

        procedure Set_VVmode(const Value: Boolean);
        function Get_VVmode: Boolean;

        procedure Set_DRCmode(const Value: Boolean);
        function Get_DRCmode: Boolean;

        procedure Set_VWmode(const Value: Boolean);
        procedure kWOut_Calc;

        function Get_WPmode: Boolean;
        procedure Set_WPmode(const Value: Boolean);

        function Get_WVmode: Boolean;
        procedure Set_WVmode(const Value: Boolean);

        function Get_CutOutkWAC: Double;
        function Get_CutInkWAC: Double;

    PROTECTED
        procedure Set_ConductorClosed(Index: Integer; ActorID: Integer; Value: Boolean); OVERRIDE;
        procedure GetTerminalCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;

    PUBLIC

        Storage2Vars: TStorage2Vars;

        VBase: Double;  // Base volts suitable for computing currents

        Connection: Integer;  {0 = line-neutral; 1=Delta}
        DailyShape: String;  // Daily (24 HR) Storage2 element shape
        DailyShapeObj: TLoadShapeObj;  // Daily Storage2 element Shape for this load
        DutyShape: String;  // Duty cycle load shape for changes typically less than one hour
        DutyShapeObj: TLoadShapeObj;  // Shape for this Storage2 element
        YearlyShape: String;  // ='fixed' means no variation  on all the time
        YearlyShapeObj: TLoadShapeObj;  // Shape for this Storage2 element

        FpctkWout: Double;   // percent of kW rated output currently dispatched
        FpctkWin: Double;

        pctReserve: Double;
        DispatchMode: Integer;
        pctIdlekW: Double;

        kWOutIdling: Double;

        pctIdlekvar: Double;
        pctChargeEff: Double;
        pctDischargeEff: Double;
        DischargeTrigger: Double;
        ChargeTrigger: Double;
        ChargeTime: Double;
        kWhBeforeUpdate: Double;
        CurrentkvarLimit: Double;
        CurrentkvarLimitNeg: Double;

        // Inverter efficiency curve
        InverterCurve: String;
        InverterCurveObj: TXYCurveObj;

        FVWStateRequested: Boolean;   // TEST Flag indicating if VW function has requested a specific state in last control iteration

        Storage2Class: Integer;
        VoltageModel: Integer;   // Variation with voltage
        PFNominal: Double;

        Registers, Derivatives: array[1..NumStorage2Registers] of Double;

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;

        function InjCurrents(ActorID: Integer): Integer; OVERRIDE;
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;
        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        function Get_InverterON: Boolean;
        procedure Set_InverterON(const Value: Boolean);
        function Get_VarFollowInverter: Boolean;
        procedure Set_VarFollowInverter(const Value: Boolean);

        procedure Set_Maxkvar(const Value: Double);
        procedure Set_Maxkvarneg(const Value: Double);

        procedure SetNominalStorage2Output(ActorID: Integer);
        procedure Randomize(Opt: Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform

        procedure ResetRegisters;
        procedure TakeSample(ActorID: Integer);

        // Support for Dynamics Mode
        procedure InitStateVars(ActorID: Integer); OVERRIDE;
        procedure IntegrateStates(ActorID: Integer); OVERRIDE;

        // Support for Harmonics Mode
        procedure InitHarmonics(ActorID: Integer); OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;


        property kW: Double READ Get_kW WRITE Set_kW;
        property kWDesired: Double READ Get_kWDesired;
        property StateDesired: Integer WRITE Set_StateDesired;
        property kWRequested: Double READ Get_kWRequested WRITE Set_kWRequested;
        property kvarRequested: Double READ Get_kvarRequested WRITE Set_kvarRequested;

        property PresentkW: Double READ Get_PresentkW;             // Present kW   at inverter output
        property Presentkvar: Double READ Get_Presentkvar;           // Present kvar at inverter output

        property PresentkV: Double READ Get_PresentkV WRITE Set_PresentkV;
        property PowerFactor: Double READ PFNominal WRITE Set_PowerFactor;
        property kVARating: Double READ Storage2Vars.FkVARating WRITE Set_kVARating;
        property pctkWrated: Double READ Storage2Vars.FpctkWrated WRITE Set_pctkWrated;
        property Varmode: Integer READ Get_Varmode WRITE Set_Varmode;  // 0=constant PF; 1=kvar specified
        property VWmode: Boolean READ Get_VWmode WRITE Set_VWmode;
        property VVmode: Boolean READ Get_VVmode WRITE Set_VVmode;
        property WPmode: Boolean READ Get_WPmode WRITE Set_WPmode;
        property WVmode: Boolean READ Get_WVmode WRITE Set_WVmode;
        property DRCmode: Boolean READ Get_DRCmode WRITE Set_DRCmode;
        property InverterON: Boolean READ Get_InverterON WRITE Set_InverterON;
        property CutOutkWAC: Double READ Get_CutOutkWAC;
        property CutInkWAC: Double READ Get_CutInkWAC;

        property VarFollowInverter: Boolean READ Get_VarFollowInverter WRITE Set_VarFollowInverter;
        property kvarLimit: Double READ Storage2Vars.Fkvarlimit WRITE Set_Maxkvar;
        property kvarLimitneg: Double READ Storage2Vars.Fkvarlimitneg WRITE Set_Maxkvarneg;

        property Storage2State: Integer READ FState WRITE Set_Storage2State;
        property PctkWOut: Double READ FpctkWOut WRITE Set_pctkWOut;
        property PctkWIn: Double READ FpctkWIn WRITE Set_pctkWIn;

        property kWTotalLosses: Double READ Get_kWTotalLosses;
        property kWIdlingLosses: Double READ Get_kWIdlingLosses;
        property kWInverterLosses: Double READ Get_InverterLosses;
        property kWChDchLosses: Double READ Get_kWChDchLosses;
        property DCkW: Double READ Get_DCkW;

        property MinModelVoltagePU: Double READ VminPu;
        property pf_wp_nominal: Double WRITE Set_pf_wp_nominal;
    end;

var
    ActiveStorage2Obj: TStorage2Obj;

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
    Utilities;

const

{  = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   To add a property,
    1) add a property constant to this list
    2) add a handler to the CASE statement in the Edit FUNCTION
    3) add a statement(s) to InitPropertyValues FUNCTION to initialize the string value
    4) add any special handlers to DumpProperties and GetPropertyValue, If needed
 = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =}

    propKV = 3;
    propKW = 4;
    propPF = 5;
    propMODEL = 6;
    propYEARLY = 7;
    propDAILY = 8;
    propDUTY = 9;
    propDISPMODE = 10;
    propCONNECTION = 11;
    propKVAR = 12;
    propPCTR = 13;
    propPCTX = 14;
    propIDLEKW = 15;
    propCLASS = 16;
    propDISPOUTTRIG = 17;
    propDISPINTRIG = 18;
    propCHARGEEFF = 19;
    propDISCHARGEEFF = 20;
    propPCTKWOUT = 21;
    propVMINPU = 22;
    propVMAXPU = 23;
    propSTATE = 24;
    propKVA = 25;
    propKWRATED = 26;
    propKWHRATED = 27;
    propKWHSTORED = 28;
    propPCTRESERVE = 29;
    propUSERMODEL = 30;
    propUSERDATA = 31;
    propDEBUGTRACE = 32;
    propPCTKWIN = 33;
    propPCTSTORED = 34;
    propCHARGETIME = 35;
    propDynaDLL = 36;
    propDynaData = 37;
    propBalanced = 38;
    propLimited = 39;

    propInvEffCurve = 40;
    propCutin = 41;
    propCutout = 42;
    proppctkWrated = 43;
    propVarFollowInverter = 44;
    propkvarLimit = 45;
    propPpriority = 46;
    propPFPriority = 47;
    propPminNoVars = 48;
    propPminkvarLimit = 49;

    propkvarLimitneg = 50;

    NumPropsThisClass = 50; // Make this agree with the last property constant

var

    cBuffer: array[1..24] of Complex;  // Temp buffer for calcs  24-phase Storage2 element?
    CDOUBLEONE: Complex;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TStorage2.Create;  // Creates superstructure for all Storage2 elements
begin
    inherited Create;
    Class_Name := 'Storage2';
    DSSClassType := DSSClassType + Storage2_ELEMENT;  // In both PCelement and Storage2 element list

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

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TStorage2.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TStorage2.DefineProperties;
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
        'Number of Phases, this Storage2 element.  Power is evenly divided among phases.');
    AddProperty('bus1', 2,
        'Bus to which the Storage2 element is connected.  May include specific node specification.');
    AddProperty('kv', propKV,
        'Nominal rated (1.0 per unit) voltage, kV, for Storage2 element. For 2- and 3-phase Storage2 elements, specify phase-phase kV. ' +
        'Otherwise, specify actual kV across each branch of the Storage2 element. ' + CRLF + CRLF +
        'If wye (star), specify phase-neutral kV. ' + CRLF + CRLF +
        'If delta or phase-phase connected, specify phase-phase kV.');  // line-neutral voltage//  base voltage
    AddProperty('conn', propCONNECTION,
        '={wye|LN|delta|LL}.  Default is wye.');
    AddProperty('kW', propKW,
        'Get/set the requested kW value. Final kW is subjected to the inverter ratings. A positive value denotes power coming OUT of the element, ' +
        'which is the opposite of a Load element. A negative value indicates the Storage2 element is in Charging state. ' +
        'This value is modified internally depending on the dispatch mode.');
    AddProperty('kvar', propKVAR,
        'Get/set the requested kvar value. Final kvar is subjected to the inverter ratings. Sets inverter to operate in constant kvar mode.');
    AddProperty('pf', propPF,
        'Get/set the requested PF value. Final PF is subjected to the inverter ratings. Sets inverter to operate in constant PF mode. Nominally, ' +
        'the power factor for discharging (acting as a generator). Default is 1.0. ' + CRLF + CRLF +
        'Enter negative for leading power factor ' +
        '(when kW and kvar have opposite signs.)' + CRLF + CRLF +
        'A positive power factor signifies kw and kvar at the same direction.');
    AddProperty('kVA', propKVA,
        'Indicates the inverter nameplate capability (in kVA). ' +
        'Used as the base for Dynamics mode and Harmonics mode values.');
    AddProperty('%Cutin', propCutin,
        'Cut-in power as a percentage of inverter kVA rating. It is the minimum DC power necessary to turn the inverter ON when it is OFF. ' +
        'Must be greater than or equal to %CutOut. Defaults to 2 for PVSystems and 0 for Storage2 elements which means that the inverter state ' +
        'will be always ON for this element.');
    AddProperty('%Cutout', propCutout,
        'Cut-out power as a percentage of inverter kVA rating. It is the minimum DC power necessary to keep the inverter ON. ' +
        'Must be less than or equal to %CutIn. Defaults to 0, which means that, once ON, the inverter state ' +
        'will be always ON for this element.');

    AddProperty('EffCurve', propInvEffCurve,
        'An XYCurve object, previously defined, that describes the PER UNIT efficiency vs PER UNIT of rated kVA for the inverter. ' +
        'Power at the AC side of the inverter is discounted by the multiplier obtained from this curve.');

    AddProperty('VarFollowInverter', propVarFollowInverter,
        'Boolean variable (Yes|No) or (True|False). Defaults to False, which indicates that the reactive power generation/absorption does not respect the inverter status.' +
        'When set to True, the reactive power generation/absorption will cease when the inverter status is off, due to DC kW dropping below %CutOut.  The reactive power ' +
        'generation/absorption will begin again when the DC kW is above %CutIn.  When set to False, the Storage2 will generate/absorb reactive power regardless of the status of the inverter.');
    AddProperty('kvarMax', propkvarLimit,
        'Indicates the maximum reactive power GENERATION (un-signed numerical variable in kvar) for the inverter. Defaults to kVA rating of the inverter.');

    AddProperty('kvarMaxAbs', propkvarLimitneg,
        'Indicates the maximum reactive power ABSORPTION (un-signed numerical variable in kvar) for the inverter. Defaults to kvarMax.');

    AddProperty('WattPriority', propPPriority,
        '{Yes/No*/True/False} Set inverter to watt priority instead of the default var priority.');

    AddProperty('PFPriority', propPFPriority,
        'If set to true, priority is given to power factor and WattPriority is neglected. It works only if operating in either constant PF ' +
        'or constant kvar modes. Defaults to False.');

    AddProperty('%PminNoVars', propPminNoVars,
        'Minimum active power as percentage of kWrated under which there is no vars production/absorption. Defaults to 0 (disabled).');

    AddProperty('%PminkvarMax', propPminkvarLimit,
        'Minimum active power as percentage of kWrated that allows the inverter to produce/absorb reactive power up to its maximum ' +
        'reactive power, which can be either kvarMax or kvarMaxAbs, depending on the current operation quadrant. Defaults to 0 (disabled).');

    AddProperty('kWrated', propKWRATED,
        'kW rating of power output. Base for Loadshapes when DispMode=Follow. Sets kVA property if it has not been specified yet. ' +
        'Defaults to 25.');
    AddProperty('%kWrated', proppctkWrated,
        'Upper limit on active power as a percentage of kWrated. Defaults to 100 (disabled).');

    AddProperty('kWhrated', propKWHRATED,
        'Rated Storage2 capacity in kWh. Default is 50.');
    AddProperty('kWhstored', propKWHSTORED,
        'Present amount of energy stored, kWh. Default is same as kWhrated.');
    AddProperty('%stored', propPCTSTORED,
        'Present amount of energy stored, % of rated kWh. Default is 100.');
    AddProperty('%reserve', propPCTRESERVE,
        'Percentage of rated kWh Storage2 capacity to be held in reserve for normal operation. Default = 20. ' + CRLF +
        'This is treated as the minimum energy discharge level unless there is an emergency. For emergency operation ' +
        'set this property lower. Cannot be less than zero.');
    AddProperty('State', propSTATE,
        '{IDLING | CHARGING | DISCHARGING}  Get/Set present operational state. In DISCHARGING mode, the Storage2 element ' +
        'acts as a generator and the kW property is positive. The element continues discharging at the scheduled output power level ' +
        'until the Storage2 reaches the reserve value. Then the state reverts to IDLING. ' +
        'In the CHARGING state, the Storage2 element behaves like a Load and the kW property is negative. ' +
        'The element continues to charge until the max Storage2 kWh is reached and then switches to IDLING state. ' +
        'In IDLING state, the element draws the idling losses plus the associated inverter losses.');
    AddProperty('%Discharge', propPCTKWOUT,
        'Discharge rate (output power) in percentage of rated kW. Default = 100.');
    AddProperty('%Charge', propPCTKWIN,
        'Charging rate (input power) in percentage of rated kW. Default = 100.');
    AddProperty('%EffCharge', propCHARGEEFF,
        'Percentage efficiency for CHARGING the Storage2 element. Default = 90.');
    AddProperty('%EffDischarge', propDISCHARGEEFF,
        'Percentage efficiency for DISCHARGING the Storage2 element. Default = 90.');
    AddProperty('%IdlingkW', propIDLEKW,
        'Percentage of rated kW consumed by idling losses. Default = 1.');
    AddProperty('%R', propPCTR,
        'Equivalent percentage internal resistance, ohms. Default is 0. Placed in series with internal voltage source' +
        ' for harmonics and dynamics modes. Use a combination of %IdlingkW, %EffCharge and %EffDischarge to account for ' +
        'losses in power flow modes.');
    AddProperty('%X', propPCTX,
        'Equivalent percentage internal reactance, ohms. Default is 50%. Placed in series with internal voltage source' +
        ' for harmonics and dynamics modes. (Limits fault current to 2 pu.');
    AddProperty('model', propMODEL,
        'Integer code (default=1) for the model to be used for power output variation with voltage. ' +
        'Valid values are:' + CRLF + CRLF +
        '1:Storage2 element injects/absorbs a CONSTANT power.' + CRLF +
        '2:Storage2 element is modeled as a CONSTANT IMPEDANCE.' + CRLF +
        '3:Compute load injection from User-written Model.');

    AddProperty('Vminpu', propVMINPU,
        'Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. ' +
        'Below this value, the load model reverts to a constant impedance model.');
    AddProperty('Vmaxpu', propVMAXPU,
        'Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. ' +
        'Above this value, the load model reverts to a constant impedance model.');
    AddProperty('Balanced', propBalanced, '{Yes | No*} Default is No. Force balanced current only for 3-phase Storage2. Forces zero- and negative-sequence to zero. ');
    AddProperty('LimitCurrent', propLimited, 'Limits current magnitude to Vminpu value for both 1-phase and 3-phase Storage2 similar to Generator Model 7. For 3-phase, ' +
        'limits the positive-sequence current but not the negative-sequence.');
    AddProperty('yearly', propYEARLY,
        'Dispatch shape to use for yearly simulations.  Must be previously defined ' +
        'as a Loadshape object. If this is not specified, the Daily dispatch shape, if any, is repeated ' +
        'during Yearly solution modes. In the default dispatch mode, ' +
        'the Storage2 element uses this loadshape to trigger State changes.');
    AddProperty('daily', propDAILY,
        'Dispatch shape to use for daily simulations.  Must be previously defined ' +
        'as a Loadshape object of 24 hrs, typically.  In the default dispatch mode, ' +
        'the Storage2 element uses this loadshape to trigger State changes.'); // daily dispatch (hourly)
    AddProperty('duty', propDUTY,
        'Load shape to use for duty cycle dispatch simulations such as for solar ramp rate studies. ' +
        'Must be previously defined as a Loadshape object. ' + CRLF + CRLF +
        'Typically would have time intervals of 1-5 seconds. ' + CRLF + CRLF +
        'Designate the number of points to solve using the Set Number=xxxx command. ' +
        'If there are fewer points in the actual shape, the shape is assumed to repeat.');  // as for wind generation
    AddProperty('DispMode', propDISPMODE,
        '{DEFAULT | FOLLOW | EXTERNAL | LOADLEVEL | PRICE } Default = "DEFAULT". Dispatch mode. ' + CRLF + CRLF +
        'In DEFAULT mode, Storage2 element state is triggered to discharge or charge at the specified rate by the ' +
        'loadshape curve corresponding to the solution mode. ' + CRLF + CRLF +
        'In FOLLOW mode the kW output of the Storage2 element follows the active loadshape multiplier ' +
        'until Storage2 is either exhausted or full. ' +
        'The element discharges for positive values and charges for negative values.  The loadshape is based on rated kW. ' + CRLF + CRLF +
        'In EXTERNAL mode, Storage2 element state is controlled by an external Storagecontroller2. ' +
        'This mode is automatically set if this Storage2 element is included in the element list of a Storage2Controller element. ' + CRLF + CRLF +
        'For the other two dispatch modes, the Storage2 element state is controlled by either the global default Loadlevel value or the price level. ');
    AddProperty('DischargeTrigger', propDISPOUTTRIG,
        'Dispatch trigger value for discharging the Storage2. ' + CRLF +
        'If = 0.0 the Storage2 element state is changed by the State command or by a StorageController2 object. ' + CRLF +
        'If <> 0  the Storage2 element state is set to DISCHARGING when this trigger level is EXCEEDED by either the specified ' +
        'Loadshape curve value or the price signal or global Loadlevel value, depending on dispatch mode. See State property.');
    AddProperty('ChargeTrigger', propDISPINTRIG,
        'Dispatch trigger value for charging the Storage2. ' + CRLF + CRLF +
        'If = 0.0 the Storage2 element state is changed by the State command or StorageController2 object.  ' + CRLF + CRLF +
        'If <> 0  the Storage2 element state is set to CHARGING when this trigger level is GREATER than either the specified ' +
        'Loadshape curve value or the price signal or global Loadlevel value, depending on dispatch mode. See State property.');
    AddProperty('TimeChargeTrig', propCHARGETIME,
        'Time of day in fractional hours (0230 = 2.5) at which Storage2 element will automatically go into charge state. ' +
        'Default is 2.0.  Enter a negative time value to disable this feature.');
    AddProperty('class', propCLASS,
        'An arbitrary integer number representing the class of Storage2 element so that Storage2 values may ' +
        'be segregated by class.'); // integer
    AddProperty('DynaDLL', propDynaDLL,
        'Name of DLL containing user-written dynamics model, which computes the terminal currents for Dynamics-mode simulations, ' +
        'overriding the default model.  Set to "none" to negate previous setting. ' +
        'This DLL has a simpler interface than the UserModel DLL and is only used for Dynamics mode.');
    AddProperty('DynaData', propDYNADATA,
        'String (in quotes or parentheses if necessary) that gets passed to the user-written dynamics model Edit function for defining the data required for that model.');
    AddProperty('UserModel', propUSERMODEL,
        'Name of DLL containing user-written model, which computes the terminal currents for both power flow and dynamics, ' +
        'overriding the default model.  Set to "none" to negate previous setting.');
    AddProperty('UserData', propUSERDATA,
        'String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model.');
    AddProperty('debugtrace', propDEBUGTRACE,
        '{Yes | No }  Default is no.  Turn this on to capture the progress of the Storage2 model ' +
        'for each iteration.  Creates a separate file for each Storage2 element named "Storage2_name.CSV".');

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override default help string
    PropertyHelp[NumPropsThisClass + 1] := 'Name of harmonic voltage or current spectrum for this Storage2 element. ' +
        'Current injection is assumed for inverter. ' +
        'Default value is "default", which is defined when the DSS starts.';

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TStorage2.NewObject(const ObjName: String): Integer;
begin
    // Make a new Storage2 element and add it to Storage2 class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TStorage2Obj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TStorage2.SetNcondsForConnection;

begin
    with ActiveStorage2Obj do
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
procedure TStorage2.UpdateAll(ActorID: Integer);
var
    i: Integer;
begin
    for i := 1 to ElementList.ListSize do
        with TStorage2Obj(ElementList.Get(i)) do
            if Enabled then
                UpdateStorage2(ActorID);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TStorage2.InterpretConnection(const S: String);

// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN
var
    TestS: String;

begin
    with ActiveStorage2Obj do
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

        case Fnphases of
            2, 3:
                VBase := Storage2Vars.kVStorage2Base * InvSQRT3x1000;    // L-N Volts
        else
            VBase := Storage2Vars.kVStorage2Base * 1000.0;   // Just use what is supplied
        end;

        VBase95 := Vminpu * VBase;
        VBase105 := Vmaxpu * VBase;

        Yorder := Fnconds * Fnterms;
        YprimInvalid[ActiveActor] := TRUE;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function InterpretDispMode(const S: String): Integer;
begin
    case lowercase(S)[1] of
        'e':
            Result := STORE_EXTERNALMODE;
        'f':
            Result := STORE_FOLLOW;
        'l':
            Result := STORE_LOADMODE;
        'p':
            Result := STORE_PRICEMODE;
    else
        Result := STORE_DEFAULT;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function ReturnDispMode(const imode: Integer): String;
begin
    case imode of
        STORE_EXTERNALMODE:
            Result := 'External';
        STORE_FOLLOW:
            Result := 'Follow';
        STORE_LOADMODE:
            Result := 'Loadshape';
        STORE_PRICEMODE:
            Result := 'Price';
    else
        Result := 'default';
    end;
end;


//- - - - - - - - - - - - - - -MAIN EDIT FUNCTION - - - - - - - - - - - - - - -
function TStorage2.Edit(ActorID: Integer): Integer;

var
    i, iCase,
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin

  // continue parsing with contents of Parser
    ActiveStorage2Obj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveStorage2Obj;

    Result := 0;

    with ActiveStorage2Obj do
    begin

        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;  // Parse next property off the command line
        Param := Parser[ActorID].StrValue;   // Put the string value of the property value in local memory for faster access
        while Length(Param) > 0 do
        begin

            if (Length(ParamName) = 0) then
                Inc(ParamPointer)       // If it is not a named property, assume the next property
            else
                ParamPointer := CommandList.GetCommand(ParamName);  // Look up the name in the list for this class

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[PropertyIdxMap[ParamPointer]] := Param   // Update the string value of the property
            else
                DoSimpleMsg('Unknown parameter "' + ParamName + '" for Storage2 "' + Name + '"', 560);

            if ParamPointer > 0 then
            begin
                iCase := PropertyIdxMap[ParamPointer];
                case iCASE of
                    0:
                        DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 561);
                    1:
                        NPhases := Parser[ActorID].Intvalue;
                    2:
                        SetBus(1, param);
                    propKV:
                        PresentkV := Parser[ActorID].DblValue;
                    propKW:
                        kW := Parser[ActorID].DblValue;
                    propPF:
                    begin
                        varMode := VARMODEPF;
                        PFnominal := Parser[ActorID].DblValue;
                    end;
                    propMODEL:
                        VoltageModel := Parser[ActorID].IntValue;
                    propYEARLY:
                        YearlyShape := Param;
                    propDAILY:
                        DailyShape := Param;
                    propDUTY:
                        DutyShape := Param;
                    propDISPMODE:
                        DispatchMode := InterpretDispMode(Param);
                    propCONNECTION:
                        InterpretConnection(Param);
                    propKVAR:
                    begin
                        varMode := VARMODEKVAR;
                        kvarRequested := Parser[ActorID].DblValue;
                    end;
                    propPCTR:
                        pctR := Parser[ActorID].DblValue;
                    propPCTX:
                        pctX := Parser[ActorID].DblValue;
                    propIDLEKW:
                        pctIdlekW := Parser[ActorID].DblValue;
                    propCLASS:
                        Storage2Class := Parser[ActorID].IntValue;
                    propInvEffCurve:
                        InverterCurve := Param;
                    propDISPOUTTRIG:
                        DischargeTrigger := Parser[ActorID].DblValue;
                    propDISPINTRIG:
                        ChargeTrigger := Parser[ActorID].DblValue;
                    propCHARGEEFF:
                        pctChargeEff := Parser[ActorID].DblValue;
                    propDISCHARGEEFF:
                        pctDischargeEff := Parser[ActorID].DblValue;
                    propPCTKWOUT:
                        pctkWout := Parser[ActorID].DblValue;
                    propCutin:
                        FpctCutIn := Parser[ActorID].DblValue;
                    propCutout:
                        FpctCutOut := Parser[ActorID].DblValue;
                    propVMINPU:
                        VMinPu := Parser[ActorID].DblValue;
                    propVMAXPU:
                        VMaxPu := Parser[ActorID].DblValue;
                    propSTATE:
                        FState := InterpretState(Param); //****
                    propKVA:
                        with Storage2Vars do
                        begin
                            FkVArating := Parser[ActorID].DblValue;
                            kVASet := TRUE;
                            if not kvarLimitSet then
                                Storage2Vars.Fkvarlimit := FkVArating;
                            if not kvarLimitSet and not kvarLimitNegSet then
                                Storage2Vars.Fkvarlimitneg := FkVArating;
                        end;
                    propKWRATED:
                        Storage2Vars.kWrating := Parser[ActorID].DblValue;
                    propKWHRATED:
                        Storage2Vars.kWhrating := Parser[ActorID].DblValue;
                    propKWHSTORED:
                        Storage2Vars.kWhstored := Parser[ActorID].DblValue;
                    propPCTRESERVE:
                        pctReserve := Parser[ActorID].DblValue;
                    propUSERMODEL:
                        UserModel.Name := Parser[ActorID].StrValue;  // Connect to user written models
                    propUSERDATA:
                        UserModel.Edit := Parser[ActorID].StrValue;  // Send edit string to user model
                    propDEBUGTRACE:
                        DebugTrace := InterpretYesNo(Param);
                    propPCTKWIN:
                        pctkWIn := Parser[ActorID].DblValue;
                    propPCTSTORED:
                        Storage2Vars.kWhStored := Parser[ActorID].DblValue * 0.01 * Storage2Vars.kWhRating;
                    propCHARGETIME:
                        ChargeTime := Parser[ActorID].DblValue;
                    propDynaDLL:
                        DynaModel.Name := Parser[ActorID].StrValue;
                    propDynaData:
                        DynaModel.Edit := Parser[ActorID].StrValue;
                    proppctkWrated:
                        Storage2Vars.FpctkWrated := Parser[ActorID].DblValue / 100.0;  // convert to pu
                    propBalanced:
                        ForceBalanced := InterpretYesNo(Param);
                    propLimited:
                        CurrentLimited := InterpretYesNo(Param);
                    propVarFollowInverter:
                        FVarFollowInverter := InterpretYesNo(Param);
                    propkvarLimit:
                    begin
                        Storage2Vars.Fkvarlimit := Abs(Parser[ActorID].DblValue);
                        kvarLimitSet := TRUE;
                        if not kvarLimitNegSet then
                            Storage2Vars.Fkvarlimitneg := Abs(Storage2Vars.Fkvarlimit);

                    end;
                    propPPriority:
                        Storage2Vars.P_priority := InterpretYesNo(Param);  // watt priority flag
                    propPFPriority:
                        Storage2Vars.PF_priority := InterpretYesNo(Param);

                    propPminNoVars:
                        FpctPminNoVars := Parser[ActorID].DblValue;
                    propPminkvarLimit:
                        FpctPminkvarLimit := Parser[ActorID].DblValue;

                    propkvarLimitneg:
                    begin
                        Storage2Vars.Fkvarlimitneg := Abs(Parser[ActorID].DblValue);
                        kvarLimitNegSet := TRUE;
                    end;

                else
               // Inherited parameters
                    ClassEdit(ActiveStorage2Obj, ParamPointer - NumPropsThisClass)
                end;

                case iCase of
                    1:
                        SetNcondsForConnection;  // Force Reallocation of terminal info
                // (PR) Make sure if we will need it
                { removed
                propKW,propPF: Begin
                                 SyncUpPowerQuantities;   // keep kvar nominal up to date with kW and PF

                               End;       }

        {Set loadshape objects;  returns nil If not valid}
                    propYEARLY:
                        YearlyShapeObj := LoadShapeClass[ActorID].Find(YearlyShape);
                    propDAILY:
                        DailyShapeObj := LoadShapeClass[ActorID].Find(DailyShape);
                    propDUTY:
                        DutyShapeObj := LoadShapeClass[ActorID].Find(DutyShape);

                    propKWRATED:
                        if not kVASet then
                            Storage2Vars.FkVArating := Storage2Vars.kWrating;
                    propKWHRATED:
                    begin
                        Storage2Vars.kWhStored := Storage2Vars.kWhRating; // Assume fully charged
                        kWhBeforeUpdate := Storage2Vars.kWhStored;
                        Storage2Vars.kWhReserve := Storage2Vars.kWhRating * pctReserve * 0.01;
                    end;

                    propPCTRESERVE:
                        Storage2Vars.kWhReserve := Storage2Vars.kWhRating * pctReserve * 0.01;

                    propInvEffCurve:
                        InverterCurveObj := XYCurveClass[ActorID].Find(InverterCurve);

                    propDEBUGTRACE:
                        if DebugTrace then
                        begin   // Init trace file
                            AssignFile(TraceFile, GetOutputDirectory + 'STOR_' + Name + '.CSV');
                            ReWrite(TraceFile);
                            Write(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, Storage2Model,  Qnominalperphase, Pnominalperphase, CurrentType');
                            for i := 1 to nphases do
                                Write(Tracefile, ', |Iinj' + IntToStr(i) + '|');
                            for i := 1 to nphases do
                                Write(Tracefile, ', |Iterm' + IntToStr(i) + '|');
                            for i := 1 to nphases do
                                Write(Tracefile, ', |Vterm' + IntToStr(i) + '|');
                            for i := 1 to NumVariables do
                                Write(Tracefile, ', ', VariableName(i));

                            Write(TraceFile, ',Vthev, Theta');
                            Writeln(TraceFile);
                            CloseFile(Tracefile);
                        end;

                    propUSERMODEL:
                        IsUserModel := UserModel.Exists;
                    propDynaDLL:
                        IsUserModel := DynaModel.Exists;

//                propPFPriority: For i := 1 to ControlElementList.ListSize Do
//                Begin
//
//                  if TControlElem(ControlElementList.Get(i)).ClassName = 'InvControl'  Then
//                      // Except for VW mode, all other modes (including combined ones) can operate with PF priority
//                      if (TInvControlObj(ControlElementList.Get(i)).Mode <> 'VOLTWATT') Then
//                          Storage2Vars.PF_Priority := FALSE; // For all other modes
//
//                End;

                end;
            end;

            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

        RecalcElementData(ActorID);
        YprimInvalid[ActorID] := TRUE;
    end;

end;

//----------------------------------------------------------------------------
function TStorage2.MakeLike(const OtherStorage2ObjName: String): Integer;

// Copy over essential properties from other object

var
    OtherStorage2Obj: TStorage2Obj;
    i: Integer;
begin
    Result := 0;
     {See If we can find this line name in the present collection}
    OtherStorage2Obj := Find(OtherStorage2ObjName);
    if (OtherStorage2Obj <> NIL) then
        with ActiveStorage2Obj do
        begin
            if (Fnphases <> OtherStorage2Obj.Fnphases) then
            begin
                Nphases := OtherStorage2Obj.Fnphases;
                NConds := Fnphases;  // Forces reallocation of terminal stuff
                Yorder := Fnconds * Fnterms;
                YprimInvalid[ActiveActor] := TRUE;
            end;

            Storage2Vars.kVStorage2Base := OtherStorage2Obj.Storage2Vars.kVStorage2Base;
            Vbase := OtherStorage2Obj.Vbase;
            Vminpu := OtherStorage2Obj.Vminpu;
            Vmaxpu := OtherStorage2Obj.Vmaxpu;
            Vbase95 := OtherStorage2Obj.Vbase95;
            Vbase105 := OtherStorage2Obj.Vbase105;
            kW_out := OtherStorage2Obj.kW_out;
            kvar_out := OtherStorage2Obj.kvar_out;
            Pnominalperphase := OtherStorage2Obj.Pnominalperphase;
            PFNominal := OtherStorage2Obj.PFNominal;
            Qnominalperphase := OtherStorage2Obj.Qnominalperphase;
            Connection := OtherStorage2Obj.Connection;
            YearlyShape := OtherStorage2Obj.YearlyShape;
            YearlyShapeObj := OtherStorage2Obj.YearlyShapeObj;
            DailyShape := OtherStorage2Obj.DailyShape;
            DailyShapeObj := OtherStorage2Obj.DailyShapeObj;
            DutyShape := OtherStorage2Obj.DutyShape;
            DutyShapeObj := OtherStorage2Obj.DutyShapeObj;
            DispatchMode := OtherStorage2Obj.DispatchMode;
            InverterCurve := OtherStorage2Obj.InverterCurve;
            InverterCurveObj := OtherStorage2Obj.InverterCurveObj;
            Storage2Class := OtherStorage2Obj.Storage2Class;
            VoltageModel := OtherStorage2Obj.VoltageModel;

            Fstate := OtherStorage2Obj.Fstate;
            FstateChanged := OtherStorage2Obj.FstateChanged;
            kvarLimitSet := OtherStorage2Obj.kvarLimitSet;
            kvarLimitNegSet := OtherStorage2Obj.kvarLimitNegSet;

            FpctCutin := OtherStorage2Obj.FpctCutin;
            FpctCutout := OtherStorage2Obj.FpctCutout;
            FVarFollowInverter := OtherStorage2Obj.FVarFollowInverter;
            Storage2Vars.Fkvarlimit := OtherStorage2Obj.Storage2Vars.Fkvarlimit;
            Storage2Vars.Fkvarlimitneg := OtherStorage2Obj.Storage2Vars.Fkvarlimitneg;
            Storage2Vars.FkVArating := OtherStorage2Obj.Storage2Vars.FkVArating;

            FpctPminNoVars := OtherStorage2Obj.FpctPminNoVars;
            FpctPminkvarLimit := OtherStorage2Obj.FpctPminkvarLimit;

            kWOutIdling := OtherStorage2Obj.kWOutIdling;

            Storage2Vars.kWRating := OtherStorage2Obj.Storage2Vars.kWRating;
            Storage2Vars.kWhRating := OtherStorage2Obj.Storage2Vars.kWhRating;
            Storage2Vars.kWhStored := OtherStorage2Obj.Storage2Vars.kWhStored;
            Storage2Vars.kWhReserve := OtherStorage2Obj.Storage2Vars.kWhReserve;
            kWhBeforeUpdate := OtherStorage2Obj.kWhBeforeUpdate;
            pctReserve := OtherStorage2Obj.pctReserve;
            DischargeTrigger := OtherStorage2Obj.DischargeTrigger;
            ChargeTrigger := OtherStorage2Obj.ChargeTrigger;
            pctChargeEff := OtherStorage2Obj.pctChargeEff;
            pctDischargeEff := OtherStorage2Obj.pctDischargeEff;
            pctkWout := OtherStorage2Obj.pctkWout;
            pctkWin := OtherStorage2Obj.pctkWin;
            pctIdlekW := OtherStorage2Obj.pctIdlekW;
            pctIdlekvar := OtherStorage2Obj.pctIdlekvar;
            ChargeTime := OtherStorage2Obj.ChargeTime;

            pctR := OtherStorage2Obj.pctR;
            pctX := OtherStorage2Obj.pctX;

            RandomMult := OtherStorage2Obj.RandomMult;
            FVWMode := OtherStorage2Obj.FVWMode;
            FVVMode := OtherStorage2Obj.FVVMode;
            FDRCMode := OtherStorage2Obj.FDRCMode;
            FWPMode := OtherStorage2Obj.FWPMode;
            FWVMode := OtherStorage2Obj.FWVMode;

            UserModel.Name := OtherStorage2Obj.UserModel.Name;  // Connect to user written models
            DynaModel.Name := OtherStorage2Obj.DynaModel.Name;
            IsUserModel := OtherStorage2Obj.IsUserModel;
            ForceBalanced := OtherStorage2Obj.ForceBalanced;
            CurrentLimited := OtherStorage2Obj.CurrentLimited;

            ClassMakeLike(OtherStorage2Obj);

            for i := 1 to ParentClass.NumProperties do
                FPropertyValue^[i] := OtherStorage2Obj.FPropertyValue^[i];

            Result := 1;
        end
    else
        DoSimpleMsg('Error in Storage2 MakeLike: "' + OtherStorage2ObjName + '" Not Found.', 562);

end;

//----------------------------------------------------------------------------
function TStorage2.Init(Handle: Integer; ActorID: Integer): Integer;
var
    p: TStorage2Obj;

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

    DoSimpleMsg('Need to implement TStorage2.Init', -1);
    Result := 0;
end;

{--------------------------------------------------------------------------}
procedure TStorage2.ResetRegistersAll;  // Force all EnergyMeters in the circuit to reset

var
    idx: Integer;

begin
    idx := First;
    while idx > 0 do
    begin
        TStorage2Obj(GetActiveObj).ResetRegisters;
        idx := Next;
    end;
end;

{--------------------------------------------------------------------------}
procedure TStorage2.SampleAll(ActorID: Integer);  // Force all Storage2 elements in the circuit to take a sample

var
    i: Integer;
begin
    for i := 1 to ElementList.ListSize do
        with TStorage2Obj(ElementList.Get(i)) do
            if Enabled then
                TakeSample(ActorID);
end;

//----------------------------------------------------------------------------
constructor TStorage2Obj.Create(ParClass: TDSSClass; const SourceName: String);
begin

    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; // + Storage2_ELEMENT;  // In both PCelement and Storage2element list

    Nphases := 3;
    Fnconds := 4;  // defaults to wye
    Yorder := 0;  // To trigger an initial allocation
    Nterms := 1;  // forces allocations

    YearlyShape := '';
    YearlyShapeObj := NIL;  // If YearlyShapeobj = nil Then the load alway stays nominal * global multipliers
    DailyShape := '';
    DailyShapeObj := NIL;  // If DaillyShapeobj = nil Then the load alway stays nominal * global multipliers
    DutyShape := '';
    DutyShapeObj := NIL;  // If DutyShapeobj = nil Then the load alway stays nominal * global multipliers

    InverterCurveObj := NIL;
    InverterCurve := '';

    Connection := 0;    // Wye (star)
    VoltageModel := 1;  {Typical fixed kW negative load}
    Storage2Class := 1;

    Storage2SolutionCount := -1;  // For keep track of the present solution in Injcurrent calcs
    OpenStorage2SolutionCount := -1;
    YPrimOpenCond := NIL;

    Storage2Vars.kVStorage2Base := 12.47;
    VBase := 7200.0;
    Vminpu := 0.90;
    Vmaxpu := 1.10;
    VBase95 := Vminpu * Vbase;
    VBase105 := Vmaxpu * Vbase;
    Yorder := Fnterms * Fnconds;
    RandomMult := 1.0;

    varMode := VARMODEPF;
    FInverterON := TRUE; // start with inverterON
    kVA_exceeded := FALSE;
    FVarFollowInverter := FALSE;

    ForceBalanced := FALSE;
    CurrentLimited := FALSE;

    with Storage2Vars do
    begin
        kWRating := 25.0;
        FkVArating := kWRating;
        kWhRating := 50;
        kWhStored := kWhRating;
        kWhBeforeUpdate := kWhRating;
        kWhReserve := kWhRating * pctReserve / 100.0;
        Fkvarlimit := FkVArating;
        Fkvarlimitneg := FkVArating;
        FpctkWrated := 1.0;
        P_Priority := FALSE;
        PF_Priority := FALSE;

        EffFactor := 1.0;

        Vreg := 9999;
        Vavg := 9999;
        VVOperation := 9999;
        VWOperation := 9999;
        DRCOperation := 9999;
        VVDRCOperation := 9999;
        WPOperation := 9999;
        WVOperation := 9999;

    end;

    FDCkW := 25.0;

    FpctCutIn := 0.0;
    FpctCutOut := 0.0;

    FpctPminNoVars := -1.0; // Deactivated by default
    FpctPminkvarLimit := -1.0; // Deactivated by default

    Fpf_wp_nominal := 1.0;

     {Output rating stuff}
    kvar_out := 0.0;
     // removed kvarBase     := kvar_out;     // initialize
    PFNominal := 1.0;

    pctR := 0.0;
    ;
    pctX := 50.0;

     {Make the Storage2Vars struct as public}
    PublicDataStruct := @Storage2Vars;
    PublicDataSize := SizeOf(TStorage2Vars);

    IsUserModel := FALSE;
    UserModel := TStoreUserModel.Create;
    DynaModel := TStoreDynaModel.Create;

    FState := STORE_IDLING;  // Idling and fully charged
    FStateChanged := TRUE;  // Force building of YPrim
    pctReserve := 20.0;  // per cent of kWhRating
    pctIdlekW := 1.0;
    pctIdlekvar := 0.0;

    DischargeTrigger := 0.0;
    ChargeTrigger := 0.0;
    pctChargeEff := 90.0;
    pctDischargeEff := 90.0;
    FpctkWout := 100.0;
    FpctkWin := 100.0;

    ChargeTime := 2.0;   // 2 AM

    kVASet := FALSE;
    kvarLimitSet := FALSE;
    kvarLimitNegSet := FALSE;

    Reg_kWh := 1;
    Reg_kvarh := 2;
    Reg_MaxkW := 3;
    Reg_MaxkVA := 4;
    Reg_Hours := 5;
    Reg_Price := 6;

    DebugTrace := FALSE;
    Storage2ObjSwitchOpen := FALSE;
    Spectrum := '';  // override base class
    SpectrumObj := NIL;
    FVWMode := FALSE;
    FVVMode := FALSE;
    FDRCMode := FALSE;
    FWPMode := FALSE;
    FWVMode := FALSE;

    InitPropertyValues(0);
    RecalcElementData(ActiveActor);

end;


//----------------------------------------------------------------------------
function TStorage2Obj.DecodeState: String;
begin
    case Fstate of
        STORE_CHARGING:
            Result := 'CHARGING';
        STORE_DISCHARGING:
            Result := 'DISCHARGING';
    else
        Result := 'IDLING';
    end;
end;

//----------------------------------------------------------------------------
procedure TStorage2Obj.InitPropertyValues(ArrayOffset: Integer);

// Define default values for the properties

begin

    PropertyValue[1] := '3';         //'phases';
    PropertyValue[2] := Getbus(1);   //'bus1';

    PropertyValue[propKV] := Format('%-g', [Storage2Vars.kVStorage2Base]);
    PropertyValue[propKW] := Format('%-g', [kW_out]);
    PropertyValue[propPF] := Format('%-g', [PFNominal]);
    PropertyValue[propMODEL] := '1';
    PropertyValue[propYEARLY] := '';
    PropertyValue[propDAILY] := '';
    PropertyValue[propDUTY] := '';
    PropertyValue[propDISPMODE] := 'Default';
    PropertyValue[propCONNECTION] := 'wye';
    PropertyValue[propKVAR] := Format('%-g', [Presentkvar]);

    PropertyValue[propPCTR] := Format('%-g', [pctR]);
    PropertyValue[propPCTX] := Format('%-g', [pctX]);

    PropertyValue[propIDLEKW] := '1';       // PERCENT
    PropertyValue[propCLASS] := '1'; //'class'
    PropertyValue[propDISPOUTTRIG] := '0';   // 0 MEANS NO TRIGGER LEVEL
    PropertyValue[propDISPINTRIG] := '0';
    PropertyValue[propCHARGEEFF] := '90';
    PropertyValue[propDISCHARGEEFF] := '90';
    PropertyValue[propPCTKWOUT] := '100';
    PropertyValue[propPCTKWIN] := '100';

    PropertyValue[propInvEffCurve] := '';
    PropertyValue[propCutin] := '0';
    PropertyValue[propCutout] := '0';
    PropertyValue[propVarFollowInverter] := 'NO';

    PropertyValue[propVMINPU] := '0.90';
    PropertyValue[propVMAXPU] := '1.10';
    PropertyValue[propSTATE] := 'IDLING';

    with Storage2Vars do
    begin
        PropertyValue[propKVA] := Format('%-g', [Storage2Vars.FkVARating]);
        PropertyValue[propkvarLimit] := Format('%-g', [Fkvarlimit]);
        PropertyValue[propkvarLimitneg] := Format('%-g', [Fkvarlimitneg]);
        PropertyValue[propKWRATED] := Format('%-g', [kWRating]);
        PropertyValue[propKWHRATED] := Format('%-g', [kWhRating]);
        PropertyValue[propKWHSTORED] := Format('%-g', [kWhStored]);
        PropertyValue[propPCTSTORED] := Format('%-g', [kWhStored / kWhRating * 100.0])
    end;

    PropertyValue[propPCTRESERVE] := Format('%-g', [pctReserve]);
    PropertyValue[propCHARGETIME] := Format('%-g', [ChargeTime]);

    PropertyValue[propUSERMODEL] := '';  // Usermodel
    PropertyValue[propUSERDATA] := '';  // Userdata
    PropertyValue[propDYNADLL] := '';  //
    PropertyValue[propDYNADATA] := '';  //
    PropertyValue[propDEBUGTRACE] := 'NO';
    PropertyValue[propBalanced] := 'NO';
    PropertyValue[propLimited] := 'NO';
    PropertyValue[proppctkWrated] := '100';  // Included
    PropertyValue[propPpriority] := 'NO';   // Included
    PropertyValue[propPFPriority] := 'NO';

    inherited  InitPropertyValues(NumPropsThisClass);

end;


//----------------------------------------------------------------------------
function TStorage2Obj.GetPropertyValue(Index: Integer): String;
begin

    Result := '';
    with Storage2Vars do
        case Index of
            propKV:
                Result := Format('%.6g', [Storage2Vars.kVStorage2Base]);
            propKW:
                Result := Format('%.6g', [kW_out]);
            propPF:
                Result := Format('%.6g', [PFNominal]);
            propMODEL:
                Result := Format('%d', [VoltageModel]);
            propYEARLY:
                Result := YearlyShape;
            propDAILY:
                Result := DailyShape;
            propDUTY:
                Result := DutyShape;

            propDISPMODE:
                Result := ReturnDispMode(DispatchMode);

          {propCONNECTION :;}
            propKVAR:
                Result := Format('%.6g', [kvar_out]);
            propPCTR:
                Result := Format('%.6g', [pctR]);
            propPCTX:
                Result := Format('%.6g', [pctX]);
            propIDLEKW:
                Result := Format('%.6g', [pctIdlekW]);
          {propCLASS      = 17;}
            propInvEffCurve:
                Result := InverterCurve;
            propCutin:
                Result := Format('%.6g', [FpctCutin]);
            propCutOut:
                Result := Format('%.6g', [FpctCutOut]);
            propVarFollowInverter:
                if FVarFollowInverter then
                    Result := 'Yes'
                else
                    Result := 'No';

            propPminNoVars:
                Result := Format('%.6g', [FpctPminNoVars]);
            propPminkvarLimit:
                Result := Format('%.6g', [FpctPminkvarLimit]);

            propDISPOUTTRIG:
                Result := Format('%.6g', [DischargeTrigger]);
            propDISPINTRIG:
                Result := Format('%.6g', [ChargeTrigger]);
            propCHARGEEFF:
                Result := Format('%.6g', [pctChargeEff]);
            propDISCHARGEEFF:
                Result := Format('%.6g', [pctDischargeEff]);
            propPCTKWOUT:
                Result := Format('%.6g', [pctkWout]);

            propVMINPU:
                Result := Format('%.6g', [VMinPu]);
            propVMAXPU:
                Result := Format('%.6g', [VMaxPu]);
            propSTATE:
                Result := DecodeState;

          {Storage2Vars}
            propKVA:
                Result := Format('%.6g', [FkVArating]);
            propKWRATED:
                Result := Format('%.6g', [kWrating]);
            propKWHRATED:
                Result := Format('%.6g', [kWhrating]);
            propKWHSTORED:
                Result := Format('%.6g', [kWHStored]);


            propPCTRESERVE:
                Result := Format('%.6g', [pctReserve]);
            propUSERMODEL:
                Result := UserModel.Name;
            propUSERDATA:
                Result := '(' + inherited GetPropertyValue(index) + ')';
            proppctkWrated:
                Result := Format('%.6g', [FpctkWrated * 100.0]);
            propDynaDLL:
                Result := DynaModel.Name;
            propdynaDATA:
                Result := '(' + inherited GetPropertyValue(index) + ')';
          {propDEBUGTRACE = 33;}
            propPCTKWIN:
                Result := Format('%.6g', [pctkWin]);
            propPCTSTORED:
                Result := Format('%.6g', [kWhStored / kWhRating * 100.0]);
            propCHARGETIME:
                Result := Format('%.6g', [Chargetime]);
            propBalanced:
                if ForceBalanced then
                    Result := 'Yes'
                else
                    Result := 'No';
            propLimited:
                if CurrentLimited then
                    Result := 'Yes'
                else
                    Result := 'No';
            propkvarLimit:
                Result := Format('%.6g', [Fkvarlimit]);
            propkvarLimitneg:
                Result := Format('%.6g', [Fkvarlimitneg]);

        else  // take the generic handler
            Result := inherited GetPropertyValue(index);
        end;
end;


//----------------------------------------------------------------------------
procedure TStorage2Obj.Randomize(Opt: Integer);
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
destructor TStorage2Obj.Destroy;
begin
    YPrimOpenCond.Free;
    UserModel.Free;
    DynaModel.Free;
    inherited Destroy;
end;


//----------------------------------------------------------------------------
procedure TStorage2Obj.CalcDailyMult(Hr: Double; ActorID: Integer);

begin
    if (DailyShapeObj <> NIL) then
    begin
        ShapeFactor := DailyShapeObj.GetMult(Hr);
    end
    else
        ShapeFactor := CDOUBLEONE;  // Default to no  variation

    CheckStateTriggerLevel(ShapeFactor.re, ActorID);   // last recourse
end;


//----------------------------------------------------------------------------
procedure TStorage2Obj.CalcDutyMult(Hr: Double; ActorID: Integer);

begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMult(Hr);
        CheckStateTriggerLevel(ShapeFactor.re, ActorID);
    end
    else
        CalcDailyMult(Hr, ActorID);  // Default to Daily Mult If no duty curve specified
end;

//----------------------------------------------------------------------------
procedure TStorage2Obj.CalcYearlyMult(Hr: Double; ActorID: Integer);

begin
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMult(Hr);
        CheckStateTriggerLevel(ShapeFactor.re, ActorID);
    end
    else
        CalcDailyMult(Hr, ActorID);  // Defaults to Daily curve
end;

//----------------------------------------------------------------------------
procedure TStorage2Obj.RecalcElementData(ActorID: Integer);
begin

    VBase95 := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

   // removed 5/8/17 kvarBase := kvar_out ;  // remember this for Follow Mode

    with Storage2Vars do
    begin

        YeqDischarge := Cmplx((kWrating * 1000.0 / SQR(vbase) / FNPhases), 0.0);

      // values in ohms for thevenin equivalents
        RThev := pctR * 0.01 * SQR(PresentkV) / FkVARating * 1000.0;      // Changed
        XThev := pctX * 0.01 * SQR(PresentkV) / FkVARating * 1000.0;      // Changed

        CutInkW := FpctCutin * FkVArating / 100.0;
        CutOutkW := FpctCutOut * FkVArating / 100.0;

        if FpctPminNoVars <= 0 then
            PminNoVars := -1.0
        else
            PminNoVars := FpctPminNoVars * kWrating / 100.0;

        if FpctPminkvarLimit <= 0 then
            PminkvarLimit := -1.0
        else
            PminkvarLimit := FpctPminkvarLimit * kWrating / 100.0;

      // efficiencies
        ChargeEff := pctChargeEff * 0.01;
        DisChargeEff := pctDisChargeEff * 0.01;

        PIdling := pctIdlekW * kWrating / 100.0;

        if Assigned(InverterCurveObj) then
        begin
            kWOutIdling := PIdling / (InverterCurveObj.GetYValue(Pidling / (FkVArating)));
        end
        else
            kWOutIdling := PIdling;

    end;

    SetNominalStorage2Output(ActorID);

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

    if Length(Spectrum) > 0 then
    begin
        SpectrumObj := SpectrumClass[ActorID].Find(Spectrum);
        if SpectrumObj = NIL then
            DoSimpleMsg('ERROR! Spectrum "' + Spectrum + '" Not Found.', 566);
    end
    else
        SpectrumObj := NIL;

    // Initialize to Zero - defaults to PQ Storage2 element
    // Solution object will reset after circuit modifications

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    {Update any user-written models}
    if Usermodel.Exists then
        UserModel.FUpdateModel;  // Checks for existence and Selects
    if Dynamodel.Exists then
        Dynamodel.FUpdateModel;  // Checks for existence and Selects

end;
//----------------------------------------------------------------------------
procedure TStorage2Obj.SetNominalStorage2Output(ActorID: Integer);
begin

    ShapeFactor := CDOUBLEONE;  // init here; changed by curve routine
    // Check to make sure the Storage2 element is ON
    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
        if not (IsDynamicModel or IsHarmonicModel) then     // Leave Storage2 element in whatever state it was prior to entering Dynamic mode
        begin
          // Check dispatch to see what state the Storage2 element should be in
            case DispatchMode of

                STORE_EXTERNALMODE: ;  // Do nothing
                STORE_LOADMODE:
                    CheckStateTriggerLevel(GeneratorDispatchReference, ActorID);
                STORE_PRICEMODE:
                    CheckStateTriggerLevel(PriceSignal, ActorID);

            else // dispatch off element's loadshapes, If any

                with Solution do
                    case Mode of
                        SNAPSHOT: ; {Just solve for the present kW, kvar}  // Don't check for state change
                        DAILYMODE:
                            CalcDailyMult(DynaVars.dblHour, ActorID); // Daily dispatch curve
                        YEARLYMODE:
                            CalcYearlyMult(DynaVars.dblHour, ActorID);
             (*
                MONTECARLO1,
                MONTEFAULT,
                FAULTSTUDY,
                DYNAMICMODE:   ; // {do nothing for these modes}
             *)
                        GENERALTIME:
                        begin
                                // This mode allows use of one class of load shape
                            case ActiveCircuit[ActorID].ActiveLoadShapeClass of
                                USEDAILY:
                                    CalcDailyMult(DynaVars.dblHour, ActorID);
                                USEYEARLY:
                                    CalcYearlyMult(DynaVars.dblHour, ActorID);
                                USEDUTY:
                                    CalcDutyMult(DynaVars.dblHour, ActorID);
                            else
                                ShapeFactor := CDOUBLEONE     // default to 1 + j1 if not known
                            end;
                        end;
                // Assume Daily curve, If any, for the following
                        MONTECARLO2,
                        MONTECARLO3,
                        LOADDURATION1,
                        LOADDURATION2:
                            CalcDailyMult(DynaVars.dblHour, ActorID);
                        PEAKDAY:
                            CalcDailyMult(DynaVars.dblHour, ActorID);

                        DUTYCYCLE:
                            CalcDutyMult(DynaVars.dblHour, ActorID);
                {AUTOADDFLAG:  ; }
                    end;

            end;

            ComputekWkvar;

          {
           Pnominalperphase is net at the terminal.  If supplying idling losses, when discharging,
           the Storage2 supplies the idling losses. When charging, the idling losses are subtracting from the amount
           entering the Storage2 element.
          }

            with Storage2Vars do
            begin
                Pnominalperphase := 1000.0 * kW_out / Fnphases;
                Qnominalperphase := 1000.0 * kvar_out / Fnphases;
            end;


            case VoltageModel of
            //****  Fix this when user model gets connected in
                3: // Yeq := Cinv(cmplx(0.0, -StoreVARs.Xd))  ;  // Gets negated in CalcYPrim
            else
             {
              Yeq no longer used for anything other than this calculation of Yeq95, Yeq105 and
              constant Z power flow model
             }
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
          { Like Model 7 generator, max current is based on amount of current to get out requested power at min voltage
          }
            with Storage2Vars do
            begin
                PhaseCurrentLimit := Cdivreal(Cmplx(Pnominalperphase, Qnominalperphase), VBase95);
                MaxDynPhaseCurrent := Cabs(PhaseCurrentLimit);
            end;

              { When we leave here, all the Yeq's are in L-N values}

        end;  {If  NOT (IsDynamicModel or IsHarmonicModel)}
    end;  {With ActiveCircuit[ActiveActor]}

   // If Storage2 element state changes, force re-calc of Y matrix
    if FStateChanged then
    begin
        YprimInvalid[ActorID] := TRUE;
        FStateChanged := FALSE;  // reset the flag
    end;

end;
// ===========================================================================================
procedure TStorage2Obj.ComputekWkvar;
begin

    ComputePresentkW;
    ComputeInverterPower; // apply inverter eff after checking for cutin/cutout

end;
//----------------------------------------------------------------------------
procedure TStorage2Obj.ComputePresentkW;
var
    OldState: Integer;
begin
    OldState := Fstate;
    FStateDesired := OldState;
    with Storage2Vars do
        case FState of

            STORE_CHARGING:
            begin
                if kWhStored < kWhRating then
                    case DispatchMode of
                        STORE_FOLLOW:
                        begin
                            kW_out := kWRating * ShapeFactor.re;
                            FpctkWin := abs(ShapeFactor.re) * 100.0;  // keep %charge updated
                        end
                    else
                        kW_out := -kWRating * pctkWin / 100.0;
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
                            FpctkWOut := abs(ShapeFactor.re) * 100.0;  // keep %discharge updated
                        end
                    else
                        kW_out := kWRating * pctkWout / 100.0;
                    end
                else
                    Fstate := STORE_IDLING;  // not enough Storage2 to discharge
            end;

        end;

    {If idling output is only losses}

    if Fstate = STORE_IDLING then
    begin
        kW_out := -kWOutIdling;
    end;

    if OldState <> Fstate then
        FstateChanged := TRUE;

end;

// ===========================================================================================
procedure TStorage2Obj.ComputeInverterPower;
var

    kVA_Gen: Double;
    OldState: Integer;
    TempPF: Double; // temporary power factor
    Qramp_limit: Double;

begin

    // Reset CurrentkvarLimit to kvarLimit
    CurrentkvarLimit := Storage2Vars.Fkvarlimit;
    CurrentkvarLimitNeg := Storage2Vars.Fkvarlimitneg;

    with Storage2Vars do
    begin

        if Assigned(InverterCurveObj) then
        begin
            if Fstate = STORE_DISCHARGING then
            begin
                FCutOutkWAC := CutOutkW * InverterCurveObj.GetYValue(abs(CutOutkW) / FkVArating);
                FCutInkWAC := CutInkW * InverterCurveObj.GetYValue(abs(CutInkW) / FkVArating);
            end
            else  // Charging or Idling
            begin
                FCutOutkWAC := CutOutkW / InverterCurveObj.GetYValue(abs(CutOutkW) / FkVArating);
                FCutInkWAC := CutInkW / InverterCurveObj.GetYValue(abs(CutInkW) / FkVArating);
            end;
        end
        else // Assume Ideal Inverter
        begin
            FCutOutkWAC := CutOutkW;
            FCutInkWAC := CutInkW;
        end;

        OldState := Fstate;

      // CutIn/CutOut checking performed on the AC side.
        if FInverterON then
        begin
            if abs(kW_Out) < FCutOutkWAC then
            begin
                FInverterON := FALSE;
                Fstate := STORE_IDLING;
            end;
        end
        else
        begin
            if abs(kW_Out) >= FCutInkWAC then
            begin
                FInverterON := TRUE;
            end
            else
            begin
                Fstate := STORE_IDLING;
            end;
        end;


        if OldState <> Fstate then
            FstateChanged := TRUE;

      // Set inverter output
        if FInverterON then
        begin
            kWOut_Calc;
        end
        else
        begin
        // Idling
            kW_Out := -kWOutIdling; // In case it has just turned off due to %CutIn/%CutOut. Necessary to make sure SOC will be kept constant (higher priority than the %CutIn/%CutOut operation)
        end;


      // Calculate kvar value based on operation mode (PF or kvar)
        if FState = STORE_IDLING then      // Should watt-pf with watt=0 be applied here?
        // If in Idling state, check for kvarlimit only
        begin
            if varMode = VARMODEPF then
            begin
//              kvar_out := 0.0; //kW = 0 leads to kvar = 0 in constant PF Mode
                kvar_out := kW_out * sqrt(1.0 / SQR(PFnominal) - 1.0) * sign(PFnominal);

                if (kvar_out > 0.0) and (abs(kvar_out) > Fkvarlimit) then
                    kvar_Out := Fkvarlimit
                else
                if (kvar_out < 0.0) and (abs(kvar_out) > Fkvarlimitneg) then
                    kvar_Out := Fkvarlimitneg * sign(kvarRequested)

            end
            else  // kvarRequested might have been set either internally or by an InvControl
            begin

                if (kvarRequested > 0.0) and (abs(kvarRequested) > Fkvarlimit) then
                    kvar_Out := Fkvarlimit
                else
                if (kvarRequested < 0.0) and (abs(kvarRequested) > Fkvarlimitneg) then
                    kvar_Out := Fkvarlimitneg * sign(kvarRequested)
                else
                    kvar_Out := kvarRequested;

            end;
        end
        else
        // If in either Charging or Discharging states
        begin
            if (abs(kW_Out) < PminNoVars) then
            begin
                kvar_out := 0.0;  // Check minimum P for Q gen/absorption. if PminNoVars is disabled (-1), this will always be false

                CurrentkvarLimit := 0;
                CurrentkvarLimitNeg := 0.0;  // InvControl uses this.
            end
            else
            if varMode = VARMODEPF then
            begin
                if PFnominal = 1.0 then
                    kvar_out := 0.0
                else
                begin
                    kvar_out := kW_out * sqrt(1.0 / SQR(PFnominal) - 1.0) * sign(PFnominal); //kvar_out desired by constant PF

                        // Check Limits
                    if abs(kW_out) < PminkvarLimit then // straight line limit check. if PminkvarLimit is disabled (-1), this will always be false.
                    begin
                            // straight line starts at max(PminNoVars, FCutOutkWAC)
                            // if CutOut differs from CutIn, take cutout since it is assumed that CutOut <= CutIn always.
                        if abs(kW_out) >= max(PminNoVars, FCutOutkWAC) then
                        begin
                            if (kvar_Out > 0.0) then
                            begin
                                Qramp_limit := Fkvarlimit / PminkvarLimit * abs(kW_out);   // generation limit
                            end
                            else
                            if (kvar_Out < 0.0) then
                            begin
                                Qramp_limit := Fkvarlimitneg / PminkvarLimit * abs(kW_out);   // absorption limit
                            end;

                            if abs(kvar_Out) > Qramp_limit then
                            begin
                                kvar_out := Qramp_limit * sign(kW_out) * sign(PFnominal);

                                if kvar_out > 0 then
                                    CurrentkvarLimit := Qramp_limit;  // For use in InvControl
                                if kvar_out < 0 then
                                    CurrentkvarLimitNeg := Qramp_limit;  // For use in InvControl

                            end;

                        end
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
            else  // VARMODE kvar
            begin
                  // Check limits
                if abs(kW_out) < PminkvarLimit then // straight line limit check. if PminkvarLimit is disabled (-1), this will always be false.
                begin
                      // straight line starts at max(PminNoVars, FCutOutkWAC)
                      // if CutOut differs from CutIn, take cutout since it is assumed that CutOut <= CutIn always.
                    if abs(kW_out) >= max(PminNoVars, FCutOutkWAC) then
                    begin
                        if (kvarRequested > 0.0) then
                        begin
                            Qramp_limit := Fkvarlimit / PminkvarLimit * abs(kW_out);   // generation limit
                            CurrentkvarLimit := Qramp_limit;    // For use in InvControl
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
                if ((kvarRequested > 0.0) and (abs(kvarRequested) > Fkvarlimit)) or ((kvarRequested < 0.0) and (abs(kvarRequested) > Fkvarlimitneg)) then
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
                      // PF Priority is not valid if controlled by an InvControl operating in at least one amongst VV and DRC modes
                    else
                    if PF_Priority and (not FVVMode or not FDRCMode or not FWVmode) then
                    begin
                        if abs(kvarRequested) > 0.0 then
                        begin

                            TempPF := cos(arctan(abs(kvarRequested / kW_out)));
                            kW_out := abs(kvar_out) * sqrt(1.0 / (1.0 - Sqr(TempPF)) - 1.0) * sign(kW_out);
                        end
                    end

                end
                else
                    kvar_Out := kvarRequested;
            end;
        end;

        if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then
            kvar_out := 0.0;

      // Limit kvar and kW so that kVA of inverter is not exceeded
        kVA_Gen := Sqrt(Sqr(kW_out) + Sqr(kvar_out));

        if kVA_Gen > FkVArating then
        begin

            kVA_exceeded := TRUE;

          // Expectional case: When kVA is exceeded and in idling state, we force P priority always
            if (FState = STORE_IDLING) then
            begin
                kvar_Out := Sqrt(SQR(FkVArating) - SQR(kW_Out)) * sign(kvar_Out);
            end

          // Regular Cases
            else
            if (varMode = VARMODEPF) and PF_Priority then
            // Operates under constant power factor when kVA rating is exceeded. PF must be specified and PFPriority must be TRUE
            begin
                kW_out := FkVArating * abs(PFnominal) * sign(kW_out);

                kvar_out := FkVArating * sqrt(1 - Sqr(PFnominal)) * sign(kW_out) * sign(PFnominal);
            end
            else
            if (varMode = VARMODEKVAR) and PF_Priority and FWPMode then
            begin
                kW_out := FkVArating * abs(Fpf_wp_nominal) * sign(kW_out);
                kvar_out := FkVArating * abs(sin(ArcCos(Fpf_wp_nominal))) * sign(kvarRequested)
            end
            else
            if (varMode = VARMODEKVAR) and PF_Priority and (not FVVMode or not FDRCMode or not FWVmode) then
            // Operates under constant power factor (PF implicitly calculated based on kw and kvar)
            begin
                if abs(kvar_out) = Fkvarlimit then
                begin   // for handling cases when kvar limit and inverter's kVA limit are exceeded
                    kW_out := FkVArating * abs(TempPF) * sign(kW_out);  // Temp PF has already been calculated at this point
                end
                else
                begin
                    kW_out := FkVArating * abs(cos(ArcTan(kvarRequested / kW_out))) * sign(kW_out);
                end;

                kvar_out := FkVArating * abs(sin(ArcCos(kW_out / FkVArating))) * sign(kvarRequested)
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
                    kW_Out := Sqrt(SQR(FkVArating) - SQR(kvar_Out)) * sign(kW_Out); // Q Priority   (Default) back off the kW

            end;

        end  {With Storage2Vars}
        else
        if abs(kVA_Gen - FkVArating) / FkVArating < 0.0005 then
            kVA_exceeded := TRUE
        else
            kVA_exceeded := FALSE;

    end;

end;
//----------------------------------------------------------------------------
procedure TStorage2Obj.CalcYPrimMatrix(Ymatrix: TcMatrix; ActorID: Integer);

var
    Y, Yij: Complex;
    i, j: Integer;
    FreqMultiplier: Double;

begin

    FYprimFreq := ActiveCircuit[ActorID].Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

    with  ActiveCircuit[ActorID].solution do
        if {IsDynamicModel or} IsHarmonicModel then
        begin
       {Yeq is computed from %R and %X -- inverse of Rthev + j Xthev}
            case Fstate of
                STORE_CHARGING:
                    Y := YeqDischarge;
                STORE_IDLING:
                    Y := cmplx(0, 0);
                STORE_DISCHARGING:
                    Y := cnegate(YeqDischarge);

               // old way Y  := Yeq   // L-N value computed in initialization routines
            end;

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
        begin  //  Regular power flow Storage2 element model

       {Yeq is always expected as the equivalent line-neutral admittance}


            case Fstate of
                STORE_CHARGING:
                    Y := YeqDischarge;
                STORE_IDLING:
                    Y := cmplx(0.0, 0.0);
                STORE_DISCHARGING:
                    Y := cnegate(YeqDischarge);
            end;

       //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, Change To State=%s, Y=%.8g +j %.8g',[ActiveCircuit[ActiveActor].Solution.dblHour, StateToStr, Y.re, Y.im]));

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
function TStorage2Obj.NormalizeToTOD(h: Integer; sec: Double): Double;
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

//----------------------------------------------------------------------------
procedure TStorage2Obj.CheckStateTriggerLevel(Level: Double; ActorID: Integer);
{This is where we set the state of the Storage2 element}

var
    OldState: Integer;

begin
    FStateChanged := FALSE;

    OldState := Fstate;

    with Storage2Vars do
        if DispatchMode = STORE_FOLLOW then
        begin

         // set charge and discharge modes based on sign of loadshape
            if (Level > 0.0) and (kWhStored > kWhReserve) then
                Storage2State := STORE_DISCHARGING
            else
            if (Level < 0.0) and (kWhStored < kWhRating) then
                Storage2State := STORE_CHARGING
            else
                Storage2State := STORE_IDLING;

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
                            with ActiveCircuit[ActorID].Solution do
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
        YprimInvalid[ActorID] := TRUE;
    end;
end;

//----------------------------------------------------------------------------
procedure TStorage2Obj.CalcYPrim(ActorID: Integer);

var
    i: Integer;

begin

     // Build only shunt Yprim
     // Build a dummy Yprim Series so that CalcV Does not fail
    if YprimInvalid[ActorID] then
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

    SetNominalStorage2Output(ActorID);
    CalcYPrimMatrix(YPrim_Shunt, ActorID);

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages Doesn't fail
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));

    YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
    inherited CalcYPrim(ActorID);

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorage2Obj.StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);
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
procedure TStorage2Obj.WriteTraceRecord(const s: String; ActorID: Integer);

var
    i: Integer;

begin

    try
        if (not InshowResults) then
        begin
            Append(TraceFile);
            Write(TraceFile, Format('%-.g, %d, %-.g, ',
                [ActiveCircuit[ActorID].Solution.DynaVars.dblHour,
                ActiveCircuit[ActorID].Solution.Iteration,
                ActiveCircuit[ActorID].LoadMultiplier]),
                GetSolutionModeID, ', ',
                GetLoadModel, ', ',
                VoltageModel: 0, ', ',
                (Qnominalperphase * 3.0 / 1.0e6): 8: 2, ', ',
                (Pnominalperphase * 3.0 / 1.0e6): 8: 2, ', ',
                s, ', ');
            for i := 1 to nphases do
                Write(TraceFile, (Cabs(InjCurrent^[i])): 8: 1, ', ');
            for i := 1 to nphases do
                Write(TraceFile, (Cabs(ITerminal^[i])): 8: 1, ', ');
            for i := 1 to nphases do
                Write(TraceFile, (Cabs(Vterminal^[i])): 8: 1, ', ');
            for i := 1 to NumVariables do
                Write(TraceFile, Format('%-.g, ', [Variable[i]]));


   //****        Write(TraceFile,VThevMag:8:1 ,', ', StoreVARs.Theta*180.0/PI);
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
procedure TStorage2Obj.DoConstantPQStorage2Obj(ActorID: Integer);

{Compute total terminal current for Constant PQ}

var
    i: Integer;
    Curr,
//   CurrIdlingZ,
    VLN, VLL: Complex;
   //---DEBUG--- S:Complex;
    VmagLN,
    VmagLL: Double;
    V012: array[0..2] of Complex;  // Sequence voltages

begin
     //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    ZeroITerminal;

    //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, State=%s, Iyprim= %s', [ActiveCircuit[ActiveActor].Solution.dblHour, StateToStr, CmplxArrayToString(InjCurrent, Yprim.Order) ]));

//    CASE FState of
//      STORE_IDLING:  // YPrim current is only current
//             Begin
//                For i := 1 to FNPhases Do
//                Begin
//                    Curr :=  InjCurrent^[i];
//                    StickCurrInTerminalArray(ITerminal, Curr, i);  // Put YPrim contribution into Terminal array taking into account connection
//                    set_ITerminalUpdated(True, ActorID);
//                    StickCurrInTerminalArray(InjCurrent, Cnegate(Curr), i);    // Compensation current is zero since terminal current is same as Yprim contribution
//                    //---DEBUG--- S := Cmul(Vterminal^[i] , Conjg(Iterminal^[i]));  // for debugging below
//                    //---DEBUG--- WriteDLLDebugFile(Format('        Phase=%d, Pnom=%.8g +j %.8g',[i, S.re, S.im ]));
//                End;
//             //---DEBUG--- WriteDLLDebugFile(Format('        Icomp=%s ', [CmplxArrayToString(InjCurrent, Yprim.Order) ]));
//             End;
//    ELSE   // For Charging and Discharging

    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load

    if ForceBalanced and (Fnphases = 3) then
    begin  // convert to pos-seq only
        Phase2SymComp(Vterminal, @V012);
        V012[0] := CZERO; // Force zero-sequence voltage to zero
        V012[2] := CZERO; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, @V012);  // Reconstitute Vterminal as balanced
    end;

    for i := 1 to Fnphases do
    begin

        case Connection of

            0:
            begin  {Wye}
                VLN := Vterminal^[i];
                VMagLN := Cabs(VLN);
                if VMagLN <= VBase95 then
                    Curr := Cmul(Yeq95, VLN)  // Below 95% use an impedance model
                else
                if VMagLN > VBase105 then
                    Curr := Cmul(Yeq105, VLN)  // above 105% use an impedance model
                else
                    Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLN));  // Between 95% -105%, constant PQ

                if CurrentLimited then
                    if Cabs(Curr) > MaxDynPhaseCurrent then
                        Curr := Conjg(Cdiv(PhaseCurrentLimit, CDivReal(VLN, VMagLN)));
            end;

            1:
            begin  {Delta}
                VLL := Vterminal^[i];
                VMagLL := Cabs(VLL);
                if Fnphases > 1 then
                    VMagLN := VMagLL / SQRT3
                else
                    VMagLN := VmagLL;  // L-N magnitude
                if VMagLN <= VBase95 then
                    Curr := Cmul(CdivReal(Yeq95, 3.0), VLL)  // Below 95% use an impedance model
                else
                if VMagLN > VBase105 then
                    Curr := Cmul(CdivReal(Yeq105, 3.0), VLL)  // above 105% use an impedance model
                else
                    Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLL));  // Between 95% -105%, constant PQ

                if CurrentLimited then
                    if Cabs(Curr) * SQRT3 > MaxDynPhaseCurrent then
                        Curr := Conjg(Cdiv(PhaseCurrentLimit, CDivReal(VLL, VMagLN))); // Note VmagLN has sqrt3 factor in it
            end;

        end;

         //---DEBUG--- WriteDLLDebugFile(Format('        Phase=%d, Pnom=%.8g +j %.8g', [i, Pnominalperphase, Qnominalperphase ]));

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE, ActorID);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
        //---DEBUG--- WriteDLLDebugFile(Format('        Icomp=%s ', [CmplxArrayToString(InjCurrent, Yprim.Order) ]));
//    END;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorage2Obj.DoConstantZStorage2Obj(ActorID: Integer);

{constant Z model}
var
    i: Integer;
    Curr,
    Yeq2: Complex;
    V012: array[0..2] of Complex;  // Sequence voltages

begin

// Assume Yeq is kept up to date

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;
    if Connection = 0 then
        Yeq2 := Yeq
    else
        Yeq2 := CdivReal(Yeq, 3.0);

    if ForceBalanced and (Fnphases = 3) then
    begin  // convert to pos-seq only
        Phase2SymComp(Vterminal, @V012);
        V012[0] := CZERO; // Force zero-sequence voltage to zero
        V012[2] := CZERO; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, @V012);  // Reconstitute Vterminal as balanced
    end;

    for i := 1 to Fnphases do
    begin

        Curr := Cmul(Yeq2, Vterminal^[i]);   // Yeq is always line to neutral
        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE, ActorID);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection

    end;

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorage2Obj.DoUserModel(ActorID: Integer);
{Compute total terminal Current from User-written model}
var
    i: Integer;

begin

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array

    if UserModel.Exists then    // Check automatically selects the usermodel If true
    begin
        UserModel.FCalc(Vterminal, Iterminal);
        set_ITerminalUpdated(TRUE, ActorID);
        with ActiveCircuit[ActorID].Solution do
        begin          // Negate currents from user model for power flow Storage2 element model
            for i := 1 to FnConds do
                Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));
        end;
    end
    else
    begin
        DoSimpleMsg('Storage2.' + name + ' model designated to use user-written model, but user-written model is not defined.', 567);
    end;

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorage2Obj.DoDynamicMode;

{Compute Total Current and add into InjTemp}
{
   For now, just assume the Storage2 element Thevenin voltage is constant
   for the duration of the dynamic simulation.
}
{****}
var
    i: Integer;
    V012,
    I012: array[0..2] of Complex;


    procedure CalcVthev_Dyn;
    begin
        with Storage2Vars do
            Vthev := pclx(VthevMag, Theta);   // keeps theta constant
    end;

begin

{****}  // Test using DESS model
   // Compute Vterminal

    if DynaModel.Exists then
        DoDynaModel(ActorID)   // do user-written model

    else
    begin

        CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
        ZeroITerminal;

       // Simple Thevenin equivalent
       // compute terminal current (Iterminal) and take out the Yprim contribution

        with Storage2Vars do
            case Fnphases of
                1:
                begin
                    CalcVthev_Dyn;  // Update for latest phase angle
                    ITerminal^[1] := CDiv(CSub(Csub(VTerminal^[1], Vthev), VTerminal^[2]), Zthev);
                    if CurrentLimited then
                        if Cabs(Iterminal^[1]) > MaxDynPhaseCurrent then   // Limit the current but keep phase angle
                            ITerminal^[1] := ptocomplex(topolar(MaxDynPhaseCurrent, cang(Iterminal^[1])));
                    ITerminal^[2] := Cnegate(ITerminal^[1]);
                end;
                3:
                begin
                    Phase2SymComp(Vterminal, @V012);

                  // Positive Sequence Contribution to Iterminal
                    CalcVthev_Dyn;  // Update for latest phase angle

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

                    I012[0] := CZERO;

                    SymComp2Phase(ITerminal, @I012);  // Convert back to phase components

                end;
            else
                DoSimpleMsg(Format('Dynamics mode is implemented only for 1- or 3-phase Storage2 Element. Storage2.%s has %d phases.', [name, Fnphases]), 5671);
                SolutionAbort := TRUE;
            end;

    {Add it into inj current array}
        for i := 1 to FnConds do
            Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));

    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorage2Obj.DoDynaModel(ActorID: Integer);
var
    DESSCurr: array[1..6] of Complex;  // Temporary biffer
    i: Integer;

begin
// do user written dynamics model

    with ActiveCircuit[ActorID].Solution do
    begin  // Just pass node voltages to ground and let dynamic model take care of it
        for i := 1 to FNconds do
            VTerminal^[i] := NodeV^[NodeRef^[i]];
        Storage2Vars.w_grid := TwoPi * Frequency;
    end;

    DynaModel.FCalc(Vterminal, @DESSCurr);

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        StickCurrInTerminalArray(ITerminal, Cnegate(DESSCurr[i]), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE, ActorID);
        StickCurrInTerminalArray(InjCurrent, DESSCurr[i], i);  // Put into Terminal array taking into account connection
    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorage2Obj.DoHarmonicMode(ActorID: Integer);

{Compute Injection Current Only when in harmonics mode}

{Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built}
{Vd is the fundamental frequency voltage behind Xd" for phase 1}

var
    i: Integer;
    E: Complex;
    Storage2Harmonic: Double;

begin

    ComputeVterminal(ActorID);

    with ActiveCircuit[ActorID].Solution do
    begin
        Storage2Harmonic := Frequency / Storage2Fundamental;
        if SpectrumObj <> NIL then
            E := CmulReal(SpectrumObj.GetMult(Storage2Harmonic), Storage2Vars.VThevHarm) // Get base harmonic magnitude
        else
            E := CZERO;

        RotatePhasorRad(E, Storage2Harmonic, Storage2Vars.ThetaHarm);  // Time shift by fundamental frequency phase shift
        for i := 1 to Fnphases do
        begin
            cBuffer[i] := E;
            if i < Fnphases then
                RotatePhasorDeg(E, Storage2Harmonic, -120.0);  // Assume 3-phase Storage2 element
        end;
    end;

   {Handle Wye Connection}
    if Connection = 0 then
        cbuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

   {Inj currents = Yprim (E) }
    YPrim.MVMult(InjCurrent, @cBuffer);

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorage2Obj.CalcVTerminalPhase(ActorID: Integer);

var
    i, j: Integer;

begin

{ Establish phase voltages and stick in Vterminal}
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

    Storage2SolutionCount := ActiveCircuit[ActorID].Solution.SolutionCount;

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
(*
PROCEDURE TStorage2Obj.CalcVTerminal;
{Put terminal voltages in an array}
Begin
   ComputeVTerminal;
   Storage2SolutionCount := ActiveCircuit[ActiveActor].Solution.SolutionCount;
End;
*)


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorage2Obj.CalcStorage2ModelContribution(ActorID: Integer);

// Calculates Storage2 element current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

begin
    set_ITerminalUpdated(FALSE, ActorID);
    with  ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
        if IsDynamicModel then
            DoDynamicMode(ActorID)
        else
        if IsHarmonicModel and (Frequency <> Fundamental) then
            DoHarmonicMode(ActorID)
        else
        begin
               //  compute currents and put into InjTemp array;
            case VoltageModel of
                1:
                    DoConstantPQStorage2Obj(ActorID);
                2:
                    DoConstantZStorage2Obj(ActorID);
                3:
                    DoUserModel(ActorID);
            else
                DoConstantPQStorage2Obj(ActorID);  // for now, until we implement the other models.
            end;
        end; {ELSE}
    end; {WITH}

   {When this is Done, ITerminal is up to date}

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorage2Obj.CalcInjCurrentArray(ActorID: Integer);
// Difference between currents in YPrim and total current
begin
      // Now Get Injection Currents
    if Storage2ObjSwitchOpen then
        ZeroInjCurrent
    else
        CalcStorage2ModelContribution(ActorID);
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorage2Obj.GetTerminalCurrents(Curr: pComplexArray; ActorID: Integer);

// Compute total Currents

begin
    with ActiveCircuit[ActorID].Solution do
    begin
        if IterminalSolutionCount[ActorID] <> ActiveCircuit[ActorID].Solution.SolutionCount then
        begin     // recalc the contribution
            if not Storage2ObjSwitchOpen then
                CalcStorage2ModelContribution(ActorID);  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr, ActorID);
    end;

    if (DebugTrace) then
        WriteTraceRecord('TotalCurrent', ActorID);

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TStorage2Obj.InjCurrents(ActorID: Integer): Integer;

begin
    with ActiveCircuit[ActorID].Solution do
    begin
        if LoadsNeedUpdating then
            SetNominalStorage2Output(ActorID); // Set the nominal kW, etc for the type of solution being Done

        CalcInjCurrentArray(ActorID);          // Difference between currents in YPrim and total terminal current

        if (DebugTrace) then
            WriteTraceRecord('Injection', ActorID);

         // Add into System Injection Current Array

        Result := inherited InjCurrents(ActorID);
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorage2Obj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);

// Gives the currents for the last solution performed

// Do not call SetNominalLoad, as that may change the load values

var
    i: Integer;

begin

    CalcInjCurrentArray(ActorID);  // Difference between currents in YPrim and total current

    try
   // Copy into buffer array
        for i := 1 to Yorder do
            Curr^[i] := InjCurrent^[i];

    except
        ON E: Exception do
            DoErrorMsg('Storage2 Object: "' + Name + '" in GetInjCurrents FUNCTION.',
                E.Message,
                'Current buffer not big enough.', 568);
    end;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TStorage2Obj.ResetRegisters;

var
    i: Integer;

begin
    for i := 1 to NumStorage2Registers do
        Registers[i] := 0.0;
    for i := 1 to NumStorage2Registers do
        Derivatives[i] := 0.0;
    FirstSampleAfterReset := TRUE;  // initialize for trapezoidal integration
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TStorage2Obj.Integrate(Reg: Integer; const Deriv: Double; const Interval: Double; ActorID: Integer);

begin
    if ActiveCircuit[ActorID].TrapezoidalIntegration then
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
procedure TStorage2Obj.TakeSample(ActorID: Integer);
// Update Energy from metered zone

var
    S: Complex;
    Smag: Double;
    HourValue: Double;

begin

// Compute energy in Storage2 element branch
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

        if (FState = STORE_DISCHARGING) or ActiveCircuit[ActorID].TrapezoidalIntegration then
        {Make sure we always integrate for Trapezoidal case
         Don't need to for Gen Off and normal integration}
            with ActiveCircuit[ActorID].Solution do
            begin
                if ActiveCircuit[ActorID].PositiveSequence then
                begin
                    S := CmulReal(S, 3.0);
                    Smag := 3.0 * Smag;
                end;
                Integrate(Reg_kWh, S.re, IntervalHrs, ActorID);   // Accumulate the power
                Integrate(Reg_kvarh, S.im, IntervalHrs, ActorID);
                SetDragHandRegister(Reg_MaxkW, abs(S.re));
                SetDragHandRegister(Reg_MaxkVA, Smag);
                Integrate(Reg_Hours, HourValue, IntervalHrs, ActorID);  // Accumulate Hours in operation
                Integrate(Reg_Price, S.re * ActiveCircuit[ActorID].PriceSignal * 0.001, IntervalHrs, ActorID);  // Accumulate Hours in operation
                FirstSampleAfterReset := FALSE;
            end;
    end;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.UpdateStorage2(ActorID: Integer);
{Update Storage2 levels}
begin

    with Storage2Vars do
    begin

        kWhBeforeUpdate := kWhStored;   // keep this for reporting change in Storage2 as a variable

      {Assume User model will take care of updating Storage2 in dynamics mode}
        if ActiveCircuit[ActorID].solution.IsDynamicModel and IsUserModel then
            Exit;

        with ActiveCircuit[ActorID].Solution do
            case FState of

                STORE_DISCHARGING:
                begin

                    kWhStored := kWhStored - (DCkW + kWIdlingLosses) / DischargeEff * IntervalHrs;
                    if kWhStored < kWhReserve then
                    begin
                        kWhStored := kWhReserve;
                        Fstate := STORE_IDLING;  // It's empty Turn it off
                        FstateChanged := TRUE;
                    end;
                end;

                STORE_CHARGING:
                begin

                    if (abs(DCkW) - kWIdlingLosses) >= 0 then // 99.9 % of the cases will fall here
                    begin
                        kWhStored := kWhStored + (abs(DCkW) - kWIdlingLosses) * ChargeEff * IntervalHrs;
                        if kWhStored > kWhRating then
                        begin
                            kWhStored := kWhRating;
                            Fstate := STORE_IDLING;  // It's full Turn it off
                            FstateChanged := TRUE;
                        end;
                    end
                    else   // Exceptional cases when the idling losses are higher than the DCkW such that the net effect is that the
                                 // the ideal Storage2 will discharge
                    begin
                        kWhStored := kWhStored + (abs(DCkW) - kWIdlingLosses) / DischargeEff * IntervalHrs;
                        if kWhStored < kWhReserve then
                        begin
                            kWhStored := kWhReserve;
                            Fstate := STORE_IDLING;  // It's empty Turn it off
                            FstateChanged := TRUE;
                        end;
                    end;

                end;

                STORE_IDLING: ;
            end;

    end;

      // the update is done at the end of a time step so have to force
      // a recalc of the Yprim for the next time step.  Else it will stay the same.
    if FstateChanged then
        YprimInvalid[ActorID] := TRUE;

end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.ComputeDCkW;
// Computes actual DCkW to Update Storage2 SOC
var

    coefGuess: TCoeff;
    coef: TCoeff;
    N_tentatives: Integer;
begin

    coefGuess[1] := 0.0;
    coefGuess[2] := 0.0;

    coef[1] := 1.0;
    coef[2] := 1.0;  // just a guess

    FDCkW := Power[1, ActiveActor].re * 0.001;  // Assume ideal inverter


    if not Assigned(InverterCurveObj) then
    begin
    // make sure sign is correct
        if (FState = STORE_IDLING) then
            FDCkW := abs(FDCkW) * -1
        else
            FDCkW := abs(FDCkW) * FState;
        Exit;
    end;


    N_tentatives := 0;
    while (coef[1] <> coefGuess[1]) and (coef[2] <> coefGuess[2]) or (N_tentatives > 9) do
    begin
        N_tentatives := N_tentatives + 1;
        coefGuess := InverterCurveObj.GetCoefficients(abs(FDCkW) / Storage2Vars.FkVArating);


        case FState of

            STORE_DISCHARGING:
                FDCkW := QuadSolver(coefGuess[1] / Storage2Vars.FkVArating, coefGuess[2], -1.0 * abs(Power[1, ActiveActor].re * 0.001));
            STORE_CHARGING,
            STORE_IDLING:
                FDCkW := abs(FDCkW) * coefGuess[2] / (1.0 - (coefGuess[1] * abs(FDCkW) / Storage2Vars.FkVArating));
        end;

      // Final coefficients
        coef := InverterCurveObj.GetCoefficients(abs(FDCkW) / Storage2Vars.FkVArating);
    end;

    // make sure sign is correct
    if (FState = STORE_IDLING) then
        FDCkW := abs(FDCkW) * -1
    else
        FDCkW := abs(FDCkW) * FState;

end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_PresentkW: Double;
begin
    Result := Pnominalperphase * 0.001 * Fnphases;
end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_DCkW: Double;
begin

    ComputeDCkW;
    Result := FDCkW;

end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_kWDesired: Double;
begin

    case FStateDesired of
        STORE_CHARGING:
            Result := -pctkWIn * Storage2Vars.kWRating / 100.0;
        STORE_DISCHARGING:
            Result := pctkWOut * Storage2Vars.kWRating / 100.0;
        STORE_IDLING:
            Result := 0.0;
    end;

end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_StateDesired(i: Integer);
begin

    FStateDesired := i;

end;

//-----------------------------------------------------------------------------

function TStorage2Obj.Get_kWTotalLosses: Double;
begin
    Result := kWIdlingLosses + kWInverterLosses + kWChDchLosses;
end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_InverterLosses: Double;
begin
    Result := 0.0;

    with Storage2Vars do
    begin
        case Storage2State of

            STORE_IDLING:
                Result := abs(Power[1, ActiveActor].re * 0.001) - abs(DCkW);
            STORE_CHARGING:
                Result := abs(Power[1, ActiveActor].re * 0.001) - abs(DCkW);
            STORE_DISCHARGING:
                Result := DCkW - abs(Power[1, ActiveActor].re * 0.001);
        end;
    end;
end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_kWIdlingLosses: Double;
begin

    if (FState = STORE_IDLING) then
    begin
        Result := abs(DCkW); // For consistency keeping with voltage variations
    end
    else
        Result := Pidling;
end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_kWChDchLosses: Double;
begin
    Result := 0.0;

    with Storage2Vars do
    begin
        case Storage2State of

            STORE_IDLING:
                Result := 0.0;

            STORE_CHARGING:
                if (abs(DCkW) - Pidling > 0) then
                    Result := (abs(DCkW) - Pidling) * (1.0 - 0.01 * pctChargeEff) // most cases will fall here
                else
                    Result := -1 * (abs(DCkW) - Pidling) * (1.0 / (0.01 * pctDischargeEff) - 1.0);             // exceptional cases when Pidling is higher than DCkW (net effect is that the ideal Storage2 will be discharged)

            STORE_DISCHARGING:
                Result := (DCkW + Pidling) * (1.0 / (0.01 * pctDischargeEff) - 1.0);
        end;
    end;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Update_EfficiencyFactor;
begin
    with Storage2Vars do
    begin
        if not Assigned(InverterCurveObj) then
            EffFactor := 1.0
        else
            EffFactor := InverterCurveObj.GetYValue(abs(DCkW) / FkVArating);
    end;
end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_PresentkV: Double;
begin
    Result := Storage2Vars.kVStorage2Base;
end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_Presentkvar: Double;
begin
    Result := Qnominalperphase * 0.001 * Fnphases;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i, idx: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            idx := PropertyIdxMap[i];
            case idx of
                propUSERDATA:
                    Writeln(F, '~ ', PropertyName^[i], '=(', PropertyValue[idx], ')');
                propDynaData:
                    Writeln(F, '~ ', PropertyName^[i], '=(', PropertyValue[idx], ')');
            else
                Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[idx]);
            end;
        end;

    Writeln(F);
end;


//----------------------------------------------------------------------------

procedure TStorage2Obj.InitHarmonics(ActorID: Integer);

// This routine makes a thevenin equivalent behis the reactance spec'd in %R and %X

var
    E, Va: complex;

begin
    YprimInvalid[ActorID] := TRUE;  // Force rebuild of YPrims
    Storage2Fundamental := ActiveCircuit[ActorID].Solution.Frequency;  // Whatever the frequency is when we enter here.

    Yeq := Cinv(Cmplx(Storage2Vars.RThev, Storage2Vars.XThev));      // used for current calcs  Always L-N

     {Compute reference Thevinen voltage from phase 1 current}

    if FState = STORE_DISCHARGING then
    begin
        ComputeIterminal(ActorID);  // Get present value of current

        with ActiveCircuit[ActorID].solution do
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

        E := Csub(Va, Cmul(Iterminal^[1], cmplx(Storage2Vars.Rthev, Storage2Vars.Xthev)));
        Storage2Vars.Vthevharm := Cabs(E);   // establish base mag and angle
        Storage2Vars.ThetaHarm := Cang(E);
    end
    else
    begin
        Storage2Vars.Vthevharm := 0.0;
        Storage2Vars.ThetaHarm := 0.0;
    end;
end;


//----------------------------------------------------------------------------
procedure TStorage2Obj.InitStateVars(ActorID: Integer);

// for going into dynamics mode
var
    VNeut: Complex;
    VThevPolar: Polar;
    i: Integer;
    V012,
    I012: array[0..2] of Complex;
    Vabc: array[1..3] of Complex;


begin

    YprimInvalid[ActorID] := TRUE;  // Force rebuild of YPrims

    with Storage2Vars do
    begin
        ZThev := Cmplx(RThev, XThev);
        Yeq := Cinv(ZThev);  // used to init state vars
    end;


    if DynaModel.Exists then   // Checks existence and selects
    begin
        ComputeIterminal(ActorID);
        ComputeVterminal(ActorID);
        with Storage2Vars do
        begin
            NumPhases := Fnphases;
            NumConductors := Fnconds;
            w_grid := twopi * ActiveCircuit[ActorID].Solution.Frequency;
        end;
        DynaModel.FInit(Vterminal, Iterminal);
    end

    else
    begin

     {Compute nominal Positive sequence voltage behind equivalent filter impedance}

        if FState = STORE_DISCHARGING then
            with ActiveCircuit[ActorID].Solution do
            begin
                ComputeIterminal(ActorID);

                if FnPhases = 3 then
                begin
                    Phase2SymComp(ITerminal, @I012);
                // Voltage behind Xdp  (transient reactance), volts
                    case Connection of
                        0:
                            Vneut := NodeV^[NodeRef^[Fnconds]]
                    else
                        Vneut := CZERO;
                    end;

                    for i := 1 to FNphases do
                        Vabc[i] := NodeV^[NodeRef^[i]];   // Wye Voltage

                    Phase2SymComp(@Vabc, @V012);
                    with Storage2Vars do
                    begin
                        Vthev := Csub(V012[1], Cmul(I012[1], ZThev));    // Pos sequence
                        VThevPolar := cToPolar(VThev);
                        VThevMag := VThevPolar.mag;
                        Theta := VThevPolar.ang;  // Initial phase angle
                    end;
                end
                else
                begin   // Single-phase Element
                    for i := 1 to Fnconds do
                        Vabc[i] := NodeV^[NodeRef^[i]];
                    with Storage2Vars do
                    begin
                        Vthev := Csub(VDiff(NodeRef^[1], NodeRef^[2]), Cmul(ITerminal^[1], ZThev));    // Pos sequence
                        VThevPolar := cToPolar(VThev);
                        VThevMag := VThevPolar.mag;
                        Theta := VThevPolar.ang;  // Initial phase angle
                    end;

                end;
            end;
    end;

end;

//----------------------------------------------------------------------------
procedure TStorage2Obj.IntegrateStates(ActorID: Integer);

// dynamics mode integration routine

var
    TracePower: Complex;

begin
   // Compute Derivatives and Then integrate

    ComputeIterminal(ActorID);

    if Dynamodel.Exists then   // Checks for existence and Selects

        DynaModel.Integrate

    else

        with ActiveCircuit[ActorID].Solution, Storage2Vars do
        begin

            with Storage2Vars do
                if (Dynavars.IterationFlag = 0) then
                begin {First iteration of new time step}
//****          ThetaHistory := Theta + 0.5*h*dTheta;
//****          SpeedHistory := Speed + 0.5*h*dSpeed;
                end;

      // Compute shaft dynamics
            TracePower := TerminalPowerIn(Vterminal, Iterminal, FnPhases);

//****      dSpeed := (Pshaft + TracePower.re - D*Speed) / Mmass;
//      dSpeed := (Torque + TerminalPowerIn(Vtemp,Itemp,FnPhases).re/Speed) / (Mmass);
//****      dTheta  := Speed ;

     // Trapezoidal method
            with Storage2Vars do
            begin
//****       Speed := SpeedHistory + 0.5*h*dSpeed;
//****       Theta := ThetaHistory + 0.5*h*dTheta;
            end;

   // Write Dynamics Trace Record
            if DebugTrace then
            begin
                Append(TraceFile);
                Write(TraceFile, Format('t=%-.5g ', [Dynavars.t]));
                Write(TraceFile, Format(' Flag=%d ', [Dynavars.Iterationflag]));
                Writeln(TraceFile);
                CloseFile(TraceFile);
            end;

        end;

end;

//----------------------------------------------------------------------------
function TStorage2Obj.InterpretState(const S: String): Integer;
begin
    case LowerCase(S)[1] of
        'c':
            Result := STORE_CHARGING;
        'd':
            Result := STORE_DISCHARGING;
    else
        Result := STORE_IDLING;
    end;
end;

{ apparently for debugging only
//----------------------------------------------------------------------------
Function TStorage2Obj.StateToStr:String;
Begin
      CASE FState of
          STORE_CHARGING: Result := 'Charging';
          STORE_IDLING: Result := 'Idling';
          STORE_DISCHARGING: Result := 'Discharging';
      END;
End;
}

//----------------------------------------------------------------------------
function TStorage2Obj.Get_Variable(i: Integer): Double;
{Return variables one at a time}

var
    N, k: Integer;

begin
    Result := -9999.99;  // error return value; no state vars
    if i < 1 then
        Exit;
    // for now, report kWhstored and mode
    with Storage2Vars do
        case i of
            1:
                Result := kWhStored;
            2:
                Result := FState;
            3:
                if not (FState = STORE_DISCHARGING) then
                    Result := 0.0
                else
                    Result := abs(Power[1, ActiveActor].re * 0.001);
            4:
                if (FState = STORE_CHARGING) or (FState = STORE_IDLING) then
                    Result := abs(Power[1, ActiveActor].re * 0.001)
                else
                    Result := 0;
            5:
                Result := -1 * Power[1, ActiveActor].im * 0.001;
            6:
                Result := DCkW;
            7:
                Result := kWTotalLosses; {Present kW charge or discharge loss incl idle losses}
            8:
                Result := kWInverterLosses; {Inverter Losses}
            9:
                Result := kWIdlingLosses; {Present kW Idling Loss}
            10:
                Result := kWChDchLosses;  // Charge/Discharge Losses
            11:
                Result := kWhStored - kWhBeforeUpdate;
            12:
            begin
                Update_EfficiencyFactor;
                Result := EffFactor;  //Old: Result    := Get_EfficiencyFactor;
            end;
            13:
                if (FInverterON) then
                    Result := 1.0
                else
                    Result := 0.0;
            14:
                Result := Vreg;
            15:
                Result := Vavg;
            16:
                Result := VVOperation;
            17:
                Result := VWOperation;
            18:
                Result := DRCOperation;
            19:
                Result := VVDRCOperation;
            20:
                Result := WPOperation;
            21:
                Result := WVOperation;
            22:
                Result := Get_kWDesired;
            23:
                if not (VWMode) then
                    Result := 9999
                else
                    Result := kWRequested;
            24:
                Result := pctkWrated * kWrating;
            25:
                if (kVA_exceeded) then
                    Result := 1.0
                else
                    Result := 0.0;

        else
        begin
            if UserModel.Exists then   // Checks for existence and Selects
            begin
                N := UserModel.FNumVars;
                k := (i - NumStorage2Variables);
                if k <= N then
                begin
                    Result := UserModel.FGetVariable(k);
                    Exit;
                end;
            end;
            if DynaModel.Exists then  // Checks for existence and Selects
            begin
                N := DynaModel.FNumVars;
                k := (i - NumStorage2Variables);
                if k <= N then
                begin
                    Result := DynaModel.FGetVariable(k);
                    Exit;
                end;
            end;
        end;
        end;
end;

//----------------------------------------------------------------------------
procedure TStorage2Obj.Set_Variable(i: Integer; Value: Double);
var
    N, k: Integer;

begin
    if i < 1 then
        Exit;  // No variables to set

    with Storage2Vars do
        case i of
            1:
                kWhStored := Value;
            2:
                Fstate := Trunc(Value);
            3..13: ; {Do Nothing; read only}
            14:
                Vreg := Value;
            15:
                Vavg := Value;
            16:
                VVOperation := Value;
            17:
                VWOperation := Value;
            18:
                DRCOperation := Value;
            19:
                VVDRCOperation := Value;
            20:
                WPOperation := Value;
            21:
                WVOperation := Value;
            22..25: ; {Do Nothing; read only}

        else
        begin
            if UserModel.Exists then    // Checks for existence and Selects
            begin
                N := UserModel.FNumVars;
                k := (i - NumStorage2Variables);
                if k <= N then
                begin
                    UserModel.FSetVariable(k, Value);
                    Exit;
                end;
            end;
            if DynaModel.Exists then     // Checks for existence and Selects
            begin
                N := DynaModel.FNumVars;
                k := (i - NumStorage2Variables);
                if k <= N then
                begin
                    DynaModel.FSetVariable(k, Value);
                    Exit;
                end;
            end;
        end;
        end;

end;

//----------------------------------------------------------------------------
procedure TStorage2Obj.GetAllVariables(States: pDoubleArray);

var
    i{, N}: Integer;
begin
    for i := 1 to NumStorage2Variables do
        States^[i] := Variable[i];

    if UserModel.Exists then
    begin    // Checks for existence and Selects
        {N := UserModel.FNumVars;}
        UserModel.FGetAllVars(@States^[NumStorage2Variables + 1]);
    end;
    if DynaModel.Exists then
    begin    // Checks for existence and Selects
        {N := UserModel.FNumVars;}
        DynaModel.FGetAllVars(@States^[NumStorage2Variables + 1]);
    end;

end;

//----------------------------------------------------------------------------

function TStorage2Obj.NumVariables: Integer;
begin
    Result := NumStorage2Variables;

     // Exists does a check and then does a Select
    if UserModel.Exists then
        Result := Result + UserModel.FNumVars;
    if DynaModel.Exists then
        Result := Result + DynaModel.FNumVars;
end;

//----------------------------------------------------------------------------

function TStorage2Obj.VariableName(i: Integer): String;

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
//          3:Result  := 'Pnom';
//          4:Result  := 'Qnom';
        3:
            Result := 'kWOut';
        4:
            Result := 'kWIn';
        5:
            Result := 'kvarOut';
        6:
            Result := 'DCkW';
        7:
            Result := 'kWTotalLosses';
        8:
            Result := 'kWInvLosses';
        9:
            Result := 'kWIdlingLosses';
        10:
            Result := 'kWChDchLosses';
        11:
            Result := 'kWh Chng';
        12:
            Result := 'InvEff';
        13:
            Result := 'InverterON';
        14:
            Result := 'Vref';
        15:
            Result := 'Vavg (DRC)';
        16:
            Result := 'VV Oper';
        17:
            Result := 'VW Oper';
        18:
            Result := 'DRC Oper';
        19:
            Result := 'VV_DRC Oper';
        20:
            Result := 'kWDesired';
        21:
            Result := 'kW VW Limit';
        22:
            Result := 'Limit kWOut Function';
        23:
            Result := 'kVA Exceeded';


    else
    begin
        if UserModel.Exists then    // Checks for existence and Selects
        begin
            pName := @Buff;
            n := UserModel.FNumVars;
            i2 := i - NumStorage2Variables;
            if i2 <= n then
            begin
                UserModel.FGetVarName(i2, pName, BuffSize);
                Result := String(pName);
                Exit;
            end;
        end;
        if DynaModel.Exists then   // Checks for existence and Selects
        begin
            pName := @Buff;
            n := DynaModel.FNumVars;
            i2 := i - NumStorage2Variables; // Relative index
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

//----------------------------------------------------------------------------

procedure TStorage2Obj.MakePosSequence(ActorID: Integer);

var
    S: String;
    V: Double;

begin

    S := 'Phases=1 conn=wye';

  // Make sure voltage is line-neutral
    if (Fnphases > 1) or (connection <> 0) then
        V := Storage2Vars.kVStorage2Base / SQRT3
    else
        V := Storage2Vars.kVStorage2Base;

    S := S + Format(' kV=%-.5g', [V]);

    if Fnphases > 1 then
    begin
        S := S + Format(' kWrating=%-.5g  PF=%-.5g', [Storage2Vars.kWrating / Fnphases, PFNominal]);
    end;

    Parser[ActorID].CmdString := S;
    Edit(ActorID);

    inherited;   // write out other properties
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_ConductorClosed(Index: Integer; ActorID: Integer;
    Value: Boolean);
begin
    inherited;

 // Just turn Storage2 element on or off;

    if Value then
        Storage2ObjSwitchOpen := FALSE
    else
        Storage2ObjSwitchOpen := TRUE;

end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_InverterON: Boolean;
begin
    if FInverterON then
        Result := TRUE
    else
        Result := FALSE;

end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.kWOut_Calc;
var
    limitkWpct: Double;

begin
    with Storage2Vars do
    begin

        FVWStateRequested := FALSE;

        if FState = STORE_DISCHARGING then
            limitkWpct := kWrating * FpctkWrated
        else
            limitkWpct := kWrating * FpctkWrated * -1;

//          if VWmode and (FState = STORE_DISCHARGING) then if (abs(kwRequested) < abs(limitkWpct)) then limitkWpct :=  kwRequested * sign(kW_Out);
          // VW works only if element is not in idling state.
          // When the VW is working in the 'limiting' region, kWRequested will be positive.
          // When in 'requesting' region, it will be negative.
        if VWmode and not (FState = STORE_IDLING) then
        begin

            if (kWRequested >= 0.0) and (abs(kwRequested) < abs(limitkWpct)) then  // Apply VW limit
            begin
                if FState = STORE_DISCHARGING then
                    limitkWpct := kwRequested
                else
                    limitkWpct := -1 * kwRequested;
            end
            else
            if kWRequested < 0.0 then // IEEE 1547 Requesting Region (not fully implemented)
            begin
                if FState = STORE_DISCHARGING then
                begin
                    if (kWhStored < kWhRating) then
                    begin  // let it charge only if enough not fully charged
                        FState := STORE_CHARGING;
                        kW_out := kWRequested;
                    end
                    else
                    begin
                        FState := STORE_IDLING;
                        kW_out := -kWOutIdling;
                    end;
                end
                else  // Charging
                begin
                    if (kWhStored > kWhReserve) then
                    begin  // let it charge only if enough not fully charged
                        Fstate := STORE_DISCHARGING;
                        kW_out := -1 * kWRequested;
                    end
                    else
                    begin
                        FState := STORE_IDLING;
                        kW_out := -kWOutIdling;
                    end;


                end;
                FStateChanged := TRUE;
                FVWStateRequested := TRUE;

                // Update limitkWpct because state might have been changed
                if FState = STORE_DISCHARGING then
                    limitkWpct := kWrating * FpctkWrated
                else
                    limitkWpct := kWrating * FpctkWrated * -1;

            end;

        end;

        if (limitkWpct > 0) and (kW_Out > limitkWpct) then
            kW_Out := limitkWpct
        else
        if (limitkWpct < 0) and (kW_Out < limitkWpct) then
            kW_Out := limitkWpct;

    end;
end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_Varmode: Integer;
begin
    Result := FvarMode;
end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_VWmode: Boolean;
begin
    if FVWmode then
        Result := TRUE
    else
        Result := FALSE;    // TRUE if volt-watt mode
                                                              //  engaged from InvControl (not ExpControl)
end;

// ============================================================Get_WPmode===============================
function TStorage2Obj.Get_WPmode: Boolean;
begin
    if FWPmode then
        Result := TRUE
    else
        Result := FALSE;                                                               //  engaged from InvControl (not ExpControl)
end;

// ============================================================Get_WVmode===============================
function TStorage2Obj.Get_WVmode: Boolean;
begin
    if FWVmode then
        Result := TRUE
    else
        Result := FALSE;                                                               //  engaged from InvControl (not ExpControl)
end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_VVmode: Boolean;
begin
    if FVVmode then
        Result := TRUE
    else
        Result := FALSE;
end;


//----------------------------------------------------------------------------

function TStorage2Obj.Get_DRCmode: Boolean;
begin
    if FDRCmode then
        Result := TRUE
    else
        Result := FALSE;
end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_CutOutkWAC: Double;
begin
    Result := FCutOutkWAC;
end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_CutInkWAC: Double;
begin
    Result := FCutInkWAC;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_pctkWOut(const Value: Double);
begin
    FpctkWOut := Value;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_pctkWIn(const Value: Double);
begin
    FpctkWIn := Value;
end;

//----------------------------------------------------------------------------

// ===========================================================================================
procedure TStorage2Obj.Set_WVmode(const Value: Boolean);
begin
    FWVmode := Value;
end;


// ===========================================================================================
procedure TStorage2Obj.Set_WPmode(const Value: Boolean);
begin
    FWPmode := Value;
end;

procedure TStorage2Obj.Set_kW(const Value: Double);
begin
    if Value > 0 then
    begin
        FState := STORE_DISCHARGING;
        FpctkWOut := Value / Storage2Vars.kWRating * 100.0;
    end
    else
    if Value < 0 then
    begin
        FState := STORE_CHARGING;
        FpctkWIn := abs(Value) / Storage2Vars.kWRating * 100.0;
    end
    else
    begin
        FState := STORE_IDLING;
    end;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_Maxkvar(const Value: Double);
begin
    Storage2Vars.Fkvarlimit := Value;
    PropertyValue[propkvarLimit] := Format('%-g', [Storage2Vars.Fkvarlimit]);
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_Maxkvarneg(const Value: Double);
begin
    Storage2Vars.Fkvarlimitneg := Value;
    PropertyValue[propkvarLimitneg] := Format('%-g', [Storage2Vars.Fkvarlimitneg]);
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_kVARating(const Value: Double);
begin
    Storage2Vars.FkVARating := Value;
    PropertyValue[propKVA] := Format('%-g', [Storage2Vars.FkVArating]);
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_PowerFactor(const Value: Double);
begin
    PFNominal := Value;
    varMode := VARMODEPF;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_Varmode(const Value: Integer);
begin
    FvarMode := Value;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_VWmode(const Value: Boolean);
begin
    FVWmode := Value;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_VVmode(const Value: Boolean);
begin
    FVVmode := Value;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_DRCmode(const Value: Boolean);
begin
    FDRCmode := Value;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_PresentkV(const Value: Double);
begin
    Storage2Vars.kVStorage2Base := Value;
    case FNphases of
        2, 3:
            VBase := Storage2Vars.kVStorage2Base * InvSQRT3x1000;
    else
        VBase := Storage2Vars.kVStorage2Base * 1000.0;
    end;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_kvarRequested(const Value: Double);
begin
    FkvarRequested := Value;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_pf_wp_nominal(const Value: Double);
begin
    Fpf_wp_nominal := Value;
end;

procedure TStorage2Obj.Set_kWRequested(const Value: Double);
begin
    FkWRequested := Value;
end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_kW: Double;
begin

    case Fstate of
        STORE_CHARGING:
            Result := -pctkWIn * Storage2Vars.kWRating / 100.0;
        STORE_DISCHARGING:
            Result := pctkWOut * Storage2Vars.kWRating / 100.0;
        STORE_IDLING:
            Result := -kWOutIdling;
    end;

end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_kWRequested: Double;
begin
    Result := FkWRequested;
end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_kvarRequested: Double;
begin
    Result := FkvarRequested;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_VarFollowInverter(const Value: Boolean);
begin
    FVarFollowInverter := Value;
end;

//----------------------------------------------------------------------------

function TStorage2Obj.Get_VarFollowInverter: Boolean;
begin
    if FVarFollowInverter then
        Result := TRUE
    else
        Result := FALSE;

end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_pctkWrated(const Value: Double);
begin
    Storage2Vars.FpctkWrated := Value;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_InverterON(const Value: Boolean);
begin
    FInverterON := Value;
end;

//----------------------------------------------------------------------------

procedure TStorage2Obj.Set_Storage2State(const Value: Integer);
var
    SavedState: Integer;
begin
    SavedState := Fstate;

     // Decline if Storage2 is at its limits ; set to idling instead

    with Storage2Vars do
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
                    Fstate := STORE_IDLING;  // not enough Storage2 to discharge
            end;
        else
            Fstate := STORE_IDLING;
        end;

    if SavedState <> Fstate then
        FStateChanged := TRUE;

     //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, ---State Set To %s', [ActiveCircuit[ActiveActor].Solution.dblHour, StateToStr ]));
end;
//----------------------------------------------------------------------------

procedure TStorage2Obj.SetDragHandRegister(Reg: Integer; const Value: Double);
begin
    if Value > Registers[reg] then
        Registers[Reg] := Value;
end;

//----------------------------------------------------------------------------
initialization

    CDOUBLEONE := CMPLX(1.0, 1.0);

end.
