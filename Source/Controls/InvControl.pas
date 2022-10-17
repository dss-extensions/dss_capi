unit InvControl;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2022,  Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$HINTS OFF}
{
  A InvControl is a control element that is connected to a terminal of another
  circuit element and sends kW and/or kvar signals to a set of PVSystem objects it controls

  A InvControl is defined by a New command:

  New InvControl.Name=myname PVSystemList = (pvsystem1  PVSystem ...)

Notes:
  WGS (11/26/2012): Using dynamic arrays for many private variables in this unit.
  Although dynamic arrays begin at 0 (by definition in Delphi),
  this unit is using 1 to numberelements in all for loops - the 0th
  element is un-used (except for Strings) in this unit.
  All dynamic arrays are set to length numberelements+1 in the appropriate dimension.
  All dynamic arrays are Finalize'd in the destroy procedure.

  // Updated 9/24/2015 to allow for simultaneous modes and additional functionality
}

interface

uses

  {$IFDEF FPC}gqueue{$else}System.Generics.Collections{$ENDIF},
  Command, ControlClass, ControlElem, CktElement, DSSClass, bus, PCElement,PVSystem, Storage, StorageVars, Arraydef, ucomplex,
  utilities, XYcurve, Dynamics, PointerList, Classes, StrUtils, MathUtil;

type

  ERateofChangeMode = (
    INACTIVE,
    LPF,
    RISEFALL
  );



  TRollAvgWindow = class(TObject)

    private
      sample                          : TQueue<Double>;
      sampletime                      : TQueue<Double>;
      runningsumsample                : Double;
      runningsumsampletime            : Double;
      bufferlength                    : Integer;
      bufferfull                      : Boolean;
      function Get_AvgVal             : Double;
      function Get_AccumSec           : Double;

      procedure Set_BuffLength(const Value: Integer);

    public
      constructor Create();
      destructor Destroy; override;
      procedure Add(IncomingSampleValue: Double;IncomingSampleTime: Double;VAvgWindowLengthSec:Double);


      Property AvgVal      :Double  Read Get_AvgVal;
      Property AccumSec    :Double  Read Get_AccumSec;
      Property BuffLength  :Integer Read bufferlength Write Set_BuffLength;
  end;

  TInvVars = Packed Record
    CondOffset              : Integer; // Offset for monitored terminal
    cBuffer                 : Array of Complex;    // Complex array buffer
    ControlledElement       : TPCElement;
    FAvgpVpuPrior           : Double;
    FAvgpDRCVpuPrior        : Double;
    FPresentVpu             : Double;
    FPresentDRCVpu          : Double;
    NPhasesDER              : Integer;
    NCondsDER               : Integer;
    FPendingChange          : Integer;
    QDesiredVV              : Double; // volt-var new set-point
    QDesiredWP              : Double; // watt-pf new set-point
    QDesiredWV              : Double; // watt-var new set-point
    QDesiredAVR             : Double;
    QOld                    : Double;
    QOldVV                  : Double;
    QOldAVR                 : Double;
    QOldDRC                 : Double;
    QOldVVDRC               : Double;
    QDesiredDRC             : Double; //dynamic reactive power new set-point
    QDesiredVVDRC           : Double;
    QHeadRoom               : Double;
    QHeadRoomNeg            : Double;
    PBase                   : Double;
    Qoutputpu               : Double;
    QoutputVVpu             : Double;
    QoutputDRCpu            : Double;
    QoutputVVDRCpu          : Double;
    QoutputAVRpu            : Double;
    QDesireEndpu            : Double;  // Q value used in the convergency algorithm
    QDesireVVpu             : Double; // Q desired caculated in volt-var curve
    QDesireWPpu             : Double; // Q desired caculated in watt-pf curve
    QDesireWVpu             : Double; // Q desired caculated in watt-var curve
    QDesireDRCpu            : Double;  // Q desired from the DRC equation
    QDesireAVRpu            : Double;
    QDesireLimitedpu        : Double; // Calculates possible Q considering kVA (watt priority) and kvarlimit limits
    QDesireOptionpu         : Double; // Calculates Q Limit considering LPF and RF
    PLimitEndpu             : Double;
    PLimitVWpu              : Double;
    PLimitLimitedpu         : Double;
    PLimitOptionpu          : Double;
    deltaVDynReac           : Double;
    PLimitVW                : Double;
    POldVWpu                : Double;
    FdeltaQFactor           : Double;
    FdeltaPFactor           : Double;
    DeltaV_old              : Double;
    FVpuSolution            : array of Double;
    FRollAvgWindow          : TRollAvgWindow;
    FDRCRollAvgWindowpu     : Double;
    FDRCRollAvgWindow       : TRollAvgWindow;
    priorRollAvgWindow      : Double;
    priorDRCRollAvgWindow   : Double;
    FlagChangeCurve         : Boolean;
    FActiveVVCurve          : Integer;
    FPriorWattspu           : Double;
    FPriorwatts             : Double;
    FPriorPLimitOptionpu    : Double;
    FPriorQDesireOptionpu   : Double;
    kW_out_desiredpu        : Double;
    kW_out_desired          : Double;
    {Variables of functions that CONTROL reactive power}
    FPriorvarspu            : Double;
    FPriorvars              : Double;
     // Active power
    FFlagVWOperates         : Boolean;  // Flag enabled when volt-watt Pdesired is less than 1. So volt-watt algorithm starts to work
    {Flags used to record function states. They are interval variables of DER}
    FVVOperation            : Double;
    FVWOperation            : Double;
    FDRCOperation           : Double;
    FVVDRCOperation         : Double;
    FWPOperation            : Double;
    FWVOperation            : Double;
    FAVROperation           : Double;
    {Variables of functions that LIMIT active power}
    {Variables of DER element}
    FVBase                  : Double;
    FVarFollowInverter      : Boolean;
    FInverterON             : Boolean;
    FpresentkW              : Double;
    FkVARating              : Double;
    Fpresentkvar            : Double;
    FkvarLimit              : Double;
    FkvarLimitNeg           : Double;
    FCurrentkvarLimit       : Double;
    FCurrentkvarLimitNeg    : Double;
    FDCkWRated              : Double;  // Pmpp for PVSystem, kWRated for Storage
    FpctDCkWRated           : Double;  // pctPmpp for PVSystem, pctkWRated for Storage
    FEffFactor              : Double;
    FDCkW                   : Double;  // PanelkW for PVSystem, DCkW for Storage
    FPPriority              : Boolean;
    // Active voltage regulation (AVR)
    DQDV                   : Double;
    Fv_setpointLimited     : Double;
    FAvgpAVRVpuPrior       : Double;
    PICtrl                 : TPICtrl;
  end;


  TInvControl = class(TControlClass)

    private
      XY_CurveClass: TDSSClass;

    protected
      procedure DefineProperties;
      function MakeLike(const InvControlName:String):Integer;Override;

    public
      constructor Create;
      destructor Destroy; override;

      function Edit(ActorID : Integer):Integer; override;     // uses global parser
      function NewObject(const ObjName:String):Integer; override;
      function GetXYCurve(Const CurveName: String; InvControlMode: Integer): TXYcurveObj;
      procedure UpdateAll(ActorID : integer);
   end;

  TInvControlObj = class(TControlElem)

    private
      ControlMode             : Integer;
      CombiControlMode        : Integer;
      ControlActionHandle     : Integer;

      MonitoredElement        : TDSSCktElement;  // First DER element for now (the first element from ControlledElement TPointerList)

      {Variables for voltages}
      FVreg                   : Double;


      FVpuSolutionIdx         : Integer;

      {Variables for convergence process}
      FdeltaQ_factor          : Double;
      FdeltaP_factor          : Double;

      FVoltageChangeTolerance : Double;
      FVarChangeTolerance     : Double;
      FActivePChangeTolerance : Double;

      // Reactive power
      FDERPointerList         : PointerList.TPointerList;
      FListSize               : Integer;
      FDERNameList            : TStringList;

      {Variables for monitored Bus/buses}
      FMonBusesNameList       : TStringList;
      FMonBusesPhase          : Integer;
      FUsingMonBuses          : Boolean;
      FMonBuses               : Array of String;
      FMonBusesIndex          : Integer;
      FMonBusesVbase          : pDoubleArray;
      FMonBusesNodes          : Array of Array of Integer;

      {Variables for LPF and RF options}
      RateofChangeMode        : ERateofChangeMode;
      FLPFTau                 : Double;
      FRiseFallLimit          : Double;

      {Variables of the smart inverter functions}
      FVoltage_CurveX_ref     : Integer;  // valid values are 0: = Vref (rated), 1:= avg
      FReacPower_ref          : String;
      FVoltwattYAxis          : Integer; // 1 = %Pmpp, 0 = %Available power

      // volt-var
      Fvvc_curve_size         : Integer; // length of the individual curve
      Fvvc_curve              : TXYcurveObj;
      Fvvc_curvename          : String;
      Fvvc_curveOffset        : Double;
      Fvvc_curve2             : TXYcurveObj;

      FVAvgWindowLengthSec    : Double; // rolling average window length in seconds

      FRollAvgWindowLength    : Integer;

      FRollAvgWindowLengthIntervalUnit  : String;

      // watt-pf
      Fwattpf_curve_size    : Integer;
      Fwattpf_curve         : TXYcurveObj;
      Fwattpf_curvename     : String;
      pf_wp_nominal         : Double;

      // watt-var
      Fwattvar_curve_size   : Integer;
      Fwattvar_curve        : TXYcurveObj;
      Fwattvar_curvename    : String;

      // DRC
      FDbVMin                 : Double;
      FDbVMax                 : Double;
      FArGraLowV              : Double;
      FArGraHiV               : Double;

      FDRCRollAvgWindowLength : Integer;
      FDRCRollAvgWindowLengthIntervalUnit : String;
      FDRCVAvgWindowLengthSec : Double; // rolling average window length in seconds

      // volt-watt
      Fvoltwatt_curve_size    : Integer;
      Fvoltwatt_curve         : TXYcurveObj;
      Fvoltwatt_curvename     : String;

      // volt-watt (charging)
      FvoltwattCH_curve_size  : Integer;
      FvoltwattCH_curve       : TXYcurveObj;
      FvoltwattCH_curvename   : String;

      // Controller model
      CtrlModel               : Integer;   // To differentiate control methods.
      {Others}
      CtrlVars                : array of TInvVars;
      Fv_setpoint             : Double;
      {Functions and Procedures}
      procedure   Set_PendingChange(Value: Integer;DevIndex: Integer);
      function    Get_PendingChange(DevIndex: Integer):Integer;
      function    InterpretAvgVWindowLen(const s:string):Integer;
      function    InterpretDRCAvgVWindowLen(const s:string):Integer;
      function    ReturnElementsList:String;
      procedure   UpdateInvControl(i:integer; ActorID : Integer);
      procedure   UpdateDERParameters(i: Integer);
      procedure   CalcVoltWatt_watts(j: Integer; ActorID : Integer);
      procedure   CalcQVVcurve_desiredpu(j: Integer; ActorID : Integer);
      procedure   CalcQWPcurve_desiredpu(j: Integer; ActorID : Integer);
      procedure   CalcQWVcurve_desiredpu(j: Integer; ActorID : Integer);
      procedure   CalcQDRC_desiredpu(j: Integer; ActorID : Integer);
      procedure   CalcQAVR_desiredpu(j: Integer; ActorID : Integer);
      procedure   Check_Qlimits(j: Integer; Q: Double; ActorID : Integer);
      procedure   Check_Qlimits_WV(j: Integer; Q: Double; ActorID : Integer);
      procedure   Calc_PQ_WV(j: Integer; ActorID : Integer);
      procedure   Calc_QHeadRoom(j: Integer; ActorID : Integer);
      procedure   CalcVoltVar_vars(j: Integer; ActorID : Integer);
      procedure   CalcAVR_vars(j: Integer; ActorID : Integer);
      procedure   CalcWATTPF_vars(j: Integer; ActorID : Integer);
      procedure   CalcWATTVAR_vars(j: Integer; ActorID : Integer);
      procedure   CalcDRC_vars(j: Integer; ActorID : Integer);
      procedure   CalcVVDRC_vars(j: Integer; ActorID : Integer);
      procedure   CalcLPF(m: Integer; powertype: String; LPF_desiredpu: Double; ActorID: Integer);
      procedure   CalcRF(m: Integer; powertype: String; RF_desiredpu: Double; ActorID: Integer);
      procedure   Calc_PBase(j: Integer; ActorID : Integer);
      procedure   Check_Plimits(j: Integer; P:Double; ActorID : Integer);
      procedure   CalcPVWcurve_limitpu(j: Integer; ActorID : Integer);
      procedure   GetmonVoltage(ActorID : Integer; var Vpresent: Double; i: Integer; BasekV: Double);
      procedure   Change_deltaQ_factor(ActorID : Integer; j: Integer);
      procedure   Change_deltaP_factor(ActorID : Integer; j: Integer);

    protected

      procedure Set_Enabled(Value:Boolean);Override;

    public

      {$IFNDEF FPC}
      MyMemoryManagerState: System.TMemoryManagerState;
      {$ENDIF}

      constructor Create(ParClass:TDSSClass; const InvControlName:String);
      destructor  Destroy; override;

      procedure   MakePosSequence(ActorID : Integer); Override;  // Make a positive Sequence Model
      procedure   RecalcElementData(ActorID : Integer); Override;
      procedure   CalcYPrim(ActorID : Integer); Override;    // Always Zero for a InvControl

      // Sample control quantities and set action times in Control Queue
      procedure   Sample(ActorID : Integer);  Override;

      // do the action that is pending from last sample
      procedure   DoPendingAction(Const Code, ProxyHdl:Integer;ActorID : Integer); Override;

      procedure   Reset(ActorID : Integer); Override;  // Reset to initial defined state

      procedure   GetCurrents(Curr: pComplexArray; ActorID : Integer); Override; // Get present value of terminal Curr
      procedure   GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override;   // Returns Injection currents


      procedure   InitPropertyValues(ArrayOffset:Integer);Override;
      procedure   DumpProperties(Var F:TextFile; Complete:Boolean);Override;


      function    MakeDERList:Boolean;
      function    GetPropertyValue(Index:Integer):String;Override;

      Property    PendingChange[DevIndex: Integer]:Integer Read Get_PendingChange Write Set_PendingChange;

      {Properties that give access to this Class variables}
      property Mode      				         : Integer            read ControlMode;
      property CombiMode 				         : Integer            read CombiControlMode;
      property DERNameList         	     : TStringList        read FDERNameList;
      property vvc_curve1           	   : String             read Fvvc_curvename;
      property hysteresis_offset    	   : Double             read Fvvc_curveOffset;
      property voltage_curvex_ref   	   : Integer            read FVoltage_CurveX_ref;
      property avgwindowlen         	   : Integer            read FRollAvgWindowLength;
      property voltwatt_curve       	   : String             read Fvoltwatt_curvename;
      property voltwattCH_curve       	 : String             read FvoltwattCH_curvename;
      property DbVMin               	   : Double             read FDbVMin;
      property DbVMax               	   : Double             read FDbVMax;
      property ArGraLowV            	   : Double             read FArGraLowV;
      property ArGraHiV             	   : Double             read FArGraHiV;
      property DynReacavgwindowlen  	   : Integer            read FDRCRollAvgWindowLength;
      property DeltaQ_factor             : Double             read FDeltaQ_factor;
      property VoltageChangeTolerance    : Double             read FVoltageChangeTolerance;
      property VarChangeTolerance        : Double             read FVarChangeTolerance;
      property VoltwattYAxis             : Integer            read FVoltwattYAxis;
      //property RateofChangeMode          : String             read
      property LPFTau                    : Double             read FLPFTau;
      property RiseFallLimit             : Double             read FRiseFallLimit;
      property DeltaP_factor             : Double             read FDeltaP_factor;
      //property EventLog                  : String             read
      property RefReactivePower          : String             read FReacPower_ref;
      property ActivePChangeTolerance    : Double             read FActivePChangeTolerance;
      property monVoltageCalc            : Integer            read FMonBusesPhase;
      property monBus                    : TStringList        read FMonBusesNameList;
      property monBusVbase               : pDoubleArray       read FMonBusesVbase;
      property v_setpoint                : Double             read Fv_setpoint;
      // Need to include the new modes here

      // for CIM export
      property VoltVarCurve: TXYCurveObj read Fvvc_curve;
      property VoltWattCurve: TXYCurveObj read Fvoltwatt_curve;
      property VoltWattChargingCurve: TXYCurveObj read FvoltwattCH_curve;
      property WattVarCurve: TXYCurveObj read Fwattvar_curve;
  end;

VAR
    ActiveInvControlObj : TInvControlObj;

IMPLEMENTATION

uses

    ParserDel, Sysutils, DSSClassDefs, DSSGlobals, Circuit,  uCmatrix, Math;

const

    NumPropsThisClass = 34;

    NONE = 0;
    CHANGEVARLEVEL = 1;
    CHANGEWATTLEVEL = 2;
    CHANGEWATTVARLEVEL = 3;
    CHANGEDRCVVARLEVEL = 4;

    AVGPHASES = -1;
    MAXPHASE  = -2;
    MINPHASE  = -3;

    FLAGDELTAQ = -1.0;
    FLAGDELTAP = -1.0;
    DELTAQDEFAULT = 0.5;
    DELTAPDEFAULT = 0.5;

    // Modes
    NONE_MODE = 0;
    VOLTVAR   = 1;
    VOLTWATT  = 2;
    DRC       = 3;
    WATTPF    = 4;
    WATTVAR   = 5;
    AVR       = 6;
    GFM       = 7;      // Grid forming inverter mode

    // Modes in string type
    myCtrlModes   : array [0..6] of string =
    ('voltvar', 'voltwatt', 'dynamicreaccurr', 'wattpf', 'wattvar', 'avr', 'gfm');

    // Combi Modes
    NONE_COMBMODE = 0;
    VV_VW         = 1;
    VV_DRC        = 2;


constructor TInvControl.Create;  // Creates superstructure for all InvControl objects
  begin
    Inherited Create;

     Class_name   := 'InvControl';
     DSSClassType := DSSClassType + INV_CONTROL2;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
     XY_CurveClass := GetDSSClassPtr('XYCurve');

  end;

destructor TInvControl.Destroy;
  begin
    Inherited Destroy;
  end;

procedure TInvControl.DefineProperties;
  begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

    // Define Property names
    PropertyName[1] := 'DERList';
    PropertyName[2] := 'Mode';
    PropertyName[3] := 'CombiMode';
    PropertyName[4] := 'vvc_curve1';
    PropertyName[5] := 'hysteresis_offset';
    PropertyName[6] := 'voltage_curvex_ref';
    PropertyName[7] := 'avgwindowlen';

    PropertyName[8] := 'voltwatt_curve';

    //following for dynamic reactive current mode
    PropertyName[9] :=  'DbVMin';
    PropertyName[10] := 'DbVMax';
    PropertyName[11] := 'ArGraLowV';
    PropertyName[12] := 'ArGraHiV';
    PropertyName[13] := 'DynReacavgwindowlen';
    PropertyName[14] := 'deltaQ_Factor';
    PropertyName[15] := 'VoltageChangeTolerance';
    PropertyName[16] := 'VarChangeTolerance';
    PropertyName[17] := 'VoltwattYAxis';
    PropertyName[18] := 'RateofChangeMode';
    PropertyName[19] := 'LPFTau';
    PropertyName[20] := 'RiseFallLimit';
    PropertyName[21] := 'deltaP_Factor';
    PropertyName[22] := 'EventLog';
    PropertyName[23] := 'RefReactivePower';
    PropertyName[24] := 'ActivePChangeTolerance';
    PropertyName[25] := 'monVoltageCalc';
    PropertyName[26] := 'monBus';
    PropertyName[27] := 'MonBusesVbase';
    PropertyName[28] := 'voltwattCH_curve';
    PropertyName[29] := 'wattpf_curve';
    PropertyName[30] := 'wattvar_curve';
    PropertyName[31] := 'VV_RefReactivePower';
    PropertyName[32] := 'PVSystemList';
    PropertyName[33] := 'Vsetpoint';
    PropertyName[34] := 'ControlModel';

    PropertyHelp[1] := 'Array list of PVSystem and/or Storage elements to be controlled. ' +
                       'If not specified, all PVSystem and Storage in the circuit are assumed to be controlled by this control. '  +CRLF+CRLF+
                      'No capability of hierarchical control between two controls for a single element is implemented at this time.';

    PropertyHelp[2] := 'Smart inverter function in which the InvControl will control the PC elements specified in DERList, according to the options below:' +CRLF+CRLF+
                      'Must be one of: {VOLTVAR* | VOLTWATT | DYNAMICREACCURR | WATTPF | WATTVAR | GFM} ' +CRLF+
                      'if the user desires to use modes simultaneously, then set the CombiMode property. Setting the Mode to any valid value disables combination mode.'+

                       CRLF+CRLF+'In volt-var mode (Default). This mode attempts to CONTROL the vars, according to one or two volt-var curves, depending on the monitored voltages, present active power output, and the capabilities of the PVSystem/Storage. ' +
                       CRLF+CRLF+'In volt-watt mode. This mode attempts to LIMIT the watts, according to one defined volt-watt curve, depending on the monitored voltages and the capabilities of the PVSystem/Storage. '+
                       CRLF+CRLF+'In dynamic reactive current mode. This mode attempts to increasingly counter deviations by CONTROLLING vars, depending on the monitored voltages, present active power output, and the capabilities of the of the PVSystem/Storage.'+
                       CRLF+CRLF+'In watt-pf mode. This mode attempts to CONTROL the vars, according to a watt-pf curve, depending on the present active power output, and the capabilities of the PVSystem/Storage. '+
                       CRLF+CRLF+'In watt-var mode. This mode attempts to CONTROL the vars, according to a watt-var curve, depending on the present active power output, and the capabilities of the PVSystem/Storage. ' +
                       CRLF+CRLF+'In GFM (Grid Forming Inverter) mode, the inverter based resource will be used for stablishing the voltage level of the microgrid/grid they are connected to. In any other control mode, DER will behave as Grid FoLlowing inverter (GFL).';

    PropertyHelp[3] := 'Combination of smart inverter functions in which the InvControl will control the PC elements in DERList, according to the options below: '+CRLF+CRLF+
                      'Must be a combination of the following: {VV_VW | VV_DRC}. Default is to not set this property, in which case the single control mode in Mode is active.  ' +

                       CRLF+CRLF+'In combined VV_VW mode, both volt-var and volt-watt control modes are active simultaneously.  See help individually for volt-var mode and volt-watt mode in Mode property.'+
                       CRLF+'Note that the PVSystem/Storage will attempt to achieve both the volt-watt and volt-var set-points based on the capabilities of the inverter in the PVSystem/Storage (kVA rating, etc), any limits set on maximum active power,' +
//                       CRLF+', any limits set on maximum reactive power. '+
//                       CRLF+'Precedence will be given to either watt production or var production based on the setting of RefReactivePower.'+
                       CRLF+CRLF+'In combined VV_DRC, both the volt-var and the dynamic reactive current modes are simultaneously active.';
//                       CRLF+CRLF+'The volt-var function will attempt to achieve its set-point based on the volt-var curve, and present voltage.  The dynamic '+
//                       CRLF+'reactive power mode function will also be active and it will add or subtract from the reactive power set-point desired by the volt-var function.'+
//                       CRLF+'Note that the precedence of active and reactive power production is defined by the RefReactivePower property.  In no event will the reactive '+
//                       CRLF+'power exceed the maximum var limit of the PVSystem, and the combination of the active and reactive power output will not exceed the kVA rating of '+
//                       CRLF+'the inverter (set in the PVSystem/Storage).';

    PropertyHelp[4] := 'Required for VOLTVAR mode. '+CRLF+CRLF+
                      'Name of the XYCurve object containing the volt-var curve. The positive values of the y-axis of the volt-var curve represent values in pu of the provided base reactive power. ' +
                      'The negative values of the y-axis are values in pu of the absorbed base reactive power. ' +CRLF+
                      'Provided and absorbed base reactive power values are defined in the RefReactivePower property' +CRLF+CRLF+
                      'Units for the x-axis are per-unit voltage, which may be in per unit of the rated voltage for the PVSystem/Storage, or may be in per unit of the average voltage at the terminals over a user-defined number of prior solutions. ';

    PropertyHelp[5] := 'Required for VOLTVAR mode, and defaults to 0. '+CRLF+CRLF+
                      'for the times when the terminal voltage is decreasing, this is the off-set in per-unit voltage of a curve whose shape is the same as vvc_curve. '+
                      'It is offset by a certain negative value of per-unit voltage, which is defined by the base quantity for the x-axis of the volt-var curve (see help for voltage_curvex_ref)'+CRLF+CRLF+
                      'if the PVSystem/Storage terminal voltage has been increasing, and has not changed directions, utilize vvc_curve1 for the volt-var response. '+CRLF+CRLF+
                      'if the PVSystem/Storage terminal voltage has been increasing and changes directions and begins to decrease, then move from utilizing vvc_curve1 to a volt-var curve of the same shape, but offset by a certain per-unit voltage value. '+CRLF+CRLF+
                      'Maintain the same per-unit available var output level (unless head-room has changed due to change in active power or kva rating of PVSystem/Storage).  Per-unit var values remain the same for this internally constructed second curve (hysteresis curve). '+CRLF+CRLF+
                      'if the terminal voltage has been decreasing and changes directions and begins to increase , then move from utilizing the offset curve, back to the vvc_curve1 for volt-var response, but stay at the same per-unit available vars output level.';

    PropertyHelp[6] := 'Required for VOLTVAR and VOLTWATT modes, and defaults to rated.  Possible values are: {rated|avg|ravg}.  '+CRLF+CRLF+
                      'Defines whether the x-axis values (voltage in per unit) for vvc_curve1 and the volt-watt curve corresponds to:'+CRLF+CRLF+
                      'rated. The rated voltage for the PVSystem/Storage object (1.0 in the volt-var curve equals rated voltage).'+CRLF+CRLF+
                      'avg. The average terminal voltage recorded over a certain number of prior power-flow solutions.'+CRLF+
                      'with the avg setting, 1.0 per unit on the x-axis of the volt-var curve(s) corresponds to the average voltage.'+CRLF+
                      'from a certain number of prior intervals.  See avgwindowlen parameter.'+CRLF+CRLF+
                      'ravg. Same as avg, with the exception that the avgerage terminal voltage is divided by the rated voltage.';

    PropertyHelp[7] := 'Required for VOLTVAR mode and VOLTWATT mode, and defaults to 0 seconds (0s). '+CRLF+CRLF+
                      'Sets the length of the averaging window over which the average PVSystem/Storage terminal voltage is calculated. '+CRLF+CRLF+
                      'Units are indicated by appending s, m, or h to the integer value. '+CRLF+CRLF+
                      'The averaging window will calculate the average PVSystem/Storage terminal voltage over the specified period of time, up to and including the last power flow solution. '+CRLF+CRLF+
                      'Note, if the solution stepsize is larger than the window length, then the voltage will be assumed to have been constant over the time-frame specified by the window length.';

    PropertyHelp[8] := 'Required for VOLTWATT mode. '+CRLF+CRLF+
                      'Name of the XYCurve object containing the volt-watt curve. '+CRLF+CRLF+
                      'Units for the x-axis are per-unit voltage, which may be in per unit of the rated voltage for the PVSystem/Storage, or may be in per unit of the average voltage at the terminals over a user-defined number of prior solutions. '+CRLF+CRLF+
                      'Units for the y-axis are either in one of the options described in the VoltwattYAxis property. ';

    PropertyHelp[9] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.95 per-unit voltage (referenced to the PVSystem/Storage object rated voltage or a windowed average value). '+CRLF+CRLF+
                      'This parameter is the minimum voltage that defines the voltage dead-band within which no reactive power is allowed to be generated. ';

    PropertyHelp[10] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 1.05 per-unit voltage (referenced to the PVSystem object rated voltage or a windowed average value). '+CRLF+CRLF+
                      'This parameter is the maximum voltage that defines the voltage dead-band within which no reactive power is allowed to be generated. ';

    PropertyHelp[11] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.1  '+CRLF+CRLF+
                       'This is a gradient, expressed in unit-less terms of %/%, to establish the ratio by which percentage capacitive reactive power production is increased as the  percent delta-voltage decreases below DbVMin. '+CRLF+CRLF+
                       'Percent delta-voltage is defined as the present PVSystem/Storage terminal voltage minus the moving average voltage, expressed as a percentage of the rated voltage for the PVSystem/Storage object. '+CRLF+CRLF+
                       'Note, the moving average voltage for the dynamic reactive current mode is different than the moving average voltage for the volt-watt and volt-var modes.';

    PropertyHelp[12] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.1  '+CRLF+CRLF+
                       'This is a gradient, expressed in unit-less terms of %/%, to establish the ratio by which percentage inductive reactive power production is increased as the  percent delta-voltage decreases above DbVMax. '+CRLF+CRLF+
                       'Percent delta-voltage is defined as the present PVSystem/Storage terminal voltage minus the moving average voltage, expressed as a percentage of the rated voltage for the PVSystem/Storage object. '+CRLF+CRLF+
                       'Note, the moving average voltage for the dynamic reactive current mode is different than the mmoving average voltage for the volt-watt and volt-var modes.';

    PropertyHelp[13] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 1 seconds (1s). do not use a value smaller than 1.0 '+CRLF+CRLF+
                       'Sets the length of the averaging window over which the average PVSystem/Storage terminal voltage is calculated '+
                       'for the dynamic reactive current mode. '+CRLF+CRLF+
                       'Units are indicated by appending s, m, or h to the integer value. '+CRLF+CRLF+
                       'Typically this will be a shorter averaging window than the volt-var and volt-watt averaging window.'+CRLF+CRLF+
                       'The averaging window will calculate the average PVSystem/Storage terminal voltage over the specified period of time, up to and including the last power flow solution.  Note, if the solution stepsize is larger than '+
                       'the window length, then the voltage will be assumed to have been constant over the time-frame specified by the window length.';

    PropertyHelp[14] := 'Required for the VOLTVAR and DYNAMICREACCURR modes.  Defaults to -1.0. '+CRLF+CRLF+
                       'Defining -1.0, OpenDSS takes care internally of delta_Q itself. It tries to improve convergence as well as speed up process'+CRLF+CRLF+
                       'Sets the maximum change (in per unit) from the prior var output level to the desired var output level during each control iteration. '+CRLF+CRLF+CRLF+
                       'if numerical instability is noticed in solutions such as var sign changing from one control iteration to the next and voltages oscillating between two values with some separation, '+
                       'this is an indication of numerical instability (use the EventLog to diagnose). '+CRLF+CRLF+
                       'if the maximum control iterations are exceeded, and no numerical instability is seen in the EventLog of via monitors, then try increasing the value of this parameter to reduce the number '+
                       'of control iterations needed to achieve the control criteria, and move to the power flow solution.' + CRLF +
                       'When operating the controller using expoenential control model (see CtrlModel), this parameter represents the sampling time gain of the controller, which is used for accelrating the controller response in terms of control iterations required.';

    PropertyHelp[15] := 'Defaults to 0.0001 per-unit voltage.  This parameter should only be modified by advanced users of the InvControl.  '+CRLF+CRLF+
                        'Tolerance in pu of the control loop convergence associated to the monitored voltage in pu. ' +
                        'This value is compared with the difference of the monitored voltage in pu of the current and previous control iterations of the control loop'+CRLF+CRLF+

                        'This voltage tolerance value plus the var/watt tolerance value (VarChangeTolerance/ActivePChangeTolerance) determine, together, when to stop control iterations by the InvControl. '+CRLF+CRLF+

                        'If an InvControl is controlling more than one PVSystem/Storage, each PVSystem/Storage has this quantity calculated independently, and so an individual '+
                        'PVSystem/Storage may reach the tolerance within different numbers of control iterations.';

    PropertyHelp[16] := 'Required for VOLTVAR and DYNAMICREACCURR modes.  Defaults to 0.025 per unit of the base provided or absorbed reactive power described in the RefReactivePower property '+

                       'This parameter should only be modified by advanced users of the InvControl. '+CRLF+CRLF+

                       'Tolerance in pu of the convergence of the control loop associated with reactive power. ' +
                       'For the same control iteration, this value is compared to the difference, as an absolute value (without sign), between the desired reactive power value in pu and the output reactive power in pu of the controlled element.'+CRLF+CRLF+

                       'This reactive power tolerance value plus the voltage tolerance value (VoltageChangeTolerance) determine, together, when to stop control iterations by the InvControl.  '+CRLF+CRLF+

                       'If an InvControl is controlling more than one PVSystem/Storage, each PVSystem/Storage has this quantity calculated independently, and so an individual '+
                       'PVSystem/Storage may reach the tolerance within different numbers of control iterations.';

    PropertyHelp[17] := 'Required for VOLTWATT mode.  Must be one of: {PMPPPU* | PAVAILABLEPU| PCTPMPPPU | KVARATINGPU}.  The default is PMPPPU.  '+CRLF+CRLF+
                       'Units for the y-axis of the volt-watt curve while in volt-watt mode. '+CRLF+CRLF+
                       'When set to PMPPPU. The y-axis corresponds to the value in pu of Pmpp property of the PVSystem. '+CRLF+CRLF+
                       'When set to PAVAILABLEPU. The y-axis corresponds to the value in pu of the available active power of the PVSystem. '+CRLF+CRLF+
                       'When set to PCTPMPPPU. The y-axis corresponds to the value in pu of the power Pmpp multiplied by 1/100 of the %Pmpp property of the PVSystem.' +CRLF+CRLF+
                       'When set to KVARATINGPU. The y-axis corresponds to the value in pu of the kVA property of the PVSystem.';


    PropertyHelp[18] := 'Required for VOLTWATT and VOLTVAR mode.  Must be one of: {INACTIVE* | LPF | RISEFALL }.  The default is INACTIVE.  '+CRLF+CRLF+
                       'Auxiliary option that aims to limit the changes of the desired reactive power and the active power limit between time steps, the alternatives are listed below: '
                       +CRLF+CRLF+ 'INACTIVE. It indicates there is no limit on rate of change imposed for either active or reactive power output. '
                       +CRLF+CRLF+ 'LPF. A low-pass RC filter is applied to the desired reactive power and/or the active power limit to determine the output power as a function of a time constant defined in the LPFTau property. '
                       +CRLF+CRLF+ 'RISEFALL. A rise and fall limit in the change of active and/or reactive power expressed in terms of pu power per second, defined in the RiseFallLimit, is applied to the desired reactive power and/or the active power limit. ';


    PropertyHelp[19] := 'Not required. Defaults to 0 seconds. '+CRLF+CRLF+
                        'Filter time constant of the LPF option of the RateofChangeMode property. ' +
                        'The time constant will cause the low-pass filter to achieve 95% of the target value in 3 time constants.';

    PropertyHelp[20] := 'Not required.  Defaults to no limit (-1). Must be -1 (no limit) or a positive value.  '+CRLF+CRLF+
                        'Limit in power in pu per second used by the RISEFALL option of the RateofChangeMode property.' +
                        'The base value for this ramp is defined in the RefReactivePower property and/or in VoltwattYAxis.';

    PropertyHelp[21] := 'Required for the VOLTWATT modes.  Defaults to -1.0. '+CRLF+CRLF+
                       'Defining -1.0, OpenDSS takes care internally of delta_P itself. It tries to improve convergence as well as speed up process'+CRLF+CRLF+
                       'Defining between 0.05 and 1.0, it sets the maximum change (in unit of the y-axis) from the prior active power output level to the desired active power output level during each control iteration. '+CRLF+CRLF+CRLF+
                       'If numerical instability is noticed in solutions such as active power changing substantially from one control iteration to the next and/or voltages oscillating between two values with some separation, '+
                       'this is an indication of numerical instability (use the EventLog to diagnose). '+CRLF+CRLF+
                       'If the maximum control iterations are exceeded, and no numerical instability is seen in the EventLog of via monitors, then try increasing the value of this parameter to reduce the number '+
                       'of control iterations needed to achieve the control criteria, and move to the power flow solution.' + CRLF +
                       'When operating the controller using expoenential control model (see CtrlModel), this parameter represents the sampling time gain of the controller, which is used for accelrating the controller response in terms of control iterations required.';;

    PropertyHelp[22] := '{Yes/True* | No/False} Default is NO for InvControl. Log control actions to Eventlog.';

    PropertyHelp[23] := 'Required for any mode that has VOLTVAR, DYNAMICREACCURR and WATTVAR. Defaults to VARAVAL.' +CRLF+CRLF+
                        'Defines the base reactive power for both the provided and absorbed reactive power, according to one of the following options: '
                        +CRLF+CRLF+ 'VARAVAL. The base values for the provided and absorbed reactive power are equal to the available reactive power.'
                        +CRLF+CRLF+ 'VARMAX: The base values of the provided and absorbed reactive power are equal to the value defined in the kvarMax and kvarMaxAbs properties, respectively.';

    PropertyHelp[24] :=  'Required for VOLTWATT. Default is 0.01'+CRLF+CRLF+
                         'Tolerance in pu of the convergence of the control loop associated with active power. ' +
                         'For the same control iteration, this value is compared to the difference between the active power limit in pu resulted from the convergence process and the one resulted from the volt-watt function.'+CRLF+CRLF+

                         'This reactive power tolerance value plus the voltage tolerance value (VoltageChangeTolerance) determine, together, when to stop control iterations by the InvControl.  '+CRLF+CRLF+

                        'If an InvControl is controlling more than one PVSystem/Storage, each PVSystem/Storage has this quantity calculated independently, and so an individual '+
                        'PVSystem/Storage may reach the tolerance within different numbers of control iterations.';

    PropertyHelp[25] := 'Number of the phase being monitored or one of {AVG | MAX | MIN} for all phases. Default=AVG. ';

    PropertyHelp[26] := 'Name of monitored bus used by the voltage-dependente control modes. Default is bus of the controlled PVSystem/Storage or Storage.' ;

    PropertyHelp[27] := 'Array list of rated voltages of the buses and their nodes presented in the monBus property. This list may have different line-to-line and/or line-to-ground voltages.' ;

    PropertyHelp[28] := 'Required for VOLTWATT mode for Storage element in CHARGING state. '+CRLF+CRLF+
                        'The name of an XYCurve object that describes the variation in active power output (in per unit of maximum active power outut for the Storage). '+CRLF+CRLF+
                        'Units for the x-axis are per-unit voltage, which may be in per unit of the rated voltage for the Storage, or may be in per unit of the average voltage at the terminals over a user-defined number of prior solutions. '+CRLF+CRLF+
                        'Units for the y-axis are either in: (1) per unit of maximum active power output capability of the Storage, or (2) maximum available active power output capability (defined by the parameter: VoltwattYAxis), '+
                        'corresponding to the terminal voltage (x-axis value in per unit). '+CRLF+CRLF+
                        'No default -- must be specified for VOLTWATT mode for Storage element in CHARGING state.';

    PropertyHelp[29] := 'Required for WATTPF mode.' +CRLF+CRLF+
                      'Name of the XYCurve object containing the watt-pf curve.' +CRLF+
                      'The positive values of the y-axis are positive power factor values. ' +
                      'The negative values of the the y-axis are negative power factor values. ' +
                      'When positive, the output reactive power has the same direction of the output active power, and when negative, it has the opposite direction.' +CRLF+
                      'Units for the x-axis are per-unit output active power, and the base active power is the Pmpp for PVSystem and kWrated for Storage.'+CRLF+CRLF+
                      'The y-axis represents the power factor and the reference is power factor equal to 0. '+CRLF+CRLF+

                      'For example, if the user wants to define the following XY coordinates: (0, 0.9); (0.2, 0.9); (0.5, -0.9); (1, -0.9).'+CRLF+
                      'Try to plot them considering the y-axis reference equal to unity power factor.' +CRLF+CRLF+
                      'The user needs to translate this curve into a plot in which the y-axis reference is equal to 0 power factor.' +
                      'It means that two new XY coordinates need to be included, in this case they are: (0.35, 1); (0.35, -1).'+CRLF+
                      'Try to plot them considering the y-axis reference equal to 0 power factor.' +CRLF+
                      'The discontinity in 0.35pu is not a problem since var is zero for either power factor equal to 1 or -1.';

    PropertyHelp[30] := 'Required for WATTVAR mode. '+CRLF+CRLF+
                      'Name of the XYCurve object containing the watt-var curve. The positive values of the y-axis of the watt-var curve represent values in pu of the provided base reactive power. ' +
                      'The negative values of the y-axis are values in pu of the absorbed base reactive power. ' +CRLF+
                      'Provided and absorbed base reactive power values are defined in the RefReactivePower property.' +CRLF+CRLF+
                      'Units for the x-axis are per-unit output active power, and the base active power is the Pmpp for PVSystem and kWrated for Storage.';
    PropertyHelp[31] := 'Deprecated, use RefReactivePower instead.';
    PropertyHelp[32] := 'Deprecated, use DERList instead.';
    PropertyHelp[33] := 'Required for Active Voltage Regulation (AVR).';
    PropertyHelp[34] := 'Integer defining the method for moving across the control curve. It can be one of the following: ' + CRLF + CRLF +
                        '0 = Lienar mode (default)' + CRLF +
                        '1 = Exponential' + CRLF + CRLF +
                        'Use this property for better tunning your controller and improve the controller response in terms of control iterations needed to reach the target.' + CRLF +
                        'This property alters the meaning of deltaQ_factor and deltaP_factor properties accroding to its value (Check help). The method can also be combined with the controller tolerance for improving performance.';


    ActiveProperty  := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

  end;

function TInvControl.NewObject(const ObjName:String):Integer;
  begin
    // Make a new InvControl and add it to InvControl class list
    with ActiveCircuit[ActiveActor] do
      begin
        ActiveCktElement := TInvControlObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
      end;
  end;

function TInvControl.Edit(ActorID : Integer):Integer;
  VAR
    CharPos,
    ParamPointer,
    i,
    j,
    NNode             : Integer;

    StrTemp,
    ParamName,
    Param             : String;

    NodeBuffer        : Array[1..10] of Integer;

  begin

    // continue parsing with contents of Parser
    ActiveInvControlObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveInvControlObj;

    Result := 0;

    with ActiveInvControlObj do
      begin

        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;
        Param := Parser[ActorID].StrValue;

        WHILE Length(Param)>0 do
          begin
            if Length(ParamName) = 0 then Inc(ParamPointer)
            else ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer>0) and (ParamPointer<=NumProperties) then PropertyValue[ParamPointer]:= Param;

            CASE ParamPointer OF
              0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 364);
              1: InterpretTStringListArray(Param, FDERNameList); // Read list of PVSystem and Storage objects in OpenDSS format and add to FDERNameList StringList.
              2: begin
                   StrTemp := Parser[ActorID].StrValue;
                   j       :=  0;
                   for i := 0 to High(myCtrlModes) do
                   Begin
                     if CompareTextShortest(StrTemp, myCtrlModes[i])= 0 then
                     Begin
                       ControlMode := i + 1;
                       CombiControlMode := NONE_COMBMODE;
                       j           :=  1;
                       break;
                     End;
                   End;
                   if j = 0 then
                   begin
                     if ControlMode = NONE_MODE then DoSimpleMsg('Invalid Control Mode selected', 1366);
                     CombiControlMode := NONE_COMBMODE;
                     SolutionAbort := True;
                     exit;
                   end;
                 end;

              3: begin
                  if CompareTextShortest(Parser[ActorID].StrValue, 'vv_vw')= 0 then
                    begin
                      ControlMode := NONE_MODE;
                      CombiControlMode := VV_VW;
                    end
                  else if CompareTextShortest(Parser[ActorID].StrValue, 'vv_drc')= 0 then
                    begin
                      ControlMode := NONE_MODE;
                      CombiControlMode := VV_DRC;
                    end
                  else
                    begin
                      if CombiControlMode = NONE_COMBMODE then DoSimpleMsg('Invalid CombiControl Mode selected', 1367);
                      CombiControlMode := NONE_COMBMODE;
                      SolutionAbort := True;
                      exit;
                    end;
                 end;

              4: begin
                  Fvvc_curvename := Parser[ActorID].StrValue;
                  if Length(Fvvc_curvename) > 0 then
                    begin
                      Fvvc_curve := GetXYCurve(Fvvc_curvename, VOLTVAR);
                      Fvvc_curve_size := Fvvc_curve.NumPoints;
                    end;
                 end;

              5: begin
                  if(Parser[ActorID].DblValue > 0.0) then
                    DoSimpleMsg('Hysteresis offset should be a negative value, or 0 "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 1364)
                  else
                      Fvvc_curveOffset := Parser[ActorID].DblValue;
                 end;

              6: begin
                  if CompareTextShortest(Parser[ActorID].StrValue, 'rated') = 0 then FVoltage_CurveX_ref := 0
                  else if CompareTextShortest(Parser[ActorID].StrValue, 'avg')= 0 then FVoltage_CurveX_ref := 1
                  else if CompareTextShortest(Parser[ActorID].StrValue, 'ravg')= 0 then FVoltage_CurveX_ref := 2
                 end;

              7: FRollAvgWindowLength := InterpretAvgVWindowLen(Param);

              8: begin
                  Fvoltwatt_curvename := Parser[ActorID].StrValue;
                  if Length(Fvoltwatt_curvename) > 0 then
                    begin
                      Fvoltwatt_curve := GetXYCurve(Fvoltwatt_curvename, VOLTWATT);
                      Fvoltwatt_curve_size := Fvoltwatt_curve.NumPoints;
                    end;
                 end;

              9: begin
                  FDbVMin := Parser[ActorID].DblValue;
                  if(FDbVMax > 0.0) and (FDbVmin > FDbVMax) then
                    begin
                      DoSimpleMsg('Minimum dead-band voltage value should be less than the maximum dead-band voltage value.  Value set to 0.0 "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 1365);
                      FDbvMin := 0.0;
                    end;
                 end;

              10: begin
                    FDbVMax := Parser[ActorID].DblValue;
                    if(FDbVMin > 0.0) and (FDbVMax < FDbVmin) then
                      begin
                        DoSimpleMsg('Maximum dead-band voltage value should be greater than the minimum dead-band voltage value.  Value set to 0.0 "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 1366);
                        FDbvMax := 0.0;
                      end;
                  end;

              11: FArGraLowV := Parser[ActorID].DblValue;
              12: FArGraHiV := Parser[ActorID].DblValue;
              13: FDRCRollAvgWindowLength := InterpretDRCAvgVWindowLen(Param);
              14: FdeltaQ_factor := Parser[ActorID].DblValue;
              15: FVoltageChangeTolerance := Parser[ActorID].DblValue;
              16: FVarChangeTolerance := Parser[ActorID].DblValue;

              17: begin
                    if CompareTextShortest(Parser[ActorID].StrValue, 'pavailablepu')= 0          then  FVoltwattYAxis := 0
                    else if CompareTextShortest(Parser[ActorID].StrValue, 'pmpppu')= 0           then  FVoltwattYAxis := 1
                    else if CompareTextShortest(Parser[ActorID].StrValue, 'pctpmpppu')= 0        then  FVoltwattYAxis := 2
                    else if CompareTextShortest(Parser[ActorID].StrValue, 'kvaratingpu')= 0      then  FVoltwattYAxis := 3
                  end;

              18: begin
                    if      CompareTextShortest(Parser[ActorID].StrValue, 'inactive')= 0          then  RateofChangeMode := INACTIVE
                    else if CompareTextShortest(Parser[ActorID].StrValue, 'lpf')= 0               then  RateofChangeMode := LPF
                    else if CompareTextShortest(Parser[ActorID].StrValue, 'risefall')= 0          then  RateofChangeMode := RISEFALL
                  end;

              19: begin
                    if Parser[ActorID].DblValue > 0 then FLPFTau := Parser[ActorID].DblValue
                    else RateofChangeMode := INACTIVE;
                  end;

              20: begin
                    if Parser[ActorID].DblValue > 0 then FRiseFallLimit := Parser[ActorID].DblValue
                    else RateofChangeMode := INACTIVE;
                  end;

              21: FdeltaP_factor := Parser[ActorID].DblValue;
              22: ShowEventLog := InterpretYesNo(param);

              23: begin
                    if      CompareTextShortest(Parser[ActorID].StrValue, 'varaval')= 0         then  FReacPower_ref := 'VARAVAL'
                    else if CompareTextShortest(Parser[ActorID].StrValue, 'varmax')= 0          then  FReacPower_ref := 'VARMAX'
                  end;

              24: FActivePChangeTolerance := Parser[ActorID].DblValue;

              25: begin
                    if      CompareTextShortest(param, 'avg') = 0                 then FMonBusesPhase := AVGPHASES
                    else if CompareTextShortest(param, 'max') = 0                 then FMonBusesPhase := MAXPHASE
                    else if CompareTextShortest(param, 'min') = 0                 then FMonBusesPhase := MINPHASE
                    else FMonBusesPhase := max(1, Parser[ActorID].IntValue);
                  end;

              26: begin //FMonBuses     := Param;

                    InterpretTStringListArray(Param, FMonBusesNameList);
                    SetLength(FMonBuses, FMonBusesNameList.Count);
                    SetLength(FMonBusesNodes, FMonBusesNameList.Count);

                    AuxParser[ActiveActor].CmdString :=  Param;  //Parser[ActorID].StrValue;  // load AuxParser

                    for i := 0 to FMonBusesNameList.Count-1 do begin
                      AuxParser[ActiveActor].NextParam;  // Gets the next token

                      FMonBuses[i] :=  AuxParser[ActiveActor].ParseAsBusName(NNode,@NodeBuffer, ActiveActor);
                      SetLength(FMonBusesNodes[i], NNode);

                      for j := 0 to NNode-1 do FMonBusesNodes[i,j] := NodeBuffer[j+1];

                    end;

                  end;

              27: begin
                    ReAllocmem(FMonBusesVbase, Sizeof(FMonBusesVbase^[1])*FMonBusesNameList.Count);
                    Parser[Activeactor].ParseAsVector(FMonBusesNameList.Count, FMonBusesVbase);
                  end;

              28: begin
                    FvoltwattCH_curvename := Parser[ActorID].StrValue;
                    if Length(FvoltwattCH_curvename) > 0 then
                      begin
                        FvoltwattCH_curve      := GetXYCurve(FvoltwattCH_curvename, VOLTWATT);
                        FvoltwattCH_curve_size := FvoltwattCH_curve.NumPoints;
                      end;
                  end;

              29: begin
                  Fwattpf_curvename := Parser[ActorID].StrValue;
                  if Length(Fwattpf_curvename) > 0 then
                    begin
                      Fwattpf_curve := GetXYCurve(Fwattpf_curvename, WATTPF);
                      Fwattpf_curve_size := Fwattpf_curve.NumPoints;
                    end;
                 end;

              30: begin
                  Fwattvar_curvename := Parser[ActorID].StrValue;
                  if Length(Fwattvar_curvename) > 0 then
                    begin
                      Fwattvar_curve := GetXYCurve(Fwattvar_curvename, WATTVAR);
                      Fwattvar_curve_size := Fwattvar_curve.NumPoints;
                    end;
                 end;

              31: begin
                    StrTemp :=  Parser[ActorID].StrValue;
                    Charpos :=  ansipos('_', StrTemp);
                    if CharPos <> 0 then
                      StrTemp :=  StrTemp.Substring(0,CharPos - 1);

                    if      CompareTextShortest(StrTemp, 'varaval')= 0         then  FReacPower_ref := 'VARAVAL'
                    else if CompareTextShortest(StrTemp, 'varmax')= 0          then  FReacPower_ref := 'VARMAX'
                 end;

              32: begin
                    InterpretTStringListArray(Param, FDERNameList); // Read list of PVSystem and Storage objects in OpenDSS format and add to FDERNameList StringList.
                    // Because is using this command from the previous version of InvControl, we assume that the list includes only
                    // PVSystems, so the list is updated
                    for CharPos := 0 to (FDERNameList.Count - 1) do
                      FDERNameList[CharPos] :=  'PVSystem.' + FDERNameList[CharPos];
                 end;

              33: Fv_setpoint := Parser[ActorID].DblValue;
              34: CtrlModel   := Parser[ActorID].IntValue;

              else
                // Inherited parameters
                ClassEdit( ActiveInvControlObj, ParamPointer - NumPropsthisClass)
            end;

            CASE ParamPointer OF
              1: begin // re-alloc based on
                    FDERPointerList.Clear;
                    FListSize := FDERNameList.count;
                  end;
            else

            end;

           ParamName := Parser[ActorID].NextParam;
           Param := Parser[ActorID].StrValue;
          end;

        RecalcElementData(ActorID);
      end;

end;

function TInvControl.MakeLike(const InvControlName:String):Integer;
  VAR
    OtherInvControl   : TInvControlObj;
    i, j              : Integer;

  begin
    Result := 0;
    {See if we can find this InvControl name in the present collection}
    OtherInvControl := Find(InvControlName);

   if OtherInvControl<>Nil then

    with ActiveInvControlObj do begin

      NPhases := OtherInvControl.Fnphases;
      NConds  := OtherInvControl.Fnconds; // Force Reallocation of terminal stuff

      for i := 1 to FDERPointerList.ListSize do
        begin
          With CtrlVars[i] do
          Begin
            ControlledElement           :=  OtherInvControl.CtrlVars[i].ControlledElement;
            CondOffset                  :=  OtherInvControl.CtrlVars[i].CondOffset;

            FVBase                      :=  OtherInvControl.CtrlVars[i].FVBase;
            FVarFollowInverter          :=  OtherInvControl.CtrlVars[i].FVarFollowInverter;
            FInverterON                 :=  OtherInvControl.CtrlVars[i].FInverterON;;
            FpresentkW                  :=  OtherInvControl.CtrlVars[i].FpresentkW;
            FkVARating                  :=  OtherInvControl.CtrlVars[i].FkVARating;
            Fpresentkvar                :=  OtherInvControl.CtrlVars[i].Fpresentkvar;
            FkvarLimit                  :=  OtherInvControl.CtrlVars[i].FkvarLimit;
            FkvarLimitNeg               :=  OtherInvControl.CtrlVars[i].FkvarLimitNeg;
            FCurrentkvarLimit           :=  OtherInvControl.CtrlVars[i].FCurrentkvarLimit;
            FCurrentkvarLimitNeg        :=  OtherInvControl.CtrlVars[i].FCurrentkvarLimitNeg;
            FDCkWRated                  :=  OtherInvControl.CtrlVars[i].FDCkWRated;
            FpctDCkWRated               :=  OtherInvControl.CtrlVars[i].FpctDCkWRated;
            FEffFactor                  :=  OtherInvControl.CtrlVars[i].FEffFactor;
            FDCkW                       :=  OtherInvControl.CtrlVars[i].FDCkW;
            FPPriority                  :=  OtherInvControl.CtrlVars[i].FPPriority;
            FActiveVVCurve              := OtherInvControl.CtrlVars[i].FActiveVVCurve;
          end;
        end;

      ControlMode                     := OtherInvControl.ControlMode;
      CombiControlMode                := OtherInvControl.CombiControlMode;
      FListSize                       := OtherInvControl.FListSize;
      Fvvc_curve_size                 := OtherInvControl.Fvvc_curve_size;
      Fvvc_curve                      := OtherInvControl.Fvvc_curve;
      Fvvc_curvename                  := OtherInvControl.Fvvc_curvename;
      Fvvc_curveOffset                := OtherInvControl.Fvvc_curveOffset;
      FVoltage_CurveX_ref             := OtherInvControl.FVoltage_CurveX_ref;
      FDRCVAvgWindowLengthSec         := OtherInvControl.FDRCVAvgWindowLengthSec;
      FVAvgWindowLengthSec            := OtherInvControl.FVAvgWindowLengthSec;
      Fvoltwatt_curve_size            := OtherInvControl.Fvoltwatt_curve_size;
      Fvoltwatt_curve                 := OtherInvControl.Fvoltwatt_curve;
      Fvoltwatt_curvename             := OtherInvControl.Fvoltwatt_curvename;
      FvoltwattCH_curve_size          := OtherInvControl.FvoltwattCH_curve_size;
      FvoltwattCH_curve               := OtherInvControl.FvoltwattCH_curve;
      FvoltwattCH_curvename           := OtherInvControl.FvoltwattCH_curvename;
      Fwattpf_curve_size              := OtherInvControl.Fwattpf_curve_size;
      Fwattpf_curve                   := OtherInvControl.Fwattpf_curve;
      Fwattpf_curvename               := OtherInvControl.Fwattpf_curvename;
      Fwattvar_curve_size             := OtherInvControl.Fwattvar_curve_size;
      Fwattvar_curve                  := OtherInvControl.Fwattvar_curve;
      Fwattvar_curvename              := OtherInvControl.Fwattvar_curvename;
      FDbVMin                         := OtherInvControl.FDbVMin;
      pf_wp_nominal                   := OtherInvControl.pf_wp_nominal;
      FDbVMax                         := OtherInvControl.FDbVMax;
      FArGraLowV                      := OtherInvControl.FArGraLowV;
      FArGraHiV                       := OtherInvControl.FArGraHiV;

      FRollAvgWindowLength            := OtherInvControl.FRollAvgWindowLength;
      FRollAvgWindowLengthIntervalUnit      := OtherInvControl.FRollAvgWindowLengthIntervalUnit;
      FDRCRollAvgWindowLength         := OtherInvControl.FDRCRollAvgWindowLength;
      FDRCRollAvgWindowLengthIntervalUnit   := OtherInvControl.FDRCRollAvgWindowLengthIntervalUnit;
      FActivePChangeTolerance         := OtherInvControl.FActivePChangeTolerance;
      FdeltaQ_factor                  := OtherInvControl.FdeltaQ_factor;
      FdeltaP_factor                  := OtherInvControl.FdeltaP_factor;
      FVoltageChangeTolerance         := OtherInvControl.FVoltageChangeTolerance;
      FVarChangeTolerance             := OtherInvControl.FVarChangeTolerance;
      FVoltwattYAxis                  := OtherInvControl.FVoltwattYAxis;
      RateofChangeMode                := OtherInvControl.RateofChangeMode;
      FLPFTau                         := OtherInvControl.FLPFTau;
      FRiseFallLimit                  := OtherInvControl.FRiseFallLimit;
      FMonBusesPhase                        := OtherInvControl.FMonBusesPhase;
      FMonBuses                       := OtherInvControl.FMonBuses;
      FMonBusesNodes                  := OtherInvControl.FMonBusesNodes;

      ReallocMem(FMonBusesVbase, SizeOf(FMonBusesVbase^[1])*FMonBusesNameList.Count);
      for j := 1 to FMonBusesNameList.Count do FMonBusesVbase^[j] := OtherInvControl.FMonBusesVbase^[j];

      TimeDelay                  := OtherInvControl.TimeDelay;
      for j := 1 to ParentClass.NumProperties do PropertyValue[j] := OtherInvControl.PropertyValue[j];

    end
   else  DoSimpleMsg('Error in InvControl MakeLike: "' + InvControlName + '" Not Found.', 370);

end;

{==========================================================================}
{                    TInvControlObj                                        }
{==========================================================================}
constructor TInvControlObj.Create(ParClass:TDSSClass; const InvControlName:String);

  begin

    Inherited Create(ParClass);
    Name                     := LowerCase(InvControlName);
    DSSObjType               := ParClass.DSSClassType;

    ElementName              := '';

    {
     Control elements are zero current sources that attach to a terminal of a
     power-carrying device, but do not alter voltage or current flow.
     Define a default number of phases and conductors here and update in
     RecalcElementData routine if necessary. This allocates arrays for voltages
     and currents and gives more direct access to the values,if needed
    }
    NPhases                  := 3;  // Directly set conds and phases
    Fnconds                  := 3;
    Nterms                   := 1;  // this forces allocation of terminals and conductors
                       // in base class
    ControlMode              := NONE_MODE;
    CombiControlMode         := NONE_COMBMODE;

    setlength(CtrlVars,0);
    {Variables for voltages}
    FVpuSolutionIdx          := 0;

    {Variables for convergence process}
    FdeltaQ_factor           := FLAGDELTAQ;
    FdeltaP_factor           := FLAGDELTAP;

    FVoltageChangeTolerance  := 0.0001;
    FVarChangeTolerance      := 0.025;
    FActivePChangeTolerance  := 0.01;

    {Variables of DER element}
    FDERNameList             := nil;
    FDERPointerList          := nil;
    FDERPointerList          := PointerList.TPointerList.Create(20);  // Default size and increment
    FDERNameList             := TSTringList.Create;

    {Variables for monitored Bus/buses}
    FMonBusesNameList        := nil;
    FMonBusesNameList        := TStringList.Create;
    FMonBusesPhase           := AVGPHASES;
    FMonBuses                := nil;
    FMonBusesVbase           := nil;
    FMonBusesNodes           := nil;

    {Variables for LPF and RF options}
    RateofChangeMode         := INACTIVE;
    FLPFTau                  := 0.001;
    FRiseFallLimit           := 0.001;

    {Variables of the smart inverter functions}
    FVoltage_CurveX_ref      := 0;
    FReacPower_ref           := 'VARAVAL';
    FVoltwattYAxis           := 1;

    // volt-var
    Fvvc_curve_size          := 0;
    Fvvc_curve               := nil;
    Fvvc_curvename           := '';
    Fvvc_curveOffset         := 0.0;
    Fvvc_curve2              := nil;

    FVAvgWindowLengthSec     := 1.0;
    FRollAvgWindowLength     := 1;
    FRollAvgWindowLengthIntervalUnit := 's';

    // watt-pf
    Fwattpf_curve_size       := 0;
    Fwattpf_curve            := nil;
    Fwattpf_curvename        := '';
    pf_wp_nominal            := 0.0;

    // watt-var
    Fwattvar_curve_size      := 0;
    Fwattvar_curve           := nil;
    Fwattvar_curvename       := '';

    // DRC
    FDbVMin                  := 0.95;
    FDbVMax                  := 1.05;
    FArGraLowV               := 0.1;
    FArGraHiV                := 0.1;
    FDRCRollAvgWindowLength  := 1;
    FDRCRollAvgWindowLengthIntervalUnit := 's';
    FDRCVAvgWindowLengthSec  := 1.0;

    // volt-watt
    Fvoltwatt_curve_size     := 0;
    Fvoltwatt_curve          := nil;
    Fvoltwatt_curvename      := '';
    FvoltwattCH_curve_size   := 0;
    FvoltwattCH_curve        := nil;
    FvoltwattCH_curvename    := '';

    // AVR
    Fv_setpoint              := 1.0;
    CtrlModel                := 0;        // Linear mode

    InitPropertyValues(0);

  end;

destructor TInvControlObj.Destroy;
  begin
    ElementName := '';
    Finalize(FMonBuses);
    Finalize(FMonBusesNodes);
    Finalize(Fv_setpoint);
    setlength(CtrlVars,0);

    if Assigned(FMonBusesVbase) then ReallocMem(FMonBusesVbase, 0);

    Inherited Destroy;
  end;

procedure TInvControlObj.RecalcElementData(ActorID : Integer);

  VAR
    i       :Integer;

  begin

    if FDERPointerList.ListSize = 0 then  MakeDERList;

    if FDERPointerList.ListSize > 0  then
    {Setting the terminal of the InvControl device to same as the 1st PVSystem/Storage element}
    { This sets it to a realistic value to avoid crashes later }
      begin
        MonitoredElement :=  TDSSCktElement(FDERPointerList.Get(1));   // Set MonitoredElement to 1st elemnent in list
        Setbus(1, MonitoredElement.Firstbus);
      end;

    for i := 1 to FDERPointerList.ListSize do
      begin
        with CtrlVars[i] do
        Begin
          // User ControlledElement[] as the pointer to the PVSystem/Storage elements
          ControlledElement :=  TPCElement(FDERPointerList.Get(i));  // pointer to i-th PVSystem/Storage element
          SetLength(cBuffer, SizeOF(Complex) * ControlledElement.Yorder );


          ControlledElement.ActiveTerminalIdx := 1; // Make the 1 st terminal active
          Nphases := ControlledElement.NPhases;
          Nconds  := Nphases;
          FRollAvgWindow.BuffLength    := FRollAvgWindowLength; // TEMc
          FDRCRollAvgWindow.BuffLength := FDRCRollAvgWindowLength;

          // for all modes other than VW and WATTPF, PF priority is not allowed
          if ((Mode <> VOLTWATT) and (Mode <> WATTPF)) Then
          Begin
              if ControlledElement.DSSClassName = 'PVSystem'     then TPVSystemObj(ControlledElement).PVSystemVars.PF_Priority := FALSE
              else if ControlledElement.DSSClassName = 'Storage' then  TStorageObj(ControlledElement).StorageVars.PF_Priority := FALSE;
          End;

          //FdeltaQFactor[i]                := FdeltaQ_factor;
          //FdeltaPFactor[i]                := FdeltaP_factor;

          if Length(FMonBuses)=0 then FUsingMonBuses := FALSE else FUsingMonBuses := TRUE;

          if (ControlledElement <> Nil) then UpdateDERParameters(i)
          else
          begin
            ControlledElement := nil;
            DoErrorMsg('InvControl: "' + Self.Name + '"',
                            'Controlled Element "' + FDERNameList.Strings[i-1] + '" Not Found.',
                            ' PVSystem or Storage object must be defined previously.', 361);
          end;
        End;
      end;

  end;

procedure TInvControlObj.MakePosSequence(ActorID : Integer);

// ***  This assumes the PVSystem/Storage devices have already been converted to pos seq

  begin

    if FDERPointerList.ListSize = 0 then  RecalcElementData(ActorID);
    Nphases := 3;
    Nconds := 3;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));

    if FDERPointerList.ListSize > 0  then
    {Setting the terminal of the InvControl device to same as the 1st PVSystem/Storage element}
    { This sets it to a realistic value to avoid crashes later }
      begin
        MonitoredElement :=  TDSSCktElement(FDERPointerList.Get(1));   // Set MonitoredElement to 1st PVSystem/Storage in list
        Setbus(1, MonitoredElement.Firstbus);
        Nphases := MonitoredElement.NPhases;
        Nconds := Nphases;
      end;
    inherited;
  end;

procedure TInvControlObj.CalcYPrim(ActorID : Integer);
  begin
    // leave YPrims as nil and they will be ignored
    // Yprim is zeroed when created.  Leave it as is.
    //  if YPrim=nil then YPrim := TcMatrix.CreateMatrix(Yorder);
  end;

procedure TInvControlObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);
  VAR
    i:Integer;
  begin
    // Control is a zero current source
    for i := 1 to Fnconds do Curr^[i] := CZERO;
  end;

procedure TInvControlObj.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);
  VAR
    i:Integer;
  begin
    // Control is a zero current source
    for i := 1 to Fnconds do Curr^[i] := CZERO;
  end;

procedure TInvControlObj.DumpProperties(Var F:TextFile; Complete:Boolean);
  VAR
    i:Integer;

  begin
    Inherited DumpProperties(F,Complete);

    with ParentClass do
      for i := 1 to NumProperties do
        begin
          Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
        end;

      if Complete then
        begin
          Writeln(F);
        end;
  end;

procedure TInvControlObj.DoPendingAction(Const Code, ProxyHdl:Integer;ActorID : Integer);

  VAR
    k                                           : Integer;
    DERelem                                     : TPCElement;

  begin

    for k := 1 to FDERPointerList.ListSize do
    begin
      with CtrlVars[k] do
      Begin
        DERelem := ControlledElement;

        // Calculates QHeadRoom
        Calc_QHeadRoom(k, ActorID);
        if QHeadRoom <> 0.0 then FPriorvarspu  := FPriorvars/QHeadRoom;

        // Calculates PBase
        Calc_PBase(k, ActorID);
        FPriorWattspu  := FPriorWatts/PBase;

        // Calculates kW_out_desiredpu. Used for VW and VV_VW
        kW_out_desiredpu := kW_out_desired / PBase;

        // -------------------Smart Inverter Functions------------------------//
        {Smart Inverter volt-var function}
        if(ControlMode = VOLTVAR) and (CombiControlMode = NONE_COMBMODE) and (PendingChange[k]=CHANGEVARLEVEL) then
          begin
            // Set var mode to VARMODEKVAR to indicate we might change kvar
            DERelem.Set_VWmode( FALSE );
            DERelem.Set_Varmode( VARMODEKVAR );
            DERelem.Set_VVmode( TRUE );
             //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireVVpu[k]
            CalcQVVcurve_desiredpu(k, ActorID);

            // LPF or RF activated
            if (RateofChangeMode = LPF) then
              begin
                CalcLPF(k, 'VARS', QDesireVVpu, ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu, ActorID);
                QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);
              end
            else if (RateofChangeMode = RISEFALL) then
              begin
                CalcRF(k, 'VARS', QDesireVVpu, ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu, ActorID);
                QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);
              end
            else
              begin
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireVVpu, ActorID);
                QDesireEndpu := Min(abs(QDesireVVpu), abs(QDesireLimitedpu)) * sign(QDesireVVpu);
              end;

            // Calculates QDesiredVV through the convergence algorithm
            CalcVoltVar_vars(k, ActorID);

            //--------------------------------------------- end Main process ---------------------------------------------//

            // Sets PVSystem/Storage's kvar_out
            if DERelem.DSSClassName = 'PVSystem' then TPVSystemObj(DERelem).Presentkvar := QDesiredVV
            else TStorageObj(DERelem).kvarRequested := QDesiredVV;

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if DERelem.DSSClassName = 'PVSystem' then
              begin
                TPVSystemObj(DERelem).SetNominalPVSystemOuput(ActorID);

                if QDesiredVV >= 0.0 then Qoutputpu := TPVSystemObj(DERelem).Presentkvar / QHeadroom
                else Qoutputpu := TPVSystemObj(DERelem).Presentkvar / QHeadroomNeg;
              end
            else
              begin
                TStorageObj(DERelem).SetNominalStorageOutput(ActorID);

                if QDesiredVV >= 0.0 then Qoutputpu := TStorageObj(DERelem).Presentkvar / QHeadroom
                else Qoutputpu := TStorageObj(DERelem).Presentkvar / QHeadroomNeg;
              end;

            // Values used in convergence
            QoutputVVpu := Qoutputpu;
            FAvgpVpuPrior := FPresentVpu;

            // Values used in CalcQVVcurve_desiredpu
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                QOld   := TPVSystemObj(DERelem).Presentkvar;
                QOldVV := TPVSystemObj(DERelem).Presentkvar;

              if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name +', '+ TPVSystemObj(DERelem).QualifiedName,
                                                    Format('VOLTVAR mode requested PVSystem output var level to**, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredVV, TPVSystemObj(DERelem).Presentkvar]),ActorID);
              end
            else
              begin
                QOld   := TStorageObj(DERelem).Presentkvar;
                QOldVV := TStorageObj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name +', '+  TStorageObj(DERelem).QualifiedName,
                                                    Format('VOLTVAR mode requested Storage output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredVV, TStorageObj(DERelem).Presentkvar]),ActorID);

              end;
          end

        {Smart Inverter active voltage regulation function}
        else if(ControlMode = AVR) and (CombiControlMode = NONE_COMBMODE) and (PendingChange[k]=CHANGEVARLEVEL) then
          begin
            // Set var mode to VARMODEKVAR to indicate we might change kvar
            DERelem.Set_VWmode( FALSE );
            DERelem.Set_Varmode( VARMODEKVAR );
            DERelem.Set_AVRmode( TRUE );
            //--------------------------------------------- Main process ---------------------------------------------//

            if ActiveCircuit[ActorID].Solution.ControlIteration = 1 then
              begin
                FAvgpVpuPrior := FPresentVpu;
                FAvgpAVRVpuPrior := FPresentVpu;

                 // Sets PVSystem/Storage's kvar_out
                if ControlledElement.DSSClassName = 'PVSystem' then TPVSystemObj(DERelem).Presentkvar := QHeadRoom / 2
                else TStorageObj(DERelem).kvarRequested := QHeadRoom / 2;
              end

            else if ActiveCircuit[ActorID].Solution.ControlIteration = 2 then
              begin
                // Sets PVSystem/Storage's kvar_out
                if ControlledElement.DSSClassName = 'PVSystem' then
                  DQDV := abs(TPVSystemObj(DERelem).Presentkvar / QHeadRoom / (FPresentVpu - FAvgpVpuPrior))
                else
                  DQDV := abs(TStorageObj(DERelem).kvarRequested / QHeadRoom / (FPresentVpu - FAvgpVpuPrior));
              end

            else
              begin
                // Calculates QDesireAVRpu
                CalcQAVR_desiredpu(k, ActorID);


                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireAVRpu, ActorID);
                QDesireEndpu := Min(abs(QDesireAVRpu), abs(QDesireLimitedpu)) * sign(QDesireAVRpu);

                if abs(QDesireEndpu - QDesireLimitedpu) < 0.05  then
                  Fv_setpointLimited := FPresentVpu
                else
                  Fv_setpointLimited := Fv_setpoint;

                // Calculates QDesiredVV through the convergence algorithm
                CalcAVR_vars(k, ActorID);

                //--------------------------------------------- end Main process ---------------------------------------------//

                // Sets PVSystem/Storage's kvar_out
                if ControlledElement.DSSClassName = 'PVSystem' then TPVSystemObj(DERelem).Presentkvar := QDesiredAVR
                else TStorageObj(DERelem).kvarRequested := QDesiredAVR;

                // Uptates PresentkW and Presentkvar considering watt and var priorities
                if ControlledElement.DSSClassName = 'PVSystem' then
                  begin
                    TPVSystemObj(DERelem).SetNominalPVSystemOuput(ActorID);

                    if QDesiredAVR >= 0.0 then Qoutputpu := TPVSystemObj(DERelem).Presentkvar / QHeadroom
                    else Qoutputpu := TPVSystemObj(DERelem).Presentkvar / QHeadroomNeg;
                  end
                else
                  begin
                    TStorageObj(DERelem).SetNominalStorageOutput(ActorID);

                    if QDesiredAVR >= 0.0 then Qoutputpu := TStorageObj(DERelem).Presentkvar / QHeadroom
                    else Qoutputpu := TStorageObj(DERelem).Presentkvar / QHeadroomNeg;
                  end;

              // Values used in convergence
              QoutputAVRpu := Qoutputpu;
              FAvgpVpuPrior := FPresentVpu;

              // Values used in CalcQVVcurve_desiredpu
              if ControlledElement.DSSClassName = 'PVSystem' then
                begin
                  QOld   := TPVSystemObj(DERelem).Presentkvar;
                  QOldAVR := TPVSystemObj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name +', '+ TPVSystemObj(DERelem).QualifiedName,
                                                      Format('VOLTVAR mode requested PVSystem output var level to**, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                             [QDesiredAVR, TPVSystemObj(DERelem).Presentkvar]),ActorID);
                end
              else
                begin
                  QOld   := TStorageObj(DERelem).Presentkvar;
                  QOldAVR := TStorageObj(DERelem).Presentkvar;

                  if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name +', '+  TStorageObj(DERelem).QualifiedName,
                                                      Format('VOLTVAR mode requested Storage output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.',
                                                             [QDesiredAVR, TStorageObj(DERelem).Presentkvar]),ActorID);

                end;
              end;
          end

        {Smart Inverter watt-pf function}
        else if(ControlMode = WATTPF) and (CombiControlMode = NONE_COMBMODE) and (PendingChange[k]=CHANGEVARLEVEL) then
          begin
            // Set var mode to VARMODEKVAR to indicate we might change kvar

            DERelem.Set_VWmode( FALSE );
            DERelem.Set_Varmode( VARMODEKVAR );
            DERelem.Set_WPmode( TRUE );

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireWPpu
            CalcQWPcurve_desiredpu(k, ActorID);

            // Checks kVA (watt priority) and kvarlimit limits
            Check_Qlimits(k, QDesireWPpu, ActorID);
            QDesireEndpu := Min(abs(QDesireWPpu), abs(QDesireLimitedpu)) * sign(QDesireWPpu);

            // Calculates QDesiredWP through the convergence algorithm
            CalcWATTPF_vars(k, ActorID);

            //--------------------------------------------- end Main process ---------------------------------------------//
            // Sets PVSystem/Storage's pf_wp_nominal
            if ControlledElement.DSSClassName = 'PVSystem' then TPVSystemObj(DERelem).pf_wp_nominal := pf_wp_nominal
            else TStorageObj(DERelem).kvarRequested := QDesiredWP;

            // Sets PVSystem/Storage's kvar_out
            if ControlledElement.DSSClassName = 'PVSystem' then TPVSystemObj(DERelem).Presentkvar := QDesiredWP
            else TStorageObj(DERelem).kvarRequested := QDesiredWP;

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                TPVSystemObj(DERelem).SetNominalPVSystemOuput(ActorID);

                if QDesiredWP >= 0.0 then Qoutputpu := TPVSystemObj(DERelem).Presentkvar / QHeadroom
                else Qoutputpu := TPVSystemObj(DERelem).Presentkvar / QHeadroomNeg;
              end
            else
              begin
                TStorageObj(DERelem).SetNominalStorageOutput(ActorID);

                if QDesiredWP >= 0.0 then Qoutputpu := TStorageObj(DERelem).Presentkvar / QHeadroom
                else Qoutputpu := TStorageObj(DERelem).Presentkvar / QHeadroomNeg;
              end;

            // Values used in convergence
            QoutputVVpu := Qoutputpu;
            FAvgpVpuPrior := FPresentVpu;

            // Values used in CalcQVVcurve_desiredpu
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                QOld   := TPVSystemObj(DERelem).Presentkvar;
                QOldVV := TPVSystemObj(DERelem).Presentkvar;

              if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name +', '+ TPVSystemObj(DERelem).QualifiedName,
                                                    Format('WATTPF mode requested PVSystem output var level to**, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredWP, TPVSystemObj(DERelem).Presentkvar]),ActorID);
              end
            else
              begin
                QOld   := TStorageObj(DERelem).Presentkvar;
                QOldVV := TStorageObj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name +', '+  TStorageObj(DERelem).QualifiedName,
                                                    Format('WATTPF mode requested Storage output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredWP, TStorageObj(DERelem).Presentkvar]),ActorID);

              end;
          end

        {Smart Inverter watt-var function}
        else if(ControlMode = WATTVAR) and (CombiControlMode = NONE_COMBMODE) and (PendingChange[k]=CHANGEVARLEVEL) then
          begin
            // Set var mode to VARMODEKVAR to indicate we might change kvar

            DERelem.Set_VWmode( FALSE );
            DERelem.Set_Varmode( VARMODEKVAR );
            DERelem.Set_WVmode( TRUE );


            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireWVpu
            CalcQWVcurve_desiredpu(k, ActorID);

            // Checks kVA (watt priority) and kvarlimit limits
            Check_Qlimits_WV(k, QDesireWVpu, ActorID);
            QDesireEndpu := Min(abs(QDesireWVpu), abs(QDesireLimitedpu)) * sign(QDesireWVpu);

            // It checks kVA or Q limits and makes sure the final P and Q stay in the watt-var curve (PauloRadatz - 2/16/2021)
            Calc_PQ_WV(k, ActorID);

            //--------------------------------------------- end Main process ---------------------------------------------//

            // Sets PVSystem/Storage's kvar_out
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                TPVSystemObj(DERelem).Presentkvar := QDesiredWV;
                TPVSystemObj(DERelem).PresentkW := PLimitEndpu * Min(FkVARating, FDCkWRated);
              end
            else TStorageObj(DERelem).kvarRequested := QDesiredWV;

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                TPVSystemObj(DERelem).SetNominalPVSystemOuput(ActorID);

                if QDesiredWV >= 0.0 then Qoutputpu := TPVSystemObj(DERelem).Presentkvar / QHeadroom
                else Qoutputpu := TPVSystemObj(DERelem).Presentkvar / QHeadroomNeg;
              end
            else
              begin
                TStorageObj(DERelem).SetNominalStorageOutput(ActorID);

                if QDesiredWV >= 0.0 then Qoutputpu := TStorageObj(DERelem).Presentkvar / QHeadroom
                else Qoutputpu := TStorageObj(DERelem).Presentkvar / QHeadroomNeg;
              end;

            // Values used in convergence
            QoutputVVpu := Qoutputpu;
            FAvgpVpuPrior := FPresentVpu;

            // Values used in CalcQVVcurve_desiredpu
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                QOld   := TPVSystemObj(DERelem).Presentkvar;
                QOldVV := TPVSystemObj(DERelem).Presentkvar;

              if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name +', '+ TPVSystemObj(DERelem).QualifiedName,
                                                    Format('WATTVAR mode requested PVSystem output var level to**, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredWV, TPVSystemObj(DERelem).Presentkvar]),ActorID);
              end
            else
              begin
                QOld   := TStorageObj(DERelem).Presentkvar;
                QOldVV := TStorageObj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name +', '+  TStorageObj(DERelem).QualifiedName,
                                                    Format('WATTVAR mode requested Storage output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredWV, TStorageObj(DERelem).Presentkvar]),ActorID);

              end;
          end

        {Smart Inverter DRC function}
        else if(ControlMode = DRC) and (CombiControlMode = NONE_COMBMODE) and (PendingChange[k]=CHANGEVARLEVEL) then
          begin

            // Set var mode to VARMODEKVAR to indicate we might change kvar
            DERelem.Set_VWmode( FALSE );
            DERelem.Set_Varmode( VARMODEKVAR );
            DERelem.Set_DRCmode( TRUE );

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireDRCpu
            CalcQDRC_desiredpu(k, ActorID);

            // LPF or RF activated
            if (RateofChangeMode = LPF) then
              begin
                CalcLPF(k, 'VARS', QDesireDRCpu, ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu, ActorID);
                QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);
              end
            else if (RateofChangeMode = RISEFALL) then
              begin
                CalcRF(k, 'VARS', QDesireDRCpu, ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu, ActorID);
                QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);
              end
            else
              begin
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireDRCpu, ActorID);
                QDesireEndpu := Min(abs(QDesireDRCpu), abs(QDesireLimitedpu)) * sign(QDesireDRCpu);
              end;

            // Calculates QDesiredDRC
            CalcDRC_vars(k, ActorID);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kvar_out
            if ControlledElement.DSSClassName = 'PVSystem' then TPVSystemObj(DERelem).Presentkvar := QDesiredDRC
            else TStorageObj(DERelem).kvarRequested := QDesiredDRC;

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                TPVSystemObj(DERelem).SetNominalPVSystemOuput(ActorID);

                if QDesiredDRC >= 0.0 then Qoutputpu := TPVSystemObj(DERelem).Presentkvar / QHeadroom
                else Qoutputpu := TPVSystemObj(DERelem).Presentkvar / QHeadroomNeg;
              end
            else
              begin
                TStorageObj(DERelem).SetNominalStorageOutput(ActorID);

                if QDesiredDRC >= 0.0 then Qoutputpu := TStorageObj(DERelem).Presentkvar / QHeadroom
                else Qoutputpu := TStorageObj(DERelem).Presentkvar / QHeadroomNeg;
              end;

            // Values used in convergence
            QoutputDRCpu := Qoutputpu;
            FAvgpDRCVpuPrior := FPresentDRCVpu;

            // Values used in CalcDRC_vars
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                QOld    := TPVSystemObj(DERelem).Presentkvar;
                QOldDRC := TPVSystemObj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name +', '+ TPVSystemObj(DERelem).QualifiedName,
                                                    Format('DRC mode requested PVSystem output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredDRC, TPVSystemObj(DERelem).Presentkvar]),ActorID);
              end
            else
              begin
                QOld    := TStorageObj(DERelem).Presentkvar;
                QOldDRC := TStorageObj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name +', '+  TStorageObj(DERelem).QualifiedName,
                                                    Format('DRC mode requested Storage output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredDRC, TStorageObj(DERelem).Presentkvar]),ActorID);

              end;
          end

        {Smart Inverter VV_DRC function}
        else if(ControlMode = NONE_MODE) and (CombiControlMode = VV_DRC) and (PendingChange[k]=CHANGEDRCVVARLEVEL) then
          begin

            // Set var mode to VARMODEKVAR to indicate we might change kvar
            DERelem.Set_VWmode( FALSE );
            DERelem.Set_Varmode( VARMODEKVAR );
            DERelem.Set_VVmode( TRUE );
            DERelem.Set_DRCmode( TRUE );

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireVVpu and  QDesireDRCpu
            CalcQVVcurve_desiredpu(k, ActorID);
            CalcQDRC_desiredpu(k, ActorID);

            // LPF or RF activated
            if (RateofChangeMode = LPF) then
              begin
                CalcLPF(k, 'VARS', QDesireVVpu + QDesireDRCpu, ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu, ActorID);
                QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);
              end
            else if (RateofChangeMode = RISEFALL) then
              begin
                CalcRF(k, 'VARS', QDesireVVpu + QDesireDRCpu, ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu, ActorID);
                QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);
              end
            else
              begin
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireVVpu + QDesireDRCpu, ActorID);
                QDesireEndpu := Min(abs(QDesireVVpu + QDesireDRCpu), abs(QDesireLimitedpu)) * sign(QDesireVVpu + QDesireDRCpu);
              end;

            // Calculates QDesiredVVDRC
            CalcVVDRC_vars(k,ActorID);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kvar_out
            if ControlledElement.DSSClassName = 'PVSystem' then TPVSystemObj(DERelem).Presentkvar := QDesiredVVDRC
            else TStorageObj(DERelem).kvarRequested := QDesiredVVDRC;

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                TPVSystemObj(DERelem).SetNominalPVSystemOuput(ActorID);

                if QDesiredVVDRC >= 0.0 then Qoutputpu := TPVSystemObj(DERelem).Presentkvar / QHeadroom
                else Qoutputpu := TPVSystemObj(DERelem).Presentkvar / QHeadroomNeg;
              end
            else
              begin
                TStorageObj(DERelem).SetNominalStorageOutput(ActorID);

                if QDesiredVVDRC >= 0.0 then Qoutputpu := TStorageObj(DERelem).Presentkvar / QHeadroom
                else Qoutputpu := TStorageObj(DERelem).Presentkvar / QHeadroomNeg;
              end;

            // Values used in convergence
            QoutputVVDRCpu := Qoutputpu;
            FAvgpVpuPrior := FPresentVpu;
            FAvgpDRCVpuPrior := FPresentDRCVpu;

            // Values used in CalcQVVcurve_desiredpu and CalcVVDRC_vars
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                QOld      := TPVSystemObj(DERelem).Presentkvar;
                QOldVVDRC := TPVSystemObj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name +', '+ TPVSystemObj(DERelem).QualifiedName,
                                                    Format('**VV_DRC mode requested PVSystem output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredVVDRC, TPVSystemObj(DERelem).Presentkvar]),ActorID);
              end
            else
              begin
                QOld      := TStorageObj(DERelem).Presentkvar;
                QOldVVDRC := TStorageObj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name +', '+  TStorageObj(DERelem).QualifiedName,
                                                    Format('**VV_DRC mode requested Storage output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredVVDRC, TStorageObj(DERelem).Presentkvar]),ActorID);
              end;
          end

        {Smart Inverter volt-watt function}
        else if(ControlMode = VOLTWATT) and (CombiControlMode = NONE_COMBMODE) and (PendingChange[k]=CHANGEWATTLEVEL) then
          begin

            DERelem.Set_VWmode( TRUE );
            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QVWcurve_limitpu
            CalcPVWcurve_limitpu(k, ActorID);

            // LPF or RF activated
            if (RateofChangeMode = LPF) then
              begin
                CalcLPF(k, 'WATTS', PLimitVWpu, ActorID);
                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitOptionpu, ActorID);
                PLimitEndpu := Min(PLimitLimitedpu, PLimitOptionpu);
              end
            else if (RateofChangeMode = RISEFALL) then
              begin
                CalcRF(k, 'WATTS', PLimitVWpu, ActorID);
                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitOptionpu, ActorID);
                PLimitEndpu := Min(PLimitLimitedpu, PLimitOptionpu);
              end
            else
              begin
                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitVWpu, ActorID);
                PLimitEndpu := Min(abs(PLimitLimitedpu), abs(PLimitVWpu)) * sign(PLimitVWpu);
              end;

            // Calculates PLimitVW through the convergence algorithm
            CalcVoltWatt_watts(k, ActorID);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kW_out
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                TPVSystemObj(DERelem).PresentkW := PLimitVW;

                // Uptates PresentkW and Presentkvar considering watt and var priorities
                TPVSystemObj(DERelem).SetNominalPVSystemOuput(ActorID);

              end
            else
              begin
                TStorageObj(DERelem).kWRequested := PLimitVW;

                // Uptates PresentkW and Presentkvar considering watt and var priorities
                TStorageObj(DERelem).SetNominalStorageOutput(ActorID);
              end;


            // Values used in convergence
            FAvgpVpuPrior := FPresentVpu;
            POldVWpu := PLimitVW / PBase;

            // Flag has to do set to 0 when kW_out is lower than Ptemp (max power allowed from volt-watt function)
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                if ((abs(PLimitVW) > 0.0) and (abs(TPVSystemObj(DERelem).presentkW - PLimitVW) / PLimitVW > 0.0001)) then FVWOperation := 0; // 0.01% is the value chosen at the moment

                if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name+', '+TPVSystemObj(DERelem).QualifiedName,
                                                    Format('**VOLTWATT mode set PVSystem kw output limit to **, kw= %.5g. Actual output is kw= %.5g.',
                                                           [PLimitVW, TPVSystemObj(DERelem).presentkW]),ActorID);
              end
            else
              begin
                if abs(abs(TStorageObj(DERelem).presentkW) - PLimitVW) / PLimitVW > 0.0001 then FVWOperation := 0; // 0.01% is the value chosen at the moment

                if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name+', '+  TStorageObj(DERelem).QualifiedName,
                                                    Format('**VOLTWATT mode set Storage kw output limit to ** kw= %.5g. Actual output is kw= %.5g.',
                                                           [PLimitVW, TStorageObj(DERelem).presentkW]),ActorID);

              end;
          end

        else if(ControlMode = NONE_MODE) and (CombiControlMode = VV_VW) and (PendingChange[k]=CHANGEWATTVARLEVEL) then
          begin

            DERelem.Set_VWmode( TRUE );
            DERelem.Set_Varmode( VARMODEKVAR );
            DERelem.Set_VVmode( TRUE );
            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireVVpu and QVWcurve_limitpu
            CalcPVWcurve_limitpu(k, ActorID);
            CalcQVVcurve_desiredpu(k, ActorID);

            // LPF or RF activated
            if (RateofChangeMode = LPF) then
              begin
                CalcLPF(k, 'VARS', QDesireVVpu, ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu, ActorID);
                QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);

                CalcLPF(k, 'WATTS', PLimitVWpu, ActorID);
                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitOptionpu, ActorID);
                PLimitEndpu := Min(PLimitLimitedpu, PLimitOptionpu);
              end
            else if (RateofChangeMode = RISEFALL) then
              begin
                CalcRF(k, 'VARS', QDesireVVpu, ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu, ActorID);
                QDesireEndpu := Min(abs(QDesireLimitedpu), abs(QDesireOptionpu)) * sign(QDesireOptionpu);

                CalcRF(k, 'WATTS', PLimitVWpu, ActorID);
                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitOptionpu, ActorID);
                PLimitEndpu := Min(PLimitLimitedpu, PLimitOptionpu);
              end
            else
              begin
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireVVpu, ActorID);
                QDesireEndpu := Min(abs(QDesireVVpu), abs(QDesireLimitedpu)) * sign(QDesireVVpu);

                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitVWpu, ActorID);
                PLimitEndpu := Min(abs(PLimitLimitedpu), abs(PLimitVWpu)) * sign(PLimitVWpu);
              end;

            // Calculates PLimitVW and QDesiredVV through the convergence algorithm
            CalcVoltWatt_watts(k, ActorID);
            CalcVoltVar_vars(k, ActorID);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kvar_out and kW_out
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                TPVSystemObj(DERelem).Presentkvar := QDesiredVV;
                TPVSystemObj(DERelem).presentkW   := PLimitVW;
              end
            else
              begin
                TStorageObj(DERelem).kvarRequested := QDesiredVV;
                TStorageObj(DERelem).kWRequested   := PLimitVW;
              end;

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                TPVSystemObj(DERelem).SetNominalPVSystemOuput(ActorID);

                if QDesiredVV >= 0.0 then Qoutputpu := TPVSystemObj(DERelem).Presentkvar / QHeadroom
                else Qoutputpu := TPVSystemObj(DERelem).Presentkvar / QHeadroomNeg;
              end
            else
              begin
                TStorageObj(DERelem).SetNominalStorageOutput(ActorID);

                if QDesiredVV >= 0.0 then Qoutputpu := TStorageObj(DERelem).Presentkvar / QHeadroom
                else Qoutputpu := TStorageObj(DERelem).Presentkvar / QHeadroomNeg;
              end;

            // Values used in convergence
            QoutputVVpu := Qoutputpu;
            FAvgpVpuPrior := FPresentVpu;
            POldVWpu := PLimitVW / PBase;

            // Values used in CalcQVVcurve_desiredpu
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                QOld   := TPVSystemObj(DERelem).Presentkvar;
                QOldVV := TPVSystemObj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name +', '+ TPVSystemObj(DERelem).QualifiedName,
                                                    Format('**VV_VW mode requested PVSystem output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredVV, TPVSystemObj(DERelem).presentkvar]), ActorID);
              end
            else
              begin
                QOld   := TStorageObj(DERelem).Presentkvar;
                QOldVV := TStorageObj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name +', '+ TStorageObj(DERelem).QualifiedName,
                                                    Format('**VV_VW mode requested Storage output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredVV, TStorageObj(DERelem).presentkvar]), ActorID);
              end;

            // Flag has to do set to 0 when kW_out is lower than Ptemp (max power allowed from volt-watt function)
            if ControlledElement.DSSClassName = 'PVSystem' then
              begin
                if abs(TPVSystemObj(DERelem).presentkW - PLimitVW) / PLimitVW > 0.0001 then FVWOperation := 0; // 0.01% is the value chosen at the moment

                if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name+', '+TPVSystemObj(DERelem).QualifiedName,
                                                    Format('**VV_VW mode set PVSystem kw output limit to **, kw= %.5g. Actual output is kw= %.5g.',
                                                           [PLimitVW, TPVSystemObj(DERelem).presentkW]),ActorID);
              end
            else
              begin
                if abs(abs(TStorageObj(DERelem).presentkW) - PLimitVW) / PLimitVW > 0.0001 then FVWOperation := 0; // 0.01% is the value chosen at the moment

                if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name+', '+  TStorageObj(DERelem).QualifiedName,
                                    Format('**VV_VW mode set Storage kw output limit to** kw= %.5g. Actual output is kw= %.5g.',
                                           [PLimitVW, TStorageObj(DERelem).presentkW]),ActorID);
              end;

          end;

        ActiveCircuit[ActorID].Solution.LoadsNeedUpdating := TRUE;
        Set_PendingChange(NONE,k);
        DERelem := Nil;

      end;
    end;

end;

procedure TInvControlObj.GetmonVoltage(ActorID : Integer; var Vpresent: Double; i: Integer; BasekV: Double);
  Var
    j           : integer;
    rBus        : TDSSBus;
    numNodes    : Integer;
    v           : Complex;
    vi          : Complex;
    vj          : Complex;

  begin
    with CtrlVars[i] do
    Begin
      if FUsingMonBuses then
        begin

          for j := 0 to Length(FMonBuses)-1 do
            begin
              FMonBusesIndex := ActiveCircuit[ActorID].BusList.Find(FMonBuses[j]);
              rBus := ActiveCircuit[ActorID].Buses^[FMonBusesIndex];

              if (length(FMonBusesNodes[j]) = 2) then
                begin
                  if not ADiakoptics or (ActorID = 1) then
                  Begin
                    vi := (ActiveCircuit[ActorID].Solution.NodeV^[rBus.GetRef(FMonBusesNodes[j][0])]);
                    vj := (ActiveCircuit[ActorID].Solution.NodeV^[rBus.GetRef(FMonBusesNodes[j][1])]);
                  End
                  else
                  Begin
                    vi := (ActiveCircuit[ActorID].Solution.VoltInActor1(rBus.GetRef(FMonBusesNodes[j][0])));
                    vj := (ActiveCircuit[ActorID].Solution.VoltInActor1(rBus.GetRef(FMonBusesNodes[j][1])));
                  End;

                cBuffer[j] := cmulreal(Csub(vi, vj), BasekV * 1000.0 / FMonBusesVbase[j+1]);
                v := cBuffer[j];
                end
              else
                begin
                  if not ADiakoptics or (ActorID = 1) then
                    cBuffer[j] := cmulreal(ActiveCircuit[ActorID].Solution.NodeV^[rBus.GetRef(FMonBusesNodes[j][0])], BasekV * 1000.0 / FMonBusesVbase[j+1])
                  else
                    cBuffer[j] := cmulreal(ActiveCircuit[ActorID].Solution.VoltInActor1(rBus.GetRef(FMonBusesNodes[j][0])), BasekV * 1000.0 / FMonBusesVbase[j+1]);
                  v := cBuffer[j];
                end;
            end;

            CASE FMonBusesPhase of
              AVGPHASES:
                begin
                  Vpresent := 0.0;
                  for j := 0 to Length(FMonBuses)-1 do
                    Vpresent := Vpresent + Cabs(cBuffer[j]);
                  Vpresent := Vpresent / Length(FMonBuses);
                end;
              MAXPHASE:
                begin
                  Vpresent := 0.0;
                  for j := 0 to Length(FMonBuses)-1 do
                    Vpresent := Max(Vpresent, Cabs(cBuffer[j]));
                end;
              MINPHASE:
                begin
                  Vpresent := 1.0E50;
                  for j := 0 to Length(FMonBuses)-1 do
                    Vpresent := Min(Vpresent, Cabs(cBuffer[j]));
                end;
              else
                Vpresent := Cabs(cBuffer[FMonBusesPhase]);
            end;
        end

      else
        begin
          ControlledElement.ComputeVTerminal(ActorID);

          numNodes := ControlledElement.NPhases;

          for j := 1 to numNodes do
            cBuffer[j] := ControlledElement.Vterminal^[j];


          CASE FMonBusesPhase of
            AVGPHASES:
              begin
                Vpresent := 0.0;
                for j := 1 to numNodes do
                  Vpresent := Vpresent + Cabs(cBuffer[j]);
                Vpresent := Vpresent / numNodes;
              end;
            MAXPHASE:
              begin
                Vpresent := 0.0;
                for j := 1 to numNodes do
                  Vpresent := Max(Vpresent, Cabs(cBuffer[j]));
              end;
            MINPHASE:
              begin
                Vpresent := 1.0E50;
                for j := 1 to numNodes do
                  Vpresent := Min(Vpresent, Cabs(cBuffer[j]));
              end;
            else
              Vpresent := Cabs(cBuffer[FMonBusesPhase]);
          end;
        end;
    end;
  end;

procedure TInvControlObj.UpdateDERParameters(i: Integer);
  begin
    with CtrlVars[i] do
    Begin
      with ControlledElement do
        if ControlledElement.DSSClassName = 'PVSystem' then
        begin
          with TPVSystemObj(ControlledElement) do
            begin
              CondOffset   := (NTerms-1) * NCondsDER; // for speedy sampling

              FVBase               :=  Vbase;
              FVarFollowInverter   :=  VarFollowInverter;
              FInverterON          :=  InverterON;
              FpresentkW           :=  PresentkW;
              FkVARating           :=  kVARating;
              Fpresentkvar         :=  Presentkvar;
              FkvarLimit           :=  kvarLimit;
              FkvarLimitNeg        :=  kvarLimitNeg;
              FCurrentkvarLimit    :=  CurrentkvarLimit;
              FCurrentkvarLimitNeg :=  CurrentkvarLimitNeg;
              FDCkWRated           :=  Pmpp;
              FpctDCkWRated        :=  puPmpp;
              FEffFactor           :=  PVSystemVars.EffFactor;
              FDCkW                :=  PVSystemVars.PanelkW;
              FPPriority           :=  PVSystemVars.P_Priority;

            end;
        end
        else if ControlledElement.DSSClassName = 'Storage' then
        begin
          with TStorageObj(ControlledElement) do
            begin
              FVBase               :=  Vbase;
              FVarFollowInverter   :=  VarFollowInverter;
              FInverterON          :=  InverterON;
              FpresentkW           :=  PresentkW;
              FkVARating           :=  kVARating;
              Fpresentkvar         :=  Presentkvar;
              FkvarLimit           :=  kvarLimit;
              FkvarLimitNeg        :=  kvarLimitNeg;
              FCurrentkvarLimit    :=  CurrentkvarLimit;
              FCurrentkvarLimitNeg :=  CurrentkvarLimitNeg;
              FDCkWRated           :=  StorageVars.kWrating;
              FpctDCkWRated        :=  pctkWrated;
              FEffFactor           :=  Storagevars.EffFactor;
              FDCkW                :=  0.0; // not using it (using TStorageObj.DCkW directly)
              FPPriority           :=  StorageVars.P_priority;

            end
        end;
    end;
  end;

procedure TInvControlObj.Sample(ActorID : Integer);

VAR
  i                           :Integer;
  basekV                      :Double;
  Vpresent                    :Double;
  PVSys                       :TPVSystemObj;
  Storage                     :TStorageObj;

begin
  PVSys:=nil;Storage:=nil;
  // if list is not defined, go make one from all PVSystem/Storage in circuit
  if FDERPointerList.ListSize=0 then   RecalcElementData(ActorID);

  if (FListSize>0) then
  begin
    // if an InvControl controls more than one PVSystem/Storage, control each one
    // separately based on the PVSystem/Storage's terminal voltages, etc.
    for i := 1 to FDERPointerList.ListSize do
    begin
      UpdateDERParameters(i);
      with CtrlVars[i] do
      Begin
        if ControlledElement.DSSClassName = 'PVSystem' then PVSys := ControlledElement as TPVSystemObj
        else Storage := ControlledElement as TStorageObj;

        BasekV := FVBase / 1000.0; // It's a line-to-ground voltage

        GetmonVoltage(ActorID, Vpresent, i, BasekV);

        // for reporting Vpriorpu correctly in EventLog (this update is normally perform at DoPendingAction)
        if ActiveCircuit[ActorID].Solution.ControlIteration = 1 then
        begin
          FAvgpVpuPrior := FPresentVpu;
          FAvgpDRCVpuPrior := FPresentDRCVpu;
        end;

        kW_out_desired := FpresentkW; // necessary to update kW_out_desired at every control iteration for Storage with SC

        // Help says that it must be used just for vv and vw
        // convert to per-unit on bus' kvbase, or
        // if using averaging window values, then set prior voltage to averaging window
        if(FVoltage_CurveX_ref = 1) and (FRollAvgWindow.Get_AvgVal <> 0.0) then
          FPresentVpu := Vpresent / (FRollAvgWindow.Get_AvgVal)
        else if(FVoltage_CurveX_ref = 2) and (FRollAvgWindow.Get_AvgVal <> 0.0) then
          FPresentVpu := (FRollAvgWindow.Get_AvgVal) / (basekV * 1000.0)
        else FPresentVpu := Vpresent / (BasekV * 1000.0);

        FPresentDRCVpu := Vpresent / (BasekV * 1000.0);

        // Sets internal variables of controlled element.
        // FVreg is the pu voltage used in the volt-var and volt-watt curves
        FVreg := FPresentVpu;
        // First, determine what control mode are we
        if CombiControlMode <> 0 then
        Begin
          // IT's CombiControl mode
          Case CombiControlMode of
          VV_DRC:     begin
                          // Sets internal variables of controlled element.
                          // FVVDRCOperation is a flag which indicates if VVDRC function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
                          if ControlledElement.DSSClassName = 'PVSystem' then
                            begin
                              PVSys.Set_Variable(5,FVreg);
                              PVSys.Set_Variable(6,FDRCRollAvgWindow.Get_AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                              PVSys.Set_Variable(10,FVVDRCOperation);
                            end
                          else
                            begin
                              Storage.Set_Variable(14,FVreg);
                              Storage.Set_Variable(15,FDRCRollAvgWindow.Get_AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                              Storage.Set_Variable(19,FVVDRCOperation);
                            end;

                          // if inverter is off then exit
                          if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then continue;

                          // if the volt-var curve does not exist, exit
                          if Length(Fvvc_curvename) = 0 then
                            begin
                              DoSimpleMsg('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.', 382);
                              exit
                            end;

                          if (ControlledElement.DSSClassName = 'PVSystem') then
                          Begin
                              PVSys.VVmode   := TRUE;
                              PVSys.DRCmode  := TRUE;
                          End
                          else
                          Begin
                              Storage.VVmode   := TRUE;
                              Storage.DRCmode  := TRUE;
                          End;

                          //DRC triggers
                          if(priorDRCRollAvgWindow = 0.0) then
                            begin

                              if (Abs(FPresentDRCVpu - FAvgpDRCVpuPrior) > FVoltageChangeTolerance) or
                                 (Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance)  then
                                begin
                                  // Resets DER state variable only if it has not converged yet
                                  FVVDRCOperation := 0.0;

                                  Set_PendingChange(CHANGEDRCVVARLEVEL,i);

                                  with ActiveCircuit[ActorID].Solution.DynaVars do
                                    ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push
                                      (intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);

                                  if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name+', '+ControlledElement.QualifiedName,
                                                                        Format('**Ready to change var output due to DRC trigger in VV_DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                               [FPresentDRCVpu,FAvgpDRCVpuPrior]),ActorID);
                                end;

                            end;

                            //Trigger from volt-var mode
                          if (((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or
                              (Abs(FPresentDRCVpu - FAvgpDRCVpuPrior) > FVoltageChangeTolerance) or
                                ((Abs(Abs(QoutputVVDRCpu) - Abs(QDesireEndpu)) > FVarChangeTolerance))) or
                                (ActiveCircuit[ActorID].Solution.ControlIteration = 1)) then
                            begin
                              // Resets DER state variable only if it has not converged yet
                              FVVDRCOperation := 0.0;

                              Set_PendingChange(CHANGEDRCVVARLEVEL,i);
                              with  ActiveCircuit[ActorID].Solution.DynaVars do
                                ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);

                              if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name+', '+ControlledElement.QualifiedName,
                                                                    Format('**Ready to change VV_DRC output due to volt-var trigger in VV_DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                           [FPresentVpu,FAvgpVpuPrior]),ActorID);

                            end;
                      end;
          VV_VW:      begin
                        // Sets internal variables of controlled element.
                        // FVVOperation is a flag which indicates if volt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
                        // FVWOperation is a flag which indicates if volt-watt function operates or not
                        // Combined modes operation is shown through TWO flags. It allows us to verify which of the individual function operates or not
                        if ControlledElement.DSSClassName = 'PVSystem' then
                        begin
                          PVSys.Set_Variable(5,FVreg);
                          PVSys.Set_Variable(7,FVVOperation);
                          PVSys.Set_Variable(8,FVWOperation)
                        end
                        else
                        Begin
                          Storage.Set_Variable(14,FVreg);
                          Storage.Set_Variable(16,FVVOperation);
                          Storage.Set_Variable(17,FVWOperation)
                        End;
                        // if inverter is off then exit
                        if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then continue;

                        // if volt-watt curve does not exist, exit
                        if ControlledElement.DSSClassName = 'PVSystem' then
                        begin
                           if Length(Fvoltwatt_curvename) = 0 then
                          begin
                            DoSimpleMsg('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.', 381);
                              exit
                          end;
                        end
                        else
                        begin
                           if (Length(Fvoltwatt_curvename) = 0) and (Length(FvoltwattCH_curvename) = 0) then
                          begin
                            DoSimpleMsg('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.', 381);
                              exit
                          end;
                        end;

                        // if the volt-var curve does not exist, exit
                        if Length(Fvvc_curvename) = 0 then
                          begin
                            DoSimpleMsg('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.', 382);
                              exit
                          end;
                        ControlledElement.Set_VVmode( TRUE );
                        ControlledElement.Set_VWmode( TRUE );
                        // Trigger from volt-watt mode
                        if ((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or (Abs(PLimitEndpu-POldVWpu)>FActivePChangeTolerance) or
                          (ActiveCircuit[ActorID].Solution.ControlIteration = 1)) then

                          begin
                            // Resets DER state variable only if it has not converged yet
                            FVWOperation := 0;
                            Set_PendingChange(CHANGEWATTVARLEVEL,i);
                            with  ActiveCircuit[ActorID].Solution.DynaVars do
                              ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push
                              (intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);
                            if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name+', '+ControlledElement.QualifiedName,
                                                                  Format('**Ready to change VV_VW output due to volt-watt trigger**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                         [FPresentVpu,FAvgpVpuPrior]),ActorID);;
                          end;

                          //Trigger from volt-var mode
                        if (((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or
                          ((Abs(Abs(Qoutputpu) - Abs(QDesireEndpu)) > FVarChangeTolerance))) or
                          (ActiveCircuit[ActorID].Solution.ControlIteration = 1)) then

                          begin

                            // Resets DER state variable only if it has not converged yet
                            FVVOperation := 0;
                            Set_PendingChange(CHANGEWATTVARLEVEL,i);
                            with  ActiveCircuit[ActorID].Solution.DynaVars do
                              ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push
                              (intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);

                            if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name+', '+ControlledElement.QualifiedName,
                                                                  Format('**Ready to change VV_VW output due to volt-var trigger**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                         [FPresentVpu,FAvgpVpuPrior]),ActorID);
                          end;
                      end
          else
            {Do nothing}
          End;
        End
        else if ControlMode <> 0 then
        Begin
          case ControLMode of
          // volt-watt control mode
          VOLTWATT:   begin
                        // Sets internal variables of controlled element.
                        // FVWOperation is a flag which indicates if volt-watt function operates or not
                        if ControlledElement.DSSClassName = 'PVSystem' then
                        Begin
                          PVSys.Set_Variable(5,FVreg);
                          PVSys.Set_Variable(8,FVWOperation)
                        end
                        else
                        Begin
                          Storage.Set_Variable(14,FVreg);
                          Storage.Set_Variable(17,FVWOperation)
                        End;


                        if (FInverterON = FALSE) then continue;

                        if ControlledElement.DSSClassName = 'PVSystem' then
                          begin
                            if Length(Fvoltwatt_curvename) = 0 then
                              begin
                                DoSimpleMsg('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.', 381);
                                exit
                              end;
                          end
                        else
                          begin
                              if (Length(Fvoltwatt_curvename) = 0) and (Length(FvoltwattCH_curvename) = 0) then
                                begin
                                  DoSimpleMsg('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.', 381);
                                  exit
                                end;

                          end;

                        if (ControlledElement.DSSClassName = 'PVSystem') then PVSys.VWmode  := TRUE
                        else Storage.VWmode  := TRUE;

                        if ((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or (Abs(PLimitEndpu-POldVWpu)>FActivePChangeTolerance) or
                          (ActiveCircuit[ActorID].Solution.ControlIteration = 1))  then
                          begin

                            // Resets DER state variable only if it has not converged yet
                            FVWOperation := 0;

                            Set_PendingChange(CHANGEWATTLEVEL,i);

                            with  ActiveCircuit[ActorID].Solution.DynaVars do
                              ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push
                              (intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);
                            if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name+', '+ControlledElement.QualifiedName,
                                                                  Format('**Ready to limit watt output due to VOLTWATT mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                         [FPresentVpu,FAvgpVpuPrior]),ActorID);
                          end;
                      end;
          // Active voltage regulation control mode
          AVR:        begin
                        // Sets internal variables of PVSystem/Storage.
                        // FAVROperation is a flag which indicates if volt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
                        // if inverter is off then exit
                        if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then continue;

                        if (ControlledElement.DSSClassName = 'PVSystem') then PVSys.AVRmode  := TRUE
                        else Storage.VVmode  := TRUE;
                          //Trigger from AVR mode
                        if (((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or
                            ((Abs(Abs(QoutputAVRpu) - Abs(QDesireEndpu)) > FVarChangeTolerance)) or
                            (Abs(FPresentVpu - Fv_setpointLimited) > FVoltageChangeTolerance)) or
                            (ActiveCircuit[ActorID].Solution.ControlIteration = 1)) then
                          begin
                            // Resets DER state variable only if it has not converged yet
                            FAVROperation := 0;
                            Set_PendingChange(CHANGEVARLEVEL,i);
                            with  ActiveCircuit[ActorID].Solution.DynaVars do
                              ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push(intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);
                            if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name+', '+ControlledElement.QualifiedName,
                                                                  Format('**Ready to change var output due to AVR trigger in AVR mode**, Vavgpu= %.5g, VPriorpu=%.5g, Vsetpoint=%.5g, VsetpointLimited=%.5g',
                                                                         [FPresentVpu,FAvgpVpuPrior, Fv_setpoint, Fv_setpointLimited]),ActorID);
                          end;
                      end;
          // volt-var control mode
          VOLTVAR:    begin
                        // Sets internal variables of PVSystem/Storage.
                        // FVVOperation is a flag which indicates if volt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)

                        if ControlledElement.DSSClassName = 'PVSystem' then
                          begin
                            PVSys.Set_Variable(5,FVreg);
                            PVSys.Set_Variable(7,FVVOperation);
                          end
                        else
                          begin
                            Storage.Set_Variable(14,FVreg);
                            Storage.Set_Variable(16,FVVOperation);
                          end;

                        // if inverter is off then exit
                        if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then continue;
                          if Length(Fvvc_curvename) = 0 then
                          begin
                            DoSimpleMsg('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.', 382);
                            exit
                          end;
                        if (ControlledElement.DSSClassName = 'PVSystem') then PVSys.VVmode  := TRUE
                        else Storage.VVmode  := TRUE;
                          //Trigger from volt-var mode
                        if (((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or
                            ((Abs(Abs(QoutputVVpu) - Abs(QDesireEndpu)) > FVarChangeTolerance))) or
                            (ActiveCircuit[ActorID].Solution.ControlIteration = 1)) then
                         begin

                          // Resets DER state variable only if it has not converged yet
                          FVVOperation := 0;
                          Set_PendingChange(CHANGEVARLEVEL,i);
                          with  ActiveCircuit[ActorID].Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push(intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);
                          if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name+', '+ControlledElement.QualifiedName,
                                                                Format('**Ready to change var output due to volt-var trigger in volt-var mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                       [FPresentVpu,FAvgpVpuPrior]),ActorID);
                        end;
                      end;
          // watt-pf control mode
          WATTPF:     begin
                        // Sets internal variables of PVSystem/Storage.
                        // FWPOperation is a flag which indicates if watt-pf function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
                        if ControlledElement.DSSClassName = 'PVSystem' then
                        begin
                          PVSys.Set_Variable(5,FVreg);
                          PVSys.Set_Variable(11,FWPOperation);
                        end
                        else
                        begin
                          Storage.Set_Variable(14,FVreg);
                          Storage.Set_Variable(16,FWPOperation);
                        end;

                        // if inverter is off then exit
                        if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then continue;
                          if Length(Fwattpf_curvename) = 0 then
                          begin
                            DoSimpleMsg('XY Curve object representing wattpf_curve does not exist or is not tied to InvControl.', 382);
                            exit
                          end;

                        if (ControlledElement.DSSClassName = 'PVSystem') then PVSys.WPmode  := TRUE
                        else Storage.WPmode  := TRUE;
                          //Trigger from volt-var mode
                        if (((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or
                            ((Abs(Abs(QoutputVVpu) - Abs(QDesireEndpu)) > FVarChangeTolerance))) or
                            (ActiveCircuit[ActorID].Solution.ControlIteration = 1)) then
                        begin
                          // Resets DER state variable only if it has not converged yet
                          FWPOperation := 0;
                          Set_PendingChange(CHANGEVARLEVEL,i);
                          with  ActiveCircuit[ActorID].Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push(intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);
                          if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name+', '+ControlledElement.QualifiedName,
                                                                Format('**Ready to change var output due to watt-pf trigger in watt-pf mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                       [FPresentVpu,FAvgpVpuPrior]),ActorID);
                        end;
                      end;
          // watt-var control mode
          WATTVAR:    begin
                        // Sets internal variables of PVSystem/Storage.
                        // FWVOperation is a flag which indicates if watt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
                        if ControlledElement.DSSClassName = 'PVSystem' then
                          begin
                            PVSys.Set_Variable(5,FVreg);
                            PVSys.Set_Variable(12,FWVOperation);        //CHANGE HERE
                          end
                        else
                          begin
                            Storage.Set_Variable(14,FVreg);
                            Storage.Set_Variable(16,FWVOperation);
                          end;
                        // if inverter is off then exit
                        if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then continue;
                          if Length(Fwattvar_curvename) = 0 then
                            begin
                              DoSimpleMsg('XY Curve object representing wattvar_curve does not exist or is not tied to InvControl.', 382);
                              exit
                            end;
                        if (ControlledElement.DSSClassName = 'PVSystem') then PVSys.WVmode := TRUE
                        else Storage.WVmode  := TRUE;
                          //Trigger from volt-var mode
                        if (((Abs(FPresentVpu - FAvgpVpuPrior) > FVoltageChangeTolerance) or
                            ((Abs(Abs(QoutputVVpu) - Abs(QDesireEndpu)) > FVarChangeTolerance))) or
                            (ActiveCircuit[ActorID].Solution.ControlIteration = 1)) then
                          begin
                            // Resets DER state variable only if it has not converged yet
                            FWVOperation := 0;
                            Set_PendingChange(CHANGEVARLEVEL,i);
                            with  ActiveCircuit[ActorID].Solution.DynaVars do
                              ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push(intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);
                            if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name+', '+ControlledElement.QualifiedName,
                                                                  Format('**Ready to change var output due to watt-var trigger in watt-var mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                         [FPresentVpu,FAvgpVpuPrior]),ActorID);
                          end;
                      end;
          // dynamic reactive current control mode
          DRC:        begin
                        // Sets internal variables of PVSystem/Storage.
                        // FDRCOperation is a flag which indicates if DRC function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)

                        if ControlledElement.DSSClassName = 'PVSystem' then
                        begin
                          PVSys.Set_Variable(5,FVreg);
                          PVSys.Set_Variable(6,FDRCRollAvgWindow.Get_AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                          PVSys.Set_Variable(9,FDRCOperation);
                        end
                        else
                        begin
                          Storage.Set_Variable(14,FVreg);
                          Storage.Set_Variable(15,FDRCRollAvgWindow.Get_AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                          Storage.Set_Variable(18,FDRCOperation);
                        end;

                        // if inverter is off then exit
                        if (FInverterON = FALSE) and (FVarFollowInverter = TRUE) then continue;

                        //DRC triggers
                        if(priorDRCRollAvgWindow = 0.0) then
                          begin

                            if ((Abs(FPresentDRCVpu - FAvgpDRCVpuPrior) > FVoltageChangeTolerance))  then
                              begin

                                // Resets DER state variable only if it has not converged yet
                                FDRCOperation := 0;


                                Set_PendingChange(CHANGEVARLEVEL,i);

                                with ActiveCircuit[ActorID].Solution.DynaVars do
                                  ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push
                                  (intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);

                                if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name+', '+ControlledElement.QualifiedName,
                                                                      Format('**Ready to change var output due to DRC trigger in DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                             [FPresentDRCVpu,FAvgpDRCVpuPrior]),ActorID);
                              end;
                          end;
                        if (ControlledElement.DSSClassName = 'PVSystem') then PVSys.DRCmode  := TRUE
                        else Storage.DRCmode  := TRUE;
                        if ((Abs(FPresentDRCVpu - FAvgpDRCVpuPrior) > FVoltageChangeTolerance) or
                          (Abs(Abs(QoutputDRCpu) - Abs(QDesireEndpu)) > FVarChangeTolerance) or // TEMc; also tried checking against QDesireEndpu
                          (ActiveCircuit[ActorID].Solution.ControlIteration = 1)) then
                            begin
                              Set_PendingChange(CHANGEVARLEVEL,i);
                              with  ActiveCircuit[ActorID].Solution.DynaVars do
                                ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);
                              if ShowEventLog then AppendtoEventLog('InvControl.' + Self.Name+', '+ControlledElement.QualifiedName,
                                                                    Format('**Ready to change var output due to DRC trigger in DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g, QoutPU=%.3g, QDesiredEndpu=%.3g',
                                                                           [FPresentDRCVpu,FAvgpDRCVpuPrior,QoutputDRCpu,QDesireEndpu]),ActorID);

                            end;
                      end;
          else
            {do nothing}
          end;
        End;
      end;
     end;
  end;
end;

procedure TInvControlObj.InitPropertyValues(ArrayOffset: Integer);
  begin
    PropertyValue[1]  := ''; //PVSystem/Storage list
    PropertyValue[2]  := 'VOLTVAR'; // initial mode
    PropertyValue[3]  := ''; // initial combination mode
    PropertyValue[4]  := '';
    PropertyValue[5]  := '0';
    PropertyValue[6]  := 'rated';
    PropertyValue[7]  := '0s';

    PropertyValue[8]  := 'NONE'; // voltwatt_curve

    PropertyValue[9]  := '0.95';  //'DbVMin';
    PropertyValue[10] := '1.05';  // 'DbVMax';
    PropertyValue[11] := '0.1';  // 'ArGraLowV';
    PropertyValue[12] := '0.1';  // 'ArGraHiV';
    PropertyValue[13] := '0s'; // 'Rollingavgwindowlen';
    PropertyValue[14] := FloatToStr(FLAGDELTAQ); // FdeltaQFactor
    PropertyValue[15] := '0.0001'; //VoltageChangeTolerance
    PropertyValue[16] := '0.025'; // Varchangetolerance
    PropertyValue[17] := 'PMPPPU'; // Voltwatt y axis units
    PropertyValue[18] := 'INACTIVE'; //rate of change limit
    PropertyValue[19] := '0.0'; // LPF tau constant, in seconds
    PropertyValue[20] := '-1.0'; // Rise/fall Limit
    PropertyValue[21] := FloatToStr(FLAGDELTAP); // FdeltaPFactor
    if ShowEventLog then PropertyValue[22] :='YES' else PropertyValue[22] := 'NO';
    PropertyValue[23] := 'VARAVAL'; // y-axis reference (and power precedence) for volt-var
    PropertyValue[24] := '0.01';

    PropertyValue[28] := 'NONE'; // voltwattCH_curve

    inherited  InitPropertyValues(NumPropsThisClass);

  end;

function TInvControlObj.MakeDERList:Boolean;
VAR
   PVSysClass    : TDSSClass;
   StorageClass : TDSSClass;
   PVSys         : TPVSystemObj;
   Storage      : TStorageObj;
   DERElem       : TPCElement;
   i,j           : Integer;

begin

  Result := FALSE;
  PVSysClass := GetDSSClassPtr('PVSystem');
  StorageClass := GetDSSClassPtr('Storage');
  PVSys := nil;DERElem:=nil;

  if FListSize > 0 then
  begin    // Name list is defined - Use it

    SetLength(CtrlVars,FListSize+1);

    for i := 1 to FListSize do
    begin
      with CtrlVars[i] do
      Begin
        setlength( FVpuSolution,3 );
        setlength( cBuffer, 7 );
        if StripExtension(LowerCase(FDERNameList.Strings[i-1])) = 'pvsystem' then
        begin
          PVSys := PVSysClass.Find(StripClassName(FDERNameList.Strings[i-1]));

          If Assigned(PVSys) Then Begin
              If PVSys.Enabled Then FDERPointerList.New := PVSys
          End
          Else Begin
              DoSimpleMsg('Error: PVSystem Element "' + FDERNameList.Strings[i-1] + '" not found.', 14403);
              Exit;
          End;

        end
        else if StripExtension(LowerCase(FDERNameList.Strings[i-1])) = 'storage' then
        begin
          Storage := StorageClass.Find(StripClassName(FDERNameList.Strings[i-1]));

          If Assigned(Storage) Then Begin
              If Storage.Enabled Then FDERPointerList.New := Storage
          End
          Else Begin
              DoSimpleMsg('Error: Storage Element "' + FDERNameList.Strings[i-1] + '" not found.', 14403);
              Exit;
          End;

        end
      End;
    end;

  end
  else
  begin
    {Search through the entire circuit for enabled PVSystem and Storage objects and add them to the list}
    // Adding PVSystem elements
    for i := 1 to PVSysClass.ElementCount do
      begin
        PVSys :=  PVSysClass.ElementList.Get(i);
        if PVSys.Enabled then FDERPointerList.New := PVSys;
        FDERNameList.Add(PVSys.QualifiedName);
      end;
    // Adding Storage elements
    for i := 1 to StorageClass.ElementCount do
      begin
        Storage :=  StorageClass.ElementList.Get(i);
        if Storage.Enabled then FDERPointerList.New := Storage;
        FDERNameList.Add(Storage.QualifiedName);
      end;

    FListSize := FDERPointerList.ListSize;

    SetLength(CtrlVars,FListSize+1);

  end;  {else}

  //Initialize arrays

  for i := 1 to FlistSize do
  begin

    if StripExtension(LowerCase(FDERNameList.Strings[i-1])) = 'pvsystem' then
    begin
      PVSys := PVSysClass.Find(StripClassName(FDERNameList.Strings[i-1]));
      if (PVSys <> nil) then
         DERElem := TPCElement(PVSys)
    end
    else
    begin
      Storage := StorageClass.Find(StripClassName(FDERNameList.Strings[i-1]));
      if (Storage <> nil) then
         DERElem := TPCElement(Storage)
    end;

    with CtrlVars[i] do
    Begin
      // Sets the constants for the PI controller
      PICtrl      :=  TPICtrl.Create();
      PICtrl.Kp   :=  1;    // Uses deltaQ-factor as sample time for tunning the controller

      setlength( FVpuSolution,3 );
      setlength( cBuffer, 7 );

      for j := 1 to 6 do cBuffer[j]         := cZERO;
      Set_NTerms(DERElem.NTerms);
      CondOffset                            := 0;
      NPhasesDER                            := DERElem.NPhases;
      NCondsDER                             := DERElem.NConds;
      FAvgpVpuPrior                         := 0.0;
      FAvgpDRCVpuPrior                      := 0.0;
      FPresentVpu                           := 0.0;
      FPresentDRCVpu                        := 0.0;
      QDesiredVV                            := 0.0;
      QDesiredWP                            := 0.0;
      QDesiredWV                            := 0.0;
      QOld                                  := -1.0;
      QOldVV                                := -1.0;
      if PVSys = nil then QOldAVR           := 0.0
      else                QOldAVR           := - PVSys.kvarLimitNeg / 2.0;
      QOldDRC                               := -1.0;
      QOldVVDRC                             := -1.0;
      QDesiredDRC                           := 0.0;
      QDesiredVVDRC                         := 0.0;
      PLimitVW                              := 0.0;
      POldVWpu                              := 0.0;
      PBase                                 := 0.0;
      QHeadroom                             := 0.0;
      QHeadroomNeg                          := 0.0;
      Qoutputpu                             := 0.0;
      QoutputVVpu                           := 0.0;
      QoutputAVRpu                          := 0.0;
      QoutputDRCpu                          := 0.0;
      QoutputVVDRCpu                        := 0.0;
      QDesireEndpu                          := 0.0;
      QDesireVVpu                           := 0.0;
      QDesireWPpu                           := 0.0;
      QDesireWVpu                           := 0.0;
      QDesireAVRpu                          := 0.0;
      QDesireLimitedpu                      := 0.0;
      QDesireOptionpu                       := 0.0;
      PLimitVWpu                            := 0.0;
      PLimitLimitedpu                       := 0.0;
      PLimitEndpu                           := 0.0;
      PLimitOptionpu                        := 0.0;
      QDesireDRCpu                          := 0.0;
      FRollAvgWindow                        := TRollAvgWindow.Create;
      FDRCRollAvgWindow                     := TRollAvgWindow.Create;

      FdeltaQFactor                         := DELTAQDEFAULT;
      FdeltaPFactor                         := DELTAPDEFAULT;
      DeltaV_old                            := -1.0;

      deltaVDynReac                         := 0.0;
      FlagChangeCurve                       := False;
      FActiveVVCurve                        := 1;
      priorRollAvgWindow                    := 0.0;
      priorDRCRollAvgWindow                 := 0.0;
      FPriorWattspu                         := 0.0;
      FPriorWatts                           := 0.0;
      FPriorPLimitOptionpu                  := 0.0;
      FPriorQDesireOptionpu                 := 0.0;
      kW_out_desiredpu                      := 0.0;
      kW_out_desired                        := 0.0;
      FPriorvarspu                          := 0.0;
      FPriorvars                            := 0.0;

      FFlagVWOperates                       := False;

      FVVOperation                          := 0.0;
      FVWOperation                          := 0.0;
      FDRCOperation                         := 0.0;
      FVVDRCOperation                       := 0.0;
      FWPOperation                          := 0.0;
      FWVOperation                          := 0.0;
      FAVROperation                         := 0.0;

      for j := 1 to 2 do  FVpuSolution[j]   := 0.0;

      FPendingChange                        := NONE;

      FVbase                                := 0.0;
      FVarFollowInverter                    := False;
      FInverterON                           := True;
      FpresentkW                            := 0.0;
      FkVARating                            := 0.0;
      Fpresentkvar                          := 0.0;
      FkvarLimit                            := 0.0;
      FkvarLimitNeg                         := 0.0;
      FCurrentkvarLimit                     := 0.0;
      FCurrentkvarLimitNeg                  := 0.0;
      FDCkWRated                            := 0.0;
      FpctDCkWRated                         := 0.0;
      FEffFactor                            := 0.0;
      FDCkW                                 := 0.0;
      FPPriority                            := False;
      DQDV                                  := 0.0;
      Fv_setpointLimited                    := 0.0;
      FAvgpAVRVpuPrior                      := 0.0;
    end; {with}
  end; {for}

  RecalcElementData(ActiveActor);
  if FDERPointerList.ListSize>0 then Result := TRUE;
end;

procedure TInvControlObj.Reset;
  begin
    // inherited;
  end;

function TInvControl.GetXYCurve(Const CurveName: String;InvControlMode: Integer): TXYcurveObj;
  VAR
    i   : Integer;

  begin

    Result := XY_CurveClass.Find(CurveName);

    if Result = NIL then begin
      DoSimpleMsg('XY Curve object: "' + CurveName + '" representing VOLTWATT or VOLTVAR curve (depending on mode) not found.', 380);
      Exit;
    end;


    // if VOLTWATT control mode then check for any negative watt values (pu)
    // and values greater than 1.0 per-unit (=100 percent output)
    if InvControlMode = VOLTWATT then
    begin
      for i:= 1 to Result.NumPoints do
        begin
          if (Result.YValue_pt[i] < 0.0) or (Result.YValue_pt[i] > 1.0) then
            begin
              DoSimpleMsg('XY Curve object: "' + CurveName + '" has active power value(s) greater than 1.0 per-unit or less than -1.0 per-unit.  Not allowed for VOLTWATT control mode for PVSystem/Storages', 381);
              Result := NIL;
              Break;
            end;
        end;
    end;

    // if WATTPF control mode then check for any negative pf values
    // and values greater than 1.0
    if InvControlMode = WATTPF then
    begin
      for i:= 1 to Result.NumPoints do
        begin
          if (Result.YValue_pt[i] < -1.0) or (Result.YValue_pt[i] > 1.0) then
            begin
              DoSimpleMsg('XY Curve object: "' + CurveName + '" has power factor value(s) greater than 1.0 or less than -1.0.  Not allowed for WATTPF control mode for PVSystem/Storages', 381);
              Result := NIL;
              Break;
            end;
        end;
    end;

    // if WATTVAR control mode then check for any negative pf values
    // and values greater than 1.0
    if InvControlMode = WATTVAR then
    begin
      for i:= 1 to Result.NumPoints do
        begin
          if (Result.YValue_pt[i] < -1.0) or (Result.YValue_pt[i] > 1.0) then
            begin
              DoSimpleMsg('XY Curve object: "' + CurveName + '" has reactive power value(s) greater than 1.0 per-unit or less than -1.0 per-unit.  Not allowed for WATTVAR control mode for PVSystem/Storages', 381);
              Result := NIL;
              Break;
            end;
        end;
    end;

  end;

function  TInvControlObj.InterpretAvgVWindowLen(const s:string):Integer;

  Var
    Code    : Integer;
    ch      : char;
    s2      : String;

  begin
    {Try to convert and see if we get an error}
    val(s,Result, Code);
    if Code = 0 then
      begin
        FRollAvgWindowLengthIntervalUnit := 's'; // Only a number was specified, so must be seconds
        FVAvgWindowLengthSec := Result*1.0;
        Exit;
      end;

    {Error occurred so must have a units specifier}
    ch := s[Length(s)];  // get last character
    s2 := copy(s, 1, Length(s)-1);
    Val(S2, Result, Code);

    if Code>0 then
      begin   {check for error}
        FRollAvgWindowLengthIntervalUnit := 's'; // Only a number was specified, so must be seconds
        FVAvgWindowLengthSec := 1.0;
        Result := 1;
        DosimpleMsg('Error in specification of Voltage Averaging Window Length: ' + s, 1134);
        Exit;
      end;

    case ch of
      'h':
        begin
          FRollAvgWindowLengthIntervalUnit := 'h';
          FVAvgWindowLengthSec := Result*3600.0;
        end;
      'm':
        begin
          FRollAvgWindowLengthIntervalUnit := 'm';
          FVAvgWindowLengthSec := Result*60.0;
        end;
      's':
        begin
          FRollAvgWindowLengthIntervalUnit := 's';
          FVAvgWindowLengthSec := Result*1.0;
        end;
     else
       FRollAvgWindowLengthIntervalUnit := 's';
       FVAvgWindowLengthSec := Result*1.0;
       Result := 0; // Don't change it
       DosimpleMsg('Error in specification of voltage sample interval size: "' + s +'" Units can only be h, m, or s (single char only) ', 99934);
    end;
  end;

function  TInvControlObj.InterpretDRCAvgVWindowLen(const s:string):Integer;

  Var
    Code      : Integer;
    ch        : char;
    s2        : String;

  begin
    {Try to convert and see if we get an error}
    val(s,Result, Code);
    if Code = 0 then
      begin
        FDRCRollAvgWindowLengthIntervalUnit := 's'; // Only a number was specified, so must be seconds
        FDRCVAvgWindowLengthSec := Result*1.0;
        Exit;
      end;

    {Error occurred so must have a units specifier}
    ch := s[Length(s)];  // get last character
    s2 := copy(s, 1, Length(s)-1);
    Val(S2, Result, Code);
    if Code>0 then
      begin   {check for error}
        FDRCRollAvgWindowLengthIntervalUnit := 's'; // Only a number was specified, so must be seconds
        FDRCVAvgWindowLengthSec := 1.0;
        Result := 1;
        DosimpleMsg('Error in specification of Voltage Averaging Window Length: ' + s, 1134);
        Exit;
      end;

    case ch of
      'h':
        begin
          FDRCRollAvgWindowLengthIntervalUnit := 'h';
          FDRCVAvgWindowLengthSec := Result*3600.0;
        end;
      'm':
        begin
          FDRCRollAvgWindowLengthIntervalUnit := 'm';
          FDRCVAvgWindowLengthSec := Result*60.0;
        end;
      's':
        begin
          FDRCRollAvgWindowLengthIntervalUnit := 's';
          FDRCVAvgWindowLengthSec := Result*1.0;
        end;
     else
       FDRCRollAvgWindowLengthIntervalUnit := 's';
       FDRCVAvgWindowLengthSec := Result*1.0;
       Result := 0; // Don't change it
       DosimpleMsg('Error in specification of voltage sample interval size: "' + s +'" Units can only be h, m, or s (single char only) ', 99934);
    end;
end;

function TInvControlObj.GetPropertyValue(Index: Integer): String;

  begin

    Result := '';

    CASE Index of
      1 : Result := ReturnElementsList;
//      2 :
//        begin
//          if ControlMode = VOLTVAR then Result := VOLTVAR;
//          if ControlMode = VOLTWATT then Result := VOLTWATT;
//          if ControlMode = DRC then Result := DRC;
//        end;

      4 : Result := Format ('%s',[Fvvc_curvename]);
      5 : Result := Format('%-.6g', [Fvvc_curveOffset]);
      6 :
        begin
          if(FVoltage_CurveX_ref = 0) then Result := 'rated'
          else if (FVoltage_CurveX_ref = 1) then Result := 'avg'
          else if (FVoltage_CurveX_ref = 2) then Result := 'avgrated'
        end;

      7 : Result := Format('%d', [FRollAvgWindowLength,FRollAvgWindowLengthIntervalUnit]);
      8 : Result := Format ('%s',[Fvoltwatt_curvename]);
      9 : Result := Format('%.6g', [FDbVMin]);
      10 : Result := Format('%.6g', [FDbVMax]);
      11 : Result := Format('%.6g', [FArGraLowV]);
      12 : Result := Format('%.6g', [FArGraHiV]);
      13 : Result := Format('%d', [FDRCRollAvgWindowLength,FDRCRollAvgWindowLengthIntervalUnit]);
      14 : Result := Format('%.6g', [FdeltaQ_factor]);
      15 : Result := Format('%.6g', [FVoltageChangeTolerance]);
      16 : Result := Format('%.6g', [FVarChangeTolerance]);
      17 :
        begin
          if(FVoltwattYAxis = 0) then Result :=   'PAVAILABLEPU';
          if(FVoltwattYAxis = 1) then Result :=   'PMPPPU';
          if(FVoltwattYAxis = 2) then Result :=   'PCTPMPPPU';
          if(FVoltwattYAxis = 3) then Result :=   'KVARATINGPU';
        end;

      18 :
        begin
          if      RateofChangeMode = INACTIVE then Result := 'INACTIVE'
          else if RateofChangeMode = LPF then      Result := 'LPF'
          else if RateofChangeMode = RISEFALL then Result := 'RISEFALL';
        end;

      21 : Result := Format('%.6g', [FdeltaP_factor]);
      23 : Result := FReacPower_ref;
      24 : Result := Format('%.6g', [FActivePChangeTolerance]);

      28 : Result := Format ('%s',[FvoltwattCH_curvename]);
      34 : Result := Format ('%d',[CtrlModel]);

      else  // take the generic handler
        Result := Inherited GetPropertyValue(index);
    end;
  end;

function TInvControlObj.ReturnElementsList: String;
  VAR
    i   : Integer;

  begin
    if FListSize=0 then
      begin
        Result := '';
        Exit;
      end;

    Result := '['+ FDERNameList.Strings[0];

    for i := 1 to FListSize-1 do
      begin
        Result := Result + ', ' + FDERNameList.Strings[i];    // we need to pass the full name..
      end;
    Result := Result + ']';  // terminate the array

  end;

procedure TInvControlObj.Set_Enabled(Value: Boolean);
  begin
    inherited;

    {Reset controlled PVSystem/Storages to original PF}

  end;

procedure TInvControlObj.Set_PendingChange(Value: Integer;DevIndex: Integer);
  begin
    CtrlVars[DevIndex].FPendingChange := Value;
    DblTraceParameter                 := Value;
  end;

procedure TInvControlObj.UpdateInvControl(i:integer; ActorID : Integer);
  Var
    j,k                    : Integer;
    solnvoltage            : Double;
    tempVbuffer            : pComplexArray;
    BasekV                 : Double;

  begin
    tempVbuffer := Nil;   // Initialize for Reallocmem

      for j := 1 to FDERPointerList.ListSize do
        begin
          // only update solution idx one time through this routine
          if (j = 1) and (i = 1) then
            begin
              //update solution voltage in per-unit for hysteresis
              if FVpuSolutionIdx = 2 then FVpuSolutionIdx := 1
              else FVpuSolutionIdx := FVpuSolutionIdx+1;
            end;

          With CtrlVars[j] do
          Begin
            BasekV :=    CtrlVars[i].FVBase / 1000.0;

            //             FPriorvars[j]  := PVSys.Presentkvar;
            //             FPriorWatts[j]  := PVSys.PresentkW;
            FPriorPLimitOptionpu   := PLimitOptionpu;
            FPriorQDesireOptionpu  := QDesireOptionpu;

            // Used to update the VW resquested kW
            ControlledElement.Set_VWMode( FALSE );
            ControlledElement.Set_VVMode( FALSE );
            ControlledElement.Set_DRCMode( FALSE );

            FFlagVWOperates := False;

            // Reset DQDV - We might not need it
            DQDV := 0.0;

            // Reset the operation flags for the new time step
            FVVOperation   := 0;
            FVWOperation   := 0;
            FDRCOperation  := 0;
            FVVDRCOperation:= 0;
            FWPOperation   := 0;
            FWVOperation   := 0;
            FAVROperation  := 0;

            // Reinitialize convergence arrays.
            //FdeltaQFactor := DELTAQDEFAULT;
            FdeltaPFactor := DELTAPDEFAULT;

            // allocated enough memory to buffer to hold voltages and initialize to cZERO
            Reallocmem(tempVbuffer, Sizeof(tempVbuffer^[1]) * ControlledElement.NConds);
            for k := 1 to ControlledElement.NConds do tempVbuffer[k] := cZERO;

            priorRollAvgWindow := FRollAvgWindow.Get_AvgVal;
            priorDRCRollAvgWindow := FDRCRollAvgWindow.Get_AvgVal;

            // compute the present terminal voltage
            ControlledElement.ComputeVterminal(ActorID);
            //PVSys.Set_Variable(5,FDRCRollAvgWindow.Get_AvgVal); // save rolling average voltage in monitor

            solnvoltage := 0.0;

            GetmonVoltage(ActorID, solnvoltage, j, BasekV);

            //for k := 1 to localControlledElement.Yorder do tempVbuffer[k] := localControlledElement.Vterminal^[k];


            //for k := 1 to localControlledElement.Nphases do solnvoltage := solnvoltage + Cabs(tempVbuffer[k]);
            //solnvoltage := solnvoltage / (localControlledElement.Nphases*1.0); // average of voltages if more than one phase

            // add present power flow solution voltage to the rolling average window
            FRollAvgWindow.Add(solnvoltage,ActiveCircuit[ActorID].Solution.DynaVars.h,FVAvgWindowLengthSec);
            FDRCRollAvgWindow.Add(solnvoltage,ActiveCircuit[ActorID].Solution.DynaVars.h,FDRCVAvgWindowLengthSec);

            FVpuSolution[FVpuSolutionIdx] := solnvoltage/((ActiveCircuit[ActorID].Buses^[ControlledElement.terminals^[1].busRef].kVBase)*1000.0);

            Reallocmem(tempVbuffer, 0);   // Clean up memory
          end;
        end;

  end;

function TInvControlObj.Get_PendingChange(DevIndex: Integer):Integer;
  begin
    Result := CtrlVars[DevIndex].FPendingChange;
  end;

procedure TInvControlObj.CalcVoltWatt_watts(j: Integer; ActorID : Integer);
VAR
  DeltaPpu 							        :Double;
// PLimitEndpu[j] <= abs(kW_out_desiredpu[j] will always be true when we are in 'resquest' region of VW
// That's what we want. In this region, VW will work similarly to VV. So we need to move slowly towards the VW curve point.
begin
  with CtrlVars[j] do
  Begin
   if ((PLimitEndpu < 1.0) and (PLimitEndpu <= abs(kW_out_desiredpu))) or (FFlagVWOperates) then
    begin
      if(ActiveCircuit[ActorID].Solution.ControlIteration=1) then POldVWpu :=  abs(kW_out_desiredpu); // take abs(kW_out_desiredpu) because might be in charging mode.
      FFlagVWOperates := True;

      // PLimitEndpu might be negative here in 'requesting' region. Do we need to give POldVW a sign in this case?
      // Yes, it will naturally evolve to a negative value with the process. It will always positive only in the 1st control iteration.
      DeltaPpu := PLimitEndpu - POldVWpu;

      if FdeltaP_factor = FLAGDELTAP then Change_deltaP_factor(ActorID, j)
      else FdeltaPFactor := FdeltaP_factor;

      PLimitVW := (POldVWpu + DeltaPpu * FdeltaPFactor) *  PBase;
    end
  else
    begin
      PLimitVW := PLimitEndpu * PBase;
    end;
  end;
end;

procedure TInvControlObj.Check_Plimits(j: Integer; P: Double; ActorID : Integer);
VAR
  P_Ppriority             :Double;
  pctDCkWRatedlimit       :Double;

begin
  With CtrlVars[j] do
  Begin
    PLimitLimitedpu := 1.0; // Not limited

    // volt-watt states
    if P < 1.0 then FVWOperation := 1.0;

    pctDCkWRatedlimit := FpctDCkWRated * FDCkWRated;

    // PLimitEndpu should be less than the P avaliable under var priority   (works for VV_VW)
    if FPPriority = False then
    begin
      P_Ppriority :=  Sqrt(SQR(FkVARating) - SQR(Fpresentkvar));
      if P_Ppriority < (abs(P) * PBase) then   // P might be negative in requesting region for storage
        begin
          PLimitLimitedpu   := P_Ppriority / PBase * sign(P);
          FVWOperation := 0.0; // kVA exceeded under watt priority
        end;
    end;

    // PLimitEndpu should be less than pctPmpp
    if (abs(P) * PBase) >  pctDCkWRatedlimit then
    begin
      FVWOperation := 0.0; // pctPmpp exceeded under watt priority
      PLimitLimitedpu   := pctDCkWRatedlimit / PBase * sign(P);
    end;
  end;
end;

procedure TInvControlObj.CalcVoltVar_vars(j: Integer; ActorID : Integer);
VAR
  DeltaQ                                   :Double;

begin
  with CtrlVars[j] do
  Begin
    if(FlagChangeCurve = False) then
    begin
      if QDesireEndpu >= 0.0 then DeltaQ := QDesireEndpu * QHeadRoom
      else                        DeltaQ := QDesireEndpu * QHeadRoomNeg;
      if CtrlModel = 0 then
      Begin
        DeltaQ := DeltaQ - QOldVV;
        if FdeltaQ_factor = FLAGDELTAQ then Change_deltaQ_factor(ActorID, j);
        QDesiredVV := QOldVV + DeltaQ * FdeltaQFactor;
      End
      else
      Begin
        // recalculates the constants in case they've changed on the go
        PICtrl.kDen :=  exp(-1*abs(FdeltaQ_factor));
        PICtrl.kNum :=  1 - PICtrl.kDen;
        QDesiredVV  :=  PICtrl.SolvePI( DeltaQ );
      End;
    end
    // else, stay at present var output level
    else
    begin
      QDesiredVV := Fpresentkvar
    end;
  end;
end;

procedure TInvControlObj.CalcAVR_vars(j: Integer; ActorID : Integer);
VAR
  DeltaQ                                   :Double;

begin

  with CtrlVars[j] do
  Begin
    if QDesireEndpu >= 0.0 then DeltaQ := QDesireEndpu * QHeadRoom
    else                        DeltaQ := QDesireEndpu * QHeadRoomNeg;
    if CtrlModel = 0 then
    Begin
      DeltaQ := DeltaQ - QOldAVR;
      if FdeltaQ_factor = FLAGDELTAQ then Change_deltaQ_factor(ActorID, j);
      QDesiredAVR := QOldAVR + 0.2 * DeltaQ;
    //      QDesiredAVR := QDesireEndpu * QHeadRoomNeg
    End
    else
    Begin
      // recalculates the constants in case they've changed on the go
      PICtrl.kDen :=  exp(-1*abs(FdeltaQ_factor));
      PICtrl.kNum :=  1 - PICtrl.kDen;
      QDesiredAVR  :=  PICtrl.SolvePI( DeltaQ );
    End;
  end;
end;

procedure TInvControlObj.CalcWATTPF_vars(j: Integer; ActorID : Integer);

begin
  with CtrlVars[j] do
  Begin
    if QDesireEndpu >= 0.0 then
      QDesiredWP := QDesireEndpu * QHeadRoom
    else
      QDesiredWP := QDesireEndpu * QHeadRoomNeg;
  end;
end;

procedure TInvControlObj.CalcWATTVAR_vars(j: Integer; ActorID : Integer);
begin
  with CtrlVars[j] do
  Begin
    if QDesireEndpu >= 0.0 then
      QDesiredWV := QDesireEndpu * QHeadRoom
    else
      QDesiredWV := QDesireEndpu * QHeadRoomNeg;
  end;
end;

procedure TInvControlObj.CalcDRC_vars(j: Integer; ActorID : Integer);
VAR
  DeltaQ                :Double;

begin
  with CtrlVars[j] do
  Begin
    if QDesireEndpu >= 0.0 then DeltaQ := QDesireEndpu * QHeadRoom
    else                        DeltaQ := QDesireEndpu * QHeadRoomNeg;
    if CtrlModel = 0 then
    Begin
      DeltaQ := DeltaQ - QOldDRC;
      if FdeltaQ_factor = FLAGDELTAQ then Change_deltaQ_factor(ActorID, j);
      QDesiredDRC       := QOldDRC + DeltaQ * FdeltaQFactor;
    End
    else
    Begin
      // recalculates the constants in case they've changed on the go
      PICtrl.kDen   :=  exp(-1*abs(FdeltaQ_factor));
      PICtrl.kNum   :=  1 - PICtrl.kDen;
      QDesiredDRC   :=  PICtrl.SolvePI( DeltaQ );
    End;
  end;
end;

procedure TInvControlObj.CalcVVDRC_vars(j: Integer; ActorID : Integer);
VAR
  DeltaQ                :Double;

begin
  with CtrlVars[j] do
  Begin
    if QDesireEndpu >= 0.0 then DeltaQ := QDesireEndpu * QHeadRoom
    else                        DeltaQ := QDesireEndpu * QHeadRoomNeg;
    if CtrlModel = 0 then
    Begin
      DeltaQ := DeltaQ - QOldVVDRC;
      if FdeltaQ_factor = FLAGDELTAQ then Change_deltaQ_factor(ActorID, j);
      QDesiredVVDRC := QOldVVDRC + DeltaQ * FdeltaQFactor;
    End
    else
    Begin
      // recalculates the constants in case they've changed on the go
      PICtrl.kDen     :=  exp(-1*abs(FdeltaQ_factor));
      PICtrl.kNum     :=  1 - PICtrl.kDen;
      QDesiredVVDRC   :=  PICtrl.SolvePI( DeltaQ );
    End;
  end;
end;

procedure TInvControlObj.Calc_PBase(j: Integer; ActorID : Integer);
Var
  DERelem                                  :TPCElement;

begin
  with CtrlVars[j] do
  Begin
    DERelem := ControlledElement;

    if DERelem.DSSClassName = 'PVSystem' then
    begin
      if(FVoltwattYaxis = 0)  then PBase := FDCkW * FEffFactor

      else if(FVoltwattYaxis = 1)  then PBase := FDCkWRated

      else if(FVoltwattYaxis = 2)  then PBase := FDCkWRated * FpctDCkWRated

      else if(FVoltwattYaxis = 3)  then PBase := FkVARating;
    end
    else
    begin
      if(FVoltwattYaxis = 0)  then PBase := TStorageObj(DERelem).DCkW * FEffFactor

      else if(FVoltwattYaxis = 1)  then PBase := FDCkWRated

      else if(FVoltwattYaxis = 2)  then PBase := FDCkWRated * FpctDCkWRated

      else if(FVoltwattYaxis = 3)  then PBase := FkVARating;

    end;
  end;
end;

procedure TInvControlObj.CalcLPF(m: Integer; powertype: String; LPF_desiredpu : Double; ActorID : Integer);
VAR
  alpha                     :Double;

  // Applies the LPF:
  //  Return value is in kvar for VARS
  //  Return value is in puPmpp for WATTS

begin
  with CtrlVars[m] do
  Begin
  // Qoutput(t) = Qdesired(t) x {1- exp[-(t-t0)/tau]} + Qoutput(t-t0) x exp[-(t-t0)/tau]
  // calculate the alpha constant: alpha = exp[-(t-t0)/tau]
    alpha := exp(-1.0 * ActiveCircuit[ActorID].Solution.DynaVars.h/FLPFTau);

    if powertype = 'VARS' then QDesireOptionpu := LPF_desiredpu * (1-alpha) + FPriorQDesireOptionpu * alpha;

    if powertype = 'WATTS' then PLimitOptionpu := LPF_desiredpu * (1-alpha) + FPriorPLimitOptionpu * alpha
  end;
end;

procedure TInvControlObj.CalcRF(m: Integer; powertype: String; RF_desiredpu: Double ; ActorID :Integer);

begin
  // Applies the Rise/Fall limiting function:
  with CtrlVars[m] do
  Begin
    if powertype='VARS' then
    begin
      // rate of change rise/fall limit
      if (RF_desiredpu - FPriorQDesireOptionpu) > (FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h) then
        QDesireOptionpu := FPriorQDesireOptionpu + FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h
      else if (RF_desiredpu - FPriorQDesireOptionpu) < (-1 * FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h) then
        QDesireOptionpu := FPriorQDesireOptionpu - FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h
      else
        QDesireOptionpu := RF_desiredpu;
    end;

    if powertype='WATTS' then
    begin
      // rate of change rise/fall limit
      if (RF_desiredpu - FPriorPLimitOptionpu) > (FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h) then
        PLimitOptionpu := FPriorPLimitOptionpu + (FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h)
      else if (RF_desiredpu - FPriorPLimitOptionpu) < (-1 * FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h) then
        PLimitOptionpu := FPriorPLimitOptionpu - (FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h)
      else
        PLimitOptionpu := RF_desiredpu;
    end;
  end;
end;

procedure TInvControlObj.CalcPVWcurve_limitpu(j: Integer; ActorID : Integer);
begin
  with CtrlVars[j] do
  Begin
    if ControlledElement.DSSClassName = 'PVSystem' then PLimitVWpu := Fvoltwatt_curve.GetYValue(FPresentVpu)
    else
    begin
      if TStorageObj(ControlledElement).StorageState  = STORE_DISCHARGING then
      Begin
          if TStorageObj(ControlledElement).FVWStateRequested then PLimitVWpu := FvoltwattCH_curve.GetYValue(FPresentVpu)
          else PLimitVWpu := Fvoltwatt_curve.GetYValue(FPresentVpu);

      End
      else if (TStorageObj(ControlledElement).StorageState  = STORE_CHARGING) and (FvoltwattCH_curve <> Nil) then
      Begin
          if TStorageObj(ControlledElement).FVWStateRequested then PLimitVWpu := Fvoltwatt_curve.GetYValue(FPresentVpu)
          else PLimitVWpu := FvoltwattCH_curve.GetYValue(FPresentVpu) // try with positive PlimitVWpu
      End

      else PLimitVWpu := 1.0; // don't limit if in idling state
    end;
  end;
end;

procedure TInvControlObj.CalcQVVcurve_desiredpu(j: Integer; ActorID : Integer);
VAR
  voltagechangesolution                    :Double;
  QPresentpu                               :Double;
  VpuFromCurve                             :Double;

begin
  with CtrlVars[j] do
  Begin
    QDesireVVpu := 0.0;

    if Fpresentkvar >= 0.0 then QPresentpu   := Fpresentkvar / QHeadRoom
    else QPresentpu   := Fpresentkvar / QHeadRoomNeg;

    voltagechangesolution := 0.0;

    // for first two seconds, keep voltagechangesolution equal to zero
    // we don't have solutions from the time-series power flow, yet
    if ((ActiveCircuit[ActorID].Solution.DynaVars.dblHour*3600.0 / ActiveCircuit[ActorID].Solution.DynaVars.h)<3.0) then voltagechangesolution := 0.0
    else if(FVpuSolutionIdx = 1) then voltagechangesolution := FVpuSolution[1] - FVpuSolution[2]
    else if(FVpuSolutionIdx = 2) then voltagechangesolution := FVpuSolution[2] - FVpuSolution[1];

    // if no hysteresis (Fvvc_curveOffset == 0), then just look up the value
    // from the volt-var curve
    if Fvvc_curveOffset = 0.0 then
      begin  // no hysteresis
        QDesireVVpu := Fvvc_curve.GetYValue(FPresentVpu)
      end // end of logic for the no-hysteresis case

    // else if we're going in the positive direction and on curve 1, stay
    // with curve 1
    else if (voltagechangesolution > 0) and (FActiveVVCurve = 1) then
      begin
        if(FlagChangeCurve = True) then
          begin
            VpuFromCurve := Fvvc_curve.GetXValue(QPresentpu);
              if(Abs(FPresentVpu - VpuFromCurve) < FVoltageChangeTolerance/2.0) then
                begin
                  QDesireVVpu := Fvvc_curve.GetYValue(FPresentVpu);      //Y value = in per-unit of headroom
                  FlagChangeCurve := False;
                end
              else
                begin
                  QDesireVVpu := QPresentpu;            // (PR) look at here
                  FlagChangeCurve := False;
                end;
          end
        else
          begin
            QDesireVVpu := Fvvc_curve.GetYValue(FPresentVpu);      //Y value = in per-unit of headroom
          end;
      end

    // with hysteresis if we're going in the positive direction on voltages
    // from last two power flow solutions, and we're using curve 2, keep vars
    // the same, and change to curve1 active
    else if (voltagechangesolution > 0) and (FActiveVVCurve = 2) then
      begin
        QDesireVVpu := QPresentpu;
        FActiveVVCurve := 1;
        FlagChangeCurve := True;
      end

    // with hysteresis if we're going in the negative direction on voltages
    // from last two power flow solutions, and we're using curve 2, either
    // lookup the vars for the voltage we're at (with offset on curve1),
    // or if we've not just changed curves, stay at the current p.u.
    // var output
    else if (voltagechangesolution < 0) and (FActiveVVCurve = 2) then
    begin
      if(FlagChangeCurve = True) then
        begin
          VpuFromCurve := Fvvc_curve.GetXValue(QPresentpu);
          VpuFromCurve := VpuFromCurve - Fvvc_curveOffset;
          if(Abs(FPresentVpu - VpuFromCurve) < FVoltageChangeTolerance/2.0)  then
          begin
            QDesireVVpu := Fvvc_curve.GetYValue(FPresentVpu-Fvvc_curveOffset);      //Y value = in per-unit of headroom
            FlagChangeCurve := False;
          end
          else
          begin
            QDesireVVpu := QPresentpu;
            FlagChangeCurve := False;
          end;
        end
      else
        begin
          QDesireVVpu := Fvvc_curve.GetYValue(FPresentVpu-Fvvc_curveOffset);      //Y value = in per-unit of headroom
        end;
    end

    // with hysteresis if we're going in the negative direction on voltages
    // from last two power flow solutions, and we're using curve 1, then
    // stay wjth present output vars and make curve2 active, set curve change
    // flag
    else if (voltagechangesolution < 0) and (FActiveVVCurve = 1) then
    begin
      QDesireVVpu := QPresentpu;
      FActiveVVCurve := 2;
      FlagChangeCurve := True;
    end


    // if no change in voltage from one powerflow to the next, then
    // do one of the following
    else if (voltagechangesolution = 0)  and (FActiveVVCurve = 1) and (FlagChangeCurve = False) then
    begin
      QDesireVVpu := Fvvc_curve.GetYValue(FPresentVpu);
    end
    else if (voltagechangesolution = 0) and (FlagChangeCurve = True) then
    begin
      QDesireVVpu := QPresentpu;
    end

    else if (voltagechangesolution = 0)  and (FActiveVVCurve = 2) and (FlagChangeCurve = False) then
    begin
      QDesireVVpu := Fvvc_curve.GetYValue(FPresentVpu-Fvvc_curveOffset);
    end;
  end;
end;

procedure TInvControlObj.CalcQWVcurve_desiredpu(j: Integer; ActorID : Integer);
VAR
  voltagechangesolution                    :Double;
  Pbase                                    :Double;


begin
  with CtrlVars[j] do
  Begin
    QDesireWVpu := 0.0;

    voltagechangesolution := 0.0;

    // for first two seconds, keep voltagechangesolution equal to zero
    // we don't have solutions from the time-series power flow, yet
    if ((ActiveCircuit[ActorID].Solution.DynaVars.dblHour*3600.0 / ActiveCircuit[ActorID].Solution.DynaVars.h)<3.0) then voltagechangesolution := 0.0
    else if(FVpuSolutionIdx = 1) then voltagechangesolution := FVpuSolution[1] - FVpuSolution[2]
    else if(FVpuSolutionIdx = 2) then voltagechangesolution := FVpuSolution[2] - FVpuSolution[1];

    Pbase:= Min(FkVARating, FDCkWRated); // Should include DC-to-AC and kW-to-KVA ratios to avoid to quick fix like this

    QDesireWVpu := Fwattvar_curve.GetYValue(FDCkW * FEffFactor * FpctDCkWRated / Pbase);
  end;
end;


procedure TInvControlObj.CalcQAVR_desiredpu(j: Integer; ActorID : Integer);
VAR
  voltagechangesolution                    : Double;
  DQ                                       : Double;
  QPresentpu                               : Double;
  DQmax                                    : Double;
  DeltaV                                   : Double;
  v                                        : Double;


begin
  with CtrlVars[j] do
  Begin
    DQmax := 0.1 * Fkvarlimit / QHeadRoomNeg;

    QDesireAVRpu := 0.0;

  //    if (((Fv_setpoint - FAvgpVpuPrior) > 0) and (ActiveCircuit[ActorID].Solution.ControlIteration = 3)) then
  //      Fpresentkvar := 0 //abs(Fpresentkvar)
  //    else
  //      Fpresentkvar := 0; //-1 * abs(Fpresentkvar);

  //    if (ActiveCircuit[ActorID].Solution.ControlIteration = 3) then
  //    begin
  //      Fpresentkvar := 0; //abs(Fpresentkvar)
  //      if (((Fv_setpoint - FAvgpAVRVpuPrior) > 0)) then
  //        Fpresentkvar := abs(Fpresentkvar)
  //      else
  //        Fpresentkvar := -1 * abs(Fpresentkvar);
  //
  //    end;


    if Fpresentkvar >= 0.0 then QPresentpu   := Fpresentkvar / QHeadRoom
    else QPresentpu   := Fpresentkvar / QHeadRoomNeg;

    if (ActiveCircuit[ActorID].Solution.ControlIteration = 3) then
    begin
      v := FAvgpAVRVpuPrior;
      QPresentpu := 0.0;
      QOldAVR := 0.0;
    end
    else
        v := FPresentVpu;

    voltagechangesolution := 0.0;

    // for first two seconds, keep voltagechangesolution equal to zero
    // we don't have solutions from the time-series power flow, yet
    if ((ActiveCircuit[ActorID].Solution.DynaVars.dblHour*3600.0 / ActiveCircuit[ActorID].Solution.DynaVars.h)<3.0) then voltagechangesolution := 0.0
    else if(FVpuSolutionIdx = 1) then voltagechangesolution := FVpuSolution[1] - FVpuSolution[2]
    else if(FVpuSolutionIdx = 2) then voltagechangesolution := FVpuSolution[2] - FVpuSolution[1];

  //    if (abs(FPresentVpu - FAvgpVpuPrior) = FVoltageChangeTolerance) then  DQ := 0.0
  //    else if Fv_setpoint <= FPresentVpu then DQ := - abs((Fv_setpoint - FPresentVpu / QHeadRoom) * (QPresentpu - Fkvarlimitneg) / (FPresentVpu - FAvgpVpuPrior))
  //    else DQ := abs((Fv_setpoint - FPresentVpu) * (QPresentpu - Fkvarlimit / QHeadRoomNeg) / (FPresentVpu - FAvgpVpuPrior));
  //
  //    If (DQ > Fkvarlimit / QHeadRoom) then DQ := Fkvarlimit / QHeadRoom
  //    else if (DQ > Fkvarlimitneg / QHeadRoomNeg) then DQ := - Fkvarlimitneg / QHeadRoomNeg;
  //
  //    QDesireAVRpu := QPresentpu + DQ;

    DeltaV := Abs(Fv_setpoint - FAvgpVpuPrior);

    if (abs(DeltaV) < 0.005) and (FdeltaQFactor > 0.2) then FdeltaQFactor := FdeltaQFactor + 0.1
    else if (abs(DeltaV) < 0.02) and (FdeltaQFactor > 0.2) then FdeltaQFactor := FdeltaQFactor + 0.05
    else if (abs(DeltaV) > 0.02) and (FdeltaQFactor < 0.9) then FdeltaQFactor := FdeltaQFactor - 0.05
    else if (abs(DeltaV) < 0.05) and (FdeltaQFactor < 0.9) then FdeltaQFactor := FdeltaQFactor - 0.1;


    FdeltaQFactor := 0.2;

    DeltaV_old := Abs(FPresentVpu - FAvgpVpuPrior);

    if (FPresentVpu - FAvgpVpuPrior = 0) then  DQ := 0
    else
      DQ := FdeltaQFactor * DQDV * (Fv_setpoint - v);
      If (Abs(DQ) > DQmax) Then IF (DQ < 0.0) Then DQ := -DQmax Else DQ := DQmax;

    QDesireAVRpu := QPresentpu + DQ;

  end;
end;

procedure TInvControlObj.CalcQWPcurve_desiredpu(j: Integer; ActorID : Integer);
VAR
  voltagechangesolution                    :Double;
  p                                        :Double;
  pf_priority                              :Boolean;
  QDesiredWP                               :Double;
  // Pbase                                    :Double;

begin
  with CtrlVars[j] do
  Begin
    QDesireWPpu := 0.0;

    voltagechangesolution := 0.0;

    pf_priority := False;

    // for first two seconds, keep voltagechangesolution equal to zero
    // we don't have solutions from the time-series power flow, yet
    if ((ActiveCircuit[ActorID].Solution.DynaVars.dblHour*3600.0 / ActiveCircuit[ActorID].Solution.DynaVars.h)<3.0) then voltagechangesolution := 0.0
    else if(FVpuSolutionIdx = 1) then voltagechangesolution := FVpuSolution[1] - FVpuSolution[2]
    else if(FVpuSolutionIdx = 2) then voltagechangesolution := FVpuSolution[2] - FVpuSolution[1];

    // Pbase = Min(FpctDCkWRated / FDCkWRated, FkVARating)

    pf_wp_nominal := Fwattpf_curve.GetYValue(FDCkW * FEffFactor * FpctDCkWRated / FDCkWRated);

    if ControlledElement.DSSClassName = 'PVSystem'     then pf_priority := TPVSystemObj(ControlledElement).PVSystemVars.PF_Priority
    else if ControlledElement.DSSClassName = 'Storage' then pf_priority :=  TStorageObj(ControlledElement).StorageVars.PF_Priority;

    if (FPPriority = FALSE) and (pf_priority = FALSE) then p := FDCkW * FEffFactor * FpctDCkWRated
    else p := kW_out_desired;

    QDesiredWP := p * sqrt(1 / (pf_wp_nominal * pf_wp_nominal) -1) * sign(pf_wp_nominal);


    if QDesiredWP >= 0.0 then  QDesireWPpu := QDesiredWP / QHeadRoom
    else  QDesireWPpu := QDesiredWP / QHeadRoomNeg;
  end;
end;

procedure TInvControlObj.CalcQDRC_desiredpu(j: Integer; ActorID : Integer);
VAR
  basekV                                    :Double;

begin
  with CtrlVars[j] do
  Begin
    QDesireDRCpu := 0.0;

    basekV :=    FVBase / 1000.0; // It's a line-to-ground voltage

    // calculate deltaV quantity in per-unit from subtracting the rolling average
    // value (in p.u.) from the present p.u. terminal voltage (average of line-ground)
    // if more than one phase
    if(FDRCRollAvgWindow.Get_AvgVal/(basekV*1000.0)) = 0.0 then deltaVDynReac:=0
    else deltaVDynReac := FPresentDRCVpu - FDRCRollAvgWindow.Get_AvgVal/(basekV*1000.0);

    // if below the lower deadband and deltaV quantity is non-zero then
    // calculate desired pu var output. In per-unit of kva rating (also
    // ampere rating), per report specifications.
    if (deltaVDynReac <>0) and (FPresentDRCVpu < FDbVMin) then QDesireDRCpu := -deltaVDynReac*FArGraLowV

    // if above the upper deadband and deltaV quantity is non-zero then
    // calculate desired pu var output. In per-unit of kva rating (also
    // ampere rating), per report specifications.

    else if (deltaVDynReac <>0) and (FPresentDRCVpu > FDbVMax) then QDesireDRCpu := -deltaVDynReac*FArGraHiV

    else if deltaVDynReac = 0.0 then QDesireDRCpu := 0.0;

    if (ActiveCircuit[ActorID].Solution.Dynavars.t=1) then QDesireDRCpu := 0.0;
  end;
end;


procedure TInvControlObj.Check_Qlimits_WV(j: Integer; Q: Double; ActorID : Integer);
VAR
  Q_Ppriority                              :Double;
  currentkvarlimitpu                       :Double;
  currentkvarlimitnegpu                    :Double;
  FOperation                               :Double;
  error                                    :Double;

begin
  with CtrlVars[j] do
  Begin
    // Will organize this part into functions later

    // states
    error := 0;
    if (ControlMode = WATTVAR)          then error := 0.005;

    if Q < -error then FOperation := -1.0
    else if Q > error then  FOperation := 1.0
    else FOperation := 0.0;


    QDesireLimitedpu := 1.0; // Not limited

    currentkvarlimitpu := FCurrentkvarLimit / QHeadRoom;
    currentkvarlimitnegpu := FCurrentkvarLimitNeg / QHeadRoomNeg;

    if currentkvarlimitpu > QDesireLimitedpu  then  currentkvarlimitpu := QDesireLimitedpu;
    if currentkvarlimitnegpu > QDesireLimitedpu  then  currentkvarlimitnegpu := QDesireLimitedpu;

    // Q curve desiredpu should be less than currentkvarlimit(neg)
    if (Q > 0.0) and (abs(Q) >= abs(currentkvarlimitpu)) then
      begin
        FOperation := 0.2 * sign(Q); // When kvarlimit is exceeded
        QDesireLimitedpu := currentkvarlimitpu * sign(Q);
      end
    else if (Q < 0.0) and (abs(Q) >= abs(currentkvarlimitnegpu))  then
      begin
        FOperation := 0.2 * sign(Q); // When kvarlimitneg is exceeded
        QDesireLimitedpu := currentkvarlimitnegpu * sign(Q);
      end;

    // States Flags
    if (ControlMode = WATTVAR)         then FWVOperation    := FOperation;
  end;
end;

procedure TInvControlObj.Calc_PQ_WV(j: Integer; ActorID : Integer);
VAR
  QPratio                                     : Double;
  coeff                                       : TCoeff;
  pre_S                                       : Double;
  var_limit_operation_value                   : Double;
  Qbase                                       : Double;
  Qbasesign                                   : Double;
  Pbase                                       : Double;

  A                                           : Double;
  B                                           : Double;
  C                                           : Double;
  a_line                                      : Double;
  b_line                                      : Double;


begin
  with CtrlVars[j] do
  Begin
    Pbase := Min(FkVARating, FDCkWRated);

    if QDesiredWV >= 0.0 then
      begin
        Qbase := QHeadroom;
        Qbasesign := 1.0;
      end
    else
      begin
        Qbase := QHeadroomNeg;
        Qbasesign := -1.0;
      end;

    var_limit_operation_value := 0.2;
    if (abs(FWVOperation) = var_limit_operation_value)  then PLimitEndpu := Fwattvar_curve.GetXValue(QDesireEndpu)
    else PLimitEndpu := 1.0;

    CalcWATTVAR_vars(j, ActorID);

    // Qdesiredpu should be less than the Q avaliable under watt priority  (works just for varmax)
    if (Sqrt(Sqr(FDCkW * FEffFactor * FpctDCkWRated * PLimitEndpu) + Sqr(QDesiredWV)) > FkVARating) then
      begin
         coeff := Fwattvar_curve.GetCoefficients(FDCkW * FEffFactor * FpctDCkWRated / Pbase);

         a_line := coeff[1] * Qbase / Pbase;
         b_line := coeff[2] * Qbase;

         A := 1 + Sqr(a_line);
         B := 2 * a_line * b_line;
         C := Sqr(b_line) - Sqr(FkVARating);


         PLimitEndpu := (-B + Sqrt(sqr(B) - 4 * A * C)) / (2 * A * Pbase);
         QDesireEndpu := Fwattvar_curve.GetYValue(PLimitEndpu);
      end;

        CalcWATTVAR_vars(j, ActorID)
  end;
end;



procedure TInvControlObj.Check_Qlimits(j: Integer; Q: Double; ActorID : Integer);
VAR
  Q_Ppriority                              :Double;
  currentkvarlimitpu                       :Double;
  currentkvarlimitnegpu                    :Double;
  FOperation                               :Double;
  error                                    :Double;

begin
  with CtrlVars[j] do
  Begin
    // states
    error := 0;
    if (ControlMode = VOLTVAR)          then error := 0.005;
    if (ControlMode = WATTPF)           then error := 0.005;
    if (ControlMode = WATTVAR)          then error := 0.005;
    if (ControlMode = DRC)              then error := 0.0005;
    if (ControlMode = AVR)              then error := 0.005;
    if (CombiControlMode = VV_DRC)      then error := 0.005;
    if (CombiControlMode = VV_VW)       then error := 0.005;

    if Q < -error then FOperation := -1.0
    else if Q > error then  FOperation := 1.0
    else FOperation := 0.0;


    QDesireLimitedpu := 1.0; // Not limited

    currentkvarlimitpu := FCurrentkvarLimit / QHeadRoom;
    currentkvarlimitnegpu := FCurrentkvarLimitNeg / QHeadRoomNeg;

    if currentkvarlimitpu > QDesireLimitedpu  then  currentkvarlimitpu := QDesireLimitedpu;
    if currentkvarlimitnegpu > QDesireLimitedpu  then  currentkvarlimitnegpu := QDesireLimitedpu;

    // Q curve desiredpu should be less than currentkvarlimit(neg)
    if (Q > 0.0) and (abs(Q) >= abs(currentkvarlimitpu)) then
      begin
        FOperation := 0.2 * sign(Q); // When kvarlimit is exceeded
        QDesireLimitedpu := currentkvarlimitpu * sign(Q);
      end
    else if (Q < 0.0) and (abs(Q) >= abs(currentkvarlimitnegpu))  then
      begin
        FOperation := 0.2 * sign(Q); // When kvarlimitneg is exceeded
        QDesireLimitedpu := currentkvarlimitnegpu * sign(Q);
      end;

    // Qdesiredpu should be less than the Q avaliable under watt priority  (works just for varmax)
    if FPPriority and ((FReacPower_ref = 'VARMAX') or (ControlMode = WATTPF)) then
      begin
        if Q >= 0.0 then Q_Ppriority :=  Sqrt(SQR(FkVARating) - SQR(FpresentkW))/QHeadRoom
        else Q_Ppriority :=  Sqrt(SQR(FkVARating) - SQR(FpresentkW))/QHeadRoomNeg;

        if (abs(Q_Ppriority) < abs(QDesireLimitedpu)) and (abs(Q_Ppriority) < abs(Q)) then
          begin
            FOperation := 0.6 * sign(Q); // kVA exceeded under watt priority is considered above
            if (abs(Q) < (0.01 / 100)) or (abs(Q_Ppriority) < epsilon) then  FOperation := 0.0;
            QDesireLimitedpu := Q_Ppriority * sign(Q);
          end;
      end;


    // States Flags
    if (ControlMode = VOLTVAR)         then FVVOperation    := FOperation;
    if (ControlMode = WATTPF)          then FWPOperation    := FOperation;
    if (ControlMode = WATTVAR)         then FWVOperation    := FOperation;
    if (ControlMode = DRC)             then FDRCOperation   := FOperation;
    if (ControlMode = AVR)             then FAVROperation   := FOperation;
    if (CombiControlMode = VV_DRC)     then FVVDRCOperation := FOperation;
    if (CombiControlMode = VV_VW)      then FVVOperation    := FOperation;
  end;
end;

procedure TInvControlObj.Calc_QHeadRoom(j: Integer; ActorID : Integer);
begin
  with CtrlVars[j] do
  Begin
    if FReacPower_ref = 'VARAVAL' then
      begin
        if(abs(FpresentkW) < FkVARating) then
          QHeadRoom := SQRT(Sqr(FkVARating)-Sqr(FpresentkW))
        else
          QHeadRoom := 0.0;

        QHeadRoomNeg := QHeadRoom;
      end;

    if (FReacPower_ref = 'VARMAX') or (ControlMode = WATTPF) then
      begin
        QHeadRoom := FkvarLimit;
        QHeadRoomNeg := FkvarLimitNeg;
      end;

    if(QHeadRoom = 0.0) then QHeadRoom := FkvarLimit;
    if(QHeadRoomNeg = 0.0) then QHeadRoomNeg := FkvarLimitNeg;
  end;
end;

procedure TInvControlObj.Change_deltaQ_factor(ActorID : Integer; j: Integer);
VAR
  DeltaV                                   :Double;

begin
  with CtrlVars[j] do
  Begin
    DeltaV := Abs(FPresentVpu - FAvgpVpuPrior);

    if (DeltaV_old >= 0.0) then
      begin
        if (abs(DeltaV) > 0.8 * DeltaV_old) and (FdeltaQFactor > 0.2) then FdeltaQFactor := FdeltaQFactor - 0.1
        else if (abs(DeltaV) > 0.6 * DeltaV_old) and (FdeltaQFactor > 0.2) then FdeltaQFactor := FdeltaQFactor - 0.05
        else if (abs(DeltaV) < 0.2 * DeltaV_old) and (FdeltaQFactor < 0.9) then FdeltaQFactor := FdeltaQFactor + 0.1
        else if (abs(DeltaV) < 0.4 * DeltaV_old) and (FdeltaQFactor < 0.9) then FdeltaQFactor := FdeltaQFactor + 0.05;
      end;

    DeltaV_old := Abs(FPresentVpu - FAvgpVpuPrior);
  end;
end;

procedure TInvControlObj.Change_deltaP_factor(ActorID : Integer; j: Integer);
VAR
  DeltaV                                   :Double;

begin
  with CtrlVars[j] do
  Begin
    DeltaV := Abs(FPresentVpu - FAvgpVpuPrior);

    if DeltaV_old >= 0.0 then
      begin
        if (abs(DeltaV) > 0.9 * DeltaV_old) and (FdeltaPFactor > 0.2)  then FdeltaPFactor := FdeltaPFactor - 0.1
        else if (abs(DeltaV) > 0.8 * DeltaV_old) and (FdeltaPFactor > 0.1) then FdeltaPFactor := FdeltaPFactor - 0.05
        else if (abs(DeltaV) < 0.2 * DeltaV_old) and (FdeltaPFactor < 0.9) then FdeltaPFactor := FdeltaPFactor + 0.05
        else if (abs(DeltaV) < 0.1 * DeltaV_old) and (FdeltaPFactor < 0.9)  then FdeltaPFactor := FdeltaPFactor + 0.1;
      end;

    DeltaV_old := Abs(FPresentVpu - FAvgpVpuPrior);
  end;
end;

//Called at end of main power flow solution loop
procedure TInvControl.UpdateAll(ActorID : integer);
VAR
  i : Integer;

begin

  for i := 1 to ElementList.ListSize  do
    with TInvControlObj(ElementList.Get(i)) do
      if Enabled then UpdateInvControl(i, ActorID);

end;

procedure TRollAvgWindow.Add(IncomingSampleValue: Double;IncomingSampleTime: Double;VAvgWindowLengthSec:Double);
begin
  {$IFNDEF FPC}
  if(sample.Count > 0) and (bufferfull) then
    begin
      runningsumsample := runningsumsample - sample.Dequeue;
      if(bufferlength = 0) then
        begin
          IncomingSampleValue := 0.0;
        end;
      sample.Enqueue(IncomingSampleValue);
      runningsumsample := runningsumsample + IncomingSampleValue;
      runningsumsampletime := runningsumsampletime - sampletime.Dequeue;
      sampletime.Enqueue(IncomingSampleTime);
      runningsumsampletime := runningsumsampletime +IncomingSampleTime;
    end

  else
    begin
      if(bufferlength = 0) then
        begin
          IncomingSampleValue := 0.0;
        end;
      sample.Enqueue(IncomingSampleValue);
      runningsumsample := runningsumsample + IncomingSampleValue;
      sampletime.Enqueue(IncomingSampleTime);
      runningsumsampletime := runningsumsampletime + IncomingSampleTime;
      if (runningsumsampletime > VAvgWindowLengthSec)
          then bufferfull := True;
      if (sample.Count = bufferlength)
          then bufferfull := True;
    end;

  {$else}
  if(sample.size > 0) and (bufferfull) then
    begin
      runningsumsample := runningsumsample - sample.front; sample.pop;
      if(bufferlength = 0) then
        begin
          IncomingSampleValue := 0.0;
        end;
      sample.push(IncomingSampleValue);
      runningsumsample := runningsumsample + IncomingSampleValue;
      runningsumsampletime := runningsumsampletime - sampletime.front; sampletime.pop;
      sampletime.push(IncomingSampleTime);
      runningsumsampletime := runningsumsampletime +IncomingSampleTime;
    end
  else
    begin
      if(bufferlength = 0) then
        begin
          IncomingSampleValue := 0.0;
        end;
      sample.push(IncomingSampleValue);
      runningsumsample := runningsumsample + IncomingSampleValue;
      sampletime.push(IncomingSampleTime);
      runningsumsampletime := runningsumsampletime + IncomingSampleTime;
      if (runningsumsampletime > VAvgWindowLengthSec)
          then bufferfull := True;
      if (sample.size = bufferlength)
          then bufferfull := True;
    end;
  {$ENDIF}
end;

constructor TRollAvgWindow.Create();
begin
  sample        := TQueue<Double>.Create();
  sampletime    := TQueue<Double>.Create();

  runningsumsample                := 0.0;
  runningsumsampletime            := 0.0;
  bufferlength                    := 0;
  bufferfull                      := False;
end;

destructor TRollAvgWindow.Destroy;
begin
  sample      := nil;
  sampletime  := nil;

  inherited;
end;

procedure TRollAvgWindow.Set_BuffLength(const Value: Integer);
begin
  bufferlength := Value;
end;

function TRollAvgWindow.Get_AvgVal: Double;
begin
{$IFNDEF FPC}
  if(sample.Count = 0) then
    Result:= 0.0
  else  Result:= runningsumsample / sample.Count;
{$else}
  if(sample.size = 0) then
    Result:= 0.0
  else  Result:= runningsumsample / sample.size;
{$ENDIF}
end;

function TRollAvgWindow.Get_AccumSec: Double;
begin
{$IFNDEF FPC}
  if(sample.Count = 0) then
    Result:= 0.0
  else Result:= runningsumsampletime;
{$else}
  if(sample.size = 0) then
    Result:= 0.0
  else Result:= runningsumsampletime;
{$ENDIF}
end;


INITIALIZATION



Finalization



end.


