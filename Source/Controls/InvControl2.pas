unit InvControl2;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015,  Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
  A InvControl is a control element that is connected to a terminal of another
  circuit element and sends kW and/or kvar signals to a set of PVSystem objects it controls

  A InvControl is defined by a New command:

  New InvControl.Name=myname PVSystemList = (pvsystem1  pvsystem2 ...)

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
  Command, ControlClass, ControlElem, CktElement, DSSClass, bus, PCElement,PVSystem2, Storage2, Storage2Vars, Arraydef, ucomplex,
  utilities, XYcurve, Dynamics, PointerList, Classes, StrUtils;

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

  TInvControl2 = class(TControlClass)

    private
      XY_CurveClass: TDSSClass;

    protected
      procedure DefineProperties;
      function MakeLike(const InvControl2Name:String):Integer;Override;

    public
      constructor Create;
      destructor Destroy; override;

      function Edit(ActorID : Integer):Integer; override;     // uses global parser
      function NewObject(const ObjName:String):Integer; override;
      function GetXYCurve(Const CurveName: String; InvControl2Mode: Integer): TXYcurveObj;
      procedure UpdateAll(ActorID : integer);
   end;

  TInvControl2Obj = class(TControlElem)

    private
      ControlMode             : Integer;
      CombiControlMode        : Integer;
      ControlActionHandle     : Integer;
      ControlledElement       : Array of TPCElement;
      MonitoredElement        : TDSSCktElement;  // First DER element for now (the first element from ControlledElement TPointerList)

      {Variables for voltages}
      FVreg                   : Double;
      FAvgpVpuPrior           : Array of Double;
      FAvgpDRCVpuPrior        : Array of Double;
      FPresentVpu             : Array of Double;
      FPresentDRCVpu          : Array of Double;
      FVpuSolution            : Array of Array of Double;
      FVpuSolutionIdx         : Integer;

      {Variables for convergence process}
      FdeltaQ_factor          : Double;
      FdeltaP_factor          : Double;

      FdeltaQFactor           : Array of Double;
      FdeltaPFactor           : Array of Double;
      DeltaV_old              : Array of Double;

      FVoltageChangeTolerance : Double;
      FVarChangeTolerance     : Double;
      FActivePChangeTolerance : Double;

      // Active power
      PLimitVW                : Array of Double;
      POldVWpu                : Array of Double;
      FFlagVWOperates         : Array of Boolean;  // Flag enabled when volt-watt Pdesired is less than 1. So volt-watt algorithm starts to work
      PLimitVWpu              : Array of Double;
      PLimitLimitedpu         : Array of Double;
      PLimitEndpu             : Array of Double;
      PLimitOptionpu          : Array of Double;
      kW_out_desiredpu        : Array of Double;
      kW_out_desired          : Array of Double;

      // Reactive power
      QDesireEndpu            : Array of Double;  // Q value used in the convergency algorithm
      QDesireVVpu             : Array of Double; // Q desired caculated in volt-var curve
      QDesireWPpu             : Array of Double; // Q desired caculated in watt-pf curve
      QDesireWVpu             : Array of Double; // Q desired caculated in watt-var curve
      QDesireDRCpu            : Array of Double;  // Q desired from the DRC equation
      QDesireLimitedpu        : Array of Double; // Calculates possible Q considering kVA (watt priority) and kvarlimit limits
      QDesireOptionpu         : Array of Double; // Calculates Q Limit considering LPF and RF
      QDesiredVV              : Array of Double; // volt-var new set-point
      QDesiredWP              : Array of Double; // watt-pf new set-point
      QDesiredWV              : Array of Double; // watt-var new set-point
      QOld                    : Array of Double;
      QOldVV                  : Array of Double;
      QOldDRC                 : Array of Double;
      QOldVVDRC               : Array of Double;
      QDesiredDRC             : Array of Double; //dynamic reactive power new set-point
      QDesiredVVDRC           : Array of Double;

      {Variables of functions that CONTROL reactive power}
      QHeadRoom               : Array of Double;
      QHeadRoomNeg            : Array of Double;
      Qoutputpu               : Array of Double;
      QoutputVVpu             : Array of Double;
      QoutputDRCpu            : Array of Double;
      QoutputVVDRCpu          : Array of Double;

      FPriorvarspu            : Array of Double;
      FPriorvars              : Array of Double;

      {Variables of functions that LIMIT active power}
      PBase                   : Array of Double;

      FPriorWattspu           : Array of Double;
      FPriorwatts             : Array of Double;

      {Variables of DER element}
      FDERPointerList         : PointerList.TPointerList;
      FListSize               : Integer;
      FDERNameList            : TStringList;
      FVBase                  : Array of Double;
      FVarFollowInverter      : Array of Boolean;
      FInverterON             : Array of Boolean;
      FpresentkW              : Array of Double;
      FkVARating              : Array of Double;
      Fpresentkvar            : Array of Double;
      FkvarLimit              : Array of Double;
      FkvarLimitNeg           : Array of Double;
      FCurrentkvarLimit       : Array of Double;
      FCurrentkvarLimitNeg    : Array of Double;
      FDCkWRated              : Array of Double;  // Pmpp for PVSystem, kWRated for Storage
      FpctDCkWRated           : Array of Double;  // pctPmpp for PVSystem, pctkWRated for Storage
      FEffFactor              : Array of Double;
      FDCkW                   : Array of Double;  // PanelkW for PVSystem, DCkW for Storage
      FPPriority              : Array of Boolean;
      NPhasesDER              : Array of Integer;
      NCondsDER               : Array of Integer;

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
      FPriorPLimitOptionpu    : Array of Double;
      FPriorQDesireOptionpu   : Array of Double;

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
      FlagChangeCurve         : Array of Boolean;
      FActiveVVCurve          : Array of Integer;
      FVAvgWindowLengthSec    : Double; // rolling average window length in seconds
      FRollAvgWindow          : Array of TRollAvgWindow;
      FRollAvgWindowLength    : Integer;
      priorRollAvgWindow      : Array of Double;
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
      deltaVDynReac           : Array of Double;
      FDRCRollAvgWindowpu     : Array of Double;
      FDRCRollAvgWindow       : Array of TRollAvgWindow;
      FDRCRollAvgWindowLength : Integer;
      FDRCRollAvgWindowLengthIntervalUnit : String;
      priorDRCRollAvgWindow   : Array of Double;
      FDRCVAvgWindowLengthSec : Double; // rolling average window length in seconds

      // volt-watt
      Fvoltwatt_curve_size    : Integer;
      Fvoltwatt_curve         : TXYcurveObj;
      Fvoltwatt_curvename     : String;

      // volt-watt (charging)
      FvoltwattCH_curve_size  : Integer;
      FvoltwattCH_curve       : TXYcurveObj;
      FvoltwattCH_curvename   : String;

      {Flags used to record function states. They are interval variables of DER}
      FVVOperation            : Array of Double;
      FVWOperation            : Array of Double;
      FDRCOperation           : Array of Double;
      FVVDRCOperation         : Array of Double;
      FWPOperation            : Array of Double;
      FWVOperation            : Array of Double;

      {Others}
      cBuffer                 : Array of Array of Complex;    // Complex array buffer
      CondOffset              : Array of Integer; // Offset for monitored terminal
      FPendingChange          : Array of Integer;

      {Functions and Procedures}
      procedure   Set_PendingChange(Value: Integer;DevIndex: Integer);
      function    Get_PendingChange(DevIndex: Integer):Integer;
      function    InterpretAvgVWindowLen(const s:string):Integer;
      function    InterpretDRCAvgVWindowLen(const s:string):Integer;
      function    ReturnElementsList:String;
      procedure   UpdateInvControl2(i:integer; ActorID : Integer);
      procedure   UpdateDERParameters(i: Integer);
      procedure   CalcVoltWatt_watts(j: Integer; ActorID : Integer);
      procedure   CalcQVVcurve_desiredpu(j: Integer; ActorID : Integer);
      procedure   CalcQWPcurve_desiredpu(j: Integer; ActorID : Integer);
      procedure   CalcQWVcurve_desiredpu(j: Integer; ActorID : Integer);
      procedure   CalcQDRC_desiredpu(j: Integer; ActorID : Integer);
      procedure   Check_Qlimits(j: Integer; Q: Double; ActorID : Integer);
      procedure   Calc_QHeadRoom(j: Integer; ActorID : Integer);
      procedure   CalcVoltVar_vars(j: Integer; ActorID : Integer);
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

      constructor Create(ParClass:TDSSClass; const InvControl2Name:String);
      destructor  Destroy; override;

      procedure   MakePosSequence(ActorID : Integer); Override;  // Make a positive Sequence Model
      procedure   RecalcElementData(ActorID : Integer); Override;
      procedure   CalcYPrim(ActorID : Integer); Override;    // Always Zero for a InvControl

      // Sample control quantities and set action times in Control Queue
      procedure   Sample(ActorID : Integer);  Override;

      // do the action that is pending from last sample
      procedure   DoPendingAction(Const Code, ProxyHdl:Integer;ActorID : Integer); Override;

      procedure   Reset; Override;  // Reset to initial defined state

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
      property monBus                    : TStringList             read FMonBusesNameList;
      property monBusVbase               : pDoubleArray       read FMonBusesVbase;
      // Need to include the new modes here

  end;


VAR
    ActiveInvControl2Obj : TInvControl2Obj;

IMPLEMENTATION

uses

    ParserDel, Sysutils, DSSClassDefs, DSSGlobals, Circuit,  uCmatrix, MathUtil, Math;

const

    NumPropsThisClass = 32;

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

    // Combi Modes
    NONE_COMBMODE = 0;
    VV_VW         = 1;
    VV_DRC        = 2;


constructor TInvControl2.Create;  // Creates superstructure for all InvControl objects
  begin
    Inherited Create;

     Class_name   := 'InvControl2';
     DSSClassType := DSSClassType + INV_CONTROL2;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
     XY_CurveClass := GetDSSClassPtr('XYCurve');

  end;

destructor TInvControl2.Destroy;
  begin
    Inherited Destroy;
  end;

procedure TInvControl2.DefineProperties;
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

    PropertyHelp[1] := 'Array list of PVSystem2 and/or Storage2 elements to be controlled. ' +
                       'If not specified, all PVSystem2 and Storage2 in the circuit are assumed to be controlled by this control. '  +CRLF+CRLF+
                      'No capability of hierarchical control between two controls for a single element is implemented at this time.';

    PropertyHelp[2] := 'Smart inverter function in which the InvControl2 will control the PC elements specified in DERList, according to the options below:' +CRLF+CRLF+
                      'Must be one of: {VOLTVAR* | VOLTWATT | DYNAMICREACCURR | WATTPF | WATTVAR} ' +CRLF+
                      'if the user desires to use modes simultaneously, then set the CombiMode property. Setting the Mode to any valid value disables combination mode.'+

                       CRLF+CRLF+'In volt-var mode (Default). This mode attempts to CONTROL the vars, according to one or two volt-var curves, depending on the monitored voltages, present active power output, and the capabilities of the PVSystem2/Storage2. ' +
                       CRLF+CRLF+'In volt-watt mode. This mode attempts to LIMIT the watts, according to one defined volt-watt curve, depending on the monitored voltages and the capabilities of the PVSystem2/Storage2. '+
                       CRLF+CRLF+'In dynamic reactive current mode. This mode attempts to increasingly counter deviations by CONTROLLING vars, depending on the monitored voltages, present active power output, and the capabilities of the of the PVSystem2/Storage2.'+
                       CRLF+CRLF+'In watt-pf mode. This mode attempts to CONTROL the vars, according to a watt-pf curve, depending on the present active power output, and the capabilities of the PVSystem2/Storage2. '+
                       CRLF+CRLF+'In watt-var mode. This mode attempts to CONTROL the vars, according to a watt-var curve, depending on the present active power output, and the capabilities of the PVSystem2/Storage2. ';

    PropertyHelp[3] := 'Combination of smart inverter functions in which the InvControl2 will control the PC elements in DERList, according to the options below: '+CRLF+CRLF+
                      'Must be a combination of the following: {VV_VW | VV_DRC}. Default is to not set this property, in which case the single control mode in Mode is active.  ' +

                       CRLF+CRLF+'In combined VV_VW mode, both volt-var and volt-watt control modes are active simultaneously.  See help individually for volt-var mode and volt-watt mode in Mode property.'+
                       CRLF+'Note that the PVSystem2/Storage2 will attempt to achieve both the volt-watt and volt-var set-points based on the capabilities of the inverter in the PVSystem2/Storage2 (kVA rating, etc), any limits set on maximum active power,' +
//                       CRLF+', any limits set on maximum reactive power. '+
//                       CRLF+'Precedence will be given to either watt production or var production based on the setting of RefReactivePower.'+
                       CRLF+CRLF+'In combined VV_DRC, both the volt-var and the dynamic reactive current modes are simultaneously active.';
//                       CRLF+CRLF+'The volt-var function will attempt to achieve its set-point based on the volt-var curve, and present voltage.  The dynamic '+
//                       CRLF+'reactive power mode function will also be active and it will add or subtract from the reactive power set-point desired by the volt-var function.'+
//                       CRLF+'Note that the precedence of active and reactive power production is defined by the RefReactivePower property.  In no event will the reactive '+
//                       CRLF+'power exceed the maximum var limit of the PVSystem2, and the combination of the active and reactive power output will not exceed the kVA rating of '+
//                       CRLF+'the inverter (set in the PVSystem2/Storage2).';

    PropertyHelp[4] := 'Required for VOLTVAR mode. '+CRLF+CRLF+
                      'Name of the XYCurve object containing the volt-var curve. The positive values of the y-axis of the volt-var curve represent values in pu of the provided base reactive power. ' +
                      'The negative values of the y-axis are values in pu of the absorbed base reactive power. ' +CRLF+
                      'Provided and absorbed base reactive power values are defined in the RefReactivePower property' +CRLF+CRLF+
                      'Units for the x-axis are per-unit voltage, which may be in per unit of the rated voltage for the PVSystem2/Storage2, or may be in per unit of the average voltage at the terminals over a user-defined number of prior solutions. ';

    PropertyHelp[5] := 'Required for VOLTVAR mode, and defaults to 0. '+CRLF+CRLF+
                      'for the times when the terminal voltage is decreasing, this is the off-set in per-unit voltage of a curve whose shape is the same as vvc_curve. '+
                      'It is offset by a certain negative value of per-unit voltage, which is defined by the base quantity for the x-axis of the volt-var curve (see help for voltage_curvex_ref)'+CRLF+CRLF+
                      'if the PVSystem2/Storage2 terminal voltage has been increasing, and has not changed directions, utilize vvc_curve1 for the volt-var response. '+CRLF+CRLF+
                      'if the PVSystem2/Storage2 terminal voltage has been increasing and changes directions and begins to decrease, then move from utilizing vvc_curve1 to a volt-var curve of the same shape, but offset by a certain per-unit voltage value. '+CRLF+CRLF+
                      'Maintain the same per-unit available var output level (unless head-room has changed due to change in active power or kva rating of PVSystem2/Storage2).  Per-unit var values remain the same for this internally constructed second curve (hysteresis curve). '+CRLF+CRLF+
                      'if the terminal voltage has been decreasing and changes directions and begins to increase , then move from utilizing the offset curve, back to the vvc_curve1 for volt-var response, but stay at the same per-unit available vars output level.';

    PropertyHelp[6] := 'Required for VOLTVAR and VOLTWATT modes, and defaults to rated.  Possible values are: {rated|avg|ravg}.  '+CRLF+CRLF+
                      'Defines whether the x-axis values (voltage in per unit) for vvc_curve1 and the volt-watt curve corresponds to:'+CRLF+CRLF+
                      'rated. The rated voltage for the PVSystem2/Storage2 object (1.0 in the volt-var curve equals rated voltage).'+CRLF+CRLF+
                      'avg. The average terminal voltage recorded over a certain number of prior power-flow solutions.'+CRLF+
                      'with the avg setting, 1.0 per unit on the x-axis of the volt-var curve(s) corresponds to the average voltage.'+CRLF+
                      'from a certain number of prior intervals.  See avgwindowlen parameter.'+CRLF+CRLF+
                      'ravg. Same as avg, with the exception that the avgerage terminal voltage is divided by the rated voltage.';

    PropertyHelp[7] := 'Required for VOLTVAR mode and VOLTWATT mode, and defaults to 0 seconds (0s). '+CRLF+CRLF+
                      'Sets the length of the averaging window over which the average PVSystem2/Storage2 terminal voltage is calculated. '+CRLF+CRLF+
                      'Units are indicated by appending s, m, or h to the integer value. '+CRLF+CRLF+
                      'The averaging window will calculate the average PVSystem2/Storage2 terminal voltage over the specified period of time, up to and including the last power flow solution. '+CRLF+CRLF+
                      'Note, if the solution stepsize is larger than the window length, then the voltage will be assumed to have been constant over the time-frame specified by the window length.';

    PropertyHelp[8] := 'Required for VOLTWATT mode. '+CRLF+CRLF+
                      'Name of the XYCurve object containing the volt-watt curve. '+CRLF+CRLF+
                      'Units for the x-axis are per-unit voltage, which may be in per unit of the rated voltage for the PVSystem2/Storage2, or may be in per unit of the average voltage at the terminals over a user-defined number of prior solutions. '+CRLF+CRLF+
                      'Units for the y-axis are either in one of the options described in the VoltwattYAxis property. ';

    PropertyHelp[9] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.95 per-unit voltage (referenced to the PVSystem2/Storage2 object rated voltage or a windowed average value). '+CRLF+CRLF+
                      'This parameter is the minimum voltage that defines the voltage dead-band within which no reactive power is allowed to be generated. ';

    PropertyHelp[10] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 1.05 per-unit voltage (referenced to the PVSystem2 object rated voltage or a windowed average value). '+CRLF+CRLF+
                      'This parameter is the maximum voltage that defines the voltage dead-band within which no reactive power is allowed to be generated. ';

    PropertyHelp[11] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.1  '+CRLF+CRLF+
                       'This is a gradient, expressed in unit-less terms of %/%, to establish the ratio by which percentage capacitive reactive power production is increased as the  percent delta-voltage decreases below DbVMin. '+CRLF+CRLF+
                       'Percent delta-voltage is defined as the present PVSystem2/Storage2 terminal voltage minus the moving average voltage, expressed as a percentage of the rated voltage for the PVSystem2/Storage2 object. '+CRLF+CRLF+
                       'Note, the moving average voltage for the dynamic reactive current mode is different than the moving average voltage for the volt-watt and volt-var modes.';

    PropertyHelp[12] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.1  '+CRLF+CRLF+
                       'This is a gradient, expressed in unit-less terms of %/%, to establish the ratio by which percentage inductive reactive power production is increased as the  percent delta-voltage decreases above DbVMax. '+CRLF+CRLF+
                       'Percent delta-voltage is defined as the present PVSystem2/Storage2 terminal voltage minus the moving average voltage, expressed as a percentage of the rated voltage for the PVSystem2/Storage2 object. '+CRLF+CRLF+
                       'Note, the moving average voltage for the dynamic reactive current mode is different than the mmoving average voltage for the volt-watt and volt-var modes.';

    PropertyHelp[13] := 'Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 1 seconds (1s). do not use a value smaller than 1.0 '+CRLF+CRLF+
                       'Sets the length of the averaging window over which the average PVSystem2/Storage2 terminal voltage is calculated '+
                       'for the dynamic reactive current mode. '+CRLF+CRLF+
                       'Units are indicated by appending s, m, or h to the integer value. '+CRLF+CRLF+
                       'Typically this will be a shorter averaging window than the volt-var and volt-watt averaging window.'+CRLF+CRLF+
                       'The averaging window will calculate the average PVSystem2/Storage2 terminal voltage over the specified period of time, up to and including the last power flow solution.  Note, if the solution stepsize is larger than '+
                       'the window length, then the voltage will be assumed to have been constant over the time-frame specified by the window length.';

    PropertyHelp[14] := 'Required for the VOLTVAR and DYNAMICREACCURR modes.  Defaults to -1.0. '+CRLF+CRLF+
                       'Defining -1.0, OpenDSS takes care internally of delta_Q itself. It tries to improve convergence as well as speed up process'+CRLF+CRLF+
                       'Sets the maximum change (in per unit) from the prior var output level to the desired var output level during each control iteration. '+CRLF+CRLF+CRLF+
                       'if numerical instability is noticed in solutions such as var sign changing from one control iteration to the next and voltages oscillating between two values with some separation, '+
                       'this is an indication of numerical instability (use the EventLog to diagnose). '+CRLF+CRLF+
                       'if the maximum control iterations are exceeded, and no numerical instability is seen in the EventLog of via monitors, then try increasing the value of this parameter to reduce the number '+
                       'of control iterations needed to achieve the control criteria, and move to the power flow solution.';

    PropertyHelp[15] := 'Defaults to 0.0001 per-unit voltage.  This parameter should only be modified by advanced users of the InvControl2.  '+CRLF+CRLF+
                        'Tolerance in pu of the control loop convergence associated to the monitored voltage in pu. ' +
                        'This value is compared with the difference of the monitored voltage in pu of the current and previous control iterations of the control loop'+CRLF+CRLF+

                        'This voltage tolerance value plus the var/watt tolerance value (VarChangeTolerance/ActivePChangeTolerance) determine, together, when to stop control iterations by the InvControl2. '+CRLF+CRLF+

                        'If an InvControl2 is controlling more than one PVSystem2/Storage2, each PVSystem2/Storage2 has this quantity calculated independently, and so an individual '+
                        'PVSystem2/Storage2 may reach the tolerance within different numbers of control iterations.';

    PropertyHelp[16] := 'Required for VOLTVAR and DYNAMICREACCURR modes.  Defaults to 0.025 per unit of the base provided or absorbed reactive power described in the RefReactivePower property '+

                       'This parameter should only be modified by advanced users of the InvControl2. '+CRLF+CRLF+

                       'Tolerance in pu of the convergence of the control loop associated with reactive power. ' +
                       'For the same control iteration, this value is compared to the difference, as an absolute value (without sign), between the desired reactive power value in pu and the output reactive power in pu of the controlled element.'+CRLF+CRLF+

                       'This reactive power tolerance value plus the voltage tolerance value (VoltageChangeTolerance) determine, together, when to stop control iterations by the InvControl2.  '+CRLF+CRLF+

                       'If an InvControl2 is controlling more than one PVSystem2/Storage2, each PVSystem2/Storage2 has this quantity calculated independently, and so an individual '+
                       'PVSystem2/Storage2 may reach the tolerance within different numbers of control iterations.';

    PropertyHelp[17] := 'Required for VOLTWATT mode.  Must be one of: {PMPPPU* | PAVAILABLEPU| PCTPMPPPU | KVARATINGPU}.  The default is PMPPPU.  '+CRLF+CRLF+
                       'Units for the y-axis of the volt-watt curve while in volt-watt mode. '+CRLF+CRLF+
                       'When set to PMPPPU. The y-axis corresponds to the value in pu of Pmpp property of the PVSystem2. '+CRLF+CRLF+
                       'When set to PAVAILABLEPU. The y-axis corresponds to the value in pu of the available active power of the PVSystem2. '+CRLF+CRLF+
                       'When set to PCTPMPPPU. The y-axis corresponds to the value in pu of the power Pmpp multiplied by 1/100 of the %Pmpp property of the PVSystem2.' +CRLF+CRLF+
                       'When set to KVARATINGPU. The y-axis corresponds to the value in pu of the kVA property of the PVSystem2.';


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
                       'of control iterations needed to achieve the control criteria, and move to the power flow solution.';

    PropertyHelp[22] := '{Yes/True* | No/False} Default is YES for InvControl2. Log control actions to Eventlog.';

    PropertyHelp[23] := 'Required for any mode that has VOLTVAR, DYNAMICREACCURR and WATTVAR. Defaults to VARAVAL.' +CRLF+CRLF+
                        'Defines the base reactive power for both the provided and absorbed reactive power, according to one of the following options: '
                        +CRLF+CRLF+ 'VARAVAL. The base values for the provided and absorbed reactive power are equal to the available reactive power.'
                        +CRLF+CRLF+ 'VARMAX: The base values of the provided and absorbed reactive power are equal to the value defined in the kvarMax and kvarMaxAbs properties, respectively.';

    PropertyHelp[24] :=  'Required for VOLTWATT. Default is 0.01'+CRLF+CRLF+
                         'Tolerance in pu of the convergence of the control loop associated with active power. ' +
                         'For the same control iteration, this value is compared to the difference between the active power limit in pu resulted from the convergence process and the one resulted from the volt-watt function.'+CRLF+CRLF+

                         'This reactive power tolerance value plus the voltage tolerance value (VoltageChangeTolerance) determine, together, when to stop control iterations by the InvControl2.  '+CRLF+CRLF+

                        'If an InvControl2 is controlling more than one PVSystem2/Storage2, each PVSystem2/Storage2 has this quantity calculated independently, and so an individual '+
                        'PVSystem2/Storage2 may reach the tolerance within different numbers of control iterations.';

    PropertyHelp[25] := 'Number of the phase being monitored or one of {AVG | MAX | MIN} for all phases. Default=AVG. ';

    PropertyHelp[26] := 'Name of monitored bus used by the voltage-dependente control modes. Default is bus of the controlled PVSystem2/Storage2 or Storage2.' ;

    PropertyHelp[27] := 'Array list of rated voltages of the buses and their nodes presented in the monBus property. This list may have different line-to-line and/or line-to-ground voltages.' ;

    PropertyHelp[28] := 'Required for VOLTWATT mode for Storage2 element in CHARGING state. '+CRLF+CRLF+
                        'The name of an XYCurve object that describes the variation in active power output (in per unit of maximum active power outut for the Storage2). '+CRLF+CRLF+
                        'Units for the x-axis are per-unit voltage, which may be in per unit of the rated voltage for the Storage2, or may be in per unit of the average voltage at the terminals over a user-defined number of prior solutions. '+CRLF+CRLF+
                        'Units for the y-axis are either in: (1) per unit of maximum active power output capability of the Storage2, or (2) maximum available active power output capability (defined by the parameter: VoltwattYAxis), '+
                        'corresponding to the terminal voltage (x-axis value in per unit). '+CRLF+CRLF+
                        'No default -- must be specified for VOLTWATT mode for Storage2 element in CHARGING state.';

    PropertyHelp[29] := 'Required for WATTPF mode.' +CRLF+CRLF+
                      'Name of the XYCurve object containing the watt-pf curve.' +CRLF+
                      'The positive values of the y-axis are positive power factor values. ' +
                      'The negative values of the the y-axis are negative power factor values. ' +
                      'When positive, the output reactive power has the same direction of the output active power, and when negative, it has the opposite direction.' +CRLF+
                      'Units for the x-axis are per-unit output active power, and the base active power is the Pmpp for PVSystem2 and kWrated for Storage2.'+CRLF+CRLF+
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
                      'Units for the x-axis are per-unit output active power, and the base active power is the Pmpp for PVSystem2 and kWrated for Storage2.';
    PropertyHelp[31] := 'Deprecated, use RefReactivePower instead.';
    PropertyHelp[32] := 'Deprecated, use DERList instead.';


    ActiveProperty  := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

  end;

function TInvControl2.NewObject(const ObjName:String):Integer;
  begin
    // Make a new InvControl and add it to InvControl class list
    with ActiveCircuit[ActiveActor] do
      begin
        ActiveCktElement := TInvControl2Obj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
      end;
  end;

function TInvControl2.Edit(ActorID : Integer):Integer;
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
    ActiveInvControl2Obj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveInvControl2Obj;

    Result := 0;

    with ActiveInvControl2Obj do
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
              1: InterpretTStringListArray(Param, FDERNameList); // Read list of PVSystem2 and Storage2 objects in OpenDSS format and add to FDERNameList StringList.
              2: begin
                  if CompareTextShortest(Parser[ActorID].StrValue, 'voltvar')= 0 then
                    begin
                      ControlMode := VOLTVAR;
                      CombiControlMode := NONE_COMBMODE;
                    end
                  else if CompareTextShortest(Parser[ActorID].StrValue, 'voltwatt')= 0 then
                    begin
                      ControlMode := VOLTWATT;
                      CombiControlMode := NONE_COMBMODE;
                    end
                  else if CompareTextShortest(Parser[ActorID].StrValue, 'dynamicreaccurr')= 0 then
                    begin
                      ControlMode := DRC;
                      CombiControlMode := NONE_COMBMODE;
                    end
//                  else if CompareTextShortest(Parser[ActorID].StrValue, 'fixedpf')= 0 then     // (PR) what is this?
//                    begin
//                      ControlMode := 'FIXEDPF';
//                      CombiControlMode := '';
//                    end
                  else if CompareTextShortest(Parser[ActorID].StrValue, 'wattpf')= 0 then
                    begin
                      ControlMode := WATTPF;
                      CombiControlMode := NONE_COMBMODE;
                    end
                  else if CompareTextShortest(Parser[ActorID].StrValue, 'wattvar')= 0 then
                    begin
                      ControlMode := WATTVAR;
                      CombiControlMode := NONE_COMBMODE;
                    end
                  else
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
                    InterpretTStringListArray(Param, FDERNameList); // Read list of PVSystem2 and Storage2 objects in OpenDSS format and add to FDERNameList StringList.
                    // Because is using this command from the previous version of InvControl, we assume that the list includes only
                    // PVSystems, so the list is updated
                    for CharPos := 0 to (FDERNameList.Count - 1) do
                      FDERNameList[CharPos] :=  'PVSystem.' + FDERNameList[CharPos];
                 end;

              else
                // Inherited parameters
                ClassEdit( ActiveInvControl2Obj, ParamPointer - NumPropsthisClass)
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

function TInvControl2.MakeLike(const InvControl2Name:String):Integer;
  VAR
    OtherInvControl2   : TInvControl2Obj;
    i, j              : Integer;

  begin
    Result := 0;
    {See if we can find this InvControl name in the present collection}
    OtherInvControl2 := Find(InvControl2Name);

   if OtherInvControl2<>Nil then

    with ActiveInvControl2Obj do begin

      NPhases := OtherInvControl2.Fnphases;
      NConds  := OtherInvControl2.Fnconds; // Force Reallocation of terminal stuff

      for i := 1 to FDERPointerList.ListSize do
        begin

          ControlledElement[i]        := OtherInvControl2.ControlledElement[i];
          CondOffset[i]               := OtherInvControl2.CondOffset[i];

          FVBase[i]                   :=  OtherInvControl2.FVBase[i];
          FVarFollowInverter[i]       :=  OtherInvControl2.FVarFollowInverter[i];
          FInverterON[i]              :=  OtherInvControl2.FInverterON[i];
          FpresentkW[i]               :=  OtherInvControl2.FpresentkW[i];
          FkVARating[i]               :=  OtherInvControl2.FkVARating[i];
          Fpresentkvar[i]             :=  OtherInvControl2.Fpresentkvar[i];
          FkvarLimit[i]               :=  OtherInvControl2.FkvarLimit[i];
          FkvarLimitNeg[i]            :=  OtherInvControl2.FkvarLimitNeg[i];
          FCurrentkvarLimit[i]        :=  OtherInvControl2.FCurrentkvarLimit[i];
          FCurrentkvarLimitNeg[i]     :=  OtherInvControl2.FCurrentkvarLimitNeg[i];
          FDCkWRated[i]               :=  OtherInvControl2.FDCkWRated[i];
          FpctDCkWRated[i]            :=  OtherInvControl2.FpctDCkWRated[i];
          FEffFactor[i]               :=  OtherInvControl2.FEffFactor[i];
          FDCkW[i]                    :=  OtherInvControl2.FDCkW[i];
          FPPriority[i]               :=  OtherInvControl2.FPPriority[i];
         end;

      ControlMode                     := OtherInvControl2.ControlMode;
      CombiControlMode                := OtherInvControl2.CombiControlMode;
      FListSize                       := OtherInvControl2.FListSize;
      Fvvc_curve_size                 := OtherInvControl2.Fvvc_curve_size;
      Fvvc_curve                      := OtherInvControl2.Fvvc_curve;
      Fvvc_curvename                  := OtherInvControl2.Fvvc_curvename;
      Fvvc_curveOffset                := OtherInvControl2.Fvvc_curveOffset;
      FVoltage_CurveX_ref             := OtherInvControl2.FVoltage_CurveX_ref;
      FDRCVAvgWindowLengthSec         := OtherInvControl2.FDRCVAvgWindowLengthSec;
      FVAvgWindowLengthSec            := OtherInvControl2.FVAvgWindowLengthSec;
      Fvoltwatt_curve_size            := OtherInvControl2.Fvoltwatt_curve_size;
      Fvoltwatt_curve                 := OtherInvControl2.Fvoltwatt_curve;
      Fvoltwatt_curvename             := OtherInvControl2.Fvoltwatt_curvename;
      FvoltwattCH_curve_size          := OtherInvControl2.FvoltwattCH_curve_size;
      FvoltwattCH_curve               := OtherInvControl2.FvoltwattCH_curve;
      FvoltwattCH_curvename           := OtherInvControl2.FvoltwattCH_curvename;
      Fwattpf_curve_size              := OtherInvControl2.Fwattpf_curve_size;
      Fwattpf_curve                   := OtherInvControl2.Fwattpf_curve;
      Fwattpf_curvename               := OtherInvControl2.Fwattpf_curvename;
      Fwattvar_curve_size             := OtherInvControl2.Fwattvar_curve_size;
      Fwattvar_curve                  := OtherInvControl2.Fwattvar_curve;
      Fwattvar_curvename              := OtherInvControl2.Fwattvar_curvename;
      FDbVMin                         := OtherInvControl2.FDbVMin;
      pf_wp_nominal                   := OtherInvControl2.pf_wp_nominal;
      FDbVMax                         := OtherInvControl2.FDbVMax;
      FArGraLowV                      := OtherInvControl2.FArGraLowV;
      FArGraHiV                       := OtherInvControl2.FArGraHiV;
      FActiveVVCurve                  := OtherInvControl2.FActiveVVCurve;
      FRollAvgWindowLength            := OtherInvControl2.FRollAvgWindowLength;
      FRollAvgWindowLengthIntervalUnit      := OtherInvControl2.FRollAvgWindowLengthIntervalUnit;
      FDRCRollAvgWindowLength         := OtherInvControl2.FDRCRollAvgWindowLength;
      FDRCRollAvgWindowLengthIntervalUnit   := OtherInvControl2.FDRCRollAvgWindowLengthIntervalUnit;
      FActivePChangeTolerance         := OtherInvControl2.FActivePChangeTolerance;
      FdeltaQ_factor                  := OtherInvControl2.FdeltaQ_factor;
      FdeltaP_factor                  := OtherInvControl2.FdeltaP_factor;
      FVoltageChangeTolerance         := OtherInvControl2.FVoltageChangeTolerance;
      FVarChangeTolerance             := OtherInvControl2.FVarChangeTolerance;
      FVoltwattYAxis                  := OtherInvControl2.FVoltwattYAxis;
      RateofChangeMode                := OtherInvControl2.RateofChangeMode;
      FLPFTau                         := OtherInvControl2.FLPFTau;
      FRiseFallLimit                  := OtherInvControl2.FRiseFallLimit;
      FMonBusesPhase                        := OtherInvControl2.FMonBusesPhase;
      FMonBuses                    := OtherInvControl2.FMonBuses;
      FMonBusesNodes                     := OtherInvControl2.FMonBusesNodes;

      ReallocMem(FMonBusesVbase, SizeOf(FMonBusesVbase^[1])*FMonBusesNameList.Count);
      for j := 1 to FMonBusesNameList.Count do FMonBusesVbase^[j] := OtherInvControl2.FMonBusesVbase^[j];

      TimeDelay                  := OtherInvControl2.TimeDelay;
      for j := 1 to ParentClass.NumProperties do PropertyValue[j] := OtherInvControl2.PropertyValue[j];

    end
   else  DoSimpleMsg('Error in InvControl2 MakeLike: "' + InvControl2Name + '" Not Found.', 370);

end;

{==========================================================================}
{                    TInvControlObj                                        }
{==========================================================================}
constructor TInvControl2Obj.Create(ParClass:TDSSClass; const InvControl2Name:String);

  begin

    Inherited Create(ParClass);
    Name                     := LowerCase(InvControl2Name);
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
    ControlledElement        := nil;

    {Variables for voltages}
    FAvgpVpuPrior            := nil;
    FAvgpDRCVpuPrior         := nil;
    FPresentVpu              := nil;
    FPresentDRCVpu           := nil;
    FVpuSolution             := nil;
    FVpuSolutionIdx          := 0;


    {Variables for convergence process}
    FdeltaQ_factor           := FLAGDELTAQ;
    FdeltaP_factor           := FLAGDELTAP;

    FdeltaQFactor            := nil;
    FdeltaPFactor            := nil;
    DeltaV_old               := nil;

    FVoltageChangeTolerance  := 0.0001;
    FVarChangeTolerance      := 0.025;
    FActivePChangeTolerance  := 0.01;

    // Active power
    PLimitVW                 := nil;
    POldVWpu                 := nil;
    FFlagVWOperates          := nil;
    PLimitVWpu               := nil;
    PLimitLimitedpu          := nil;
    PLimitEndpu              := nil;
    PLimitOptionpu           := nil;
    kW_out_desiredpu         := nil;
    kW_out_desired           := nil;

    // Reactive power
    QDesireEndpu             := nil;
    QDesireVVpu              := nil;
    QDesireWPpu              := nil;
    QDesireWVpu              := nil;
    QDesireDRCpu             := nil;
    QDesireLimitedpu         := nil;
    QDesireOptionpu          := nil;
    QDesiredVV               := nil;
    QDesiredWP               := nil;
    QDesiredWV               := nil;
    QOld                     := nil;
    QOldVV                   := nil;
    QOldDRC                  := nil;
    QOldVVDRC                := nil;
    QDesiredDRC              := nil;
    QDesiredVVDRC            := nil;

    {Variables of functions that CONTROL reactive power}
    QHeadRoom                := nil;
    QHeadRoomNeg             := nil;
    Qoutputpu                := nil;
    QoutputVVpu              := nil;
    QoutputDRCpu             := nil;
    QoutputVVDRCpu           := nil;

    FPriorvarspu             := nil;
    FPriorvars               := nil;

    {Variables of functions that LIMIT active power}
    PBase                    := nil;

    FPriorWattspu            := nil;
    FPriorWatts              := nil;

    {Variables of DER element}
    FDERNameList             := nil;
    FDERPointerList          := nil;
    FDERPointerList          := PointerList.TPointerList.Create(20);  // Default size and increment
    FDERNameList             := TSTringList.Create;
    FVBase                   := nil;
    FVarFollowInverter       := nil;
    FInverterON              := nil;
    FpresentkW               := nil;
    FkVARating               := nil;
    Fpresentkvar             := nil;
    FkvarLimit               := nil;
    FkvarLimitNeg            := nil;
    FCurrentkvarLimit        := nil;
    FCurrentkvarLimitNeg     := nil;
    FDCkWRated               := nil;
    FpctDCkWRated            := nil;
    FEffFactor               := nil;
    FDCkW                    := nil;
    FPPriority               := nil;
    NPhasesDER               := nil;
    NCondsDER                := nil;

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
    FPriorPLimitOptionpu     := nil;
    FPriorQDesireOptionpu    := nil;

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
    FActiveVVCurve           := nil;
    FlagChangeCurve          := nil;
    FVAvgWindowLengthSec     := 1.0;
    FRollAvgWindow           := nil;
    FRollAvgWindowLength     := 1;
    FRollAvgWindowLengthIntervalUnit := 's';
    priorRollAvgWindow       := nil;

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
    FDRCRollAvgWindow        := nil;
    FDRCRollAvgWindowLength  := 1;
    FDRCRollAvgWindowLengthIntervalUnit := 's';
    FDRCVAvgWindowLengthSec  := 1.0;
    priorDRCRollAvgWindow    := nil;
    deltaVDynReac            := nil;

    // volt-watt
    Fvoltwatt_curve_size     := 0;
    Fvoltwatt_curve          := nil;
    Fvoltwatt_curvename      := '';
    FvoltwattCH_curve_size   := 0;
    FvoltwattCH_curve        := nil;
    FvoltwattCH_curvename    := '';

    {Flags used to record function states. They are interval variables of DER}
    FVVOperation             := nil;
    FVWOperation             := nil;
    FDRCOperation            := nil;
    FVVDRCOperation          := nil;
    FWPOperation             := nil;
    FWVOperation             := nil;

    {Others}
    FPendingChange           := nil;
    cBuffer                  := nil;
    CondOffset               := nil;
    InitPropertyValues(0);

  end;

destructor TInvControl2Obj.Destroy;
  begin
    ElementName := '';
    Finalize(ControlledElement);
    Finalize(NPhasesDER);
    Finalize(NCondsDER);
    Finalize(cBuffer);
    Finalize(CondOffset);
    Finalize(FRollAvgWindow);
    Finalize(FDRCRollAvgWindow);
    Finalize(FDRCRollAvgWindowpu);
    Finalize(FAvgpVpuPrior);
    Finalize(FAvgpDRCVpuPrior);
    Finalize(FPresentVpu);
    Finalize(FPresentDRCVpu);
    Finalize(FPendingChange);
    Finalize(QDesiredVV);
    Finalize(QDesiredWP);
    Finalize(QDesiredWV);
    Finalize(QOld);
    Finalize(QOldVV);
    Finalize(QOldDRC);
    Finalize(QOldVVDRC);
    Finalize(QHeadroom);
    Finalize(QHeadroomNeg);
    Finalize(Qoutputpu);
    Finalize(QoutputVVpu);
    Finalize(QoutputDRCpu);
    Finalize(QoutputVVDRCpu);
    Finalize(QDesireEndpu);
    Finalize(QDesireVVpu);
    Finalize(QDesireWPpu);
    Finalize(QDesireWVpu);
    Finalize(QDesireLimitedpu);
    Finalize(QDesireOptionpu);
    Finalize(PLimitEndpu);
    Finalize(PLimitVWpu);
    Finalize(PLimitLimitedpu);
    Finalize(PLimitOptionpu);
    Finalize(QDesireDRCpu);
    Finalize(QDesiredDRC);
    Finalize(QDesiredVVDRC);
    Finalize(PLimitVW);
    Finalize(POldVWpu);
    Finalize(PBase);
    Finalize(deltaVDynReac);
    Finalize(priorRollAvgWindow);
    Finalize(priorDRCRollAvgWindow);
    Finalize(FVpuSolution);
    Finalize(FlagChangeCurve);
    Finalize(FActiveVVCurve);
    Finalize(FPriorWattspu);
    Finalize(FPriorWatts);
    Finalize(FPriorPLimitOptionpu);
    Finalize(FPriorQDesireOptionpu);
    Finalize(kW_out_desiredpu);
    Finalize(kW_out_desired);
    Finalize(FPriorvarspu);
    Finalize(FPriorvars);
    Finalize(FdeltaQFactor);
    Finalize(FdeltaPFactor);
    Finalize(DeltaV_old);
    Finalize(FFlagVWOperates);
    Finalize(FVVOperation);
    Finalize(FVWOperation);
    Finalize(FDRCOperation);
    Finalize(FVVDRCOperation);
    Finalize(FWPOperation);
    Finalize(FWVOperation);
    Finalize(FMonBuses);
    Finalize(FMonBusesNodes);
    Finalize(FVBase);
    Finalize(FVarFollowInverter);
    Finalize(FInverterON);
    Finalize(FpresentkW);
    Finalize(FkVARating);
    Finalize(Fpresentkvar);
    Finalize(FkvarLimit);
    Finalize(FkvarLimitNeg);
    Finalize(FCurrentkvarLimit);
    Finalize(FCurrentkvarLimitNeg);
    Finalize(FDCkWRated);
    Finalize(FpctDCkWRated);
    Finalize(FEffFactor);
    Finalize(FDCkW);
    Finalize(FPPriority);

    if Assigned(FMonBusesVbase) then ReallocMem(FMonBusesVbase, 0);

    Inherited Destroy;
  end;

procedure TInvControl2Obj.RecalcElementData(ActorID : Integer);

  VAR
    i       :Integer;

  begin

    if FDERPointerList.ListSize = 0 then  MakeDERList;

    if FDERPointerList.ListSize > 0  then
    {Setting the terminal of the InvControl device to same as the 1st PVSystem2/Storage2 element}
    { This sets it to a realistic value to avoid crashes later }
      begin
        MonitoredElement :=  TDSSCktElement(FDERPointerList.Get(1));   // Set MonitoredElement to 1st elemnent in list
        Setbus(1, MonitoredElement.Firstbus);
      end;

    for i := 1 to FDERPointerList.ListSize do
      begin

        // User ControlledElement[] as the pointer to the PVSystem2/Storage2 elements
        ControlledElement[i] :=  TPCElement(FDERPointerList.Get(i));  // pointer to i-th PVSystem2/Storage2 element
        SetLength(cBuffer[i], SizeOF(Complex) * ControlledElement[i].Yorder );


        ControlledElement[i].ActiveTerminalIdx := 1; // Make the 1 st terminal active
        Nphases := ControlledElement[i].NPhases;
        Nconds  := Nphases;
        FRollAvgWindow[i].BuffLength    := FRollAvgWindowLength; // TEMc
        FDRCRollAvgWindow[i].BuffLength := FDRCRollAvgWindowLength;

        // for all modes other than VW and WATTPF, PF priority is not allowed
        if ((Mode <> VOLTWATT) and (Mode <> WATTPF)) Then
        Begin
            if ControlledElement[i].DSSClassName = 'PVSystem2'     then TPVSystem2Obj(ControlledElement[i]).PVSystem2Vars.PF_Priority := FALSE
            else if ControlledElement[i].DSSClassName = 'Storage2' then  TStorage2Obj(ControlledElement[i]).Storage2Vars.PF_Priority := FALSE;
        End;

        //FdeltaQFactor[i]                := FdeltaQ_factor;
        //FdeltaPFactor[i]                := FdeltaP_factor;

        if Length(FMonBuses)=0 then FUsingMonBuses := FALSE else FUsingMonBuses := TRUE;

        if (ControlledElement[i] <> Nil) then UpdateDERParameters(i)
        else
        begin
          ControlledElement[i] := nil;
          DoErrorMsg('InvControl2: "' + Self.Name + '"',
                          'Controlled Element "' + FDERNameList.Strings[i-1] + '" Not Found.',
                          ' PVSystem2 or Storage2 object must be defined previously.', 361);
        end;
      end;

  end;

procedure TInvControl2Obj.MakePosSequence(ActorID : Integer);

// ***  This assumes the PVSystem2/Storage2 devices have already been converted to pos seq

  begin

    if FDERPointerList.ListSize = 0 then  RecalcElementData(ActorID);
    Nphases := 3;
    Nconds := 3;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));

    if FDERPointerList.ListSize > 0  then
    {Setting the terminal of the InvControl device to same as the 1st PVSystem2/Storage2 element}
    { This sets it to a realistic value to avoid crashes later }
      begin
        MonitoredElement :=  TDSSCktElement(FDERPointerList.Get(1));   // Set MonitoredElement to 1st PVSystem2/Storage2 in list
        Setbus(1, MonitoredElement.Firstbus);
        Nphases := MonitoredElement.NPhases;
        Nconds := Nphases;
      end;
    inherited;
  end;

procedure TInvControl2Obj.CalcYPrim(ActorID : Integer);
  begin
    // leave YPrims as nil and they will be ignored
    // Yprim is zeroed when created.  Leave it as is.
    //  if YPrim=nil then YPrim := TcMatrix.CreateMatrix(Yorder);
  end;

procedure TInvControl2Obj.GetCurrents(Curr: pComplexArray; ActorID : Integer);
  VAR
    i:Integer;
  begin
    // Control is a zero current source
    for i := 1 to Fnconds do Curr^[i] := CZERO;
  end;

procedure TInvControl2Obj.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);
  VAR
    i:Integer;
  begin
    // Control is a zero current source
    for i := 1 to Fnconds do Curr^[i] := CZERO;
  end;

procedure TInvControl2Obj.DumpProperties(Var F:TextFile; Complete:Boolean);
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

procedure TInvControl2Obj.DoPendingAction(Const Code, ProxyHdl:Integer;ActorID : Integer);

  VAR
    k                                           : Integer;
    DERelem                                     : TPCElement;

  begin

    for k := 1 to FDERPointerList.ListSize do
      begin

        DERelem := ControlledElement[k];

        // Calculates QHeadRoom
        Calc_QHeadRoom(k, ActorID);
        if QHeadRoom[k] <> 0.0 then FPriorvarspu[k]  := FPriorvars[k]/QHeadRoom[k];

        // Calculates PBase
        Calc_PBase(k, ActorID);
        FPriorWattspu[k]  := FPriorWatts[k]/PBase[k];

        // Calculates kW_out_desiredpu. Used for VW and VV_VW
        kW_out_desiredpu[k] := kW_out_desired[k] / PBase[k];

        // -------------------Smart Inverter Functions------------------------//
        {Smart Inverter volt-var function}
        if(ControlMode = VOLTVAR) and (CombiControlMode = NONE_COMBMODE) and (PendingChange[k]=CHANGEVARLEVEL) then
          begin
            // Set var mode to VARMODEKVAR to indicate we might change kvar
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                TPVSystem2Obj(DERelem).VWmode  := FALSE;
                TPVSystem2Obj(DERelem).Varmode := VARMODEKVAR;
                TPVSystem2Obj(DERelem).VVmode := TRUE;
              end
            else
              begin
                TStorage2Obj(DERelem).VWmode     := FALSE;
                TStorage2Obj(DERelem).Varmode    := VARMODEKVAR;
                TStorage2Obj(DERelem).VVmode     := TRUE;
              end;

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireVVpu[k]
            CalcQVVcurve_desiredpu(k, ActorID);

            // LPF or RF activated
            if (RateofChangeMode = LPF) then
              begin
                CalcLPF(k, 'VARS', QDesireVVpu[k], ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k], ActorID);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);
              end
            else if (RateofChangeMode = RISEFALL) then
              begin
                CalcRF(k, 'VARS', QDesireVVpu[k], ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k], ActorID);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);
              end
            else
              begin
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireVVpu[k], ActorID);
                QDesireEndpu[k] := Min(abs(QDesireVVpu[k]), abs(QDesireLimitedpu[k])) * sign(QDesireVVpu[k]);
              end;

            // Calculates QDesiredVV[k] through the convergence algorithm
            CalcVoltVar_vars(k, ActorID);

            //--------------------------------------------- end Main process ---------------------------------------------//

            // Sets PVSystem2/Storage2's kvar_out
            if ControlledElement[k].DSSClassName = 'PVSystem2' then TPVSystem2Obj(DERelem).Presentkvar := QDesiredVV[k]
            else TStorage2Obj(DERelem).kvarRequested := QDesiredVV[k];

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                TPVSystem2Obj(DERelem).SetNominalPVSystem2Ouput(ActorID);

                if QDesiredVV[k] >= 0.0 then Qoutputpu[k] := TPVSystem2Obj(DERelem).Presentkvar / QHeadroom[k]
                else Qoutputpu[k] := TPVSystem2Obj(DERelem).Presentkvar / QHeadroomNeg[k];
              end
            else
              begin
                TStorage2Obj(DERelem).SetNominalStorage2Output(ActorID);

                if QDesiredVV[k] >= 0.0 then Qoutputpu[k] := TStorage2Obj(DERelem).Presentkvar / QHeadroom[k]
                else Qoutputpu[k] := TStorage2Obj(DERelem).Presentkvar / QHeadroomNeg[k];
              end;

            // Values used in convergence
            QoutputVVpu[k] := Qoutputpu[k];
            FAvgpVpuPrior[k] := FPresentVpu[k];

            // Values used in CalcQVVcurve_desiredpu
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                QOld[k]   := TPVSystem2Obj(DERelem).Presentkvar;
                QOldVV[k] := TPVSystem2Obj(DERelem).Presentkvar;

              if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name +', '+ TPVSystem2Obj(DERelem).QualifiedName,
                                                    Format('VOLTVAR mode requested PVSystem2 output var level to**, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredVV[k], TPVSystem2Obj(DERelem).Presentkvar]),ActorID);
              end
            else
              begin
                QOld[k]   := TStorage2Obj(DERelem).Presentkvar;
                QOldVV[k] := TStorage2Obj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name +', '+  TStorage2Obj(DERelem).QualifiedName,
                                                    Format('VOLTVAR mode requested Storage2 output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredVV[k], TStorage2Obj(DERelem).Presentkvar]),ActorID);

              end;
          end

        {Smart Inverter watt-pf function}
        else if(ControlMode = WATTPF) and (CombiControlMode = NONE_COMBMODE) and (PendingChange[k]=CHANGEVARLEVEL) then
          begin
            // Set var mode to VARMODEKVAR to indicate we might change kvar
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                TPVSystem2Obj(DERelem).VWmode  := FALSE;
                TPVSystem2Obj(DERelem).Varmode := VARMODEKVAR;
                TPVSystem2Obj(DERelem).WPmode  := TRUE;
              end
            else
              begin
                TStorage2Obj(DERelem).VWmode     := FALSE;
                TStorage2Obj(DERelem).Varmode    := VARMODEKVAR;
                TStorage2Obj(DERelem).WPmode     := TRUE;
              end;

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireWPpu[k]
            CalcQWPcurve_desiredpu(k, ActorID);

            // Checks kVA (watt priority) and kvarlimit limits
            Check_Qlimits(k, QDesireWPpu[k], ActorID);
            QDesireEndpu[k] := Min(abs(QDesireWPpu[k]), abs(QDesireLimitedpu[k])) * sign(QDesireWPpu[k]);

            // Calculates QDesiredWP[k] through the convergence algorithm
            CalcWATTPF_vars(k, ActorID);

            //--------------------------------------------- end Main process ---------------------------------------------//
            // Sets PVSystem2/Storage2's pf_wp_nominal
            if ControlledElement[k].DSSClassName = 'PVSystem2' then TPVSystem2Obj(DERelem).pf_wp_nominal := pf_wp_nominal
            else TStorage2Obj(DERelem).kvarRequested := QDesiredWP[k];

            // Sets PVSystem2/Storage2's kvar_out
            if ControlledElement[k].DSSClassName = 'PVSystem2' then TPVSystem2Obj(DERelem).Presentkvar := QDesiredWP[k]
            else TStorage2Obj(DERelem).kvarRequested := QDesiredWP[k];

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                TPVSystem2Obj(DERelem).SetNominalPVSystem2Ouput(ActorID);

                if QDesiredWP[k] >= 0.0 then Qoutputpu[k] := TPVSystem2Obj(DERelem).Presentkvar / QHeadroom[k]
                else Qoutputpu[k] := TPVSystem2Obj(DERelem).Presentkvar / QHeadroomNeg[k];
              end
            else
              begin
                TStorage2Obj(DERelem).SetNominalStorage2Output(ActorID);

                if QDesiredWP[k] >= 0.0 then Qoutputpu[k] := TStorage2Obj(DERelem).Presentkvar / QHeadroom[k]
                else Qoutputpu[k] := TStorage2Obj(DERelem).Presentkvar / QHeadroomNeg[k];
              end;

            // Values used in convergence
            QoutputVVpu[k] := Qoutputpu[k];
            FAvgpVpuPrior[k] := FPresentVpu[k];

            // Values used in CalcQVVcurve_desiredpu
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                QOld[k]   := TPVSystem2Obj(DERelem).Presentkvar;
                QOldVV[k] := TPVSystem2Obj(DERelem).Presentkvar;

              if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name +', '+ TPVSystem2Obj(DERelem).QualifiedName,
                                                    Format('WATTPF mode requested PVSystem2 output var level to**, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredWP[k], TPVSystem2Obj(DERelem).Presentkvar]),ActorID);
              end
            else
              begin
                QOld[k]   := TStorage2Obj(DERelem).Presentkvar;
                QOldVV[k] := TStorage2Obj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name +', '+  TStorage2Obj(DERelem).QualifiedName,
                                                    Format('WATTPF mode requested Storage2 output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredWP[k], TStorage2Obj(DERelem).Presentkvar]),ActorID);

              end;
          end

        {Smart Inverter watt-var function}
        else if(ControlMode = WATTVAR) and (CombiControlMode = NONE_COMBMODE) and (PendingChange[k]=CHANGEVARLEVEL) then
          begin
            // Set var mode to VARMODEKVAR to indicate we might change kvar
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                TPVSystem2Obj(DERelem).VWmode  := FALSE;
                TPVSystem2Obj(DERelem).Varmode := VARMODEKVAR;
                TPVSystem2Obj(DERelem).WVmode  := TRUE;
              end
            else
              begin
                TStorage2Obj(DERelem).VWmode     := FALSE;
                TStorage2Obj(DERelem).Varmode    := VARMODEKVAR;
                TStorage2Obj(DERelem).WVmode     := TRUE;
              end;

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireWVpu[k]
            CalcQWVcurve_desiredpu(k, ActorID);

            // Checks kVA (watt priority) and kvarlimit limits
            Check_Qlimits(k, QDesireWVpu[k], ActorID);
            QDesireEndpu[k] := Min(abs(QDesireWVpu[k]), abs(QDesireLimitedpu[k])) * sign(QDesireWVpu[k]);

            // Calculates QDesiredWV[k] through the convergence algorithm
            CalcWATTVAR_vars(k, ActorID);

            //--------------------------------------------- end Main process ---------------------------------------------//

            // Sets PVSystem2/Storage2's kvar_out
            if ControlledElement[k].DSSClassName = 'PVSystem2' then TPVSystem2Obj(DERelem).Presentkvar := QDesiredWV[k]
            else TStorage2Obj(DERelem).kvarRequested := QDesiredWV[k];

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                TPVSystem2Obj(DERelem).SetNominalPVSystem2Ouput(ActorID);

                if QDesiredWV[k] >= 0.0 then Qoutputpu[k] := TPVSystem2Obj(DERelem).Presentkvar / QHeadroom[k]
                else Qoutputpu[k] := TPVSystem2Obj(DERelem).Presentkvar / QHeadroomNeg[k];
              end
            else
              begin
                TStorage2Obj(DERelem).SetNominalStorage2Output(ActorID);

                if QDesiredWV[k] >= 0.0 then Qoutputpu[k] := TStorage2Obj(DERelem).Presentkvar / QHeadroom[k]
                else Qoutputpu[k] := TStorage2Obj(DERelem).Presentkvar / QHeadroomNeg[k];
              end;

            // Values used in convergence
            QoutputVVpu[k] := Qoutputpu[k];
            FAvgpVpuPrior[k] := FPresentVpu[k];

            // Values used in CalcQVVcurve_desiredpu
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                QOld[k]   := TPVSystem2Obj(DERelem).Presentkvar;
                QOldVV[k] := TPVSystem2Obj(DERelem).Presentkvar;

              if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name +', '+ TPVSystem2Obj(DERelem).QualifiedName,
                                                    Format('WATTVAR mode requested PVSystem2 output var level to**, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredWV[k], TPVSystem2Obj(DERelem).Presentkvar]),ActorID);
              end
            else
              begin
                QOld[k]   := TStorage2Obj(DERelem).Presentkvar;
                QOldVV[k] := TStorage2Obj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name +', '+  TStorage2Obj(DERelem).QualifiedName,
                                                    Format('WATTVAR mode requested Storage2 output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredWV[k], TStorage2Obj(DERelem).Presentkvar]),ActorID);

              end;
          end

        {Smart Inverter DRC function}
        else if(ControlMode = DRC) and (CombiControlMode = NONE_COMBMODE) and (PendingChange[k]=CHANGEVARLEVEL) then
          begin

            // Set var mode to VARMODEKVAR to indicate we might change kvar
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                TPVSystem2Obj(DERelem).VWmode  := FALSE;
                TPVSystem2Obj(DERelem).Varmode := VARMODEKVAR;
                TPVSystem2Obj(DERelem).DRCmode := TRUE;
              end
            else
              begin
                TStorage2Obj(DERelem).VWmode  := FALSE;
                TStorage2Obj(DERelem).Varmode := VARMODEKVAR;
                TStorage2Obj(DERelem).DRCmode := TRUE;
              end;

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireDRCpu[k]
            CalcQDRC_desiredpu(k, ActorID);

            // LPF or RF activated
            if (RateofChangeMode = LPF) then
              begin
                CalcLPF(k, 'VARS', QDesireDRCpu[k], ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k], ActorID);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);
              end
            else if (RateofChangeMode = RISEFALL) then
              begin
                CalcRF(k, 'VARS', QDesireDRCpu[k], ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k], ActorID);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);
              end
            else
              begin
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireDRCpu[k], ActorID);
                QDesireEndpu[k] := Min(abs(QDesireDRCpu[k]), abs(QDesireLimitedpu[k])) * sign(QDesireDRCpu[k]);
              end;

            // Calculates QDesiredDRC[k]
            CalcDRC_vars(k, ActorID);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kvar_out
            if ControlledElement[k].DSSClassName = 'PVSystem2' then TPVSystem2Obj(DERelem).Presentkvar := QDesiredDRC[k]
            else TStorage2Obj(DERelem).kvarRequested := QDesiredDRC[k];

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                TPVSystem2Obj(DERelem).SetNominalPVSystem2Ouput(ActorID);

                if QDesiredDRC[k] >= 0.0 then Qoutputpu[k] := TPVSystem2Obj(DERelem).Presentkvar / QHeadroom[k]
                else Qoutputpu[k] := TPVSystem2Obj(DERelem).Presentkvar / QHeadroomNeg[k];
              end
            else
              begin
                TStorage2Obj(DERelem).SetNominalStorage2Output(ActorID);

                if QDesiredDRC[k] >= 0.0 then Qoutputpu[k] := TStorage2Obj(DERelem).Presentkvar / QHeadroom[k]
                else Qoutputpu[k] := TStorage2Obj(DERelem).Presentkvar / QHeadroomNeg[k];
              end;

            // Values used in convergence
            QoutputDRCpu[k] := Qoutputpu[k];
            FAvgpDRCVpuPrior[k] := FPresentDRCVpu[k];

            // Values used in CalcDRC_vars
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                QOld[k]    := TPVSystem2Obj(DERelem).Presentkvar;
                QOldDRC[k] := TPVSystem2Obj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name +', '+ TPVSystem2Obj(DERelem).QualifiedName,
                                                    Format('DRC mode requested PVSystem2 output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredDRC[k], TPVSystem2Obj(DERelem).Presentkvar]),ActorID);
              end
            else
              begin
                QOld[k]    := TStorage2Obj(DERelem).Presentkvar;
                QOldDRC[k] := TStorage2Obj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name +', '+  TStorage2Obj(DERelem).QualifiedName,
                                                    Format('DRC mode requested Storage2 output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredDRC[k], TStorage2Obj(DERelem).Presentkvar]),ActorID);

              end;
          end

        {Smart Inverter VV_DRC function}
        else if(ControlMode = NONE_MODE) and (CombiControlMode = VV_DRC) and (PendingChange[k]=CHANGEDRCVVARLEVEL) then
          begin

            // Set var mode to VARMODEKVAR to indicate we might change kvar
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                TPVSystem2Obj(DERelem).VWmode  := FALSE;
                TPVSystem2Obj(DERelem).Varmode := VARMODEKVAR;
                TPVSystem2Obj(DERelem).VVmode  := TRUE;
                TPVSystem2Obj(DERelem).DRCmode := TRUE;
              end
            else
              begin
                TStorage2Obj(DERelem).VWmode  := FALSE;
                TStorage2Obj(DERelem).Varmode := VARMODEKVAR;
                TStorage2Obj(DERelem).VVmode  := TRUE;
                TStorage2Obj(DERelem).DRCmode := TRUE;
              end;

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireVVpu[k] and  QDesireDRCpu[k]
            CalcQVVcurve_desiredpu(k, ActorID);
            CalcQDRC_desiredpu(k, ActorID);

            // LPF or RF activated
            if (RateofChangeMode = LPF) then
              begin
                CalcLPF(k, 'VARS', QDesireVVpu[k] + QDesireDRCpu[k], ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k], ActorID);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);
              end
            else if (RateofChangeMode = RISEFALL) then
              begin
                CalcRF(k, 'VARS', QDesireVVpu[k] + QDesireDRCpu[k], ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k], ActorID);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);
              end
            else
              begin
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireVVpu[k] + QDesireDRCpu[k], ActorID);
                QDesireEndpu[k] := Min(abs(QDesireVVpu[k] + QDesireDRCpu[k]), abs(QDesireLimitedpu[k])) * sign(QDesireVVpu[k] + QDesireDRCpu[k]);
              end;

            // Calculates QDesiredVVDRC[k]
            CalcVVDRC_vars(k,ActorID);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kvar_out
            if ControlledElement[k].DSSClassName = 'PVSystem2' then TPVSystem2Obj(DERelem).Presentkvar := QDesiredVVDRC[k]
            else TStorage2Obj(DERelem).kvarRequested := QDesiredVVDRC[k];

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                TPVSystem2Obj(DERelem).SetNominalPVSystem2Ouput(ActorID);

                if QDesiredVVDRC[k] >= 0.0 then Qoutputpu[k] := TPVSystem2Obj(DERelem).Presentkvar / QHeadroom[k]
                else Qoutputpu[k] := TPVSystem2Obj(DERelem).Presentkvar / QHeadroomNeg[k];
              end
            else
              begin
                TStorage2Obj(DERelem).SetNominalStorage2Output(ActorID);

                if QDesiredVVDRC[k] >= 0.0 then Qoutputpu[k] := TStorage2Obj(DERelem).Presentkvar / QHeadroom[k]
                else Qoutputpu[k] := TStorage2Obj(DERelem).Presentkvar / QHeadroomNeg[k];
              end;

            // Values used in convergence
            QoutputVVDRCpu[k] := Qoutputpu[k];
            FAvgpVpuPrior[k] := FPresentVpu[k];
            FAvgpDRCVpuPrior[k] := FPresentDRCVpu[k];

            // Values used in CalcQVVcurve_desiredpu and CalcVVDRC_vars
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                QOld[k]      := TPVSystem2Obj(DERelem).Presentkvar;
                QOldVVDRC[k] := TPVSystem2Obj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name +', '+ TPVSystem2Obj(DERelem).QualifiedName,
                                                    Format('**VV_DRC mode requested PVSystem2 output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredVVDRC[k], TPVSystem2Obj(DERelem).Presentkvar]),ActorID);
              end
            else
              begin
                QOld[k]      := TStorage2Obj(DERelem).Presentkvar;
                QOldVVDRC[k] := TStorage2Obj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name +', '+  TStorage2Obj(DERelem).QualifiedName,
                                                    Format('**VV_DRC mode requested Storage2 output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredVVDRC[k], TStorage2Obj(DERelem).Presentkvar]),ActorID);
              end;
          end

        {Smart Inverter volt-watt function}
        else if(ControlMode = VOLTWATT) and (CombiControlMode = NONE_COMBMODE) and (PendingChange[k]=CHANGEWATTLEVEL) then
          begin

            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                TPVSystem2Obj(DERelem).VWmode            := TRUE;
              end
            else
              begin
                TStorage2Obj(DERelem).VWmode            := TRUE;
              end;

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QVWcurve_limitpu[k]
            CalcPVWcurve_limitpu(k, ActorID);

            // LPF or RF activated
            if (RateofChangeMode = LPF) then
              begin
                CalcLPF(k, 'WATTS', PLimitVWpu[k], ActorID);
                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitOptionpu[k], ActorID);
                PLimitEndpu[k] := Min(PLimitLimitedpu[k], PLimitOptionpu[k]);
              end
            else if (RateofChangeMode = RISEFALL) then
              begin
                CalcRF(k, 'WATTS', PLimitVWpu[k], ActorID);
                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitOptionpu[k], ActorID);
                PLimitEndpu[k] := Min(PLimitLimitedpu[k], PLimitOptionpu[k]);
              end
            else
              begin
                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitVWpu[k], ActorID);
                PLimitEndpu[k] := Min(abs(PLimitLimitedpu[k]), abs(PLimitVWpu[k])) * sign(PLimitVWpu[k]);
              end;

            // Calculates PLimitVW[k] through the convergence algorithm
            CalcVoltWatt_watts(k, ActorID);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kW_out
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                TPVSystem2Obj(DERelem).PresentkW := PLimitVW[k];

                // Uptates PresentkW and Presentkvar considering watt and var priorities
                TPVSystem2Obj(DERelem).SetNominalPVSystem2Ouput(ActorID);

              end
            else
              begin
                TStorage2Obj(DERelem).kWRequested := PLimitVW[k];

                // Uptates PresentkW and Presentkvar considering watt and var priorities
                TStorage2Obj(DERelem).SetNominalStorage2Output(ActorID);
              end;


            // Values used in convergence
            FAvgpVpuPrior[k] := FPresentVpu[k];
            POldVWpu[k] := PLimitVW[k] / PBase[k];

            // Flag has to do set to 0 when kW_out is lower than Ptemp (max power allowed from volt-watt function)
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                if ((abs(PLimitVW[k]) > 0.0) and (abs(TPVSystem2Obj(DERelem).presentkW - PLimitVW[k]) / PLimitVW[k] > 0.0001)) then FVWOperation[k] := 0; // 0.01% is the value chosen at the moment

                if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name+', '+TPVSystem2Obj(DERelem).QualifiedName,
                                                    Format('**VOLTWATT mode set PVSystem2 kw output limit to **, kw= %.5g. Actual output is kw= %.5g.',
                                                           [PLimitVW[k], TPVSystem2Obj(DERelem).presentkW]),ActorID);
              end
            else
              begin
                if abs(abs(TStorage2Obj(DERelem).presentkW) - PLimitVW[k]) / PLimitVW[k] > 0.0001 then FVWOperation[k] := 0; // 0.01% is the value chosen at the moment

                if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name+', '+  TStorage2Obj(DERelem).QualifiedName,
                                                    Format('**VOLTWATT mode set Storage2 kw output limit to ** kw= %.5g. Actual output is kw= %.5g.',
                                                           [PLimitVW[k], TStorage2Obj(DERelem).presentkW]),ActorID);

              end;
          end

        else if(ControlMode = NONE_MODE) and (CombiControlMode = VV_VW) and (PendingChange[k]=CHANGEWATTVARLEVEL) then
          begin

            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                TPVSystem2Obj(DERelem).VWmode  := TRUE;
                TPVSystem2Obj(DERelem).Varmode := VARMODEKVAR;
                TPVSystem2Obj(DERelem).VVmode  := TRUE;
              end
            else
              begin
                TStorage2Obj(DERelem).VWmode  := TRUE;
                TStorage2Obj(DERelem).Varmode := VARMODEKVAR;
                TStorage2Obj(DERelem).VVmode  := TRUE;
              end;

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireVVpu[k] and QVWcurve_limitpu[k]
            CalcPVWcurve_limitpu(k, ActorID);
            CalcQVVcurve_desiredpu(k, ActorID);

            // LPF or RF activated
            if (RateofChangeMode = LPF) then
              begin
                CalcLPF(k, 'VARS', QDesireVVpu[k], ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k], ActorID);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);

                CalcLPF(k, 'WATTS', PLimitVWpu[k], ActorID);
                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitOptionpu[k], ActorID);
                PLimitEndpu[k] := Min(PLimitLimitedpu[k], PLimitOptionpu[k]);
              end
            else if (RateofChangeMode = RISEFALL) then
              begin
                CalcRF(k, 'VARS', QDesireVVpu[k], ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireOptionpu[k], ActorID);
                QDesireEndpu[k] := Min(abs(QDesireLimitedpu[k]), abs(QDesireOptionpu[k])) * sign(QDesireOptionpu[k]);

                CalcRF(k, 'WATTS', PLimitVWpu[k], ActorID);
                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitOptionpu[k], ActorID);
                PLimitEndpu[k] := Min(PLimitLimitedpu[k], PLimitOptionpu[k]);
              end
            else
              begin
                // Checks kVA (watt priority) and kvarlimit limits
                Check_Qlimits(k, QDesireVVpu[k], ActorID);
                QDesireEndpu[k] := Min(abs(QDesireVVpu[k]), abs(QDesireLimitedpu[k])) * sign(QDesireVVpu[k]);

                // Checks kVA (var priority) and pctPmpp limits
                Check_Plimits(k, PLimitVWpu[k], ActorID);
                PLimitEndpu[k] := Min(abs(PLimitLimitedpu[k]), abs(PLimitVWpu[k])) * sign(PLimitVWpu[k]);
              end;

            // Calculates PLimitVW[k] and QDesiredVV[k] through the convergence algorithm
            CalcVoltWatt_watts(k, ActorID);
            CalcVoltVar_vars(k, ActorID);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kvar_out and kW_out
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                TPVSystem2Obj(DERelem).Presentkvar := QDesiredVV[k];
                TPVSystem2Obj(DERelem).presentkW   := PLimitVW[k];
              end
            else
              begin
                TStorage2Obj(DERelem).kvarRequested := QDesiredVV[k];
                TStorage2Obj(DERelem).kWRequested   := PLimitVW[k];
              end;

            // Uptates PresentkW and Presentkvar considering watt and var priorities
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                TPVSystem2Obj(DERelem).SetNominalPVSystem2Ouput(ActorID);

                if QDesiredVV[k] >= 0.0 then Qoutputpu[k] := TPVSystem2Obj(DERelem).Presentkvar / QHeadroom[k]
                else Qoutputpu[k] := TPVSystem2Obj(DERelem).Presentkvar / QHeadroomNeg[k];
              end
            else
              begin
                TStorage2Obj(DERelem).SetNominalStorage2Output(ActorID);

                if QDesiredVV[k] >= 0.0 then Qoutputpu[k] := TStorage2Obj(DERelem).Presentkvar / QHeadroom[k]
                else Qoutputpu[k] := TStorage2Obj(DERelem).Presentkvar / QHeadroomNeg[k];
              end;

            // Values used in convergence
            QoutputVVpu[k] := Qoutputpu[k];
            FAvgpVpuPrior[k] := FPresentVpu[k];
            POldVWpu[k] := PLimitVW[k] / PBase[k];

            // Values used in CalcQVVcurve_desiredpu
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                QOld[k]   := TPVSystem2Obj(DERelem).Presentkvar;
                QOldVV[k] := TPVSystem2Obj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name +', '+ TPVSystem2Obj(DERelem).QualifiedName,
                                                    Format('**VV_VW mode requested PVSystem2 output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredVV[k], TPVSystem2Obj(DERelem).presentkvar]), ActorID);
              end
            else
              begin
                QOld[k]   := TStorage2Obj(DERelem).Presentkvar;
                QOldVV[k] := TStorage2Obj(DERelem).Presentkvar;

                if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name +', '+ TStorage2Obj(DERelem).QualifiedName,
                                                    Format('**VV_VW mode requested Storage2 output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.',
                                                           [QDesiredVV[k], TStorage2Obj(DERelem).presentkvar]), ActorID);
              end;

            // Flag has to do set to 0 when kW_out is lower than Ptemp (max power allowed from volt-watt function)
            if ControlledElement[k].DSSClassName = 'PVSystem2' then
              begin
                if abs(TPVSystem2Obj(DERelem).presentkW - PLimitVW[k]) / PLimitVW[k] > 0.0001 then FVWOperation[k] := 0; // 0.01% is the value chosen at the moment

                if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name+', '+TPVSystem2Obj(DERelem).QualifiedName,
                                                    Format('**VV_VW mode set PVSystem2 kw output limit to **, kw= %.5g. Actual output is kw= %.5g.',
                                                           [PLimitVW[k], TPVSystem2Obj(DERelem).presentkW]),ActorID);
              end
            else
              begin
                if abs(abs(TStorage2Obj(DERelem).presentkW) - PLimitVW[k]) / PLimitVW[k] > 0.0001 then FVWOperation[k] := 0; // 0.01% is the value chosen at the moment

                if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name+', '+  TStorage2Obj(DERelem).QualifiedName,
                                    Format('**VV_VW mode set Storage2 kw output limit to** kw= %.5g. Actual output is kw= %.5g.',
                                           [PLimitVW[k], TStorage2Obj(DERelem).presentkW]),ActorID);
              end;

          end;

        ActiveCircuit[ActorID].Solution.LoadsNeedUpdating := TRUE;
        Set_PendingChange(NONE,k);
        DERelem := Nil;
    end;

end;

procedure TInvControl2Obj.GetmonVoltage(ActorID : Integer; var Vpresent: Double; i: Integer; BasekV: Double);
  Var
    j           : integer;
    rBus        : TDSSBus;
    numNodes    : Integer;
    v           : Complex;
    vi          : Complex;
    vj          : Complex;

  begin

    if FUsingMonBuses then
      begin

        for j := 0 to Length(FMonBuses)-1 do
          begin
            FMonBusesIndex := ActiveCircuit[ActorID].BusList.Find(FMonBuses[j]);
            rBus := ActiveCircuit[ActorID].Buses^[FMonBusesIndex];

            if (length(FMonBusesNodes[j]) = 2) then
              begin
              vi := (ActiveCircuit[ActorID].Solution.NodeV^[rBus.GetRef(FMonBusesNodes[j][0])]);
              vj := (ActiveCircuit[ActorID].Solution.NodeV^[rBus.GetRef(FMonBusesNodes[j][1])]);
              cBuffer[i,j] := cmulreal(Csub(vi, vj), BasekV * 1000.0 / FMonBusesVbase[j+1]);
              v := cBuffer[i,j];
              end
            else
              begin
                cBuffer[i,j] := cmulreal(ActiveCircuit[ActorID].Solution.NodeV^[rBus.GetRef(FMonBusesNodes[j][0])], BasekV * 1000.0 / FMonBusesVbase[j+1]);
                v := cBuffer[i,j];
              end;
          end;

          CASE FMonBusesPhase of
            AVGPHASES:
              begin
                Vpresent := 0.0;
                for j := 0 to Length(FMonBuses)-1 do
                  Vpresent := Vpresent + Cabs(cBuffer[i,j]);
                Vpresent := Vpresent / Length(FMonBuses);
              end;
            MAXPHASE:
              begin
                Vpresent := 0.0;
                for j := 0 to Length(FMonBuses)-1 do
                  Vpresent := Max(Vpresent, Cabs(cBuffer[i,j]));
              end;
            MINPHASE:
              begin
                Vpresent := 1.0E50;
                for j := 0 to Length(FMonBuses)-1 do
                  Vpresent := Min(Vpresent, Cabs(cBuffer[i,j]));
              end;
            else
              Vpresent := Cabs(cBuffer[i, FMonBusesPhase]);
          end;
      end

    else
      begin
        ControlledElement[i].ComputeVTerminal(ActorID);

        numNodes := ControlledElement[i].NPhases;

        for j := 1 to numNodes do
          cBuffer[i,j] := ControlledElement[i].Vterminal^[j];


        CASE FMonBusesPhase of
          AVGPHASES:
            begin
              Vpresent := 0.0;
              for j := 1 to numNodes do
                Vpresent := Vpresent + Cabs(cBuffer[i,j]);
              Vpresent := Vpresent / numNodes;
            end;
          MAXPHASE:
            begin
              Vpresent := 0.0;
              for j := 1 to numNodes do
                Vpresent := Max(Vpresent, Cabs(cBuffer[i,j]));
            end;
          MINPHASE:
            begin
              Vpresent := 1.0E50;
              for j := 1 to numNodes do
                Vpresent := Min(Vpresent, Cabs(cBuffer[i,j]));
            end;
          else
            Vpresent := Cabs(cBuffer[i, FMonBusesPhase]);
        end;
      end;

  end;

procedure TInvControl2Obj.UpdateDERParameters(i: Integer);
  begin

    with ControlledElement[i] do
      if ControlledElement[i].DSSClassName = 'PVSystem2' then
        begin
          with TPVSystem2Obj(ControlledElement[i]) do
            begin
              CondOffset[i]   := (NTerms-1) * NCondsDER[i]; // for speedy sampling

              FVBase[i]               :=  Vbase;
              FVarFollowInverter[i]   :=  VarFollowInverter;
              FInverterON[i]          :=  InverterON;
              FpresentkW[i]           :=  PresentkW;
              FkVARating[i]           :=  kVARating;
              Fpresentkvar[i]         :=  Presentkvar;
              FkvarLimit[i]           :=  kvarLimit;
              FkvarLimitNeg[i]        :=  kvarLimitNeg;
              FCurrentkvarLimit[i]    :=  CurrentkvarLimit;
              FCurrentkvarLimitNeg[i] :=  CurrentkvarLimitNeg;
              FDCkWRated[i]           :=  Pmpp;
              FpctDCkWRated[i]        :=  puPmpp;
              FEffFactor[i]           :=  PVSystem2Vars.EffFactor;
              FDCkW[i]                :=  PVSystem2Vars.PanelkW;
              FPPriority[i]           :=  PVSystem2Vars.P_Priority;

            end;
        end
      else if ControlledElement[i].DSSClassName = 'Storage2' then
        begin
          with TStorage2Obj(ControlledElement[i]) do
            begin
              FVBase[i]               :=  Vbase;
              FVarFollowInverter[i]   :=  VarFollowInverter;
              FInverterON[i]          :=  InverterON;
              FpresentkW[i]           :=  PresentkW;
              FkVARating[i]           :=  kVARating;
              Fpresentkvar[i]         :=  Presentkvar;
              FkvarLimit[i]           :=  kvarLimit;
              FkvarLimitNeg[i]        :=  kvarLimitNeg;
              FCurrentkvarLimit[i]    :=  CurrentkvarLimit;
              FCurrentkvarLimitNeg[i] :=  CurrentkvarLimitNeg;
              FDCkWRated[i]           :=  Storage2Vars.kWrating;
              FpctDCkWRated[i]        :=  pctkWrated;
              FEffFactor[i]           :=  Storage2vars.EffFactor;
              FDCkW[i]                :=  0.0; // not using it (using TStorage2Obj.DCkW directly)
              FPPriority[i]           :=  Storage2Vars.P_priority;

            end
        end;
  end;

procedure TInvControl2Obj.Sample(ActorID : Integer);

  VAR
    i                           :Integer;
    basekV                      :Double;
    Vpresent                    :Double;
    PVSys                       :TPVSystem2Obj;
    Storage2                    :TStorage2Obj;

  begin
    // if list is not defined, go make one from all PVSystem2/Storage2 in circuit
     if FDERPointerList.ListSize=0 then   RecalcElementData(ActorID);

     if (FListSize>0) then
      begin
        // if an InvControl2 controls more than one PVSystem2/Storage2, control each one
        // separately based on the PVSystem2/Storage2's terminal voltages, etc.
        for i := 1 to FDERPointerList.ListSize do
          begin
            UpdateDERParameters(i);

            if ControlledElement[i].DSSClassName = 'PVSystem2' then PVSys := ControlledElement[i] as TPVSystem2Obj
            else Storage2 := ControlledElement[i] as TStorage2Obj;

            BasekV := FVBase[i] / 1000.0; // It's a line-to-ground voltage

            GetmonVoltage(ActorID, Vpresent, i, BasekV);

            // for reporting Vpriorpu correctly in EventLog (this update is normally perform at DoPendingAction)
            if ActiveCircuit[ActorID].Solution.ControlIteration = 1 then
            begin
              FAvgpVpuPrior[i] := FPresentVpu[i];
              FAvgpDRCVpuPrior[i] := FPresentDRCVpu[i];
            end;

            kW_out_desired[i] := FpresentkW[i]; // necessary to update kW_out_desired at every control iteration for Storage2 with SC

            // Help says that it must be used just for vv and vw
            // convert to per-unit on bus' kvbase, or
            // if using averaging window values, then set prior voltage to averaging window
            if(FVoltage_CurveX_ref = 1) and (FRollAvgWindow[i].Get_AvgVal <> 0.0) then
              FPresentVpu[i] := Vpresent / (FRollAvgWindow[i].Get_AvgVal)
            else if(FVoltage_CurveX_ref = 2) and (FRollAvgWindow[i].Get_AvgVal <> 0.0) then
              FPresentVpu[i] := (FRollAvgWindow[i].Get_AvgVal) / (basekV * 1000.0)
            else FPresentVpu[i] := Vpresent / (BasekV * 1000.0);

            FPresentDRCVpu[i] := Vpresent / (BasekV * 1000.0);

            // Sets internal variables of controlled element.
            // FVreg is the pu voltage used in the volt-var and volt-watt curves
            FVreg := FPresentVpu[i];

            if CombiControlMode = VV_DRC then
              begin
                  // Sets internal variables of controlled element.
                  // FVVDRCOperation is a flag which indicates if VVDRC function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
                  if ControlledElement[i].DSSClassName = 'PVSystem2' then
                    begin
                      PVSys.Set_Variable(5,FVreg);
                      PVSys.Set_Variable(6,FDRCRollAvgWindow[i].Get_AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                      PVSys.Set_Variable(10,FVVDRCOperation[i]);
                    end
                  else
                    begin
                      Storage2.Set_Variable(14,FVreg);
                      Storage2.Set_Variable(15,FDRCRollAvgWindow[i].Get_AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                      Storage2.Set_Variable(19,FVVDRCOperation[i]);
                    end;

                  // if inverter is off then exit
                  if (FInverterON[i] = FALSE) and (FVarFollowInverter[i] = TRUE) then continue;

                  // if the volt-var curve does not exist, exit
                  if Length(Fvvc_curvename) = 0 then
                    begin
                      DoSimpleMsg('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl2.', 382);
                      exit
                    end;

                  if (ControlledElement[i].DSSClassName = 'PVSystem2') then
                  Begin
                      PVSys.VVmode   := TRUE;
                      PVSys.DRCmode  := TRUE;
                  End
                  else
                  Begin
                      Storage2.VVmode   := TRUE;
                      Storage2.DRCmode  := TRUE;
                  End;

                  //DRC triggers
                  if(priorDRCRollAvgWindow[i] = 0.0) then
                    begin

                      if (Abs(FPresentDRCVpu[i] - FAvgpDRCVpuPrior[i]) > FVoltageChangeTolerance) or
                         (Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance)  then
                        begin
                          // Resets DER state variable only if it has not converged yet
                          FVVDRCOperation[i] := 0.0;

                          Set_PendingChange(CHANGEDRCVVARLEVEL,i);

                          with ActiveCircuit[ActorID].Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push
                              (intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);

                          if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name+', '+ControlledElement[i].QualifiedName,
                                                                Format('**Ready to change var output due to DRC trigger in VV_DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                       [FPresentDRCVpu[i],FAvgpDRCVpuPrior[i]]),ActorID);
                        end;

                    end;

                    //Trigger from volt-var mode
                  if (((Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) or
                      (Abs(FPresentDRCVpu[i] - FAvgpDRCVpuPrior[i]) > FVoltageChangeTolerance) or
                        ((Abs(Abs(QoutputVVDRCpu[i]) - Abs(QDesireEndpu[i])) > FVarChangeTolerance))) or
                        (ActiveCircuit[ActorID].Solution.ControlIteration = 1)) then
                    begin
                      // Resets DER state variable only if it has not converged yet
                      FVVDRCOperation[i] := 0.0;

                      Set_PendingChange(CHANGEDRCVVARLEVEL,i);
                      with  ActiveCircuit[ActorID].Solution.DynaVars do
                        ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push
                        (intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);

                      if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name+', '+ControlledElement[i].QualifiedName,
                                                            Format('**Ready to change VV_DRC output due to volt-var trigger in VV_DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                   [FPresentVpu[i],FAvgpVpuPrior[i]]),ActorID);

                    end;
              end

            else if CombiControlMode = VV_VW then
              begin
                // Sets internal variables of controlled element.
                // FVVOperation is a flag which indicates if volt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
                // FVWOperation is a flag which indicates if volt-watt function operates or not
                // Combined modes operation is shown through TWO flags. It allows us to verify which of the individual function operates or not

                if ControlledElement[i].DSSClassName = 'PVSystem2' then
                  begin
                    PVSys.Set_Variable(5,FVreg);
                    PVSys.Set_Variable(7,FVVOperation[i]);
                    PVSys.Set_Variable(8,FVWOperation[i]);
                  end
                else
                  begin
                    Storage2.Set_Variable(14,FVreg);
                    Storage2.Set_Variable(16,FVVOperation[i]);
                    Storage2.Set_Variable(17,FVWOperation[i]);
                  end;

                // if inverter is off then exit
                if (FInverterON[i] = FALSE) and (FVarFollowInverter[i] = TRUE) then continue;

                // if volt-watt curve does not exist, exit
                if ControlledElement[i].DSSClassName = 'PVSystem2' then
                begin
                   if Length(Fvoltwatt_curvename) = 0 then
                  begin
                    DoSimpleMsg('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl2.', 381);
                      exit
                  end;
                end
                else
                begin
                   if (Length(Fvoltwatt_curvename) = 0) and (Length(FvoltwattCH_curvename) = 0) then
                  begin
                    DoSimpleMsg('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl2.', 381);
                      exit
                  end;
                end;

                // if the volt-var curve does not exist, exit
                if Length(Fvvc_curvename) = 0 then
                  begin
                    DoSimpleMsg('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl2.', 382);
                      exit
                  end;

                if (ControlledElement[i].DSSClassName = 'PVSystem2') then
                Begin
                  PVSys.VVmode     := TRUE;
                  PVSys.VWmode     := TRUE
                End
                else
                Begin
                  Storage2.VVmode   := TRUE;
                  Storage2.VWmode   := TRUE;
                End;

                // Trigger from volt-watt mode
                if ((Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) or (Abs(PLimitEndpu[i]-POldVWpu[i])>FActivePChangeTolerance) or
                  (ActiveCircuit[ActorID].Solution.ControlIteration = 1)) then

                  begin

                    // Resets DER state variable only if it has not converged yet
                    FVWOperation[i] := 0;

                    Set_PendingChange(CHANGEWATTVARLEVEL,i);

                    with  ActiveCircuit[ActorID].Solution.DynaVars do
                      ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push
                      (intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);

                    if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name+', '+ControlledElement[i].QualifiedName,
                                                          Format('**Ready to change VV_VW output due to volt-watt trigger**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                 [FPresentVpu[i],FAvgpVpuPrior[i]]),ActorID);;
                  end;

                  //Trigger from volt-var mode
                if (((Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) or
                  ((Abs(Abs(Qoutputpu[i]) - Abs(QDesireEndpu[i])) > FVarChangeTolerance))) or
                  (ActiveCircuit[ActorID].Solution.ControlIteration = 1)) then

                  begin

                    // Resets DER state variable only if it has not converged yet
                    FVVOperation[i] := 0;
                    Set_PendingChange(CHANGEWATTVARLEVEL,i);
                    with  ActiveCircuit[ActorID].Solution.DynaVars do
                      ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push
                      (intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);

                    if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name+', '+ControlledElement[i].QualifiedName,
                                                          Format('**Ready to change VV_VW output due to volt-var trigger**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                 [FPresentVpu[i],FAvgpVpuPrior[i]]),ActorID);
                  end;
              end

            else if ControlMode = VOLTWATT then  // volt-watt control mode
              begin
                // Sets internal variables of controlled element.
                // FVWOperation is a flag which indicates if volt-watt function operates or not

                if ControlledElement[i].DSSClassName = 'PVSystem2' then
                  begin
                    PVSys.Set_Variable(5,FVreg);
                    PVSys.Set_Variable(8,FVWOperation[i]);
                  end
                else
                  begin
                    Storage2.Set_Variable(14,FVreg);
                    Storage2.Set_Variable(17,FVWOperation[i]);
                  end;

                if (FInverterON[i] = FALSE) then continue;

                if ControlledElement[i].DSSClassName = 'PVSystem2' then
                  begin
                    if Length(Fvoltwatt_curvename) = 0 then
                      begin
                        DoSimpleMsg('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl2.', 381);
                        exit
                      end;
                  end
                else
                  begin
                      if (Length(Fvoltwatt_curvename) = 0) and (Length(FvoltwattCH_curvename) = 0) then
                        begin
                          DoSimpleMsg('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl2.', 381);
                          exit
                        end;

                  end;

                if (ControlledElement[i].DSSClassName = 'PVSystem2') then PVSys.VWmode  := TRUE
                else Storage2.VWmode  := TRUE;

                if ((Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) or (Abs(PLimitEndpu[i]-POldVWpu[i])>FActivePChangeTolerance) or
                  (ActiveCircuit[ActorID].Solution.ControlIteration = 1))  then
                  begin

                    // Resets DER state variable only if it has not converged yet
                    FVWOperation[i] := 0;

                    Set_PendingChange(CHANGEWATTLEVEL,i);

                    with  ActiveCircuit[ActorID].Solution.DynaVars do
                      ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push
                      (intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);
                    if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name+', '+ControlledElement[i].QualifiedName,
                                                          Format('**Ready to limit watt output due to VOLTWATT mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                 [FPresentVpu[i],FAvgpVpuPrior[i]]),ActorID);
                  end;
              end

            else if ControlMode = VOLTVAR then // volt-var control mode
              begin
                // Sets internal variables of PVSystem2/Storage2.
                // FVVOperation is a flag which indicates if volt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)

                if ControlledElement[i].DSSClassName = 'PVSystem2' then
                  begin
                    PVSys.Set_Variable(5,FVreg);
                    PVSys.Set_Variable(7,FVVOperation[i]);
                  end
                else
                  begin
                    Storage2.Set_Variable(14,FVreg);
                    Storage2.Set_Variable(16,FVVOperation[i]);
                  end;

                // if inverter is off then exit
                if (FInverterON[i] = FALSE) and (FVarFollowInverter[i] = TRUE) then continue;

                  if Length(Fvvc_curvename) = 0 then
                    begin
                      DoSimpleMsg('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl2.', 382);
                      exit
                    end;

                if (ControlledElement[i].DSSClassName = 'PVSystem2') then PVSys.VVmode  := TRUE
                else Storage2.VVmode  := TRUE;

                  //Trigger from volt-var mode
                if (((Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) or
                    ((Abs(Abs(QoutputVVpu[i]) - Abs(QDesireEndpu[i])) > FVarChangeTolerance))) or
                    (ActiveCircuit[ActorID].Solution.ControlIteration = 1)) then

                  begin

                    // Resets DER state variable only if it has not converged yet
                    FVVOperation[i] := 0;

                    Set_PendingChange(CHANGEVARLEVEL,i);

                    with  ActiveCircuit[ActorID].Solution.DynaVars do
                      ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push(intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);

                    if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name+', '+ControlledElement[i].QualifiedName,
                                                          Format('**Ready to change var output due to volt-var trigger in volt-var mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                 [FPresentVpu[i],FAvgpVpuPrior[i]]),ActorID);
                  end;
              end

            else if ControlMode = WATTPF then // watt-pf control mode
              begin
                // Sets internal variables of PVSystem2/Storage2.
                // FWPOperation is a flag which indicates if watt-pf function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)

                if ControlledElement[i].DSSClassName = 'PVSystem2' then
                  begin
                    PVSys.Set_Variable(5,FVreg);
                    PVSys.Set_Variable(11,FWPOperation[i]);
                  end
                else
                  begin
                    Storage2.Set_Variable(14,FVreg);
                    Storage2.Set_Variable(16,FWPOperation[i]);
                  end;

                // if inverter is off then exit
                if (FInverterON[i] = FALSE) and (FVarFollowInverter[i] = TRUE) then continue;

                  if Length(Fwattpf_curvename) = 0 then
                    begin
                      DoSimpleMsg('XY Curve object representing wattpf_curve does not exist or is not tied to InvControl2.', 382);
                      exit
                    end;

                if (ControlledElement[i].DSSClassName = 'PVSystem2') then PVSys.WPmode  := TRUE
                else Storage2.WPmode  := TRUE;

                  //Trigger from volt-var mode
                if (((Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) or
                    ((Abs(Abs(QoutputVVpu[i]) - Abs(QDesireEndpu[i])) > FVarChangeTolerance))) or
                    (ActiveCircuit[ActorID].Solution.ControlIteration = 1)) then

                  begin

                    // Resets DER state variable only if it has not converged yet
                    FWPOperation[i] := 0;

                    Set_PendingChange(CHANGEVARLEVEL,i);

                    with  ActiveCircuit[ActorID].Solution.DynaVars do
                      ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push(intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);

                    if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name+', '+ControlledElement[i].QualifiedName,
                                                          Format('**Ready to change var output due to watt-pf trigger in watt-pf mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                 [FPresentVpu[i],FAvgpVpuPrior[i]]),ActorID);
                  end;
              end

            else if ControlMode = WATTVAR then // watt-var control mode
              begin
                // Sets internal variables of PVSystem2/Storage2.
                // FWVOperation is a flag which indicates if watt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)

                if ControlledElement[i].DSSClassName = 'PVSystem2' then
                  begin
                    PVSys.Set_Variable(5,FVreg);
                    PVSys.Set_Variable(12,FWVOperation[i]);        //CHANGE HERE
                  end
                else
                  begin
                    Storage2.Set_Variable(14,FVreg);
                    Storage2.Set_Variable(16,FWVOperation[i]);
                  end;

                // if inverter is off then exit
                if (FInverterON[i] = FALSE) and (FVarFollowInverter[i] = TRUE) then continue;

                  if Length(Fwattvar_curvename) = 0 then
                    begin
                      DoSimpleMsg('XY Curve object representing wattvar_curve does not exist or is not tied to InvControl2.', 382);
                      exit
                    end;

                if (ControlledElement[i].DSSClassName = 'PVSystem2') then PVSys.WVmode := TRUE
                else Storage2.WVmode  := TRUE;

                  //Trigger from volt-var mode
                if (((Abs(FPresentVpu[i] - FAvgpVpuPrior[i]) > FVoltageChangeTolerance) or
                    ((Abs(Abs(QoutputVVpu[i]) - Abs(QDesireEndpu[i])) > FVarChangeTolerance))) or
                    (ActiveCircuit[ActorID].Solution.ControlIteration = 1)) then

                  begin

                    // Resets DER state variable only if it has not converged yet
                    FWVOperation[i] := 0;

                    Set_PendingChange(CHANGEVARLEVEL,i);

                    with  ActiveCircuit[ActorID].Solution.DynaVars do
                      ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push(intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);

                    if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name+', '+ControlledElement[i].QualifiedName,
                                                          Format('**Ready to change var output due to watt-var trigger in watt-var mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                 [FPresentVpu[i],FAvgpVpuPrior[i]]),ActorID);
                  end;
              end

            else if ControlMode = DRC then // dynamic reactive current control mode
              begin
                // Sets internal variables of PVSystem2/Storage2.
                // FDRCOperation is a flag which indicates if DRC function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)

                if ControlledElement[i].DSSClassName = 'PVSystem2' then
                begin
                  PVSys.Set_Variable(5,FVreg);
                  PVSys.Set_Variable(6,FDRCRollAvgWindow[i].Get_AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                  PVSys.Set_Variable(9,FDRCOperation[i]);
                end
                else
                begin
                  Storage2.Set_Variable(14,FVreg);
                  Storage2.Set_Variable(15,FDRCRollAvgWindow[i].Get_AvgVal / (basekV * 1000.0)); // save rolling average voltage in monitor
                  Storage2.Set_Variable(18,FDRCOperation[i]);
                end;

                // if inverter is off then exit
                if (FInverterON[i] = FALSE) and (FVarFollowInverter[i] = TRUE) then continue;

                //DRC triggers
                if(priorDRCRollAvgWindow[i] = 0.0) then
                  begin

                    if ((Abs(FPresentDRCVpu[i] - FAvgpDRCVpuPrior[i]) > FVoltageChangeTolerance))  then
                      begin

                        // Resets DER state variable only if it has not converged yet
                        FDRCOperation[i] := 0;


                        Set_PendingChange(CHANGEVARLEVEL,i);

                        with ActiveCircuit[ActorID].Solution.DynaVars do
                          ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push
                          (intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);

                        if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name+', '+ControlledElement[i].QualifiedName,
                                                              Format('**Ready to change var output due to DRC trigger in DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g',
                                                                     [FPresentDRCVpu[i],FAvgpDRCVpuPrior[i]]),ActorID);
                      end;
                  end;

                if (ControlledElement[i].DSSClassName = 'PVSystem2') then PVSys.DRCmode  := TRUE
                else Storage2.DRCmode  := TRUE;

                if ((Abs(FPresentDRCVpu[i] - FAvgpDRCVpuPrior[i]) > FVoltageChangeTolerance) or
                  (Abs(Abs(QoutputDRCpu[i]) - Abs(QDesireEndpu[i])) > FVarChangeTolerance) or // TEMc; also tried checking against QDesireEndpu
                  (ActiveCircuit[ActorID].Solution.ControlIteration = 1)) then
                    begin

                      Set_PendingChange(CHANGEVARLEVEL,i);
                      with  ActiveCircuit[ActorID].Solution.DynaVars do
                        ControlActionHandle := ActiveCircuit[ActorID].ControlQueue.Push
                        (intHour, t + TimeDelay, PendingChange[i], 0, Self, ActorID);

                      if ShowEventLog then AppendtoEventLog('InvControl2.' + Self.Name+', '+ControlledElement[i].QualifiedName,
                                                            Format('**Ready to change var output due to DRC trigger in DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g, QoutPU=%.3g, QDesiredEndpu=%.3g',
                                                                   [FPresentDRCVpu[i],FAvgpDRCVpuPrior[i],QoutputDRCpu[i],QDesireEndpu[i]]),ActorID);

                    end;
              end;
         end;
      end;

  end;

procedure TInvControl2Obj.InitPropertyValues(ArrayOffset: Integer);
  begin
    PropertyValue[1]  := ''; //PVSystem2/Storage2 list
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
    PropertyValue[22] := 'yes'; // show event log
    PropertyValue[23] := 'VARAVAL'; // y-axis reference (and power precedence) for volt-var
    PropertyValue[24] := '0.01';

    PropertyValue[28] := 'NONE'; // voltwattCH_curve

    inherited  InitPropertyValues(NumPropsThisClass);

  end;

function TInvControl2Obj.MakeDERList:Boolean;

  VAR
     PVSysClass    : TDSSClass;
     Storage2Class : TDSSClass;
     PVSys         : TPVsystem2Obj;
     Storage2      : TStorage2Obj;
     DERElem       : TPCElement;
     i,j           : Integer;

  begin

    Result := FALSE;
    PVSysClass := GetDSSClassPtr('PVsystem2');
    Storage2Class := GetDSSClassPtr('Storage2');

    if FListSize > 0 then
      begin    // Name list is defined - Use it

        SetLength(CondOffset,FListSize+1);
        SetLength(cBuffer,FListSize+1,7);  // assuming no more than 6 conductors
        SetLength(ControlledElement,FListSize+1);  // Use this as the main pointer to PVSystem2 and Storage2 Elements
        SetLength(FAvgpVpuPrior, FListSize+1);
        SetLength(FAvgpDRCVpuPrior, FListSize+1);
        SetLength(FPresentVpu, FListSize+1);
        SetLength(FPresentDRCVpu, FListSize+1);
        SetLength(NPhasesDER,FListSize+1);
        SetLength(NCondsDER,FListSize+1);
        SetLength(FPendingChange,FListSize+1);
        SetLength(QDesiredVV,FListSize+1);
        SetLength(QDesiredWP,FListSize+1);
        SetLength(QDesiredWV,FListSize+1);
        SetLength(QOld,FListSize+1);
        SetLength(QOldVV,FListSize+1);
        SetLength(QOldDRC,FListSize+1);
        SetLength(QOldVVDRC,FListSize+1);
        SetLength(QDesiredDRC,FListSize+1);
        SetLength(QDesiredVVDRC,FListSize+1);
        SetLength(QHeadroom,FListSize+1);
        SetLength(QHeadroomNeg,FListSize+1);
        SetLength(PBase,FListSize+1);
        SetLength(Qoutputpu,FListSize+1);
        SetLength(QoutputVVpu,FListSize+1);
        SetLength(QoutputDRCpu,FListSize+1);
        SetLength(QoutputVVDRCpu,FListSize+1);
        SetLength(QDesireEndpu,FListSize+1);
        SetLength(QDesireVVpu, FListSize+1);
        SetLength(QDesireWPpu, FListSize+1);
        SetLength(QDesireWVpu, FListSize+1);
        SetLength(QDesireLimitedpu, FListSize+1);
        SetLength(QDesireOptionpu, FListSize+1);
        SetLength(PLimitEndpu,FListSize+1);
        SetLength(PLimitVWpu, FListSize+1);
        SetLength(PLimitLimitedpu, FListSize+1);
        SetLength(PLimitOptionpu, FListSize+1);
        SetLength(QDesireDRCpu,FListSize+1);
        SetLength(deltaVDynReac,FListSize+1);
        SetLength(PLimitVW,FListSize+1);
        SetLength(POldVWpu,FListSize+1);
        SetLength(FdeltaQFactor,FListSize+1);
        SetLength(FdeltaPFactor,FListSize+1);
        SetLength(DeltaV_old,FListSize+1);
        SetLength(FVpuSolution,FListSize+1,2+1);
        SetLength(FRollAvgWindow,FListSize+1);
        SetLength(FDRCRollAvgWindow, FListSize+1);
        SetLength(FDRCRollAvgWindowpu, FListSize+1);
        SetLength(priorRollAvgWindow,FListSize+1);
        SetLength(priorDRCRollAvgWindow,FListSize+1);
        SetLength(FlagChangeCurve,FListSize+1);
        SetLength(FActiveVVCurve, FListSize+1);
        SetLength(FPriorWattspu, FListSize+1);
        SetLength(FPriorWatts, FListSize+1);
        SetLength(FPriorPLimitOptionpu, FListSize+1);
        SetLength(FPriorQDesireOptionpu, FListSize+1);
        SetLength(kW_out_desiredpu, FListSize+1);
        SetLength(kW_out_desired, FListSize+1);
        SetLength(FPriorvarspu, FListSize+1);
        SetLength(FPriorvars, FListSize+1);
        SetLength(FFlagVWOperates, FListSize+1);
        SetLength(FVVOperation, FListSize+1);
        SetLength(FWPOperation, FListSize+1);
        SetLength(FWVOperation, FListSize+1);
        SetLength(FVWOperation, FListSize+1);
        SetLength(FDRCOperation, FListSize+1);
        SetLength(FVVDRCOperation, FListSize+1);
        SetLength(FVBase, FListSize+1);
        SetLength(FVarFollowInverter, FListSize+1);
        SetLength(FInverterON,           FListSize+1);
        SetLength(FpresentkW,            FListSize+1);
        SetLength(FkVARating,            FListSize+1);
        SetLength(Fpresentkvar,          FListSize+1);
        SetLength(FkvarLimit,            FListSize+1);
        SetLength(FkvarLimitNeg,         FListSize+1);
        SetLength(FCurrentkvarLimit,     FListSize+1);
        SetLength(FCurrentkvarLimitNeg,  FListSize+1);
        SetLength(FDCkWRated,            FListSize+1);
        SetLength(FpctDCkWRated,         FListSize+1);
        SetLength(FEffFactor,            FListSize+1);
        SetLength(FDCkW,                 FListSize+1);
        SetLength(FPPriority,            FListSize+1);


        for i := 1 to FListSize do
          begin
            if StripExtension(LowerCase(FDERNameList.Strings[i-1])) = 'pvsystem2' then
              begin
                PVSys := PVSysClass.Find(StripClassName(FDERNameList.Strings[i-1]));

                If Assigned(PVSys) Then Begin
                    If PVSys.Enabled Then FDERPointerList.New := PVSys
                End
                Else Begin
                    DoSimpleMsg('Error: PVSystem2 Element "' + FDERNameList.Strings[i-1] + '" not found.', 14403);
                    Exit;
                End;

              end
            else if StripExtension(LowerCase(FDERNameList.Strings[i-1])) = 'storage2' then
              begin
                Storage2 := Storage2Class.Find(StripClassName(FDERNameList.Strings[i-1]));

                If Assigned(Storage2) Then Begin
                    If Storage2.Enabled Then FDERPointerList.New := Storage2
                End
                Else Begin
                    DoSimpleMsg('Error: Storage2 Element "' + FDERNameList.Strings[i-1] + '" not found.', 14403);
                    Exit;
                End;

              end
         end;

      end
    else
      begin
        {Search through the entire circuit for enabled PVSystem2 and Storage2 objects and add them to the list}

        // Adding PVSystem2 elements
        for i := 1 to PVSysClass.ElementCount do
          begin
            PVSys :=  PVSysClass.ElementList.Get(i);
            if PVSys.Enabled then FDERPointerList.New := PVSys;
            FDERNameList.Add(PVSys.QualifiedName);
          end;
        // Adding Storage2 elements
        for i := 1 to Storage2Class.ElementCount do
          begin
            Storage2 :=  Storage2Class.ElementList.Get(i);
            if Storage2.Enabled then FDERPointerList.New := Storage2;
            FDERNameList.Add(Storage2.QualifiedName);
          end;

        FListSize := FDERPointerList.ListSize;

        SetLength(ControlledElement,FListSize+1);
        SetLength(FAvgpVpuPrior, FListSize+1);
        SetLength(FAvgpDRCVpuPrior, FListSize+1);
        SetLength(FPresentVpu, FListSize+1);
        SetLength(FPresentDRCVpu, FListSize+1);
        SetLength(NPhasesDER,FListSize+1);
        SetLength(NCondsDER,FListSize+1);
        SetLength(CondOffset,FListSize+1);
        SetLength(cBuffer,FListSize+1,7);  // assuming no more than 6 conductors
        SetLength(FPendingChange,FListSize+1);
        SetLength(QDesiredVV,FListSize+1);
        SetLength(QDesiredWP,FListSize+1);
        SetLength(QDesiredWV,FListSize+1);
        SetLength(QOld,FListSize+1);
        SetLength(QOldVV,FListSize+1);
        SetLength(QOldDRC,FListSize+1);
        SetLength(QOldVVDRC,FListSize+1);
        SetLength(QDesiredDRC,FListSize+1);
        SetLength(QDesiredVVDRC,FListSize+1);
        SetLength(QHeadroom,FListSize+1);
        SetLength(QHeadroomNeg,FListSize+1);
        SetLength(PBase,FListSize+1);
        SetLength(Qoutputpu,FListSize+1);
        SetLength(QoutputVVpu,FListSize+1);
        SetLength(QoutputDRCpu,FListSize+1);
        SetLength(QoutputVVDRCpu,FListSize+1);
        SetLength(QDesireEndpu,FListSize+1);
        SetLength(QDesireVVpu, FListSize+1);
        SetLength(QDesireWPpu, FListSize+1);
        SetLength(QDesireWVpu, FListSize+1);
        SetLength(QDesireLimitedpu, FListSize+1);
        SetLength(QDesireOptionpu, FListSize+1);
        SetLength(PLimitEndpu,FListSize+1);
        SetLength(PLimitVWpu, FListSize+1);
        SetLength(PLimitLimitedpu, FListSize+1);
        SetLength(PLimitOptionpu, FListSize+1);
        SetLength(QDesireDRCpu,FListSize+1);
        SetLength(PLimitVW,FListSize+1);
        SetLength(POldVWpu,FListSize+1);
        SetLength(FdeltaQFactor,FListSize+1);
        SetLength(FdeltaPFactor,FListSize+1);
        SetLength(DeltaV_old,FListSize+1);
        SetLength(FRollAvgWindow,FListSize+1);
        SetLength(FDRCRollAvgWindow, FListSize+1);
        SetLength(FDRCRollAvgWindowpu, FListSize+1);
        SetLength(deltaVDynReac,FListSize+1);
        SetLength(priorRollAvgWindow,FListSize+1);
        SetLength(priorDRCRollAvgWindow,FListSize+1);
        SetLength(FVpuSolution,FListSize+1,2+1);
        SetLength(FlagChangeCurve,FListSize+1);
        SetLength(FActiveVVCurve, FListSize+1);
        SetLength(FPriorWattspu, FListSize+1);
        SetLength(FPriorWatts, FListSize+1);
        SetLength(FPriorPLimitOptionpu, FListSize+1);
        SetLength(FPriorQDesireOptionpu, FListSize+1);
        SetLength(kW_out_desiredpu, FListSize+1);
        SetLength(kW_out_desired, FListSize+1);
        SetLength(FPriorvarspu, FListSize+1);
        SetLength(FPriorvars, FListSize+1);
        SetLength(FFlagVWOperates, FListSize+1);
        SetLength(FVVOperation, FListSize+1);
        SetLength(FWVOperation, FListSize+1);
        SetLength(FWPOperation, FListSize+1);
        SetLength(FVWOperation, FListSize+1);
        SetLength(FDRCOperation, FListSize+1);
        SetLength(FVVDRCOperation, FListSize+1);
        SetLength(FVBase, FListSize+1);
        SetLength(FVarFollowInverter, FListSize+1);
        SetLength(FInverterON, FListSize+1);
        SetLength(FpresentkW,            FListSize+1);
        SetLength(FkVARating,            FListSize+1);
        SetLength(Fpresentkvar,          FListSize+1);
        SetLength(FkvarLimit,            FListSize+1);
        SetLength(FkvarLimitNeg,         FListSize+1);
        SetLength(FCurrentkvarLimit,     FListSize+1);
        SetLength(FCurrentkvarLimitNeg,  FListSize+1);
        SetLength(FDCkWRated,            FListSize+1);
        SetLength(FpctDCkWRated,         FListSize+1);
        SetLength(FEffFactor,            FListSize+1);
        SetLength(FDCkW,                 FListSize+1);
        SetLength(FPPriority,            FListSize+1);


      end;  {else}

    //Initialize arrays

    for i := 1 to FlistSize do
      begin

        if StripExtension(LowerCase(FDERNameList.Strings[i-1])) = 'pvsystem2' then
          begin
            PVSys := PVSysClass.Find(StripClassName(FDERNameList.Strings[i-1]));
            if (PVSys <> nil) then
               DERElem := TPCElement(PVSys)
          end
        else
          begin
            Storage2 := Storage2Class.Find(StripClassName(FDERNameList.Strings[i-1]));
            if (Storage2 <> nil) then
               DERElem := TPCElement(Storage2)
          end;


        for j := 1 to 6 do cBuffer[i,j]          := cZERO;

        Set_NTerms(DERElem.NTerms);

        CondOffset[i]                            := 0;
        NPhasesDER[i]                            := DERElem.NPhases;
        NCondsDER[i]                             := DERElem.NConds;
        FAvgpVpuPrior[i]                         := 0.0;
        FAvgpDRCVpuPrior[i]                      := 0.0;
        FPresentVpu[i]                           := 0.0;
        FPresentDRCVpu[i]                        := 0.0;
        QDesiredVV[i]                            := 0.0;
        QDesiredWP[i]                            := 0.0;
        QDesiredWV[i]                            := 0.0;
        QOld[i]                                  := -1.0;
        QOldVV[i]                                := -1.0;
        QOldDRC[i]                               := -1.0;
        QOldVVDRC[i]                             := -1.0;
        QDesiredDRC[i]                           := 0.0;
        QDesiredVVDRC[i]                         := 0.0;
        PLimitVW[i]                              := 0.0;
        POldVWpu[i]                              := 0.0;
        PBase[i]                                 := 0.0;
        QHeadroom[i]                             := 0.0;
        QHeadroomNeg[i]                          := 0.0;
        Qoutputpu[i]                             := 0.0;
        QoutputVVpu[i]                           := 0.0;
        QoutputDRCpu[i]                          := 0.0;
        QoutputVVDRCpu[i]                        := 0.0;
        QDesireEndpu[i]                          := 0.0;
        QDesireVVpu[i]                           := 0.0;
        QDesireWPpu[i]                           := 0.0;
        QDesireWVpu[i]                           := 0.0;
        QDesireLimitedpu[i]                      := 0.0;
        QDesireOptionpu[i]                       := 0.0;
        PLimitVWpu[i]                            := 0.0;
        PLimitLimitedpu[i]                       := 0.0;
        PLimitEndpu[i]                           := 0.0;
        PLimitOptionpu[i]                        := 0.0;
        QDesireDRCpu[i]                          := 0.0;
        FRollAvgWindow[i]                        := TRollAvgWindow.Create;
        FDRCRollAvgWindow[i]                     := TRollAvgWindow.Create;

        FdeltaQFactor[i]                         := DELTAQDEFAULT;
        FdeltaPFactor[i]                         := DELTAPDEFAULT;
        DeltaV_old[i]                            := -1.0;

        deltaVDynReac[i]                         := 0.0;
        FlagChangeCurve[i]                       := False;
        FActiveVVCurve[i]                        := 1;
        priorRollAvgWindow[i]                    := 0.0;
        priorDRCRollAvgWindow[i]                 := 0.0;
        FPriorWattspu[i]                         := 0.0;
        FPriorWatts[i]                           := 0.0;
        FPriorPLimitOptionpu[i]                  := 0.0;
        FPriorQDesireOptionpu[i]                 := 0.0;
        kW_out_desiredpu[i]                      := 0.0;
        kW_out_desired[i]                        := 0.0;
        FPriorvarspu[i]                          := 0.0;
        FPriorvars[i]                            := 0.0;

        FFlagVWOperates[i]                       := False;

        FVVOperation[i]                          := 0.0;
        FVWOperation[i]                          := 0.0;
        FDRCOperation[i]                         := 0.0;
        FVVDRCOperation[i]                       := 0.0;
        FWPOperation[i]                          := 0.0;
        FWVOperation[i]                          := 0.0;

        for j := 1 to 2 do  FVpuSolution[i,j]    := 0.0;

        FPendingChange[i]                        := NONE;

        FVbase[i]                                := 0.0;
        FVarFollowInverter[i]                    := False;
        FInverterON[i]                           := True;
        FpresentkW[i]                            := 0.0;
        FkVARating[i]                            := 0.0;
        Fpresentkvar[i]                          := 0.0;
        FkvarLimit[i]                            := 0.0;
        FkvarLimitNeg[i]                         := 0.0;
        FCurrentkvarLimit[i]                     := 0.0;
        FCurrentkvarLimitNeg[i]                  := 0.0;
        FDCkWRated[i]                            := 0.0;
        FpctDCkWRated[i]                         := 0.0;
        FEffFactor[i]                            := 0.0;
        FDCkW[i]                                 := 0.0;
        FPPriority[i]                            := False;


      end; {for}

    RecalcElementData(ActiveActor);
    if FDERPointerList.ListSize>0 then Result := TRUE;
  end;

procedure TInvControl2Obj.Reset;
  begin
    // inherited;
  end;

function TInvControl2.GetXYCurve(Const CurveName: String;InvControl2Mode: Integer): TXYcurveObj;
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
    if InvControl2Mode = VOLTWATT then
    begin
      for i:= 1 to Result.NumPoints do
        begin
          if (Result.YValue_pt[i] < 0.0) or (Result.YValue_pt[i] > 1.0) then
            begin
              DoSimpleMsg('XY Curve object: "' + CurveName + '" has active power value(s) greater than 1.0 per-unit or less than -1.0 per-unit.  Not allowed for VOLTWATT control mode for PVSystem2/Storage2s', 381);
              Result := NIL;
              Break;
            end;
        end;
    end;

    // if WATTPF control mode then check for any negative pf values
    // and values greater than 1.0
    if InvControl2Mode = WATTPF then
    begin
      for i:= 1 to Result.NumPoints do
        begin
          if (Result.YValue_pt[i] < -1.0) or (Result.YValue_pt[i] > 1.0) then
            begin
              DoSimpleMsg('XY Curve object: "' + CurveName + '" has power factor value(s) greater than 1.0 or less than -1.0.  Not allowed for WATTPF control mode for PVSystem2/Storage2s', 381);
              Result := NIL;
              Break;
            end;
        end;
    end;

    // if WATTVAR control mode then check for any negative pf values
    // and values greater than 1.0
    if InvControl2Mode = WATTVAR then
    begin
      for i:= 1 to Result.NumPoints do
        begin
          if (Result.YValue_pt[i] < -1.0) or (Result.YValue_pt[i] > 1.0) then
            begin
              DoSimpleMsg('XY Curve object: "' + CurveName + '" has reactive power value(s) greater than 1.0 per-unit or less than -1.0 per-unit.  Not allowed for WATTVAR control mode for PVSystem2/Storage2s', 381);
              Result := NIL;
              Break;
            end;
        end;
    end;

  end;

function  TInvControl2Obj.InterpretAvgVWindowLen(const s:string):Integer;

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

function  TInvControl2Obj.InterpretDRCAvgVWindowLen(const s:string):Integer;

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

function TInvControl2Obj.GetPropertyValue(Index: Integer): String;

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

      else  // take the generic handler
        Result := Inherited GetPropertyValue(index);
    end;
  end;


function TInvControl2Obj.ReturnElementsList: String;
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

procedure TInvControl2Obj.Set_Enabled(Value: Boolean);
  begin
    inherited;

    {Reset controlled PVSystem2/Storage2s to original PF}

  end;

procedure TInvControl2Obj.Set_PendingChange(Value: Integer;DevIndex: Integer);
  begin
    FPendingChange[DevIndex] := Value;
    DblTraceParameter := Value;
  end;

procedure TInvControl2Obj.UpdateInvControl2(i:integer; ActorID : Integer);
  Var
    j,k                    : Integer;
    solnvoltage            : Double;
    tempVbuffer            : pComplexArray;
    PVSys                  : TPVSystem2Obj;
    Storage2               : TStorage2Obj;
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

          if ControlledElement[j].DSSClassName = 'PVSystem2' then PVSys := ControlledElement[j] as TPVSystem2Obj
          else Storage2 := ControlledElement[j] as TStorage2Obj;

          BasekV :=    FVBase[i] / 1000.0;

          //             FPriorvars[j]  := PVSys.Presentkvar;
          //             FPriorWatts[j]  := PVSys.PresentkW;
          FPriorPLimitOptionpu[j]   := PLimitOptionpu[j];
          FPriorQDesireOptionpu[j]  := QDesireOptionpu[j];

          // Used to update the VW resquested kW
          if ControlledElement[j].DSSClassName = 'PVSystem2' then PVSys.VWmode := FALSE
          else Storage2.VWMode := FALSE;

          if ControlledElement[j].DSSClassName = 'PVSystem2' then PVSys.VVmode := FALSE
          else Storage2.VVMode := FALSE;

          if ControlledElement[j].DSSClassName = 'PVSystem2' then PVSys.DRCmode := FALSE
          else Storage2.DRCMode := FALSE;


          FFlagVWOperates[j] := False;

          // Reset the operation flags for the new time step
          FVVOperation[j]   := 0;
          FVWOperation[j]   := 0;
          FDRCOperation[j]  := 0;
          FVVDRCOperation[j]:= 0;
          FWPOperation[j]   := 0;
          FWVOperation[j]   := 0;

          // Reinitialize convergence arrays.
          //FdeltaQFactor[j] := DELTAQDEFAULT;
          FdeltaPFactor[j] := DELTAPDEFAULT;

          // allocated enough memory to buffer to hold voltages and initialize to cZERO
          Reallocmem(tempVbuffer, Sizeof(tempVbuffer^[1]) * ControlledElement[j].NConds);
          for k := 1 to ControlledElement[j].NConds do tempVbuffer[k] := cZERO;

          priorRollAvgWindow[j] := FRollAvgWindow[j].Get_AvgVal;
          priorDRCRollAvgWindow[j] := FDRCRollAvgWindow[j].Get_AvgVal;

          // compute the present terminal voltage
          ControlledElement[j].ComputeVterminal(ActorID);
          //PVSys.Set_Variable(5,FDRCRollAvgWindow[j].Get_AvgVal); // save rolling average voltage in monitor

          solnvoltage := 0.0;

          GetmonVoltage(ActorID, solnvoltage, j, BasekV);

          //for k := 1 to localControlledElement.Yorder do tempVbuffer[k] := localControlledElement.Vterminal^[k];


          //for k := 1 to localControlledElement.Nphases do solnvoltage := solnvoltage + Cabs(tempVbuffer[k]);
          //solnvoltage := solnvoltage / (localControlledElement.Nphases*1.0); // average of voltages if more than one phase

          // add present power flow solution voltage to the rolling average window
          FRollAvgWindow[j].Add(solnvoltage,ActiveCircuit[ActorID].Solution.DynaVars.h,FVAvgWindowLengthSec);
          FDRCRollAvgWindow[j].Add(solnvoltage,ActiveCircuit[ActorID].Solution.DynaVars.h,FDRCVAvgWindowLengthSec);

          FVpuSolution[j,FVpuSolutionIdx] := solnvoltage/((ActiveCircuit[ActorID].Buses^[ControlledElement[j].terminals^[1].busRef].kVBase)*1000.0);

          Reallocmem(tempVbuffer, 0);   // Clean up memory

        end;

  end;

function TInvControl2Obj.Get_PendingChange(DevIndex: Integer):Integer;
  begin
    Result := FPendingChange[DevIndex];
  end;

procedure TInvControl2Obj.CalcVoltWatt_watts(j: Integer; ActorID : Integer);
  VAR
    DeltaPpu 							        :Double;
  // PLimitEndpu[j] <= abs(kW_out_desiredpu[j] will always be true when we are in 'resquest' region of VW
  // That's what we want. In this region, VW will work similarly to VV. So we need to move slowly towards the VW curve point.
  begin
     if ((PLimitEndpu[j] < 1.0) and (PLimitEndpu[j] <= abs(kW_out_desiredpu[j]))) or (FFlagVWOperates[j]) then
      begin
        if(ActiveCircuit[ActorID].Solution.ControlIteration=1) then POldVWpu[j] :=  abs(kW_out_desiredpu[j]); // take abs(kW_out_desiredpu[j]) because might be in charging mode.
        FFlagVWOperates[j] := True;

        // PLimitEndpu might be negative here in 'requesting' region. Do we need to give POldVW a sign in this case?
        // Yes, it will naturally evolve to a negative value with the process. It will always positive only in the 1st control iteration.
        DeltaPpu := PLimitEndpu[j] - POldVWpu[j];

        if FdeltaP_factor = FLAGDELTAP then Change_deltaP_factor(ActorID, j)
        else FdeltaPFactor[j] := FdeltaP_factor;

        PLimitVW[j] := (POldVWpu[j] + DeltaPpu * FdeltaPFactor[j]) *  PBase[j];
      end
    else
      begin
        PLimitVW[j] := PLimitEndpu[j] * PBase[j];
      end;

  end;

procedure TInvControl2Obj.Check_Plimits(j: Integer; P: Double; ActorID : Integer);
  VAR
    P_Ppriority             :Double;
    pctDCkWRatedlimit       :Double;

  begin

    PLimitLimitedpu[j] := 1.0; // Not limited

    // volt-watt states
    if P < 1.0 then FVWOperation[j] := 1.0;

    pctDCkWRatedlimit := FpctDCkWRated[j] * FDCkWRated[j];

    // PLimitEndpu should be less than the P avaliable under var priority   (works for VV_VW)
    if FPPriority[j] = False then
      begin
        P_Ppriority :=  Sqrt(SQR(FkVARating[j]) - SQR(Fpresentkvar[j]));
        if P_Ppriority < (abs(P) * PBase[j]) then   // P might be negative in requesting region for storage
          begin
            PLimitLimitedpu[j]   := P_Ppriority / PBase[j] * sign(P);
            FVWOperation[j] := 0.0; // kVA exceeded under watt priority
          end;
      end;

    // PLimitEndpu should be less than pctPmpp
    if (abs(P) * PBase[j]) >  pctDCkWRatedlimit then
      begin
        FVWOperation[j] := 0.0; // pctPmpp exceeded under watt priority
        PLimitLimitedpu[j]   := pctDCkWRatedlimit / PBase[j] * sign(P);
      end;

  end;

procedure TInvControl2Obj.CalcVoltVar_vars(j: Integer; ActorID : Integer);
  VAR
    DeltaQ                                   :Double;

  begin
    if(FlagChangeCurve[j] = False) then
      begin
        if QDesireEndpu[j] >= 0.0 then DeltaQ := QDesireEndpu[j] * QHeadRoom[j] - QOldVV[j]
        else DeltaQ := QDesireEndpu[j] * QHeadRoomNeg[j] - QOldVV[j];

        if FdeltaQ_factor = FLAGDELTAQ then Change_deltaQ_factor(ActorID, j)
        else FdeltaQFactor[j] := FdeltaQ_factor;

        QDesiredVV[j] := QOldVV[j] + DeltaQ * FdeltaQFactor[j];
      end
    // else, stay at present var output level
    else
      begin
        QDesiredVV[j] := Fpresentkvar[j]
      end;
  end;

procedure TInvControl2Obj.CalcWATTPF_vars(j: Integer; ActorID : Integer);

  begin

    if QDesireEndpu[j] >= 0.0 then
      QDesiredWP[j] := QDesireEndpu[j] * QHeadRoom[j]
    else
      QDesiredWP[j] := QDesireEndpu[j] * QHeadRoomNeg[j];
  end;

procedure TInvControl2Obj.CalcWATTVAR_vars(j: Integer; ActorID : Integer);

  begin

    if QDesireEndpu[j] >= 0.0 then
      QDesiredWV[j] := QDesireEndpu[j] * QHeadRoom[j]
    else
      QDesiredWV[j] := QDesireEndpu[j] * QHeadRoomNeg[j];
  end;

procedure TInvControl2Obj.CalcDRC_vars(j: Integer; ActorID : Integer);
  VAR
    DeltaQ                :Double;

  begin
    if QDesireEndpu[j] >= 0.0 then DeltaQ := QDesireEndpu[j] * QHeadRoom[j] - QOldDRC[j]
    else DeltaQ := QDesireEndpu[j] * QHeadRoomNeg[j] - QOldDRC[j];

    if FdeltaQ_factor = FLAGDELTAQ then Change_deltaQ_factor(ActorID, j)
    else FdeltaQFactor[j] := FdeltaQ_factor;

    QDesiredDRC[j]       := QOldDRC[j] + DeltaQ * FdeltaQFactor[j];

  end;

procedure TInvControl2Obj.CalcVVDRC_vars(j: Integer; ActorID : Integer);
  VAR
    DeltaQ                :Double;

  begin
    if QDesireEndpu[j] >= 0.0 then DeltaQ := QDesireEndpu[j] * QHeadRoom[j] - QOldVVDRC[j]
    else DeltaQ := QDesireEndpu[j] * QHeadRoomNeg[j] - QOldVVDRC[j];

    if FdeltaQ_factor = FLAGDELTAQ then Change_deltaQ_factor(ActorID, j)
    else FdeltaQFactor[j] := FdeltaQ_factor;

    QDesiredVVDRC[j] := QOldVVDRC[j] + DeltaQ * FdeltaQFactor[j];

  end;

procedure TInvControl2Obj.Calc_PBase(j: Integer; ActorID : Integer);
  Var
    DERelem                                  :TPCElement;

  begin

    DERelem := ControlledElement[j];

    if DERelem.DSSClassName = 'PVSystem2' then
      begin
        if(FVoltwattYaxis = 0)  then PBase[j] := FDCkW[j] * FEffFactor[j]

        else if(FVoltwattYaxis = 1)  then PBase[j] := FDCkWRated[j]

        else if(FVoltwattYaxis = 2)  then PBase[j] := FDCkWRated[j] * FpctDCkWRated[j]

        else if(FVoltwattYaxis = 3)  then PBase[j] := FkVARating[j];
      end
    else
      begin
        if(FVoltwattYaxis = 0)  then PBase[j] := TStorage2Obj(DERelem).DCkW * FEffFactor[j]

        else if(FVoltwattYaxis = 1)  then PBase[j] := FDCkWRated[j]

        else if(FVoltwattYaxis = 2)  then PBase[j] := FDCkWRated[j] * FpctDCkWRated[j]

        else if(FVoltwattYaxis = 3)  then PBase[j] := FkVARating[j];

      end;

  end;

procedure TInvControl2Obj.CalcLPF(m: Integer; powertype: String; LPF_desiredpu : Double; ActorID : Integer);
  VAR
    alpha                     :Double;

    // Applies the LPF:
    //  Return value is in kvar for VARS
    //  Return value is in puPmpp for WATTS

  begin
    // Qoutput(t) = Qdesired(t) x {1- exp[-(t-t0)/tau]} + Qoutput(t-t0) x exp[-(t-t0)/tau]
    // calculate the alpha constant: alpha = exp[-(t-t0)/tau]
    alpha := exp(-1.0 * ActiveCircuit[ActorID].Solution.DynaVars.h/FLPFTau);

    if powertype = 'VARS' then QDesireOptionpu[m] := LPF_desiredpu * (1-alpha) + FPriorQDesireOptionpu[m] * alpha;

    if powertype = 'WATTS' then PLimitOptionpu[m] := LPF_desiredpu * (1-alpha) + FPriorPLimitOptionpu[m] * alpha

  end;

procedure TInvControl2Obj.CalcRF(m: Integer; powertype: String; RF_desiredpu: Double ; ActorID :Integer);

  begin
    // Applies the Rise/Fall limiting function:

    if powertype='VARS' then
      begin
        // rate of change rise/fall limit
        if (RF_desiredpu - FPriorQDesireOptionpu[m]) > (FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h) then
          QDesireOptionpu[m] := FPriorQDesireOptionpu[m] + FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h
        else if (RF_desiredpu - FPriorQDesireOptionpu[m]) < (-1 * FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h) then
          QDesireOptionpu[m] := FPriorQDesireOptionpu[m] - FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h
        else
          QDesireOptionpu[m] := RF_desiredpu;
      end;

    if powertype='WATTS' then
      begin
        // rate of change rise/fall limit
        if (RF_desiredpu - FPriorPLimitOptionpu[m]) > (FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h) then
          PLimitOptionpu[m] := FPriorPLimitOptionpu[m] + (FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h)
        else if (RF_desiredpu - FPriorPLimitOptionpu[m]) < (-1 * FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h) then
          PLimitOptionpu[m] := FPriorPLimitOptionpu[m] - (FRiseFallLimit * ActiveCircuit[ActorID].Solution.DynaVars.h)
        else
          PLimitOptionpu[m] := RF_desiredpu;
      end;

  end;


procedure TInvControl2Obj.CalcPVWcurve_limitpu(j: Integer; ActorID : Integer);
  begin

     if ControlledElement[j].DSSClassName = 'PVSystem2' then PLimitVWpu[j] := Fvoltwatt_curve.GetYValue(FPresentVpu[j])
     else
      begin
        if TStorage2Obj(ControlledElement[j]).Storage2State  = STORE_DISCHARGING then
        Begin
            if TStorage2Obj(ControlledElement[j]).FVWStateRequested then PLimitVWpu[j] := FvoltwattCH_curve.GetYValue(FPresentVpu[j])
            else PLimitVWpu[j] := Fvoltwatt_curve.GetYValue(FPresentVpu[j]);

        End
        else if (TStorage2Obj(ControlledElement[j]).Storage2State  = STORE_CHARGING) and (FvoltwattCH_curve <> Nil) then
        Begin
            if TStorage2Obj(ControlledElement[j]).FVWStateRequested then PLimitVWpu[j] := Fvoltwatt_curve.GetYValue(FPresentVpu[j])
            else PLimitVWpu[j] := FvoltwattCH_curve.GetYValue(FPresentVpu[j]) // try with positive PlimitVWpu
        End

        else PLimitVWpu[j] := 1.0; // don't limit if in idling state
      end;

  end;


procedure TInvControl2Obj.CalcQVVcurve_desiredpu(j: Integer; ActorID : Integer);
  VAR
    voltagechangesolution                    :Double;
    QPresentpu                               :Double;
    VpuFromCurve                             :Double;

  begin

    QDesireVVpu[j] := 0.0;

    if Fpresentkvar[j] >= 0.0 then QPresentpu   := Fpresentkvar[j] / QHeadRoom[j]
    else QPresentpu   := Fpresentkvar[j] / QHeadRoomNeg[j];

    voltagechangesolution := 0.0;

    // for first two seconds, keep voltagechangesolution equal to zero
    // we don't have solutions from the time-series power flow, yet
    if ((ActiveCircuit[ActorID].Solution.DynaVars.dblHour*3600.0 / ActiveCircuit[ActorID].Solution.DynaVars.h)<3.0) then voltagechangesolution := 0.0
    else if(FVpuSolutionIdx = 1) then voltagechangesolution := FVpuSolution[j,1] - FVpuSolution[j,2]
    else if(FVpuSolutionIdx = 2) then voltagechangesolution := FVpuSolution[j,2] - FVpuSolution[j,1];

    // if no hysteresis (Fvvc_curveOffset == 0), then just look up the value
    // from the volt-var curve
    if Fvvc_curveOffset = 0.0 then
      begin  // no hysteresis
        QDesireVVpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j])
      end // end of logic for the no-hysteresis case

    // else if we're going in the positive direction and on curve 1, stay
    // with curve 1
    else if (voltagechangesolution > 0) and (FActiveVVCurve[j] = 1) then
      begin
        if(FlagChangeCurve[j] = True) then
          begin
            VpuFromCurve := Fvvc_curve.GetXValue(QPresentpu);
              if(Abs(FPresentVpu[j] - VpuFromCurve) < FVoltageChangeTolerance/2.0) then
                begin
                  QDesireVVpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]);      //Y value = in per-unit of headroom
                  FlagChangeCurve[j] := False;
                end
              else
                begin
                  QDesireVVpu[j] := QPresentpu;            // (PR) look at here
                  FlagChangeCurve[j] := False;
                end;
          end
        else
          begin
            QDesireVVpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]);      //Y value = in per-unit of headroom
          end;
      end

    // with hysteresis if we're going in the positive direction on voltages
    // from last two power flow solutions, and we're using curve 2, keep vars
    // the same, and change to curve1 active
    else if (voltagechangesolution > 0) and (FActiveVVCurve[j] = 2) then
      begin
        QDesireVVpu[j] := QPresentpu;
        FActiveVVCurve[j] := 1;
        FlagChangeCurve[j] := True;
      end

    // with hysteresis if we're going in the negative direction on voltages
    // from last two power flow solutions, and we're using curve 2, either
    // lookup the vars for the voltage we're at (with offset on curve1),
    // or if we've not just changed curves, stay at the current p.u.
    // var output
    else if (voltagechangesolution < 0) and (FActiveVVCurve[j] = 2) then
      begin
        if(FlagChangeCurve[j] = True) then
          begin
            VpuFromCurve := Fvvc_curve.GetXValue(QPresentpu);
            VpuFromCurve := VpuFromCurve - Fvvc_curveOffset;
            if(Abs(FPresentVpu[j] - VpuFromCurve) < FVoltageChangeTolerance/2.0)  then
              begin
                QDesireVVpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]-Fvvc_curveOffset);      //Y value = in per-unit of headroom
                FlagChangeCurve[j] := False;
              end
            else
              begin
                QDesireVVpu[j] := QPresentpu;
                FlagChangeCurve[j] := False;
              end;
          end
        else
          begin
            QDesireVVpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]-Fvvc_curveOffset);      //Y value = in per-unit of headroom
          end;
      end

    // with hysteresis if we're going in the negative direction on voltages
    // from last two power flow solutions, and we're using curve 1, then
    // stay wjth present output vars and make curve2 active, set curve change
    // flag
    else if (voltagechangesolution < 0) and (FActiveVVCurve[j] = 1) then
      begin
        QDesireVVpu[j] := QPresentpu;
        FActiveVVCurve[j] := 2;
        FlagChangeCurve[j] := True;
      end


    // if no change in voltage from one powerflow to the next, then
    // do one of the following
    else if (voltagechangesolution = 0)  and (FActiveVVCurve[j] = 1) and (FlagChangeCurve[j] = False) then
      begin
        QDesireVVpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]);
      end
    else if (voltagechangesolution = 0) and (FlagChangeCurve[j] = True) then
      begin
        QDesireVVpu[j] := QPresentpu;
      end

    else if (voltagechangesolution = 0)  and (FActiveVVCurve[j] = 2) and (FlagChangeCurve[j] = False) then
      begin
        QDesireVVpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]-Fvvc_curveOffset);
      end;

  end;

procedure TInvControl2Obj.CalcQWVcurve_desiredpu(j: Integer; ActorID : Integer);
  VAR
    voltagechangesolution                    :Double;


  begin

    QDesireWVpu[j] := 0.0;

    voltagechangesolution := 0.0;

    // for first two seconds, keep voltagechangesolution equal to zero
    // we don't have solutions from the time-series power flow, yet
    if ((ActiveCircuit[ActorID].Solution.DynaVars.dblHour*3600.0 / ActiveCircuit[ActorID].Solution.DynaVars.h)<3.0) then voltagechangesolution := 0.0
    else if(FVpuSolutionIdx = 1) then voltagechangesolution := FVpuSolution[j,1] - FVpuSolution[j,2]
    else if(FVpuSolutionIdx = 2) then voltagechangesolution := FVpuSolution[j,2] - FVpuSolution[j,1];

    QDesireWVpu[j] := Fwattvar_curve.GetYValue(FDCkW[j] * FEffFactor[j] * FpctDCkWRated[j] / FDCkWRated[j]);

  end;

procedure TInvControl2Obj.CalcQWPcurve_desiredpu(j: Integer; ActorID : Integer);
  VAR
    voltagechangesolution                    :Double;
    p                                        :Double;
    pf_priority                              :Boolean;
    QDesiredWP                               :Double;

  begin

    QDesireWPpu[j] := 0.0;

    voltagechangesolution := 0.0;

    // for first two seconds, keep voltagechangesolution equal to zero
    // we don't have solutions from the time-series power flow, yet
    if ((ActiveCircuit[ActorID].Solution.DynaVars.dblHour*3600.0 / ActiveCircuit[ActorID].Solution.DynaVars.h)<3.0) then voltagechangesolution := 0.0
    else if(FVpuSolutionIdx = 1) then voltagechangesolution := FVpuSolution[j,1] - FVpuSolution[j,2]
    else if(FVpuSolutionIdx = 2) then voltagechangesolution := FVpuSolution[j,2] - FVpuSolution[j,1];

    pf_wp_nominal := Fwattpf_curve.GetYValue(FDCkW[j] * FEffFactor[j] * FpctDCkWRated[j] / FDCkWRated[j]);

    if ControlledElement[j].DSSClassName = 'PVSystem2'     then pf_priority := TPVSystem2Obj(ControlledElement[j]).PVSystem2Vars.PF_Priority
    else if ControlledElement[j].DSSClassName = 'Storage2' then pf_priority :=  TStorage2Obj(ControlledElement[j]).Storage2Vars.PF_Priority;

    if (FPPriority[j] = FALSE) and (pf_priority = FALSE) then p := FDCkW[j] * FEffFactor[j] * FpctDCkWRated[j]
    else p := kW_out_desired[j];

    QDesiredWP := p * sqrt(1 / (pf_wp_nominal * pf_wp_nominal) -1) * sign(pf_wp_nominal);


    if QDesiredWP >= 0.0 then  QDesireWPpu[j] := QDesiredWP / QHeadRoom[j]
    else  QDesireWPpu[j] := QDesiredWP / QHeadRoomNeg[j];

  end;

procedure TInvControl2Obj.CalcQDRC_desiredpu(j: Integer; ActorID : Integer);
  VAR
    basekV                                    :Double;

  begin

    QDesireDRCpu[j] := 0.0;

    basekV :=    FVBase[j] / 1000.0; // It's a line-to-ground voltage

    // calculate deltaV quantity in per-unit from subtracting the rolling average
    // value (in p.u.) from the present p.u. terminal voltage (average of line-ground)
    // if more than one phase
    if(FDRCRollAvgWindow[j].Get_AvgVal/(basekV*1000.0)) = 0.0 then deltaVDynReac[j]:=0
    else deltaVDynReac[j] := FPresentDRCVpu[j] - FDRCRollAvgWindow[j].Get_AvgVal/(basekV*1000.0);

    // if below the lower deadband and deltaV quantity is non-zero then
    // calculate desired pu var output. In per-unit of kva rating (also
    // ampere rating), per report specifications.
    if (deltaVDynReac[j] <>0) and (FPresentDRCVpu[j] < FDbVMin) then QDesireDRCpu[j] := -deltaVDynReac[j]*FArGraLowV

    // if above the upper deadband and deltaV quantity is non-zero then
    // calculate desired pu var output. In per-unit of kva rating (also
    // ampere rating), per report specifications.

    else if (deltaVDynReac[j] <>0) and (FPresentDRCVpu[j] > FDbVMax) then QDesireDRCpu[j] := -deltaVDynReac[j]*FArGraHiV

    else if deltaVDynReac[j] = 0.0 then QDesireDRCpu[j] := 0.0;

    if (ActiveCircuit[ActorID].Solution.Dynavars.t=1) then QDesireDRCpu[j] := 0.0;

  end;

procedure TInvControl2Obj.Check_Qlimits(j: Integer; Q: Double; ActorID : Integer);
  VAR
    Q_Ppriority                              :Double;
    currentkvarlimitpu                       :Double;
    currentkvarlimitnegpu                    :Double;
    FOperation                               :Double;
    error                                    :Double;

  begin

    QDesireLimitedpu[j] := 1.0; // Not limited

    currentkvarlimitpu := FCurrentkvarLimit[j] / QHeadRoom[j];
    currentkvarlimitnegpu := FCurrentkvarLimitNeg[j] / QHeadRoomNeg[j];

    if currentkvarlimitpu > QDesireLimitedpu[j]  then  currentkvarlimitpu := QDesireLimitedpu[j];
    if currentkvarlimitnegpu > QDesireLimitedpu[j]  then  currentkvarlimitnegpu := QDesireLimitedpu[j];

    // states
    error := 0;
    if (ControlMode = VOLTVAR)          then error := 0.005;
    if (ControlMode = WATTPF)           then error := 0.005;
    if (ControlMode = WATTVAR)          then error := 0.005;
    if (ControlMode = DRC)              then error := 0.0005;
    if (CombiControlMode = VV_DRC)      then error := 0.005;
    if (CombiControlMode = VV_VW)       then error := 0.005;

    if Q < -error then FOperation := -1.0
    else if Q > error then  FOperation := 1.0
    else FOperation := 0.0;

    // QVV curve desiredpu should be less than currentkvarlimit(neg)
    if (Q > 0.0) and (abs(Q) >= abs(currentkvarlimitpu)) then
      begin
        FOperation := 0.2 * sign(Q); // When kvarlimit is exceeded
        QDesireLimitedpu[j] := currentkvarlimitpu * sign(Q);
      end
    else if (Q < 0.0) and (abs(Q) >= abs(currentkvarlimitnegpu))  then
      begin
        FOperation := 0.2 * sign(Q); // When kvarlimitneg is exceeded
        QDesireLimitedpu[j] := currentkvarlimitnegpu * sign(Q);
      end;

    // Qdesiredpu should be less than the Q avaliable under watt priority  (works just for varmax)
    if FPPriority[j] and ((FReacPower_ref = 'VARMAX') or (ControlMode = WATTPF)) then
      begin
        if Q >= 0.0 then Q_Ppriority :=  Sqrt(SQR(FkVARating[j]) - SQR(FpresentkW[j]))/QHeadRoom[j]
        else Q_Ppriority :=  Sqrt(SQR(FkVARating[j]) - SQR(FpresentkW[j]))/QHeadRoomNeg[j];

        if (abs(Q_Ppriority) < abs(QDesireLimitedpu[j])) and (abs(Q_Ppriority) < abs(Q)) then
          begin
            FOperation := 0.6 * sign(Q); // kVA exceeded under watt priority is considered above
            if (abs(Q) < (0.01 / 100)) or (abs(Q_Ppriority) < epsilon) then  FOperation := 0.0;
            QDesireLimitedpu[j] := Q_Ppriority * sign(Q);
          end;
      end;


    // States Flags
    if (ControlMode = VOLTVAR)         then FVVOperation[j]    := FOperation;
    if (ControlMode = WATTPF)          then FWPOperation[j]    := FOperation;
    if (ControlMode = WATTVAR)         then FWVOperation[j]    := FOperation;
    if (ControlMode = DRC)             then FDRCOperation[j]   := FOperation;
    if (CombiControlMode = VV_DRC)     then FVVDRCOperation[j] := FOperation;
    if (CombiControlMode = VV_VW)      then FVVOperation[j]    := FOperation;

  end;

procedure TInvControl2Obj.Calc_QHeadRoom(j: Integer; ActorID : Integer);
  begin

    if FReacPower_ref = 'VARAVAL' then
      begin
        if(abs(FpresentkW[j]) < FkVARating[j]) then
          QHeadRoom[j] := SQRT(Sqr(FkVARating[j])-Sqr(FpresentkW[j]))
        else
          QHeadRoom[j] := 0.0;

        QHeadRoomNeg[j] := QHeadRoom[j];
      end;

    if (FReacPower_ref = 'VARMAX') or (ControlMode = WATTPF) then
      begin
        QHeadRoom[j] := FkvarLimit[j];
        QHeadRoomNeg[j] := FkvarLimitNeg[j];
      end;

    if(QHeadRoom[j] = 0.0) then QHeadRoom[j] := FkvarLimit[j];
    if(QHeadRoomNeg[j] = 0.0) then QHeadRoomNeg[j] := FkvarLimitNeg[j];

  end;

procedure TInvControl2Obj.Change_deltaQ_factor(ActorID : Integer; j: Integer);
  VAR
    DeltaV                                   :Double;

  begin
    DeltaV := Abs(FPresentVpu[j] - FAvgpVpuPrior[j]);

    if (DeltaV_old[j] >= 0.0) then
      begin
        if (abs(DeltaV) > 0.8 * DeltaV_old[j]) and (FdeltaQFactor[j] > 0.2) then FdeltaQFactor[j] := FdeltaQFactor[j] - 0.1
        else if (abs(DeltaV) > 0.6 * DeltaV_old[j]) and (FdeltaQFactor[j] > 0.2) then FdeltaQFactor[j] := FdeltaQFactor[j] - 0.05
        else if (abs(DeltaV) < 0.2 * DeltaV_old[j]) and (FdeltaQFactor[j] < 0.9) then FdeltaQFactor[j] := FdeltaQFactor[j] + 0.1
        else if (abs(DeltaV) < 0.4 * DeltaV_old[j]) and (FdeltaQFactor[j] < 0.9) then FdeltaQFactor[j] := FdeltaQFactor[j] + 0.05;
      end;

    DeltaV_old[j] := Abs(FPresentVpu[j] - FAvgpVpuPrior[j]);
  end;

procedure TInvControl2Obj.Change_deltaP_factor(ActorID : Integer; j: Integer);
  VAR
    DeltaV                                   :Double;

  begin
    DeltaV := Abs(FPresentVpu[j] - FAvgpVpuPrior[j]);

    if DeltaV_old[j] >= 0.0 then
      begin
        if (abs(DeltaV) > 0.9 * DeltaV_old[j]) and (FdeltaPFactor[j] > 0.2)  then FdeltaPFactor[j] := FdeltaPFactor[j] - 0.1
        else if (abs(DeltaV) > 0.8 * DeltaV_old[j]) and (FdeltaPFactor[j] > 0.1) then FdeltaPFactor[j] := FdeltaPFactor[j] - 0.05
        else if (abs(DeltaV) < 0.2 * DeltaV_old[j]) and (FdeltaPFactor[j] < 0.9) then FdeltaPFactor[j] := FdeltaPFactor[j] + 0.05
        else if (abs(DeltaV) < 0.1 * DeltaV_old[j]) and (FdeltaPFactor[j] < 0.9)  then FdeltaPFactor[j] := FdeltaPFactor[j] + 0.1;
      end;

    DeltaV_old[j] := Abs(FPresentVpu[j] - FAvgpVpuPrior[j]);
  end;


//Called at end of main power flow solution loop
procedure TInvControl2.UpdateAll(ActorID : integer);
  VAR
    i : Integer;

  begin

    for i := 1 to ElementList.ListSize  do
      with TInvControl2Obj(ElementList.Get(i)) do
        if Enabled then UpdateInvControl2(i, ActorID);

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

