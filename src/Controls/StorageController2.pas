unit StorageController2;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2019, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
  A StorageController is a control element that is connected to a terminal of another
  circuit element and sends dispatch  signals to a fleet of energy storage elements it controls

  A StorageController is defined by a New command:

  New StorageController.Name=myname Element=devclass.name terminal=[ 1|2|...] Elementlist = (elem1  elem2 ...)

  or ... ElementList = [File=filename] where storage class elements are listed one to a line
  If omitted, all storage elements found in the active circuit are included by default and controlled as a fleet.

  Added new control mode for charging 12/19/2018
  Proposed by Valentin Rigoni

}

interface

uses
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    Arraydef,
    ucomplex,
    utilities,
    DSSPointerList,
    Classes,
    Loadshape;

const
    AVG = -1;
    MAXPHASE = -2;
    MINPHASE = -3;

type

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TStorageController2 = class(TControlClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const StorageController2Name: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(): Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TStorageController2Obj = class(TControlElem)
    PRIVATE

        FkWTarget,
        FkWTargetLow,
        FkWThreshold,
        FpctkWBand,
        FkWBand,
        FpctkWBandLow,
        FkWBandLow,
        HalfkWBand,
        HalfkWBandLow,
//            FPFTarget,                  // Range on this is 0..2 where 1..2 is leading
        TotalWeight,
//            HalfPFBand,
//            FPFBand,
        UpRamptime,
        FlatTime,
        DnrampTime,
        UpPlusFlat,
        UpPlusFlatPlusDn,
        DischargeTriggerTime,
        ChargeTriggerTime,
        pctKWRate,
//            pctkvarRate,
        pctChargeRate,
        LastpctDischargeRate,
        TotalkWCapacity,
        TotalkWhCapacity,
        pctFleetReserve,
        ResetLevel,
        kWNeeded,
        DispFactor: Double;  // for slower convergence

        FStorageNameList: TStringList;
        FleetPointerList: TDSSPointerList;
        SeasonTargets,
        SeasonTargetsLow: Array of Double;
        FWeights: pDoubleArray;
        cBuffer: pComplexArray;    // Complex Array buffer

        FleetListChanged,
        ChargingAllowed,
//            DispatchVars,
        DischargeTriggeredByTime,
        DischargeInhibited,
        OutOfOomph,
        FElementListSpecified,
        Wait4Step,
        FkWBandSpecified: Boolean;  // true if kWBand specified as an absolute value (for use in Follow Discharge Mode to update the target)

        Seasons,
        FleetSize,
        FleetState,
        DischargeMode,
        InhibitHrs,
        ChargeMode,
        FMonPhase,
        CondOffset: Integer;

        YearlyShape: String;  // ='fixed' means no variation  on all the time
        YearlyShapeObj: TLoadShapeObj;  // Shape for this Storage element
        DailyShape: String;  // Daily (24 HR) Storage element shape
        DailyShapeObj: TLoadShapeObj;  // Daily Storage element Shape for this load
        DutyShape: String;  // Duty cycle load shape for changes typically less than one hour
        DutyShapeObj: TLoadShapeObj;  // Shape for this Storage element

        LoadShapeMult: Complex;

           // PROCEDURE SetPctReserve;
        procedure SetAllFleetValues;
        procedure SetFleetkWRate(pctkw: Double);
//            PROCEDURE SetFleetkvarRate(pctkvar:Double);
        procedure SetFleetChargeRate;
        procedure SetFleetToCharge;
        procedure SetFleetToDisCharge;
        procedure SetFleetToIdle;
        procedure SetFleetToExternal;
        procedure SetFleetDesiredState(state: Integer);
        function InterpretMode(Opt: Integer; const S: String): Integer;
        function GetModeString(Opt, Mode: Integer): String;
        function GetkWTotal(var Sum: Double): String;
        function GetkWhTotal(var Sum: Double): String;
        function GetkWhActual: String;
        function GetkWActual: String;

        procedure CalcYearlyMult(Hr: Double);
        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);

        function ReturnSeasonTarget(THigh: Integer): String;
        function ReturnElementsList: String;
        function ReturnWeightsList: String;

        function MakeFleetList: Boolean;
        procedure DoLoadFollowMode();
        procedure DoLoadShapeMode();
        procedure DoTimeMode(Opt: Integer);
        procedure DoScheduleMode();
        procedure DoPeakShaveModeLow();
        procedure PushTimeOntoControlQueue(Code: Integer);
        function NormalizeToTOD(h: Integer; sec: Double): Double;
        procedure GetControlPower(var ControlPower: Complex);
        procedure GetControlCurrent(var ControlCurrent: Double);
//            procedure Set_PFBand(const Value: Double);
        function Get_FleetkW: Double;
        function Get_FleetkWh: Double;
        function Get_FleetkWhRating: Double;
        function Get_FleetReservekWh: Double;

        function Get_DynamicTarget(THigh: Integer): Double;

    PUBLIC

        constructor Create(ParClass: TDSSClass; const StorageController2Name: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData(); OVERRIDE;
        procedure CalcYPrim(); OVERRIDE;    // Always Zero for a StorageController

        procedure Sample(); OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state

        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Get present value of terminal Curr
        procedure GetInjCurrents(Curr: pComplexArray); OVERRIDE;   // Returns Injextion currents

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;

//           Property PFBand   :Double   Read FPFBand  Write  Set_PFBand;
        property FleetkW: Double READ Get_FleetkW;
        property FleetkWh: Double READ Get_FleetkWh;
        property FleetkWhRating: Double READ Get_FleetkWhRating;
        property FleetReservekWh: Double READ Get_FleetReservekWh;

    end;


var
    ActiveStorageController2Obj: TStorageController2Obj;

{--------------------------------------------------------------------------}
implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Storage2,
    Sysutils,
    uCmatrix,
    MathUtil,
    Math,
    Solution,
    Dynamics,
    XYCurve;

const

    propELEMENT = 1;
    propTERMINAL = 2;
    propMONPHASE = 3;
    propKWTARGET = 4;
    propKWTARGETLOW = 5;
    propPCTKWBAND = 6;
    propKWBAND = 7;
    propPCTKWBANDLOW = 8;
    propKWBANDLOW = 9;
//    propPFTARGET      = 7;
//    propPFBAND        = 8;
    propELEMENTLIST = 10;
    propWEIGHTS = 11;
    propMODEDISCHARGE = 12;
    propMODECHARGE = 13;
    propTIMEDISCHARGETRIGGER = 14;
    propTIMECHARGETRIGGER = 15;
    propRATEKW = 16;
//    propRATEKVAR      = 16;
    propRATECHARGE = 17;
    propRESERVE = 18;
    propKWHTOTAL = 19;
    propKWTOTAL = 20;
    propKWHACTUAL = 21;
    propKWACTUAL = 22;
    propKWNEED = 23;
//    propPARTICIPATION = 24;
    propYEARLY = 24;
    propDAILY = 25;
    propDUTY = 26;
    propEVENTLOG = 27;
//    propVARDISPATCH   = 29;
    propINHIBITTIME = 28;
    propTUPRAMP = 29;
    propTFLAT = 30;
    propTDNRAMP = 31;
    propKWTHRESHOLD = 32;
    propDispFactor = 33;
    propRESETLEVEL = 34;
    propSEASONS = 35;
    propSEASONTARGETS = 36;
    propSEASONTARGETSLOW = 37;

    NumPropsThisClass = 37;

//= = = = = = = = = = = = = = DEFINE CONTROL MODE CONSTANTS = = = = = = = = = = = = = = = = = = = = = = = = =

    MODEFOLLOW = 1;
    MODELOADSHAPE = 2;
    MODESUPPORT = 3;
    MODETIME = 4;
    MODEPEAKSHAVE = 5;
    MODESCHEDULE = 6;
    MODEPEAKSHAVELOW = 7;
    CURRENTPEAKSHAVE = 8;
    CURRENTPEAKSHAVELOW = 9;

//= = = = = = = = = = = = = = DEFINE OTHER CONSTANTS = = = = = = = = = = = = = = = = = = = = = = = = =
    RELEASE_INHIBIT = 999;

type
    TStorageObj = TStorage2Obj;

var
    CDoubleOne: Complex;

{--------------------------------------------------------------------------}
constructor TStorageController2.Create;  // Creates superstructure for all StorageController objects
begin
    inherited Create;

    Class_name := 'StorageController';
    DSSClassType := DSSClassType + Storage_CONTROL;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

{--------------------------------------------------------------------------}
destructor TStorageController2.Destroy;

begin
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TStorageController2.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

     // Define Property names

    PropertyName[propELEMENT] := 'Element';
    PropertyName[propTERMINAL] := 'Terminal';
    PropertyName[propMONPHASE] := 'MonPhase';
    PropertyName[propKWTARGET] := 'kWTarget';
    PropertyName[propKWTARGETLOW] := 'kWTargetLow';
    PropertyName[propPCTKWBAND] := '%kWBand';
    PropertyName[propKWBAND] := 'kWBand';
    PropertyName[propPCTKWBANDLOW] := '%kWBandLow';
    PropertyName[propKWBANDLOW] := 'kWBandLow';
//     PropertyName[propPFTARGET]               := 'PFTarget';
//     PropertyName[propPFBAND]                 := 'PFBand';
    PropertyName[propELEMENTLIST] := 'ElementList';
    PropertyName[propWEIGHTS] := 'Weights';
    PropertyName[propMODEDISCHARGE] := 'ModeDischarge';
    PropertyName[propMODECHARGE] := 'ModeCharge';
    PropertyName[propTIMEDISCHARGETRIGGER] := 'TimeDischargeTrigger';
    PropertyName[propTIMECHARGETRIGGER] := 'TimeChargeTrigger';
    PropertyName[propRATEKW] := '%RatekW';
//     PropertyName[propRATEKVAR]               := '%Ratekvar';
    PropertyName[propRATECHARGE] := '%RateCharge';
    PropertyName[propRESERVE] := '%Reserve';
    PropertyName[propKWHTOTAL] := 'kWhTotal';
    PropertyName[propKWTOTAL] := 'kWTotal';
    PropertyName[propKWHACTUAL] := 'kWhActual';
    PropertyName[propKWACTUAL] := 'kWActual';
    PropertyName[propKWNEED] := 'kWneed';
//     PropertyName[propPARTICIPATION]          := '%Participation';
    PropertyName[propYEARLY] := 'Yearly';
    PropertyName[propDAILY] := 'Daily';
    PropertyName[propDUTY] := 'Duty';
    PropertyName[propEVENTLOG] := 'EventLog';
//     PropertyName[propVARDISPATCH]            := 'VarDispatch';
    PropertyName[propINHIBITTIME] := 'InhibitTime';
    PropertyName[propTUPRAMP] := 'Tup';
    PropertyName[propTFLAT] := 'TFlat';
    PropertyName[propTDNRAMP] := 'Tdn';
    PropertyName[propKWTHRESHOLD] := 'kWThreshold';
    PropertyName[propDispFactor] := 'DispFactor';
    PropertyName[propRESETLEVEL] := 'ResetLevel';
    PropertyName[propSEASONS] := 'Seasons';
    PropertyName[propSEASONTARGETS] := 'SeasonTargets';
    PropertyName[propSEASONTARGETSLOW] := 'SeasonTargetsLow';

    PropertyHelp[propELEMENT] :=
        'Full object name of the circuit element, typically a line or transformer, ' +
        'which the control is monitoring. There is no default; Must be specified.';
    PropertyHelp[propTERMINAL] :=
        'Number of the terminal of the circuit element to which the StorageController2 control is connected. ' +
        '1 or 2, typically.  Default is 1. Make sure to select the proper direction on the power for the respective dispatch mode.';
    PropertyHelp[propMONPHASE] :=
        'Number of the phase being monitored or one of {AVG | MAX | MIN} for all phases. Default=MAX. ' +
        'Must be less than the number of phases. Used in PeakShave, Follow, Support and I-PeakShave discharging modes ' +
        'and in PeakShaveLow, I-PeakShaveLow charging modes. For modes based on active power measurements, the value ' +
        'used by the control is the monitored one multiplied by the number of phases of the monitored element.';
    PropertyHelp[propKWTARGET] :=
        'kW/kamps target for Discharging. The Storage element fleet is dispatched to try to hold the power/current in band ' +
        'at least until the Storage is depleted. The selection of power or current depends on the Discharge mode (PeakShave->kW, I-PeakShave->kamps).';
    PropertyHelp[propKWTARGETLOW] :=
        'kW/kamps target for Charging. The Storage element fleet is dispatched to try to hold the power/current in band ' +
        'at least until the Storage is fully charged. The selection of power or current depends on the charge mode (PeakShavelow->kW, I-PeakShavelow->kamps).';
    PropertyHelp[propPCTKWBAND] :=
        'Bandwidth (% of Target kW/kamps) of the dead band around the kW/kamps target value. Default is 2% (+/-1%).' +
        'No dispatch changes are attempted if the power in the monitored terminal stays within this band.';
    PropertyHelp[propKWBAND] :=
        'Alternative way of specifying the bandwidth. (kW/kamps) of the dead band around the kW/kamps target value. Default is 2% of kWTarget (+/-1%).' +
        'No dispatch changes are attempted if the power in the monitored terminal stays within this band.';
    PropertyHelp[propPCTKWBANDLOW] :=
        'Bandwidth (% of kWTargetLow) of the dead band around the kW/kamps low target value. Default is 2% (+/-1%).' +
        'No charging is attempted if the power in the monitored terminal stays within this band.';
    PropertyHelp[propKWBANDLOW] :=
        'Alternative way of specifying the bandwidth. (kW/kamps) of the dead band around the kW/kamps low target value. Default is 2% of kWTargetLow (+/-1%).' +
        'No charging is attempted if the power in the monitored terminal stays within this band.';
//    PropertyHelp[propPFTARGET]          :=
//      'Power Factor target for dispatching the reactive power. Default is 0.96. The reactive power of the storage element fleet is dispatched to try to hold the power factor in band. '+
//      'It is assumed that the storage element inverter can produce kvar up to its kVA limit regardless of storage level.';
//    PropertyHelp[propPFBAND]            :=
//      'Bandwidth of the Target power factor of the monitored element. of the dead band around the kvar target value. Default is 0.04 (+/- 0.02).' +
//      'No dispatch changes of the kvar are attempted If the power factor of the monitored terminal stays within this band.';
    PropertyHelp[propELEMENTLIST] :=
        'Array list of Storage elements to be controlled.  If not specified, all Storage elements in the circuit not presently dispatched by another controller ' +
        'are assumed dispatched by this controller.';
    PropertyHelp[propWEIGHTS] :=
        'Array of proportional weights corresponding to each Storage element in the ElementList. ' +
        'The needed kW or kvar to get back to center band is dispatched to each Storage element according to these weights. ' +
        'Default is to set all weights to 1.0.';
    PropertyHelp[propMODEDISCHARGE] :=
        '{PeakShave* | Follow | Support | Loadshape | Time | Schedule | I-PeakShave} Mode of operation for the DISCHARGE FUNCTION of this controller. ' +
        CRLF + CRLF + 'In PeakShave mode (Default), the control attempts to discharge Storage to keep power in the monitored element below the kWTarget. ' +
        CRLF + CRLF + 'In Follow mode, the control is triggered by time and resets the kWTarget value to the present monitored element power. ' +
        'It then attempts to discharge Storage to keep power in the monitored element below the new kWTarget. See TimeDischargeTrigger.' +
        CRLF + CRLF + 'In Support mode, the control operates oppositely of PeakShave mode: Storage is discharged to keep kW power output up near the target. ' +
        CRLF + CRLF + 'In Loadshape mode, both charging and discharging precisely follows the per unit loadshape. ' +
        'Storage is discharged when the loadshape value is positive. ' +
        CRLF + CRLF + 'In Time mode, the Storage discharge is turned on at the specified %RatekW at the specified discharge trigger time in fractional hours.' +
        CRLF + CRLF + 'In Schedule mode, the Tup, TFlat, and Tdn properties specify the up ramp duration, flat duration, and down ramp duration for the schedule. ' +
        'The schedule start time is set by TimeDischargeTrigger and the rate of discharge for the flat part is determined by %RatekW.' +
        CRLF + CRLF + 'In I-PeakShave mode, the control attempts to discharge Storage to keep current in the monitored element below the target given in k-amps ' +
        '(thousands of amps), when this control mode is active, the property kWTarget will be expressed in k-amps. ';
    PropertyHelp[propMODECHARGE] :=
        '{Loadshape | Time* | PeakShaveLow | I-PeakShaveLow} Mode of operation for the CHARGE FUNCTION of this controller. ' +
        CRLF + CRLF + 'In Loadshape mode, both charging and discharging precisely follows the per unit loadshape. ' +
        'Storage is charged when the loadshape value is negative. ' +
        CRLF + CRLF + 'In Time mode, the Storage charging FUNCTION is triggered at the specified %RateCharge at the specified charge trigger time in fractional hours.' +
        CRLF + CRLF + 'In PeakShaveLow mode, the charging operation will charge the Storage fleet when the power at a' +
        'monitored element is below a specified KW target (kWTarget_low). The Storage will charge as much power as necessary to keep the power within the deadband around kWTarget_low.' +
        CRLF + CRLF + 'In I-PeakShaveLow mode, the charging operation will charge the Storage fleet when the current (Amps) at a' +
        'monitored element is below a specified amps target (kWTarget_low). The Storage will charge as much power as necessary to keep the amps within the deadband around kWTarget_low. ' +
        'When this control mode is active, the property kWTarget_low will be expressed in k-amps and all the other parameters will be adjusted to match the amps (current) control criteria.';
    PropertyHelp[propTIMEDISCHARGETRIGGER] :=
        'Default time of day (hr) for initiating Discharging of the fleet. During Follow or Time mode discharging is triggered at a fixed time ' +
        'each day at this hour. If Follow mode, Storage will be discharged to attempt to hold the load at or below the power level at the time of triggering. ' +
        'In Time mode, the discharge is based on the %RatekW property value. ' +
        'Set this to a negative value to ignore. Default is 12.0 for Follow mode; otherwise it is -1 (ignored). ';
    PropertyHelp[propTIMECHARGETRIGGER] :=
        'Default time of day (hr) for initiating charging in Time control mode. Set this to a negative value to ignore. Default is 2.0.  (0200).' +
        'When this value is >0 the Storage fleet is set to charging at this time regardless of other control criteria to make sure Storage is ' +
        'topped off for the next discharge cycle.';
    PropertyHelp[propRATEKW] :=
        'Sets the kW discharge rate in % of rated capacity for each element of the fleet. Applies to TIME control mode, SCHEDULE mode, or anytime discharging is triggered ' +
        'by time.';
//    PropertyHelp[propRATEKVAR]            :=
//      'Sets the kvar discharge rate in % of rated capacity for each element of the fleet. Applies to TIME control mode or anytime discharging is triggered ' +
//      'by time.' ;
    PropertyHelp[propRATECHARGE] :=
        'Sets the kW charging rate in % of rated capacity for each element of the fleet. Applies to TIME control mode and anytime charging mode is ' +
        'entered due to a time trigger.';
    PropertyHelp[propRESERVE] :=
        'Use this property to change the % reserve for each Storage element under control of this controller. This might be used, for example, to ' +
        'allow deeper discharges of Storage or in case of emergency operation to use the remainder of the Storage element.';
    PropertyHelp[propKWHTOTAL] :=
        '(Read only). Total rated kWh energy Storage capacity of Storage elements controlled by this controller.';
    PropertyHelp[propKWTOTAL] :=
        '(Read only). Total rated kW power capacity of Storage elements controlled by this controller.';
    PropertyHelp[propKWHACTUAL] :=
        '(Read only). Actual kWh stored of all controlled Storage elements. ';
    PropertyHelp[propKWACTUAL] :=
        '(Read only). Actual kW output of all controlled Storage elements. ';
    PropertyHelp[propKWNEED] :=
        '(Read only). KW needed to meet target.';
//    PropertyHelp[propPARTICIPATION]       :=
//      'Participation factor, %. Default = 100.';
    PropertyHelp[propYEARLY] :=
        'Dispatch loadshape object, If any, for Yearly solution Mode.';
    PropertyHelp[propDAILY] :=
        'Dispatch loadshape object, If any, for Daily solution mode.';
    PropertyHelp[propDUTY] :=
        'Dispatch loadshape object, If any, for Dutycycle solution mode.';
    PropertyHelp[propEVENTLOG] :=
        '{Yes/True | No/False} Default is No. Log control actions to Eventlog.';
//    PropertyHelp[propVARDISPATCH]         :=
//      '{Yes/True | No/False} Default is No. Flag to indicate whether or not to disatch vars as well as watts.';
    PropertyHelp[propINHIBITTIME] :=
        'Hours (integer) to inhibit Discharging after going into Charge mode. Default is 5.';
    PropertyHelp[propTUPRAMP] := 'Duration, hrs, of upramp part for SCHEDULE mode. Default is 0.25.';
    PropertyHelp[propTFLAT] := 'Duration, hrs, of flat part for SCHEDULE mode. Default is 2.0.';
    PropertyHelp[propTDNRAMP] := 'Duration, hrs, of downramp part for SCHEDULE mode. Default is 0.25.';
    PropertyHelp[propKWTHRESHOLD] := 'Threshold, kW, for Follow mode. kW has to be above this value for the Storage element ' +
        'to be dispatched on. Defaults to 75% of the kWTarget value. Must reset this property after ' +
        'setting kWTarget if you want a different value.';
    PropertyHelp[propDispFactor] := 'Defaults to 1 (disabled). Set to any value between 0 and 1 to enable this parameter.' + CRLF + CRLF + 'Use this parameter to reduce the amount of power requested by the controller in each control iteration. ' +
        'It can be useful when maximum control iterations are exceeded due to numerical instability such as ' +
        'fleet being set to charging and idling in subsequent control iterations (check the Eventlog). ';
    PropertyHelp[propRESETLEVEL] := 'The level of charge required for allowing the storage to discharge again after reaching ' +
        'the reserve storage level. After reaching this level, the storage control  will not allow ' +
        'the storage device to discharge, forcing the storage to charge. Once the storage reaches this' +
        'level, the storage will be able to discharge again. This value is a number between 0.2 and 1';
    PropertyHelp[propSEASONS] := 'With this property the user can' +
        ' specify the number of targets to be used by the controller using the list given at "SeasonTargets"/' +
        '"SeasonTargetsLow", which can be used to dynamically adjust the storage controller during a QSTS' +
        ' simulation. The default value is 1. This property needs to be defined before defining SeasonTargets/SeasonTargetsLow.';
    PropertyHelp[propSEASONTARGETS] := 'An array of doubles specifying the targets to be used during a QSTS simulation. These targets will take effect' +
        ' only if SeasonRating=true. The number of targets cannot exceed the number of seasons defined at the SeasonSignal.' +
        'The difference between the targets defined at SeasonTargets and SeasonTargetsLow is that SeasonTargets' +
        ' applies to discharging modes, while SeasonTargetsLow applies to charging modes.';
    PropertyHelp[propSEASONTARGETSLOW] := 'An array of doubles specifying the targets to be used during a QSTS simulation. These targets will take effect' +
        ' only if SeasonRating=true. The number of targets cannot exceed the number of seasons defined at the SeasonSignal.' +
        'The difference between the targets defined at SeasonTargets and SeasonTargetsLow is that SeasonTargets' +
        ' applies to discharging modes, while SeasonTargetsLow applies to charging modes.';


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

{--------------------------------------------------------------------------}
function TStorageController2.NewObject(const ObjName: String): Integer;
begin
    // Make a new StorageController and add it to StorageController class list
    with ActiveCircuit do
    begin
        ActiveCktElement := TStorageController2Obj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

{--------------------------------------------------------------------------}
function TStorageController2.Edit(): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    i: Integer;
    casemult: Double;

begin

  // continue parsing with contents of Parser
    ActiveStorageController2Obj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveStorageController2Obj;

    Result := 0;

    with ActiveStorageController2Obj do
    begin

        ParamPointer := 0;
        ParamName := Parser.NextParam;
        Param := Parser.StrValue;
        while Length(Param) > 0 do
        begin
            if Length(ParamName) = 0 then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[ParamPointer] := Param;

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 14407);
                propELEMENT:
                    ElementName := lowercase(param);
                propTERMINAL:
                    ElementTerminal := Parser.IntValue;
                propMONPHASE:
                    if CompareTextShortest(param, 'avg') = 0 then
                        FMonPhase := AVG
                    else
                    if CompareTextShortest(param, 'max') = 0 then
                        FMonPhase := MAXPHASE
                    else
                    if CompareTextShortest(param, 'min') = 0 then
                        FMonPhase := MINPHASE
                    else
                        FMonPhase := max(1, Parser.IntValue);
                propKWTARGET:
                    FkWTarget := Parser.DblValue;
                propKWTARGETLOW:
                    FkWTargetLow := Parser.DblValue;
                propPCTKWBAND:
                    FpctkWBand := Parser.DblValue;
                propKWBAND:
                    FkWBand := Parser.DblValue;
                propPCTKWBANDLOW:
                    FpctkWBandLow := Parser.DblValue;
                propKWBANDLOW:
                    FkWBandLow := Parser.DblValue;
//            propPFTARGET: FPFTarget        := ConvertPFToPFRange2(Parser.DblValue);
//            propPFBAND:   FPFBand          := Parser.DblValue;
                propELEMENTLIST:
                    InterpretTStringListArray(Param, FStorageNameList);
                propWEIGHTS:
                begin
                    FleetSize := FStorageNameList.count;
                    if FleetSize > 0 then
                    begin
                        Reallocmem(FWeights, Sizeof(FWeights^[1]) * FleetSize);
                        FleetSize := InterpretDblArray(Param, FleetSize, FWeights);
                    end;
                end;
                propMODEDISCHARGE:
                    DisChargeMode := InterpretMode(propMODEDISCHARGE, Param);
                propMODECHARGE:
                    ChargeMode := InterpretMode(propMODECHARGE, Param);
                propTIMEDISCHARGETRIGGER:
                    DischargeTriggerTime := Parser.DblValue;
                propTIMECHARGETRIGGER:
                    ChargeTriggerTime := Parser.DblValue;
                propRATEKW:
                    pctkWRate := Parser.DblValue;
//            propRATEKVAR:    pctkvarRate    := Parser.DblValue;
                propRATECHARGE:
                    pctChargeRate := Parser.DblValue;
                propRESERVE:
                    pctFleetReserve := Parser.DblValue;
                propKWHTOTAL: ;  // Do nothing (Read ONly)
                propKWTOTAL: ;  // Do nothing (Read ONly)
                propKWHACTUAL: ;  // Do nothing (Read ONly)
                propKWACTUAL: ;  // Do nothing (Read ONly)
                propKWNEED: ;  // Do nothing (Read ONly)
//            propPARTICIPATION: ;
                propYEARLY:
                    YearlyShape := Param;
                propDAILY:
                    DailyShape := Param;
                propDUTY:
                    DutyShape := Param;
                propEVENTLOG:
                    ShowEventLog := InterpretYesNo(Param);
//            propVARDISPATCH: DispatchVars := InterpretYesNo(Param);
                propINHIBITTIME:
                    Inhibithrs := Max(1, Parser.IntValue);  // >=1
                propTUPRAMP:
                    UpRamptime := Parser.DblValue;
                propTFLAT:
                    FlatTime := Parser.DblValue;
                propTDNRAMP:
                    DnrampTime := Parser.DblValue;
                propKWTHRESHOLD:
                    FkWThreshold := Parser.DblValue;
                propDispFactor:
                    if (Parser.DblValue > 1.0) or (Parser.DblValue <= 0.0) then
                        DispFactor := 1.0
                    else
                        DispFactor := Parser.DblValue;
                propRESETLEVEL:
                    ResetLevel := Parser.DblValue;
                propSEASONS:
                    Seasons := Parser.IntValue;
                propSEASONTARGETS:
                begin
                    if Seasons > 1 then
                    begin
                        setlength(SeasonTargets, Seasons);
                        Seasons := InterpretDblArray(Param, Seasons, Pointer(SeasonTargets));
                    end;
                end;
                propSEASONTARGETSLOW:
                begin
                    if Seasons > 1 then
                    begin
                        setlength(SeasonTargetsLow, Seasons);
                        Seasons := InterpretDblArray(Param, Seasons, Pointer(SeasonTargetsLow));
                    end;
                end;

            else
           // Inherited parameters
                ClassEdit(ActiveStorageController2Obj, ParamPointer - NumPropsthisClass)
            end;

         // Side effects of setting properties above

            case ParamPointer of
                propKWTARGET:
                begin
                    if DischargeMode = CURRENTPEAKSHAVE then  // evaluates the discharging mode to apply
                        Casemult := 1000.0                 // a compensation value (for kamps)
                    else
                        Casemult := 1.0;

                    FkWThreshold := FkWTarget * 0.75 * Casemult;

                    HalfkWBand := FpctkWBand / 200.0 * FkWTarget * Casemult;
                    FkWBand := 2.0 * HalfkWBand;
                    FpctkWBand := FkWBand / FkWTarget * 100.0; // sync FpctkWBand

                end;
                propPCTKWBAND:
                begin
                    if DischargeMode = CURRENTPEAKSHAVE then  // evaluates the discharging mode to apply
                        Casemult := 1000.0                 // a compensation value (for kamps)
                    else
                        Casemult := 1.0;

                    HalfkWBand := FpctkWBand / 200.0 * FkWTarget * Casemult;
                    FkWBand := 2.0 * HalfkWBand;
                    FkWBandSpecified := FALSE;

                end;
                propKWBAND:
                begin
                    if DischargeMode = CURRENTPEAKSHAVE then  // evaluates the discharging mode to apply
                        Casemult := 1000.0                 // a compensation value (for kamps)
                    else
                        Casemult := 1.0;

                    HalfkWBand := FkWBand / 2.0 * Casemult;
                    FpctkWBand := FkWBand / FkWTarget * 100.0; // sync FpctkWBand
                    FkWBandSpecified := TRUE;
                end;
                propKWTARGETLOW,
                propPCTKWBANDLOW:
                begin
                    if ChargeMode = CURRENTPEAKSHAVELOW then  // evaluates the charging mode to apply
                        Casemult := 1000.0                 // a compensation value (for kamps)
                    else
                        Casemult := 1.0;

                    HalfkWBandLow := FpctkWBandLow / 200.0 * FkWTargetLow * Casemult;
                    FkWBandLow := HalfkWBandLow * 2.0;
                end;
                propKWBANDLOW:
                begin
                    if ChargeMode = CURRENTPEAKSHAVELOW then  // evaluates the charging mode to apply
                        Casemult := 1000.0                 // a compensation value (for kamps)
                    else
                        Casemult := 1.0;

                    HalfkWBandLow := FkWBandLow / 2.0 * Casemult;
                    FpctkWBand := FkWBandLow / FkWTarget * 100.0; // sync FpctkWBandLow
                end;
//            propPFBAND: HalfPFBand := FPFBand / 2.0;
                propMODEDISCHARGE:
                    if DischargeMode = MODEFOLLOW then
                        DischargeTriggerTime := 12.0; // Noon

                propMONPHASE:
                    if FMonPhase > FNphases then
                    begin
                        DoSimpleMsg(Format('Error: Monitored phase(%d) must be less than or equal to number of phases(%d). ', [FMonPhase, FNphases]), 35302);
                        FMonPhase := 1;
                    end;

                propELEMENTLIST:
                begin   // levelize the list
                    FleetPointerList.Clear;  // clear this for resetting on first sample
                    FleetListChanged := TRUE;
                    FElementListSpecified := TRUE;
                    FleetSize := FStorageNameList.count;
                // Realloc weights to be same size as possible number of storage elements
                    Reallocmem(FWeights, Sizeof(FWeights^[1]) * FleetSize);
                    for i := 1 to FleetSize do
                        FWeights^[i] := 1.0;
                end;
                propYEARLY:
                begin
                    YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
                    if YearlyShapeObj = NIL then
                        DoSimpleMsg('Yearly loadshape "' + YearlyShape + '" not found.', 14404);
                end;
                propDAILY:
                begin
                    DailyShapeObj := LoadShapeClass.Find(DailyShape);
                    if DailyShapeObj = NIL then
                        DoSimpleMsg('Daily loadshape "' + DailyShape + '" not found.', 14405);
                end;
                propDUTY:
                begin
                    DutyShapeObj := LoadShapeClass.Find(DutyShape);
                    if DutyShapeObj = NIL then
                        DoSimpleMsg('Dutycycle loadshape "' + DutyShape + '" not found.', 14406);
                end

            else

            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        RecalcElementData();
    end;

end;

{--------------------------------------------------------------------------}
function TStorageController2.MakeLike(const StorageController2Name: String): Integer;
var
    OtherStorageController: TStorageController2Obj;
    i: Integer;
begin
    Result := 0;
   {See If we can find this StorageController name in the present collection}
    OtherStorageController := Find(StorageController2Name);
    if OtherStorageController <> NIL then
        with ActiveStorageController2Obj do
        begin

            NPhases := OtherStorageController.Fnphases;
            NConds := OtherStorageController.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherStorageController.ElementName;
            ControlledElement := OtherStorageController.ControlledElement;  // Pointer to target circuit element
            MonitoredElement := OtherStorageController.MonitoredElement;  // Pointer to target circuit element
            ElementTerminal := OtherStorageController.ElementTerminal;
            FMonPhase := OtherStorageController.FMonPhase;
            CondOffset := OtherStorageController.CondOffset;

            FkWTarget := OtherStorageController.FkWTarget;
            FkWTargetLow := OtherStorageController.FkWTargetLow;
            FkWThreshold := OtherStorageController.FkWThreshold;

            DispFactor := OtherStorageController.DispFactor;

            FpctkWBand := OtherStorageController.FpctkWBand;
            FkWBand := OtherStorageController.FkWBand;
            FpctkWBandLow := OtherStorageController.FpctkWBandLow;
            FkWBandLow := OtherStorageController.FkWBandLow;
//        FPFTarget             := OtherStorageController.FPFTarget;
//        FPFBand               := OtherStorageController.FPFBand;
//        HalfPFBand            := OtherStorageController.HalfPFBand;
            ResetLevel := OtherStorageController.ResetLevel;

            FkWBandSpecified := OtherStorageController.FkWBandSpecified;
            FStorageNameList.Clear;
            for i := 1 to OtherStorageController.FStorageNameList.Count do
                FStorageNameList.Add(OtherStorageController.FStorageNameList.Strings[i - 1]);

            FleetSize := FStorageNameList.count;
            if FleetSize > 0 then
            begin
                Reallocmem(FWeights, Sizeof(FWeights^[1]) * FleetSize);
                for i := 1 to FleetSize do
                    FWeights^[i] := OtherStorageController.FWeights^[i];
            end;

            DisChargeMode := OtherStorageController.DisChargeMode;
            ChargeMode := OtherStorageController.ChargeMode;
            DischargeTriggerTime := OtherStorageController.DischargeTriggerTime;
            ChargeTriggerTime := OtherStorageController.ChargeTriggerTime;
            pctkWRate := OtherStorageController.pctkWRate;
//        pctkvarRate           := OtherStorageController.pctkvarRate;
            pctChargeRate := OtherStorageController.pctChargeRate;
            pctFleetReserve := OtherStorageController.pctFleetReserve;
            YearlyShape := OtherStorageController.YearlyShape;
            DailyShape := OtherStorageController.DailyShape;
            DutyShape := OtherStorageController.DutyShape;
//        DispatchVars          := OtherStorageController.DispatchVars;
            ShowEventLog := OtherStorageController.ShowEventLog;
            Inhibithrs := OtherStorageController.Inhibithrs;

            UpRamptime := OtherStorageController.UpRamptime;
            FlatTime := OtherStorageController.FlatTime;
            DnrampTime := OtherStorageController.DnrampTime;

            Seasons := OtherStorageController.Seasons;
            if Seasons > 1 then
            begin
                setlength(SeasonTargets, Seasons);
                setlength(SeasonTargetsLow, Seasons);
                for i := 0 to (Seasons - 1) do
                begin
                    SeasonTargets[i] := OtherStorageController.SeasonTargets[i];
                    SeasonTargetsLow[i] := OtherStorageController.SeasonTargetsLow[i];
                end;
            end;


        //**** fill in private properties

            for i := 1 to ParentClass.NumProperties do
           // Skip Read only properties
                case i of
                    propKWHTOTAL: ; {Do Nothing}
                    propKWTOTAL: ; {Do Nothing}
                    propKWHACTUAL: ; {Do Nothing}
                    propKWACTUAL: ; {Do Nothing}
                    propKWNEED: ; {Do Nothing}
                else
                    PropertyValue[i] := OtherStorageController.PropertyValue[i];
                end;


        end
    else
        DoSimpleMsg('Error in StorageController2 MakeLike: "' + StorageController2Name + '" Not Found.', 370);

end;


{==========================================================================}
{                    TStorageControllerObj                                           }
{==========================================================================}


{--------------------------------------------------------------------------}
constructor TStorageController2Obj.Create(ParClass: TDSSClass; const StorageController2Name: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(StorageController2Name);
    DSSObjType := ParClass.DSSClassType;

    NPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors

    ElementName := '';
    ControlledElement := NIL;    // not used in this control
    ElementTerminal := 1;
    MonitoredElement := NIL;
    FMonPhase := MAXPHASE;
    cBuffer := NIL; // Complex buffer

    FStorageNameList := TSTringList.Create;
    FWeights := NIL;
    FleetPointerList := TDSSPointerList.Create(20);  // Default size and increment
    FleetSize := 0;
    FleetState := STORE_IDLING;
    FkWTarget := 8000.0;
    FkWTargetLow := 4000.0;
    FkWThreshold := 6000.0;
    DispFactor := 1.0;
    FpctkWBand := 2.0;
    FpctkWBandLow := 2.0;
    HalfkWBand := FpctkWBand / 200.0 * FkWTarget;
    HalfkWBandLow := FpctkWBandLow / 200.0 * FkWTargetLow;
    FkWBand := HalfkWBand * 2.0;
    FkWBandLow := HalfkWBandLow * 2.0;
    TotalWeight := 1.0;

//     FPFTarget                := 0.96;
//     FPFBand                  := 0.04;
//     HalfPFBand               := FPFBand / 2.0;
    kWNeeded := 0.0;

    DischargeMode := MODEPEAKSHAVE;
    ChargeMode := MODETIME;

    DischargeTriggerTime := -1.0;  // disabled
    ChargeTriggerTime := 2.0;   // 2 AM
    FElementListSpecified := FALSE;
    FleetListChanged := TRUE;  // force building of list
    FkWBandSpecified := FALSE;  // adopt pctkWBand by default

    pctkWRate := 20.0;
//     pctkvarRate              := 20.0;
    pctChargeRate := 20.0;
    pctFleetReserve := 25.0;

    ShowEventLog := FALSE;
//     DispatchVars             := FALSE;
    DischargeTriggeredByTime := FALSE;
    DischargeInhibited := FALSE;
    OutOfOomph := FALSE;
    InhibitHrs := 5;   // No. Hours to inhibit discharging after going into charge mode

    UpRamptime := 0.25; // hr
    FlatTime := 2.0;
    DnrampTime := 0.25;
    LastpctDischargeRate := 0.0;
    Wait4Step := FALSE;     // for sync discharge with charge when there is a transition
    ResetLevel := 0.8;
    Seasons := 1;         // For dynamic targets
    setlength(SeasonTargets, 1);
    SeasonTargets[0] := FkWTarget;
    setlength(SeasonTargetsLow, 1);
    SeasonTargetsLow[0] := FkWTargetLow;


    InitPropertyValues(0);

end;

destructor TStorageController2Obj.Destroy;
begin
    ElementName := '';
    YearlyShape := '';
    DailyShape := '';
    DutyShape := '';

    if Assigned(cBuffer) then
        ReallocMem(cBuffer, 0);

(*    Don't Do this here!! Disposes of actual object;
       YearlyShapeObj.Free;
       DailyShapeObj.Free;
       DutyShapeObj.Free;
*)


    FleetPointerList.Free;
    FStorageNameList.Free;

    inherited Destroy;
end;

//----------------------------------------------------------------------------
procedure TStorageController2Obj.InitPropertyValues(ArrayOffset: Integer);
begin


    PropertyValue[propELEMENT] := '';
    PropertyValue[propTERMINAL] := '1';
    PropertyValue[propMONPHASE] := 'MAX';
    PropertyValue[propKWTARGET] := '8000';
    PropertyValue[propKWTARGETLOW] := '4000';
    PropertyValue[propPCTKWBAND] := '2';
    PropertyValue[propPCTKWBANDLOW] := '2';
//     PropertyValue[propPFTARGET]             :='.96';
//     PropertyValue[propPFBAND]               :='.04';
    PropertyValue[propELEMENTLIST] := '';
    PropertyValue[propWEIGHTS] := '';
    PropertyValue[propMODEDISCHARGE] := 'Follow';
    PropertyValue[propMODECHARGE] := 'Time';
    PropertyValue[propTIMEDISCHARGETRIGGER] := '-1';
    PropertyValue[propTIMECHARGETRIGGER] := '2';
    PropertyValue[propRATEKW] := '20';
//     PropertyValue[propRATEKVAR]             :='20';
    PropertyValue[propRATECHARGE] := '20';
    PropertyValue[propRESERVE] := '25';
    PropertyValue[propKWHTOTAL] := '';
    PropertyValue[propKWTOTAL] := '';
    PropertyValue[propKWACTUAL] := '';
    PropertyValue[propKWNEED] := '';
//     PropertyValue[propPARTICIPATION]        :='';
    PropertyValue[propYEARLY] := '';
    PropertyValue[propDAILY] := '';
    PropertyValue[propDUTY] := '';
    PropertyValue[propEVENTLOG] := 'No';
    PropertyValue[propINHIBITTIME] := '5';
    PropertyValue[propTUPRAMP] := '0.25';
    PropertyValue[propTFLAT] := '2.0';
    PropertyValue[propTDNRAMP] := '0.25';
    PropertyValue[propKWTHRESHOLD] := '4000';
    PropertyValue[propDispFactor] := '1.0';
    PropertyValue[propRESETLEVEL] := '0.8';
    PropertyValue[propSEASONS] := '1';
    PropertyValue[propSEASONTARGETS] := '[8000,]';
    PropertyValue[propSEASONTARGETSLOW] := '[4000,]';


    inherited  InitPropertyValues(NumPropsThisClass);

end;

function TStorageController2Obj.GetPropertyValue(Index: Integer): String;
begin
    Result := '';
    case Index of

        propMONPHASE:
            if FMonPhase = AVG then
                Result := 'AVG'
            else
            if FMonPhase = MAXPHASE then
                Result := 'MAX'
            else
            if FMonPhase = MINPHASE then
                Result := 'MIN'
            else
                Result := Format('%d', [FMonPhase]);
        propKWTARGET:
            Result := Format('%-.6g', [FkWTarget]);
        propKWTARGETLOW:
            Result := Format('%-.6g', [FkWTargetLow]);
        propPCTKWBAND:
            Result := Format('%-.6g', [FpctkWBand]);
        propKWBAND:
            Result := Format('%-.6g', [FkWBand]);
        propPCTKWBANDLOW:
            Result := Format('%-.6g', [FpctkWBandLow]);
        propKWBANDLOW:
            Result := Format('%-.6g', [FkWBandLow]);
//          propPFTARGET             : Result := Format('%-.6g',[ConvertPFRange2ToPF(FPFTarget)]);
//          propPFBAND               : Result := Format('%-.6g',[FPFBand]);
        propELEMENTLIST:
            Result := ReturnElementsList;
        propWEIGHTS:
            Result := ReturnWeightsList;
        propMODEDISCHARGE:
            Result := GetModeString(propMODEDISCHARGE, DischargeMode);
        propMODECHARGE:
            Result := GetModeString(propMODECHARGE, ChargeMode);
        propTIMEDISCHARGETRIGGER:
            Result := Format('%.6g', [DisChargeTriggerTime]);
        propTIMECHARGETRIGGER:
            Result := Format('%.6g', [ChargeTriggerTime]);
        propRATEKW:
            Result := Format('%-.8g', [pctkWRate]);
//          propRATEKVAR             : Result := Format('%-.8g',[pctkvarRate]);
        propRATECHARGE:
            Result := Format('%-.8g', [pctChargeRate]);
        propRESERVE:
            Result := Format('%-.8g', [pctFleetReserve]);
        propKWHTOTAL:
            Result := GetkWhTotal(TotalkWhCapacity);
        propKWTOTAL:
            Result := GetkWTotal(TotalkWCapacity);
        propKWHACTUAL:
            Result := GetkWhActual;
        propKWACTUAL:
            Result := GetkWActual;
        propKWNEED:
            Result := Format('%-.6g', [kWNeeded]);
          {propPARTICIPATION        : Result := PropertyValue[Index]; }
        propYEARLY:
            Result := YearlyShape;
        propDAILY:
            Result := DailyShape;
        propDUTY:
            Result := DutyShape;
        propEVENTLOG:
            Result := StrYorN(ShowEventLog);
//          propVARDISPATCH          : If DispatchVars Then Result := 'Yes' Else Result := 'No';
        propINHIBITTIME:
            Result := Format('%d', [InhibitHrs]);
        propTUPRAMP:
            Result := Format('%.6g', [UpRamptime]);
        propTFLAT:
            Result := Format('%.6g', [FlatTime]);
        propTDNRAMP:
            Result := Format('%.6g', [DnrampTime]);
        propKWTHRESHOLD:
            Result := Format('%.6g', [FkWThreshold]);
        propDispFactor:
            Result := Format('%.6g', [DispFactor]);
        propRESETLEVEL:
            Result := Format('%.6g', [ResetLevel]);
        propSEASONS:
            Result := Format('%d', [seasons]);
        propSEASONTARGETS:
            Result := ReturnSeasonTarget(1);
        propSEASONTARGETSLOW:
            Result := ReturnSeasonTarget(0);

    else  // take the generic handler
        Result := inherited GetPropertyValue(index);

    end;
end;

function TStorageController2Obj.Get_FleetkW: Double;

var
    pStorage: TStorageObj;
    i: Integer;
begin
    Result := 0.0;
    for I := 1 to FleetPointerList.Count do
    begin
        pStorage := FleetPointerList.Get(i);
        Result := Result + pStorage.PresentkW;
    end;
end;

function TStorageController2Obj.Get_FleetkWh: Double;
var
    pStorage: TStorageObj;
    i: Integer;
begin
    Result := 0.0;
    for I := 1 to FleetPointerList.Count do
    begin
        pStorage := FleetPointerList.Get(i);
        Result := Result + pStorage.StorageVars.kWhStored;
    end;
end;

function TStorageController2Obj.Get_FleetkWhRating: Double;
var
    pStorage: TStorageObj;
    i: Integer;
begin
    Result := 0.0;
    for I := 1 to FleetPointerList.Count do
    begin
        pStorage := FleetPointerList.Get(i);
        Result := Result + pStorage.StorageVars.kWhRating;
    end;
end;

function TStorageController2Obj.Get_FleetReservekWh: Double;
var
    pStorage: TStorageObj;
    i: Integer;
begin
    Result := 0.0;
    for I := 1 to FleetPointerList.Count do
    begin
        pStorage := FleetPointerList.Get(i);
        Result := Result + pStorage.StorageVars.kWhReserve;
    end;

end;

{--------------------------------------------------------------------------}
procedure TStorageController2Obj.RecalcElementData();

// Recalculate critical element values after changes have been made

var
    DevIndex: Integer;

begin

        {Check for existence of monitored element}

    Devindex := GetCktElementIndex(ElementName); // Global FUNCTION
    if DevIndex > 0 then
    begin
        MonitoredElement := ActiveCircuit.CktElements.Get(DevIndex);
        if ElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg('StorageController2: "' + Name + '"',
                'Terminal no. "' + '" Does not exist.',
                'Re-specify terminal no.', 371);
        end
        else
        begin
            Nphases := MonitoredElement.Nphases;
            NConds := FNphases;

                 // Sets name of i-th terminal's connected bus in StorageController's buslist
            Setbus(1, MonitoredElement.GetBus(ElementTerminal));

                 // Allocate a buffer bigenough to hold everything from the monitored element
            ReAllocMem(cBuffer, SizeOF(cBuffer^[1]) * MonitoredElement.Yorder);
            CondOffset := (ElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling
        end;
    end
    else
        DoSimpleMsg('Monitored Element in StorageController2.' + Name + ' Does not exist:"' + ElementName + '"', 372);

    if FleetListChanged then
        if not MakeFleetList then
            DoSimpleMsg('No unassigned Storage Elements found to assign to StorageController.' + Name, 37201);

    GetkWTotal(TotalkWCapacity);
    GetkWhTotal(TotalkWhCapacity);

    if FleetSize > 0 then
    begin
        SetFleetToExternal;
        SetAllFleetValues;
    end;

    UpPlusFlat := UpRampTime + FlatTime;
    UpPlusFlatPlusDn := UpPlusFlat + DnRampTime;

end;

procedure TStorageController2Obj.MakePosSequence();
begin
    if MonitoredElement <> NIL then
    begin
        Nphases := MonitoredElement.NPhases;
        Nconds := FNphases;
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    // Allocate a buffer bigenough to hold everything from the monitored element
        ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder);
        CondOffset := (ElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling
    end;
    inherited;
end;

{--------------------------------------------------------------------------}
procedure TStorageController2Obj.CalcYPrim();
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);


end;


{--------------------------------------------------------------------------}
procedure TStorageController2Obj.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;

procedure TStorageController2Obj.GetInjCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

function TStorageController2Obj.GetkWActual: String;
begin
    Result := Format('%-.8g', [FleetkW]);
end;

function TStorageController2Obj.GetkWhActual: String;
begin
    Result := Format('%-.8g', [FleetkWh]);
end;

function TStorageController2Obj.GetkWhTotal(var Sum: Double): String;
var
    pStorage: TStorageObj;
    i: Integer;

begin
    Sum := 0.0;
    for i := 1 to FleetPointerList.Count do
    begin
        pStorage := FleetPointerList.Get(i);
        sum := sum + pStorage.StorageVars.kWhRating;
    end;
    Result := Format('%-.8g', [sum]);
end;

function TStorageController2Obj.GetkWTotal(var Sum: Double): String;
var
    pStorage: TStorageObj;
    i: Integer;

begin
    Sum := 0.0;
    for i := 1 to FleetPointerList.Count do
    begin
        pStorage := FleetPointerList.Get(i);
        sum := sum + pStorage.StorageVars.kWRating;
    end;
    Result := Format('%-.8g', [sum]);
end;

function TStorageController2Obj.GetModeString(Opt, Mode: Integer): String;
begin
    Result := '';
    case Opt of
        propMODEDISCHARGE:
            case Mode of
                MODEFOLLOW:
                    Result := 'Follow';
                MODELOADSHAPE:
                    Result := 'Loadshape';
                MODESUPPORT:
                    Result := 'Support';
                MODETIME:
                    Result := 'Time';
                MODEPEAKSHAVE:
                    Result := 'Peakshave';
                CURRENTPEAKSHAVE:
                    Result := 'I-Peakshave';
            else
                Result := 'UNKNOWN'
            end;
        propMODECHARGE:
            case Mode of
                   // 1: Result := 'Follow';
                MODELOADSHAPE:
                    Result := 'Loadshape';
                  //  3: Result := 'Support';
                MODETIME:
                    Result := 'Time';
                MODEPEAKSHAVELOW:
                    Result := 'PeakshaveLow';
                CURRENTPEAKSHAVELOW:
                    Result := 'I-PeakShaveLow';
            else
                Result := 'UNKNOWN'
            end;
    else
        DoSimpleMsg('Unknown Charge/Discharge designation', 14401);
    end;
end;

{--------------------------------------------------------------------------}
procedure TStorageController2Obj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;

    if Complete then
    begin
        Writeln(F);
    end;

end;

{--------------------------------------------------------------------------}
procedure TStorageController2Obj.DoPendingAction(const Code, ProxyHdl: Integer);
begin

        {
           Release  the discharge inhibit .
           Do nothing for other codes
        }

    if (Code = RELEASE_INHIBIT) and (DischargeMode <> MODEFOLLOW) then
        DischargeInhibited := FALSE;

end;

procedure TStorageController2Obj.DoScheduleMode();
{
  In SCHEDULE mode we ramp up the storage from zero to the specified pctkWRate.
  This value is held for the flattime or until they  turn themselves
  off when they are either fully discharged, or ramped down

  The discharge trigger time must be greater than 0
}

var
    TDiff: Double;
    pctDischargeRate: Double;
begin
    pctDischargeRate := 0.0;   // init for test
    if (DisChargeTriggerTime > 0.0) then
        with ActiveCircuit.Solution do
        begin
               // turn on if time within 1/2 time step
            if not (FleetState = STORE_DISCHARGING) then
            begin
                ChargingAllowed := TRUE;
                TDiff := NormalizeToTOD(DynaVars.intHour, DynaVars.t) - DisChargeTriggerTime;
                if abs(TDiff) < DynaVars.h / 7200.0 then
                begin
                        {Time is within 1 time step of the trigger time}
                    if ShowEventLog then
                        AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Discharging (up ramp) by Schedule');
                    SetFleetToDischarge;
                    SetFleetDesiredState(STORE_DISCHARGING);
                    ChargingAllowed := FALSE;
                    pctDischargeRate := min(pctkWRate, max(pctKWRate * Tdiff / UpRampTime, 0.0));
                    SetFleetkWRate(pctDischargeRate);
                    DischargeInhibited := FALSE;
                    PushTimeOntoControlQueue(STORE_DISCHARGING);
                end;
            end
            else
            begin    // fleet is already discharging
                TDiff := NormalizeToTOD(DynaVars.intHour, DynaVars.t) - DisChargeTriggerTime;
                if TDiff < UpRampTime then
                begin

                    pctDischargeRate := min(pctkWRate, max(pctKWRate * Tdiff / UpRampTime, 0.0));
                    SetFleetDesiredState(STORE_DISCHARGING);

                    if pctDischargeRate <> LastpctDischargeRate then
                    begin
                        SetFleetkWRate(pctDischargeRate);
                        SetFleetToDischarge;
                    end;

                end
                else
                begin

                    if TDiff < UpPlusFlat then
                    begin

                        pctDischargeRate := pctkWRate;
                        SetFleetDesiredState(STORE_DISCHARGING);
                        if PctDischargeRate <> LastpctDischargeRate then
                            SetFleetkWRate(pctkWRate);  // on the flat part

                    end
                    else
                    if TDiff > UpPlusFlatPlusDn then
                    begin

                        SetFleetToIdle;
                        ChargingAllowed := TRUE;
                        pctDischargeRate := 0.0;
                        if ShowEventLog then
                            AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Idling by Schedule');

                    end
                    else
                    begin  // We're on the down ramp

                        TDiff := UpPlusFlatPlusDn - TDiff;
                        pctDischargeRate := max(0.0, min(pctKWRate * Tdiff / DnRampTime, pctKWRate));
                        SetFleetDesiredState(STORE_DISCHARGING);
                        SetFleetkWRate(pctDischargeRate);

                    end;

                end;

                if pctDischargeRate <> LastpctDischargeRate then
                    PushTimeOntoControlQueue(STORE_DISCHARGING);

            end;  {If not fleetstate ...}
        end;
    LastpctDischargeRate := pctDischargeRate;   // remember this value
end;

procedure TStorageController2Obj.DoTimeMode(Opt: Integer);
{
  In Time mode we need to only turn the storage elements on. They will turn themselves
  off when they are either fully discharged, fully charged, or receive another command
  from the controller
}
var
    RemainingkWh,
    ReservekWh,
    TotalRatingkWh: Double;
begin

    TotalRatingkWh := FleetkWhRating;
    RemainingkWh := FleetkWh;
    ReservekWh := FleetReservekWh;


    case Opt of

        1:
        begin
            if (DisChargeTriggerTime > 0.0) then
                with ActiveCircuit.Solution do
                begin
                 // turn on if time within 1/2 time step
                    if abs(NormalizeToTOD(DynaVars.intHour, DynaVars.t) - DisChargeTriggerTime) < DynaVars.h / 7200.0 then
                    begin
                        SetFleetDesiredState(STORE_DISCHARGING);
                        if not (FleetState = STORE_DISCHARGING) and (RemainingkWh > ReservekWh) then
                        begin
                        {Time is within 1 time step of the trigger time}
                            if ShowEventLog then
                                AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Discharging by Time Trigger');
                            SetFleetToDischarge;
                            SetFleetkWRate(pctKWRate);
                            DischargeInhibited := FALSE;
                            if DischargeMode = MODEFOLLOW then
                                DischargeTriggeredByTime := TRUE
                            else
                                PushTimeOntoControlQueue(STORE_DISCHARGING);
                        end;
                    end
                    else
                        ChargingAllowed := TRUE;
                end;
        end; // Discharge mode
        2:
        begin
            if ChargeTriggerTime > 0.0 then
                with ActiveCircuit.Solution do
                begin
                    if abs(NormalizeToTOD(DynaVars.intHour, DynaVars.t) - ChargeTriggerTime) < DynaVars.h / 7200.0 then
                    begin
                        SetFleetDesiredState(STORE_CHARGING);
                        if not (FleetState = STORE_CHARGING) and (RemainingkWh < TotalRatingkWh) then
                        begin
                          {Time is within 1 time step of the trigger time}
                            if ShowEventLog then
                                AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Charging by Time Trigger');
                            SetFleetToCharge;
                            DischargeInhibited := TRUE;
                            OutOfOomph := FALSE;
                            PushTimeOntoControlQueue(STORE_CHARGING);   // force re-solve at this time step
                          // Push message onto control queue to release inhibit at a later time
                            with ActiveCircuit do
                            begin
                                Solution.LoadsNeedUpdating := TRUE; // Force recalc of power parms
                                ControlQueue.Push(DynaVars.intHour + InhibitHrs, Dynavars.t, RELEASE_INHIBIT, 0, Self);
                            end;
                        end;
                    end;
                end;
        end; //Charge mode
    end;

end;

//----------------------------------------------------------------------------
function TStorageController2Obj.NormalizeToTOD(h: Integer; sec: Double): Double;
// Normalize time to a floating point number representing time of day If Hour > 24
// time should be 0 to 23.999999....
var
    HourOfDay: Integer;

begin

    if h > 23 then
        HourOfDay := (h - (h div 24) * 24)
    else
        HourOfDay := h;

    Result := HourOfDay + sec / 3600.0;

    if Result >= 24.0 then
        Result := Result - 24.0;   // Wrap around

end;


procedure TStorageController2Obj.PushTimeOntoControlQueue(Code: Integer);
{
   Push present time onto control queue to force re solve at new dispatch value
}
begin
    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        LoadsNeedUpdating := TRUE; // Force recalc of power parms
        ControlQueue.Push(DynaVars.intHour, DynaVars.t, Code, 0, Self);
    end;

end;

{--------------------------------------------------------------------------}
function TStorageController2Obj.Get_DynamicTarget(THigh: Integer): Double;
var
    Temp, temp2: Double;
    RatingIdx: Integer;
    RSignal: TXYCurveObj;
begin
    if SeasonSignal <> '' then
    begin
        RSignal := XYCurveClass.Find(SeasonSignal);
        if RSignal <> NIL then
            RatingIdx := trunc(RSignal.GetYValue(ActiveCircuit.Solution.DynaVars.intHour));

        if (RatingIdx <= Seasons) and (Seasons > 1) then
        begin
            if THigh = 1 then
                Result := SeasonTargets[RatingIdx]
            else
                Result := SeasonTargetsLow[RatingIdx]
        end
        else
        begin
            if THigh = 1 then
                Result := FkWTarget
            else
                Result := FkWTargetLow
        end;
    end;
end;


{--------------------------------------------------------------------------}
procedure TStorageController2Obj.DoLoadFollowMode();

var
    i: Integer;
    S: Complex;
    StorageObj: TStorageObj;
    StorekWChanged,
    StorekvarChanged,
    SkipkWDispatch: Boolean;
    VoltsArr: pComplexArray;
    kWhActual,
    ElemVolts,
    Amps,
    AmpsDiff,
    PDiff,
//   PFDiff,
    DispatchkW,
//   Dispatchkvar,
    RemainingkWh,
    CtrlTarget,
    ReservekWh,
    ActualkWDispatch: Double;

begin

     // If list is not defined, go make one from all storage elements in circuit
    if FleetPointerList.Count = 0 then
        MakeFleetList;

    if FleetSize > 0 then
    begin

        StorekWChanged := FALSE;
        StorekvarChanged := FALSE;
        SkipkWDispatch := FALSE;

       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
        if DischargeMode = CURRENTPEAKSHAVE then
        begin

            MonitoredElement.GetCurrents(cBuffer);
            GetControlCurrent(Amps);

//          Amps := MonitoredElement.MaxCurrent[ElementTerminal]; // Max current in active terminal  // old
        end
        else
            GetControlPower(S);

       // In case of having seasonal targets
        if SeasonalRating then
            CtrlTarget := Get_DynamicTarget(1)
        else
            CtrlTarget := FkWTarget;


        case DischargeMode of
             // Following Load; try to keep load below kW Target
            MODEFOLLOW:
            begin
                if DischargeTriggeredByTime then
                begin
                    if ShowEventLog then
                        AppendToEventLog('StorageController.' + Self.Name,
                            Format('Fleet Set to Discharging by Time Trigger; Old kWTarget = %-.6g; New = %-.6g', [FkwTarget, S.re * 0.001]));
                    FkwTarget := Max(FkWThreshold, S.re * 0.001);  // Capture present kW and reset target
                    if not FkWBandSpecified then
                        HalfkWBand := FpctkWBand / 200.0 * FkWTarget;  // Update band to new target if absolute kWBand hasn`t been specified
                    DischargeTriggeredByTime := FALSE;  // so we don't come back in here right away
                    SetFleetToIdle;
                    SetFleetDesiredState(STORE_IDLING);
                end;
                PDiff := S.re * 0.001 - FkWTarget;  // Assume S.re is normally positive
//                                  PFDiff        := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for peak shaving
            end;
             // supporting DG; Try to keep load above kW target
            MODESUPPORT:
            begin
                PDiff := S.re * 0.001 + FkWTarget;  // assume S.re is normally negative
//                                  PFDiff        := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for generator
            end;

            MODEPEAKSHAVE:
            begin
                PDiff := S.re * 0.001 - CtrlTarget;  // Assume S.re is normally positive
//                                  PFDiff        := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for peak shaving
            end;

            CURRENTPEAKSHAVE:
            begin
                PDiff := Amps - CtrlTarget * 1000;  // Gets the difference in terms of amps
//                                  DispatchVars  :=  False;
            end;
        else
            PDiff := 0.0;
        end;

        if Dischargemode = CURRENTPEAKSHAVE then    // convert Pdiff from Amps to kW
        begin
            MonitoredElement.ComputeVterminal();
            VoltsArr := MonitoredElement.Vterminal;
            ElemVolts := cabs(VoltsArr^[1]);
            kWNeeded := ((MonitoredElement.NPhases * Pdiff * ElemVolts) / 1000.0);
//         kWNeeded     :=  ((Pdiff * ElemVolts) / 1000.0);
            AmpsDiff := PDiff;
        end
        else
//         kWNeeded := Pdiff + FleetkW;
            kWNeeded := Pdiff;

       {  kW dispatch  }

       // Check if Fleet is Idling (FleetState is updated only if entire fleet is idling)
        if not (FleetState = STORE_IDLING) then
        begin
            for i := 1 to FleetSize do
            begin
                StorageObj := FleetPointerList.Get(i);
                if StorageObj.StorageState <> STORE_IDLING then
                    Break;
                if i = FleetSize then
                    FleetState := STORE_IDLING;
            end;
        end;

        if DischargeInhibited then
            SkipkWDispatch := TRUE
        else
        begin
//         If FleetState = STORE_CHARGING Then
//                   Begin
//                     if Not (Dischargemode = CURRENTPEAKSHAVE) then
//                      Pdiff :=  Pdiff + FleetkW  // ignore overload due to charging
//                     else
//                     Begin
//                       MonitoredElement.ComputeVterminal();
//                       VoltsArr     :=  MonitoredElement.Vterminal;
//                       ElemVolts    :=  cabs(VoltsArr^[1]);
//                       Pdiff        :=  Pdiff + (FleetkW * 1000 / ElemVolts);
//                     End;
//                   end;

//           If Not (FleetState = STORE_DISCHARGING) Then  // ignore overload due to charging or idling (trickle charging) - FleetkW < 0
            if (FleetState = STORE_CHARGING) then
            begin
                if not (DischargeMode = CURRENTPEAKSHAVE) then
                    Pdiff := Pdiff + FleetkW
                else
                begin
                    MonitoredElement.ComputeVterminal();
                    VoltsArr := MonitoredElement.Vterminal;
                    ElemVolts := cabs(VoltsArr^[1]);
                    Pdiff := Pdiff + (FleetkW * 1000 / (ElemVolts * MonitoredElement.NPhases));
//                 Pdiff        :=  Pdiff + (FleetkW * 1000 / (ElemVolts ));
                end;

            end;

            case FleetState of
                STORE_CHARGING,
                STORE_IDLING:
                    if (PDiff - HalfkWBand < 0.0) or OutOfOomph then
                    begin  // Don't bother trying to dispatch
                        ChargingAllowed := TRUE;
                        SkipkWDispatch := TRUE;
                        if OutofOomph then  // --------------------------------- new 04/20/2020 ----------
                        begin
                            for i := 1 to FleetSize do
                            begin
                                StorageObj := FleetPointerList.Get(i);
                                kWhActual := StorageObj.StorageVars.kWhStored / StorageObj.StorageVars.kWhRating;
                                OutOfOomph := OutOfOomph and (kWhActual >= ResetLevel);  // If we have more than the 80% we are good to dispatch
                            end;
                            OutOfOomph := not OutOfOomph;  // If everybody in the fleet has at least the 80% of the storage capacity full

                        end;    // -----------------------------------------------------------------------
                    end;
//                STORE_DISCHARGING: If ((PDiff + FleetkW) < 0.0)  or OutOfOomph Then
//                STORE_DISCHARGING: If (((PDiff + FleetkW) < 0.0) and (abs(PDiff) > HalfkWBand)) or OutOfOomph Then // CR: set to idle only if out of band
//                  Begin   // desired decrease is greater then present output; just cancel
//                        If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name,
//                        Format('Desired decrease is greater than present output. Pdiff = %-.6g, FleetkW = %-.6g. Setting Fleet to Idle', [PDiff, FleetkW]));
//                        SetFleetToIdle;   // also sets presentkW = 0
//                        For i := 1 to FleetSize Do Begin TStorageObj(FleetPointerList.Get(i)).SetNominalStorageOutput() End; // To Update Current kvarLimit
//                        PushTimeOntoControlQueue(STORE_IDLING);  // force a new power flow solution
//                        ChargingAllowed := TRUE;
//                        SkipkWDispatch  := TRUE;
//                        Wait4Step       := TRUE; // To tell to the charging section to wait for the next sim step
//                                                 // useful when workin with large simulation time steps
//                  End;
            end;
        end;


        if not SkipkWDispatch then
        begin
            RemainingkWh := FleetkWh;
            ReservekWh := FleetReservekWh;
            if (RemainingkWh > ReservekWh) then
            begin
               //  don't dispatch kW  if not enough storage left or an endless control loop will occur
                if abs(PDiff) > HalfkWBand then
                begin // Attempt to change storage dispatch
                    if not (FleetState = STORE_DISCHARGING) then
                    begin
                        SetFleetToDischarge;
//                    StorekWChanged:= TRUE;  // if not already discharging, force new power flow.
                    end;
                    if ShowEventLog then
                        AppendToEventLog('StorageController.' + Self.Name, Format('Attempting to dispatch %-.6g kW with %-.6g kWh remaining and %-.6g kWh reserve.', [kWNeeded, RemainingkWh, ReservekWh]));
                    for i := 1 to FleetSize do
                    begin
                        StorageObj := FleetPointerList.Get(i);

                        if Dischargemode = CURRENTPEAKSHAVE then // Current to power
                        begin    //  (MonitoredElement.MaxVoltage[ElementTerminal] / 1000)
                            if StorageObj.NPhases = 1 then
                                kWNeeded := StorageObj.PresentkV * AmpsDiff
                            else
//                          kWNeeded :=  StorageObj.PresentkV * InvSQRT3 * AmpsDiff;
                                kWNeeded := StorageObj.PresentkV * SQRT3 * AmpsDiff;
                        end;

                        with StorageObj do
                        begin
                      // compute new dispatch value for this storage element ...
                            DispatchkW := Min(StorageVars.kWrating, (PresentkW + kWNeeded * DispFactor * (FWeights^[i] / TotalWeight))); // Dispatch kWNeeded

                            if DispatchkW <= 0.0 then // if kWNeeded is too low, DispatchkW may be negative depending on idling losses. In this case, just set it to idling
                            begin

                                StorageState := STORE_IDLING;  // overrides SetFleetToDischarge

                                if (abs(PresentkW) - StorageObj.kWOutIdling > EPSILON) then  // if not already idling
                                begin
                                    SetNominalStorageOutput();
                                    ActualkWDispatch := PresentkW;
                                    StorekWChanged := TRUE; // if not idling at first, force a new powerflow

                                    if ShowEventLog then
                                        AppendToEventLog('StorageController.' + Self.Name,
                                            Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW. Setting ' + StorageObj.QualifiedName + ' to idling state. Final kWOut is %-.6g kW', [DispatchkW, ActualkWDispatch]));
                                end
//                          DispatchkW := 0.0;

                            end
                            else
                            begin
                                if abs(kW - DispatchkW) / abs(DispatchkW) > 0.0001 then // redispatch only if change requested
                                begin
                                    if DispatchkW < Max(CutInkWAC, CutOutkWAC) then   // Necessary check to avoid the control to go into an infinite loop when DispatchkW is less than CutOutkWAC
                                    begin
                                        if InverterON = TRUE then  // request Dispatch only if the inverter is on (only once).
                                        begin
                                // Next time, the inverter will be OFF and the control won't dispatch a new power
                                            if StorageVars.kWhStored > StorageVars.kWhReserve then
                                            begin

                                                kW := DispatchkW;
                                                SetNominalStorageOutput();
                                                ActualkWDispatch := PresentkW;
                                                StorekWChanged := TRUE;     // This is what keeps the control iterations going

                                                if ShowEventLog then
                                                    AppendToEventLog('StorageController.' + Self.Name,
                                                        Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW, less than CutIn/CutOut.' + ' Final kWOut is %-.6g kW', [DispatchkW, ActualkWDispatch]));
                                            end;
                                        end
                                        else
                                        begin

                                      // if inverter is already off, just override discharging state to
                                      // idling and update current kvarlimit for usage by InvControl

                                            StorageState := STORE_IDLING;     // overrides SetFleetToDischarge
                                            SetNominalStorageOutput(); // to update current kvarLimit
                                            ActualkWDispatch := PresentkW;
                                            if ShowEventLog then
                                                AppendToEventLog('StorageController.' + Self.Name,
                                                    Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW, less than CutIn/CutOut.' + ' Inverter is OFF. Final kWOut is %-.6g kW', [DispatchkW, ActualkWDispatch]));
                                        end
                                    end
                                    else
                                    if StorageVars.kWhStored > StorageVars.kWhReserve then
                                    begin  // Attempt to set discharge kW;  Storage element will revert to idling if out of capacity

                                        kW := DispatchkW;
                                        SetNominalStorageOutput();
                                        ActualkWDispatch := PresentkW;
                                        StorekWChanged := TRUE;     // This is what keeps the control iterations going

                                        if ShowEventLog then
                                            AppendToEventLog('StorageController.' + Self.Name,
                                                Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW. Final kWOut is %-.6g kW',
                                                [DispatchkW, ActualkWDispatch]));
                                    end;

                                end;

                            end;

                        end;
                    end;
                end
            end
            else
            begin
                if not FleetState = STORE_IDLING then
                begin
                    SetFleetToIdle;
                    PushTimeOntoControlQueue(STORE_IDLING);  // force a new power flow solution
                end;
                ChargingAllowed := TRUE;
                OutOfOomph := TRUE;
                if ShowEventLog then
                    AppendToEventLog('StorageController.' + Self.Name,
                        Format('Ran out of OOMPH: %-.6g kWh remaining and %-.6g reserve. Fleet has been set to idling state.', [RemainingkWh, ReservekWh]));
            end;
        end;

        if StorekWChanged or StorekvarChanged then  // Only push onto controlqueue If there has been a change
            PushTimeOntoControlQueue(STORE_DISCHARGING);

       {Else just continue}
    end;


end;

{--------------------------------------------------------------------------}
procedure TStorageController2Obj.DoPeakShaveModeLow();
    // This is the peakShaving mode for controlling the charging operation of the storage fleet
    // The objective is to charge the storage fleet when the power at a monitored element is bellow a specified KW target (kWTarget_low)
    // The storage will charge as much power as necessary to keet the power within the deadband around kWTarget_low

  // WILL NOT IMPLEMENT REACTIVE POWER CONTROL FOR NOW
var
    i: Integer;
    S: Complex;
    VoltsArr: PComplexArray;
    StorageObj: TStorageObj;
    StorekWChanged,
    SkipkWCharge: Boolean;
    ElemVolts,
    PDiff,
    kWNeeded,
    Amps,
    AmpsDiff,
    ChargekW,
    ActualkWh,
    ActualkW,
    TotalRatingkWh,
    KwtoPercentagekW,
    CtrlTarget,
    ActualkWDispatch: Double;

begin
     // If list is not defined, go make one from all storage elements in circuit
    if FleetPointerList.Count = 0 then
        MakeFleetList;

//     If (FleetSize>0) And(Not(FleetState = STORE_DISCHARGING)) Then
    if (FleetSize > 0) then
    begin
        StorekWChanged := FALSE;
        SkipkWCharge := FALSE;


        if SeasonalRating then
            CtrlTarget := Get_DynamicTarget(0)
        else
            CtrlTarget := FkWTargetLow;


       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
        if Chargemode = CURRENTPEAKSHAVELOW then
        begin

            MonitoredElement.GetCurrents(cBuffer);
            GetControlCurrent(Amps);
//         Amps         := MonitoredElement.MaxCurrent[ElementTerminal]; // Max current in active terminal
            PDiff := Amps - CtrlTarget * 1000;  // Gets the difference in terms of amps
        end
        else
        begin
            GetControlPower(S);
            PDiff := S.re * 0.001 - CtrlTarget;  // Assume S.re is normally positive
        end;

        ActualkW := FleetkW;
        ActualkWh := FleetkWh;
        TotalRatingkWh := FleetkWhRating;

        if Chargemode = CURRENTPEAKSHAVELOW then   // convert Pdiff from Amps to kW
        begin
            MonitoredElement.ComputeVterminal();
            VoltsArr := MonitoredElement.Vterminal;
            ElemVolts := cabs(VoltsArr^[1]);     // LN voltage
            kWNeeded := ((MonitoredElement.NPhases * PDiff * ElemVolts) / 1000.0);
//         kWNeeded     :=  (( PDiff * ElemVolts) / 1000.0);
            AmpsDiff := PDiff;
        end
        else
//         kWNeeded := Pdiff + FleetkW;
            kWNeeded := Pdiff;


        // Check if Fleet is Idling (FleetState is updated only if entire fleet is idling)
        if not (FleetState = STORE_IDLING) then
        begin
            for i := 1 to FleetSize do
            begin
                StorageObj := FleetPointerList.Get(i);
                if StorageObj.StorageState <> STORE_IDLING then
                    Break;
                if i = FleetSize then
                    FleetState := STORE_IDLING;
            end;
        end;

//
//           CASE  FleetState of
//                STORE_CHARGING,
//                STORE_IDLING: If (PDiff < 0.0) or OutOfOomph Then
//                  Begin  // Don't bother trying to dispatch
//                       ChargingAllowed  := TRUE;
//                       SkipkWDispatch   := TRUE;
//                  End;
//           END;


//       If Not (FleetState = STORE_CHARGING) Then  // ignore underload due to discharging  (FleetkW > 0) and discount idlings losses (may delay the charging)
        if (FleetState = STORE_DISCHARGING) then
        begin
            if not (ChargeMode = CURRENTPEAKSHAVELOW) then
                Pdiff := Pdiff + FleetkW
            else
            begin
                MonitoredElement.ComputeVterminal();
                VoltsArr := MonitoredElement.Vterminal;
                ElemVolts := cabs(VoltsArr^[1]);
                Pdiff := Pdiff + (FleetkW * 1000 / (ElemVolts * MonitoredElement.NPhases));   // get actual Pdiff in Currents (discount FleetkW)  (assuming same number of phases of Fleet and Monitored Element)
//                 Pdiff        :=  Pdiff + (FleetkW * 1000 / (ElemVolts ));
            end;

        end;

        case FleetState of
            STORE_DISCHARGING,
            STORE_IDLING:
                if (PDiff > 0.0) or (ActualkWh >= TotalRatingkWh) or Wait4Step then
                begin  // Don't bother trying to charge
                    ChargingAllowed := FALSE;
                    SkipkWCharge := TRUE;
                    Wait4Step := FALSE;
                end
//                        End;
//          STORE_CHARGING: If (kWNeeded > 0.0) or (ActualkWh>=TotalRatingkWh) // old approach
//          STORE_CHARGING: If (Pdiff + FleetkW > 0.0) or (ActualkWh >= TotalRatingkWh) Then
//                          Begin   // desired decrease (in absolute value) is greater than present output; just cancel
//                                SetFleetToIdle;   // also sets presentkW = 0
//                                PushTimeOntoControlQueue(STORE_IDLING);  // force a new power flow solution
//                                ChargingAllowed := FALSE;
//                                SkipkWCharge  := TRUE;
//                          End;
        end;

        if not SkipkWCharge then
        begin
            if (ActualkWh < TotalRatingkWh) then
            begin
               //  don't dispatch kW  if fully charged or an endless control loop will occur
                if abs(PDiff) > HalfkWBandLow then
                begin // Attempt to change storage kW charge
                    if not (FleetState = STORE_CHARGING) then
                    begin
                        SetFleetToCharge;
//                      StorekWChanged:= TRUE;  // if not already charging, force new power flow.
                    end;
  //                       If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, Format('Attempting to charge %-.6g kW with %-.6g kWh remaining and %-.6g rating.', [kWNeeded, (TotalRatingkWh-ActualkWh), TotalRatingkWh]));
                    if ShowEventLog then
                        AppendToEventLog('StorageController.' + Self.Name, Format('Attempting to charge %-.6g kW with %-.6g kWh remaining and %-.6g rating.', [kWNeeded, (TotalRatingkWh - ActualkWh), TotalRatingkWh]));
                    for i := 1 to FleetSize do
                    begin
                        StorageObj := FleetPointerList.Get(i);
                        with StorageObj do
                        begin

                        // Checks if PDiff needs to be adjusted considering the charging mode
                            if Chargemode = CURRENTPEAKSHAVELOW then
                            begin
                                if StorageObj.NPhases = 1 then
                                    kWNeeded := StorageObj.PresentkV * AmpsDiff
                                else
//                           kWNeeded :=  StorageObj.PresentkV * InvSQRT3 * AmpsDiff;
                                    kWNeeded := StorageObj.PresentkV * SQRT3 * AmpsDiff;
                            end;


                       // compute new charging value for this storage element ...
  //                                ChargekW := -1 * Min(StorageVars.kWrating, abs(PresentkW + Pdiff *(FWeights^[i]/TotalWeight)));  // old approach

                            ChargekW := PresentkW + kWNeeded * (FWeights^[i] / TotalWeight) * DispFactor; // may be positive or negative
                            if ChargekW < 0 then
                                ChargekW := Max(-1 * StorageVars.kWrating, ChargekW); // check against kVA rating

                            if ChargekW >= 0 then // chargekW may be positive if increase in demand is too high.
                            begin

                                StorageState := STORE_IDLING;  // overrides SetFleetToDischarge

                                if (abs(PresentkW) - StorageObj.kWOutIdling > EPSILON) then  // if not already idling
                                begin
                                    SetNominalStorageOutput();
                                    ActualkWDispatch := PresentkW;
                                    StorekWChanged := TRUE; // if not idling at first, force a new powerflow

                                    if ShowEventLog then
                                        AppendToEventLog('StorageController.' + Self.Name,
                                            Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW. Setting ' + StorageObj.QualifiedName + ' to idling state. Final kWOut is %-.6g kW', [ChargekW, ActualkWDispatch]));
                                end

                            end
                            else
                            begin
                          //                       If ChargekW <> PresentkW Then    // do only if change requested
                                if abs(StorageObj.kW - ChargekW) / abs(ChargekW) > 0.0001 then    // do only if change requested
                                begin

                                    if abs(ChargekW) < Max(CutInkWAC, CutOutkWAC) then   // Necessary check to avoid the control to go into an infinite loop when ChargekW is less than CutOutkWAC
                                    begin
                                        if InverterON = TRUE then  // request Dispatch only if the inverter is on (only once).
                                        begin
                                // Next time the inverter will be OFF and the control won't dispatch a new power
                                            if StorageVars.kWhStored > StorageVars.kWhReserve then
                                            begin

                                                kW := ChargekW;
                                                SetNominalStorageOutput();
                                                ActualkWDispatch := PresentkW;
                                                StorekWChanged := TRUE;     // This is what keeps the control iterations going

                                                if ShowEventLog then
                                                    AppendToEventLog('StorageController.' + Self.Name,
                                                        Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW, less than CutIn/CutOut.' + ' Final kWOut is %-.6g kW', [ChargekW, ActualkWDispatch]));
                                            end;
                                        end
                                        else
                                        begin

                                      // if inverter is already off, just override discharging state to
                                      // idling and update current kvarlimit for usage by InvControl

                                            StorageState := STORE_IDLING;     // overrides SetFleetToCharge
                                            SetNominalStorageOutput(); // to update current kvarLimit
                                            ActualkWDispatch := PresentkW;
                                            if ShowEventLog then
                                                AppendToEventLog('StorageController.' + Self.Name,
                                                    Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW, less than CutIn/CutOut.' + ' Inverter is OFF. Final kWOut is %-.6g kW', [ChargekW, ActualkWDispatch]));
                                        end
                                    end
                                    else
                                    if StorageVars.kWhStored < StorageVars.kWhRating then
                                    begin  // Attempt to set discharge kW;  Storage element will revert to idling if out of capacity
                                     //StorageObj.PresentkW  :=  ChargekW;
                                        kW := ChargekW;
                                        SetNominalStorageOutput();
                                        ActualkWDispatch := PresentkW;
        //                                           KwtoPercentagekW := (ChargekW*100) / StorageVars.kWrating;  // old approach
        //                                           StorageObj.pctkWin := abs(KwtoPercentagekW);                // old approach
                                        StorekWChanged := TRUE;     // This is what keeps the control iterations going

                                        if ShowEventLog then
                                            AppendToEventLog('StorageController.' + Self.Name,
                                                Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW. Final kWOut is %-.6g kW',
                                                [ChargekW, ActualkWDispatch]));

                                    end;

                                end;
                            end;

                        end;
                    end;
                end
            end
            else
            begin
                if not FleetState = STORE_IDLING then
                begin
                    SetFleetToIdle;
                    PushTimeOntoControlQueue(STORE_IDLING);  // force a new power flow solution
                end;
                ChargingAllowed := FALSE;
                if ShowEventLog then
                    AppendToEventLog('StorageController.' + Self.Name, Format('Fully charged: %-.6g kWh of rated %-.6g.', [ActualkWh, TotalRatingkWh]));
            end;
        end;

        if StorekWChanged then  // Only push onto controlqueue If there has been a change
            PushTimeOntoControlQueue(STORE_CHARGING);
       {Else just continue}
    end;
end;

{--------------------------------------------------------------------------}
procedure TStorageController2Obj.Sample();

begin
    ChargingAllowed := FALSE;
//       UpdateFleetState;
{
  Check discharge mode first. Then if not discharging, we can check for charging
}
    Wait4Step := FALSE;        // Initializes the variable for the new control step
    case DischargeMode of
        MODEFOLLOW:
        begin
            DoTimeMode(1);
            DoLoadFollowMode();
        end;
        MODELOADSHAPE:
            DoLoadShapeMode();
        MODESUPPORT:
            DoLoadFollowMode();
        MODETIME:
            DoTimeMode(1);
        MODEPEAKSHAVE:
            DoLoadFollowMode();
        CURRENTPEAKSHAVE:
            DoLoadFollowMode();
        MODESCHEDULE:
            DoScheduleMode();
    else
        DoSimpleMsg(Format('Invalid DisCharging Mode: %d', [DisChargeMode]), 14408);
    end;

    if ChargingAllowed then
        case ChargeMode of
            MODELOADSHAPE: ; // DoLoadShapeMode;  already executed above
            MODETIME:
                DoTimeMode(2);
            MODEPEAKSHAVELOW:
                DoPeakShaveModeLow();
            CURRENTPEAKSHAVELOW:
                DoPeakShaveModeLow()
        else
            DoSimpleMsg(Format('Invalid Charging Mode: %d', [ChargeMode]), 14409);
        end;


end;


//----------------------------------------------------------------------------
procedure TStorageController2Obj.CalcDailyMult(Hr: Double);

begin
    if (DailyShapeObj <> NIL) then
    begin
        LoadShapeMult := DailyShapeObj.GetMult(Hr);
    end
    else
        LoadShapeMult := CDoubleOne;  // Default to no  variation
end;


//----------------------------------------------------------------------------
procedure TStorageController2Obj.CalcDutyMult(Hr: Double);

begin
    if DutyShapeObj <> NIL then
    begin
        LoadShapeMult := DutyShapeObj.GetMult(Hr);
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult If no duty curve specified
end;

//----------------------------------------------------------------------------
procedure TStorageController2Obj.CalcYearlyMult(Hr: Double);

begin
    if YearlyShapeObj <> NIL then
    begin
        LoadShapeMult := YearlyShapeObj.GetMult(Hr);
    end
    else
        CalcDailyMult(Hr);  // Defaults to Daily curve
end;

//----------------------------------------------------------------------------
procedure TStorageController2Obj.DoLoadShapeMode();
var
    FleetStateSaved: Integer;
    RateChanged: Boolean;
    NewChargeRate: Double;
    NewkWRate,
    NewkvarRate: Double;
begin

    FleetStateSaved := FleetState;
    RateChanged := FALSE;

    // Get multiplier

    with ActiveCircuit.Solution do
        case Mode of
            DAILYMODE:
                CalcDailyMult(DynaVars.dblHour); // Daily dispatch curve
            YEARLYMODE:
                CalcYearlyMult(DynaVars.dblHour);
            LOADDURATION2:
                CalcDailyMult(DynaVars.dblHour);
            PEAKDAY:
                CalcDailyMult(DynaVars.dblHour);
            DUTYCYCLE:
                CalcDutyMult(DynaVars.dblHour);
        end;

    if LoadShapeMult.re < 0.0 then
    begin
        ChargingAllowed := TRUE;
        NewChargeRate := Abs(LoadShapeMult.re) * 100.0;
        SetFleetDesiredState(STORE_CHARGING);

        if NewChargeRate <> pctChargeRate then
        begin
            RateChanged := TRUE;
            pctChargeRate := NewChargeRate;
            SetFleetChargeRate;
            SetFleetToCharge;
        end;
    end

    else
    if LoadShapeMult.re = 0.0 then
        SetFleetToIdle
    else
    begin   // Set fleet to discharging at a rate
        NewkWRate := LoadShapeMult.re * 100.0;
//           NewkvarRate := LoadShapeMult.im * 100.0;
        SetFleetDesiredState(STORE_DISCHARGING);

//           If (NewkWRate <> pctkWRate) or (NewkvarRate <> pctkvarRate) then
        if (NewkWRate <> pctkWRate) then
           // only set rate if it has changed. otherwise the debugtrace report will not report kWOut correctly.
        begin
            RateChanged := TRUE;
            pctkWRate := NewkWRate;
//              pctkvarRate := NewkvarRate;
            SetFleetkWRate(pctKWRate);
//              SetFleetkvarRate(pctkvarRate);
            SetFleetToDischarge;

            ActiveCircuit.Solution.LoadsNeedUpdating := TRUE; // Force recalc of power parms
        end;
    end;

    {Force a new power flow solution if fleet state has changed}
    if (FleetState <> FleetStateSaved) or RateChanged then
        PushTimeOntoControlQueue(0);


end;

//----------------------------------------------------------------------------
procedure TStorageController2Obj.SetAllFleetValues;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        with TStorageObj(FleetPointerList.Get(i)) do
        begin
            pctkWin := pctChargeRate;
//              Fpctkvarout := pctkvarRate;  CR
            pctkWout := pctkWRate;
            pctReserve := pctFleetReserve;
        end;
end;

//----------------------------------------------------------------------------
procedure TStorageController2Obj.SetFleetChargeRate;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).pctkWin := pctChargeRate;
end;

//----------------------------------------------------------------------------
//PROCEDURE TStorageController2Obj.SetFleetkvarRate;
//VAR
//      i   :Integer;
//Begin
//    {For side effects see pctkvarout property of Storage element}
////      For i := 1 to FleetPointerList.Count Do
////            TStorageObj(FleetPointerList.Get(i)).pctkvarout := pctkvarRate;
//End;

//----------------------------------------------------------------------------
procedure TStorageController2Obj.SetFleetkWRate(pctkw: Double);
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).pctkWout := pctkw;
end;

//----------------------------------------------------------------------------
procedure TStorageController2Obj.SetFleetToCharge;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).StorageState := STORE_CHARGING;
    FleetState := STORE_CHARGING;
end;

//----------------------------------------------------------------------------
procedure TStorageController2Obj.SetFleetToDisCharge;
var
    i: Integer;
begin

    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).StorageState := STORE_DISCHARGING;
    FleetState := STORE_DISCHARGING;
end;

//----------------------------------------------------------------------------
procedure TStorageController2Obj.SetFleetToIdle;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        with TStorageObj(FleetPointerList.Get(i)) do
        begin
            StorageState := STORE_IDLING;
//                  PresentkW := 0.0;
            kW := 0.0;
        end;
    FleetState := STORE_IDLING;
end;

//-----------------------------------------------------------------------------

procedure TStorageController2Obj.SetFleetDesiredState(state: Integer);
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).StateDesired := state;
end;

//-----------------------------------------------------------------------------
//
//procedure TStorageController2Obj.Set_PFBand(const Value: Double);
//begin
//      FPFBand    := Value;
//      HalfPFBand := FPFBand / 2.0;
//end;

//----------------------------------------------------------------------------
procedure TStorageController2Obj.SetFleetToExternal;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.Count do
        TStorageObj(FleetPointerList.Get(i)).DispatchMode := STORE_EXTERNALMODE;
end;

//----------------------------------------------------------------------------
(*
  PROCEDURE TStorageController2Obj.SetPctReserve;
  VAR
        i   :Integer;
  Begin
        For i := 1 to FleetPointerList.Count Do
              TStorageObj(FleetPointerList.Get(i)).pctReserve := pctFleetReserve;
  End;
*)


//----------------------------------------------------------------------------
function TStorageController2Obj.InterpretMode(Opt: Integer;
    const S: String): Integer;
begin

    Result := -1;  // Unknown: error
    case Opt of
        propMODEDISCHARGE:
            case LowerCase(S)[1] of
                'f':
                    Result := MODEFOLLOW;
                'l':
                    Result := MODELOADSHAPE;
                'p':
                    Result := MODEPEAKSHAVE;
                's':
                    if LowerCase(S)[2] = 'c' then
                        Result := MODESCHEDULE
                    else
                        Result := MODESUPPORT;
                't':
                    Result := MODETIME;
                'i':
                    Result := CURRENTPEAKSHAVE;
            else
                DoSimpleMsg('Discharge Mode "' + S + '" not recognized.', 14402);
            end;
        propMODECHARGE:
            case LowerCase(S)[1] of
                 // 'f': Result := MODEFOLLOW;
                'l':
                    Result := MODELOADSHAPE;
                 // 's': Result := MODESUPPORT;
                't':
                    Result := MODETIME;
                'p':
                    Result := MODEPEAKSHAVELOW;
                'i':
                    Result := CURRENTPEAKSHAVELOW;
            else
                DoSimpleMsg('Charge Mode "' + S + '" not recognized.', 14402);
            end;
    else
    end;
end;

//----------------------------------------------------------------------------
function TStorageController2Obj.MakeFleetList: Boolean;

var
    StorageObj: TStorageObj;
    i: Integer;

begin

    Result := FALSE;

    if FElementListSpecified then
    begin    // Name list is defined - Use it

        FleetPointerList.Clear;
        for i := 1 to FleetSize do
        begin
            StorageObj := Storage2Class.Find(FStorageNameList.Strings[i - 1]);
            if Assigned(StorageObj) then
            begin
                if StorageObj.Enabled then
                    FleetPointerList.New := StorageObj;
            end
            else
            begin
                DoSimpleMsg('Error: Storage Element "' + FStorageNameList.Strings[i - 1] + '" not found.', 14403);
                Exit;
            end;
        end;

    end

    else
    begin

     {Search through the entire circuit for enabled Storage Elements and add them to the list}
        FStorageNameList.Clear;
        FleetPointerList.Clear;
        for i := 1 to Storage2Class.ElementCount do
        begin
            StorageObj := Storage2Class.ElementList.Get(i);
        // Look for a storage element not already assigned
            if StorageObj.Enabled and (StorageObj.DispatchMode <> STORE_EXTERNALMODE) then
            begin
                FStorageNameList.Add(StorageObj.Name);  // Add to list of names
                FleetPointerList.New := StorageObj;
            end;
        end;

     {Allocate uniform weights}
        FleetSize := FleetPointerList.Count;
        Reallocmem(FWeights, Sizeof(FWeights^[1]) * FleetSize);
        for i := 1 to FleetSize do
            FWeights^[i] := 1.0;

    end;

   // Add up total weights
    TotalWeight := 0.0;
    for i := 1 to FleetSize do
        TotalWeight := TotalWeight + FWeights^[i];

    if FleetPointerList.Count > 0 then
        Result := TRUE;

    FleetListChanged := FALSE;

end;


//----------------------------------------------------------------------------
procedure TStorageController2Obj.Reset;
begin
  // inherited;
    SetFleetToIdle;

 // do we want to set fleet to 100% charged storage?
end;


//----------------------------------------------------------------------------
function TStorageController2Obj.ReturnElementsList: String;
var
    i: Integer;
begin
    if FleetSize = 0 then
    begin
        Result := '';
        Exit;
    end;

    Result := '[' + FStorageNameList.Strings[0];
    for i := 1 to FleetSize - 1 do
    begin
        Result := Result + ', ' + FStorageNameList.Strings[i];
    end;
    Result := Result + ']';  // terminate the array

end;

//----------------------------------------------------------------------------
function TStorageController2Obj.ReturnSeasonTarget(THigh: Integer): String;
var
    i: Integer;
begin
    if Seasons = 1 then
    begin
        Result := '';
        Exit;
    end;

    Result := '[';
    for i := 0 to (Seasons - 1) do
    begin
        if THigh = 1 then
            Result := Result + format('%.6g', [SeasonTargets[i]]) + ', '
        else
            Result := Result + format('%.6g', [SeasonTargetsLow[i]]) + ', ';
    end;
    Result := Result + ']';  // terminate the array

end;


//----------------------------------------------------------------------------
function TStorageController2Obj.ReturnWeightsList: String;
begin
    if FleetSize = 0 then
    begin
        Result := '';
        Exit;
    end;

    Result := GetDSSArray_Real(FleetSize, FWeights);

end;

//----------------------------------------------------------------------------
procedure TStorageController2Obj.GetControlPower(var ControlPower: Complex);
// Get power to control based on active power
var
    i, ControlPowerPhase: Integer;
    TempPower: Double;

begin

    if MonitoredElement.NPhases = 1 then
    begin
        ControlPower := MonitoredElement.Power[ElementTerminal]; // just take the total power (works also for 1ph elements with 2 conductors)
    end
    else
    begin
        MonitoredElement.GetPhasePower(cBuffer);

        case FMonPhase of
            AVG:
            begin  // Get avg of all phases
                ControlPower := Cmplx(0.0, 0.0);
                for i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) do
                    ControlPower := Cadd(ControlPower, cBuffer^[i]);
            end;
            MAXPHASE:
            begin  // Get abs max of all phases
                ControlPower := Cmplx(0.0, 0.0);
                for i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) do
                begin
                    TempPower := abs(cBuffer^[i].re);
                    if TempPower > abs(ControlPower.re) then
                        ControlPower := cBuffer^[i];
                    ControlPowerPhase := i;
                end;
                          // Compute equivalent total power of all phases assuming equal to max power in all phases
                ControlPower := cMulReal(ControlPower, Fnphases);
            end;
            MINPHASE:
            begin // Get abs min of all phases
                ControlPower := Cmplx(1.0e50, 1.0e50);
                for i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) do
                begin
                    TempPower := abs(cBuffer^[i].re);
                    if TempPower < abs(ControlPower.re) then
                        ControlPower := cBuffer^[i];
                    ControlPowerPhase := i;
                end;
                          // Compute equivalent total power of all phases assuming equal to min power in all phases
                ControlPower := cMulReal(ControlPower, Fnphases);  // sign according to phase with min abs value
            end;
        else
            // Compute equivalent total power of all phases assuming equal to power in selected phases
            ControlPower := cMulReal(Cbuffer^[FMonPhase], Fnphases);  // monitored phase only
        end;
    end;

    {If this is a positive sequence circuit (Fnphases=1),
    then we need to multiply by 3 to get the 3-phase power}
    if ActiveCircuit.PositiveSequence then
        ControlPower := cMulReal(ControlPower, 3.0);

end;

//----------------------------------------------------------------------------
procedure TStorageController2Obj.GetControlCurrent(var ControlCurrent: Double);
// Get current to control
var
    i: Integer;

begin

    case FMonPhase of
        AVG:
        begin
            ControlCurrent := 0.0;     // Get avg of all phases
            for i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) do
                ControlCurrent := ControlCurrent + Cabs(cBuffer^[i]);
            ControlCurrent := ControlCurrent / Fnphases;
        end;
        MAXPHASE:
        begin
            ControlCurrent := 0.0;     // Get max of all phases
            for i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) do
                ControlCurrent := max(ControlCurrent, Cabs(cBuffer^[i]));
            ControlCurrent := ControlCurrent;
        end;
        MINPHASE:
        begin
            ControlCurrent := 1.0e50;     // Get min of all phases
            for i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) do
                ControlCurrent := min(ControlCurrent, Cabs(cBuffer^[i]));
            ControlCurrent := ControlCurrent;
        end;
    else
    {Just use one phase because that's what most controls do.}
        ControlCurrent := Cabs(Cbuffer^[FMonPhase]);  // monitored phase only
    end;

end;


{--------------------------------------------------------------------------}

initialization

    CDoubleOne := Cmplx(1.0, 1.0);

end.
