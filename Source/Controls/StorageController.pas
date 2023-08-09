{$HINTS OFF}
unit StorageController;
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

INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex,
     utilities, PointerList, Classes, Loadshape, solution;

CONST   AVG= -1;
        MAXPHASE  = -2;
        MINPHASE  = -3;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TStorageController = class(TControlClass)
     private

     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const StorageController2Name:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit(ActorID : Integer):Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;

   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TStorageControllerObj = class(TControlElem)
     private

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
            UpPlusFlatPlusDn ,
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
            DispFactor               :Double;  // for slower convergence

            FStorageNameList        :TStringList;
            FleetPointerList         :PointerList.TPointerList;
            SeasonTargets,
            SeasonTargetsLow         :TRatingsArray;
            FWeights                 :pDoubleArray;
            cBuffer : pComplexArray;    // Complex Array buffer

            FleetListChanged,
            ChargingAllowed,
//            DispatchVars,
            DischargeTriggeredByTime,
            DischargeInhibited,
            OutOfOomph,
            FElementListSpecified,
            Wait4Step,
            FkWBandSpecified         :Boolean;  // true if kWBand specified as an absolute value (for use in Follow Discharge Mode to update the target)

            Seasons,
            FleetSize,
            FleetState,
            DischargeMode,
            InhibitHrs,
            ChargeMode,
            FMonPhase,
            CondOffset               :Integer;

            YearlyShape              :String;         // ='fixed' means no variation  on all the time
            YearlyShapeObj           :TLoadShapeObj;  // Shape for this Storage element
            DailyShape               :String;         // Daily (24 HR) Storage element shape
            DailyShapeObj            :TLoadShapeObj;  // Daily Storage element Shape for this load
            DutyShape                :String;         // Duty cycle load shape for changes typically less than one hour
            DutyShapeObj             :TLoadShapeObj;  // Shape for this Storage element

            LoadShapeMult            :Complex;
            pctVpu                   : double;        // The voltage level for the storage to start discharging in local control mode

           // PROCEDURE SetPctReserve;
            PROCEDURE SetAllFleetValues;
            PROCEDURE SetFleetkWRate(pctkw:Double);
//            PROCEDURE SetFleetkvarRate(pctkvar:Double);
            PROCEDURE SetFleetChargeRate;
            PROCEDURE SetFleetToCharge;
            PROCEDURE SetFleetToDisCharge;
            PROCEDURE SetFleetToIdle;
            PROCEDURE SetFleetToExternal;
            PROCEDURE SetFleetDesiredState(state: Integer);
            FUNCTION  InterpretMode(Opt :Integer; Const S:String):Integer;
            FUNCTION  GetModeString(Opt, Mode :Integer):String;
            FUNCTION  GetkWTotal(Var Sum:double):String;
            FUNCTION  GetkWhTotal(Var Sum:Double):String;
            FUNCTION  GetkWhActual:String;
            FUNCTION  GetkWActual:String;

            PROCEDURE CalcYearlyMult(Hr:double);
            PROCEDURE CalcDailyMult(Hr:double);
            PROCEDURE CalcDutyMult(Hr:double);

            FUNCTION  ReturnSeasonTarget(THigh :  Integer):String;
            FUNCTION  ReturnElementsList:String;
            FUNCTION  ReturnWeightsList:String;

            FUNCTION  MakeFleetList:Boolean;
            PROCEDURE DoLoadFollowMode(ActorID : Integer);
            PROCEDURE DoLoadShapeMode(ActorID : Integer);
            PROCEDURE DoTimeMode (Opt:Integer; ActorID : Integer);
            PROCEDURE DoScheduleMode(ActorID : Integer);
            PROCEDURE DoPeakShaveModeLow(ActorID : Integer);
            PROCEDURE PushTimeOntoControlQueue(Code:Integer; ActorID : Integer);
            FUNCTION  NormalizeToTOD(h: Integer; sec: Double): Double;
            PROCEDURE GetControlPower(var ControlPower: Complex; ActorID : Integer);
            PROCEDURE GetControlCurrent(var ControlCurrent: Double);
//            procedure Set_PFBand(const Value: Double);
            function  Get_FleetkW: Double;
            function  Get_FleetkWh: Double;
            function  Get_FleetkWhRating: Double;
            function  Get_FleetReservekWh: Double;

            function Get_DynamicTarget(THigh : Integer; ActorID : Integer): Double;

     public

           constructor Create(ParClass:TDSSClass; const StorageController2Name:String);
           destructor Destroy; override;

           PROCEDURE MakePosSequence(ActorID : Integer); Override;  // Make a positive Sequence Model
           PROCEDURE RecalcElementData(ActorID : Integer); Override;
           PROCEDURE CalcYPrim(ActorID : Integer); Override;    // Always Zero for a StorageController

           PROCEDURE Sample(ActorID : Integer);  Override;    // Sample control quantities and set action times in Control Queue
           PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer); Override;   // Do the action that is pending from last sample
           PROCEDURE Reset(ActorID : Integer); Override;  // Reset to initial defined state

           PROCEDURE GetCurrents(Curr: pComplexArray; ActorID : Integer); Override; // Get present value of terminal Curr
           PROCEDURE GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override;   // Returns Injextion currents

           PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
           PROCEDURE DumpProperties(VAR F:TextFile; Complete:Boolean);Override;
           FUNCTION  GetPropertyValue(Index:Integer):String;Override;

//           Property PFBand   :Double   Read FPFBand  Write  Set_PFBand;
           Property FleetkW  :Double   Read Get_FleetkW;
           Property FleetkWh :Double   Read Get_FleetkWh;
           Property FleetkWhRating :Double   Read Get_FleetkWhRating;
           Property FleetReservekWh :Double Read Get_FleetReservekWh;

   End;


VAR
    ActiveStorageController2Obj:   TStorageControllerObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit,  Storage,
    Sysutils, uCmatrix, MathUtil, Math, Dynamics,
    XYCurve;

CONST

    propELEMENT               = 1;
    propTERMINAL              = 2;
    propMONPHASE              = 3;
    propKWTARGET              = 4;
    propKWTARGETLOW           = 5;
    propPCTKWBAND             = 6;
    propKWBAND                = 7;
    propPCTKWBANDLOW          = 8;
    propKWBANDLOW             = 9;
//    propPFTARGET      = 7;
//    propPFBAND        = 8;
    propELEMENTLIST           = 10;
    propWEIGHTS               = 11;
    propMODEDISCHARGE         = 12;
    propMODECHARGE            = 13;
    propTIMEDISCHARGETRIGGER  = 14;
    propTIMECHARGETRIGGER     = 15;
    propRATEKW                = 16;
//    propRATEKVAR      = 16;
    propRATECHARGE            = 17;
    propRESERVE               = 18;
    propKWHTOTAL              = 19;
    propKWTOTAL               = 20;
    propKWHACTUAL             = 21;
    propKWACTUAL              = 22;
    propKWNEED                = 23;
//    propPARTICIPATION = 24;
    propYEARLY                = 24;
    propDAILY                 = 25;
    propDUTY                  = 26;
    propEVENTLOG              = 27;
//    propVARDISPATCH   = 29;
    propINHIBITTIME           = 28;
    propTUPRAMP               = 29;
    propTFLAT                 = 30;
    propTDNRAMP               = 31;
    propKWTHRESHOLD           = 32;
    propDispFactor            = 33;
    propRESETLEVEL            = 34;
    propSEASONS               = 35;
    propSEASONTARGETS         = 36;
    propSEASONTARGETSLOW      = 37;

    NumPropsThisClass         = 37;

//= = = = = = = = = = = = = = DEFINE CONTROL MODE CONSTANTS = = = = = = = = = = = = = = = = = = = = = = = = =

    MODEFOLLOW          = 1;
    MODELOADSHAPE       = 2;
    MODESUPPORT         = 3;
    MODETIME            = 4;
    MODEPEAKSHAVE       = 5;
    MODESCHEDULE        = 6;
    MODEPEAKSHAVELOW    = 7;
    CURRENTPEAKSHAVE    = 8;
    CURRENTPEAKSHAVELOW = 9;

//= = = = = = = = = = = = = = DEFINE OTHER CONSTANTS = = = = = = = = = = = = = = = = = = = = = = = = =
    RELEASE_INHIBIT = 999;

VAR
    CDoubleOne :Complex;

{--------------------------------------------------------------------------}
constructor TStorageController.Create;  // Creates superstructure for all StorageController objects
Begin
     Inherited Create;

     Class_name         := 'StorageController';
     DSSClassType       := DSSClassType + Storage_CONTROL;

     DefineProperties;

     CommandList        := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

{--------------------------------------------------------------------------}
destructor TStorageController.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TStorageController.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;

     // Define Property names

     PropertyName[propELEMENT]                := 'Element';
     PropertyName[propTERMINAL]               := 'Terminal';
     PropertyName[propMONPHASE]               := 'MonPhase';
     PropertyName[propKWTARGET]               := 'kWTarget';
     PropertyName[propKWTARGETLOW]            := 'kWTargetLow';
     PropertyName[propPCTKWBAND]              := '%kWBand';
     PropertyName[propKWBAND]                 := 'kWBand';
     PropertyName[propPCTKWBANDLOW]           := '%kWBandLow';
     PropertyName[propKWBANDLOW]              := 'kWBandLow';
//     PropertyName[propPFTARGET]               := 'PFTarget';
//     PropertyName[propPFBAND]                 := 'PFBand';
     PropertyName[propELEMENTLIST]            := 'ElementList';
     PropertyName[propWEIGHTS]                := 'Weights';
     PropertyName[propMODEDISCHARGE]          := 'ModeDischarge';
     PropertyName[propMODECHARGE]             := 'ModeCharge';
     PropertyName[propTIMEDISCHARGETRIGGER]   := 'TimeDischargeTrigger';
     PropertyName[propTIMECHARGETRIGGER]      := 'TimeChargeTrigger';
     PropertyName[propRATEKW]                 := '%RatekW';
//     PropertyName[propRATEKVAR]               := '%Ratekvar';
     PropertyName[propRATECHARGE]             := '%RateCharge';
     PropertyName[propRESERVE]                := '%Reserve';
     PropertyName[propKWHTOTAL]               := 'kWhTotal';
     PropertyName[propKWTOTAL]                := 'kWTotal';
     PropertyName[propKWHACTUAL]              := 'kWhActual';
     PropertyName[propKWACTUAL]               := 'kWActual';
     PropertyName[propKWNEED]                 := 'kWneed';
//     PropertyName[propPARTICIPATION]          := '%Participation';
     PropertyName[propYEARLY]                 := 'Yearly';
     PropertyName[propDAILY]                  := 'Daily';
     PropertyName[propDUTY]                   := 'Duty';
     PropertyName[propEVENTLOG]               := 'EventLog';
//     PropertyName[propVARDISPATCH]            := 'VarDispatch';
     PropertyName[propINHIBITTIME]            := 'InhibitTime';
     PropertyName[propTUPRAMP]                := 'Tup';
     PropertyName[propTFLAT]                  := 'TFlat';
     PropertyName[propTDNRAMP]                := 'Tdn';
     PropertyName[propKWTHRESHOLD]            := 'kWThreshold';
     PropertyName[propDispFactor]             := 'DispFactor';
     PropertyName[propRESETLEVEL]             := 'ResetLevel';
     PropertyName[propSEASONS]                := 'Seasons';
     PropertyName[propSEASONTARGETS]          := 'SeasonTargets';
     PropertyName[propSEASONTARGETSLOW]       := 'SeasonTargetsLow';

    PropertyHelp[propELEMENT]             :=
      'Full object name of the circuit element, typically a line or transformer, '+
      'which the control is monitoring. There is no default; Must be specified.' +
      'In "Local" control mode, is the name of the load that will be managed by the storage device, which should be installed at the same bus.';
    PropertyHelp[propTERMINAL]            :=
      'Number of the terminal of the circuit element to which the StorageController control is connected. '+
      '1 or 2, typically.  Default is 1. Make sure to select the proper direction on the power for the respective dispatch mode.';
    PropertyHelp[propMONPHASE]            :=
      'Number of the phase being monitored or one of {AVG | MAX | MIN} for all phases. Default=MAX. ' +
      'Must be less than the number of phases. Used in PeakShave, Follow, Support and I-PeakShave discharging modes ' +
      'and in PeakShaveLow, I-PeakShaveLow charging modes. For modes based on active power measurements, the value ' +
      'used by the control is the monitored one multiplied by the number of phases of the monitored element.';
    PropertyHelp[propKWTARGET]            :=
      'kW/kamps target for Discharging. The Storage element fleet is dispatched to try to hold the power/current in band '+
      'at least until the Storage is depleted. The selection of power or current depends on the Discharge mode (PeakShave->kW, I-PeakShave->kamps).';
    PropertyHelp[propKWTARGETLOW]         :=
      'kW/kamps target for Charging. The Storage element fleet is dispatched to try to hold the power/current in band '+
      'at least until the Storage is fully charged. The selection of power or current depends on the charge mode (PeakShavelow->kW, I-PeakShavelow->kamps).';
    PropertyHelp[propPCTKWBAND]              :=
      'Bandwidth (% of Target kW/kamps) of the dead band around the kW/kamps target value. Default is 2% (+/-1%).' +
      'No dispatch changes are attempted if the power in the monitored terminal stays within this band.';
    PropertyHelp[propKWBAND]              :=
      'Alternative way of specifying the bandwidth. (kW/kamps) of the dead band around the kW/kamps target value. Default is 2% of kWTarget (+/-1%).' +
      'No dispatch changes are attempted if the power in the monitored terminal stays within this band.';
    PropertyHelp[propPCTKWBANDLOW]           :=
      'Bandwidth (% of kWTargetLow) of the dead band around the kW/kamps low target value. Default is 2% (+/-1%).' +
      'No charging is attempted if the power in the monitored terminal stays within this band.';
    PropertyHelp[propKWBANDLOW]           :=
      'Alternative way of specifying the bandwidth. (kW/kamps) of the dead band around the kW/kamps low target value. Default is 2% of kWTargetLow (+/-1%).' +
      'No charging is attempted if the power in the monitored terminal stays within this band.';
//    PropertyHelp[propPFTARGET]          :=
//      'Power Factor target for dispatching the reactive power. Default is 0.96. The reactive power of the storage element fleet is dispatched to try to hold the power factor in band. '+
//      'It is assumed that the storage element inverter can produce kvar up to its kVA limit regardless of storage level.';
//    PropertyHelp[propPFBAND]            :=
//      'Bandwidth of the Target power factor of the monitored element. of the dead band around the kvar target value. Default is 0.04 (+/- 0.02).' +
//      'No dispatch changes of the kvar are attempted If the power factor of the monitored terminal stays within this band.';
    PropertyHelp[propELEMENTLIST]         :=
      'Array list of Storage elements to be controlled.  If not specified, all Storage elements in the circuit not presently dispatched by another controller ' +
      'are assumed dispatched by this controller.';
    PropertyHelp[propWEIGHTS]             := 
     'Array of proportional weights corresponding to each Storage element in the ElementList. ' +
     'The needed kW or kvar to get back to center band is dispatched to each Storage element according to these weights. ' +
     'Default is to set all weights to 1.0.';
    PropertyHelp[propMODEDISCHARGE]       :=
     '{PeakShave* | Follow | Support | Loadshape | Time | Schedule | I-PeakShave } Mode of operation for the DISCHARGE FUNCTION of this controller. ' +
     CRLF+CRLF+'In PeakShave mode (Default), the control attempts to discharge Storage to keep power in the monitored element below the kWTarget. ' +
     CRLF+CRLF+'In Follow mode, the control is triggered by time and resets the kWTarget value to the present monitored element power. ' +
     'It then attempts to discharge Storage to keep power in the monitored element below the new kWTarget. See TimeDischargeTrigger.' +
     CRLF+CRLF+'In Support mode, the control operates oppositely of PeakShave mode: Storage is discharged to keep kW power output up near the target. ' +
     CRLF+CRLF+'In Loadshape mode, both charging and discharging precisely follows the per unit loadshape. ' +
     'Storage is discharged when the loadshape value is positive. ' +
     CRLF+CRLF+'In Time mode, the Storage discharge is turned on at the specified %RatekW at the specified discharge trigger time in fractional hours.' +
     CRLF+CRLF+'In Schedule mode, the Tup, TFlat, and Tdn properties specify the up ramp duration, flat duration, and down ramp duration for the schedule. ' +
     'The schedule start time is set by TimeDischargeTrigger and the rate of discharge for the flat part is determined by %RatekW.' +
     CRLF+CRLF+'In I-PeakShave mode, the control attempts to discharge Storage to keep current in the monitored element below the target given in k-amps ' +
     '(thousands of amps), when this control mode is active, the property kWTarget will be expressed in k-amps. ';
    PropertyHelp[propMODECHARGE]          :=
     '{Loadshape | Time* | PeakShaveLow | I-PeakShaveLow} Mode of operation for the CHARGE FUNCTION of this controller. ' +
      CRLF+CRLF+'In Loadshape mode, both charging and discharging precisely follows the per unit loadshape. ' +
     'Storage is charged when the loadshape value is negative. ' +
      CRLF+CRLF+'In Time mode, the Storage charging FUNCTION is triggered at the specified %RateCharge at the specified charge trigger time in fractional hours.' +
      CRLF+CRLF+'In PeakShaveLow mode, the charging operation will charge the Storage fleet when the power at a' +
     'monitored element is below a specified KW target (kWTarget_low). The Storage will charge as much power as necessary to keep the power within the deadband around kWTarget_low.' +
      CRLF+CRLF+'In I-PeakShaveLow mode, the charging operation will charge the Storage fleet when the current (Amps) at a' +
     'monitored element is below a specified amps target (kWTarget_low). The Storage will charge as much power as necessary to keep the amps within the deadband around kWTarget_low. ' +
     'When this control mode is active, the property kWTarget_low will be expressed in k-amps and all the other parameters will be adjusted to match the amps (current) control criteria.';
    PropertyHelp[propTIMEDISCHARGETRIGGER]:=
     'Default time of day (hr) for initiating Discharging of the fleet. During Follow or Time mode discharging is triggered at a fixed time ' +
     'each day at this hour. If Follow mode, Storage will be discharged to attempt to hold the load at or below the power level at the time of triggering. ' +
     'In Time mode, the discharge is based on the %RatekW property value. ' +
     'Set this to a negative value to ignore. Default is 12.0 for Follow mode; otherwise it is -1 (ignored). ';
    PropertyHelp[propTIMECHARGETRIGGER]   :=
     'Default time of day (hr) for initiating charging in Time control mode. Set this to a negative value to ignore. Default is 2.0.  (0200).' +
     'When this value is >0 the Storage fleet is set to charging at this time regardless of other control criteria to make sure Storage is ' +
     'topped off for the next discharge cycle.';
    PropertyHelp[propRATEKW]              :=
      'Sets the kW discharge rate in % of rated capacity for each element of the fleet. Applies to TIME control mode, SCHEDULE mode, or anytime discharging is triggered ' +
      'by time.';
//    PropertyHelp[propRATEKVAR]            :=
//      'Sets the kvar discharge rate in % of rated capacity for each element of the fleet. Applies to TIME control mode or anytime discharging is triggered ' +
//      'by time.' ;
    PropertyHelp[propRATECHARGE]          :=
      'Sets the kW charging rate in % of rated capacity for each element of the fleet. Applies to TIME control mode and anytime charging mode is ' +
      'entered due to a time trigger.';
    PropertyHelp[propRESERVE]             :=
       'Use this property to change the % reserve for each Storage element under control of this controller. This might be used, for example, to ' +
       'allow deeper discharges of Storage or in case of emergency operation to use the remainder of the Storage element.';
    PropertyHelp[propKWHTOTAL]            :=
      '(Read only). Total rated kWh energy Storage capacity of Storage elements controlled by this controller.';
    PropertyHelp[propKWTOTAL]             :=
      '(Read only). Total rated kW power capacity of Storage elements controlled by this controller.';
    PropertyHelp[propKWHACTUAL]            :=
      '(Read only). Actual kWh stored of all controlled Storage elements. ';
    PropertyHelp[propKWACTUAL]            :=
      '(Read only). Actual kW output of all controlled Storage elements. ';
    PropertyHelp[propKWNEED]              :=
      '(Read only). KW needed to meet target.';
//    PropertyHelp[propPARTICIPATION]       :=
//      'Participation factor, %. Default = 100.';
    PropertyHelp[propYEARLY]              :=
      'Dispatch loadshape object, If any, for Yearly solution Mode.';
    PropertyHelp[propDAILY]               :=
      'Dispatch loadshape object, If any, for Daily solution mode.';
    PropertyHelp[propDUTY]                :=
      'Dispatch loadshape object, If any, for Dutycycle solution mode.';
    PropertyHelp[propEVENTLOG]            :=
      '{Yes/True | No/False} Default is No. Log control actions to Eventlog.';
//    PropertyHelp[propVARDISPATCH]         :=
//      '{Yes/True | No/False} Default is No. Flag to indicate whether or not to disatch vars as well as watts.';
    PropertyHelp[propINHIBITTIME]         :=
      'Hours (integer) to inhibit Discharging after going into Charge mode. Default is 5.';
     PropertyHelp[propTUPRAMP]            := 'Duration, hrs, of upramp part for SCHEDULE mode. Default is 0.25.';
     PropertyHelp[propTFLAT]              := 'Duration, hrs, of flat part for SCHEDULE mode. Default is 2.0.';
     PropertyHelp[propTDNRAMP]            := 'Duration, hrs, of downramp part for SCHEDULE mode. Default is 0.25.';
     PropertyHelp[propKWTHRESHOLD]        := 'Threshold, kW, for Follow mode. kW has to be above this value for the Storage element ' +
                                      'to be dispatched on. Defaults to 75% of the kWTarget value. Must reset this property after ' +
                                      'setting kWTarget if you want a different value.';
     PropertyHelp[propDispFactor]         := 'Defaults to 1 (disabled). Set to any value between 0 and 1 to enable this parameter.'
      + CRLF + CRLF + 'Use this parameter to reduce the amount of power requested by the controller in each control iteration. ' +
      'It can be useful when maximum control iterations are exceeded due to numerical instability such as ' +
      'fleet being set to charging and idling in subsequent control iterations (check the Eventlog). ';
     PropertyHelp[propRESETLEVEL]         := 'The level of charge required for allowing the storage to discharge again after reaching ' +
                                      'the reserve storage level. After reaching this level, the storage control  will not allow ' +
                                      'the storage device to discharge, forcing the storage to charge. Once the storage reaches this' +
                                      'level, the storage will be able to discharge again. This value is a number between 0.2 and 1';
     PropertyHelp[propSEASONS]            := 'With this property the user can' +
                                        ' specify the number of targets to be used by the controller using the list given at "SeasonTargets"/' +
                                        '"SeasonTargetsLow", which can be used to dynamically adjust the storage controller during a QSTS' +
                                        ' simulation. The default value is 1. This property needs to be defined before defining SeasonTargets/SeasonTargetsLow.';
     PropertyHelp[propSEASONTARGETS]      := 'An array of doubles specifying the targets to be used during a QSTS simulation. These targets will take effect' +
                                        ' only if SeasonRating=true. The number of targets cannot exceed the number of seasons defined at the SeasonSignal.' +
                                        'The difference between the targets defined at SeasonTargets and SeasonTargetsLow is that SeasonTargets' +
                                        ' applies to discharging modes, while SeasonTargetsLow applies to charging modes.';
     PropertyHelp[propSEASONTARGETSLOW]   := 'An array of doubles specifying the targets to be used during a QSTS simulation. These targets will take effect' +
                                        ' only if SeasonRating=true. The number of targets cannot exceed the number of seasons defined at the SeasonSignal.' +
                                        'The difference between the targets defined at SeasonTargets and SeasonTargetsLow is that SeasonTargets' +
                                        ' applies to discharging modes, while SeasonTargetsLow applies to charging modes.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TStorageController.NewObject(const ObjName:String):Integer;
Begin
    // Make a new StorageController and add it to StorageController class list
    WITH ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TStorageControllerObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;

{--------------------------------------------------------------------------}
FUNCTION TStorageController.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer  : Integer;
   ParamName     : String;
   Param         : String;
   i             : Integer;
   casemult      : Double;

Begin

  // continue parsing with contents of Parser
  ActiveStorageController2Obj := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveStorageController2Obj;

  Result := 0;

  WITH ActiveStorageController2Obj Do Begin

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param := Parser[ActorID].StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer <= NumProperties)
         THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 14407);
            propELEMENT       : ElementName      := lowercase(param);
            propTERMINAL      : ElementTerminal  := Parser[ActorID].IntValue;
            propMONPHASE      : If      CompareTextShortest(param, 'avg') = 0 Then FMonPhase := AVG
                                Else If CompareTextShortest(param, 'max') = 0 Then FMonPhase := MAXPHASE
                                Else If CompareTextShortest(param, 'min') = 0 Then FMonPhase := MINPHASE
                                Else FMonPhase := max(1, Parser[ActorID].IntValue);
            propKWTARGET      : FkWTarget     := Parser[ActorID].DblValue;
            propKWTARGETLOW   : FkWTargetLow  := Parser[ActorID].DblValue;
            propPCTKWBAND     : FpctkWBand    := Parser[ActorID].DblValue;
            propKWBAND        : FkWBand       := Parser[ActorID].DblValue;
            propPCTKWBANDLOW  : FpctkWBandLow := Parser[ActorID].DblValue;
            propKWBANDLOW     : FkWBandLow    := Parser[ActorID].DblValue;
//            propPFTARGET: FPFTarget        := ConvertPFToPFRange2(Parser[ActorID].DblValue);
//            propPFBAND:   FPFBand          := Parser[ActorID].DblValue;
            propELEMENTLIST   : InterpretTStringListArray(Param, FStorageNameList);
            propWEIGHTS       :
                              Begin
                                FleetSize   := FStorageNameList.count;
                                IF FleetSize>0 Then
                                Begin
                                  Reallocmem(FWeights, Sizeof(FWeights^[1])*FleetSize);
                                  FleetSize := InterpretDblArray(Param, FleetSize, FWeights);
                                End;
                              End;
            propMODEDISCHARGE : DisChargeMode := InterpretMode(propMODEDISCHARGE, Param);
            propMODECHARGE    : ChargeMode    := InterpretMode(propMODECHARGE, Param);
            propTIMEDISCHARGETRIGGER: DischargeTriggerTime := Parser[ActorID].DblValue;
            propTIMECHARGETRIGGER:    ChargeTriggerTime    := Parser[ActorID].DblValue;
            propRATEKW        : pctkWRate      := Parser[ActorID].DblValue;
//            propRATEKVAR:    pctkvarRate    := Parser[ActorID].DblValue;
            propRATECHARGE    : pctChargeRate  := Parser[ActorID].DblValue;
            propRESERVE       : pctFleetReserve:= Parser[ActorID].DblValue;
            propKWHTOTAL      : ;  // Do nothing (Read ONly)
            propKWTOTAL       : ;  // Do nothing (Read ONly)
            propKWHACTUAL     : ;  // Do nothing (Read ONly)
            propKWACTUAL      : ;  // Do nothing (Read ONly)
            propKWNEED        : ;  // Do nothing (Read ONly)
//            propPARTICIPATION: ;
            propYEARLY        : YearlyShape  := Param;
            propDAILY         : DailyShape   := Param;
            propDUTY          : DutyShape    := Param;
            propEVENTLOG      : ShowEventLog := InterpretYesNo(Param);
//            propVARDISPATCH: DispatchVars := InterpretYesNo(Param);
            propINHIBITTIME   : Inhibithrs   := Max(1, Parser[ActorID].IntValue);  // >=1
            propTUPRAMP       : UpRamptime   := Parser[ActorID].DblValue;
            propTFLAT         : FlatTime     := Parser[ActorID].DblValue;
            propTDNRAMP       : DnrampTime   := Parser[ActorID].DblValue;
            propKWTHRESHOLD   : FkWThreshold := Parser[ActorID].DblValue;
            propDispFactor    : if (Parser[ActorID].DblValue > 1.0) or (Parser[ActorID].DblValue <= 0.0) then  DispFactor:= 1.0
                                else DispFactor := Parser[ActorID].DblValue;
            propRESETLEVEL    : ResetLevel   := Parser[ActorID].DblValue;
            propSEASONS       : Seasons      := Parser[ActorID].IntValue;
            propSEASONTARGETS :
                            Begin
                              IF Seasons > 1 Then
                              Begin
                                setlength(SeasonTargets, Seasons);
                                Seasons := InterpretDblArray(Param, Seasons, Pointer(SeasonTargets));
                              End;
                            End;
            propSEASONTARGETSLOW :
                            Begin
                              IF Seasons > 1 Then
                              Begin
                                setlength(SeasonTargetsLow, Seasons);
                                Seasons := InterpretDblArray(Param, Seasons, Pointer(SeasonTargetsLow));
                              End;
                            End;
          ELSE
           // Inherited parameters
           ClassEdit(ActiveStorageController2Obj, ParamPointer - NumPropsthisClass)
         End;

         // Side effects of setting properties above

         CASE ParamPointer OF
            propKWTARGET        : Begin
                                if DischargeMode =  CURRENTPEAKSHAVE then  // evaluates the discharging mode to apply
                                  Casemult      :=  1000.0                 // a compensation value (for kamps)
                                Else
                                  Casemult      :=  1.0;

                                FkWThreshold     := FkWTarget * 0.75 * Casemult;

                                HalfkWBand       := FpctkWBand / 200.0 * FkWTarget * Casemult;
                                FkWBand          := 2.0 * HalfkWBand;
                                FpctkWBand       := FkWBand / FkWTarget * 100.0; // sync FpctkWBand

                              End;
            propPCTKWBAND      : Begin
                                if DischargeMode =  CURRENTPEAKSHAVE then  // evaluates the discharging mode to apply
                                  Casemult      :=  1000.0                 // a compensation value (for kamps)
                                Else
                                  Casemult      :=  1.0;

                                  HalfkWBand       := FpctkWBand / 200.0 * FkWTarget * Casemult;
                                  FkWBand          := 2.0 * HalfkWBand;
                                  FkWBandSpecified := FALSE;

                              End;
            propKWBAND         : Begin
                                if DischargeMode =  CURRENTPEAKSHAVE then  // evaluates the discharging mode to apply
                                  Casemult      :=  1000.0                 // a compensation value (for kamps)
                                Else
                                  Casemult      :=  1.0;

                                HalfkWBand       := FkWBand / 2.0 * Casemult;
                                FpctkWBand       := FkWBand / FkWTarget * 100.0; // sync FpctkWBand
                                FkWBandSpecified := TRUE;
                              End;
            propKWTARGETLOW,
            propPCTKWBANDLOW   : Begin
                                if ChargeMode =  CURRENTPEAKSHAVELOW then  // evaluates the charging mode to apply
                                  Casemult      :=  1000.0                 // a compensation value (for kamps)
                                Else
                                  Casemult      :=  1.0;

                                HalfkWBandLow := FpctkWBandLow / 200.0 * FkWTargetLow * Casemult;
                                FkWBandLow    := HalfkWBandLow * 2.0;
                              End;
            propKWBANDLOW      : Begin
                                if ChargeMode =  CURRENTPEAKSHAVELOW then  // evaluates the charging mode to apply
                                  Casemult      :=  1000.0                 // a compensation value (for kamps)
                                Else
                                  Casemult      :=  1.0;

                                HalfkWBandLow := FkWBandLow / 2.0 * Casemult;
                                FpctkWBand    := FkWBandLow / FkWTarget * 100.0; // sync FpctkWBandLow
                              End;
//            propPFBAND: HalfPFBand := FPFBand / 2.0;
            propMODEDISCHARGE: If DischargeMode = MODEFOLLOW Then  DischargeTriggerTime := 12.0; // Noon

            propMONPHASE:   If FMonPhase > FNphases Then Begin
                                 DoSimpleMsg(Format('Error: Monitored phase(%d) must be less than or equal to number of phases(%d). ', [FMonPhase, FNphases]), 35302);
                                 FMonPhase := 1;
                            End;

            propELEMENTLIST:
              Begin   // levelize the list
                FleetPointerList.Clear;  // clear this for resetting on first sample
                FleetListChanged := TRUE;
                FElementListSpecified := TRUE;
                FleetSize := FStorageNameList.count;
                // Realloc weights to be same size as possible number of storage elements
                Reallocmem(FWeights, Sizeof(FWeights^[1])*FleetSize);
                For i := 1 to FleetSize Do FWeights^[i] := 1.0;
              End;
            propYEARLY:
              Begin
                YearlyShapeObj := LoadShapeClass[ActorID].Find(YearlyShape);
                If YearlyShapeObj = nil Then  DoSimpleMsg('Yearly loadshape "' + YearlyShape + '" not found.', 14404);
              End;
            propDAILY:
              Begin
                DailyShapeObj  := LoadShapeClass[ActorID].Find(DailyShape);
                If DailyShapeObj = nil Then  DoSimpleMsg('Daily loadshape "' + DailyShape + '" not found.', 14405);
              End;
            propDUTY:
              Begin
                DutyShapeObj   := LoadShapeClass[ActorID].Find(DutyShape);
                If DutyShapeObj = nil Then  DoSimpleMsg('Dutycycle loadshape "' + DutyShape + '" not found.', 14406);
              End

         ELSE

         END;

         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
     End;

     RecalcElementData(ActorID);
  End;

End;

{--------------------------------------------------------------------------}
FUNCTION TStorageController.MakeLike(const StorageController2Name:String):Integer;
VAR
   OtherStorageController2:TStorageControllerObj;
   i:Integer;
Begin
   Result := 0;
   {See If we can find this StorageController name in the present collection}
   OtherStorageController2 := Find(StorageController2Name);
   IF OtherStorageController2 <> Nil THEN
   WITH ActiveStorageController2Obj Do Begin

        NPhases               := OtherStorageController2.Fnphases;
        NConds                := OtherStorageController2.Fnconds; // Force Reallocation of terminal stuff

        ElementName           := OtherStorageController2.ElementName;
        ControlledElement     := OtherStorageController2.ControlledElement;  // Pointer to target circuit element
        MonitoredElement      := OtherStorageController2.MonitoredElement;  // Pointer to target circuit element
        ElementTerminal       := OtherStorageController2.ElementTerminal;
        FMonPhase             := OtherStorageController2.FMonPhase;
        CondOffset            := OtherStorageController2.CondOffset;

        FkWTarget             := OtherStorageController2.FkWTarget;
        FkWTargetLow          := OtherStorageController2.FkWTargetLow;
        FkWThreshold          := OtherStorageController2.FkWThreshold;

        DispFactor            := OtherStorageController2.DispFactor;

        FpctkWBand            := OtherStorageController2.FpctkWBand;
        FkWBand               := OtherStorageController2.FkWBand;
        FpctkWBandLow         := OtherStorageController2.FpctkWBandLow;
        FkWBandLow            := OtherStorageController2.FkWBandLow;
//        FPFTarget             := OtherStorageController.FPFTarget;
//        FPFBand               := OtherStorageController.FPFBand;
//        HalfPFBand            := OtherStorageController.HalfPFBand;
        ResetLevel            := OtherStorageController2.ResetLevel;

        FkWBandSpecified      := OtherStorageController2.FkWBandSpecified;
        FStorageNameList.Clear;
        For i := 1 to OtherStorageController2.FStorageNameList.Count Do
              FStorageNameList.Add(OtherStorageController2.FStorageNameList.Strings[i-1] );

        FleetSize := FStorageNameList.count;
        IF FleetSize>0 Then
        Begin
            Reallocmem(FWeights, Sizeof(FWeights^[1])*FleetSize);
            For i := 1 to FleetSize Do  FWeights^[i] := OtherStoragecontroller2.FWeights^[i];
        End;

        DisChargeMode         := OtherStorageController2.DisChargeMode;
        ChargeMode            := OtherStorageController2.ChargeMode;
        DischargeTriggerTime  := OtherStorageController2.DischargeTriggerTime;
        ChargeTriggerTime     := OtherStorageController2.ChargeTriggerTime;
        pctkWRate             := OtherStorageController2.pctkWRate;
//        pctkvarRate           := OtherStorageController.pctkvarRate;
        pctChargeRate         := OtherStorageController2.pctChargeRate;
        pctFleetReserve       := OtherStorageController2.pctFleetReserve;
        YearlyShape           := OtherStorageController2.YearlyShape;
        DailyShape            := OtherStorageController2.DailyShape;
        DutyShape             := OtherStorageController2.DutyShape;
//        DispatchVars          := OtherStorageController.DispatchVars;
        ShowEventLog          := OtherStorageController2.ShowEventLog;
        Inhibithrs            := OtherStorageController2.Inhibithrs;

        UpRamptime            := OtherStorageController2.UpRamptime;
        FlatTime              := OtherStorageController2.FlatTime;
        DnrampTime            := OtherStorageController2.DnrampTime;

        Seasons               := OtherStorageController2.Seasons;
        IF Seasons > 1 Then
        Begin
            setlength(SeasonTargets, Seasons);
            setlength(SeasonTargetsLow, Seasons);
            For i := 0 to (Seasons - 1) Do
            Begin
              SeasonTargets[i]    := OtherStoragecontroller2.SeasonTargets[i];
              SeasonTargetsLow[i] := OtherStoragecontroller2.SeasonTargetsLow[i];
            End;
        End;


        //**** fill in private properties

        For i := 1 to ParentClass.NumProperties Do
           // Skip Read only properties
           Case i of
                propKWHTOTAL      :; {Do Nothing}
                propKWTOTAL       :; {Do Nothing}
                propKWHACTUAL     :; {Do Nothing}
                propKWACTUAL      :; {Do Nothing}
                propKWNEED        :; {Do Nothing}
           Else
             PropertyValue[i] := OtherStorageController2.PropertyValue[i];
           End;


   End
   ELSE  DoSimpleMsg('Error in StorageController MakeLike: "' + StorageController2Name + '" Not Found.', 370);

End;




{==========================================================================}
{                    TStorageControllerObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TStorageControllerObj.Create(ParClass:TDSSClass; const StorageController2Name:String);

Begin
     Inherited Create(ParClass);
     Name                     := LowerCase(StorageController2Name);
     DSSObjType               := ParClass.DSSClassType;

     NPhases                  := 3;  // Directly set conds and phases
     Fnconds                  := 3;
     Nterms                   := 1;  // this forces allocation of terminals and conductors

     ElementName              := '';
     ControlledElement        := nil;    // not used in this control
     ElementTerminal          := 1;
     MonitoredElement         := Nil;
     FMonPhase                := MAXPHASE;
     cBuffer                  := Nil; // Complex buffer

     FStorageNameList         := TSTringList.Create;
     FWeights                 := Nil;
     FleetPointerList         := PointerList.TPointerList.Create(20);  // Default size and increment
     FleetSize                := 0;
     FleetState               := STORE_IDLING;
     FkWTarget                := 8000.0;
     FkWTargetLow             := 4000.0;
     FkWThreshold             := 6000.0;
     DispFactor               := 1.0;
     FpctkWBand               := 2.0;
     FpctkWBandLow            := 2.0;
     HalfkWBand               := FpctkWBand/200.0 * FkWTarget;
     HalfkWBandLow            := FpctkWBandLow/200.0 * FkWTargetLow;
     FkWBand                  := HalfkWBand * 2.0;
     FkWBandLow               := HalfkWBandLow * 2.0;
     TotalWeight              := 1.0;

//     FPFTarget                := 0.96;
//     FPFBand                  := 0.04;
//     HalfPFBand               := FPFBand / 2.0;
     kWNeeded                 := 0.0;

     DischargeMode            := MODEPEAKSHAVE;
     ChargeMode               := MODETIME;

     DischargeTriggerTime     := -1.0;  // disabled
     ChargeTriggerTime        := 2.0;   // 2 AM
     FElementListSpecified    := FALSE;
     FleetListChanged         := TRUE;  // force building of list
     FkWBandSpecified         := FALSE;  // adopt pctkWBand by default

     pctkWRate                := 20.0;
//     pctkvarRate              := 20.0;
     pctChargeRate            := 20.0;
     pctFleetReserve          := 25.0;

//     DispatchVars             := FALSE;
     DischargeTriggeredByTime := FALSE;
     DischargeInhibited       := FALSE;
     OutOfOomph               := FALSE;
     InhibitHrs               := 5;   // No. Hours to inhibit discharging after going into charge mode

     UpRamptime               := 0.25; // hr
     FlatTime                 := 2.0;
     DnrampTime               := 0.25;
     LastpctDischargeRate     := 0.0;
     Wait4Step                := False;     // for sync discharge with charge when there is a transition
     ResetLevel               := 0.8;
     Seasons                  := 1;         // For dynamic targets
     setlength(SeasonTargets,1);
     SeasonTargets[0]         := FkWTarget;
     setlength(SeasonTargetsLow,1);
     SeasonTargetsLow[0]      := FkWTargetLow;
     pctVpu                   :=  0.8;

     InitPropertyValues(0);

End;

destructor TStorageControllerObj.Destroy;
Begin
     ElementName := '';
     YearlyShape := '';
     DailyShape  := '';
     DutyShape   := '';

     if Assigned(cBuffer) then ReallocMem (cBuffer, 0);

(*    Don't Do this here!! Disposes of actual object;
       YearlyShapeObj.Free;
       DailyShapeObj.Free;
       DutyShapeObj.Free;
*)


     FleetPointerList.Free;
     FStorageNameList.Free;

     Inherited Destroy;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.InitPropertyValues(ArrayOffset: Integer);
Begin


     PropertyValue[propELEMENT]              :='';
     PropertyValue[propTERMINAL]             :='1';
     PropertyValue[propMONPHASE]             :='MAX';
     PropertyValue[propKWTARGET]             :='8000';
     PropertyValue[propKWTARGETLOW]          :='4000';
     PropertyValue[propPCTKWBAND]               :='2';
     PropertyValue[propPCTKWBANDLOW]            :='2';
//     PropertyValue[propPFTARGET]             :='.96';
//     PropertyValue[propPFBAND]               :='.04';
     PropertyValue[propELEMENTLIST]          :='';
     PropertyValue[propWEIGHTS]              :='';
     PropertyValue[propMODEDISCHARGE]        :='Follow';
     PropertyValue[propMODECHARGE]           :='Time';
     PropertyValue[propTIMEDISCHARGETRIGGER] :='-1';
     PropertyValue[propTIMECHARGETRIGGER]    :='2';
     PropertyValue[propRATEKW]               :='20';
//     PropertyValue[propRATEKVAR]             :='20';
     PropertyValue[propRATECHARGE]           :='20';
     PropertyValue[propRESERVE]              :='25';
     PropertyValue[propKWHTOTAL]             :='';
     PropertyValue[propKWTOTAL]              :='';
     PropertyValue[propKWACTUAL]             :='';
     PropertyValue[propKWNEED]               :='';
//     PropertyValue[propPARTICIPATION]        :='';
     PropertyValue[propYEARLY]               :='';
     PropertyValue[propDAILY]                :='';
     PropertyValue[propDUTY]                 :='';
     PropertyValue[propEVENTLOG]             :='No';
     PropertyValue[propINHIBITTIME]          := '5';
     PropertyValue[propTUPRAMP]              := '0.25';
     PropertyValue[propTFLAT]                := '2.0';
     PropertyValue[propTDNRAMP]              := '0.25';
     PropertyValue[propKWTHRESHOLD]          := '4000';
     PropertyValue[propDispFactor]           := '1.0';
     PropertyValue[propRESETLEVEL]           := '0.8';
     PropertyValue[propSEASONS]              := '1';
     PropertyValue[propSEASONTARGETS]        := '[8000,]';
     PropertyValue[propSEASONTARGETSLOW]     := '[4000,]';


  inherited  InitPropertyValues(NumPropsThisClass);

End;

FUNCTION TStorageControllerObj.GetPropertyValue(Index: Integer): String;
Begin
     Result := '';
     CASE Index of

          propMONPHASE             : if      FMonPhase = AVG         then Result := 'AVG'
                                     else if FMonPhase = MAXPHASE    then Result := 'MAX'
                                     else if FMonPhase = MINPHASE    then Result := 'MIN'
                                     else                                 Result := Format('%d',[FMonPhase]);
          propKWTARGET             : Result := Format('%-.6g',[FkWTarget]);
          propKWTARGETLOW          : Result := Format('%-.6g',[FkWTargetLow]);
          propPCTKWBAND            : Result := Format('%-.6g',[FpctkWBand]);
          propKWBAND               : Result := Format('%-.6g',[FkWBand]);
          propPCTKWBANDLOW         : Result := Format('%-.6g',[FpctkWBandLow]);
          propKWBANDLOW            : Result := Format('%-.6g',[FkWBandLow]);
//          propPFTARGET             : Result := Format('%-.6g',[ConvertPFRange2ToPF(FPFTarget)]);
//          propPFBAND               : Result := Format('%-.6g',[FPFBand]);
          propELEMENTLIST          : Result := ReturnElementsList;
          propWEIGHTS              : Result := ReturnWeightsList;
          propMODEDISCHARGE        : Result := GetModeString(propMODEDISCHARGE, DischargeMode);
          propMODECHARGE           : Result := GetModeString(propMODECHARGE,    ChargeMode);
          propTIMEDISCHARGETRIGGER : Result := Format('%.6g', [DisChargeTriggerTime]);
          propTIMECHARGETRIGGER    : Result := Format('%.6g', [ChargeTriggerTime]);
          propRATEKW               : Result := Format('%-.8g',[pctkWRate]);
//          propRATEKVAR             : Result := Format('%-.8g',[pctkvarRate]);
          propRATECHARGE           : Result := Format('%-.8g',[pctChargeRate]);
          propRESERVE              : Result := Format('%-.8g',[pctFleetReserve]);
          propKWHTOTAL             : Result := GetkWhTotal(TotalkWhCapacity);
          propKWTOTAL              : Result := GetkWTotal(TotalkWCapacity);
          propKWHACTUAL            : Result := GetkWhActual;
          propKWACTUAL             : Result := GetkWActual;
          propKWNEED               : Result := Format('%-.6g',[kWNeeded]);
          {propPARTICIPATION        : Result := PropertyValue[Index]; }
          propYEARLY               : Result := YearlyShape;
          propDAILY                : Result := DailyShape;
          propDUTY                 : Result := DutyShape;
          propEVENTLOG             : If ShowEventLog Then Result := 'Yes' Else Result := 'No';
//          propVARDISPATCH          : If DispatchVars Then Result := 'Yes' Else Result := 'No';
          propINHIBITTIME          : Result := Format('%d', [InhibitHrs]);
          propTUPRAMP              : Result := Format('%.6g', [UpRamptime]);
          propTFLAT                : Result := Format('%.6g', [FlatTime]);
          propTDNRAMP              : Result := Format('%.6g', [DnrampTime]);
          propKWTHRESHOLD          : Result := Format('%.6g', [FkWThreshold]);
          propDispFactor           : Result := Format('%.6g', [DispFactor]);
          propRESETLEVEL           : Result := Format('%.6g', [ResetLevel]);
          propSEASONS              : Result := Format('%d', [seasons]);
          propSEASONTARGETS        : Result := ReturnSeasonTarget(1);
          propSEASONTARGETSLOW     : Result := ReturnSeasonTarget(0);

     ELSE  // take the generic handler
           Result := Inherited GetPropertyValue(index);

     END;
End;

function TStorageControllerObj.Get_FleetkW: Double;

VAR
    pStorage:TStorageObj;
    i       :Integer;
Begin
      Result := 0.0;
      for I := 1 to FleetPointerList.ListSize Do Begin
          pStorage :=  FleetPointerList.Get(i);
          Result := Result + pStorage.PresentkW;
      End;
end;

function TStorageControllerObj.Get_FleetkWh: Double;
VAR
    pStorage:TStorageObj;
    i       :Integer;
Begin
      Result := 0.0;
      for I := 1 to FleetPointerList.ListSize Do Begin
          pStorage :=  FleetPointerList.Get(i);
          Result := Result + pStorage.StorageVars.kWhStored;
      End;
end;

function TStorageControllerObj.Get_FleetkWhRating: Double;
VAR
    pStorage:TStorageObj;
    i       :Integer;
Begin
      Result := 0.0;
      for I := 1 to FleetPointerList.ListSize Do Begin
          pStorage :=  FleetPointerList.Get(i);
          Result := Result + pStorage.StorageVars.kWhRating;
      End;
end;

function TStorageControllerObj.Get_FleetReservekWh: Double;
VAR
    pStorage:TStorageObj;
    i       :Integer;
Begin
      Result := 0.0;
      for I := 1 to FleetPointerList.ListSize Do Begin
          pStorage :=  FleetPointerList.Get(i);
          Result := Result + pStorage.StorageVars.kWhReserve;
      End;

end;

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.RecalcElementData(ActorID : Integer);

// Recalculate critical element values after changes have been made

VAR
   DevIndex :Integer;

Begin

        {Check for existence of monitored element}

         Devindex := GetCktElementIndex(ElementName); // Global FUNCTION
         IF   DevIndex>0  THEN Begin
             MonitoredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
             IF ElementTerminal > MonitoredElement.Nterms
             THEN Begin
                 DoErrorMsg('StorageController: "' + Name + '"',
                                 'Terminal no. "' +'" Does not exist.',
                                 'Re-specify terminal no.', 371);
             End
             ELSE Begin
                 Nphases := MonitoredElement.Nphases;
                 NConds  := FNphases;

                 // Sets name of i-th terminal's connected bus in StorageController's buslist
                 Setbus(1, MonitoredElement.GetBus(ElementTerminal));

                 // Allocate a buffer bigenough to hold everything from the monitored element
                 ReAllocMem(cBuffer, SizeOF(cBuffer^[1]) * MonitoredElement.Yorder);
                 CondOffset := (ElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling
             End;
         End
         ELSE DoSimpleMsg('Monitored Element in StorageController.'+Name+ ' Does not exist:"'+ElementName+'"', 372);

       If FleetListChanged Then
         If Not MakeFleetList Then DoSimpleMsg('No unassigned Storage Elements found to assign to StorageController.'+Name, 37201);

       GetkWTotal(TotalkWCapacity);
       GetkWhTotal(TotalkWhCapacity);

       If FleetSize > 0 Then
       Begin
            SetFleetToExternal;
            SetAllFleetValues;
       End;

       UpPlusFlat := UpRampTime + FlatTime;
       UpPlusFlatPlusDn := UpPlusFlat + DnRampTime;

End;

procedure TStorageControllerObj.MakePosSequence(ActorID : Integer);
begin
  if MonitoredElement <> Nil then begin
    Nphases   := MonitoredElement.NPhases;
    Nconds    := FNphases;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    // Allocate a buffer bigenough to hold everything from the monitored element
    ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder );
    CondOffset := (ElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling
  end;
  inherited;
end;

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.CalcYPrim(ActorID : Integer);
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);


End;


{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;

PROCEDURE TStorageControllerObj.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);
VAR
     i:Integer;
Begin
     FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

FUNCTION TStorageControllerObj.GetkWActual: String;
Begin
      Result := Format('%-.8g',[FleetkW]);
End;

FUNCTION TStorageControllerObj.GetkWhActual: String;
Begin
      Result := Format('%-.8g',[FleetkWh]);
End;

FUNCTION TStorageControllerObj.GetkWhTotal(Var Sum:Double): String;
VAR
    pStorage  : TStorageObj;
    i         : Integer;

Begin
      Sum := 0.0;
      for i := 1 to FleetPointerList.ListSize Do Begin
          pStorage  :=  FleetPointerList.Get(i);
          sum       :=  sum + pStorage.StorageVars.kWhRating;
      End;
      Result := Format('%-.8g',[sum]);
End;

FUNCTION TStorageControllerObj.GetkWTotal(Var Sum:Double): String;
VAR
    pStorage  : TStorageObj;
    i         : Integer;

Begin
      Sum := 0.0;
      for i := 1 to FleetPointerList.ListSize Do Begin
          pStorage  :=  FleetPointerList.Get(i);
          sum       :=  sum + pStorage.StorageVars.kWRating;
      End;
      Result := Format('%-.8g',[sum]);
End;

FUNCTION TStorageControllerObj.GetModeString(Opt, Mode: Integer): String;
Begin
      Result := '';
      CASE Opt of
          propMODEDISCHARGE:
               CASE Mode of
                    MODEFOLLOW          : Result := 'Follow';
                    MODELOADSHAPE       : Result := 'Loadshape';
                    MODESUPPORT         : Result := 'Support';
                    MODETIME            : Result := 'Time';
                    MODEPEAKSHAVE       : Result := 'Peakshave';
                    CURRENTPEAKSHAVE    : Result := 'I-Peakshave';
               ELSE
                    Result               := 'UNKNOWN'
               END;
          propMODECHARGE:
               CASE Mode of
                   // 1: Result := 'Follow';
                    MODELOADSHAPE       : Result := 'Loadshape';
                  //  3: Result := 'Support';
                    MODETIME            : Result := 'Time';
                    MODEPEAKSHAVELOW    : Result := 'PeakshaveLow';
                    CURRENTPEAKSHAVELOW : Result := 'I-PeakShaveLow';
               ELSE
                    Result              := 'UNKNOWN'
               END;
      ELSE
           DoSimpleMsg('Unknown Charge/Discharge designation', 14401);
      END;
End;

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.DumpProperties(VAR F:TextFile; Complete:Boolean);

VAR
   i:Integer;

Begin
    Inherited DumpProperties(F,Complete);

    WITH ParentClass Do
     For i := 1 to NumProperties Do
     Begin
          Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    If Complete THEN
    Begin
          Writeln(F);
    End;

End;

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer);
Begin

        {
           Release  the discharge inhibit .
           Do nothing for other codes
        }

        If (Code = RELEASE_INHIBIT) and (DischargeMode <> MODEFOLLOW)
        Then DischargeInhibited := FALSE;
        
End;

procedure TStorageControllerObj.DoScheduleMode(ActorID : Integer);
{
  In SCHEDULE mode we ramp up the storage from zero to the specified pctkWRate.
  This value is held for the flattime or until they  turn themselves
  off when they are either fully discharged, or ramped down

  The discharge trigger time must be greater than 0
}

Var
   TDiff :Double;
   pctDischargeRate :Double;
Begin
       pctDischargeRate := 0.0;   // init for test
       If (DisChargeTriggerTime > 0.0)  Then
         WITH ActiveCircuit[ActorID].Solution Do
         Begin
               // turn on if time within 1/2 time step
               If Not (FleetState=STORE_DISCHARGING) Then
               Begin
                    ChargingAllowed := TRUE;
                    TDiff := NormalizeToTOD(DynaVars.intHour, DynaVars.t) - DisChargeTriggerTime;
                    If abs(TDiff) < DynaVars.h/7200.0 Then
                    Begin
                        {Time is within 1 time step of the trigger time}
                          If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Discharging (up ramp) by Schedule',ActorID);
                          SetFleetToDischarge;
                          SetFleetDesiredState(STORE_DISCHARGING);
                          ChargingAllowed := FALSE;
                          pctDischargeRate :=  min(pctkWRate, max(pctKWRate * Tdiff/UpRampTime, 0.0));
                          SetFleetkWRate(pctDischargeRate);
                          DischargeInhibited := FALSE;
                          PushTimeOntoControlQueue(STORE_DISCHARGING, ActorID);
                    End;
               End
               Else Begin    // fleet is already discharging
                    TDiff := NormalizeToTOD(DynaVars.intHour, DynaVars.t) - DisChargeTriggerTime;
                    If TDiff < UpRampTime Then
                    Begin

                          pctDischargeRate :=  min(pctkWRate, max(pctKWRate * Tdiff/UpRampTime, 0.0));
                          SetFleetDesiredState(STORE_DISCHARGING);

                          If pctDischargeRate <> LastpctDischargeRate Then
                          Begin
                            SetFleetkWRate(pctDischargeRate);
                            SetFleetToDischarge;
                          End;

                    end
                    Else
                    Begin

                          If TDiff < UpPlusFlat Then  Begin

                              pctDischargeRate := pctkWRate;
                              SetFleetDesiredState(STORE_DISCHARGING);
                              If PctDischargeRate <> LastpctDischargeRate Then
                                 SetFleetkWRate(pctkWRate);  // on the flat part

                          End Else If TDiff > UpPlusFlatPlusDn Then Begin

                              SetFleetToIdle;
                              ChargingAllowed := TRUE;
                              pctDischargeRate := 0.0;
                              If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Idling by Schedule',ActorID);

                          End Else Begin  // We're on the down ramp

                              TDiff := UpPlusFlatPlusDn - TDiff;
                              pctDischargeRate :=  max(0.0, min(pctKWRate * Tdiff/DnRampTime, pctKWRate));
                              SetFleetDesiredState(STORE_DISCHARGING);
                              SetFleetkWRate(pctDischargeRate);

                          End;

                    End;

                    If pctDischargeRate <> LastpctDischargeRate Then PushTimeOntoControlQueue(STORE_DISCHARGING, ActorID);

               End;  {If not fleetstate ...}
         End;
         LastpctDischargeRate := pctDischargeRate;   // remember this value
end;

PROCEDURE TStorageControllerObj.DoTimeMode(Opt: Integer; ActorID : Integer);
{
  In Time mode we need to only turn the storage elements on. They will turn themselves
  off when they are either fully discharged, fully charged, or receive another command
  from the controller
}
var
   RemainingkWh,
   ReservekWh,
   TotalRatingkWh : Double;
Begin

      TotalRatingkWh := FleetkWhRating;
      RemainingkWh   := FleetkWh;
      ReservekWh     := FleetReservekWh;


      CASE Opt of

          1:Begin
             If (DisChargeTriggerTime > 0.0)  Then
               WITH ActiveCircuit[ActorID].Solution Do
               Begin
                 // turn on if time within 1/2 time step
                 If abs(NormalizeToTOD(DynaVars.intHour, DynaVars.t) - DisChargeTriggerTime) < DynaVars.h/7200.0 Then
                 Begin
                     SetFleetDesiredState(STORE_DISCHARGING);
                     If Not (FleetState=STORE_DISCHARGING) and (RemainingkWh > ReservekWh) Then
                     Begin
                        {Time is within 1 time step of the trigger time}
                          If ShowEventLog Then  AppendToEventLog('StorageController1.' + Self.Name, 'Fleet Set to Discharging by Time Trigger',ActorID);
                          SetFleetToDischarge;
                          SetFleetkWRate(pctKWRate);
                          DischargeInhibited := FALSE;
                          If DischargeMode = MODEFOLLOW Then  DischargeTriggeredByTime := TRUE
                          Else
                              PushTimeOntoControlQueue(STORE_DISCHARGING, ActorID);
                     End;
                 End
                 Else ChargingAllowed := TRUE;
               End;
            End; // Discharge mode
          2:Begin
            If ChargeTriggerTime > 0.0 Then
               WITH ActiveCircuit[ActorID].Solution Do Begin
                 If abs(NormalizeToTOD(DynaVars.intHour, DynaVars.t) - ChargeTriggerTime) < DynaVars.h/7200.0 Then
                 Begin
                   SetFleetDesiredState(STORE_CHARGING);
                   If Not (FleetState=STORE_CHARGING) and (RemainingkWh < TotalRatingkWh) Then
                     Begin
                          {Time is within 1 time step of the trigger time}
                          If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Charging by Time Trigger',ActorID);
                          SetFleetToCharge;
                          DischargeInhibited := TRUE;
                          OutOfOomph         := FALSE;
                          PushTimeOntoControlQueue(STORE_CHARGING, ActorID);   // force re-solve at this time step
                          // Push message onto control queue to release inhibit at a later time
                          With ActiveCircuit[ActorID]  Do  Begin
                                Solution.LoadsNeedUpdating := TRUE; // Force recalc of power parms
                                ControlQueue.Push(DynaVars.intHour+InhibitHrs, Dynavars.t, RELEASE_INHIBIT, 0, Self, ActorID);
                          End;
                     End;
                 End;
               End;
            End; //Charge mode
      END;

End;

//----------------------------------------------------------------------------
FUNCTION TStorageControllerObj.NormalizeToTOD(h: Integer; sec: Double): Double;
// Normalize time to a floating point number representing time of day If Hour > 24
// time should be 0 to 23.999999....
VAR
    HourOfDay :Integer;

Begin

   IF    h > 23
   THEN  HourOfDay := (h - (h div 24)*24)
   ELSE  HourOfDay := h;

   Result := HourOfDay + sec/3600.0;

   If  Result >= 24.0 THEN Result := Result - 24.0;   // Wrap around

End;


procedure TStorageControllerObj.PushTimeOntoControlQueue(Code: Integer; ActorID : Integer);
{
   Push present time onto control queue to force re solve at new dispatch value
}
begin
      With ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution Do
      Begin
            LoadsNeedUpdating := TRUE; // Force recalc of power parms
            ControlQueue.Push(DynaVars.intHour, DynaVars.t, Code, 0, Self, ActorID);
      End;

end;

{--------------------------------------------------------------------------}
Function TStorageControllerObj.Get_DynamicTarget(THigh : Integer; ActorID : Integer): Double;
var
  Temp , temp2       : Double;
  RatingIdx   : Integer;
  RSignal     : TXYCurveObj;
Begin
  RatingIdx:=0;Result:=0.0;
  if SeasonSignal <> '' then
  Begin
    RSignal     :=  XYCurveClass[ActorID].Find(SeasonSignal);
    if RSignal <> nil then
      RatingIdx :=  trunc(RSignal.GetYValue(ActiveCircuit[ActorID].Solution.DynaVars.intHour));

    if (RatingIdx <= Seasons) and (Seasons > 1) then
    Begin
      if THigh = 1 then
        Result    :=  SeasonTargets[RatingIdx]
       else
        Result    :=  SeasonTargetsLow[RatingIdx]
    End
    else
    Begin
      if THigh = 1 then
        Result    :=  FkWTarget
      else
        Result    :=  FkWTargetLow
    End;
  End;
End;


{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.DoLoadFollowMode(ActorID : Integer);

Var
   i                  :Integer;
   S                  :Complex ;
   StorageObj         :TStorageObj;
   StorekWChanged,
   StorekvarChanged,
   SkipkWDispatch     :Boolean;
   VoltsArr           :pComplexArray;
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
   ActualkWDispatch   :Double;

Begin
     AmpsDiff:=0.0;

     // If list is not defined, go make one from all storage elements in circuit
     IF FleetPointerList.ListSize=0 Then  MakeFleetList;

     If FleetSize>0 Then
     Begin

       StorekWChanged   := FALSE;
       StorekvarChanged := FALSE;
       SkipkWDispatch   := FALSE;

       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
       if DischargeMode = CURRENTPEAKSHAVE then
       Begin

          MonitoredElement.GetCurrents(cBuffer, ActorID);
          GetControlCurrent(Amps);

//          Amps := MonitoredElement.MaxCurrent[ElementTerminal,ActorID]; // Max current in active terminal  // old
       end
       else GetControlPower(S, ActorID);

       // In case of having seasonal targets
       if SeasonalRating then
        CtrlTarget  :=  Get_DynamicTarget(1, ActorID)
       else
        CtrlTarget  :=  FkWTarget;


       CASE  DischargeMode of
             // Following Load; try to keep load below kW Target
             MODEFOLLOW:        Begin
                                  If DischargeTriggeredByTime Then Begin
                                    If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name,
                                      Format('Fleet Set to Discharging by Time Trigger; Old kWTarget = %-.6g; New = %-.6g',[FkwTarget, S.re * 0.001]),ActorID);
                                    FkwTarget   := Max(FkWThreshold, S.re * 0.001);  // Capture present kW and reset target
                                    if Not FkWBandSpecified then HalfkWBand  := FpctkWBand / 200.0 * FkWTarget;  // Update band to new target if absolute kWBand hasn`t been specified
                                    DischargeTriggeredByTime := FALSE;  // so we don't come back in here right away
                                    SetFleetToIdle;
                                    SetFleetDesiredState(STORE_IDLING);
                                  End;
                                  PDiff         := S.re * 0.001 - FkWTarget;  // Assume S.re is normally positive
//                                  PFDiff        := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for peak shaving
                                End;
             // supporting DG; Try to keep load above kW target
             MODESUPPORT:       Begin
                                  PDiff         := S.re * 0.001 + FkWTarget;  // assume S.re is normally negative
//                                  PFDiff        := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for generator
                                End;

             MODEPEAKSHAVE:     Begin
                                  PDiff         := S.re * 0.001 - CtrlTarget;  // Assume S.re is normally positive
//                                  PFDiff        := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for peak shaving
                                End;

             CURRENTPEAKSHAVE:  Begin
                                  PDiff         := Amps - CtrlTarget * 1000;  // Gets the difference in terms of amps
//                                  DispatchVars  :=  False;
                                End;
       ELSE
           PDiff := 0.0;
       END;

       if Dischargemode = CURRENTPEAKSHAVE then    // convert Pdiff from Amps to kW
       Begin
         MonitoredElement.ComputeVterminal(ActorID);
         VoltsArr     :=  MonitoredElement.Vterminal;
         ElemVolts    :=  cabs(VoltsArr^[1]);
         kWNeeded     :=  ((MonitoredElement.NPhases * Pdiff * ElemVolts) / 1000.0);
//         kWNeeded     :=  ((Pdiff * ElemVolts) / 1000.0);
         AmpsDiff     := PDiff;
       End
       else
//         kWNeeded := Pdiff + FleetkW;
         kWNeeded := Pdiff;

       {  kW dispatch  }

       // Check if Fleet is Idling (FleetState is updated only if entire fleet is idling)
       if Not (FleetState=STORE_IDLING) then
       Begin
         For i := 1 to FleetSize Do Begin
            StorageObj := FleetPointerList.Get(i);
            if StorageObj.StorageState <> STORE_IDLING Then Break;
            if i = FleetSize then FleetState := STORE_IDLING;
         End;
       End;

       If DischargeInhibited  Then
           SkipkWDispatch   := TRUE
       Else Begin
//         If FleetState = STORE_CHARGING Then
//                   Begin
//                     if Not (Dischargemode = CURRENTPEAKSHAVE) then
//                      Pdiff :=  Pdiff + FleetkW  // ignore overload due to charging
//                     else
//                     Begin
//                       MonitoredElement.ComputeVterminal(ActorID);
//                       VoltsArr     :=  MonitoredElement.Vterminal;
//                       ElemVolts    :=  cabs(VoltsArr^[1]);
//                       Pdiff        :=  Pdiff + (FleetkW * 1000 / ElemVolts);
//                     End;
//                   end;

//           If Not (FleetState = STORE_DISCHARGING) Then  // ignore overload due to charging or idling (trickle charging) - FleetkW < 0
           If (FleetState = STORE_CHARGING) Then
           Begin
             if Not (DischargeMode = CURRENTPEAKSHAVE) then
                Pdiff := Pdiff + FleetkW
             else
             Begin
                 MonitoredElement.ComputeVterminal(ActorID);
                 VoltsArr     :=  MonitoredElement.Vterminal;
                 ElemVolts    :=  cabs(VoltsArr^[1]);
                 Pdiff        :=  Pdiff + (FleetkW * 1000 / (ElemVolts * MonitoredElement.NPhases));
//                 Pdiff        :=  Pdiff + (FleetkW * 1000 / (ElemVolts ));
             End;

           End;

           CASE  FleetState of
                STORE_CHARGING,
                STORE_IDLING: If (PDiff - HalfkWBand < 0.0) or OutOfOomph Then
                  Begin  // Don't bother trying to dispatch
                       ChargingAllowed  := TRUE;
                       SkipkWDispatch   := TRUE;
                       if OutofOomph then  // --------------------------------- new 04/20/2020 ----------
                       Begin
                         For i := 1 to FleetSize Do
                         Begin
                           StorageObj := FleetPointerList.Get(i);
                           kWhActual  :=  StorageObj.StorageVars.kWhStored / StorageObj.StorageVars.kWhRating;
                           OutOfOomph := OutOfOomph and (kWhActual >= ResetLevel);  // If we have more than the 80% we are good to dispatch
                         End;
                         OutOfOomph :=  not OutOfOomph;  // If everybody in the fleet has at least the 80% of the storage capacity full

                       End;    // -----------------------------------------------------------------------
                  End;

 {               STORE_DISCHARGING: If (PDiff < 0.0) or OutOfOomph Then
                  Begin   // desired decrease is greater then present output; just cancel
                        SetFleetToIdle;   // also sets presentkW = 0
                        PushTimeOntoControlQueue(STORE_IDLING, ActorID);  // force a new power flow solution
                        ChargingAllowed := TRUE;
                        SkipkWDispatch  := TRUE;
                        Wait4Step       := TRUE; // To tell to the charging section to wait for the next sim step
                                                 // useful when workin with large simulation time steps
                  End;}
           END;
       End;


       If Not SkipkWDispatch Then
       Begin
            RemainingkWh := FleetkWh;
            ReservekWh   := FleetReservekWh;
            If (RemainingkWh > ReservekWh) Then
            Begin
               //  don't dispatch kW  if not enough storage left or an endless control loop will occur
               If abs(PDiff) > HalfkWBand Then
               Begin // Attempt to change storage dispatch
                 If Not (FleetState=STORE_DISCHARGING) Then
                  Begin
                    SetFleetToDischarge;
//                    StorekWChanged:= TRUE;  // if not already discharging, force new power flow.
                  End;
                 If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, Format('Attempting to dispatch %-.6g kW with %-.6g kWh remaining and %-.6g kWh reserve.', [kWNeeded, RemainingkWh, ReservekWh]),ActorID);
                 For i := 1 to FleetSize Do
                 Begin
                    StorageObj := FleetPointerList.Get(i);

                    if Dischargemode = CURRENTPEAKSHAVE then // Current to power
                      Begin    //  (MonitoredElement.MaxVoltage[ElementTerminal,ActorID] / 1000)
                        if StorageObj.NPhases = 1 then
                          kWNeeded :=  StorageObj.PresentkV * AmpsDiff
                        else
//                          kWNeeded :=  StorageObj.PresentkV * InvSQRT3 * AmpsDiff;
                          kWNeeded :=  StorageObj.PresentkV * SQRT3 * AmpsDiff;
                      End;

                    WITH StorageObj Do
                    Begin
                      // compute new dispatch value for this storage element ...
                      DispatchkW := Min(StorageVars.kWrating, (PresentkW + kWNeeded * DispFactor *(FWeights^[i]/TotalWeight))); // Dispatch kWNeeded

                      if DispatchkW <= 0.0 Then // if kWNeeded is too low, DispatchkW may be negative depending on idling losses. In this case, just set it to idling
                      Begin

                          StorageState := STORE_IDLING;  // overrides SetFleetToDischarge

                          if (abs(PresentkW) - StorageObj.kWOutIdling > EPSILON) Then  // if not already idling
                          Begin
                            SetNominalStorageOutput(ActorID);
                            ActualkWDispatch := PresentkW;
                            StorekWChanged := TRUE; // if not idling at first, force a new powerflow

                            If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name,
                            Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW. Setting ' + StorageObj.QualifiedName + ' to idling state. Final kWOut is %-.6g kW', [DispatchkW, ActualkWDispatch]),ActorID);
                          End
//                          DispatchkW := 0.0;

                      End
                      else
                      Begin
                        if abs(kW - DispatchkW) / abs(DispatchkW) > 0.0001 then // redispatch only if change requested
                        Begin
                            if DispatchkW < Max(CutInkWAC, CutOutkWAC) then   // Necessary check to avoid the control to go into an infinite loop when DispatchkW is less than CutOutkWAC
                            Begin
                                if InverterON = TRUE then  // request Dispatch only if the inverter is on (only once).
                                Begin
                                // Next time, the inverter will be OFF and the control won't dispatch a new power
                                  If StorageVars.kWhStored > StorageVars.kWhReserve Then
                                  Begin

                                     kW  := DispatchkW;
                                     SetNominalStorageOutput(ActorID);
                                     ActualkWDispatch := PresentkW;
                                     StorekWChanged := TRUE;     // This is what keeps the control iterations going

                                     If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name,
                                                                            Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW, less than CutIn/CutOut.'
                                                                            + ' Final kWOut is %-.6g kW', [DispatchkW, ActualkWDispatch]), ActorID);
                                  End;
                                end
                                else Begin

                                      // if inverter is already off, just override discharging state to
                                      // idling and update current kvarlimit for usage by InvControl

                                      StorageState := STORE_IDLING;     // overrides SetFleetToDischarge
                                      SetNominalStorageOutput(ActorID); // to update current kvarLimit
                                      ActualkWDispatch := PresentkW;
                                      If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name,
                                                                             Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW, less than CutIn/CutOut.'
                                                                             + ' Inverter is OFF. Final kWOut is %-.6g kW', [DispatchkW, ActualkWDispatch]),ActorID);
                                end
                            End
                            else
                              If StorageVars.kWhStored > StorageVars.kWhReserve Then
                              Begin  // Attempt to set discharge kW;  Storage element will revert to idling if out of capacity

                                   kW  := DispatchkW;
                                   SetNominalStorageOutput(ActorID);
                                   ActualkWDispatch := PresentkW;
                                   StorekWChanged := TRUE;     // This is what keeps the control iterations going

                                   If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name,
                                                                          Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW. Final kWOut is %-.6g kW',
                                                                          [DispatchkW, ActualkWDispatch]),ActorID);
                              End;

                        End;

                      End;

                    End;
                 End;
               End
            End
            Else
            Begin If not FleetState = STORE_IDLING Then
                  Begin
                      SetFleetToIdle;
                      PushTimeOntoControlQueue(STORE_IDLING, ActorID);  // force a new power flow solution
                  End;
                  ChargingAllowed := TRUE;
                  OutOfOomph := TRUE;
                  If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name,
                  Format('Ran out of OOMPH: %-.6g kWh remaining and %-.6g reserve. Fleet has been set to idling state.', [RemainingkWh, ReservekWh]),ActorID);
            End;
       End;

       If StorekWChanged or StorekvarChanged Then  // Only push onto controlqueue If there has been a change
           PushTimeOntoControlQueue(STORE_DISCHARGING, ActorID);

       {Else just continue}
    End;


End;


{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.DoPeakShaveModeLow(ActorID: Integer);
	// This is the peakShaving mode for controlling the charging operation of the storage fleet
	// The objective is to charge the storage fleet when the power at a monitored element is bellow a specified KW target (kWTarget_low)
	// The storage will charge as much power as necessary to keet the power within the deadband around kWTarget_low

  // WILL NOT IMPLEMENT REACTIVE POWER CONTROL FOR NOW
Var
   i                  : Integer;
   S                  : Complex;
   VoltsArr           : PComplexArray;
   StorageObj         : TStorageObj;
   StorekWChanged,
   SkipkWCharge       : Boolean;
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
   ActualkWDispatch   : Double;

Begin
     AmpsDiff:=0.0;
     // If list is not defined, go make one from all storage elements in circuit
     IF FleetPointerList.ListSize=0 Then  MakeFleetList;

//     If (FleetSize>0) And(Not(FleetState = STORE_DISCHARGING)) Then
     If (FleetSize > 0) Then
     Begin
       StorekWChanged := FALSE;
       SkipkWCharge   := FALSE;


       if SeasonalRating then
         CtrlTarget  :=  Get_DynamicTarget(0, ActorID)
       else
         CtrlTarget  :=  FkWTargetLow;


       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
       if Chargemode = CURRENTPEAKSHAVELOW then
       Begin

          MonitoredElement.GetCurrents(cBuffer, ActorID);
          GetControlCurrent(Amps);
//         Amps         := MonitoredElement.MaxCurrent[ElementTerminal,ActorID]; // Max current in active terminal
          PDiff        := Amps - CtrlTarget * 1000;  // Gets the difference in terms of amps
       End
       else
       Begin
          GetControlPower(S, ActorID);
          PDiff        := S.re * 0.001 - CtrlTarget;  // Assume S.re is normally positive
       end;

       ActualkW       := FleetkW;
       ActualkWh      := FleetkWh;
       TotalRatingkWh := FleetkWhRating;

       if Chargemode = CURRENTPEAKSHAVELOW then   // convert Pdiff from Amps to kW
       Begin
         MonitoredElement.ComputeVterminal(ActorID);
         VoltsArr     :=  MonitoredElement.Vterminal;
         ElemVolts    :=  cabs(VoltsArr^[1]);     // LN voltage
         kWNeeded     :=  ((MonitoredElement.NPhases * PDiff * ElemVolts) / 1000.0);
//         kWNeeded     :=  (( PDiff * ElemVolts) / 1000.0);
         AmpsDiff     := PDiff;
       End
       else
//         kWNeeded := Pdiff + FleetkW;
         kWNeeded := Pdiff;


        // Check if Fleet is Idling (FleetState is updated only if entire fleet is idling)
       if Not (FleetState=STORE_IDLING) then
       Begin
          For i := 1 to FleetSize Do Begin
            StorageObj := FleetPointerList.Get(i);
              if StorageObj.StorageState <> STORE_IDLING Then Break;
              if i = FleetSize then FleetState := STORE_IDLING;
          End;
       End;

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
       If (FleetState = STORE_DISCHARGING)  Then
           Begin
             if Not (ChargeMode = CURRENTPEAKSHAVELOW) then
                Pdiff := Pdiff + FleetkW
             else
             Begin
                 MonitoredElement.ComputeVterminal(ActorID);
                 VoltsArr     :=  MonitoredElement.Vterminal;
                 ElemVolts    :=  cabs(VoltsArr^[1]);
                 Pdiff        :=  Pdiff + (FleetkW * 1000 / (ElemVolts * MonitoredElement.NPhases));   // get actual Pdiff in Currents (discount FleetkW)  (assuming same number of phases of Fleet and Monitored Element)
//                 Pdiff        :=  Pdiff + (FleetkW * 1000 / (ElemVolts ));
             End;

           End;

       CASE  FleetState of
          STORE_DISCHARGING,
          STORE_IDLING:
                        If (PDiff > 0.0) or (ActualkWh >= TotalRatingkWh) or Wait4Step Then
                        Begin  // Don't bother trying to charge
                          ChargingAllowed  := FALSE;
                          SkipkWCharge     := TRUE;
                          Wait4Step        := False;
                        End
//                        End;
//          STORE_CHARGING: If (kWNeeded > 0.0) or (ActualkWh>=TotalRatingkWh) // old approach
//          STORE_CHARGING: If (Pdiff + FleetkW > 0.0) or (ActualkWh >= TotalRatingkWh) Then
//                          Begin   // desired decrease (in absolute value) is greater than present output; just cancel
//                                SetFleetToIdle;   // also sets presentkW = 0
//                                PushTimeOntoControlQueue(STORE_IDLING, ActorID);  // force a new power flow solution
//                                ChargingAllowed := FALSE;
//                                SkipkWCharge  := TRUE;
//                          End;
       END;

       If Not SkipkWCharge Then
       Begin
            If (ActualkWh<TotalRatingkWh) Then
            Begin
               //  don't dispatch kW  if fully charged or an endless control loop will occur
               If abs(PDiff) > HalfkWBandLow Then
               Begin // Attempt to change storage kW charge
                   If Not (FleetState=STORE_CHARGING) Then Begin
                      SetFleetToCharge;
//                      StorekWChanged:= TRUE;  // if not already charging, force new power flow.
                   End;
  //                       If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, Format('Attempting to charge %-.6g kW with %-.6g kWh remaining and %-.6g rating.', [kWNeeded, (TotalRatingkWh-ActualkWh), TotalRatingkWh]), ActorID);
                   If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, Format('Attempting to charge %-.6g kW with %-.6g kWh remaining and %-.6g rating.', [kWNeeded, (TotalRatingkWh-ActualkWh), TotalRatingkWh]), ActorID);
                   For i := 1 to FleetSize Do
                   Begin
                     StorageObj := FleetPointerList.Get(i);
                     WITH StorageObj Do
                     Begin

                        // Checks if PDiff needs to be adjusted considering the charging mode
                       if Chargemode = CURRENTPEAKSHAVELOW then
                       Begin
                         if StorageObj.NPhases = 1 then
                           kWNeeded :=  StorageObj.PresentkV * AmpsDiff
                         else
//                           kWNeeded :=  StorageObj.PresentkV * InvSQRT3 * AmpsDiff;
                           kWNeeded :=  StorageObj.PresentkV * SQRT3 * AmpsDiff;
                       End;


                       // compute new charging value for this storage element ...
  //                                ChargekW := -1 * Min(StorageVars.kWrating, abs(PresentkW + Pdiff *(FWeights^[i]/TotalWeight)));  // old approach

                       ChargekW := PresentkW + kWNeeded *(FWeights^[i]/TotalWeight)*DispFactor; // may be positive or negative
                       if ChargekW < 0 then  ChargekW := Max(-1*StorageVars.kWrating, ChargekW); // check against kVA rating

                       if ChargekW>= 0 then // chargekW may be positive if increase in demand is too high.
                       begin

                          StorageState := STORE_IDLING;  // overrides SetFleetToDischarge

                          if (abs(PresentkW) - StorageObj.kWOutIdling > EPSILON) Then  // if not already idling
                          Begin
                            SetNominalStorageOutput(ActorID);
                            ActualkWDispatch := PresentkW;
                            StorekWChanged := TRUE; // if not idling at first, force a new powerflow

                            If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name,
                            Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW. Setting ' + StorageObj.QualifiedName + ' to idling state. Final kWOut is %-.6g kW', [ChargekW, ActualkWDispatch]),ActorID);
                          End

                       end
                       else
                       begin
                          //                       If ChargekW <> PresentkW Then    // do only if change requested
                       If abs(StorageObj.kW - ChargekW)/ abs(ChargekW) > 0.0001 Then    // do only if change requested
                       Begin

                            if abs(ChargekW) < Max(CutInkWAC, CutOutkWAC) then   // Necessary check to avoid the control to go into an infinite loop when ChargekW is less than CutOutkWAC
                            Begin
                                 if InverterON = TRUE then  // request Dispatch only if the inverter is on (only once).
                                Begin
                                // Next time the inverter will be OFF and the control won't dispatch a new power
                                  If StorageVars.kWhStored > StorageVars.kWhReserve Then
                                  Begin

                                     kW  := ChargekW;
                                     SetNominalStorageOutput(ActorID);
                                     ActualkWDispatch := PresentkW;
                                     StorekWChanged := TRUE;     // This is what keeps the control iterations going

                                     If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name,
                                                                            Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW, less than CutIn/CutOut.'
                                                                            + ' Final kWOut is %-.6g kW', [ChargekW, ActualkWDispatch]), ActorID);
                                  End;
                                end
                                else Begin

                                      // if inverter is already off, just override discharging state to
                                      // idling and update current kvarlimit for usage by InvControl

                                      StorageState := STORE_IDLING;     // overrides SetFleetToCharge
                                      SetNominalStorageOutput(ActorID); // to update current kvarLimit
                                      ActualkWDispatch := PresentkW;
                                      If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name,
                                                                             Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW, less than CutIn/CutOut.'
                                                                             + ' Inverter is OFF. Final kWOut is %-.6g kW', [ChargekW, ActualkWDispatch]),ActorID);
                                end
                            End
                            else
                              If StorageVars.kWhStored < StorageVars.kWhRating Then
                              Begin  // Attempt to set discharge kW;  Storage element will revert to idling if out of capacity
                                     //StorageObj.PresentkW  :=  ChargekW;
                                   kW  :=  ChargekW;
                                   SetNominalStorageOutput(ActorID);
                                   ActualkWDispatch := PresentkW;
        //                                           KwtoPercentagekW := (ChargekW*100) / StorageVars.kWrating;  // old approach
        //                                           StorageObj.pctkWin := abs(KwtoPercentagekW);                // old approach
                                   StorekWChanged        := TRUE;     // This is what keeps the control iterations going

                                   If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name,
                                                                          Format('Requesting ' + StorageObj.QualifiedName + ' to dispatch %-.6g kW. Final kWOut is %-.6g kW',
                                                                          [ChargekW, ActualkWDispatch]),ActorID);

                              End;

                       End;
                       end;

                     End;
                   End;
               End
            End
            Else
            Begin
              If not FleetState = STORE_IDLING Then
              Begin
                SetFleetToIdle;
                PushTimeOntoControlQueue(STORE_IDLING, ActorID);  // force a new power flow solution
              End;
              ChargingAllowed := FALSE;
              If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, Format('Fully charged: %-.6g kWh of rated %-.6g.', [ActualkWh, TotalRatingkWh]), ActorID);
            End;
       End;

       If StorekWChanged Then  // Only push onto controlqueue If there has been a change
           PushTimeOntoControlQueue(STORE_CHARGING, ActorID);
       {Else just continue}
     End;
End;

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.Sample(ActorID : Integer);

Begin
       ChargingAllowed  := FALSE;
//       UpdateFleetState;
{
  Check discharge mode first. Then if not discharging, we can check for charging
}
       Wait4Step        :=  FALSE;        // Initializes the variable for the new control step
       CASE DischargeMode of
            MODEFOLLOW          : Begin
                                    DoTimeMode(1, ActorID);
                                    DoLoadFollowMode(ActorID);
                                  End;
            MODELOADSHAPE       : DoLoadShapeMode(ActorID);
            MODESUPPORT         : DoLoadFollowMode(ActorID);
            MODETIME            : DoTimeMode(1, ActorID);
            MODEPEAKSHAVE       : DoLoadFollowMode(ActorID);
            CURRENTPEAKSHAVE    : DoLoadFollowMode(ActorID);
            MODESCHEDULE        : DoScheduleMode(ActorID);
       ELSE
           DoSimpleMsg(Format('Invalid DisCharging Mode: %d',[DisChargeMode]), 14408);
       END;

       If ChargingAllowed Then
       CASE ChargeMode of
            MODELOADSHAPE       : ; // DoLoadShapeMode;  already executed above
            MODETIME            : DoTimeMode(2, ActorID);
            MODEPEAKSHAVELOW    : DoPeakShaveModeLow(ActorID);
            CURRENTPEAKSHAVELOW : DoPeakShaveModeLow(ActorID)
       ELSE
           DoSimpleMsg(Format('Invalid Charging Mode: %d',[ChargeMode]),14409);
       END;


End;


//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.CalcDailyMult(Hr:Double);

Begin
     If (DailyShapeObj <> Nil) Then
       Begin
            LoadShapeMult := DailyShapeObj.GetMult(Hr);
       End
     ELSE LoadShapeMult := CDoubleOne;  // Default to no  variation
End;


//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.CalcDutyMult(Hr:Double);

Begin
     If DutyShapeObj <> Nil Then
       Begin
             LoadShapeMult := DutyShapeObj.GetMult(Hr);
       End
     ELSE CalcDailyMult(Hr);  // Default to Daily Mult If no duty curve specified
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.CalcYearlyMult(Hr:Double);

Begin
     If YearlyShapeObj<>Nil Then
       Begin
            LoadShapeMult := YearlyShapeObj.GetMult(Hr) ;
       End
     ELSE CalcDailyMult(Hr);  // Defaults to Daily curve
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.DoLoadShapeMode(ActorID : Integer);
VAR
     FleetStateSaved  :Integer;
     RateChanged      :Boolean;
     NewChargeRate    :Double;
     NewkWRate,
     NewkvarRate      :Double;
Begin

    FleetStateSaved := FleetState;
    RateChanged     := FALSE;

    // Get multiplier

     With ActiveCircuit[ActorID].Solution Do
        CASE Mode OF
            DAILYMODE:     CalcDailyMult(DynaVars.dblHour); // Daily dispatch curve
            YEARLYMODE:    CalcYearlyMult(DynaVars.dblHour);
            LOADDURATION2: CalcDailyMult(DynaVars.dblHour);
            PEAKDAY:       CalcDailyMult(DynaVars.dblHour);
            DUTYCYCLE:     CalcDutyMult(DynaVars.dblHour) ;
        End;

    If LoadShapeMult.re < 0.0 Then
        Begin
           ChargingAllowed := TRUE;
           NewChargeRate := Abs(LoadShapeMult.re) * 100.0;
           SetFleetDesiredState(STORE_CHARGING);

           If NewChargeRate <> pctChargeRate then
           Begin
              RateChanged := TRUE;
              pctChargeRate  := NewChargeRate;
              SetFleetChargeRate;
              SetFleetToCharge;
           End;
        End

    Else If LoadShapeMult.re = 0.0  Then  SetFleetToIdle
    Else
      Begin   // Set fleet to discharging at a rate
           NewkWRate   := LoadShapeMult.re * 100.0;
//           NewkvarRate := LoadShapeMult.im * 100.0;
           SetFleetDesiredState(STORE_DISCHARGING);

//           If (NewkWRate <> pctkWRate) or (NewkvarRate <> pctkvarRate) then
           If (NewkWRate <> pctkWRate) then
           // only set rate if it has changed. otherwise the debugtrace report will not report kWOut correctly.
           Begin
              RateChanged := TRUE;
              pctkWRate   := NewkWRate;
//              pctkvarRate := NewkvarRate;
              SetFleetkWRate(pctKWRate);
//              SetFleetkvarRate(pctkvarRate);
              SetFleetToDischarge;

              ActiveCircuit[ActorID].Solution.LoadsNeedUpdating := TRUE; // Force recalc of power parms
           End;
      End;

    {Force a new power flow solution if fleet state has changed}
    If (FleetState <> FleetStateSaved) or RateChanged Then  PushTimeOntoControlQueue(0, ActorID);


End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.SetAllFleetValues;
VAR
      i   :Integer;
Begin
      For i := 1 to FleetPointerList.ListSize Do
         WITH TStorageObj(FleetPointerList.Get(i)) Do
         Begin
              pctkWin     := pctChargeRate;
//              Fpctkvarout := pctkvarRate;  CR
              pctkWout    := pctkWRate;
              pctReserve  := pctFleetReserve;
         End;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.SetFleetChargeRate;
VAR
      i   :Integer;
Begin
      For i := 1 to FleetPointerList.ListSize Do
            TStorageObj(FleetPointerList.Get(i)).pctkWin := pctChargeRate;
End;

//----------------------------------------------------------------------------
//PROCEDURE TStorageControllerObj.SetFleetkvarRate;
//VAR
//      i   :Integer;
//Begin
//    {For side effects see pctkvarout property of Storage element}
////      For i := 1 to FleetPointerList.ListSize Do
////            TStorageObj(FleetPointerList.Get(i)).pctkvarout := pctkvarRate;
//End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.SetFleetkWRate(pctkw:Double);
VAR
      i   :Integer;
Begin
      For i := 1 to FleetPointerList.ListSize Do
            TStorageObj(FleetPointerList.Get(i)).pctkWout := pctkw;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.SetFleetToCharge;
VAR
      i   :Integer;
Begin
      For i := 1 to FleetPointerList.ListSize Do
            TStorageObj(FleetPointerList.Get(i)).StorageState := STORE_CHARGING;
      FleetState :=  STORE_CHARGING;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.SetFleetToDisCharge;
VAR
      i   :Integer;
Begin

      For i := 1 to FleetPointerList.ListSize Do
            TStorageObj(FleetPointerList.Get(i)).StorageState := STORE_DISCHARGING;
      FleetState :=  STORE_DISCHARGING;
End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.SetFleetToIdle;
VAR
      i   :Integer;
Begin
      For i := 1 to FleetPointerList.ListSize Do
          With TStorageObj(FleetPointerList.Get(i))do
            Begin
              StorageState := STORE_IDLING;
//                  PresentkW := 0.0;
              kW := 0.0;
            End;
      FleetState := STORE_IDLING;
End;

//-----------------------------------------------------------------------------

PROCEDURE TStorageControllerObj.SetFleetDesiredState(state: Integer);
VAR
      i   :Integer;
Begin
      For i := 1 to FleetPointerList.ListSize Do
            TStorageObj(FleetPointerList.Get(i)).StateDesired := state;
End;

//-----------------------------------------------------------------------------
//
//procedure TStorageControllerObj.Set_PFBand(const Value: Double);
//begin
//      FPFBand    := Value;
//      HalfPFBand := FPFBand / 2.0;
//end;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.SetFleetToExternal;
VAR
      i   :Integer;
Begin
      For i := 1 to FleetPointerList.ListSize Do
            TStorageObj(FleetPointerList.Get(i)).DispatchMode := STORE_EXTERNALMODE;
End;

//----------------------------------------------------------------------------
(*
  PROCEDURE TStorageControllerObj.SetPctReserve;
  VAR
        i   :Integer;
  Begin
        For i := 1 to FleetPointerList.ListSize Do
              TStorageObj(FleetPointerList.Get(i)).pctReserve := pctFleetReserve;
  End;
*)


//----------------------------------------------------------------------------
FUNCTION TStorageControllerObj.InterpretMode(Opt: Integer;
  const S: String): Integer;
Begin

   Result := -1;  // Unknown: error
   CASE Opt of
        propMODEDISCHARGE:
              CASE LowerCase(S)[1] of
                  'f': Result := MODEFOLLOW;
                  'l': Result := MODELOADSHAPE;
                  'p': Result := MODEPEAKSHAVE;
                  's': If LowerCase(S)[2] = 'c' Then Result := MODESCHEDULE
                                                Else Result := MODESUPPORT;
                  't': Result := MODETIME;
                  'i': Result := CURRENTPEAKSHAVE;
              ELSE
                  DoSimpleMsg('Discharge Mode "' + S + '" not recognized.', 14402);
              END;
        propMODECHARGE:
              CASE LowerCase(S)[1] of
                 // 'f': Result := MODEFOLLOW;
                  'l': Result := MODELOADSHAPE;
                 // 's': Result := MODESUPPORT;
                  't': Result := MODETIME;
                  'p': Result := MODEPEAKSHAVELOW;
                  'i': Result := CURRENTPEAKSHAVELOW;
              ELSE
                  DoSimpleMsg('Charge Mode "' + S + '" not recognized.', 14402);
              END;
   ELSE
   END;
End;

//----------------------------------------------------------------------------
FUNCTION TStorageControllerObj.MakeFleetList:Boolean;

Var
   StorageObj:TStorageObj;
   i:Integer;

Begin

   Result := FALSE;

   If FElementListSpecified Then Begin    // Name list is defined - Use it

     FleetPointerList.Clear;
     For i := 1 to FleetSize Do
       Begin
             StorageObj := StorageClass[ActiveActor].Find(FStorageNameList.Strings[i-1]);
             If Assigned(StorageObj) Then Begin
                If StorageObj.Enabled Then FleetPointerList.New := StorageObj;
             End Else Begin
               DoSimpleMsg('Error: Storage Element "' + FStorageNameList.Strings[i-1] + '" not found.', 14403);
               Exit;
             End;
       End;

   End

   Else Begin

     {Search through the entire circuit for enabled Storage Elements and add them to the list}
     FStorageNameList.Clear;
     FleetPointerList.Clear;
     For i := 1 to StorageClass[ActiveActor].ElementCount Do Begin
        StorageObj :=  StorageClass[ActiveActor].ElementList.Get(i);
        // Look for a storage element not already assigned
        If StorageObj.Enabled and (StorageObj.DispatchMode <> STORE_EXTERNALMODE) Then Begin
           FStorageNameList.Add(StorageObj.Name);  // Add to list of names
           FleetPointerList.New := StorageObj;
        End;
     End;

     {Allocate uniform weights}
     FleetSize := FleetPointerList.ListSize;
     Reallocmem(FWeights, Sizeof(FWeights^[1])*FleetSize);
     For i := 1 to FleetSize Do FWeights^[i] := 1.0;

   End;

   // Add up total weights
   TotalWeight := 0.0;
   For i := 1 to FleetSize Do  TotalWeight := TotalWeight + FWeights^[i];

   If FleetPointerList.ListSize>0 Then Result := TRUE;

   FleetListChanged := FALSE;

End;



//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.Reset;
Begin
  // inherited;
     SetFleetToIdle;

 // do we want to set fleet to 100% charged storage?
End;



//----------------------------------------------------------------------------
FUNCTION TStorageControllerObj.ReturnElementsList: String;
VAR
     i :Integer;
Begin
     If FleetSize=0 Then
       Begin
            Result := '';
            Exit;
       End;

     Result := '['+ FStorageNameList.Strings[0];
     For i := 1 to FleetSize-1 Do
       Begin
             Result := Result + ', ' + FStorageNameList.Strings[i];
       End;
     Result := Result + ']';  // terminate the array

End;

//----------------------------------------------------------------------------
FUNCTION TStorageControllerObj.ReturnSeasonTarget(THigh :  Integer):String;
VAR
     i :Integer;
Begin
     If Seasons=1 Then
       Begin
            Result := '';
            Exit;
       End;

     Result := '[';
     For i := 0 to (Seasons - 1) Do
       Begin
          if THigh = 1 then
             Result := Result + format('%.6g',[SeasonTargets[i]]) + ', '
          else
             Result := Result + format('%.6g',[SeasonTargetsLow[i]]) + ', ';
       End;
     Result := Result + ']';  // terminate the array

End;


//----------------------------------------------------------------------------
FUNCTION TStorageControllerObj.ReturnWeightsList: String;
Begin
     If FleetSize=0 Then
       Begin
            Result := '';
            Exit;
       End;

     Result := GetDSSArray_Real(FleetSize, FWeights);

End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.GetControlPower(var ControlPower: Complex; ActorID : Integer);
// Get power to control based on active power
Var
   i, ControlPowerPhase  : Integer;
   TempPower             : Double;

Begin

   if  MonitoredElement.NPhases = 1 Then
   Begin
      ControlPower := MonitoredElement.Power[ElementTerminal, ActorID]; // just take the total power (works also for 1ph elements with 2 conductors)
   End
   else
   Begin
        MonitoredElement.GetPhasePower(cBuffer, ActorID);

        CASE FMonPhase of
         AVG:         Begin  // Get avg of all phases
                          ControlPower := Cmplx(0.0, 0.0);
                          FOR i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) Do
                                          ControlPower := Cadd(ControlPower, cBuffer^[i]);
                      End;
         MAXPHASE:    Begin  // Get abs max of all phases
                          ControlPower := Cmplx(0.0, 0.0);
                          FOR i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) Do Begin
                                          TempPower := abs(cBuffer^[i].re);
                                          if TempPower > abs(ControlPower.re)  then
                                              ControlPower      := cBuffer^[i];
                                              ControlPowerPhase := i;
                          End;
                          // Compute equivalent total power of all phases assuming equal to max power in all phases
                          ControlPower := cMulReal(ControlPower, Fnphases);
                      End;
         MINPHASE:    Begin // Get abs min of all phases
                          ControlPower := Cmplx(1.0e50, 1.0e50);
                          FOR i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) Do Begin
                                          TempPower := abs(cBuffer^[i].re);
                                            if TempPower < abs(ControlPower.re)  then
                                                ControlPower      := cBuffer^[i];
                                                ControlPowerPhase := i;
                          End;
                          // Compute equivalent total power of all phases assuming equal to min power in all phases
                          ControlPower := cMulReal(ControlPower, Fnphases);  // sign according to phase with min abs value
                      End;
        Else
            // Compute equivalent total power of all phases assuming equal to power in selected phases
            ControlPower := cMulReal(Cbuffer^[FMonPhase], Fnphases);  // monitored phase only
        End;
   End;

    {If this is a positive sequence circuit (Fnphases=1),
    then we need to multiply by 3 to get the 3-phase power}
    If   ActiveCircuit[ActorID].PositiveSequence
    Then ControlPower := cMulReal(ControlPower, 3.0);

End;

//----------------------------------------------------------------------------
PROCEDURE TStorageControllerObj.GetControlCurrent(var ControlCurrent: Double);
// Get current to control
Var
   i  :Integer;

Begin

    CASE FMonPhase of
       AVG:       Begin
                        ControlCurrent := 0.0;     // Get avg of all phases
                        FOR i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) Do
                                        ControlCurrent := ControlCurrent + Cabs(cBuffer^[i]);
                        ControlCurrent := ControlCurrent / Fnphases;
                  End;
       MAXPHASE:  Begin
                        ControlCurrent := 0.0;     // Get max of all phases
                        FOR i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) Do
                                        ControlCurrent := max(ControlCurrent, Cabs(cBuffer^[i]));
                        ControlCurrent := ControlCurrent;
                  End;
       MINPHASE:  Begin
                        ControlCurrent := 1.0e50;     // Get min of all phases
                        FOR i := (1 + CondOffset) to (MonitoredElement.NConds + CondOffset) Do
                                        ControlCurrent := min(ControlCurrent, Cabs(cBuffer^[i]));
                        ControlCurrent := ControlCurrent;
                  End;
    Else
    {Just use one phase because that's what most controls do.}
        ControlCurrent := Cabs(Cbuffer^[FMonPhase]);  // monitored phase only
    End;

End;


{--------------------------------------------------------------------------}

INITIALIZATION

     CDoubleOne := Cmplx(1.0, 1.0);

end.
