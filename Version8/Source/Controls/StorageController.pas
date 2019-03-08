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
    PointerList,
    Classes,
    Loadshape;

type

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TStorageController = class(TControlClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const StorageControllerName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TStorageControllerObj = class(TControlElem)
    PRIVATE

        FkWTarget,
        FkWTargetLow,
        FkWThreshold,
        FpctkWBand,
        FpctkWBandLow,
        HalfkWBand,
        HalfkWBandLow,
        FPFTarget,    // Range on this is 0..2 where 1..2 is leading
        TotalWeight: Double;
        HalfPFBand: Double;
        FPFBand: Double;
        kWNeeded: Double;
        FleetSize: Integer;
        FleetState: Integer;

        FStorageNameList: TStringList;
        FleetPointerList: PointerList.TPointerList;
        FWeights: pDoubleArray;

        FElementListSpecified: Boolean;

        DischargeMode: Integer;
        ChargeMode: Integer;
        DischargeTriggerTime: Double;
        ChargeTriggerTime: Double;
        pctKWRate: Double;
        pctkvarRate: Double;
        pctChargeRate: Double;
        pctFleetReserve: Double;
        FleetListChanged: Boolean;
        ChargingAllowed: Boolean;
        DispatchVars: Boolean;
        DischargeTriggeredByTime: Boolean;
        DischargeInhibited: Boolean;
        OutOfOomph: Boolean;
        InhibitHrs: Integer;
        UpRamptime: Double;
        FlatTime: Double;
        DnrampTime: Double;
        UpPlusFlat: Double;
        UpPlusFlatPlusDn: Double;
        LastpctDischargeRate: Double;


        TotalkWCapacity: Double;
        TotalkWhCapacity: Double;

        YearlyShape: String;  // ='fixed' means no variation  on all the time
        YearlyShapeObj: TLoadShapeObj;  // Shape for this Storage element
        DailyShape: String;  // Daily (24 HR) Storage element shape
        DailyShapeObj: TLoadShapeObj;  // Daily Storage element Shape for this load
        DutyShape: String;  // Duty cycle load shape for changes typically less than one hour
        DutyShapeObj: TLoadShapeObj;  // Shape for this Storage element

        LoadShapeMult: Complex;

        Wait4Step: Boolean;

           // PROCEDURE SetPctReserve;
        procedure SetAllFleetValues;
        procedure SetFleetkWRate(pctkw: Double);
        procedure SetFleetkvarRate(pctkvar: Double);
        procedure SetFleetChargeRate;
        procedure SetFleetToCharge;
        procedure SetFleetToDisCharge;
        procedure SetFleetToIdle;
        procedure SetFleetToExternal;
        function InterpretMode(Opt: Integer; const S: String): Integer;
        function GetModeString(Opt, Mode: Integer): String;
        function GetkWTotal(var Sum: Double): String;
        function GetkWhTotal(var Sum: Double): String;
        function GetkWhActual: String;
        function GetkWActual: String;

        procedure CalcYearlyMult(Hr: Double);
        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);

        function ReturnElementsList: String;
        function ReturnWeightsList: String;

        function MakeFleetList: Boolean;
        procedure DoLoadFollowMode(ActorID: Integer);
        procedure DoLoadShapeMode(ActorID: Integer);
        procedure DoTimeMode(Opt: Integer; ActorID: Integer);
        procedure DoScheduleMode(ActorID: Integer);
        procedure DoPeakShaveModeLow(ActorID: Integer);
        procedure PushTimeOntoControlQueue(Code: Integer; ActorID: Integer);
        function NormalizeToTOD(h: Integer; sec: Double): Double;
        procedure Set_PFBand(const Value: Double);
        function Get_FleetkW: Double;
        function Get_FleetkWh: Double;
        function Get_FleetkWhRating: Double;
        function Get_FleetReservekWh: Double;

    PUBLIC

        constructor Create(ParClass: TDSSClass; const StorageControllerName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;    // Always Zero for a StorageController

        procedure Sample(ActorID: Integer); OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state

        procedure GetCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE; // Get present value of terminal Curr
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;   // Returns Injextion currents

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;

        property PFBand: Double READ FPFBand WRITE Set_PFBand;
        property FleetkW: Double READ Get_FleetkW;
        property FleetkWh: Double READ Get_FleetkWh;
        property FleetkWhRating: Double READ Get_FleetkWhRating;
        property FleetReservekWh: Double READ Get_FleetReservekWh;

    end;


var
    ActiveStorageControllerObj: TStorageControllerObj;

{--------------------------------------------------------------------------}
implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Storage,
    Sysutils,
    uCmatrix,
    MathUtil,
    Math,
    Solution,
    Dynamics;

const

    propELEMENT = 1;
    propTERMINAL = 2;
    propKWTARGET = 3;
    propKWTARGETLOW = 4;
    propKWBAND = 5;
    propKWBANDLOW = 6;
    propPFTARGET = 7;
    propPFBAND = 8;
    propELEMENTLIST = 9;
    propWEIGHTS = 10;
    propMODEDISCHARGE = 11;
    propMODECHARGE = 12;
    propTIMEDISCHARGETRIGGER = 13;
    propTIMECHARGETRIGGER = 14;
    propRATEKW = 15;
    propRATEKVAR = 16;
    propRATECHARGE = 17;
    propRESERVE = 18;
    propKWHTOTAL = 19;
    propKWTOTAL = 20;
    propKWHACTUAL = 21;
    propKWACTUAL = 22;
    propKWNEED = 23;
    propPARTICIPATION = 24;
    propYEARLY = 25;
    propDAILY = 26;
    propDUTY = 27;
    propEVENTLOG = 28;
    propVARDISPATCH = 29;
    propINHIBITTIME = 30;
    propTUPRAMP = 31;
    propTFLAT = 32;
    propTDNRAMP = 33;
    propKWTHRESHOLD = 34;

    NumPropsThisClass = 34;

//= = = = = = = = = = = = = = DEFINE CONTROL MODE CONSTANTS = = = = = = = = = = = = = = = = = = = = = = = = =

    MODEFOLLOW = 1;
    MODELOADSHAPE = 2;
    MODESUPPORT = 3;
    MODETIME = 4;
    MODEPEAKSHAVE = 5;
    MODESCHEDULE = 6;
    MODEPEAKSHAVELOW = 7;

//= = = = = = = = = = = = = = DEFINE OTHER CONSTANTS = = = = = = = = = = = = = = = = = = = = = = = = =
    RELEASE_INHIBIT = 999;

var
    CDoubleOne: Complex;

{--------------------------------------------------------------------------}
constructor TStorageController.Create;  // Creates superstructure for all StorageController objects
begin
    inherited Create;

    Class_name := 'StorageController';
    DSSClassType := DSSClassType + STORAGE_CONTROL;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

{--------------------------------------------------------------------------}
destructor TStorageController.Destroy;

begin
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TStorageController.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

     // Define Property names

    PropertyName[propELEMENT] := 'Element';
    PropertyName[propTERMINAL] := 'Terminal';
    PropertyName[propKWTARGET] := 'kWTarget';
    PropertyName[propKWTARGETLOW] := 'kWTargetLow';
    PropertyName[propKWBAND] := '%kWBand';
    PropertyName[propKWBANDLOW] := '%kWBandLow';
    PropertyName[propPFTARGET] := 'PFTarget';
    PropertyName[propPFBAND] := 'PFBand';
    PropertyName[propELEMENTLIST] := 'ElementList';
    PropertyName[propWEIGHTS] := 'Weights';
    PropertyName[propMODEDISCHARGE] := 'ModeDischarge';
    PropertyName[propMODECHARGE] := 'ModeCharge';
    PropertyName[propTIMEDISCHARGETRIGGER] := 'TimeDischargeTrigger';
    PropertyName[propTIMECHARGETRIGGER] := 'TimeChargeTrigger';
    PropertyName[propRATEKW] := '%RatekW';
    PropertyName[propRATEKVAR] := '%Ratekvar';
    PropertyName[propRATECHARGE] := '%RateCharge';
    PropertyName[propRESERVE] := '%Reserve';
    PropertyName[propKWHTOTAL] := 'kWhTotal';
    PropertyName[propKWTOTAL] := 'kWTotal';
    PropertyName[propKWHACTUAL] := 'kWhActual';
    PropertyName[propKWACTUAL] := 'kWActual';
    PropertyName[propKWNEED] := 'kWneed';
    PropertyName[propPARTICIPATION] := '%Participation';
    PropertyName[propYEARLY] := 'Yearly';
    PropertyName[propDAILY] := 'Daily';
    PropertyName[propDUTY] := 'Duty';
    PropertyName[propEVENTLOG] := 'EventLog';
    PropertyName[propVARDISPATCH] := 'VarDispatch';
    PropertyName[propINHIBITTIME] := 'InhibitTime';
    PropertyName[propTUPRAMP] := 'Tup';
    PropertyName[propTFLAT] := 'TFlat';
    PropertyName[propTDNRAMP] := 'Tdn';
    PropertyName[propKWTHRESHOLD] := 'kWThreshold';


    PropertyHelp[propELEMENT] :=
        'Full object name of the circuit element, typically a line or transformer, ' +
        'which the control is monitoring. There is no default; must be specified.';
    PropertyHelp[propTERMINAL] :=
        'Number of the terminal of the circuit element to which the StorageController control is connected. ' +
        '1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit.';
    PropertyHelp[propKWTARGET] :=
        'kW target for Discharging. The storage element fleet is dispatched to try to hold the power in band ' +
        'at least until the storage is depleted.';
    PropertyHelp[propKWTARGETLOW] :=
        'kW target for Charging. The storage element fleet is dispatched to try to hold the power in band ' +
        'at least until the storage is fully charged.';
    PropertyHelp[propKWBAND] :=
        'Bandwidth (% of Target kW) of the dead band around the kW target value. Default is 2% (+/-1%).' +
        'No dispatch changes are attempted If the power in the monitored terminal stays within this band.';
    PropertyHelp[propKWBANDLOW] :=
        'Bandwidth (% of TargetkWLow) of the dead band around the kWtargetLow value. Default is 2% (+/-1%).' +
        'No charging is attempted if the power in the monitored terminal stays within this band.';
    PropertyHelp[propPFTARGET] :=
        'Power Factor target for dispatching the reactive power. Default is 0.96. The reactive power of the storage element fleet is dispatched to try to hold the power factor in band. ' +
        'It is assumed that the storage element inverter can produce kvar up to its kVA limit regardless of storage level.';
    PropertyHelp[propPFBAND] :=
        'Bandwidth of the Target power factor of the monitored element. of the dead band around the kvar target value. Default is 0.04 (+/- 0.02).' +
        'No dispatch changes of the kvar are attempted If the power factor of the monitored terminal stays within this band.';
    PropertyHelp[propELEMENTLIST] :=
        'Array list of Storage elements to be controlled.  If not specified, all storage elements in the circuit not presently dispatched by another controller ' +
        'are assumed dispatched by this controller.';
    PropertyHelp[propWEIGHTS] :=
        'Array of proportional weights corresponding to each storage element in the ElementList. ' +
        'The needed kW or kvar to get back to center band is dispatched to each storage element according to these weights. ' +
        'Default is to set all weights to 1.0.';
    PropertyHelp[propMODEDISCHARGE] :=
        '{PeakShave* | Follow | Support | Loadshape | Time | Schedule} Mode of operation for the DISCHARGE FUNCTION of this controller. ' +
        CRLF + CRLF + 'In PeakShave mode (Default), the control attempts to discharge storage to keep power in the monitored element below the kWTarget. ' +
        CRLF + CRLF + 'In Follow mode, the control is triggered by time and resets the kWTarget value to the present monitored element power. ' +
        'It then attempts to discharge storage to keep power in the monitored element below the new kWTarget. See TimeDischargeTrigger.' +
        CRLF + CRLF + 'In Suport mode, the control operates oppositely of PeakShave mode: storage is discharged to keep kW power output up near the target. ' +
        CRLF + CRLF + 'In Loadshape mode, both charging and discharging precisely follows the per unit loadshape. ' +
        'Storage is discharged when the loadshape value is positive. ' +
        CRLF + CRLF + 'In Time mode, the storage discharge is turned on at the specified %RatekW and %Ratekvar at the specified discharge trigger time in fractional hours.' +
        CRLF + CRLF + 'In Schedule mode, the Tup, TFlat, and Tdn properties specify the up ramp duration, flat duration, and down ramp duration for the schedule. ' +
        'The schedule start time is set by TimeDischargeTrigger and the rate of discharge for the flat part is determined by RatekW.';
    PropertyHelp[propMODECHARGE] :=
        '{Loadshape | Time* | PeakShaveLow} Mode of operation for the CHARGE FUNCTION of this controller. ' +
        CRLF + CRLF + 'In Loadshape mode, both charging and discharging precisely follows the per unit loadshape. ' +
        'Storage is charged when the loadshape value is negative. ' +
        CRLF + CRLF + 'In Time mode, the storage charging FUNCTION is triggered at the specified %RateCharge at the specified sharge trigger time in fractional hours.' +
        CRLF + CRLF + 'In PeakShaveLow mode, the charging operation will charge the storage fleet when the power at a' +
        'monitored element is bellow a specified KW target (kWTarget_low). The storage will charge as much power as necessary to keep the power within the deadband around kWTarget_low.';
    PropertyHelp[propTIMEDISCHARGETRIGGER] :=
        'Default time of day (hr) for initiating Discharging of the fleet. During Follow or Time mode discharging is triggered at a fixed time ' +
        'each day at this hour. If Follow mode, storage will be discharged to attempt to hold the load at or below the power level at the time of triggering. ' +
        'In Time mode, the discharge is based on the %RatekW property value. ' +
        'Set this to a negative value to ignore. Default is 12.0 for Follow mode; otherwise it is -1 (ignored). ';
    PropertyHelp[propTIMECHARGETRIGGER] :=
        'Default time of day (hr) for initiating charging in Time control mode. Set this to a negative value to ignore. Default is 2.0.  (0200).' +
        'When this value is >0 the storage fleet is set to charging at this time regardless of other control criteria to make sure storage is ' +
        'topped off for the next discharge cycle.';
    PropertyHelp[propRATEKW] :=
        'Sets the kW discharge rate in % of rated capacity for each element of the fleet. Applies to TIME control mode, SCHEDULE mode, or anytime discharging is triggered ' +
        'by time.';
    PropertyHelp[propRATEKVAR] :=
        'Sets the kvar discharge rate in % of rated capacity for each element of the fleet. Applies to TIME control mode or anytime discharging is triggered ' +
        'by time.';
    PropertyHelp[propRATECHARGE] :=
        'Sets the kW charging rate in % of rated capacity for each element of the fleet. Applies to TIME control mode and anytime charging mode is ' +
        'entered due to a time trigger.';
    PropertyHelp[propRESERVE] :=
        'Use this property to change the % reserve for each storage element under control of this controller. This might be used, for example, to ' +
        'allow deeper discharges of storage or in case of emergency operation to use the remainder of the storage element.';
    PropertyHelp[propKWHTOTAL] :=
        '(Read only). Total rated kWh energy storage capacity of storage elements controlled by this controller.';
    PropertyHelp[propKWTOTAL] :=
        '(Read only). Total rated kW power capacity of storage elements controlled by this controller.';
    PropertyHelp[propKWHACTUAL] :=
        '(Read only). Actual kWh output of all controlled storage elements. ';
    PropertyHelp[propKWACTUAL] :=
        '(Read only). Actual kW output of all controlled storage elements. ';
    PropertyHelp[propKWNEED] :=
        '(Read only). KW needed to meet target.';
    PropertyHelp[propPARTICIPATION] :=
        'Participation factor, %. Default = 100.';
    PropertyHelp[propYEARLY] :=
        'Dispatch loadshape object, If any, for Yearly solution Mode.';
    PropertyHelp[propDAILY] :=
        'Dispatch loadshape object, If any, for Daily solution mode.';
    PropertyHelp[propDUTY] :=
        'Dispatch loadshape object, If any, for Dutycycle solution mode.';
    PropertyHelp[propEVENTLOG] :=
        '{Yes/True | No/False} Default is No. Log control actions to Eventlog.';
    PropertyHelp[propVARDISPATCH] :=
        '{Yes/True | No/False} Default is No. Flag to indicate whether or not to disatch vars as well as watts.';
    PropertyHelp[propINHIBITTIME] :=
        'Hours (integer) to inhibit Discharging after going into Charge mode. Default is 5';
    PropertyHelp[propTUPRAMP] := 'Duration, hrs, of upramp part for SCHEDULE mode. Default is 0.25.';
    PropertyHelp[propTFLAT] := 'Duration, hrs, of flat part for SCHEDULE mode. Default is 2.0.';
    PropertyHelp[propTDNRAMP] := 'Duration, hrs, of downramp part for SCHEDULE mode. Default is 0.25.';
    PropertyHelp[propKWTHRESHOLD] := 'Threshold, kW, for Follow mode. kW has to be above this value for the Storage element ' +
        'to be dispatched on. Defaults to 75% of the kWTarget value. Must reset this property after ' +
        'setting kWTarget if you want a different value.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

{--------------------------------------------------------------------------}
function TStorageController.NewObject(const ObjName: String): Integer;
begin
    // Make a new StorageController and add it to StorageController class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TStorageControllerObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

{--------------------------------------------------------------------------}
function TStorageController.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    i: Integer;

begin

  // continue parsing with contents of Parser
    ActiveStorageControllerObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveStorageControllerObj;

    Result := 0;

    with ActiveStorageControllerObj do
    begin

        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;
        Param := Parser[ActorID].StrValue;
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
                    ElementTerminal := Parser[ActorID].IntValue;
                propKWTARGET:
                    FkWTarget := Parser[ActorID].DblValue;
                propKWTARGETLOW:
                    FkWTargetLow := Parser[ActorID].DblValue;
                propKWBAND:
                    FpctkWBand := Parser[ActorID].DblValue;
                propKWBANDLOW:
                    FpctkWBandLow := Parser[ActorID].DblValue;
                propPFTARGET:
                    FPFTarget := ConvertPFToPFRange2(Parser[ActorID].DblValue);
                propPFBAND:
                    FPFBand := Parser[ActorID].DblValue;
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
                    DischargeTriggerTime := Parser[ActorID].DblValue;
                propTIMECHARGETRIGGER:
                    ChargeTriggerTime := Parser[ActorID].DblValue;
                propRATEKW:
                    pctkWRate := Parser[ActorID].DblValue;
                propRATEKVAR:
                    pctkvarRate := Parser[ActorID].DblValue;
                propRATECHARGE:
                    pctChargeRate := Parser[ActorID].DblValue;
                propRESERVE:
                    pctFleetReserve := Parser[ActorID].DblValue;
                propKWHTOTAL: ;  // Do nothing (Read ONly)
                propKWTOTAL: ;  // Do nothing (Read ONly)
                propKWHACTUAL: ;  // Do nothing (Read ONly)
                propKWACTUAL: ;  // Do nothing (Read ONly)
                propKWNEED: ;  // Do nothing (Read ONly)
                propPARTICIPATION: ;
                propYEARLY:
                    YearlyShape := Param;
                propDAILY:
                    DailyShape := Param;
                propDUTY:
                    DutyShape := Param;
                propEVENTLOG:
                    ShowEventLog := InterpretYesNo(Param);
                propVARDISPATCH:
                    DispatchVars := InterpretYesNo(Param);
                propINHIBITTIME:
                    Inhibithrs := Max(1, Parser[ActorID].IntValue);  // >=1
                propTUPRAMP:
                    UpRamptime := Parser[ActorID].DblValue;
                propTFLAT:
                    FlatTime := Parser[ActorID].DblValue;
                propTDNRAMP:
                    DnrampTime := Parser[ActorID].DblValue;
                propKWTHRESHOLD:
                    FkWThreshold := Parser[ActorID].DblValue;

            else
           // Inherited parameters
                ClassEdit(ActiveStorageControllerObj, ParamPointer - NumPropsthisClass)
            end;

         // Side effects of setting properties above

            case ParamPointer of
                propKWTARGET,
                propKWBAND:
                begin
                    HalfkWBand := FpctkWBand / 200.0 * FkWTarget;
                    FkWThreshold := FkWTarget * 0.75;
                end;
                propKWTARGETLOW,
                propKWBANDLOW:
                    HalfkWBandLow := FpctkWBandLow / 200.0 * FkWTargetLow;
                propPFBAND:
                    HalfPFBand := FPFBand / 2.0;
                propMODEDISCHARGE:
                    if DischargeMode = MODEFOLLOW then
                        DischargeTriggerTime := 12.0; // Noon

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
                    YearlyShapeObj := LoadShapeClass[ActorID].Find(YearlyShape);
                    if YearlyShapeObj = NIL then
                        DoSimpleMsg('Yearly loadshape "' + YearlyShape + '" not found.', 14404);
                end;
                propDAILY:
                begin
                    DailyShapeObj := LoadShapeClass[ActorID].Find(DailyShape);
                    if DailyShapeObj = NIL then
                        DoSimpleMsg('Daily loadshape "' + DailyShape + '" not found.', 14405);
                end;
                propDUTY:
                begin
                    DutyShapeObj := LoadShapeClass[ActorID].Find(DutyShape);
                    if DutyShapeObj = NIL then
                        DoSimpleMsg('Dutycycle loadshape "' + DutyShape + '" not found.', 14406);
                end

            else

            end;

            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

        RecalcElementData(ActorID);
    end;

end;


{--------------------------------------------------------------------------}
function TStorageController.MakeLike(const StorageControllerName: String): Integer;
var
    OtherStorageController: TStorageControllerObj;
    i: Integer;
begin
    Result := 0;
   {See If we can find this StorageController name in the present collection}
    OtherStorageController := Find(StorageControllerName);
    if OtherStorageController <> NIL then
        with ActiveStorageControllerObj do
        begin

            NPhases := OtherStorageController.Fnphases;
            NConds := OtherStorageController.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherStorageController.ElementName;
            ControlledElement := OtherStorageController.ControlledElement;  // Pointer to target circuit element
            MonitoredElement := OtherStorageController.MonitoredElement;  // Pointer to target circuit element
            ElementTerminal := OtherStorageController.ElementTerminal;

            FkWTarget := OtherStorageController.FkWTarget;
            FkWTargetLow := OtherStorageController.FkWTargetLow;
            FkWThreshold := OtherStorageController.FkWThreshold;
            FpctkWBand := OtherStorageController.FpctkWBand;
            FpctkWBandLow := OtherStorageController.FpctkWBandLow;
            FPFTarget := OtherStorageController.FPFTarget;
            FPFBand := OtherStorageController.FPFBand;
            HalfPFBand := OtherStorageController.HalfPFBand;

            FStorageNameList.Clear;
            for i := 1 to OtherStorageController.FStorageNameList.Count do
                FStorageNameList.Add(OtherStorageController.FStorageNameList.Strings[i - 1]);

            FleetSize := FStorageNameList.count;
            if FleetSize > 0 then
            begin
                Reallocmem(FWeights, Sizeof(FWeights^[1]) * FleetSize);
                for i := 1 to FleetSize do
                    FWeights^[i] := OtherStoragecontroller.FWeights^[i];
            end;

            DisChargeMode := OtherStorageController.DisChargeMode;
            ChargeMode := OtherStorageController.ChargeMode;
            DischargeTriggerTime := OtherStorageController.DischargeTriggerTime;
            ChargeTriggerTime := OtherStorageController.ChargeTriggerTime;
            pctkWRate := OtherStorageController.pctkWRate;
            pctkvarRate := OtherStorageController.pctkvarRate;
            pctChargeRate := OtherStorageController.pctChargeRate;
            pctFleetReserve := OtherStorageController.pctFleetReserve;
            YearlyShape := OtherStorageController.YearlyShape;
            DailyShape := OtherStorageController.DailyShape;
            DutyShape := OtherStorageController.DutyShape;
            DispatchVars := OtherStorageController.DispatchVars;
            ShowEventLog := OtherStorageController.ShowEventLog;
            Inhibithrs := OtherStorageController.Inhibithrs;

            UpRamptime := OtherStorageController.UpRamptime;
            FlatTime := OtherStorageController.FlatTime;
            DnrampTime := OtherStorageController.DnrampTime;


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
        DoSimpleMsg('Error in StorageController MakeLike: "' + StorageControllerName + '" Not Found.', 370);

end;


{==========================================================================}
{                    TStorageControllerObj                                           }
{==========================================================================}


{--------------------------------------------------------------------------}
constructor TStorageControllerObj.Create(ParClass: TDSSClass; const StorageControllerName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(StorageControllerName);
    DSSObjType := ParClass.DSSClassType;

    NPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors

    ElementName := '';
    ControlledElement := NIL;  // not used in this control
    ElementTerminal := 1;
    MonitoredElement := NIL;

    FStorageNameList := TSTringList.Create;
    FWeights := NIL;
    FleetPointerList := PointerList.TPointerList.Create(20);  // Default size and increment
    FleetSize := 0;
    FleetState := STORE_IDLING;
    FkWTarget := 8000.0;
    FkWTargetLow := 4000.0;
    FkWThreshold := 6000.0;
    FpctkWBand := 2.0;
    FpctkWBandLow := 2.0;
    TotalWeight := 1.0;
    HalfkWBand := FpctkWBand / 200.0 * FkWTarget;
    FPFTarget := 0.96;
    FPFBand := 0.04;
    HalfPFBand := FPFBand / 2.0;
    kWNeeded := 0.0;

    DischargeMode := MODEPEAKSHAVE;
    ChargeMode := MODETIME;

    DischargeTriggerTime := -1.0;  // disabled
    ChargeTriggerTime := 2.0;   // 2 AM
    FElementListSpecified := FALSE;
    FleetListChanged := TRUE;  // force building of list
    pctkWRate := 20.0;
    pctkvarRate := 20.0;
    pctChargeRate := 20.0;
    pctFleetReserve := 25.0;

    ShowEventLog := FALSE;
    DispatchVars := FALSE;
    DischargeTriggeredByTime := FALSE;
    DischargeInhibited := FALSE;
    OutOfOomph := FALSE;
    InhibitHrs := 5;   // No. Hours to inhibit discharging after going into charge mode

    UpRamptime := 0.25; // hr
    FlatTime := 2.0;
    DnrampTime := 0.25;
    LastpctDischargeRate := 0.0;
    Wait4Step := FALSE;     // for sync discharge with charge when there is a transition

    InitPropertyValues(0);

end;

destructor TStorageControllerObj.Destroy;
begin
    ElementName := '';
    YearlyShape := '';
    DailyShape := '';
    DutyShape := '';

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
procedure TStorageControllerObj.InitPropertyValues(ArrayOffset: Integer);
begin


    PropertyValue[propELEMENT] := '';
    PropertyValue[propTERMINAL] := '1';
    PropertyValue[propKWTARGET] := '8000';
    PropertyValue[propKWTARGETLOW] := '4000';
    PropertyValue[propKWBAND] := '2';
    PropertyValue[propKWBANDLOW] := '2';
    PropertyValue[propPFTARGET] := '.96';
    PropertyValue[propPFBAND] := '.04';
    PropertyValue[propELEMENTLIST] := '';
    PropertyValue[propWEIGHTS] := '';
    PropertyValue[propMODEDISCHARGE] := 'Follow';
    PropertyValue[propMODECHARGE] := 'Time';
    PropertyValue[propTIMEDISCHARGETRIGGER] := '-1';
    PropertyValue[propTIMECHARGETRIGGER] := '2';
    PropertyValue[propRATEKW] := '20';
    PropertyValue[propRATEKVAR] := '20';
    PropertyValue[propRATECHARGE] := '20';
    PropertyValue[propRESERVE] := '25';
    PropertyValue[propKWHTOTAL] := '';
    PropertyValue[propKWTOTAL] := '';
    PropertyValue[propKWACTUAL] := '';
    PropertyValue[propKWNEED] := '';
    PropertyValue[propPARTICIPATION] := '';
    PropertyValue[propYEARLY] := '';
    PropertyValue[propDAILY] := '';
    PropertyValue[propDUTY] := '';
    PropertyValue[propEVENTLOG] := 'No';
    PropertyValue[propINHIBITTIME] := '5';
    PropertyValue[propTUPRAMP] := '0.25';
    PropertyValue[propTFLAT] := '2.0';
    PropertyValue[propTDNRAMP] := '0.25';
    PropertyValue[propKWTHRESHOLD] := '4000';


    inherited  InitPropertyValues(NumPropsThisClass);

end;

function TStorageControllerObj.GetPropertyValue(Index: Integer): String;
begin
    Result := '';
    case Index of

        propKWTARGET:
            Result := Format('%-.6g', [FkWTarget]);
        propKWTARGETLOW:
            Result := Format('%-.6g', [FkWTargetLow]);
        propKWBAND:
            Result := Format('%-.6g', [FpctkWBand]);
        propKWBANDLOW:
            Result := Format('%-.6g', [FpctkWBandLow]);
        propPFTARGET:
            Result := Format('%-.6g', [ConvertPFRange2ToPF(FPFTarget)]);
        propPFBAND:
            Result := Format('%-.6g', [FPFBand]);
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
        propRATEKVAR:
            Result := Format('%-.8g', [pctkvarRate]);
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
            if ShowEventLog then
                Result := 'Yes'
            else
                Result := 'No';
        propVARDISPATCH:
            if DispatchVars then
                Result := 'Yes'
            else
                Result := 'No';
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

    else  // take the generic handler
        Result := inherited GetPropertyValue(index);

    end;
end;

function TStorageControllerObj.Get_FleetkW: Double;

var
    pStorage: TStorageObj;
    i: Integer;
begin
    Result := 0.0;
    for I := 1 to FleetPointerList.ListSize do
    begin
        pStorage := FleetPointerList.Get(i);
        Result := Result + pStorage.PresentkW;
    end;
end;

function TStorageControllerObj.Get_FleetkWh: Double;
var
    pStorage: TStorageObj;
    i: Integer;
begin
    Result := 0.0;
    for I := 1 to FleetPointerList.ListSize do
    begin
        pStorage := FleetPointerList.Get(i);
        Result := Result + pStorage.StorageVars.kWhStored;
    end;
end;

function TStorageControllerObj.Get_FleetkWhRating: Double;
var
    pStorage: TStorageObj;
    i: Integer;
begin
    Result := 0.0;
    for I := 1 to FleetPointerList.ListSize do
    begin
        pStorage := FleetPointerList.Get(i);
        Result := Result + pStorage.StorageVars.kWhRating;
    end;
end;

function TStorageControllerObj.Get_FleetReservekWh: Double;
var
    pStorage: TStorageObj;
    i: Integer;
begin
    Result := 0.0;
    for I := 1 to FleetPointerList.ListSize do
    begin
        pStorage := FleetPointerList.Get(i);
        Result := Result + pStorage.StorageVars.kWhReserve;
    end;

end;

{--------------------------------------------------------------------------}
procedure TStorageControllerObj.RecalcElementData(ActorID: Integer);

// Recalculate critical element values after changes have been made

var
    DevIndex: Integer;

begin

        {Check for existence of monitored element}

    Devindex := GetCktElementIndex(ElementName); // Global FUNCTION
    if DevIndex > 0 then
    begin
        MonitoredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
        if ElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg('StorageController: "' + Name + '"',
                'Terminal no. "' + '" Does not exist.',
                'Re-specify terminal no.', 371);
        end
        else
        begin
            Nphases := MonitoredElement.Nphases;
            NConds := FNphases;
               // Sets name of i-th terminal's connected bus in StorageController's buslist
            Setbus(1, MonitoredElement.GetBus(ElementTerminal));
        end;
    end
    else
        DoSimpleMsg('Monitored Element in StorageController.' + Name + ' Does not exist:"' + ElementName + '"', 372);

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

procedure TStorageControllerObj.MakePosSequence(ActorID: Integer);
begin
    if MonitoredElement <> NIL then
    begin
        Nphases := MonitoredElement.NPhases;
        Nconds := FNphases;
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    end;
    inherited;
end;

{--------------------------------------------------------------------------}
procedure TStorageControllerObj.CalcYPrim(ActorID: Integer);
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);


end;


{--------------------------------------------------------------------------}
procedure TStorageControllerObj.GetCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;

begin

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;

procedure TStorageControllerObj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;

begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

function TStorageControllerObj.GetkWActual: String;
begin
    Result := Format('%-.8g', [FleetkW]);
end;

function TStorageControllerObj.GetkWhActual: String;

begin
    Result := Format('%-.8g', [FleetkWh]);
end;

function TStorageControllerObj.GetkWhTotal(var Sum: Double): String;
var
    pStorage: TStorageObj;
    i: Integer;

begin
    Sum := 0.0;
    for i := 1 to FleetPointerList.ListSize do
    begin
        pStorage := FleetPointerList.Get(i);
        sum := sum + pStorage.StorageVars.kWhRating;
    end;
    Result := Format('%-.8g', [sum]);
end;

function TStorageControllerObj.GetkWTotal(var Sum: Double): String;
var
    pStorage: TStorageObj;
    i: Integer;

begin
    Sum := 0.0;
    for i := 1 to FleetPointerList.ListSize do
    begin
        pStorage := FleetPointerList.Get(i);
        sum := sum + pStorage.StorageVars.kWRating;
    end;
    Result := Format('%-.8g', [sum]);
end;

function TStorageControllerObj.GetModeString(Opt, Mode: Integer): String;
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
            else
                Result := 'UNKNOWN'
            end;
    else
        DoSimpleMsg('Unknown Charge/Discharge designation', 14401);
    end;
end;


{--------------------------------------------------------------------------}
procedure TStorageControllerObj.DumpProperties(var F: TextFile; Complete: Boolean);

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
procedure TStorageControllerObj.DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer);
begin

        {
           Release  the discharge inhibit .
           Do nothing for other codes
        }

    if (Code = RELEASE_INHIBIT) and (DischargeMode <> MODEFOLLOW) then
        DischargeInhibited := FALSE;

end;

procedure TStorageControllerObj.DoScheduleMode(ActorID: Integer);
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
        with ActiveCircuit[ActorID].Solution do
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
                        AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Discharging (up ramp)by Schedule', ActorID);
                    SetFleetToDischarge;
                    ChargingAllowed := FALSE;
                    pctDischargeRate := min(pctkWRate, max(pctKWRate * Tdiff / UpRampTime, 0.0));
                    SetFleetkWRate(pctDischargeRate);
                    DischargeInhibited := FALSE;
                    PushTimeOntoControlQueue(STORE_DISCHARGING, ActorID);
                end;
            end

            else
            begin    // fleet is already discharging
                TDiff := NormalizeToTOD(DynaVars.intHour, DynaVars.t) - DisChargeTriggerTime;
                if TDiff < UpRampTime then
                begin

                    pctDischargeRate := min(pctkWRate, max(pctKWRate * Tdiff / UpRampTime, 0.0));
                    SetFleetkWRate(pctDischargeRate);

                end
                else
                begin

                    if TDiff < UpPlusFlat then
                    begin

                        pctDischargeRate := pctkWRate;
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
                            AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Idling by Schedule', ActorID);

                    end
                    else
                    begin  // We're on the down ramp

                        TDiff := UpPlusFlatPlusDn - TDiff;
                        pctDischargeRate := max(0.0, min(pctKWRate * Tdiff / DnRampTime, pctKWRate));
                        SetFleetkWRate(pctDischargeRate);

                    end;

                end;

                if pctDischargeRate <> LastpctDischargeRate then
                    PushTimeOntoControlQueue(STORE_DISCHARGING, ActorID);

            end;  {If not fleetstate ...}
        end;
    LastpctDischargeRate := pctDischargeRate;   // remember this value
end;

procedure TStorageControllerObj.DoTimeMode(Opt: Integer; ActorID: Integer);
{
  In Time mode we need to only turn the storage elements on. They will turn themselves
  off when they are either fully discharged, fully charged, or receive another command
  from the controller
}
begin

    case Opt of

        1:
        begin
            if (DisChargeTriggerTime > 0.0) then
                with ActiveCircuit[ActorID].Solution do
                begin
                 // turn on if time within 1/2 time step
                    if abs(NormalizeToTOD(DynaVars.intHour, DynaVars.t) - DisChargeTriggerTime) < DynaVars.h / 7200.0 then
                    begin
                        if not (FleetState = STORE_DISCHARGING) then
                        begin
                        {Time is within 1 time step of the trigger time}
                            if ShowEventLog then
                                AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Discharging by Time Trigger', ActorID);
                            SetFleetToDischarge;
                            SetFleetkWRate(pctKWRate);
                            DischargeInhibited := FALSE;
                            if DischargeMode = MODEFOLLOW then
                                DischargeTriggeredByTime := TRUE
                            else
                                PushTimeOntoControlQueue(STORE_DISCHARGING, ActorID);
                        end;
                    end
                    else
                        ChargingAllowed := TRUE;
                end;
        end; // Discharge mode
        2:
        begin
            if ChargeTriggerTime > 0.0 then
                with ActiveCircuit[ActorID].Solution do
                begin
                    if abs(NormalizeToTOD(DynaVars.intHour, DynaVars.t) - ChargeTriggerTime) < DynaVars.h / 7200.0 then
                        if not (FleetState = STORE_CHARGING) then
                        begin
                    {Time is within 1 time step of the trigger time}
                            if ShowEventLog then
                                AppendToEventLog('StorageController.' + Self.Name, 'Fleet Set to Charging by Time Trigger', ActorID);
                            SetFleetToCharge;
                            DischargeInhibited := TRUE;
                            OutOfOomph := FALSE;
                            PushTimeOntoControlQueue(STORE_CHARGING, ActorID);   // force re-solve at this time step
                    // Push message onto control queue to release inhibit at a later time
                            with ActiveCircuit[ActorID] do
                            begin
                                Solution.LoadsNeedUpdating := TRUE; // Force recalc of power parms
                                ControlQueue.Push(DynaVars.intHour + InhibitHrs, Dynavars.t, RELEASE_INHIBIT, 0, Self, ActorID);
                            end;
                        end;
                end;
        end; //Charge mode
    end;

end;

//----------------------------------------------------------------------------
function TStorageControllerObj.NormalizeToTOD(h: Integer; sec: Double): Double;
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


procedure TStorageControllerObj.PushTimeOntoControlQueue(Code: Integer; ActorID: Integer);
{
   Push present time onto control queue to force re solve at new dispatch value
}
begin
    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
        LoadsNeedUpdating := TRUE; // Force recalc of power parms
        ControlQueue.Push(DynaVars.intHour, DynaVars.t, Code, 0, Self, ActorID);
    end;

end;

{--------------------------------------------------------------------------}
procedure TStorageControllerObj.DoLoadFollowMode(ActorID: Integer);

var
    i: Integer;
    PDiff,
    PFDiff: Double;
    S: Complex;
    StorageObj: TSTorageObj;
    StorekWChanged,
    StorekvarChanged: Boolean;
    DispatchkW,
    Dispatchkvar: Double;
    SkipkWDispatch: Boolean;
    RemainingkWh: Double;
    ReservekWh: Double;


begin
     // If list is not defined, go make one from all storage elements in circuit
    if FleetPointerList.ListSize = 0 then
        MakeFleetList;

    if FleetSize > 0 then
    begin

        StorekWChanged := FALSE;
        StorekvarChanged := FALSE;
        SkipkWDispatch := FALSE;

       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
        S := MonitoredElement.MaxPower[ElementTerminal, ActorID];  // Power in active terminal
                                                         // based on max phase current
        case DischargeMode of
             // Following Load; try to keep load below kW Target
            MODEFOLLOW:
            begin
                if DischargeTriggeredByTime then
                begin
                    if ShowEventLog then
                        AppendToEventLog('StorageController.' + Self.Name,
                            Format('Fleet Set to Discharging by Time Trigger; Old kWTarget = %-.6g; New = %-.6g', [FkwTarget, S.re * 0.001]), ActorID);
                    FkwTarget := Max(FkWThreshold, S.re * 0.001);  // Capture present kW and reset target
                    DischargeTriggeredByTime := FALSE;  // so we don't come back in here right away
                    SetFleetToIdle;
                end;
                PDiff := S.re * 0.001 - FkWTarget;  // Assume S.re is normally positive
                PFDiff := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for peak shaving
            end;
             // supporting DG; Try to keep load above kW target
            MODESUPPORT:
            begin
                PDiff := S.re * 0.001 + FkWTarget;  // assume S.re is normally negative
                PFDiff := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for generator
            end;

            MODEPEAKSHAVE:
            begin
                PDiff := S.re * 0.001 - FkWTarget;  // Assume S.re is normally positive
                PFDiff := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for peak shaving
            end;
        else
            PDiff := 0.0;
            PFDiff := 0.0;
        end;


        kWNeeded := PDiff;

       {  kW dispatch  }

        if DischargeInhibited then
            SkipkWDispatch := TRUE
        else
        begin
            if FleetState = STORE_CHARGING then
                Pdiff := Pdiff + FleetkW;  // ignore overload due to charging

            case FleetState of
                STORE_CHARGING,
                STORE_IDLING:
                    if (PDiff < 0.0) or OutOfOomph then
                    begin  // Don't bother trying to dispatch
                        ChargingAllowed := TRUE;
                        SkipkWDispatch := TRUE;
                    end;
                STORE_DISCHARGING:
                    if ((PDiff + FleetkW) < 0.0) or OutOfOomph then
                    begin   // desired decrease is greater then present output; just cancel
                        SetFleetToIdle;   // also sets presentkW = 0
                        PushTimeOntoControlQueue(STORE_IDLING, ActorID);  // force a new power flow solution
                        ChargingAllowed := TRUE;
                        SkipkWDispatch := TRUE;
                        Wait4Step := TRUE; // To tell to the charging section to wait for the next sim step
                                                 // useful when workin with large simulation time steps
                    end;
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
                        SetFleetToDischarge;
                    if ShowEventLog then
                        AppendToEventLog('StorageController.' + Self.Name, Format('Attempting to dispatch %-.6g kW with %-.6g kWh remaining and %-.6g reserve.', [kWneeded, RemainingkWh, ReservekWh]), ActorID);
                    for i := 1 to FleetSize do
                    begin
                        StorageObj := FleetPointerList.Get(i);
                        with StorageObj do
                        begin
                            // compute new dispatch value for this storage element ...
                            DispatchkW := Min(StorageVars.kWrating, (PresentkW + PDiff * (FWeights^[i] / TotalWeight)));
                            if DispatchkW <> PresentkW then    // redispatch only if change requested
                                if StorageVars.kWhStored > StorageVars.kWhReserve then
                                begin  // Attempt to set discharge kW;  Storage element will revert to idling if out of capacity
                                    StorageObj.PresentkW := DispatchkW;
                                    StorekWChanged := TRUE;     // This is what keeps the control iterations going
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
                    PushTimeOntoControlQueue(STORE_IDLING, ActorID);  // force a new power flow solution
                end;
                ChargingAllowed := TRUE;
                OutOfOomph := TRUE;
                if ShowEventLog then
                    AppendToEventLog('StorageController.' + Self.Name, Format('Ran out of OOMPH: %-.6g kWh remaining and %-.6g reserve.', [RemainingkWh, ReservekWh]), ActorID);
            end;
        end;


       // kvar dispatch  NOTE: PFDiff computed from PF in range of 0..2
       // Redispatch the vars only if the PF is outside the band
        if DispatchVars and (Abs(PFDiff) > HalfPFBand) then
        begin
            if ShowEventLog then
                AppendToEventLog('StorageController.' + Self.Name, Format('Changed kvar Dispatch. PF Diff needed = %.6g', [PFDiff]), ActorID);
          // Redispatch Storage elements
            for i := 1 to FleetSize do
            begin
                StorageObj := FleetPointerList.Get(i);
                    // compute new var dispatch value for this storage element ...
                if FPFTarget = 1.0 then
                    Dispatchkvar := 0.0
                else
                begin
                    Dispatchkvar := S.re * Sqrt(1.0 / SQR(ConvertPFRange2ToPF(FPFTarget)) - 1.0) * (FWeights^[i] / TotalWeight);
                    if FPFTarget > 1.0 then
                        Dispatchkvar := -Dispatchkvar;  // for watts and vars in opposite direction
                end;

                if Dispatchkvar <> StorageObj.Presentkvar then
                begin
                    StorageObj.Presentkvar := Dispatchkvar;  // Ask for this much kvar  but may be limited by element
                    StorekvarChanged := TRUE;
                end;
            end;
        end;

        if StorekWChanged or StorekvarChanged then  // Only push onto controlqueue If there has been a change
            PushTimeOntoControlQueue(STORE_DISCHARGING, ActorID);


       {Else just continue}
    end;


end;

{--------------------------------------------------------------------------}
procedure TStorageControllerObj.DoPeakShaveModeLow(ActorID: Integer);
    // This is the peakShaving mode for controlling the charging operation of the storage fleet
    // The objective is to charge the storage fleet when the power at a monitored element is bellow a specified KW target (kWTarget_low)
    // The storage will charge as much power as necessary to keet the power within the deadband around kWTarget_low

  // WILL NOT IMPLEMENT REACTIVE POWER CONTROL FOR NOW
var
    i: Integer;
    PDiff: Double;
    kWNeeded: Double;
    S: Complex;
    StorageObj: TSTorageObj;
    StorekWChanged: Boolean;
    ChargekW: Double;
    SkipkWCharge: Boolean;
    ActualkWh: Double;
    ActualkW: Double;
    TotalRatingkWh: Double;
    KwtoPercentagekW: Double;

begin
     // If list is not defined, go make one from all storage elements in circuit
    if FleetPointerList.ListSize = 0 then
        MakeFleetList;

    if (FleetSize > 0) and (not (FleetState = STORE_DISCHARGING)) then
    begin

        StorekWChanged := FALSE;
        SkipkWCharge := FALSE;

       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
        S := MonitoredElement.MaxPower[ElementTerminal, ActorID];  // Power in active terminal
        PDiff := S.re * 0.001 - FkWTargetLow;  // Assume S.re is normally positive

        ActualkW := FleetkW;
        ActualkWh := FleetkWh;
        TotalRatingkWh := FleetkWhRating;

        kWNeeded := Pdiff + FleetkW;

        case FleetState of
            STORE_IDLING:
                if (PDiff > 0.0) or (ActualkWh >= TotalRatingkWh) or Wait4Step then
                begin  // Don't bother trying to charge
                    ChargingAllowed := FALSE;
                    SkipkWCharge := TRUE;
                    Wait4Step := FALSE;
                end
                else
                    ChargingAllowed := ChargingAllowed;
            STORE_CHARGING:
                if (kWNeeded > 0.0) or (ActualkWh >= TotalRatingkWh) then
                begin   // desired decrease is greater then present output; just cancel
                    SetFleetToIdle;   // also sets presentkW = 0
                    PushTimeOntoControlQueue(STORE_IDLING, ActorID);  // force a new power flow solution
                    ChargingAllowed := FALSE;
                    SkipkWCharge := TRUE;
                end;
        end;

        if not SkipkWCharge then
        begin
            if (ActualkWh < TotalRatingkWh) then
            begin
               //  don't dispatch kW  if fully charged or an endless control loop will occur
                if abs(PDiff) > HalfkWBandLow then
                begin // Attempt to change storage kW charge
                    if not (FleetState = STORE_CHARGING) then
                        SetFleetToCharge;
                    if ShowEventLog then
                        AppendToEventLog('StorageController.' + Self.Name, Format('Attempting to charge %-.6g kW with %-.6g kWh remaining and %-.6g rating.', [PDiff, (TotalRatingkWh - ActualkWh), TotalRatingkWh]), ActorID);
                    for i := 1 to FleetSize do
                    begin
                        StorageObj := FleetPointerList.Get(i);
                        with StorageObj do
                        begin
                            // compute new charging value for this storage element ...
                            ChargekW := -1 * Min(StorageVars.kWrating, abs(PresentkW + PDiff * (FWeights^[i] / TotalWeight)));
                            if ChargekW <> PresentkW then    // do only if change requested
                                if StorageVars.kWhStored < StorageVars.kWhRating then
                                begin  // Attempt to set discharge kW;  Storage element will revert to idling if out of capacity
                                           //StorageObj.PresentkW  :=  ChargekW;
                                    KwtoPercentagekW := (ChargekW * 100) / StorageVars.kWrating;
                                    StorageObj.pctkWin := abs(KwtoPercentagekW);
                                    StorekWChanged := TRUE;     // This is what keeps the control iterations going
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
                    PushTimeOntoControlQueue(STORE_IDLING, ActorID);  // force a new power flow solution
                end;
                ChargingAllowed := FALSE;
                if ShowEventLog then
                    AppendToEventLog('StorageController.' + Self.Name, Format('Fully charged: %-.6g kWh of rated %-.6g.', [ActualkWh, TotalRatingkWh]), ActorID);
            end;
        end;

        if StorekWChanged then  // Only push onto controlqueue If there has been a change
            PushTimeOntoControlQueue(STORE_CHARGING, ActorID);
       {Else just continue}
    end;
end;

{--------------------------------------------------------------------------}
procedure TStorageControllerObj.Sample(ActorID: Integer);

begin
    ChargingAllowed := FALSE;
{
  Check discharge mode first. Then if not discharging, we can check for charging
}
    Wait4Step := FALSE;        // Initializes the variable for the new control step
    case DischargeMode of
        MODEFOLLOW:
        begin
            DoTimeMode(1, ActorID);
            DoLoadFollowMode(ActorID);
        end;
        MODELOADSHAPE:
            DoLoadShapeMode(ActorID);
        MODESUPPORT:
            DoLoadFollowMode(ActorID);
        MODETIME:
            DoTimeMode(1, ActorID);
        MODEPEAKSHAVE:
            DoLoadFollowMode(ActorID);
        MODESCHEDULE:
            DoScheduleMode(ActorID);
    else
        DoSimpleMsg(Format('Invalid DisCharging Mode: %d', [DisChargeMode]), 14408);
    end;

    if ChargingAllowed then
        case ChargeMode of
            MODELOADSHAPE: ; // DoLoadShapeMode;  already executed above
            MODETIME:
                DoTimeMode(2, ActorID);
            MODEPEAKSHAVELOW:
                DoPeakShaveModeLow(ActorID)
        else
            DoSimpleMsg(Format('Invalid Charging Mode: %d', [ChargeMode]), 14409);
        end;


end;


//----------------------------------------------------------------------------
procedure TStorageControllerObj.CalcDailyMult(Hr: Double);

begin
    if (DailyShapeObj <> NIL) then
    begin
        LoadShapeMult := DailyShapeObj.GetMult(Hr);
    end
    else
        LoadShapeMult := CDoubleOne;  // Default to no  variation
end;


//----------------------------------------------------------------------------
procedure TStorageControllerObj.CalcDutyMult(Hr: Double);

begin
    if DutyShapeObj <> NIL then
    begin
        LoadShapeMult := DutyShapeObj.GetMult(Hr);
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult If no duty curve specified
end;

//----------------------------------------------------------------------------
procedure TStorageControllerObj.CalcYearlyMult(Hr: Double);

begin
    if YearlyShapeObj <> NIL then
    begin
        LoadShapeMult := YearlyShapeObj.GetMult(Hr);
    end
    else
        CalcDailyMult(Hr);  // Defaults to Daily curve
end;

//----------------------------------------------------------------------------
procedure TStorageControllerObj.DoLoadShapeMode(ActorID: Integer);
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

    with ActiveCircuit[ActorID].Solution do
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
        if NewChargeRate <> pctChargeRate then
            RateChanged := TRUE;
        pctChargeRate := NewChargeRate;
        SetFleetChargeRate;
        SetFleetToCharge;
    end
    else
    if LoadShapeMult.re = 0.0 then
        SetFleetToIdle
    else
    begin   // Set fleet to discharging at a rate
        NewkWRate := LoadShapeMult.re * 100.0;
        NewkvarRate := LoadShapeMult.im * 100.0;
        if (NewkWRate <> pctkWRate) or (NewkvarRate <> pctkvarRate) then
            RateChanged := TRUE;
        pctkWRate := NewkWRate;
        pctkvarRate := NewkvarRate;
        SetFleetkWRate(pctKWRate);
        SetFleetkvarRate(pctkvarRate);
        SetFleetToDischarge;
        ActiveCircuit[ActorID].Solution.LoadsNeedUpdating := TRUE; // Force recalc of power parms
    end;

    {Force a new power flow solution if fleet state has changed}
    if (FleetState <> FleetStateSaved) or RateChanged then
        PushTimeOntoControlQueue(0, ActorID);


end;

//----------------------------------------------------------------------------
procedure TStorageControllerObj.SetAllFleetValues;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.ListSize do
        with TStorageObj(FleetPointerList.Get(i)) do
        begin
            pctkWin := pctChargeRate;
            Fpctkvarout := pctkvarRate;
            pctkWout := pctkWRate;
            pctReserve := pctFleetReserve;
        end;
end;

//----------------------------------------------------------------------------
procedure TStorageControllerObj.SetFleetChargeRate;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.ListSize do
        TStorageObj(FleetPointerList.Get(i)).pctkWin := pctChargeRate;
end;

//----------------------------------------------------------------------------
procedure TStorageControllerObj.SetFleetkvarRate;
var
    i: Integer;
begin
    {For side effects see pctkvarout property of Storage element}
    for i := 1 to FleetPointerList.ListSize do
        TStorageObj(FleetPointerList.Get(i)).pctkvarout := pctkvarRate;
end;

//----------------------------------------------------------------------------
procedure TStorageControllerObj.SetFleetkWRate(pctkw: Double);
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.ListSize do
        TStorageObj(FleetPointerList.Get(i)).pctkWout := pctkw;
end;

//----------------------------------------------------------------------------
procedure TStorageControllerObj.SetFleetToCharge;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.ListSize do
        TStorageObj(FleetPointerList.Get(i)).StorageState := STORE_CHARGING;
    FleetState := STORE_CHARGING;
end;

//----------------------------------------------------------------------------
procedure TStorageControllerObj.SetFleetToDisCharge;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.ListSize do
        TStorageObj(FleetPointerList.Get(i)).StorageState := STORE_DISCHARGING;
    FleetState := STORE_DISCHARGING;
end;

//----------------------------------------------------------------------------
procedure TStorageControllerObj.SetFleetToIdle;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.ListSize do
        with TStorageObj(FleetPointerList.Get(i)) do
        begin
            StorageState := STORE_IDLING;
            PresentkW := 0.0;
        end;
    FleetState := STORE_IDLING;
end;

procedure TStorageControllerObj.Set_PFBand(const Value: Double);
begin
    FPFBand := Value;
    HalfPFBand := FPFBand / 2.0;
end;

//----------------------------------------------------------------------------
procedure TStorageControllerObj.SetFleetToExternal;
var
    i: Integer;
begin
    for i := 1 to FleetPointerList.ListSize do
        TStorageObj(FleetPointerList.Get(i)).DispatchMode := STORE_EXTERNALMODE;
end;

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
function TStorageControllerObj.InterpretMode(Opt: Integer;
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
            else
                DoSimpleMsg('Charge Mode "' + S + '" not recognized.', 14402);
            end;
    else
    end;
end;

//----------------------------------------------------------------------------
function TStorageControllerObj.MakeFleetList: Boolean;

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
            StorageObj := StorageClass[ActiveActor].Find(FStorageNameList.Strings[i - 1]);
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
        for i := 1 to StorageClass[ActiveActor].ElementCount do
        begin
            StorageObj := StorageClass[ActiveActor].ElementList.Get(i);
        // Look for a storage element not already assigned
            if StorageObj.Enabled and (StorageObj.DispatchMode <> STORE_EXTERNALMODE) then
            begin
                FStorageNameList.Add(StorageObj.Name);  // Add to list of names
                FleetPointerList.New := StorageObj;
            end;
        end;

     {Allocate uniform weights}
        FleetSize := FleetPointerList.ListSize;
        Reallocmem(FWeights, Sizeof(FWeights^[1]) * FleetSize);
        for i := 1 to FleetSize do
            FWeights^[i] := 1.0;

    end;

   // Add up total weights
    TotalWeight := 0.0;
    for i := 1 to FleetSize do
        TotalWeight := TotalWeight + FWeights^[i];

    if FleetPointerList.ListSize > 0 then
        Result := TRUE;

    FleetListChanged := FALSE;

end;


//----------------------------------------------------------------------------
procedure TStorageControllerObj.Reset;
begin
  // inherited;
    SetFleetToIdle;

 // do we want to set fleet to 100% charged storage?
end;


//----------------------------------------------------------------------------
function TStorageControllerObj.ReturnElementsList: String;
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
function TStorageControllerObj.ReturnWeightsList: String;
begin
    if FleetSize = 0 then
    begin
        Result := '';
        Exit;
    end;

    Result := GetDSSArray_Real(FleetSize, FWeights);

end;

initialization

    CDoubleOne := Cmplx(1.0, 1.0);

end.
