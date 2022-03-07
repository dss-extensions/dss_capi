unit InvControl;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015,  Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
//   A InvControl is a control element that is connected to a terminal of another
//   circuit element and sends kW and/or kvar signals to a set of PVSystem objects it controls
// 
//   A InvControl is defined by a New command:
// 
//   New InvControl.Name=myname PVSystemList = (pvsystem1  pvsystem2 ...)
// 
// Notes:
//   WGS (11/26/2012): Using dynamic arrays for many private variables in this unit.
//   Although dynamic arrays begin at 0 (by definition in Delphi),
//   this unit is using 1 to numberelements in all for loops - the 0th
//   element is un-used (except for Strings) in this unit.
//   All dynamic arrays are set to length numberelements+1 in the appropriate dimension.
//   All dynamic arrays are Finalize'd in the destroy procedure.
// 
//   // Updated 9/24/2015 to allow for simultaneous modes and additional functionality

interface

uses
    RollAvgWindow,
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    PVSystem,
    Arraydef,
    UComplex, DSSUcomplex,
    utilities,
    XYcurve,
    Dynamics,
    DSSPointerList,
    Classes,
    StrUtils;

type
{$SCOPEDENUMS ON}
    TInvControlProp = (
        INVALID = 0,
        PVSystemList = 1,
        Mode = 2,
        CombiMode = 3,
        vvc_curve1 = 4,
        hysteresis_offset = 5,
        voltage_curvex_ref = 6,
        avgwindowlen = 7,
        voltwatt_curve = 8,
        //following for dynamic reactive current mode
        DbVMin = 9,
        DbVMax = 10,
        ArGraLowV = 11,
        ArGraHiV = 12,
        DynReacavgwindowlen = 13,
        DeltaQ_factor = 14,
        VoltageChangeTolerance = 15,
        VarChangeTolerance = 16,
        VoltwattYAxis = 17,
        RateofChangeMode = 18,
        LPFTau = 19,
        RiseFallLimit = 20,
        DeltaP_factor = 21,
        EventLog = 22,
        VV_RefReactivePower = 23,
        ActivePChangeTolerance = 24
    );
{$SCOPEDENUMS OFF}

    ERateofChangeMode = (
        INACTIVE,
        LPF,
        RISEFALL
    );

    TInvControl = class(TControlClass)
    PRIVATE
        XY_CurveClass: TDSSClass;
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;
        function NewObject(const ObjName: Ansistring; Activate: Boolean = True): Pointer; OVERRIDE;

        procedure UpdateAll;
    end;

    TInvControlObj = class(TControlElem)
    PRIVATE
        ControlMode: Integer;
        CombiMode: Integer;
        ControlActionHandle: Integer;
        ControlledElement: array of TPVSystemObj;    // list of pointers to controlled PVSystem elements

        FkWLimit,
        FkvarLimit,
        FkVALimit,
        FVref,  // kV rating for the PVSystem object
        FPpf,  // power factor parameter from the PVSystem object, not necessarily present pf 'output' if limited by kva rating or other parameters
        Fpresentkvar, // kvar parameter from the PVSystem object, not necessarily present kvar output if limited by kva rating or other parameters
        FpresentkW: array of Double;
        NPhasesPVSys: array of Integer;
        NCondsPVSys: array of Integer;
        FListSize: Integer;
        FPVSystemNameList: TStringList;
        RateofChangeMode: ERateofChangeMode;
        FLPFTau: Double;
        FPVSystemPointerList: TDSSPointerList;

        Fvvc_curve: TXYcurveObj;
        Fvvc_curveOffset: Double;
        FActiveVVCurve: array of Integer;
        FVoltage_CurveX_ref: Integer;  // valid values are 0: = Vref (rated), 1:= avg
        FFinalpuPmpp: array of Double;
        FFinalkvar: array of Double;
        cBuffer: array of array of Complex;    // Complexarray buffer
        CondOffset: array of Integer; // Offset for monitored terminal
        FVV_ReacPower_ref: Integer;

        FVVDeltaVtolerance: Double;

        Fvoltwatt_curve: TXYcurveObj;

        FAvgpVuPrior: array of Double;
        FPriorWattspu: array of Double;
        FPriorvarspu: array of Double;
        FLPFTime: array of Double;
        FRiseFallLimit: Double;
        FPresentVpu: array of Double;
        FvoltwattDeltaVTolerance: Double; // tolerance of voltage change from one solution to the
        FPendingChange: array of Integer;
        FFlagROCOnly: array of Boolean;


        QDeliver: array of Double;
        QNew: array of Double; //volt-var new set-point
        QOld: array of Double;
        QOldVV: array of Double;
        QOldDRC: array of Double;
        PNew: array of Double;
        POld: array of Double;
        QDRCNew: array of Double; //dynamic reactive power new set-point

        QHeadRoom: array of Double;
        Qoutputpu: array of Double;
        QoutputVVpu: array of Double;
        QoutputDRCpu: array of Double;


        Qdesiredpu: array of Double;
        QDRCdesiredpu: array of Double;
        FVpuSolution: array of array of Double;
        FVpuSolutionIdx: Integer;
        FdeltaQ_factor: Double;
        FdeltaP_factor: Double;


            //following for dynamic reactive current mode
        FDbVMin, FDbVMax, FArGraLowV, FArGraHiV: Double;
        FRollAvgWindow: array of TRollAvgWindow;
        FRollAvgWindowLength: Integer; // FVAvgWindowLengthSec // rolling average window length in seconds
        deltaVDynReac: array of Double;
        priorRollAvgWindow: array of Double;
        FDRCRollAvgWindow: array of TRollAvgWindow;
        FDRCRollAvgWindowLength: Integer; // FDRCVAvgWindowLengthSec // rolling average window length in seconds
        priorDRCRollAvgWindow: array of Double;
        FlagChangeCurve: array of Boolean;
        FVoltwattYAxis: Integer; // 1 = %Pmpp, 0 = %Available power
        FVoltageChangeTolerance: Double;
        FVarChangeTolerance: Double;
        FActivePChangeTolerance: Double;
        FWithinTol: array of Boolean;
        FWithinTolVV: array of Boolean;
        FWithinTolVW: array of Boolean;
        FROCEvaluated: array of Boolean;
        FHitkVALimit: array of Boolean;
        FHitkvarLimit: array of Boolean;

        procedure Set_PendingChange(Value: Integer; DevIndex: Integer);
        function Get_PendingChange(DevIndex: Integer): Integer;
        function ReturnElementsList: Ansistring;
        procedure UpdateInvControl(i: Integer);
    PUBLIC
        constructor Create(ParClass: TDSSClass; const InvControlName: Ansistring);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        // procedure Set_Enabled(Value: Boolean); OVERRIDE;
        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData; OVERRIDE;

            // Sample control quantities and set action times in Control Queue
        procedure Sample; OVERRIDE;

            // Do the action that is pending from last sample
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;

        procedure Reset; OVERRIDE;  // Reset to initial defined state

        procedure CalcVoltWatt_pu(j: Integer);
        procedure CalcVoltVar_vars(j: Integer);
        procedure CalcDRC_vars(j: Integer);
        function CalcLPF(m: Integer; powertype: Ansistring; PVSys: TPVSystemObj): Double;
        function CalcRF(m: Integer; powertype: Ansistring; PVSys: TPVSystemObj): Double;

        function MakePVSystemList: Boolean;

        property PendingChange[DevIndex: Integer]: Integer READ Get_PendingChange WRITE Set_PendingChange;

    end;

implementation

uses
    Sysutils,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    uCmatrix,
    MathUtil,
    Math,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TInvControlObj;
    TProp = TInvControlProp;

const
    NumPropsThisClass = Ord(High(TProp));
    NONE = 0;
    CHANGEVARLEVEL = 1;
    CHANGEWATTLEVEL = 2;
    CHANGEWATTVARLEVEL = 3;
    CHANGEDRCVVARLEVEL = 4;
    VARAVAL_WATTS = 0;
    VARMAX_VARS = 1;
    VARMAX_WATTS = 2;

    NONE_MODE = 0;
    VOLTVAR = 1;
    VOLTWATT = 2;
    DRC = 3;
    FIXEDPF = 4;

    NONE_COMBMODE = 0;
    VV_VW = 1;
    VV_DRC = 2;
var
    PropInfo: Pointer = NIL;    
    ModeEnum, CombiModeEnum, RoCEnum, VV_RefQEnum, VWYAxisEnum, VCurveXRefEnum: TDSSEnum;

constructor TInvControl.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        VCurveXRefEnum := TDSSEnum.Create('InvControl: Curve X Reference', True, 1, 3, 
            ['rated', 'avg', 'ravg'],
            [0, 1, 2]);
        VWYAxisEnum := TDSSEnum.Create('InvControl: Volt-Watt Y-Axis', True, 2, 2, 
            ['pavailablepu', 'pmpppu', 'pctpmpppu'],
            [0, 1, 2]);
        ModeEnum := TDSSEnum.Create('LegacyInvControl: Control Mode', True, 1, 5, 
            ['Voltvar', 'VoltWatt', 'DynamicReaccurr', 'FixedPF'],
            [ord(VOLTVAR), ord(VOLTWATT), ord(DRC), ord(FIXEDPF)]);
        CombiModeEnum := TDSSEnum.Create('LegacyInvControl: Combi Mode', True, 4, 4, 
            ['VV_VW', 'VV_DRC'], [ord(VV_VW), ord(VV_DRC)]);
        RoCEnum := TDSSEnum.Create('LegacyInvControl: Rate-of-change Mode', True, 3, 3, 
            ['Inactive', 'LPF', 'RiseFall'], [ord(INACTIVE), ord(LPF), ord(RISEFALL)]);
        VV_RefQEnum := TDSSEnum.Create('LegacyInvControl: VV Reactive Power Reference', True, 4, 8, 
            ['VARAVAL_WATTS', 'VARMAX_VARS', 'VARMAX_WATTS'], [0, 1, 2]);
        VV_RefQEnum.AllowLonger := True;
        VV_RefQEnum.DefaultValue := VARAVAL_WATTS;
    end;
    XY_CurveClass := GetDSSClassPtr(dssContext, 'XYCurve');

    inherited Create(dssContext, INV_CONTROL, 'InvControl');
end;

destructor TInvControl.Destroy;
begin
    inherited Destroy;
end;

procedure TInvControl.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // enums
    PropertyType[ord(TProp.voltage_curvex_ref)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.voltage_curvex_ref)] := ptruint(@obj.FVoltage_CurveX_ref);
    PropertyOffset2[ord(TProp.voltage_curvex_ref)] := PtrInt(VCurveXRefEnum);

    PropertyType[ord(TProp.VoltwattYAxis)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.VoltwattYAxis)] := ptruint(@obj.FVoltwattYAxis);
    PropertyOffset2[ord(TProp.VoltwattYAxis)] := PtrInt(VWYAxisEnum);

    PropertyType[ord(TProp.Mode)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Mode)] := ptruint(@obj.ControlMode);
    PropertyOffset2[ord(TProp.Mode)] := PtrInt(ModeEnum);

    PropertyType[ord(TProp.CombiMode)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.CombiMode)] := ptruint(@obj.CombiMode);
    PropertyOffset2[ord(TProp.CombiMode)] := PtrInt(CombiModeEnum);

    PropertyType[ord(TProp.RateofChangeMode)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.RateofChangeMode)] := ptruint(@obj.RateofChangeMode);
    PropertyOffset2[ord(TProp.RateofChangeMode)] := PtrInt(RoCEnum);

    PropertyType[ord(TProp.VV_RefReactivePower)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.VV_RefReactivePower)] := ptruint(@obj.FVV_ReacPower_ref);
    PropertyOffset2[ord(TProp.VV_RefReactivePower)] := PtrUInt(VV_RefQEnum);

    // object references
    PropertyType[ord(TProp.vvc_curve1)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.vvc_curve1)] := ptruint(@obj.Fvvc_curve);
    PropertyOffset2[ord(TProp.vvc_curve1)] := ptruint(DSS.XYCurveClass);

    PropertyType[ord(TProp.voltwatt_curve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.voltwatt_curve)] := ptruint(@obj.Fvoltwatt_curve);
    PropertyOffset2[ord(TProp.voltwatt_curve)] := ptruint(DSS.XYCurveClass);

    // boolean properties
    PropertyType[ord(TProp.EventLog)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.EventLog)] := ptruint(@obj.ShowEventLog);

    // string lists
    PropertyType[ord(TProp.PVSystemList)] := TPropertyType.StringListProperty; // TODO: convert to array of objects?
    PropertyOffset[ord(TProp.PVSystemList)] := ptruint(@obj.FPVSystemNameList);

    // double properties (default type)
    PropertyOffset[ord(TProp.DbVMin)] := ptruint(@obj.FDbVMin);
    PropertyOffset[ord(TProp.DbVMax)] := ptruint(@obj.FDbVMax);
    PropertyOffset[ord(TProp.ArGraLowV)] := ptruint(@obj.FArGraLowV);
    PropertyOffset[ord(TProp.ArGraHiV)] := ptruint(@obj.FArGraHiV);
    PropertyOffset[ord(TProp.deltaQ_Factor)] := ptruint(@obj.FdeltaQ_factor);
    PropertyOffset[ord(TProp.VoltageChangeTolerance)] := ptruint(@obj.FVoltageChangeTolerance);
    PropertyOffset[ord(TProp.VarChangeTolerance)] := ptruint(@obj.FVarChangeTolerance);
    PropertyOffset[ord(TProp.deltaP_Factor)] := ptruint(@obj.FdeltaP_factor);
    PropertyOffset[ord(TProp.ActivePChangeTolerance)] := ptruint(@obj.FActivePChangeTolerance);
    PropertyOffset[ord(TProp.LPFTau)] := ptruint(@obj.FLPFTau);
    PropertyOffset[ord(TProp.RiseFallLimit)] := ptruint(@obj.FRiseFallLimit);

    // advanced doubles
    PropertyOffset[ord(TProp.hysteresis_offset)] := ptruint(@obj.Fvvc_curveOffset);
    PropertyFlags[ord(TProp.hysteresis_offset)] := [TPropertyFlag.NonPositive];

    // integer
    PropertyType[ord(TProp.DynReacavgwindowlen)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.DynReacavgwindowlen)] := ptruint(@obj.FDRCRollAvgWindowLength);
    PropertyFlags[ord(TProp.DynReacavgwindowlen)] := [TPropertyFlag.IntervalUnits];

    PropertyType[ord(TProp.avgwindowlen)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.avgwindowlen)] := ptruint(@obj.FRollAvgWindowLength);
    PropertyFlags[ord(TProp.avgwindowlen)] := [TPropertyFlag.IntervalUnits];

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TInvControl.NewObject(const ObjName: Ansistring; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure ValidateXYCurve(dss: TDSSContext; var curve: TXYcurveObj; InvControlMode: Integer);
var
    i: Integer;
begin
    if curve = NIL then
        Exit;

    // If VOLTWATT control mode then check for any negative watt values (pu)
    // and values greater than 1.0 per-unit (=100 percent output)
    if InvControlMode = VOLTWATT then
    begin
        for i := 1 to curve.NumPoints do
        begin
            if (curve.YValue_pt[i] < 0.0) or (curve.YValue_pt[i] > 1.0) then
            begin
                DoSimpleMsg(DSS, 'XY Curve object: "%s" has active power value(s) greater than 1.0 per-unit or less than 0.0 per-unit.  Not allowed for VOLTWATT control mode for PVSystems', [curve.Name], 381);
                curve := NIL;
                Break;
            end;
        end;
    end;
end;

procedure TInvControlObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of
        1:
        begin // re-alloc based on
            FPVSystemPointerList.Clear; // clear this for resetting on first sample
            FListSize := FPVSystemNameList.count;
        end;
        4:
            ValidateXYCurve(DSS, Fvvc_curve, VOLTVAR);
        8:
            ValidateXYCurve(DSS, Fvoltwatt_curve, VOLTWATT);
        9:
            if (FDbVMax > 0.0) and (FDbVmin > FDbVMax) then
            begin
                DoSimpleMsg('Minimum dead-band voltage value should be less than the maximum dead-band voltage value.  Value set to 0.0 "%s" for object "%s"', [ParentClass.PropertyName[Idx], FullName], 1365);
                FDbvMin := 0.0;
            end;
        10:
            if (FDbVMin > 0.0) and (FDbVMax < FDbVmin) then
            begin
                DoSimpleMsg('Maximum dead-band voltage value should be greater than the minimum dead-band voltage value.  Value set to 0.0 "%s" for object "%s"', [ParentClass.PropertyName[Idx], FullName], 1366);
                FDbvMax := 0.0;
            end;
        ord(TProp.Mode):
            CombiMode := NONE_COMBMODE;
        ord(TProp.CombiMode):
            ControlMode := NONE_MODE;
        ord(TProp.LPFTau):
            if FLPFTau <= 0  then
                RateofChangeMode := INACTIVE;
        ord(TProp.RiseFallLimit):
            if FRiseFallLimit <= 0 then
                RateofChangeMode := INACTIVE;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TInvControlObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff

    for i := 1 to FPVSystemPointerList.Count do
    begin
        ControlledElement[i] := Other.ControlledElement[i];

        FkWLimit[i] := Other.FkWLimit[i];
        FkvarLimit[i] := Other.FkvarLimit[i];
        FkVALimit[i] := Other.FkVALimit[i];
        FVref[i] := Other.FVref[i];
        FPpf[i] := Other.FPpf[i];
        Fpresentkvar[i] := Other.Fpresentkvar[i];
        FpresentkW[i] := Other.FpresentkW[i];

        CondOffset[i] := Other.CondOffset[i];
        FWithinTol[i] := Other.FWithinTol[i];
        FWithinTolVV[i] := Other.FWithinTolVV[i];
        FWithinTolVW[i] := Other.FWithinTolVW[i];
        FROCEvaluated[i] := Other.FROCEvaluated[i];
        FFinalpuPmpp[i] := Other.FFinalpuPmpp[i];
        FFinalkvar[i] := Other.FFinalkvar[i];
        FHitkVALimit[i] := Other.FHitkVALimit[i];
        FHitkvarLimit[i] := Other.FHitkvarLimit[i];
    end;

    ControlMode := Other.ControlMode;
    CombiMode := Other.CombiMode;
    FListSize := Other.FListSize;
    Fvvc_curve := Other.Fvvc_curve;
    Fvvc_curveOffset := Other.Fvvc_curveOffset;
    FVoltage_CurveX_ref := Other.FVoltage_CurveX_ref;
    Fvoltwatt_curve := Other.Fvoltwatt_curve;
    FDbVMin := Other.FDbVMin;
    FDbVMax := Other.FDbVMax;
    FArGraLowV := Other.FArGraLowV;
    FArGraHiV := Other.FArGraHiV;
    FActiveVVCurve := Other.FActiveVVCurve;
    FRollAvgWindowLength := Other.FRollAvgWindowLength;
    FDRCRollAvgWindowLength := Other.FDRCRollAvgWindowLength;
    FActivePChangeTolerance := Other.FActivePChangeTolerance;
    FvoltwattDeltaVTolerance := Other.FvoltwattDeltaVTolerance;
    FdeltaQ_factor := Other.FdeltaQ_factor;
    FdeltaP_factor := Other.FdeltaP_factor;
    FVoltageChangeTolerance := Other.FVoltageChangeTolerance;
    FVarChangeTolerance := Other.FVarChangeTolerance;
    FVoltwattYAxis := Other.FVoltwattYAxis;
    RateofChangeMode := Other.RateofChangeMode;
    FLPFTau := Other.FLPFTau;
    FRiseFallLimit := Other.FRiseFallLimit;
    TimeDelay := Other.TimeDelay;
end;

constructor TInvControlObj.Create(ParClass: TDSSClass; const InvControlName: Ansistring);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(InvControlName);
    DSSObjType := ParClass.DSSClassType;
     {
       Control elements are zero current sources that attach to a terminal of a
       power-carrying device, but do not alter voltage or current flow.
       Define a default number of phases and conductors here and update in
       RecalcElementData  routine if necessary. This allocates arrays for voltages
       and currents and gives more direct access to the values,if needed
     }
    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class

    ControlMode := NONE_MODE;
    CombiMode := NONE_COMBMODE;
    ControlledElement := NIL;
    FkWLimit := NIL;
    FkvarLimit := NIL;
    FkVALimit := NIL;
    FVref := NIL;
    FPpf := NIL;
    Fpresentkvar := NIL;
    FpresentkW := NIL;
    NPhasesPVSys := NIL;
    NCondsPVSys := NIL;
    FPVSystemNameList := NIL;
    FPVSystemPointerList := NIL;
    Fvvc_curve := NIL;
    Fvvc_curveOffset := 0.0;
    FActiveVVCurve := NIL;
    FVoltage_CurveX_ref := 0;
    cBuffer := NIL;
    CondOffset := NIL;
    FPriorWattspu := NIL;
    FPriorvarspu := NIL;
    FLPFTime := NIL;
    FRiseFallLimit := 0.001;

     // following applicable to volt-watt and volt-var
    FRollAvgWindow := NIL;
    FRollAvgWindowLength := 1;

    FDRCRollAvgWindow := NIL;
    FDRCRollAvgWindowLength := 1;

     // volt-watt, only related variables
    Fvoltwatt_curve := NIL;
    FAvgpVuPrior := NIL;
    FPresentVpu := NIL;
    FvoltwattDeltaVTolerance := 0.00001;  // per-unit change in voltage tolerance
                                         // typically between a prior solution and the present solution
    FVVDeltaVtolerance := 0.00001;
    FPendingChange := NIL;
    FFlagROCOnly := NIL;
      // following apply to volt-var only
    QDeliver := NIL;
    QNew := NIL;
    QOld := NIL;
    QOldVV := NIL;
    QOldDRC := NIL;
    QHeadRoom := NIL;
    PNew := NIL;
    POld := NIL;

    QDRCNew := NIL;

    FVpuSolution := NIL;
    FVpuSolutionIdx := 0;
    FdeltaQ_factor := 0.7;
    FdeltaP_factor := 1.0;
    Qoutputpu := NIL;
    QoutputVVpu := NIL;
    QoutputDRCpu := NIL;
    Qdesiredpu := NIL;
    QDRCdesiredpu := NIL;
    FVoltwattYAxis := 1;
    FVoltageChangeTolerance := 0.0001;
    FVarChangeTolerance := 0.025;
    FActivePChangeTolerance := 0.01;
    RateofChangeMode := INACTIVE;
    FLPFTau := 0.001;

    FlagChangeCurve := NIL;
    FWithinTol := NIL;
    FWithinTolVV := NIL;
    FWithinTolVW := NIL;
    FROCEvaluated := NIL;
    FHitkVALimit := NIL;
    FHitkvarLimit := NIL;

    FPVSystemNameList := TSTringList.Create;
    FPVSystemPointerList := TDSSPointerList.Create(20);  // Default size and increment

      //following for dynamic reactive current mode
    FDbVMin := 0.95;
    FDbVMax := 1.05;
    FArGraLowV := 0.1;
    FArGraHiV := 0.1;
    deltaVDynReac := NIL;
    priorRollAvgWindow := NIL;
    priorDRCRollAvgWindow := NIL;
    FVV_ReacPower_ref := VARAVAL_WATTS;


    FFinalpuPmpp := NIL;
    FFinalkvar := NIL;

     //generic for control
    FPendingChange := NIL;
    FFlagROCOnly := NIL;
end;

destructor TInvControlObj.Destroy;
begin
    Finalize(ControlledElement);
    Finalize(FkWLimit);
    Finalize(FkvarLimit);
    Finalize(FkVALimit);
    Finalize(FVref);
    Finalize(FPpf);
    Finalize(Fpresentkvar);
    Finalize(FpresentkW);
    Finalize(NPhasesPVSys);
    Finalize(NCondsPVSys);
    Finalize(cBuffer);
    Finalize(CondOffset);
    Finalize(FRollAvgWindow);
    Finalize(FDRCRollAvgWindow);

    Finalize(FAvgpVuPrior);
    Finalize(FPresentVpu);

    Finalize(FPendingChange);
    Finalize(FFlagROCOnly);
    Finalize(QDeliver);
    Finalize(QNew);
    Finalize(QOld);
    Finalize(QOldVV);
    Finalize(QOldDRC);
    Finalize(QHeadroom);
    Finalize(Qoutputpu);
    Finalize(QoutputVVpu);
    Finalize(QoutputDRCpu);
    Finalize(Qdesiredpu);
    Finalize(QDRCdesiredpu);
    Finalize(QDRCNew);
    Finalize(PNew);
    Finalize(POld);
    Finalize(deltaVDynReac);
    Finalize(priorRollAvgWindow);
    Finalize(priorDRCRollAvgWindow);
    Finalize(FVpuSolution);
    Finalize(FlagChangeCurve);
    Finalize(FActiveVVCurve);
    Finalize(FPriorWattspu);
    Finalize(FPriorvarspu);
    Finalize(FLPFTime);
    Finalize(FWithinTol);
    Finalize(FWithinTolVV);
    Finalize(FWithinTolVW);
    Finalize(FROCEvaluated);
    Finalize(FFinalpuPmpp);
    Finalize(FFinalkvar);
    Finalize(FHitkVALimit);
    Finalize(FHitkvarLimit);


    inherited Destroy;
end;

procedure TInvControlObj.RecalcElementData;
var
    i: Integer;
begin
    if FPVSystemPointerList.Count = 0 then
        MakePVSystemList;

    if FPVSystemPointerList.Count > 0 then
    {Setting the terminal of the InvControl device to same as the 1st PVSystem element}
    { This sets it to a realistic value to avoid crashes later }
    begin
        MonitoredElement := TDSSCktElement(FPVSystemPointerList.Get(1));   // Set MonitoredElement to 1st PVSystem in lise
        Setbus(1, MonitoredElement.Firstbus);
    end;

    for i := 1 to FPVSystemPointerList.Count do
    begin
        // User ControlledElement[] as the pointer to the PVSystem elements
        ControlledElement[i] := TPVSystemObj(FPVSystemPointerList.Get(i));  // pointer to i-th PVSystem
        SetLength(cBuffer[i], SizeOF(Complex) * ControlledElement[i].Yorder);

        ControlledElement[i].ActiveTerminalIdx := 1; // Make the 1 st terminal active
        FNphases := ControlledElement[i].NPhases;
        Nconds := Nphases;
        FRollAvgWindow[i].SetLength(FRollAvgWindowLength);
        FDRCRollAvgWindow[i].SetLength(FDRCRollAvgWindowLength);
        if (ControlledElement[i] <> NIL) then
            with ControlledElement[i] do
            begin
                FkVALimit[i] := kVARating;
                FVref[i] := PresentkV;
                FkWLimit[i] := Pmpp; // AC
                FkvarLimit[i] := kVARating;  // can output vars up to the kva limit of the inverter
                FPpf[i] := PowerFactor;
                Fpresentkvar[i] := Presentkvar;
                FpresentkW[i] := PresentkW;
                CondOffset[i] := (NTerms - 1) * NCondsPVSys[i]; // for speedy sampling
            end
        else
        begin
            ControlledElement[i] := NIL; // PVSystem element not found
            DoErrorMsg(Format(_('InvControl: "%s"'), [Self.Name]),
                Format(_('Controlled Element "%s" not found.'), [FPVSystemNameList.Strings[i - 1]]),
                _('PVSystem object must be defined previously.'), 361);
        end;
    end;
end;

procedure TInvControlObj.MakePosSequence();

// ***  This assumes the PVSystem devices have already been converted to pos seq

begin
    if FPVSystemPointerList.Count = 0 then
        RecalcElementData;
    FNphases := 3;
    Nconds := 3;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));

    if FPVSystemPointerList.Count > 0 then
    {Setting the terminal of the InvControl device to same as the 1st PVSystem element}
    { This sets it to a realistic value to avoid crashes later }
    begin
        MonitoredElement := TDSSCktElement(FPVSystemPointerList.Get(1));   // Set MonitoredElement to 1st PVSystem in lise
        Setbus(1, MonitoredElement.Firstbus);
        FNphases := MonitoredElement.NPhases;
        Nconds := Nphases;

    end;
    inherited;
end;

procedure TInvControlObj.DoPendingAction;
var
    k: Integer;
    // SMonitoredElement                         :Complex;
    Qtemp, PTemp, QTemp2: Double;
    pctVV, pctDRC, QTemporig: Double;

    // local pointer to current PVSystem element
    PVSys: TPVSystemObj;
begin
    QTemp2 := 0.0;

    for k := 1 to FPVSystemPointerList.Count do
    begin
        PVSys := ControlledElement[k];   // Use local variable in loop

        // SMonitoredElement := PVSys.Power[1]; // s is in va

        if (ControlMode = NONE_MODE) and (CombiMode = VV_DRC) and (PendingChange[k] = CHANGEDRCVVARLEVEL) then
        begin
            if (FFlagROCOnly[k] = FALSE) then
            begin
                CalcVoltVar_vars(k);
                CalcDRC_vars(k);
                QTemp := QNew[k] + QDRCNew[k];


                QTemporig := QTemp;
                if (QTemp = 0) then
                begin
                    if abs(QTemp) > abs(PVSys.kvarLimit) then
                        QTemp := sign(QTemp) * 1.0 * PVSys.kvarLimit;
                    PVSys.Presentkvar := QTemp;
                    QTemp2 := PVSys.Presentkvar;
                    Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];

                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                            Format('**VV_DRC mode set PVSystem output var level to**, kvar= %.5g',
                            [PVSys.Presentkvar, FPresentVpu[k]]));

                    ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                    FAvgpVuPrior[k] := FPresentVpu[k];
                    QOld[k] := QTemp;


//              WriteDLLDebugFile(Format('%g, %d, %.6g, %.6g, %.6g, %s', [ActiveCircuit.Solution.Dynavars.t, ActiveCircuit.Solution.ControlIteration, QOldVV[k],QoldDRC[k],QTemp, 'after limit (set-point).']));

                    Set_PendingChange(NONE, k);

                    exit;

                end;


                pctVV := QNew[k] / QTemp;
                pctDRC := QDRCNew[k] / QTemp;
//              WriteDLLDebugFile(Format('%g, %d, %.6g, %.6g, %.6g, %s', [ActiveCircuit.Solution.Dynavars.t, ActiveCircuit.Solution.ControlIteration, QNew[k],QDRCNew[k], QTemp, 'before limit.']));

              //Respect the PVSystem's maximum kvar limit, first
                if abs(Qtemp2) > abs(PVSys.kvarLimit) then
                begin
                    Qtemp2 := sign(Qtemp) * 0.99 * PVSys.kvarLimit;
                    QDesiredpu[k] := pctVV * (Qtemp2 / QTemporig) * QDesiredpu[k];
                    QDRCDesiredpu[k] := pctDRC * (Qtemp2 / QTemporig) * QDRCDesiredpu[k];
                    FHitkvarLimit[k] := TRUE;
                end;

                PVSys.SetNominalPVSystemOuput;
                PTemp := PVSys.PresentkW;
              // if the desired kW and desired kvar exceed the kva rating of the PVSystem's inverter then...
                if SQRT(Sqr(QTemp2) + Sqr(PTemp)) > PVSys.kVARating then
                begin
                  //...if watts have precedence, reduce the reactive power to not exceed the kva rating
                    if (FVV_ReacPower_ref = VARAVAL_WATTS) or (FVV_ReacPower_ref = VARMAX_WATTS) then
                    begin
                        Qtemp2 := 0.99 * sign(Qtemp2) * SQRT(Sqr(PVSys.kVARating) - Sqr(PTemp));
                        Qnew[k] := Qtemp2;
                        PVSys.Presentkvar := Qnew[k];
                    end

                  //...else, vars have precedence, reduce the active power to not exceed the kva rating
                    else
                    begin
                        PTemp := 0.99 * sign(PTemp) * SQRT(Sqr(PVSys.kVARating) - Sqr(QTemp2));
                      // Set the active power
                        FFinalpuPmpp[k] := PTemp / PVSys.Pmpp;
                        PVSys.VWmode := TRUE;
                        PVSys.VWYAxis := FVoltwattYAxis;
                        PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
                        if (FFlagROCOnly[k] = FALSE) then
                        begin
                            if (RateofChangeMode = INACTIVE) or (ActiveCircuit.Solution.Dynavars.dblHour = 0.0) then
                            begin
                                PVSys.puPmpp := FFinalpuPmpp[k];
                                PNew[k] := FFinalpuPmpp[k];

                                if ShowEventLog then
                                    AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                                        Format('**VOLTVAR VARMAX_VARS mode limited PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp, FPriorWattspu[k]]));
                                Qnew[k] := Qtemp2;
                                PVSys.Presentkvar := Qnew[k];

                                ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                                FAvgpVuPrior[k] := FPresentVpu[k];
                                POld[k] := PVSys.puPmpp;
                            end;
                        end;
                    end;
                    FHitkvaLimit[k] := TRUE;
                end;


              // Set the reactive power, if it is different than the present PVSystem kvar setting
                PVSys.VWmode := FALSE;
                PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
                PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar

                if PVSys.Presentkvar <> QTemp then
                begin
                    if abs(QTemp) > abs(PVSys.kvarLimit) then
                        QTemp := sign(QTemp) * 1.0 * PVSys.kvarLimit;
                    PVSys.Presentkvar := QTemp;
                end;

                QoutputVVpu[k] := pctVV * PVSys.Presentkvar / QHeadroom[k];
                QoutputDRCpu[k] := pctDRC * PVSys.Presentkvar / QHeadroom[k];
                Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];

                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                        Format('**VV_DRC mode set PVSystem output var level to**, kvar= %.5g',
                        [PVSys.Presentkvar, FPresentVpu[k]]));

                ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                FAvgpVuPrior[k] := FPresentVpu[k];
                QOld[k] := QTemp;

                QOldVV[k] := pctVV * QTemp;

                QoldDRC[k] := pctDRC * QTemp;


            end;

            if (FFlagROCOnly[k] = TRUE) then
            begin
            // Apply LPF volt-var
                if (RateofChangeMode = LPF) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
                begin
                    FROCEvaluated[k] := TRUE;
                    Qtemp := CalcLPF(k, 'VARS', PVSys);
                    if (Qtemp <> -999.99) then
                    begin
                        if abs(Qtemp) > abs(PVSys.kvarLimit) then
                            Qnew[k] := sign(Qtemp) * 1.0 * PVSys.kvarLimit
                        else
                            Qnew[k] := Qtemp;
                        PVSys.Presentkvar := Qnew[k];
                        Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                                Format('VV_DRC mode, LPF set PVSystem output var level to**, kvar= %.5g',
                                [Qnew[k]]));
                        FAvgpVuPrior[k] := FPresentVpu[k];
                        QOld[k] := QNew[k];
                        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                    end
                end;

            // Apply Rise/Fall volt-var
                if (RateofChangeMode = RISEFALL) and (ActiveCircuit.Solution.DynaVars.dblHour > 0.0) then
                begin
                    Qtemp := CalcRF(k, 'VARS', PVSys);
                    FROCEvaluated[k] := TRUE;
                    if (Qtemp <> -999.99) then
                    begin
                        if abs(Qtemp) > abs(PVSys.kvarLimit) then
                            Qnew[k] := sign(Qtemp) * 1.0 * PVSys.kvarLimit
                        else
                            Qnew[k] := Qtemp;
                        PVSys.Presentkvar := Qnew[k];
                        Qoutputpu[k] := Qnew[k] / QHeadroom[k];
                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                                Format('VV_DRC mode, RISEFALL set PVSystem output var level to**, kvar= %.5g',
                                [Qnew[k]]));
                        FAvgpVuPrior[k] := FPresentVpu[k];
                        QOld[k] := QNew[k];
                        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                    end;
                end;
            end;
            Set_PendingChange(NONE, k);
        end;

        if (ControlMode = NONE_MODE) and (CombiMode = VV_VW) and (PendingChange[k] = CHANGEWATTVARLEVEL) then
        begin
            if (FFlagROCOnly[k] = FALSE) then
            begin
                CalcVoltVar_vars(k);
                CalcVoltWatt_pu(k);

              //Respect the PVSystem's maximum kvar limit, first
                if abs(QNew[k]) > abs(PVSys.kvarLimit) then
                begin
                    Qnew[k] := sign(QNew[k]) * 0.99 * PVSys.kvarLimit;
                    FHitkvarLimit[k] := TRUE;
                end;
                QTemp2 := Qnew[k];
              //Convert output from CalcVoltWatt_pu to kW
                PVSys.VWmode := TRUE;
                PVSys.VWYAxis := FVoltwattYAxis;
                PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
                if (FFlagROCOnly[k] = FALSE) then
                begin
                    if (RateofChangeMode = INACTIVE) or (ActiveCircuit.Solution.Dynavars.dblHour = 0.0) then
                        if (FVoltwattYAxis = 0) or (FVoltwattYAxis = 1) then
                        begin
                            if (FVoltwattYaxis = 0) then
                                Ptemp := (PVSys.PVSystemVars.PanelkW * PVSys.PVSystemVars.EffFactor * FFinalpuPmpp[k]);
                            if (FVoltwattYaxis = 1) then
                                Ptemp := (FFinalpuPmpp[k] * PVSys.Pmpp);
//                      Ptemp := PVSys.PVSystemVars.PanelkW*PVSys.PVSystemVars.EffFactor*FFinalpuPmpp[k];
                            if SQRT(Sqr(QTemp2) + Sqr(PTemp)) > PVSys.kVARating then
                            begin
                              //...if watts have precedence, reduce the reactive power to not exceed the kva rating
                                if (FVV_ReacPower_ref = VARAVAL_WATTS) or (FVV_ReacPower_ref = VARMAX_WATTS) then
                                begin
                                    if (Ptemp = PVSys.Pmpp) then
                                        QTemp2 := 0.0
                                    else
                                        Qtemp2 := 0.99 * sign(Qtemp2) * SQRT(Sqr(PVSys.kVARating) - Sqr(PTemp));
                                    Qnew[k] := Qtemp2;
                                    PVSys.Presentkvar := Qnew[k];
                                end

                              //...else, vars have precedence, reduce the active power to not exceed the kva rating
                                else
                                begin
                                    if (QTemp2 = PVSys.Pmpp) then
                                        PTemp := 0.0
                                    else
                                        PTemp := 0.99 * sign(PTemp) * SQRT(Sqr(PVSys.kVARating) - Sqr(QTemp2));
                                  // Set the active power
                                    FFinalpuPmpp[k] := PTemp / PVSys.Pmpp;
                                    PVSys.VWmode := TRUE;
                                    PVSys.VWYAxis := FVoltwattYAxis;
                                    PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
                                    if (FFlagROCOnly[k] = FALSE) then
                                    begin
                                        if (RateofChangeMode = INACTIVE) or (ActiveCircuit.Solution.Dynavars.dblHour = 0.0) then
                                        begin
                                            PVSys.puPmpp := FFinalpuPmpp[k];
                                            PNew[k] := FFinalpuPmpp[k];

                                            if ShowEventLog then
                                                AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                                                    Format('**VOLTVAR VARMAX_VARS mode limited PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp, FPriorWattspu[k]]));
                                            Qnew[k] := Qtemp2;
                                            PVSys.Presentkvar := Qnew[k];

                                            ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                                            FAvgpVuPrior[k] := FPresentVpu[k];
                                            POld[k] := PVSys.puPmpp;
                                        end;
                                    end;
                                end;
                                FHitkvaLimit[k] := TRUE;
                            end;


                            PVSys.puPmpp := FFinalpuPmpp[k];
                            PVSys.SetNominalPVSystemOuput;
                            ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                            PNew[k] := FFinalpuPmpp[k];
                        end
                        else
                        begin
                            PVSys.PresentkW := FFinalpuPmpp[k] * PVSys.Pmpp * PVSys.puPmpp;
                        end;
                end;

              // if the desired kW and desired kvar exceed the kva rating of the PVSystem's inverter then...
                PVSys.SetNominalPVSystemOuput;
                PTemp := PVSys.PresentkW;
              // if the desired kW and desired kvar exceed the kva rating of the PVSystem's inverter then...
                if SQRT(Sqr(QTemp2) + Sqr(PTemp)) > PVSys.kVARating then
                begin
                  //...if watts have precedence, reduce the reactive power to not exceed the kva rating
                    if (FVV_ReacPower_ref = VARAVAL_WATTS) or (FVV_ReacPower_ref = VARMAX_WATTS) then
                    begin
                        Qtemp2 := 0.99 * sign(Qtemp2) * SQRT(Sqr(PVSys.kVARating) - Sqr(PTemp));
                        Qnew[k] := Qtemp2;
                        PVSys.Presentkvar := Qnew[k];
                    end

                  //...else, vars have precedence, reduce the active power to not exceed the kva rating
                    else
                    begin
                        PTemp := 0.99 * sign(PTemp) * SQRT(Sqr(PVSys.kVARating) - Sqr(QTemp2));
                      // Set the active power
                        FFinalpuPmpp[k] := PTemp / PVSys.Pmpp;
                        PVSys.VWmode := TRUE;
                        PVSys.VWYAxis := FVoltwattYAxis;
                        PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
                        if (FFlagROCOnly[k] = FALSE) then
                        begin
                            if (RateofChangeMode = INACTIVE) or (ActiveCircuit.Solution.Dynavars.dblHour = 0.0) then
                            begin
                                PVSys.puPmpp := FFinalpuPmpp[k];
                                PNew[k] := FFinalpuPmpp[k];

                                if ShowEventLog then
                                    AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                                        Format('**VOLTVAR VARMAX_VARS mode limited PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp, FPriorWattspu[k]]));
                                Qnew[k] := Qtemp2;
                                PVSys.Presentkvar := Qnew[k];

                                ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                                FAvgpVuPrior[k] := FPresentVpu[k];
                                POld[k] := PVSys.puPmpp;
                            end;
                        end;
                    end;
                    FHitkvaLimit[k] := TRUE;
                end;


              // Set the reactive power, if it is different than the present PVSystem kvar setting
                PVSys.VWmode := FALSE;
                PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1

                PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar
                if PVSys.Presentkvar <> Qnew[k] then
                begin
                    if abs(QNew[k]) > abs(PVSys.kvarLimit) then
                        Qnew[k] := sign(QNew[k]) * 1.0 * PVSys.kvarLimit;
                    PVSys.Presentkvar := Qnew[k];
                end;
              //Respect the PVSystem's maximum kvar limit
                if abs(Qnew[k]) > abs(PVSys.kvarLimit) then
                begin
                    Qnew[k] := sign(Qnew[k]) * 0.99 * PVSys.kvarLimit;
                    FHitkvarLimit[k] := TRUE;
                end;

                PVSys.Presentkvar := Qnew[k];
                Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                        Format('**VV_VW mode set PVSystem output var level to**, kvar= %.5g',
                        [Qnew[k]]));

                ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                FAvgpVuPrior[k] := FPresentVpu[k];
                QOldVV[k] := QNew[k];

              // Set the active power
                FFinalpuPmpp[k] := PTemp / PVSys.Pmpp;
                PVSys.VWmode := TRUE;
                PVSys.VWYAxis := FVoltwattYAxis;
                PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
                if (FFlagROCOnly[k] = FALSE) then
                begin
                    if (RateofChangeMode = INACTIVE) or (ActiveCircuit.Solution.Dynavars.dblHour = 0.0) then
                    begin
                        PVSys.puPmpp := FFinalpuPmpp[k];
                        PNew[k] := FFinalpuPmpp[k];

                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                                Format('**VV_VW mode set PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp, POld[k]]));

                        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                        FAvgpVuPrior[k] := FPresentVpu[k];
                        POld[k] := PVSys.puPmpp;
                    end;
                end;
            end;

            if (FFlagROCOnly[k] = TRUE) then
            begin
            // Apply LPF volt-var
                if (RateofChangeMode = LPF) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
                begin
                    FROCEvaluated[k] := TRUE;
                    Qtemp := CalcLPF(k, 'VARS', PVSys);
                    if (Qtemp <> -999.99) then
                    begin
                        if abs(Qtemp) > abs(PVSys.kvarLimit) then
                            Qnew[k] := sign(Qtemp) * 1.0 * PVSys.kvarLimit
                        else
                            Qnew[k] := Qtemp;
                        PVSys.Presentkvar := Qnew[k];
                        Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                                Format('VV_VW mode, LPF set PVSystem output var level to**, kvar= %.5g',
                                [Qnew[k]]));
                        FAvgpVuPrior[k] := FPresentVpu[k];
                        QOld[k] := QNew[k];
                        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                    end
                end;

            // Apply Rise/Fall volt-var
                if (RateofChangeMode = RISEFALL) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
                begin
                    Qtemp := CalcRF(k, 'VARS', PVSys);
                    if (Qtemp <> -999.99) then
                    begin
                        if abs(Qtemp) > abs(PVSys.kvarLimit) then
                            Qnew[k] := sign(Qtemp) * 1.0 * PVSys.kvarLimit
                        else
                            Qnew[k] := Qtemp;
                        PVSys.Presentkvar := Qnew[k];
                        Qoutputpu[k] := Qnew[k] / QHeadroom[k];
                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                                Format('VV_VW mode, RISEFALL set PVSystem output var level to**, kvar= %.5g',
                                [Qnew[k]]));
                        FAvgpVuPrior[k] := FPresentVpu[k];
                        QOld[k] := QNew[k];
                        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                    end;
                end;

            // rate of change LPF - watts
                if (RateofChangeMode = LPF) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
                begin
                    FROCEvaluated[k] := TRUE;

                    Ptemp := CalcLPF(k, 'WATTS', PVSys);
                    if (Ptemp <> -999.99) then
                    begin
                        if PTemp <> 0.0 then
                        begin
                            PNew[k] := PTemp;
                            PVSys.puPmpp := PNew[k];

                            if ShowEventLog then
                                AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                                    Format('**VV_VW mode, LPF set PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp, FPriorWattspu[k]]));

                            ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                            FAvgpVuPrior[k] := FPresentVpu[k];
                            POld[k] := PVSys.puPmpp;
                        end;
                    end;
                    Set_PendingChange(NONE, k);
                end;


            // rate of change rise/fall limit
                if (RateofChangeMode = RISEFALL) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
                begin
                    FROCEvaluated[k] := TRUE;
                    PTemp := CalcRF(k, 'WATTS', PVSys);
                    if (Ptemp <> -999.99) then
                    begin
                        PNew[k] := PTemp;
                        PVSys.puPmpp := PNew[k];
                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                                Format('**VV_VW mode, RISEFALL set PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp, FPriorWattspu[k]]));

                        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                        FAvgpVuPrior[k] := FPresentVpu[k];
                        POld[k] := PVSys.puPmpp;
                    end;
                      // Force recalc of power parms
                    Set_PendingChange(NONE, k);

                end


            end;
            Set_PendingChange(NONE, k);
        end;


        if (ControlMode = VOLTVAR) and (CombiMode = NONE_COMBMODE) and (PendingChange[k] = CHANGEVARLEVEL) then
        begin
            if (FFlagROCOnly[k] = FALSE) then
            begin
                PVSys.VWmode := FALSE;
                PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1

                PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar
                CalcVoltVar_vars(k);

                if PVSys.Presentkvar <> Qnew[k] then
                begin
                    if abs(QNew[k]) > abs(PVSys.kvarLimit) then
                    begin
                        Qnew[k] := sign(QNew[k]) * 1.0 * PVSys.kvarLimit;
                        FHitkvarLimit[k] := TRUE;
                    end;
                    PVSys.Presentkvar := Qnew[k];
                    QTemp2 := Qnew[k];
                end;
                PVSys.Presentkvar := Qnew[k];
                QTemp2 := Qnew[k];
                PVSys.SetNominalPVSystemOuput;
                PTemp := PVSys.PresentkW;
              // if the desired kW and desired kvar exceed the kva rating of the PVSystem's inverter then...
                if SQRT(Sqr(QTemp2) + Sqr(PTemp)) > PVSys.kVARating then
                begin
                  //...if watts have precedence, reduce the reactive power to not exceed the kva rating
                    if (FVV_ReacPower_ref = VARAVAL_WATTS) or (FVV_ReacPower_ref = VARMAX_WATTS) then
                    begin
                        Qtemp2 := 0.99 * sign(Qtemp2) * SQRT(Sqr(PVSys.kVARating) - Sqr(PTemp));
                        Qnew[k] := Qtemp2;
                        PVSys.Presentkvar := Qnew[k];
                    end

                  //...else, vars have precedence, reduce the active power to not exceed the kva rating
                    else
                    begin
                        PTemp := 0.99 * sign(PTemp) * SQRT(Sqr(PVSys.kVARating) - Sqr(QTemp2));
                      // Set the active power
                        FFinalpuPmpp[k] := PTemp / PVSys.Pmpp;
                        PVSys.VWmode := TRUE;
                        PVSys.VWYAxis := FVoltwattYAxis;
                        PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
                        if (FFlagROCOnly[k] = FALSE) then
                        begin
                            if (RateofChangeMode = INACTIVE) or (ActiveCircuit.Solution.Dynavars.dblHour = 0.0) then
                            begin
                                PVSys.puPmpp := FFinalpuPmpp[k];
                                PNew[k] := FFinalpuPmpp[k];

                                if ShowEventLog then
                                    AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                                        Format('**VOLTVAR VARMAX_VARS mode limited PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp, FPriorWattspu[k]]));
                                Qnew[k] := Qtemp2;
                                PVSys.Presentkvar := Qnew[k];

                                ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                                FAvgpVuPrior[k] := FPresentVpu[k];
                                POld[k] := PVSys.puPmpp;
                            end;
                        end;
                    end;
                    FHitkvaLimit[k] := TRUE;
                end;

                Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
                QoutputVVpu[k] := Qoutputpu[k];
                if ShowEventLog then
                    AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                        Format('VOLTVAR mode set PVSystem output var level to**, kvar= %.5g',
                        [PVSys.Presentkvar]));
                FAvgpVuPrior[k] := FPresentVpu[k];
                QOld[k] := QNew[k];
                QOldVV[k] := QNew[k];
                ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
            end

        // Apply LPF volt-var
            else
            begin
                if (RateofChangeMode = LPF) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
                begin
                    FROCEvaluated[k] := TRUE;
                    Qtemp := CalcLPF(k, 'VARS', PVSys);
                    if (Qtemp <> -999.99) then
                    begin
                        if abs(Qtemp) > abs(PVSys.kvarLimit) then
                            Qnew[k] := sign(Qtemp) * 1.0 * PVSys.kvarLimit
                        else
                            Qnew[k] := Qtemp;
                        PVSys.Presentkvar := Qnew[k];
                        Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                                Format('VOLTVAR mode, LPF set PVSystem output var level to**, kvar= %.5g',
                                [Qnew[k]]));
                        FAvgpVuPrior[k] := FPresentVpu[k];
                        QOld[k] := QNew[k];
                        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                    end
                end;

          // Apply Rise/Fall volt-var
                if (RateofChangeMode = RISEFALL) and (ActiveCircuit.Solution.DynaVars.dblHour > 0.0) then
                begin
                    FROCEvaluated[k] := TRUE;
                    Qtemp := CalcRF(k, 'VARS', PVSys);
                    if (Qtemp <> -999.99) then
                    begin
                        if abs(Qtemp) > abs(PVSys.kvarLimit) then
                            Qnew[k] := sign(Qtemp) * 1.0 * PVSys.kvarLimit
                        else
                            Qnew[k] := Qtemp;
                        PVSys.Presentkvar := Qnew[k];
                        Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                                Format('VOLTVAR mode, RISEFALL set PVSystem output var level to**, kvar= %.5g',
                                [Qnew[k]]));
                        FAvgpVuPrior[k] := FPresentVpu[k];
                        QOld[k] := QNew[k];
                        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                    end;
                end;
            end;
            Set_PendingChange(NONE, k);
        end;

        if (ControlMode = DRC) and (CombiMode = NONE_COMBMODE) and (PendingChange[k] = CHANGEVARLEVEL) then
        begin
            PVSys.VWmode := FALSE;
            PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1

            PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar
            CalcDRC_vars(k);
            QTemp := QDRCNew[k];
            QTempOrig := QDRCNew[k];

            if abs(QDRCNew[k]) > abs(PVSys.kvarLimit) then
            begin
                QTemp := sign(QDRCNew[k]) * 0.99 * PVSys.kvarLimit;
                QDesiredpu[k] := (Qtemp / QTemporig) * QDesiredpu[k];
                QDRCDesiredpu[k] := (Qtemp / QTemporig) * QDRCDesiredpu[k];
                FHitkvarLimit[k] := TRUE;
            end;

            PVSys.SetNominalPVSystemOuput;
            PTemp := PVSys.PresentkW;
              // if the desired kW and desired kvar exceed the kva rating of the PVSystem's inverter then...
            if SQRT(Sqr(Qtemp) + Sqr(PTemp)) > PVSys.kVARating then
            begin
                  //...if watts have precedence, reduce the reactive power to not exceed the kva rating
                if (FVV_ReacPower_ref = VARAVAL_WATTS) or (FVV_ReacPower_ref = VARMAX_WATTS) then
                begin
                    Qtemp := 0.99 * sign(Qtemp) * SQRT(Sqr(PVSys.kVARating) - Sqr(PTemp));
                    QDesiredpu[k] := (Qtemp / QTemporig) * QDesiredpu[k];
                    QDRCDesiredpu[k] := (Qtemp / QTemporig) * QDRCDesiredpu[k];
                end

                  //...else, vars have precedence, reduce the active power to not exceed the kva rating
                else
                begin
                    PTemp := 0.99 * sign(PTemp) * SQRT(Sqr(PVSys.kVARating) - Sqr(Qtemp));
                      // Set the active power
                    FFinalpuPmpp[k] := PTemp / PVSys.Pmpp;
                    PVSys.VWmode := TRUE;
                    PVSys.VWYAxis := FVoltwattYAxis;
                    PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
                    if (FFlagROCOnly[k] = FALSE) then
                    begin
                        if (RateofChangeMode = INACTIVE) or (ActiveCircuit.Solution.Dynavars.dblHour = 0.0) then
                        begin
                            PVSys.puPmpp := FFinalpuPmpp[k];
                            PNew[k] := FFinalpuPmpp[k];

                            if ShowEventLog then
                                AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                                    Format('**DRC VARMAX_VARS mode set PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp, FPriorWattspu[k]]));

                            ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                            FAvgpVuPrior[k] := FPresentVpu[k];
                            POld[k] := PVSys.puPmpp;
                        end;
                    end;
                end;
                FHitkvaLimit[k] := TRUE;
            end;


            if abs(QTemp) > abs(PVSys.kvarLimit) then
                QTemp := sign(QTemp) * 1.0 * PVSys.kvarLimit;
            PVSys.Presentkvar := QTemp;


            Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
            if ShowEventLog then
                AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                    Format('DRC mode set PVSystem output var level to**, kvar= %.5g',
                    [QTemp]));

            QoutputDRCpu[k] := PVSys.Presentkvar / QHeadroom[k];


            QoldDRC[k] := QTemp;

            FAvgpVuPrior[k] := FPresentVpu[k];
            QOld[k] := QDRCNew[k];
            ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;

            // Apply LPF
            if (RateofChangeMode = LPF) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
            begin
                FROCEvaluated[k] := TRUE;
                Qtemp := CalcLPF(k, 'VARS', PVSys);
                if (Qtemp <> -999.99) then
                begin
                    if abs(Qtemp) > abs(PVSys.kvarLimit) then
                        Qnew[k] := sign(Qtemp) * 1.0 * PVSys.kvarLimit
                    else
                        Qnew[k] := Qtemp;
                    PVSys.Presentkvar := Qnew[k];
                    Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                            Format('DYNAMICREACTIVECURRENT mode, LPF set PVSystem output var level to**, kvar= %.5g',
                            [Qnew[k]]));
                    FAvgpVuPrior[k] := FPresentVpu[k];
                    QOld[k] := QNew[k];
                    ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                end
            end;

            // Apply Rise/Fall
            if (RateofChangeMode = RISEFALL) and (ActiveCircuit.Solution.DynaVars.dblHour > 0.0) then
            begin
                FROCEvaluated[k] := TRUE;
                Qtemp := CalcRF(k, 'VARS', PVSys);
                if (Qtemp <> -999.99) then
                begin
                    if abs(Qtemp) > abs(PVSys.kvarLimit) then
                        Qnew[k] := sign(Qtemp) * 1.0 * PVSys.kvarLimit
                    else
                        Qnew[k] := Qtemp;
                    PVSys.Presentkvar := Qnew[k];
                    Qoutputpu[k] := PVSys.Presentkvar / QHeadroom[k];
                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                            Format('DYNAMICREACTIVECURRENT mode, RISEFALL set PVSystem output var level to**, kvar= %.5g',
                            [Qnew[k], FPresentVpu[k]]));
                    FAvgpVuPrior[k] := FPresentVpu[k];
                    QOld[k] := QNew[k];
                    ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                end;
            end;
            Set_PendingChange(NONE, k);
        end;


        if (ControlMode = VOLTWATT) and (CombiMode = NONE_COMBMODE) and (PendingChange[k] = CHANGEWATTLEVEL) then
        begin
            PVSys.VWmode := TRUE;
            PVSys.VWYAxis := FVoltwattYAxis;
            PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
            if (FFlagROCOnly[k] = FALSE) then
            begin
                CalcVoltWatt_pu(k);


                if (RateofChangeMode = INACTIVE) or (ActiveCircuit.Solution.Dynavars.dblHour = 0.0) then
                begin
                    PVSys.puPmpp := FFinalpuPmpp[k];
                    PNew[k] := FFinalpuPmpp[k];

                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                            Format('**VOLTWATT mode set PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp, FPriorWattspu[k]]));

                    ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                    FAvgpVuPrior[k] := FPresentVpu[k];
                    POld[k] := PVSys.puPmpp;
//                  Set_PendingChange(NONE,k);

                end;
            end;


          // rate of change LPF
            if (RateofChangeMode = LPF) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
            begin
                FROCEvaluated[k] := TRUE;
                Ptemp := CalcLPF(k, 'WATTS', PVSys);
                if (Ptemp <> -999.99) then
                begin
                    if PTemp <> 0.0 then
                    begin
                        PNew[k] := PTemp;
                        PVSys.puPmpp := PNew[k];

                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                                Format('**VOLTWATT mode, LPF limited PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp, FPriorWattspu[k]]));

                        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                        FAvgpVuPrior[k] := FPresentVpu[k];
                        POld[k] := PVSys.puPmpp;
                    end;
                end;
              //  Set_PendingChange(NONE,k);

            end;


          // rate of change rise/fall limit
            if (RateofChangeMode = RISEFALL) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
            begin
                FROCEvaluated[k] := TRUE;
                PTemp := CalcRF(k, 'WATTS', PVSys);
                if (Ptemp <> -999.99) then
                begin
                    PNew[k] := PTemp;
                    PVSys.puPmpp := PNew[k];
                    if ShowEventLog then
                        AppendtoEventLog('InvControl.' + Self.Name + ',' + PVSys.Name + ',',
                            Format('**VOLTWATT mode, RISEFALL limited PVSystem output level to**, puPmpp= %.5g, PriorWatts= %.5g', [PVSys.puPmpp, FPriorWattspu[k]]));

                    ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
                    FAvgpVuPrior[k] := FPresentVpu[k];
                    POld[k] := PVSys.puPmpp;
                end;
                    // Force recalc of power parms
      //              Set_PendingChange(NONE,k);
            end
        end;
        Set_PendingChange(NONE, k);
        PVSys := NIL;
    end;

        {Do Nothing}
end;

procedure TInvControlObj.Sample;

var
    i, j: Integer;
    basekV,
    Vpresent: Double;

begin
     // If list is not defined, go make one from all PVSystem in circuit
    if FPVSystemPointerList.Count = 0 then
        RecalcElementData;

    if (FListSize > 0) then
    begin
         // If an InvControl controls more than one PV, control each one
         // separately based on the PVSystem's terminal voltages, etc.
        for i := 1 to FPVSystemPointerList.Count do
        begin
            if (ActiveCircuit.Solution.DynaVars.t = 1) and (ActiveCircuit.Solution.ControlIteration = 1) then
                FWithinTol[i] := FALSE;
            FWithinTolVV[i] := FALSE;
            FWithinTolVW[i] := FALSE;
            ControlledElement[i].ComputeVTerminal;
            for j := 1 to ControlledElement[i].Yorder do
                cBuffer[i, j] := ControlledElement[i].Vterminal^[j];

            BasekV := ActiveCircuit.Buses^[ControlledElement[i].terminals[0].busRef].kVBase;

            Vpresent := 0;

            // Calculate the present average voltage  magnitude
            for j := 1 to ControlledElement[i].NPhases do
                Vpresent := Vpresent + Cabs(cBuffer[i, j]);

            // convert to per-unit on bus' kvbase, or
            // if using averaging window values, then set prior voltage to averaging window
            if (FVoltage_CurveX_ref = 1) and (FRollAvgWindow[i].AvgVal <> 0.0) then
                FPresentVpu[i] := (Vpresent / ControlledElement[i].NPhases) / (FRollAvgWindow[i].AvgVal)
            else
            if (FVoltage_CurveX_ref = 2) and (FRollAvgWindow[i].AvgVal <> 0.0) then
                FPresentVpu[i] := (FRollAvgWindow[i].AvgVal) / (basekV * 1000.0)
            else
                FPresentVpu[i] := (Vpresent / ControlledElement[i].NPhases) / (basekV * 1000.0);


            if CombiMode = VV_DRC then
            begin
                if ((FHitkVALimit[i] = TRUE) or (FHitkvarLimit[i] = TRUE)) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
                    exit;
                  // if inverter is off then exit
                if (ControlledElement[i].InverterON = FALSE) and (ControlledElement[i].VarFollowInverter = TRUE) then
                    continue;
                  // if the volt-var curve does not exist, exit
                if Fvvc_curve = NIL then
                begin
                    DoSimpleMsg(_('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.'), 382);
                    exit
                end;

                  //DRC triggers
                if (priorDRCRollAvgWindow[i] = 0.0) then
                begin
                    if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance)) then
                    begin
                        Set_PendingChange(CHANGEDRCVVARLEVEL, i);


                        with ActiveCircuit.Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ' ' + ControlledElement[i].Name, Format
                                ('**Ready to change var output due to DRC trigger in VV_DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i], FAvgpVuPrior[i]]));
                    end;

                end;

                    //Trigger from volt-var mode
                if (FRocEvaluated[i] = FALSE) and (FWithinTolVV[i] = FALSE) then
                begin
                    if (((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance) or
                        ((Abs(Abs(QoutputVVpu[i]) - Abs(Qdesiredpu[i])) > FVarChangeTolerance))) or
                        (ActiveCircuit.Solution.ControlIteration = 1)) then
                    begin
                        FWithinTolVV[i] := FALSE;

                        Set_PendingChange(CHANGEDRCVVARLEVEL, i);
                        with  ActiveCircuit.Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self);

                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ' ' + ControlledElement[i].Name, Format
                                ('**Ready to change VV_DRC output due to volt-var trigger in VV_DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g',

                                [FPresentVpu[i], FAvgpVuPrior[i]]));

                    end
                    else
                    begin
                        if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) <= FVoltageChangeTolerance) and
                            ((Abs(Abs(QoutputVVpu[i]) - Abs(Qdesiredpu[i])) <= FVarChangeTolerance))) then
                            FWithinTolVV[i] := TRUE;
//                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
  //                          ('**Hit Tolerance with volt-var**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));

                    end;
                end;

                    //Trigger for ROC
                if (RateofChangeMode <> INACTIVE) and (ActiveCircuit.Solution.DynaVars.dblHour > 0.0) then
                begin
                    if (FWithinTolVV[i] = TRUE) and (FRocEvaluated[i] = FALSE) then
                    begin
                        FFlagROCOnly[i] := TRUE;
                        Set_PendingChange(CHANGEDRCVVARLEVEL, i);

                        with  ActiveCircuit.Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self);
//                            If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
//                                ('**Ready to change VV_DRC output due to ROC trigger (ROC)**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                    end;
                end;

            end;

            // comment
            if CombiMode = VV_VW then
            begin
                if ((FHitkVALimit[i] = TRUE) or (FHitkvarLimit[i] = TRUE)) and (ActiveCircuit.Solution.Dynavars.dblHour > 0.0) then
                    exit;
//                  if ((FHitkVALimit[i] = True) or (FHitkvarLimit[i] = True)) and (ActiveCircuit.Solution.Dynavars.dblHour=0.0) and ((ActiveCircuit.Solution.ControlIteration) >= (0.5*ActiveCircuit.Solution.MaxControlIterations)) then exit;
                  // if inverter is off then exit
//                  if (ControlledElement[i].InverterON = FALSE) then exit;
                if (ControlledElement[i].InverterON = FALSE) and (ControlledElement[i].VarFollowInverter = TRUE) then
                    continue;

                  // if volt-watt curve does not exist, exit
                if Fvoltwatt_curve = NIL then
                begin
                    DoSimpleMsg(_('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.'), 381);
                    exit
                end;
                  // if inverter is off and varfollowinverter is true, then exit.
                if (ControlledElement[i].InverterON = FALSE) and (ControlledElement[i].VarFollowInverter = TRUE) then
                    continue;

                  // if the volt-var curve does not exist, exit
                if Fvvc_curve = NIL then
                begin
                    DoSimpleMsg(_('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.'), 382);
                    exit
                end;

                  // Trigger from volt-watt mode
                if (FRocEvaluated[i] = FALSE) and (FWithinTolVW[i] = FALSE) then
                begin
                    if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance) or (Abs(PNew[i] - POld[i]) > FActivePChangeTolerance) or
                        (ActiveCircuit.Solution.ControlIteration = 1)) and (FROCEvaluated[i] = FALSE) then
                    begin
                        FWithinTolVW[i] := FALSE;
                        FFlagROCOnly[i] := FALSE;
                        Set_PendingChange(CHANGEWATTVARLEVEL, i);

                        with  ActiveCircuit.Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ' ' + ControlledElement[i].Name, Format
                                ('**Ready to change VV_VW output due to volt-watt trigger**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i], FAvgpVuPrior[i]]));
                    end
                    else
                    begin
                        if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) <= FVoltageChangeTolerance) or
                            (Abs(PNew[i] - Pold[i]) <= FActivePChangeTolerance)) then
                            FWithinTolVW[i] := TRUE;
                        FFlagROCOnly[i] := FALSE;
                    end;
                end;
                    //Trigger from volt-var mode
                if (FRocEvaluated[i] = FALSE) and (FWithinTolVV[i] = FALSE) then
                begin
                    if (((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance) or
                        ((Abs(Abs(Qoutputpu[i]) - Abs(Qdesiredpu[i])) > FVarChangeTolerance))) or
                        (ActiveCircuit.Solution.ControlIteration = 1)) then
                    begin
                        FWithinTolVV[i] := FALSE;

                        Set_PendingChange(CHANGEWATTVARLEVEL, i);
                        with  ActiveCircuit.Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self);

                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ' ' + ControlledElement[i].Name, Format
                                ('**Ready to change VV_VW output due to volt-var trigger**, Vavgpu= %.5g, VPriorpu=%.5g',

                                [FPresentVpu[i], FAvgpVuPrior[i]]));

                    end
                    else
                    begin
                        if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) <= FVoltageChangeTolerance) and
                            ((Abs(Abs(Qoutputpu[i]) - Abs(Qdesiredpu[i])) <= FVarChangeTolerance))) then
                            FWithinTolVV[i] := TRUE;
//                          If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
//                            ('**Hit Tolerance with volt-var**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));

                    end;
                end;

                    //Trigger for ROC
                if (RateofChangeMode <> INACTIVE) and (ActiveCircuit.Solution.DynaVars.dblHour > 0.0) then
                begin
                    if (FWithinTolVV[i] = TRUE) and (FWithinTolVW[i] = TRUE) and (FRocEvaluated[i] = FALSE) then
                    begin
                        FFlagROCOnly[i] := TRUE;
                        Set_PendingChange(CHANGEWATTVARLEVEL, i);

                        with  ActiveCircuit.Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self);
//                            If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
//                                ('**Ready to change VV_VW output due to volt-watt trigger (ROC)**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                    end;
                end;

            end;


            if ControlMode = VOLTWATT then  // volt-watt control mode
            begin
                if (ControlledElement[i].InverterON = FALSE) then
                    continue;

                if Fvoltwatt_curve = NIL then
                begin
                    DoSimpleMsg(_('XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.'), 381);
                    exit
                end;


                ControlledElement[i].VWmode := TRUE;
                if (FRocEvaluated[i] = FALSE) and (FWithinTolVW[i] = FALSE) then
                begin
                    if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance) or (Abs(PNew[i] - POld[i]) > FActivePChangeTolerance) or
                        (ActiveCircuit.Solution.ControlIteration = 1)) and (FROCEvaluated[i] = FALSE) then


                    begin
                        FWithinTolVW[i] := FALSE;
                        FFlagROCOnly[i] := FALSE;
                        Set_PendingChange(CHANGEWATTLEVEL, i);

                        with  ActiveCircuit.Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ' ' + ControlledElement[i].Name, Format
                                ('**Ready to change watt output due in VOLTWATT mode**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i], FAvgpVuPrior[i]]));
                    end

                    else
                    begin
                        if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) <= FVoltageChangeTolerance) or
                            (Abs(PNew[i] - Pold[i]) <= FActivePChangeTolerance)) then
                            FWithinTolVW[i] := TRUE;
                        FFlagROCOnly[i] := FALSE;
                    end;
                end;
                if (RateofChangeMode <> INACTIVE) and (ActiveCircuit.Solution.DynaVars.dblHour > 0.0) then
                begin
                    if (FWithinTol[i] = TRUE) and (FRocEvaluated[i] = FALSE) then
                    begin
                        FFlagROCOnly[i] := TRUE;
                        Set_PendingChange(CHANGEWATTLEVEL, i);

                        with  ActiveCircuit.Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self);
//                            If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
//                                ('**Ready to change watt output in VOLTWATT mode (ROC)**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                    end
                    else
                    begin
                    end;
                end;
            end;


            if ControlMode = VOLTVAR then // volt-var control mode
            begin
                if (ControlledElement[i].InverterON = FALSE) and (ControlledElement[i].VarFollowInverter = TRUE) then
                    continue;
                ControlledElement[i].VWmode := FALSE;
                if Fvvc_curve = NIL then
                begin
                    DoSimpleMsg(_('XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.'), 382);
                    exit
                end;

                    //Trigger from volt-var mode
                if (FRocEvaluated[i] = FALSE) and (FWithinTolVV[i] = FALSE) then
                begin
                    if (((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance) or
                        ((Abs(Abs(QoutputVVpu[i]) - Abs(Qdesiredpu[i])) > FVarChangeTolerance))) or
                        (ActiveCircuit.Solution.ControlIteration = 1)) then
                    begin
                        FWithinTolVV[i] := FALSE;

                        Set_PendingChange(CHANGEVARLEVEL, i);
                        with  ActiveCircuit.Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self);

                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ' ' + ControlledElement[i].Name, Format
                                ('**Ready to change var output due to volt-var trigger in volt-var mode**, Vavgpu= %.5g, VPriorpu=%.5g',

                                [FPresentVpu[i], FAvgpVuPrior[i]]));

                    end
                    else
                    begin
                        if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) <= FVoltageChangeTolerance) and
                            ((Abs(Abs(QoutputVVpu[i]) - Abs(Qdesiredpu[i])) <= FVarChangeTolerance))) then
                            FWithinTolVV[i] := TRUE;
  //                        If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
//                            ('**Hit Tolerance with volt-var**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));

                    end;
                end;

                    //Trigger for ROC
                if (RateofChangeMode <> INACTIVE) and (ActiveCircuit.Solution.DynaVars.dblHour > 0.0) then
                begin
                    if (FWithinTolVV[i] = TRUE) and (FRocEvaluated[i] = FALSE) then
                    begin
                        FFlagROCOnly[i] := TRUE;
                        Set_PendingChange(CHANGEVARLEVEL, i);

                        with  ActiveCircuit.Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self);
//                            If ShowEventLog Then AppendtoEventLog('InvControl.' + Self.Name+' '+ControlledElement[i].Name, Format
//                                ('**Ready to change var output due to ROC trigger (ROC) in volt-var mode**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i],FAvgpVuPrior[i]]));
                    end;
                end;

            end;

            if ControlMode = DRC then // dynamic reactive current control mode
            begin
                if (ControlledElement[i].InverterON = FALSE) and (ControlledElement[i].VarFollowInverter = TRUE) then
                    continue;
                ControlledElement[i].VWmode := FALSE;
                  //DRC triggers
                if (priorDRCRollAvgWindow[i] = 0.0) then
                begin
                    if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance)) then
                    begin
                        Set_PendingChange(CHANGEVARLEVEL, i);


                        with ActiveCircuit.Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self);
                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ' ' + ControlledElement[i].Name, Format
                                ('**Ready to change var output due in DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i], FAvgpVuPrior[i]]));
                    end;

                end;

                if (FRocEvaluated[i] = FALSE) and (FWithinTol[i] = FALSE) then
                begin
                    if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) > FVoltageChangeTolerance) or
//                      (Abs(Abs(QoutputDRCpu[i]) - Abs(Qdesiredpu[i])) > FVarChangeTolerance) or // TEMc; also tried checking against QDRCdesiredpu
                        (ActiveCircuit.Solution.ControlIteration = 1)) then
                    begin
                        FWithinTol[i] := FALSE;

                        Set_PendingChange(CHANGEVARLEVEL, i);
                        with  ActiveCircuit.Solution.DynaVars do
                            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                                (intHour, t + TimeDelay, PendingChange[i], 0, Self);

                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ' ' + ControlledElement[i].Name, Format
                                ('**Ready to change DRC output because V or Q out of tolerance**, Vavgpu= %.5g, VPriorpu=%.5g, QoutPU=%.3g, QdesiredPU=%.3g, QDRCdesiredPU=%.3g',
                                [FPresentVpu[i], FAvgpVuPrior[i], QoutputDRCpu[i], Qdesiredpu[i], QDRCdesiredpu[i]]));

                    end
                    else
                    begin
                        if ((Abs(FPresentVpu[i] - FAvgpVuPrior[i]) <= FVoltageChangeTolerance) and
                            ((Abs(Abs(QoutputDRCpu[i]) - Abs(Qdesiredpu[i])) <= FVarChangeTolerance))) then
                            FWithinTol[i] := TRUE;
                        if ShowEventLog then
                            AppendtoEventLog('InvControl.' + Self.Name + ' ' + ControlledElement[i].Name, Format
                                ('**Hit Tolerance with DRCvar**, Vavgpu= %.5g, VPriorpu=%.5g', [FPresentVpu[i], FAvgpVuPrior[i]]));

                    end;
                end;
            end;
        end;
    end;
end;

function TInvControlObj.MakePVSystemList: Boolean;

var
    PVSysClass: TDSSClass;
    PVSys: TPVsystemObj;
    i, j: Integer;

begin
    Result := FALSE;
    PVSysClass := GetDSSClassPtr(DSS, 'PVsystem');

    if FListSize > 0 then
    begin    // Name list is defined - Use it

        SetLength(CondOffset, FListSize + 1);
        SetLength(cBuffer, FListSize + 1, 7);  // assuming no more than 6 conductors


        SetLength(ControlledElement, FListSize + 1);  // Use this as the main pointer to PVSystem Elements

        SetLength(FkWLimit, FListSize + 1);
        SetLength(FkVALimit, FListSize + 1);
        SetLength(FkvarLimit, FListSize + 1);
        SetLength(FVref, FListSize + 1);
        SetLength(FPpf, FListSize + 1);
        SetLength(Fpresentkvar, FListSize + 1);
        SetLength(FpresentkW, FListSize + 1);
        SetLength(FAvgpVuPrior, FListSize + 1);
        SetLength(FPresentVpu, FListSize + 1);

        SetLength(NPhasesPVSys, FListSize + 1);
        SetLength(NCondsPVSys, FListSize + 1);

        SetLength(FPendingChange, FListSize + 1);
        SetLength(FFlagROCOnly, FListSize + 1);
        SetLength(QDeliver, FListSize + 1);
        SetLength(QNew, FListSize + 1);
        SetLength(QOld, FListSize + 1);
        SetLength(QOldVV, FListSize + 1);
        SetLength(QOldDRC, FListSize + 1);
        SetLength(QDRCNew, FListSize + 1);
        SetLength(QHeadroom, FListSize + 1);
        SetLength(Qoutputpu, FListSize + 1);
        SetLength(QoutputVVpu, FListSize + 1);
        SetLength(QoutputDRCpu, FListSize + 1);
        SetLength(Qdesiredpu, FListSize + 1);
        SetLength(QDRCdesiredpu, FListSize + 1);
        SetLength(deltaVDynReac, FListSize + 1);
        SetLength(PNew, FListSize + 1);
        SetLength(POld, FListSize + 1);

        SetLength(FVpuSolution, FListSize + 1, 2 + 1);
        SetLength(FRollAvgWindow, FListSize + 1);
        SetLength(FDRCRollAvgWindow, FListSize + 1);

        SetLength(priorRollAvgWindow, FListSize + 1);
        SetLength(priorDRCRollAvgWindow, FListSize + 1);
        SetLength(FlagChangeCurve, FListSize + 1);
        SetLength(FActiveVVCurve, FListSize + 1);
        SetLength(FPriorWattspu, FListSize + 1);
        SetLength(FPriorvarspu, FListSize + 1);
        SetLength(FLPFTime, FListSize + 1);
        SetLength(FWithinTol, FListSize + 1);
        SetLength(FWithinTolVV, FListSize + 1);
        SetLength(FWithinTolVW, FListSize + 1);
        SetLength(FROCEvaluated, FListSize + 1);
        SetLength(FHitkVALimit, FListSize + 1);
        SetLength(FHitkvarLimit, FListSize + 1);


        SetLength(FFinalpuPmpp, FListSize + 1);
        SetLength(FFinalkvar, FListSize + 1);


        for i := 1 to FListSize do
        begin
            PVSys := PVSysClass.Find(FPVSystemNameList.Strings[i - 1]);
            if Assigned(PVSys) and PVSys.Enabled then
                FPVSystemPointerList.Add(PVSys);
        end;

    end
    else
    begin
     {Search through the entire circuit for enabled pvsysten objects and add them to the list}

        for i := 1 to PVSysClass.ElementCount do
        begin
            PVSys := PVSysClass.ElementList.Get(i);
            if PVSys.Enabled then
                FPVSystemPointerList.Add(PVSys);
            FPVSystemNameList.Add(PVSys.Name);
        end;


        FListSize := FPVSystemPointerList.Count;

        SetLength(ControlledElement, FListSize + 1);

        SetLength(FkWLimit, FListSize + 1);
        SetLength(FkVALimit, FListSize + 1);
        SetLength(FkvarLimit, FListSize + 1);
        SetLength(FVref, FListSize + 1);
        SetLength(FPpf, FListSize + 1);
        SetLength(Fpresentkvar, FListSize + 1);
        SetLength(FpresentkW, FListSize + 1);
        SetLength(FAvgpVuPrior, FListSize + 1);
        SetLength(FPresentVpu, FListSize + 1);

        SetLength(NPhasesPVSys, FListSize + 1);
        SetLength(NCondsPVSys, FListSize + 1);
        SetLength(CondOffset, FListSize + 1);
        SetLength(cBuffer, FListSize + 1, 7);  // assuming no more than 6 conductors
        SetLength(FPendingChange, FListSize + 1);
        SetLength(FFlagROCOnly, FListSize + 1);

        SetLength(QDeliver, FListSize + 1);
        SetLength(QNew, FListSize + 1);
        SetLength(QOld, FListSize + 1);
        SetLength(QOldVV, FListSize + 1);
        SetLength(QOldDRC, FListSize + 1);
        SetLength(QDRCNew, FListSize + 1);
        SetLength(QHeadroom, FListSize + 1);
        SetLength(Qoutputpu, FListSize + 1);
        SetLength(QoutputVVpu, FListSize + 1);
        SetLength(QoutputDRCpu, FListSize + 1);
        SetLength(Qdesiredpu, FListSize + 1);
        SetLength(QDRCdesiredpu, FListSize + 1);
        SetLength(PNew, FListSize + 1);
        SetLength(POld, FListSize + 1);

        SetLength(FRollAvgWindow, FListSize + 1);
        SetLength(FDRCRollAvgWindow, FListSize + 1);

        SetLength(deltaVDynReac, FListSize + 1);
        SetLength(priorRollAvgWindow, FListSize + 1);
        SetLength(priorDRCRollAvgWindow, FListSize + 1);
        SetLength(FVpuSolution, FListSize + 1, 2 + 1);
        SetLength(FlagChangeCurve, FListSize + 1);
        SetLength(FActiveVVCurve, FListSize + 1);
        SetLength(FPriorWattspu, FListSize + 1);
        SetLength(FPriorvarspu, FListSize + 1);
        SetLength(FLPFTime, FListSize + 1);
        SetLength(FWithinTol, FListSize + 1);
        SetLength(FWithinTolVV, FListSize + 1);
        SetLength(FWithinTolVW, FListSize + 1);
        SetLength(FROCEvaluated, FListSize + 1);
        SetLength(FHitkVALimit, FListSize + 1);
        SetLength(FHitkvarLimit, FListSize + 1);


        SetLength(FFinalpuPmpp, FListSize + 1);
        SetLength(FFinalkvar, FListSize + 1);

    end;  {Else}


     //Initialize arrays

    for i := 1 to FlistSize do
    begin
        PVSys := PVSysClass.Find(FPVSystemNameList.Strings[i - 1]);

        for j := 1 to 6 do
            cBuffer[i, j] := cZERO;

        Set_NTerms(PVSys.NTerms);


        FkWLimit[i] := 0.0;
        FkVALimit[i] := 0.0;
        FkvarLimit[i] := 0.0;
        FVref[i] := 0.0;
        FPpf[i] := 0.0;
        Fpresentkvar[i] := 0.0;
        FpresentkW[i] := 0.0;
        CondOffset[i] := 0;
        NPhasesPVSys[i] := PVSys.NPhases;
        NCondsPVSys[i] := PVSys.NConds;
        FAvgpVuPrior[i] := 0.0;
        FPresentVpu[i] := 0.0;
        QDeliver[i] := 0.0;
        QNew[i] := 0.0;
        QOld[i] := -1.0;
        QOldVV[i] := -1.0;
        QOldDRC[i] := -1.0;
        QDRCNew[i] := 0.0;
        PNew[i] := 0.0;
        POld[i] := 0.0;
        QHeadroom[i] := 0.0;
        Qoutputpu[i] := 0.0;
        QoutputVVpu[i] := 0.0;
        QoutputDRCpu[i] := 0.0;
        Qdesiredpu[i] := 0.0;
        QDRCdesiredpu[i] := 0.0;
        FRollAvgWindow[i] := TRollAvgWindow.Create;
        FDRCRollAvgWindow[i] := TRollAvgWindow.Create;
        deltaVDynReac[i] := 0.0;
        FlagChangeCurve[i] := FALSE;
        FActiveVVCurve[i] := 1;
        priorRollAvgWindow[i] := 0.0;
        priorDRCRollAvgWindow[i] := 0.0;
        FPriorWattspu[i] := 0.0;
        FPriorvarspu[i] := 0.0;
        FLPFTime[i] := 0.0;
        FWithinTol[i] := FALSE;
        FWithinTolVV[i] := FALSE;
        FWithinTolVW[i] := FALSE;
        FROCEvaluated[i] := FALSE;
        FHitkVALimit[i] := FALSE;
        FHitkvarLimit[i] := FALSE;

        for j := 1 to 2 do
            FVpuSolution[i, j] := 0.0;


        FFinalpuPmpp[i] := 0.0;
        FFinalkvar[i] := 0.0;

        FPendingChange[i] := NONE;
        FFlagROCOnly[i] := FALSE;
    end; {For}

    RecalcElementData;
    if FPVSystemPointerList.Count > 0 then
        Result := TRUE;
end;


procedure TInvControlObj.Reset;
begin
  // inherited;
end;

function TInvControlObj.ReturnElementsList: Ansistring;
var
    i: Integer;
begin
    if FListSize = 0 then
    begin
        Result := '';
        Exit;
    end;

    Result := '[' + FPVSystemNameList.Strings[0];
    for i := 1 to FListSize - 1 do
    begin
        Result := Result + ', ' + FPVSystemNameList.Strings[i];
    end;
    Result := Result + ']';  // terminate the array

end;

//procedure TInvControlObj.Set_Enabled(Value: Boolean);
//begin
//    inherited;
//    // Reset controlled PVSystems to original PF
//end;

procedure TInvControlObj.Set_PendingChange(Value: Integer; DevIndex: Integer);
begin
    FPendingChange[DevIndex] := Value;
    DblTraceParameter := Value;
end;

procedure TInvControlObj.UpdateInvControl(i: Integer);
var
    j, k: Integer;
    solnvoltage: Double;
    localControlledElement: TDSSCktElement;
    tempVbuffer: pComplexArray;
    PVSys: TPVSystemObj;


begin
    tempVbuffer := NIL;   // Initialize for Reallocmem

    for j := 1 to FPVSystemPointerList.Count do
    begin
             // only update solution idx one time through this routine
        if (j = 1) and (i = 1) then
        begin
                 //update solution voltage in per-unit for hysteresis
            if FVpuSolutionIdx = 2 then
                FVpuSolutionIdx := 1
            else
                FVpuSolutionIdx := FVpuSolutionIdx + 1;

        end;

        localControlledElement := ControlledElement[j];
        PVSys := localControlledElement as TPVSystemObj;
        FPriorWattspu[j] := PVSys.PresentkW / PVSys.PVSystemVars.FPmpp;
        FPriorvarspu[j] := PVSys.Presentkvar / SQRT(Sqr(PVSys.kVARating) - Sqr(PVSys.PresentkW));
        PVSys.PVSystemVars.FpuPmpp := 1.0;
        FWithinTol[j] := FALSE;
        FWithinTolVV[j] := FALSE;
        FWithinTolVW[j] := FALSE;
        FROCEvaluated[j] := FALSE;

        FHitkVALimit[j] := FALSE;
        FHitkvarLimit[j] := FALSE;

        FFlagROCOnly[j] := FALSE;

             // allocated enough memory to buffer to hold voltages and initialize to cZERO
        Reallocmem(tempVbuffer, Sizeof(tempVbuffer^[1]) * localControlledElement.NConds);
        for k := 1 to localControlledElement.NConds do
            tempVbuffer[k] := cZERO;

        priorRollAvgWindow[j] := FRollAvgWindow[j].AvgVal;
        priorDRCRollAvgWindow[j] := FDRCRollAvgWindow[j].AvgVal;
             // compute the present terminal voltage
        localControlledElement.ComputeVterminal;
             // save the applicable rolling average voltage in monitor
        if (ControlMode = VOLTVAR) and (FRollAvgWindowLength > 0.0) then
            PVSys.Set_Variable(5, FRollAvgWindow[j].AvgVal)
        else
            PVSys.Set_Variable(5, FDRCRollAvgWindow[j].AvgVal);

        for k := 1 to localControlledElement.Yorder do
            tempVbuffer[k] := localControlledElement.Vterminal^[k];

        solnvoltage := 0.0;
        for k := 1 to localControlledElement.Nphases do
            solnvoltage := solnvoltage + Cabs(tempVbuffer[k]);
        solnvoltage := solnvoltage / (localControlledElement.Nphases * 1.0); // average of voltages if more than one phase

             // add present power flow solution voltage to the rolling average window
        FRollAvgWindow[j].Add(solnvoltage, ActiveCircuit.Solution.DynaVars.h, FRollAvgWindowLength);
        FDRCRollAvgWindow[j].Add(solnvoltage, ActiveCircuit.Solution.DynaVars.h, FDRCRollAvgWindowLength);

        FVpuSolution[j, FVpuSolutionIdx] := solnvoltage / ((ActiveCircuit.Buses^[localcontrolledelement.terminals[0].busRef].kVBase) * 1000.0);

        Reallocmem(tempVbuffer, 0);   // Clean up memory

    end;
end;

function TInvControlObj.Get_PendingChange(DevIndex: Integer): Integer;
begin
    Result := FPendingChange[DevIndex];
end;


procedure TInvControlObj.CalcVoltWatt_pu(j: Integer);
var
    Pdesiredpu: Double;
    DeltaP: Double;

 // local pointer to current PVSystem element
    PVSys: TPVSystemObj;


begin
    PVSys := ControlledElement[j];   // Use local variable in loop


    PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
    PVSys.VWmode := TRUE;
    PVSys.VWYAxis := FVoltwattYAxis;

      // P desired pu is the desired output based on the avg pu voltage on the
      // monitored element
    Pdesiredpu := Fvoltwatt_curve.GetYValue(FPresentVpu[j]);      //Y value = watts in per-unit of Pmpp


    if (FROCEvaluated[j] = FALSE) then
    begin
        DeltaP := Pdesiredpu - POld[j];
        PNew[j] := POld[j] + DeltaP * FdeltaP_factor;
        FFinalpuPmpp[j] := PNew[j];
    end
    else
        FFinalpuPmpp[j] := PVSys.puPmpp;
    PVSys := NIL;
end;


procedure TInvControlObj.CalcDRC_vars(j: Integer);
var

    DeltaQ, basekV,
    QTemp{,TempQ}: Double;
  // SMonitoredElement                         :Complex;


 // local pointer to current PVSystem element
    PVSys: TPVSystemObj;

begin
    PVSys := ControlledElement[j];   // Use local variable in loop


      // SMonitoredElement := PVSys.Power[1]; // s is in va
    PVSys.VWmode := FALSE;
    PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
    PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar

    QDRCDesiredpu[j] := 0.0;

      // calculate headroom from kva rating of PVSystem and presentkW output level
    if FVV_ReacPower_ref = VARAVAL_WATTS then
        QHeadRoom[j] := SQRT(Sqr(PVSys.kVARating) - Sqr(PVSys.PresentkW));
    if (FVV_ReacPower_ref = VARMAX_VARS) or (FVV_ReacPower_ref = VARMAX_WATTS) then
        QHeadRoom[j] := PVSys.kvarLimit;


    basekV := ActiveCircuit.Buses^[PVSys.terminals[0].busRef].kVBase;

      // calculate deltaV quantity in per-unit from subtracting the rolling average
      // value (in p.u.) from the present p.u. terminal voltage (average of line-ground)
      // if more than one phase
    if (FDRCRollAvgWindow[j].AvgVal / (basekV * 1000.0)) = 0.0 then
        deltaVDynReac[j] := 0
    else
        deltaVDynReac[j] := FPresentVpu[j] - FDRCRollAvgWindow[j].AvgVal / (basekV * 1000.0);

      // if below the lower deadband and deltaV quantity is non-zero then
      // calculate desired pu var output. In per-unit of kva rating (also
      // ampere rating), per report specifications.
    if (deltaVDynReac[j] <> 0) and (FPresentVpu[j] < FDbVMin) then
        QDRCDesiredpu[j] := -deltaVDynReac[j] * FArGraLowV

      // if above the upper deadband and deltaV quantity is non-zero then
      // calculate desired pu var output. In per-unit of kva rating (also
      // ampere rating), per report specifications.

    else
    if (deltaVDynReac[j] <> 0) and (FPresentVpu[j] > FDbVMax) then
        QDRCDesiredpu[j] := -deltaVDynReac[j] * FArGraHiV

    else
    if deltaVDynReac[j] = 0.0 then
        QDRCDesiredpu[j] := 0.0;

    if (ActiveCircuit.Solution.Dynavars.t = 1) then
        QDRCDesiredpu[j] := 0.0;

      // as with volt-var mode, we don't want to jump directly to solution
      // or we'll have oscillatory behavior
    QTemp := 0;
    if FVV_ReacPower_ref = VARAVAL_WATTS then
        QTemp := QDRCDesiredpu[j] * PVSys.kVARating;
    if (FVV_ReacPower_ref = VARMAX_VARS) or (FVV_ReacPower_ref = VARMAX_WATTS) then
        QTemp := QDRCDesiredpu[j] * PVSys.kvarLimit;

    if (Abs(QTemp) > QHeadroom[j]) then
    begin
        if FVV_ReacPower_ref = VARAVAL_WATTS then
            QDRCDesiredpu[j] := sign(QDRCDesiredpu[j]) * 1.0
        else
            QDRCDesiredpu[j] := sign(QDRCDesiredpu[j]) * 1.0;
    end;
    if FVV_ReacPower_ref = VARAVAL_WATTS then
        DeltaQ := QDRCDesiredpu[j] * PVSys.kVARating - QoldDRC[j]
    else
        DeltaQ := QDRCDesiredpu[j] * PVSys.kvarLimit - QoldDRC[j];
      // if FVV_ReacPower_ref = VARAVAL_WATTS then TempQ := QDRCDesiredpu[j]*PVSys.kVARating
      // else TempQ := QDRCDesiredpu[j]*PVSys.kvarLimit;
    if abs(DeltaQ) > PVSys.kvarLimit then
        DeltaQ := 1.0 * sign(DeltaQ) * PVSys.kvarLimit;

    QDRCNew[j] := QoldDRC[j] + (DeltaQ * FDeltaQ_factor);
end;

function TInvControlObj.CalcLPF(m: Integer; powertype: Ansistring; PVSys: TPVSystemObj): Double;
var
    Pdesiredpu: Double;
    DeltaQ, alpha, DeltaP,
    LPFvarspu, LPFwattspu: Double;

  // Applies the LPF:
  //  Return value is in kvar for VARS
  //  Return value is in puPmpp for WATTS

begin
    Result := -999.999;
  // calculate the alpha constant
    alpha := 1.0 / (ActiveCircuit.Solution.DynaVars.h) / (FLPFTau + 1.0 / ActiveCircuit.Solution.DynaVars.h);
    if powertype = 'VARS' then
    begin
        LPFvarspu := alpha * (Qoutputpu[m]) + (1 - alpha) * (FPriorvarspu[m]);
        if (LPFvarspu <> 0.0) then
        begin
            QDeliver[m] := LPFvarspu * QHeadRoom[m];
            DeltaQ := QDeliver[m] - Qold[m];
            Result := QOld[m] + DeltaQ * FdeltaQ_factor;
        end;

    end;
    if powertype = 'WATTS' then
    begin
        LPFWattspu := alpha * (FFinalpuPmpp[m]) + (1 - alpha) * (FPriorWattspu[m]);
        if (LPFWattspu <> 0.0) then
        begin
            Pdesiredpu := LPFWattspu;
            DeltaP := Pdesiredpu - POld[m];
            Result := POld[m] + DeltaP * FdeltaP_factor;
        end;
    end;
end;

function TInvControlObj.CalcRF(m: Integer; powertype: Ansistring; PVSys: TPVSystemObj): Double;
var
    Pdesiredpu: Double;
    DeltaP,
    Pdesiredpu_temp,
    Qdesiredpu_temp: Double;

begin
    Result := 0.0;
  // Applies the Rise/Fall limiting function:
  //  Return value is in kvar for VARS
  //  Return value is in puPmpp for WATTS
    if FVV_ReacPower_ref = VARAVAL_WATTS then
        QHeadRoom[m] := SQRT(Sqr(PVSys.kVARating) - Sqr(PVSys.PresentkW));
    if (FVV_ReacPower_ref = VARMAX_VARS) or (FVV_ReacPower_ref = VARMAX_WATTS) then
        QHeadRoom[m] := PVSys.kvarLimit;

    if powertype = 'VARS' then
    begin
        if (abs(PVSys.Presentkvar) < 0.00001) then
        begin
            exit;
        end;
    // rate of change rise/fall limit
        if (PVSys.Presentkvar / QHeadroom[m] - FPriorvarspu[m]) <= 0 then
        begin
            if (PVSys.Presentkvar <= 0) then
                Qdesiredpu_temp := Max((FPriorvarspu[m] - (FRiseFallLimit * (1.0 / ActiveCircuit.Solution.DynaVars.h))), PVSys.Presentkvar / QHeadroom[m])
            else
                Qdesiredpu_temp := Min((FPriorvarspu[m] - (FRiseFallLimit * (1.0 / ActiveCircuit.Solution.DynaVars.h))), PVSys.Presentkvar / QHeadroom[m])
        end
        else
        begin
            if (PVSys.Presentkvar <= 0) then
                Qdesiredpu_temp := Max((FPriorvarspu[m] + (-1.0 * FRiseFallLimit * (1.0 / ActiveCircuit.Solution.DynaVars.h))), PVSys.Presentkvar / QHeadroom[m])
            else  // TODO - Wes check the following, Tom prepended Qdesiredpu_temp :=
                Qdesiredpu_temp := Min((FPriorvarspu[m] + (-1.0 * FRiseFallLimit * (1.0 / ActiveCircuit.Solution.DynaVars.h))), PVSys.Presentkvar / QHeadroom[m]);
        end;
        FROCEvaluated[m] := TRUE;
        Result := Qdesiredpu_temp * QHeadRoom[m];
    end;

    if powertype = 'WATTS' then
    begin
        // rate of change rise/fall limit
        if (abs(FFinalpuPmpp[m] - FPriorWattspu[m]) / (1.0 / ActiveCircuit.Solution.DynaVars.h * 1.0)) > FRiseFallLimit then
        begin
            if (FFinalpuPmpp[m] - FPriorWattspu[m]) <= 0 then
                Pdesiredpu_temp := (FPriorWattspu[m] - (FRiseFallLimit * (1.0 / ActiveCircuit.Solution.DynaVars.h)))
            else
                Pdesiredpu_temp := (FPriorWattspu[m] + (FRiseFallLimit * (1.0 / ActiveCircuit.Solution.DynaVars.h)));
            if (Pdesiredpu_temp > PVSys.PresentkW / PVSys.PVSystemVars.FPmpp) then
                Pdesiredpu_temp := PVSys.PresentkW / PVSys.PVSystemVars.FPmpp;
            if (Pdesiredpu_temp <> 0.0) then
            begin
                Pdesiredpu := Pdesiredpu_temp;
                DeltaP := Pdesiredpu - POld[m];
                Result := POld[m] + DeltaP * FdeltaP_factor;
            end;
        end
        else
            Result := PVSys.PresentkW / PVSys.PVSystemVars.FPmpp;
    end;
end;

procedure TInvControlObj.CalcVoltVar_vars(j: Integer);
var
    voltagechangesolution, QPresentpu, VpuFromCurve,
    DeltaQ: Double;
    // SMonitoredElement                        :Complex;

    // local pointer to current PVSystem element
    PVSys: TPVSystemObj;
    FDiffvar: array of Double;
    FDesiredpu_temp: array of Double;
begin
    SetLength(FDiffvar, 4 + 1);
    SetLength(FDesiredpu_temp, 4 + 1);

    PVSys := ControlledElement[j];

    // SMonitoredElement := PVSys.Power[1]; // s is in va

    QDesiredpu[j] := 0.0;

    if FVV_ReacPower_ref = VARAVAL_WATTS then
    begin
        if (PVSys.PresentkW < PVSys.kVARating) then
            QHeadRoom[j] := SQRT(Sqr(PVSys.kVARating) - Sqr(PVSys.PresentkW))
        else
            QHeadRoom[j] := 0.0
    end;

    if (FVV_ReacPower_ref = VARMAX_VARS) or (FVV_ReacPower_ref = VARMAX_WATTS) then
        QHeadRoom[j] := PVSys.kvarLimit;

    if (QHeadRoom[j] = 0.0) then
        QHeadRoom[j] := PVSys.kvarLimit;
    QPresentpu := PVSys.Presentkvar / QHeadRoom[j];
    voltagechangesolution := 0.0;

      // for first two seconds, keep voltagechangesolution equal to zero
      // we don't have solutions from the time-series power flow, yet
    if ((ActiveCircuit.Solution.DynaVars.dblHour * 3600.0 / ActiveCircuit.Solution.DynaVars.h) < 3.0) then
        voltagechangesolution := 0.0
    else
    if (FVpuSolutionIdx = 1) then
        voltagechangesolution := FVpuSolution[j, 1] - FVpuSolution[j, 2]
    else
    if (FVpuSolutionIdx = 2) then
        voltagechangesolution := FVpuSolution[j, 2] - FVpuSolution[j, 1];

      // if no hysteresis (Fvvc_curveOffset == 0), then just look up the value
      // from the volt-var curve
    if (FWithinTolVV[j] = FALSE) then
    begin
        if Fvvc_curveOffset = 0.0 then
        begin  // no hysteresis
            Qdesiredpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j])
        end // end of logic for the no-hysteresis case

      // else if we're going in the positive direction and on curve 1, stay
      // with curve 1
        else
        if (voltagechangesolution > 0) and (FActiveVVCurve[j] = 1) then
        begin
            if (FlagChangeCurve[j] = TRUE) then
            begin
                VpuFromCurve := Fvvc_curve.GetXValue(QPresentpu);
                if (Abs(FPresentVpu[j] - VpuFromCurve) < FVoltageChangeTolerance / 2.0) then
                begin
                    Qdesiredpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]);      //Y value = in per-unit of headroom
                    FlagChangeCurve[j] := FALSE;

                end
                else
                begin
                    Qdesiredpu[j] := QPresentpu;
                    FlagChangeCurve[j] := FALSE;
                end;
            end
            else
            begin
                Qdesiredpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]);      //Y value = in per-unit of headroom
            end
        end

      // with hysteresis if we're going in the positive direction on voltages
      // from last two power flow solutions, and we're using curve 2, keep vars
      // the same, and change to curve1 active
        else
        if (voltagechangesolution > 0) and (FActiveVVCurve[j] = 2) then
        begin
            Qdesiredpu[j] := QPresentpu;
            FActiveVVCurve[j] := 1;
            FlagChangeCurve[j] := TRUE;
        end

      // with hysteresis if we're going in the negative direction on voltages
      // from last two power flow solutions, and we're using curve 2, either
      // lookup the vars for the voltage we're at (with offset on curve1),
      // or if we've not just changed curves, stay at the current p.u.
      // var output
        else
        if (voltagechangesolution < 0) and (FActiveVVCurve[j] = 2) then
        begin
            if (FlagChangeCurve[j] = TRUE) then
            begin
                VpuFromCurve := Fvvc_curve.GetXValue(QPresentpu);
                VpuFromCurve := VpuFromCurve - Fvvc_curveOffset;
                if (Abs(FPresentVpu[j] - VpuFromCurve) < FVoltageChangeTolerance / 2.0) then
                begin
                    Qdesiredpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j] - Fvvc_curveOffset);      //Y value = in per-unit of headroom
                    FlagChangeCurve[j] := FALSE;
                end
                else
                begin
                    Qdesiredpu[j] := QPresentpu;
                    FlagChangeCurve[j] := FALSE;

                end;
            end
            else
            begin
                Qdesiredpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j] - Fvvc_curveOffset);      //Y value = in per-unit of headroom

            end
        end

      // with hysteresis if we're going in the negative direction on voltages
      // from last two power flow solutions, and we're using curve 1, then
      // stay wjth present output vars and make curve2 active, set curve change
      // flag
        else
        if (voltagechangesolution < 0) and (FActiveVVCurve[j] = 1) then
        begin
            Qdesiredpu[j] := QPresentpu;
            FActiveVVCurve[j] := 2;
            FlagChangeCurve[j] := TRUE;
        end


      // if no change in voltage from one powerflow to the next, then
      // do one of the following
        else
        if (voltagechangesolution = 0) and (FActiveVVCurve[j] = 1) and (FlagChangeCurve[j] = FALSE) then
        begin
            Qdesiredpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j]);
        end
        else
        if (voltagechangesolution = 0) and (FlagChangeCurve[j] = TRUE) then
        begin
            Qdesiredpu[j] := QPresentpu;
        end

        else
        if (voltagechangesolution = 0) and (FActiveVVCurve[j] = 2) and (FlagChangeCurve[j] = FALSE) then
        begin
            Qdesiredpu[j] := Fvvc_curve.GetYValue(FPresentVpu[j] - Fvvc_curveOffset);
        end;

        if SQRT(Sqr(PVSys.kVARating) - Sqr(PVSys.PresentkW)) = 0 then
            QDesiredpu[j] := 0.0;
    // only move deltaQ_factor amount to the desired p.u. available var
    // output
        if (FROCEvaluated[j] = FALSE) then
        begin
            if (FlagChangeCurve[j] = FALSE) then
            begin
                QDeliver[j] := QDesiredpu[j] * QHeadRoom[j];
                DeltaQ := QDeliver[j] - QoldVV[j];

                QNew[j] := QOldVV[j] + DeltaQ * FdeltaQ_factor;
            end

        // else, stay at present var output level
            else
            begin
                QNew[j] := PVSys.Presentkvar;

            end;
        end;

    end;
    Finalize(FDiffvar);
    Finalize(FDesiredpu_temp);
end;


//Called at end of main power flow solution loop
procedure TInvControl.UpdateAll;
var
    i: Integer;
begin
    for i := 1 to ElementList.Count do
        with TInvControlObj(ElementList.Get(i)) do
            if Enabled then
                UpdateInvControl(i);
end;

finalization    ModeEnum.Free;
    CombiModeEnum.Free;
    RoCEnum.Free;
    VV_RefQEnum.Free;
    VWYAxisEnum.Free;
    VCurveXRefEnum.Free;
end.
