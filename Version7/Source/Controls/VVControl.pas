unit VVControl;

// added using GenDispatcher as a template
{
  ----------------------------------------------------------
  Copyright (c) 2010, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
  A VVCControl is a control element that is connected to a terminal of another
  circuit element and sends dispatch kW signals and kvar to a set of generators it controls

  A VVCControl is defined by a New command:

  New VVCControl.Name=myname Element=devclass.name terminal=[ 1|2|...] GenList = (gen1  gen2 ...)


}

interface

uses
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    Bus,
    DSSClass,
    Arraydef,
    ucomplex,
    utilities,
    XYcurve,
    PointerList,
    Classes;

type
  // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TVVControl = class(TControlClass)
    PRIVATE
        XY_CurveClass: TDSSClass;
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const VVCControlName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE; // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;
        function GetVVCCurve(const CurveName: String): TXYcurveObj;
    end;

  // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TVVControlObj = class(TControlElem)
    PRIVATE

        Fvvc_Vmaxpu, Fvvc_Vminpu, Fkva_rating, FkW_rating, Fkvar_fulloutput, Fpf,
        Fdelay, Fdelayoff, FkW_ramp_rate, Fkvar_ramp_rate, FkW_limit,
    // kw limit at the monitored element
        Fkvar_limit, // kvar limit at the monitored element
        DeltaVTolerance, // tolerance of voltage change from one solution to the
    // next for the voltage at the monitored element - in pu
        TotalWeight: Double;
        QOldDeliver: Double;
        Qdeliver: Double;
        QNew: Double;
        VavgpuPrior: Double;
        Vavgpu: Double;
        presentHour: Double;
        ControlActionHandle: Integer;
        FListSize: Integer;
        FGeneratorNameList: TStringList;
        FGenPointerList: PointerList.TPointerList;
        FWeights: pDoubleArray;
        Fvvc_curve_size: Integer;
        Fvvc_curve: TXYcurveObj;
        FPendingChange: Integer;
        FdeltaQ_factor: Double;

        MonitoredElement: TDSSCktElement;

        cBuffer: pComplexArray; // Complexarray buffer
        CondOffset: Integer; // Offset for monitored terminal
        procedure Set_PendingChange(const Value: Integer);

    PUBLIC
        property PendingChange: Integer READ FPendingChange WRITE Set_PendingChange;

        constructor Create(ParClass: TDSSClass; const VVCControlName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence; OVERRIDE; // Make a positive Sequence Model
        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE; // Always Zero for a VVCControl

        procedure Sample; OVERRIDE; // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;
    // Do the action that is pending from last sample
        procedure Reset; OVERRIDE; // Reset to initial defined state

        procedure GetCurrents(Curr: pComplexArray); OVERRIDE;
    // Get present value of terminal Curr
        procedure GetInjCurrents(Curr: pComplexArray); OVERRIDE;
    // Returns Injextion currents

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;

        function MakeGenList: Boolean;
        function ReturnGensList: String;
        function ReturnWeightsList: String;
        function ReturnVVCurve: String;

    end;

var
    ActiveVVCControlObj: TVVControlObj;

  { -------------------------------------------------------------------------- }
implementation

uses
    ParserDel,
    DSSClassDefs,
    MyDSSClassDefs,
    DSSGlobals,
    Circuit,
    Generator,
    Sysutils,
    uCmatrix,
    MathUtil,
    Math;

const

    NumPropsThisClass = 19;

    NONE = 0;
    CHANGEVARLEVEL = 1;

  { -------------------------------------------------------------------------- }
constructor TVVControl.Create; // Creates superstructure for all VVCControl objects
begin
    inherited Create;

    Class_name := 'VVControl';
    DSSClassType := DSSClassType + VV_CONTROL;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
    XY_CurveClass := GetDSSClassPtr('XYCurve');

end;

{ -------------------------------------------------------------------------- }
destructor TVVControl.Destroy;

begin
    inherited Destroy;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TVVControl.DefineProperties;
begin

    NumProperties := NumPropsThisClass;
    CountProperties; // Get inherited property count
    AllocatePropertyArrays;

  // Define Property names
    PropertyName[1] := 'Element';
    PropertyName[2] := 'Terminal';
    PropertyName[3] := 'vvc_Vmaxpu';
    PropertyName[4] := 'vvc_Vminpu';
    PropertyName[5] := 'kva_rating';
    PropertyName[6] := 'kW_rating';
    PropertyName[7] := 'kvar_full_output';
    PropertyName[8] := 'pf';
    PropertyName[9] := 'delay';
    PropertyName[10] := 'delayoff';
    PropertyName[11] := 'kW_ramp_rate';
    PropertyName[12] := 'kvar_ramp_rate';
    PropertyName[13] := 'kW_limit';
    PropertyName[14] := 'kvar_limit';
    PropertyName[15] := 'GenList';
    PropertyName[16] := 'Weights';
    PropertyName[17] := 'NumPts';
    PropertyName[18] := 'VVC_curve';
    PropertyName[19] := 'deltaQ_factor';

    PropertyHelp[1] :=
        'Full object name of the circuit element, typically a line or transformer, ' + 'which the control is monitoring. There is no default; must be specified.';
    PropertyHelp[2] :=
        'Number of the terminal of the circuit element to which the VVCControl control is connected. ' + '1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit.';
    PropertyHelp[3] :=
        'Default = 0.90.  Minimum per unit voltage for which the vvccurve volts property is assumed to apply. ' + 'Below this value, the var output is zero (i.e., the unit will not operate).';
    PropertyHelp[4] :=
        'Default = 1.10.  Maximum per unit voltage for which the vvccurve volts property is assumed to apply. ' + 'Above this value, the var output is zero (i.e., the unit will not operate).';
    PropertyHelp[5] :=
        'Default = 1.2 times the kW_rating of the unit.  Maximum steady-state apparent power output.';
    PropertyHelp[6] :=
        'Default = 4.0.  Maximum steady-state active power output of the unit under control.';
    PropertyHelp[7] :=
        'Max kvar to be delivered through the element.  Corresponds to the +/- 1.0 per-unit var value in the volt/var curve.';
    PropertyHelp[8] :=
        'Displacement power factor set-point of the inverter (modeled as a generator).  PF set-point will not cause delivered kvar to exceed the maximum kvar limit.';
    PropertyHelp[9] :=
        'Delay in seconds for switching ON the inverter (modeled as a generator). Default is 0.0 s';
    PropertyHelp[10] :=
        'Delay in seconds for switching OFF the inverter (modeled as a generator). Default is 0.0 s';
    PropertyHelp[11] :=
        'Ramp rate in kW per second for turning ON and OFF the inverter.  Ramps the kW from 0 or other full to kW_rating over x seconds. Default is -1 denoting immediate switch ON/OFF, after optional delay';
    PropertyHelp[12] :=
        'Ramp rate in kvar per second for turning ON and OFF the inverter.  Ramps the kW from 0 or other full to kvar_limit over x seconds. Default is -1 denoting immediate switch ON/OFF, after optional delay';

    PropertyHelp[13] :=
        'kW Limit for the monitored element. The generators are dispatched to hold the active power to attempt to achieve this value.';
    PropertyHelp[14] :=
        'kvar Limit for the monitored element. The generators are dispatched to hold the reactive power to attempt to achieve this value.';
    PropertyHelp[15] :=
        'Array list of generators to be dispatched.  If not specified, all generators in the circuit are assumed dispatchable.';
    PropertyHelp[16] :=
        'Array of proportional weights corresponding to each generator in the GenList. The needed kW to get back to center band is dispatched to each generator according to these weights. Default is to set all weights to 1.0.';
    PropertyHelp[17] :=
        'Number of points expected to be in the volt curve or the var curve (XYcurve object).';
    PropertyHelp[18] :=
        'Name of the volt-var curve that has been previously defined using the XYcurve object.';
    PropertyHelp[19] :=
        'The maximum change in per-unit from the prior var output to the var output indicated by the volt-var curve (XYcurve object).';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties; // Add defs of inherited properties to bottom of list

end;

{ -------------------------------------------------------------------------- }

function TVVControl.GetVVCCurve(const CurveName: String): TXYcurveObj;

begin

    Result := XY_CurveClass.Find(CurveName);

    if Result = NIL then
        DoSimpleMsg('XY Curve object: "' + CurveName + '" not found.', 380);

end;

{ -------------------------------------------------------------------------- }

{ -------------------------------------------------------------------------- }
function TVVControl.NewObject(const ObjName: String): Integer;
begin
  // Make a new VVCControl and add it to VVCControl class list
    with ActiveCircuit do
    begin
        ActiveCktElement := TVVControlObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

{ -------------------------------------------------------------------------- }
function TVVControl.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    i: Integer;

begin

  // continue parsing WITH contents of Parser
    ActiveVVCControlObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveVVCControlObj;

    Result := 0;

    with ActiveVVCControlObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' +
                        Class_name + '.' + Name + '"', 364);
                1:
                    ElementName := lowercase(Param);
                2:
                    ElementTerminal := Parser.IntValue;
                3:
                    Fvvc_Vmaxpu := Parser.DblValue;
                4:
                    Fvvc_Vminpu := Parser.DblValue;
                5:
                    Fkva_rating := Parser.DblValue;
                6:
                    FkW_rating := Parser.DblValue;
                7:
                    Fkvar_fulloutput := Parser.DblValue;
                8:
                    Fpf := Parser.DblValue;
                9:
                    Fdelay := Parser.DblValue;
                10:
                    Fdelayoff := Parser.DblValue;
                11:
                    FkW_ramp_rate := Parser.DblValue;
                12:
                    Fkvar_ramp_rate := Parser.DblValue;
                13:
                    FkW_limit := Parser.DblValue;
                14:
                    Fkvar_limit := Parser.DblValue;

                15:
                    InterpretTStringListArray(Param, FGeneratorNameList);
                16:
                begin
                    FListSize := FGeneratorNameList.count;
                    if FListSize > 0 then
                    begin
                        Reallocmem(FWeights, Sizeof(FWeights^[1]) * FListSize);
                        InterpretDblArray(Param, FListSize, FWeights);
                    end;
                end;
                17:
                    Fvvc_curve_size := Parser.IntValue;

                18:
                    Fvvc_curve := GetVVCCurve(Param);
                19:
                    FdeltaQ_factor := Parser.DblValue;
            else
        // Inherited parameters
                ClassEdit(ActiveVVCControlObj, ParamPointer - NumPropsThisClass)
            end;

            case ParamPointer of
                15:
                begin // re-alloc based on
                    FGenPointerList.Clear; // clear this for resetting on first sample
                    FListSize := FGeneratorNameList.count;
                    Reallocmem(FWeights, Sizeof(FWeights^[1]) * FListSize);
                    for i := 1 to FListSize do
                        FWeights^[i] := 1.0;
                end;
                18:
                begin // re-set the number vvc_curve_size property to the number
            // of points in the curve
                    if Fvvc_curve.NumPoints <> Fvvc_curve_size then
                        Fvvc_curve_size := Fvvc_curve.NumPoints;
                end;

            else

            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        RecalcElementData;
    end;

end;

{ -------------------------------------------------------------------------- }
function TVVControl.MakeLike(const VVCControlName: String): Integer;
var
    OtherVVCControl: TVVControlObj;
    i: Integer;
begin
    Result := 0;
  { See if we can find this VVCControl name in the present collection }
    OtherVVCControl := Find(VVCControlName);
    if OtherVVCControl <> NIL then
        with ActiveVVCControlObj do
        begin

            NPhases := OtherVVCControl.Fnphases;
            NConds := OtherVVCControl.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherVVCControl.ElementName;
            ControlledElement := OtherVVCControl.ControlledElement;
      // Pointer to target circuit element
            MonitoredElement := OtherVVCControl.MonitoredElement;
      // Pointer to monitored circuit element
            ElementTerminal := OtherVVCControl.ElementTerminal;
            CondOffset := OtherVVCControl.CondOffset;
            DeltaVTolerance := OtherVVCControl.DeltaVTolerance;

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherVVCControl.PropertyValue[i];

        end
    else
        DoSimpleMsg('Error in VVCControl MakeLike: "' + VVCControlName +
            '" Not Found.', 370);

end;

{ ========================================================================== }
{ TVVControlObj }
{ ========================================================================== }

{ -------------------------------------------------------------------------- }
constructor TVVControlObj.Create(ParClass: TDSSClass;
    const VVCControlName: String);

begin
    inherited Create(ParClass);
    Name := lowercase(VVCControlName);
    DSSObjType := ParClass.DSSClassType;

    NPhases := 1; // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1; // this forces allocation of terminals and conductors
  // in base class

    ElementName := '';
    ControlledElement := NIL;
    ElementTerminal := 1;
    MonitoredElement := NIL;
    FGeneratorNameList := TStringList.Create;
    FWeights := NIL;
    FGenPointerList := PointerList.TPointerList.Create(20);
  // Default size and increment
    FListSize := 0;
    Fvvc_Vmaxpu := 1.1;
    Fvvc_Vminpu := 0.9;
    Fkva_rating := 7.0;
    FkW_rating := 5.83;
    Fkvar_fulloutput := 3.86;
    Fpf := 0.83;
    Fdelay := 0.0;
    Fdelayoff := 0.0;
    FkW_ramp_rate := -1.0;
    Fkvar_ramp_rate := -1.0;
    FkW_limit := 10000;
    Fkvar_limit := FkW_limit / 2.0;
    FdeltaQ_factor := 0.1;
    DeltaVTolerance := 0.00001; // in per-unit
    Qdeliver := 1.0;
    QOldDeliver := 0.0;
    QNew := 0.0;
    VavgpuPrior := 0.0;
    Vavgpu := 0.0;
    presentHour := -1.0;
    TotalWeight := 1.0;
    InitPropertyValues(0);

    Fvvc_curve := NIL;
    Fvvc_curve_size := 0;
    PendingChange := NONE;
end;

destructor TVVControlObj.Destroy;
begin
    ElementName := '';
    inherited Destroy;
end;

{ -------------------------------------------------------------------------- }
procedure TVVControlObj.RecalcElementData;

var
    DevIndex: Integer;
begin

  { Check for existence of monitored element }

    DevIndex := GetCktElementIndex(ElementName); // Global function
    if DevIndex > 0 then
    begin
        MonitoredElement := ActiveCircuit.CktElements.Get(DevIndex);
        if ElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg('VVCControl: "' + Name + '"',
                'Terminal no. "' + Format('%-d', [ElementTerminal]) + '" does not exist.', 'Re-specify terminal no.', 371);
        end
        else
        begin
      // Sets name of i-th terminal's connected bus in VVCControl's buslist
            Setbus(1, MonitoredElement.GetBus(ElementTerminal));
        end;
        Reallocmem(cBuffer, Sizeof(cBuffer^[1]) * MonitoredElement.Yorder);
        CondOffset := (ElementTerminal - 1) * MonitoredElement.NConds;
    // for speedy sampling
    end
    else
        DoSimpleMsg('Monitored Element in VVCControl.' + Name +
            ' does not exist:"' + ElementName + '"', 372);

    if FGenPointerList.ListSize = 0 then
        MakeGenList;

    DevIndex := GetCktElementIndex('generator.' + FGeneratorNameList.Strings[0]);
  // Global function

    if DevIndex > 0 then
  // right now we only support one controlled element (generator) per vvcontrol
    begin // Controlled element must already exist
        ControlledElement := ActiveCircuit.CktElements.Get(DevIndex);
        ControlledElement.ActiveTerminalIdx := 1; // Make the 1 st terminal active
    // Get control synched up with capacitor
    end
    else
    begin
        ControlledElement := NIL; // element not found
        DoErrorMsg('VVControl: "' + Self.Name + '"',
            'Controlled Element "' + FGeneratorNameList.Strings[0] + '" Not Found.',
            ' Element must be defined previously.', 361);
    end;

end;

procedure TVVControlObj.MakePosSequence;
begin
    if ControlledElement <> NIL then
    begin
        Enabled := ControlledElement.Enabled;
        NPhases := ControlledElement.NPhases;
        NConds := Fnphases;
    end;
    if MonitoredElement <> NIL then
    begin
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    // Allocate a buffer bigenough to hold everything from the monitored element
    // ReAllocMem(cBuffer, SizeOF(cBuffer^[1]) * MonitoredElement.Yorder );
    // CondOffset := (ElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling
    end;

    inherited;
end;

{ -------------------------------------------------------------------------- }
procedure TVVControlObj.CalcYPrim;
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  // IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;

{ -------------------------------------------------------------------------- }
procedure TVVControlObj.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;

procedure TVVControlObj.GetInjCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

{ -------------------------------------------------------------------------- }
procedure TVVControlObj.DumpProperties(var F: TextFile; Complete: Boolean);

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

{ -------------------------------------------------------------------------- }
procedure TVVControlObj.DoPendingAction;
var
    i: Integer;
    DeltaQ, Qheadroom, Qdesiredpu, QNeeded, PPresentGenOutput,
    QPresentGenOutput, QMonitoredElement, Genkvar: Double;
    SMonitoredElement, SPresentGenOutput: Complex;


    Gen: TGeneratorObj;

begin
    Genkvar := 0.0;
  // we need P and/or we need Q
    if (PendingChange = CHANGEVARLEVEL) then
    begin
        SMonitoredElement := MonitoredElement.Power[ElementTerminal]; // s is in va
    // PMonitoredElement := SMonitoredElement.re;
        QMonitoredElement := SMonitoredElement.im;

    // PNeeded := FkW_limit*1000 - PMonitoredElement;
        QNeeded := Fkvar_limit * 1000 - QMonitoredElement;
    // If the generator list is not defined, go make one
        if FGenPointerList.ListSize = 0 then
            MakeGenList;

        ControlledElement.ActiveTerminalIdx := 1; // Set active terminal of generator to terminal 1
        if (QNeeded <> 0.0) then
        begin
            for i := 1 to FListSize do
            begin
                SPresentGenOutput := ControlledElement.Power[1];
        // s is in va; we want terminal 1 of the generator
                PPresentGenOutput := SPresentGenOutput.re;
                QPresentGenOutput := SPresentGenOutput.im;

        // q desired pu is the desired output based on the avg pu voltage on the
        // monitored element
                Qdesiredpu := Fvvc_curve.GetYValue(Vavgpu);      //Y value = var in pu

        // The var 'head-room' available on the inverter given its rating
        // and present kW output
                if (Abs(PPresentGenOutput) > Fkva_rating * 1000.0) then
                begin
                    Qheadroom := 0.0;
                end
                else
                    Qheadroom := SQRT(Sqr(Fkva_rating * 1000.0) - Sqr(PPresentGenOutput));
                Qdeliver := Min(Abs(Fkvar_fulloutput * 1000.0), Abs(Qheadroom));

                Qdeliver := Qdeliver * Qdesiredpu;
                DeltaQ := Qdeliver - QOldDeliver;

        // only allow a small movement from old delivered (prior gen Q)
        // to the desired delivered Q
                QNew := QOldDeliver + DeltaQ * FdeltaQ_factor;

                if (QNew <> QPresentGenOutput) then
                begin
                    Gen := FGenPointerList.Get(i);
                    Genkvar := Sign(QNew) * (Min(Abs(Fkvar_limit * 1000.0), Abs(QNew))) / 1000.0;
                    if Genkvar <> Gen.kvarBase then
                    begin
                        Gen.Presentkvar := Genkvar;
                    end;
                end;
                AppendtoEventLog('VoltVarControl.' + Self.Name,
                    Format('**Set var output level to**, kvar= %.5g', [Genkvar]));
            end; // end for i equals 1 to number of generators under this control
        end; // end if vars needed is not equal to zero
    // WriteDLLDebugFile(Self.Name+','+Format('%-.5g',[ActiveCircuit.Solution.dblHour])+','+Format('%-.5g',[QPresentGenOutput])+','+Format('%-.5g',[Qdeliver])+','+Format('%-.5g',[DeltaQ])+','+Format('%-.5g',[Qnew])+','+Format('%-.5g',[Gen.Presentkvar*1000.0])+','+Format('%-.5g',[QoldDeliver])+','+Format('%-.5g',[PPresentGenOutput])+','+Format('%-.5g',[Vavgpu])+','+Format('%-.5g',[Vavgpuprior])+','+Format('%-.5g',[Qdesiredpu]));
        QOldDeliver := QNew;
        VavgpuPrior := Vavgpu;
        ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
    // Force recalc of power parms
        Set_PendingChange(NONE);
    end // end if PendingChange = CHANGEVARLEVEL

    else // else set PendingChange to NONE
    begin
        Set_PendingChange(NONE);
    end;

end;

{ -------------------------------------------------------------------------- }
procedure TVVControlObj.Sample;
var
    i: Integer;

    basekV, Vavg: Double;

begin
  // If list is not defined, go make one for all generators in circuit
    if FGenPointerList.ListSize = 0 then
        MakeGenList;

    if ((FListSize > 0) and (Fvvc_curve_size > 0)) then
    begin

    // if(presentHour <> ActiveCircuit.Solution.dblHour) then begin
    // WriteDLLDebugFile(Self.Name+','+Format('%-.5g',[ActiveCircuit.Solution.dblHour])+','+Format('%-.5g',[QPresentGenOutput])+','+Format('%-.5g',[Qdeliver])+','+Format('%-.5g',[QNew])+','+Format('%-.5g',[Gen.Presentkvar*1000.0])+','+Format('%-.5g',[QoldDeliver])+','+Format('%-.5g',[PPresentGenOutput])+','+Format('%-.5g',[Vavgpu])+','+Format('%-.5g',[Vavgpuprior]));
    // presentHour = ActiveCircuit.Solution.dblHour;
    // end;

        MonitoredElement.ComputeVTerminal;
        cBuffer := MonitoredElement.Vterminal;

    // get the basekV for the monitored bus
        basekV := ActiveCircuit.Buses^[Terminals^[ElementTerminal].BusRef].kVbase;
        Vavg := 0;

    // Calculate the average voltage
        for i := 1 to MonitoredElement.NPhases do
            Vavg := Vavg + Cabs(cBuffer^[i]);

    // and convert to pu
        Vavgpu := (Vavg / MonitoredElement.NPhases) / (basekV * 1000.0);

        TimeDelay := Fdelay;
    // and
    // if (ActiveCircuit.Solution.ControlIteration < ActiveCircuit.Solution.MaxControlIterations) then
    // begin
        if (Abs(Vavgpu - VavgpuPrior) > DeltaVTolerance) or
            (Abs(Abs(Qdeliver) - Abs(QNew)) > 0.5) then
        begin
            Set_PendingChange(CHANGEVARLEVEL);
      // ActiveCircuit.Solution.LoadsNeedUpdating := TRUE; // Force recalc of power parms
            ControlActionHandle := ActiveCircuit.ControlQueue.Push
                (ActiveCircuit.Solution.DynaVars.intHour,
                ActiveCircuit.Solution.DynaVars.t + TimeDelay, PendingChange, 0, Self);
            AppendtoEventLog('VoltVarControl.' + Self.Name, Format
                ('**Ready to change var output**, Vavgpu= %.5g sec,', [Vavgpu]));
        end
    // end;
        else
        begin
            ActiveCircuit.ControlQueue.Delete(ControlActionHandle);
            AppendtoEventLog('VoltVarControl.' + Self.Name, '**DONE**');
        end;

    end
    else
    begin
        DoSimpleMsg(
            'Could not find any generators, or the vvc curve size is zero.  Please correct in your script.', 1234);
    end;
end;

procedure TVVControlObj.InitPropertyValues(ArrayOffset: Integer);

begin

    PropertyValue[1] := ''; // 'element';
    PropertyValue[2] := '1'; // 'terminal';
    PropertyValue[3] := '1.1'; // vmax_pu of the inverter
    PropertyValue[4] := '0.9'; // vmin_pu of the inverter
    PropertyValue[5] := '7.0';
    PropertyValue[6] := '5.83';
    PropertyValue[7] := '3.5';
    PropertyValue[8] := '0.83';
    PropertyValue[9] := '0.0';
    PropertyValue[10] := '0.0';
    PropertyValue[11] := '-1.0';
    PropertyValue[12] := '-1.0';
    PropertyValue[13] := '1000.0'; // kw_limit through the monitored element
    PropertyValue[14] := '500.0'; // kvar_limit through the monitored element
    PropertyValue[15] := '';
    PropertyValue[16] := '';
    PropertyValue[17] := '0'; // curve size
    PropertyValue[18] := 'none'; // volt-var curve
    PropertyValue[19] := '0.1'; // deltaQ_factor

    inherited InitPropertyValues(NumPropsThisClass);

end;

// need to edit this for the :  WGS
// ----------------------------------------------------------------------------
function TVVControlObj.GetPropertyValue(Index: Integer): String;
begin

    Result := '';
    case Index of
        1:
            Result := MonitoredElement.DisplayName;
        2:
            Result := Format('%-d', [ElementTerminal]);
        3:
            Result := Format('%-.3g', [Fvvc_Vmaxpu]);
        4:
            Result := Format('%-.3g', [Fvvc_Vminpu]);
        5:
            Result := Format('%-.3g', [Fkva_rating]);
        6:
            Result := Format('%-.3g', [FkW_rating]);
        7:
            Result := Format('%-.3g', [Fkvar_fulloutput]);
        8:
            Result := Format('%-.3g', [Fpf]);
        9:
            Result := Format('%-.3g', [Fdelay]);
        10:
            Result := Format('%-.3g', [Fdelayoff]);
        11:
            Result := Format('%-.3g', [FkW_ramp_rate]);
        12:
            Result := Format('%-.3g', [Fkvar_ramp_rate]);
        13:
            Result := Format('%-.3g', [FkW_limit]);
        14:
            Result := Format('%-.3g', [Fkvar_limit]);
        15:
            Result := ReturnGensList;
        16:
            Result := ReturnWeightsList;
        17:
            Result := Format('%-d', [Fvvc_curve_size]);
        18:
            Result := ReturnVVCurve;
        19:
            Result := Format('%-.3g', [FdeltaQ_factor]);

    else // take the generic handler
        Result := inherited GetPropertyValue(index);

    end;

end;

function TVVControlObj.MakeGenList: Boolean;

var
    GenClass: TDSSClass;
    Gen: TGeneratorObj;
    i: Integer;

begin

    Result := FALSE;
    GenClass := GetDSSClassPtr('generator');

    if FListSize > 0 then
    begin // Name list is defined - Use it

        for i := 1 to FListSize do
        begin
            Gen := GenClass.Find(FGeneratorNameList.Strings[i - 1]);
            if Assigned(Gen) and Gen.Enabled then
                FGenPointerList.New := Gen;
        end;

    end
    else
    begin
    { Search through the entire circuit for enabled generators and add them to the list }

        for i := 1 to GenClass.ElementCount do
        begin
            Gen := GenClass.ElementList.Get(i);
            if Gen.Enabled then
                FGenPointerList.New := Gen;
            FGeneratorNameList.Add(Gen.DisplayName);
        end;

    { Allocate uniform weights }
        FListSize := FGenPointerList.ListSize;
        Reallocmem(FWeights, Sizeof(FWeights^[1]) * FListSize);
        for i := 1 to FListSize do
            FWeights^[i] := 1.0;

    end;

  // Add up total weights
    TotalWeight := 0.0;
    for i := 1 to FListSize do
        TotalWeight := TotalWeight + FWeights^[i];

    if FGenPointerList.ListSize > 0 then
        Result := TRUE;
end;

// -----------------------------------------------------------------------------
function TVVControlObj.ReturnGensList: String;
var
    i: Integer;
begin
    if FListSize = 0 then
    begin
        Result := '';
        Exit;
    end;

    Result := '[' + FGeneratorNameList.Strings[0];
    for i := 1 to FListSize - 1 do
    begin
        Result := Result + ', ' + FGeneratorNameList.Strings[i];
    end;
    Result := Result + ']'; // terminate the array

end;

// ----------------------------------------------------------------------------
function TVVControlObj.ReturnWeightsList: String;
var
    i: Integer;
begin
    if FListSize = 0 then
    begin
        Result := '';
        Exit;
    end;

    Result := '[' + Format('%-.6g', [FWeights^[1]]);
    for i := 2 to FListSize do
    begin
        Result := Result + Format(', %-.6g', [FWeights^[i]]);
    end;
    Result := Result + ']'; // terminate the array
end;

// ----------------------------------------------------------------------------
function TVVControlObj.ReturnVVCurve: String;
var
    i: Integer;
begin
    if Fvvc_curve_size = 0 then
    begin
        Result := '';
        Exit;
    end;

    Result := '[{' + Format('%-.3g,', [Fvvc_curve.XValue_pt[1]]) + Format
        ('%-.3g', [Fvvc_curve.YValue_pt[1]]) + '},';
    for i := 2 to Fvvc_curve_size do
    begin
        Result := Result + Format('{ %-.3g,', [Fvvc_curve.XValue_pt[i]]) + Format
            ('%-.3g', [Fvvc_curve.YValue_pt[i]]) + '},';
    end;
    Result := Result + ']'; // terminate the array
end;

procedure TVVControlObj.Reset;
begin
    PendingChange := NONE;

end;

procedure TVVControlObj.Set_PendingChange(const Value: Integer);
begin
    FPendingChange := Value;
    DblTraceParameter := Integer(Value);
end;

initialization

end.
