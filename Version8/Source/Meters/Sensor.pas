unit Sensor;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Change Log
   8-24-2007 Created from Monitor Object
   Sept-Oct 2008 Modified for new load allocation and state estimator algorithms
}

{
   Sensor compares voltages and currents. Power quantities are converted to current quantities
   based on rated kVBase, or actual voltage if voltage measurement specified.
}

interface

uses
    Command,
    MeterClass,
    Meterelement,
    DSSClass,
    Arraydef,
    ucomplex,
    utilities,
    Classes;

type

{==============================================================================}

    TSensor = class(TMeterClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const SensorName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

        procedure ResetAll(ActorID: Integer); OVERRIDE;
        procedure SampleAll(ActorID: Integer); OVERRIDE;  // Force all Sensors to take a sample
        procedure SaveAll(ActorID: Integer); OVERRIDE;   // Force all Sensors to save their buffers to disk
        procedure SetHasSensorFlag;

    end;

{==============================================================================}

    TSensorObj = class(TMeterElement)
    PRIVATE
        ValidSensor: Boolean;
        SensorkW: pDoubleArray;
        Sensorkvar: pDoubleArray;
        kVBase: Double; // value specified
        Vbase: Double; // in volts

        FConn: Integer;

        Vspecified,
        Ispecified,
        Pspecified,
        Qspecified: Boolean;

        ClearSpecified: Boolean;
        FDeltaDirection: Integer;

        procedure Set_Conn(const Value: Integer);
        procedure Set_Action(const Value: String);
        procedure ZeroSensorArrays;
        procedure AllocateSensorObjArrays;
        procedure RecalcVbase;
        function RotatePhases(const j: Integer): Integer;
        function LimitToPlusMinusOne(const i: Integer): Integer;
        procedure ClearSensor;
        procedure UpdateCurrentVector;
        function Get_WLSCurrentError: Double;
        function Get_WLSVoltageError: Double;

    PUBLIC

        pctError,
        Weight: Double;

        constructor Create(ParClass: TDSSClass; const SensorName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model, reset nphases
        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;    // Always Zero for a Sensor
        procedure TakeSample(ActorID: Integer); OVERRIDE; // Go add a sample to the buffer
        procedure ResetIt;
        procedure Save;  // Saves present buffer to file

        procedure GetCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE; // Get present value of terminal Curr
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;   // Returns Injextion currents
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

       {Properties to interpret input to the sensor}

        property Conn: Integer READ Fconn WRITE Set_Conn;      // Connection code
        property Action: String WRITE Set_Action;
        property WLSCurrentError: Double READ Get_WLSCurrentError;
        property WLSVoltageError: Double READ Get_WLSVoltageError;

        property BaseKV: Double READ kvbase;
        property DeltaDirection: Integer READ FDeltaDirection;
       // the following two properties actually give write access, since they are pointers
        property SensorP: pDoubleArray READ SensorKW;
        property SensorQ: pDoubleArray READ SensorKVAR;
    end;

{==============================================================================}


var
    ActiveSensorObj: TSensorObj;

{==============================================================================}

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    CktElement,
    Transformer,
    PCElement,
    PDElement,
    Sysutils,
    ucmatrix,
    showresults,
    mathUtil,
    PointerList, {TOPExport,} Dynamics;

const

    NumPropsThisClass = 13;

{==============================================================================}

constructor TSensor.Create;  // Creates superstructure for all Sensor objects
begin
    inherited Create;

    Class_name := 'Sensor';
    DSSClassType := DSSClassType + SENSOR_ELEMENT;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

{==============================================================================}

destructor TSensor.Destroy;

begin
    inherited Destroy;
end;

{==============================================================================}

procedure TSensor.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names

    PropertyName[1] := 'element';
    PropertyName[2] := 'terminal';
    PropertyName[3] := 'kvbase';
    PropertyName[4] := 'clear';
    PropertyName[5] := 'kVs';
    PropertyName[6] := 'currents';
    PropertyName[7] := 'kWs';
    PropertyName[8] := 'kvars';
    PropertyName[9] := 'conn';  //  Sensor connection
    PropertyName[10] := 'Deltadirection';  //  +/- 1
    PropertyName[11] := '%Error';  //  %Error of sensor
    PropertyName[12] := 'Weight';  // for WLS calc
    PropertyName[13] := 'action';

    PropertyHelp[1] := 'Name (Full Object name) of element to which the Sensor is connected.';
    PropertyHelp[2] := 'Number of the terminal of the circuit element to which the Sensor is connected. ' +
        '1 or 2, typically. Default is 1.';
    PropertyHelp[3] := 'Voltage base for the sensor, in kV. If connected to a 2- or 3-phase terminal, ' + CRLF +
        'specify L-L voltage. For 1-phase devices specify L-N or actual 1-phase voltage. ' +
        'Like many other DSS devices, default is 12.47kV.';
    PropertyHelp[4] := '{ Yes | No }. Clear=Yes clears sensor values. Should be issued before putting in a new set of measurements.';
    PropertyHelp[5] := 'Array of Voltages (kV) measured by the voltage sensor. For Delta-connected ' +
        'sensors, Line-Line voltages are expected. For Wye, Line-Neutral are expected.';
    PropertyHelp[6] := 'Array of Currents (amps) measured by the current sensor. Specify this or power quantities; not both.';
    PropertyHelp[7] := 'Array of Active power (kW) measurements at the sensor. Is converted into Currents along with q=[...]' + CRLF +
        'Will override any currents=[...] specification.';
    PropertyHelp[8] := 'Array of Reactive power (kvar) measurements at the sensor. Is converted into Currents along with p=[...]';
    PropertyHelp[9] := 'Voltage sensor Connection: { wye | delta | LN | LL }.  Default is wye. Applies to voltage measurement only. ' + CRLF +
        'Currents are always assumed to be line currents.' + CRLF +
        'If wye or LN, voltage is assumed measured line-neutral; otherwise, line-line.';
    PropertyHelp[10] := '{1 or -1}  Default is 1:  1-2, 2-3, 3-1.  For reverse rotation, enter -1. Any positive or negative entry will suffice.';
    PropertyHelp[11] := 'Assumed percent error in the measurement. Default is 1.';
    PropertyHelp[12] := 'Weighting factor: Default is 1.';
    PropertyHelp[13] := 'NOT IMPLEMENTED.Action options: ' + CRLF + 'SQERROR: Show square error of the present value of the monitored terminal  ' + CRLF +
        'quantity vs the sensor value. Actual values - convert to per unit in calling program.  ' + CRLF +
        'Value reported in result window/result variable.';


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

{==============================================================================}

function TSensor.NewObject(const ObjName: String): Integer;
begin
    // Make a new Sensor and add it to Sensor class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TSensorObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

{==============================================================================}

function TSensor.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    DoRecalcElementData: Boolean;

begin

  // continue parsing with contents of Parser
    ActiveSensorObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveSensorObj;

    Result := 0;
    DoRecalcElementData := FALSE;

    with ActiveSensorObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 661);
                1:
                    ElementName := lowercase(param);
                2:
                    MeteredTerminal := Parser[ActorID].IntValue;
                3:
                    kVBase := Parser[ActorID].DblValue;
                4:
                    ClearSpecified := InterpretYesNo(Param);
                5:
                    Parser[ActorID].ParseAsVector(Fnphases, SensorVoltage);  // Inits to zero
                6:
                    Parser[ActorID].ParseAsVector(Fnphases, SensorCurrent);  // Inits to zero
                7:
                begin
                    Parser[ActorID].ParseAsVector(Fnphases, SensorkW);
                    Pspecified := TRUE;
                    UpdateCurrentVector;
                end;
                8:
                begin
                    Parser[ActorID].ParseAsVector(Fnphases, Sensorkvar);
                    Qspecified := TRUE;
                    UpdateCurrentVector;
                end;
                9:
                    Conn := InterpretConnection(Param);
                10:
                    FDeltaDirection := LimitToPlusMinusOne(Parser[ActorID].IntValue);
                11:
                    pctError := Parser[ActorID].dblValue;
                12:
                    Weight := Parser[ActorID].dblValue;
                13:
                    Action := Param;  // Put sq error in Global Result
            else
           // Inherited parameters
                ClassEdit(ActiveSensorObj, ParamPointer - NumPropsthisClass)
            end;

            case ParamPointer of
                1..2:
                begin
                    DoRecalcElementData := TRUE;
                    MeteredElementChanged := TRUE;
                end;
                3:
                    DoRecalcElementData := TRUE;

              {Do not recalc element data for setting of sensor quantities}
                4:
                    if ClearSpecified then
                        ClearSensor;
                5:
                    Vspecified := TRUE;
                6:
                    Ispecified := TRUE;
                7:
                    Pspecified := TRUE;
                8:
                    Qspecified := TRUE;

                9:
                    DoRecalcElementData := TRUE;
                10:
                    DoRecalcElementData := TRUE;
            end;

            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

        if DoRecalcElementData then
            RecalcElementData(ActorID);
    end;

end;

{==============================================================================}

procedure TSensor.ResetAll(ActorID: Integer);  // Force all Sensors in the circuit to reset

var
    pSensor: TSensorObj;

begin

    pSensor := ActiveCircuit[ActorID].Sensors.First;
    while pSensor <> NIL do
    begin
        if pSensor.enabled then
            pSensor.ResetIt;
        pSensor := ActiveCircuit[ActiveActor].Sensors.Next;
    end;

end;

{==============================================================================}

procedure TSensor.SampleAll(ActorID: Integer);  // Force all Sensors in the circuit to take a sample

var
    pSensor: TSensorObj;

begin


    pSensor := ActiveCircuit[ActorID].Sensors.First;
    while pSensor <> NIL do
    begin
        if pSensor.enabled then
            pSensor.TakeSample(ActorID);
        pSensor := ActiveCircuit[ActorID].Sensors.Next;
    end;

end;

{==============================================================================}

procedure TSensor.SaveAll(ActorID: Integer);     // Force all Sensors in the circuit to save their buffers to disk

//VAR
//   Mon:TSensorObj;

begin
{
   Mon := ActiveCircuit[ActiveActor].Sensors.First;
   WHILE Mon<>Nil DO
   Begin
       If Mon.Enabled Then Mon.Save;
       Mon := ActiveCircuit[ActiveActor].Sensors.Next;
   End;
}
end;

{==============================================================================}

procedure TSensor.SetHasSensorFlag;
// Set the HasSensorObj Flag for all cktElement;
var
    i: Integer;
    ThisSensor: TSensorObj;
    CktElem: TDSSCktElement;

begin
   {Initialize all to FALSE}
    with  ActiveCircuit[ActiveActor] do
    begin
        CktElem := PDElements.First;
        while CktElem <> NIL do
        begin
            CktElem.HasSensorObj := FALSE;
            CktElem := PDElements.Next;
        end;  {WHILE}
        CktElem := PCElements.First;
        while CktElem <> NIL do
        begin
            CktElem.HasSensorObj := FALSE;
            CktElem := PCElements.Next;
        end;  {WHILE}
    end; {WITH}

    for i := 1 to ActiveCircuit[ActiveActor].Sensors.ListSize do
    begin
        ThisSensor := ActiveCircuit[ActiveActor].Sensors.Get(i);
        with ThisSensor do
            if MeteredElement <> NIL then
            begin
                MeteredElement.HasSensorObj := TRUE;
                if MeteredElement is TPCElement then
                    TPCElement(MeteredElement).SensorObj := ThisSensor
                else
                    TPDElement(MeteredElement).SensorObj := ThisSensor;
            end;
    end;   {FOR}

end;

{==============================================================================}

function TSensor.MakeLike(const SensorName: String): Integer;
var
    OtherSensor: TSensorObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this Sensor name in the present collection}
    OtherSensor := Find(SensorName);
    if OtherSensor <> NIL then
        with ActiveSensorObj do
        begin

            NPhases := OtherSensor.Fnphases;
            NConds := OtherSensor.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherSensor.ElementName;
            MeteredElement := OtherSensor.MeteredElement;  // Pointer to target circuit element
            MeteredTerminal := OtherSensor.MeteredTerminal;
{==========================================================================}


{==========================================================================}
            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherSensor.PropertyValue[i];

            BaseFrequency := OtherSensor.BaseFrequency;

        end
    else
        DoSimpleMsg('Error in Sensor MakeLike: "' + SensorName + '" Not Found.', 662);

end;

{==============================================================================}

function TSensor.Init(Handle: Integer; ActorID: Integer): Integer;
var
    pSensor: TSensorObj;

begin
    Result := 0;

    if Handle > 0 then
    begin
        pSensor := ElementList.Get(Handle);
        pSensor.ResetIt;
    end
    else
    begin  // Do 'em all
        pSensor := ElementList.First;
        while pSensor <> NIL do
        begin
            pSensor.ResetIt;
            pSensor := ElementList.Next;
        end;
    end;

end;


{==========================================================================}
{                    TSensorObj                                           }
{==========================================================================}


{==============================================================================}

constructor TSensorObj.Create(ParClass: TDSSClass; const SensorName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(SensorName);

    Nphases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class

    SensorkW := NIL;
    Sensorkvar := NIL;

    kVBase := 12.47; // default 3-phase voltage
    Weight := 1.0;
    pctError := 1.0;

    Conn := 0;  // Wye

    ClearSensor;

    DSSObjType := ParClass.DSSClassType; //SENSOR_ELEMENT;

    InitPropertyValues(0);

   //  RecalcElementData;

end;

{==============================================================================}

destructor TSensorObj.Destroy;
begin
    ElementName := '';
    ReAllocMem(SensorkW, 0);
    ReAllocMem(Sensorkvar, 0);

    inherited Destroy;
end;

{==============================================================================}

procedure TSensorObj.RecalcElementData(ActorID: Integer);

var
    DevIndex: Integer;

begin
    ValidSensor := FALSE;
    Devindex := GetCktElementIndex(ElementName); // Global function
    if DevIndex > 0 then
    begin  // Sensored element must already exist
        MeteredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);

        if MeteredTerminal > MeteredElement.Nterms then
        begin
            DoErrorMsg('Sensor: "' + Name + '"',
                'Terminal no. "' + '" does not exist.',
                'Respecify terminal no.', 665);
        end
        else
        begin
            Nphases := MeteredElement.NPhases;
            Nconds := MeteredElement.NConds;

               // Sets name of i-th terminal's connected bus in Sensor's buslist
               // This value will be used to set the NodeRef array (see TakeSample)
            Setbus(1, MeteredElement.GetBus(MeteredTerminal));

            ClearSensor;

            ValidSensor := TRUE;

            AllocateSensorObjArrays;
            ZeroSensorArrays;
            RecalcVbase;
        end;

    end
    else
    begin
        MeteredElement := NIL;   // element not found
        DoErrorMsg('Sensor: "' + Self.Name + '"', 'Circuit Element "' + ElementName + '" Not Found.',
            ' Element must be defined previously.', 666);
    end;
end;

procedure TSensorObj.MakePosSequence(ActorID: Integer);
begin
    if MeteredElement <> NIL then
    begin
        Setbus(1, MeteredElement.GetBus(MeteredTerminal));
        Nphases := MeteredElement.NPhases;
        Nconds := MeteredElement.Nconds;
        ClearSensor;
        ValidSensor := TRUE;
        AllocateSensorObjArrays;
        ZeroSensorArrays;
        RecalcVbase;
    end;
    inherited;
end;

{==============================================================================}

procedure TSensorObj.RecalcVbase;
begin
    case Fconn of
        0:
            if Fnphases = 1 then
                Vbase := kVBase * 1000.0
            else
                Vbase := kVBase * 1000.0 / sqrt3;

        1:
            Vbase := kVBase * 1000.0;
    end;
end;

{==============================================================================}

procedure TSensorObj.CalcYPrim(ActorID: Integer);
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;

{==============================================================================}

procedure TSensorObj.ResetIt;

{What does it mean to reset a sensor?}
begin

    ClearSensor;

end;

{==============================================================================}

function TSensorObj.RotatePhases(const j: Integer): Integer;
// For Delta connections or Line-Line voltages
begin
    Result := j + FDeltaDirection;

     // make sure result is within limits
    if FnPhases > 2 then
    begin
         // Assumes 2 phase delta is open delta
        if Result > Fnphases then
            Result := 1;
        if Result < 1 then
            Result := Fnphases;
    end
    else
    if Result < 1 then
        Result := 3;    // For 2-phase delta, next phase will be 3rd phase

end;

{==============================================================================}

procedure TSensorObj.TakeSample(ActorID: Integer);
var
    i: Integer;
begin
    if not (ValidSensor and Enabled) then
        Exit;

    MeteredElement.GetCurrents(CalculatedCurrent, ActorID);
    ComputeVterminal(ActorID);
    case Fconn of
        1:
            for i := 1 to Fnphases do
                CalculatedVoltage^[i] := Csub(VTerminal^[i], VTerminal^[RotatePhases(i)]);
    else
        for i := 1 to Fnphases do
            CalculatedVoltage^[i] := VTerminal^[i];
    end;

   {NOTE: CalculatedVoltage is complex}

end;

{==============================================================================}

procedure TSensorObj.GetCurrents(Curr: pComplexArray; ActorID: Integer);  //Get present value of terminal Curr for reports
var
    i: Integer;
begin
{
  Return array of zero
}
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

{==============================================================================}

procedure TSensorObj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := cZero;
end;

procedure TSensorObj.UpdateCurrentVector;
{Updates the currentvector when P and Q are defined
 as the input vectors for the sensor}
var
    kVA: Double;
    i: Integer;
begin
{Convert P and Q specification to Currents}
    if Pspecified then
    begin    // compute currents assuming vbase
        if Qspecified then
        begin
            for i := 1 to FNPhases do
            begin
                kVA := Cabs(Cmplx(SensorkW^[i], Sensorkvar^[i]));
                SensorCurrent^[i] := kVA * 1000.0 / Vbase;
            end;
        end
        else
        begin    // No Q just use P
            for i := 1 to FNPhases do
            begin
                SensorCurrent^[i] := SensorkW^[i] * 1000.0 / Vbase;
            end;
        end;
        Ispecified := TRUE;    // Overrides current specification
    end;
end;

function TSensorObj.Get_WLSCurrentError: Double;
{
  Return the WLS Error for Currents
  Get Square error and weight it

}

var
    kVA: Double;
    i: Integer;
begin

    Result := 0.0;
{Convert P and Q specification to Currents}
    if Pspecified then
    begin    // compute currents assuming vbase
        if Qspecified then
        begin
            for i := 1 to FNPhases do
            begin
                kVA := Cabs(Cmplx(SensorkW^[i], Sensorkvar^[i]));
                SensorCurrent^[i] := kVA * 1000.0 / Vbase;
            end;
        end
        else
        begin    // No Q just use P
            for i := 1 to FNPhases do
            begin
                SensorCurrent^[i] := SensorkW^[i] * 1000.0 / Vbase;
            end;
        end;
        Ispecified := TRUE;    // Overrides current specification
    end;

    if Ispecified then
    begin
        for i := 1 to FnPhases do
            Result := Result + SQR(CalculatedCurrent^[i].re) + SQR(CalculatedCurrent^[i].im) - SQR(SensorCurrent^[i]);
    end;

    Result := Result * Weight;

end;

{==============================================================================}

function TSensorObj.Get_WLSVoltageError: Double;
// Get Square error and weight it
var
    i: Integer;
begin
    Result := 0.0;
    if Vspecified then
    begin
        for i := 1 to FnPhases do
            Result := Result + SQR(CalculatedVoltage^[i].re) + SQR(CalculatedVoltage^[i].im) - SQR(SensorVoltage^[i]);
    end;
    Result := Result * Weight;
end;

{==============================================================================}

procedure TSensorObj.DumpProperties(var F: TextFile; Complete: Boolean);

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

{==============================================================================}

procedure TSensorObj.ClearSensor;
begin
    Vspecified := FALSE;
    Ispecified := FALSE;
    Pspecified := FALSE;
    Qspecified := FALSE;
    ClearSpecified := FALSE;
end;

{==============================================================================}

procedure TSensorObj.AllocateSensorObjArrays;
begin
    ReAllocMem(SensorkW, Sizeof(SensorkW^[1]) * Fnphases);
    ReAllocMem(Sensorkvar, Sizeof(Sensorkvar^[1]) * Fnphases);
    AllocateSensorArrays;
end;

{==============================================================================}

procedure TSensorObj.ZeroSensorArrays;
var
    i: Integer;
begin
    for i := 1 to FnPhases do
    begin
        SensorCurrent^[i] := 0.0;
        SensorVoltage^[i] := 0.0;
        SensorkW^[i] := 0.0;
        Sensorkvar^[i] := 0.0;
    end;
end;

{==============================================================================}

procedure TSensorObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := ''; //'element';
    PropertyValue[2] := '1'; //'terminal';
    PropertyValue[3] := '12.47'; //'kVBase';
    PropertyValue[4] := 'No'; // Must be set to yes to clear before setting quantities
    PropertyValue[5] := '[7.2, 7.2, 7.2]';
    PropertyValue[6] := '[0.0, 0.0, 0.0]';  // currents
    PropertyValue[7] := '[0.0, 0.0, 0.0]';  // P kW
    PropertyValue[8] := '[0.0, 0.0, 0.0]';  // Q kvar
    PropertyValue[9] := 'wye';
    PropertyValue[10] := '1';
    PropertyValue[11] := '1';  // %Error
    PropertyValue[12] := '1';  // %Error
    PropertyValue[13] := '';   // Action


    inherited  InitPropertyValues(NumPropsThisClass);

end;


{==============================================================================}

function TSensorObj.LimitToPlusMinusOne(const i: Integer): Integer;
begin
    if i >= 0 then
        Result := 1
    else
        Result := -1;
end;

{--------------------------------------------------------------------------}

{ - function is not actually used
function TSensorObj.Get_FileName: String;
begin
        Result := GetOutputDirectory +  CircuitName_ + 'Sensor_' + Name + '.csv'
end;
}

{==============================================================================}

procedure TSensorObj.Save;
begin

end;


{==============================================================================}

procedure TSensorObj.Set_Conn(const Value: Integer);
{Interpret the Connection}
begin
    Fconn := Value;
    RecalcVbase;
end;

{==============================================================================}

procedure TSensorObj.Set_Action(const Value: String);
{Interpret Action Property}
begin

end;

{==============================================================================}

initialization
  //WriteDLLDebugFile('Sensor');

end.
