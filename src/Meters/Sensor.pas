unit Sensor;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

// Sensor compares voltages and currents. Power quantities are converted to current quantities
// based on rated kVBase, or actual voltage if voltage measurement specified.

interface

uses
    Command,
    MeterClass,
    Meterelement,
    DSSClass,
    Arraydef,
    UComplex, DSSUcomplex,
    utilities,
    Classes;

type
{$SCOPEDENUMS ON}
    TSensorProp = (
        INVALID = 0,
        element = 1,
        terminal = 2,
        kvbase = 3,
        clear = 4,
        kVs = 5,
        currents = 6,
        kWs = 7,
        kvars = 8,
        conn = 9,  //  Sensor connection
        Deltadirection = 10,  //  +/- 1
        pctError = 11,  //  %Error of sensor
        Weight = 12  // for WLS calc
        // action = 13  // unused
    );
{$SCOPEDENUMS OFF}


    TSensor = class(TMeterClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;

        procedure ResetAll; OVERRIDE;
        procedure SampleAll; OVERRIDE;  // Force all Sensors to take a sample
        procedure SaveAll; OVERRIDE;   // Force all Sensors to save their buffers to disk
        procedure SetHasSensorFlag;
    end;

    TSensorObj = class(TMeterElement)
    PRIVATE
        ValidSensor: Boolean;
        SensorkW: pDoubleArray;
        Sensorkvar: pDoubleArray;
        kVBase: Double; // value specified
        Vbase: Double; // in volts

        Vspecified,
        Ispecified,
        Pspecified,
        Qspecified: Boolean;

        ClearSpecified: LongBool;
        FDeltaDirection: Integer;

        //procedure Set_Action(const Value: String);
        procedure ZeroSensorArrays;
        procedure AllocateSensorObjArrays;

        function RotatePhases(const j: Integer): Integer;
        procedure ClearSensor;
        procedure UpdateCurrentVector;
        function Get_WLSCurrentError: Double;
        function Get_WLSVoltageError: Double;

    PUBLIC

        pctError,
        Weight: Double;
        FConn: Integer;
        procedure RecalcVbase;

        constructor Create(ParClass: TDSSClass; const SensorName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model, reset nphases
        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;    // Always Zero for a Sensor
        procedure TakeSample; OVERRIDE; // Go add a sample to the buffer
        procedure ResetIt;
        procedure Save;  // Saves present buffer to file

        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Get present value of terminal Curr

       {Properties to interpret input to the sensor}

        // property Action: String WRITE Set_Action;
        property WLSCurrentError: Double READ Get_WLSCurrentError;
        property WLSVoltageError: Double READ Get_WLSVoltageError;

        property BaseKV: Double READ kvbase;
        property DeltaDirection: Integer READ FDeltaDirection;
       // the following two properties actually give write access, since they are pointers
        property SensorP: pDoubleArray READ SensorKW;
        property SensorQ: pDoubleArray READ SensorKVAR;
    end;

implementation

uses
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
    DSSPointerList, {TOPExport,} Dynamics,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TSensorObj;
    TProp = TSensorProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TSensor.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, SENSOR_ELEMENT, 'Sensor');
end;

destructor TSensor.Destroy;
begin
    inherited Destroy;
end;

procedure TSensor.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // double arrays/vectors
    PropertyType[ord(TProp.kVs)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.kVs)] := ptruint(@obj.SensorVoltage);
    PropertyOffset2[ord(TProp.kVs)] := ptruint(@obj.Fnphases);

    PropertyType[ord(TProp.currents)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.currents)] := ptruint(@obj.SensorCurrent);
    PropertyOffset2[ord(TProp.currents)] := ptruint(@obj.Fnphases);

    PropertyType[ord(TProp.kWs)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.kWs)] := ptruint(@obj.SensorkW);
    PropertyOffset2[ord(TProp.kWs)] := ptruint(@obj.Fnphases);

    PropertyType[ord(TProp.kvars)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.kvars)] := ptruint(@obj.Sensorkvar);
    PropertyOffset2[ord(TProp.kvars)] := ptruint(@obj.Fnphases);

    // enum properties
    PropertyType[ord(TProp.conn)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.conn)] := ptruint(@obj.FConn);
    PropertyOffset2[ord(TProp.conn)] := PtrInt(DSS.ConnectionEnum);

    // object reference
    PropertyType[ord(TProp.element)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.element)] := ptruint(@obj.MeteredElement);
    PropertyOffset2[ord(TProp.element)] := 0;
    //PropertyFlags[ord(TProp.element)] := [TPropertyFlag.CheckForVar]; // not required for general cktelements

    // integer properties
    PropertyType[ord(TProp.terminal)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.terminal)] := ptruint(@obj.MeteredTerminal);

    PropertyType[ord(TProp.DeltaDirection)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.DeltaDirection)] := ptruint(@obj.FDeltaDirection);

    // boolean properties
    PropertyType[ord(TProp.clear)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.clear)] := ptruint(@obj.ClearSpecified);

    // double properties (default type)
    PropertyOffset[ord(TProp.kvbase)] := ptruint(@obj.kVBase);
    PropertyOffset[ord(TProp.pctError)] := ptruint(@obj.pctError);
    PropertyOffset[ord(TProp.Weight)] := ptruint(@obj.Weight);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TSensor.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TSensorObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of
        1..2:
        begin
            Include(Flags, Flg.NeedsRecalc);
            MeteredElementChanged := TRUE;
        end;
        3:
            Include(Flags, Flg.NeedsRecalc);

        // Do not recalc element data for setting of sensor quantities
        4:
            if ClearSpecified then
                ClearSensor;
        5:
            Vspecified := TRUE;
        6:
            Ispecified := TRUE;
        7:
        begin
            Pspecified := TRUE;
            UpdateCurrentVector;
        end;
        8:
        begin
            Qspecified := TRUE;
            UpdateCurrentVector;
        end;
        9:
        begin
            RecalcVbase;
            Include(Flags, Flg.NeedsRecalc);
        end;
        10:
        begin
            if FDeltaDirection >= 0 then
                FDeltaDirection := 1
            else
                FDeltaDirection := -1;

            Include(Flags, Flg.NeedsRecalc);
        end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TSensor.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        if Flg.NeedsRecalc in Flags then
            RecalcElementData;
        Exclude(Flags, Flg.EditionActive);
    end;
end;

procedure TSensor.ResetAll; // Force all Sensors in the circuit to reset
var
    pSensor: TSensorObj;
begin
    pSensor := ActiveCircuit.Sensors.First;
    while pSensor <> NIL do
    begin
        if pSensor.enabled then
            pSensor.ResetIt;
        pSensor := ActiveCircuit.Sensors.Next;
    end;
end;

procedure TSensor.SampleAll; // Force all Sensors in the circuit to take a sample
var
    pSensor: TSensorObj;
begin
    pSensor := ActiveCircuit.Sensors.First;
    while pSensor <> NIL do
    begin
        if pSensor.enabled then
            pSensor.TakeSample;
        pSensor := ActiveCircuit.Sensors.Next;
    end;
end;

procedure TSensor.SaveAll; // Force all Sensors in the circuit to save their buffers to disk
begin
end;

procedure TSensor.SetHasSensorFlag;
// Set the HasSensorObj Flag for all cktElement;
var
    i: Integer;
    ThisSensor: TSensorObj;
    CktElem: TDSSCktElement;
begin
    // Initialize all to FALSE
    with  ActiveCircuit do
    begin
        CktElem := PDElements.First;
        while CktElem <> NIL do
        begin
            Exclude(CktElem.Flags, Flg.HasSensorObj);
            CktElem := PDElements.Next;
        end;
        CktElem := PCElements.First;
        while CktElem <> NIL do
        begin
            Exclude(CktElem.Flags, Flg.HasSensorObj);
            CktElem := PCElements.Next;
        end;
    end;

    for i := 1 to ActiveCircuit.Sensors.Count do
    begin
        ThisSensor := ActiveCircuit.Sensors.Get(i);
        with ThisSensor do
            if MeteredElement <> NIL then
            begin
                Include(MeteredElement.Flags, Flg.HasSensorObj);
                if MeteredElement is TPCElement then
                    TPCElement(MeteredElement).SensorObj := ThisSensor
                else
                    TPDElement(MeteredElement).SensorObj := ThisSensor;
            end;
    end;
end;

procedure TSensorObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff

    MeteredElement := Other.MeteredElement;  // Pointer to target circuit element
    MeteredTerminal := Other.MeteredTerminal;

    BaseFrequency := Other.BaseFrequency;
end;

constructor TSensorObj.Create(ParClass: TDSSClass; const SensorName: String);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(SensorName);

    FNphases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class

    SensorkW := NIL;
    Sensorkvar := NIL;

    kVBase := 12.47; // default 3-phase voltage
    Weight := 1.0;
    pctError := 1.0;

    FConn := 0;  // Wye
    RecalcVbase();

    ClearSensor;

    DSSObjType := ParClass.DSSClassType; //SENSOR_ELEMENT;
    //  RecalcElementData;
end;

destructor TSensorObj.Destroy;
begin
    ReAllocMem(SensorkW, 0);
    ReAllocMem(Sensorkvar, 0);

    inherited Destroy;
end;

procedure TSensorObj.RecalcElementData;
begin
    Exclude(Flags, Flg.NeedsRecalc);
    ValidSensor := FALSE;
    if MeteredElement <> NIL then
    begin  // Sensored element must already exist
        if MeteredTerminal > MeteredElement.Nterms then
        begin
            DoErrorMsg(Format(_('Sensor: "%s"'), [Name]),
                Format(_('Terminal no. "%d" does not exist.'), [MeteredTerminal]),
                _('Respecify terminal no.'), 665);
        end
        else
        begin
            FNphases := MeteredElement.NPhases;
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
        Exit;
    end;

    // element not found/set
    DoErrorMsg(
        Format(_('Sensor: "%s"'), [Self.Name]), 
        _('Circuit Element is not set.'),
        _('Element must be defined previously.'), 666);
end;

procedure TSensorObj.MakePosSequence();
begin
    if MeteredElement <> NIL then
    begin
        Setbus(1, MeteredElement.GetBus(MeteredTerminal));
        FNphases := MeteredElement.NPhases;
        Nconds := MeteredElement.Nconds;
        ClearSensor;
        ValidSensor := TRUE;
        AllocateSensorObjArrays;
        ZeroSensorArrays;
        RecalcVbase;
    end;
    inherited;
end;

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

procedure TSensorObj.CalcYPrim;
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;

procedure TSensorObj.ResetIt;
{What does it mean to reset a sensor?}
begin
    ClearSensor;
end;

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

procedure TSensorObj.TakeSample;
var
    i: Integer;
begin
    if not (ValidSensor and Enabled) then
        Exit;

    MeteredElement.GetCurrents(CalculatedCurrent);
    ComputeVterminal;
    case Fconn of
        1:
            for i := 1 to Fnphases do
                CalculatedVoltage^[i] := VTerminal^[i] - VTerminal^[RotatePhases(i)];
    else
        for i := 1 to Fnphases do
            CalculatedVoltage^[i] := VTerminal^[i];
    end;
    // NOTE: CalculatedVoltage is complex
end;

procedure TSensorObj.GetCurrents(Curr: pComplexArray);  //Get present value of terminal Curr for reports
var
    i: Integer;
begin
    // Return array of zero
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

procedure TSensorObj.UpdateCurrentVector;
// Updates the currentvector when P and Q are defined
// as the input vectors for the sensor
var
    kVA: Double;
    i: Integer;
begin
    // Convert P and Q specification to Currents
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
// Return the WLS Error for Currents
// Get Square error and weight it
var
    kVA: Double;
    i: Integer;
begin
    Result := 0.0;
    // Convert P and Q specification to Currents
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

procedure TSensorObj.ClearSensor;
begin
    Vspecified := FALSE;
    Ispecified := FALSE;
    Pspecified := FALSE;
    Qspecified := FALSE;
    ClearSpecified := FALSE;
end;

procedure TSensorObj.AllocateSensorObjArrays;
begin
    ReAllocMem(SensorkW, Sizeof(SensorkW^[1]) * Fnphases);
    ReAllocMem(Sensorkvar, Sizeof(Sensorkvar^[1]) * Fnphases);
    AllocateSensorArrays;
end;

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

procedure TSensorObj.Save;
begin
end;

end.