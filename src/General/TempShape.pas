unit TempShape;

// ----------------------------------------------------------
// Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

// The Tshape object is a general DSS object used by all circuits
// as a reference for obtaining yearly, daily, and other Temperature shapes.
//
// The values are set by the normal New and Edit PROCEDUREs for any DSS object.
//
// The values may be retrieved by setting the Code Property in the Tshape Class.
// This sets the active Tshape object to be the one referenced by the Code Property;
//
// Then the values of that code can be retrieved via the public variables.  Or you
// can pick up the ActiveTShapeObj object and save the direct reference to the object.
//
// Tshapes default to fixed interval data (like Loadshapes).  If the Interval is specified to be 0.0,
// then both time and temperature data are expected.  If the Interval is  greater than 0.0,
// the user specifies only the Temperatures.  The Hour command is ignored and the files are
// assumed to contain only the temperature data.
//
// The Interval may also be specified in seconds (sinterval) or minutes (minterval).
//
// The user may place the data in CSV or binary files as well as passing through the
// command interface. Obviously, for large amounts of data such as 8760 load curves, the
// command interface is cumbersome.  CSV files are text separated by commas, one interval to a line.
// There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.
//
// For fixed interval data, only the Temperature values are expected.  Therefore, the CSV format would
// contain only one number per line.  The two binary formats are packed.
//
// For variable interval data, (hour, Temperature) pairs are expected in both formats.
//
// The Mean and Std Deviation are automatically computed upon demand when new series of points is entered.

uses
    Classes,
    Command,
    DSSClass,
    DSSObject,
    Arraydef;

type
{$SCOPEDENUMS ON}
    TTShapePropLegacy = (
        INVALID = 0,
        npts = 1, // Number of points to expect
        interval = 2, // default = 1.0;
        temp = 3, // vector of temperature values
        hour = 4, // vector of hour values
        mean = 5, // set the mean temp (otherwise computed)
        stddev = 6, // set the std dev of the temp (otherwise computed)
        csvfile = 7, // Switch input to a csvfile
        sngfile = 8, // switch input to a binary file of singles
        dblfile = 9, // switch input to a binary file of singles
        sinterval = 10, // Interval in seconds
        minterval = 11, // Interval in minutes
        action = 12 // Interval in minutes
    );
    TTShapeProp = (
        INVALID = 0,
        NPts = 1, // Number of points to expect
        Interval = 2, // default = 1.0;
        Temp = 3, // vector of temperature values
        Hour = 4, // vector of hour values
        Mean = 5, // set the mean temp (otherwise computed)
        StdDev = 6, // set the std dev of the temp (otherwise computed)
        CSVFile = 7, // Switch input to a csvfile
        SngFile = 8, // switch input to a binary file of singles
        DblFile = 9, // switch input to a binary file of singles
        SInterval = 10, // Interval in seconds
        MInterval = 11, // Interval in minutes
        Action = 12 // Interval in minutes
    );
{$SCOPEDENUMS OFF}

    TTShape = class(TDSSClass)
    PROTECTED
        procedure DefineProperties(); override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
        function Find(const ObjName: String; const ChangeActive: Boolean=True): Pointer; OVERRIDE;  // Find an obj of this class by name
    end;

    TTShapeObj = class(TDSSObject)
    PRIVATE

        FStdDevCalculated: Boolean;
        FMean,
        FStdDev: Double;

        procedure SaveToDblFile();
        procedure SaveToSngFile();
        procedure CalcMeanandStdDev();
    PUBLIC
        numPoints: Integer;  // Number of points in curve

        Interval: Double;  //=0.0 then random interval     (hr)
        Hours,          // Time values (hr) if Interval > 0.0  Else nil
        TValues: pDoubleArray;  // Temperatures
        csvfile, dblfile, sngfile: String;

        constructor Create(ParClass: TDSSClass; const TShapeName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        function GetTemperatureAtHour(Hr: Double): Double;  // Get Temperatures at specified time, hr

        function GetMean(): Double;
        function GetStdDev(): Double;
        procedure SetMean(const Value: Double);
        procedure SetStdDev(const Value: Double);  // Normalize the curve presently in memory
    end;

implementation

uses
    BufStream,
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    MathUtil,
    Utilities,
    Math,
    DSSPointerList,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TTShapeObj;
    TProp = TTShapeProp;
    TPropLegacy = TTShapePropLegacy;
{$PUSH}
{$Z4} // keep enums as int32 values
    TTShapeAction = (
        DblSave = 0,
        SngSave = 1
    );
{$POP}
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;    
    ActionEnum: TDSSEnum;

constructor TTShape.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
        ActionEnum := TDSSEnum.Create('TShape: Action', True, 1, 1, 
            ['DblSave', 'SngSave'], 
            [ord(TTShapeAction.DblSave), ord(TTShapeAction.SngSave)]);
    end;
    inherited Create(dssContext, DSS_OBJECT, 'TShape');
end;

destructor TTShape.Destroy;
begin
    inherited Destroy;
end;

procedure DoAction(obj: TObj; action: TTShapeAction);
begin
    case action of
        TTShapeAction.DblSave:
            obj.SaveToDblFile;
        TTShapeAction.SngSave:
            obj.SaveToSngFile;
    end;
end;

function GetMean(obj: TObj): Double;
begin
    Result := obj.GetMean();
end;

procedure SetMean(obj: TObj; value: Double);
begin
    obj.SetMean(value);
end;

function GetStdDev(obj: TObj): Double;
begin
    Result := obj.GetStdDev();
end;

procedure SetStdDev(obj: TObj; value: Double);
begin
    obj.SetStdDev(value);
end;

procedure TTShape.DefineProperties();
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    PropertyStructArrayCountOffset := ptruint(@obj.numPoints);

    SpecSetNames := ArrayOfString.Create(
        'Temp, Hour',
        'Temp, Interval',
        'CSVFile',
        'SngFile',
        'DblFile'
    );
    SpecSets := TSpecSets.Create(
        TSpecSet.Create(ord(TProp.Temp), ord(TProp.Hour)),
        TSpecSet.Create(ord(TProp.Temp), ord(TProp.Interval)),
        TSpecSet.Create(ord(TProp.CSVFile)),
        TSpecSet.Create(ord(TProp.SngFile)),
        TSpecSet.Create(ord(TProp.DblFile))
    );

    // doubles
    PropertyOffset[ord(TProp.interval)] := ptruint(@obj.Interval);
    PropertyFlags[ord(TProp.interval)] := [TPropertyFlag.RequiredInSpecSet, TPropertyFlag.NonNegative];

    PropertyOffset[ord(TProp.mean)] := ptruint(@obj.FMean);
    PropertyReadFunction[ord(TProp.mean)] := @GetMean;
    PropertyWriteFunction[ord(TProp.mean)] := @SetMean;
    PropertyFlags[ord(TProp.mean)] := [TPropertyFlag.ReadByFunction, TPropertyFlag.DynamicDefault, TPropertyFlag.WriteByFunction];
    
    PropertyOffset[ord(TProp.stddev)] := ptruint(@obj.FStdDev);
    PropertyReadFunction[ord(TProp.stddev)] := @GetStdDev;
    PropertyWriteFunction[ord(TProp.stddev)] := @SetStdDev;
    PropertyFlags[ord(TProp.stddev)] := [TPropertyFlag.ReadByFunction, TPropertyFlag.DynamicDefault, TPropertyFlag.WriteByFunction];

    // advanced doubles
    PropertyOffset[ord(TProp.sinterval)] := ptruint(@obj.Interval);
    PropertyScale[ord(TProp.sinterval)] := 1 / 3600.0;
    PropertyFlags[ord(TProp.sinterval)] := [TPropertyFlag.Redundant, TPropertyFlag.NonNegative];
    PropertyRedundantWith[ord(TProp.sinterval)] := ord(TProp.interval);

    PropertyOffset[ord(TProp.minterval)] := ptruint(@obj.Interval);
    PropertyScale[ord(TProp.minterval)] := 1 / 60.0;
    PropertyFlags[ord(TProp.minterval)] := [TPropertyFlag.Redundant, TPropertyFlag.NonNegative];
    PropertyRedundantWith[ord(TProp.minterval)] := ord(TProp.interval);

    // strings
    PropertyType[ord(TProp.csvfile)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.csvfile)] := ptruint(@obj.csvfile);
    PropertyFlags[ord(TProp.csvfile)] := [TPropertyFlag.IsFilename, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.GlobalCount];

    PropertyType[ord(TProp.dblfile)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.dblfile)] := ptruint(@obj.dblfile);
    PropertyFlags[ord(TProp.dblfile)] := [TPropertyFlag.IsFilename, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.GlobalCount];

    PropertyType[ord(TProp.sngfile)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.sngfile)] := ptruint(@obj.sngfile);
    PropertyFlags[ord(TProp.sngfile)] := [TPropertyFlag.IsFilename, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.GlobalCount];

    // integer
    PropertyType[ord(TProp.Npts)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Npts)] := ptruint(@obj.numPoints);
    PropertyFlags[ord(TProp.Npts)] := [TPropertyFlag.SuppressJSON];

    // double arrays
    PropertyType[ord(TProp.hour)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.hour)] := ptruint(@obj.Hours);
    PropertyOffset2[ord(TProp.hour)] := ptruint(@obj.numPoints);
    PropertyFlags[ord(TProp.hour)] := [TPropertyFlag.RequiredInSpecSet];

    PropertyType[ord(TProp.temp)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.temp)] := ptruint(@obj.TValues);
    PropertyOffset2[ord(TProp.temp)] := ptruint(@obj.numPoints);
    PropertyFlags[ord(TProp.temp)] := [TPropertyFlag.RequiredInSpecSet];

    // enum action
    PropertyType[ord(TProp.Action)] := TPropertyType.StringEnumActionProperty;
    PropertyOffset[ord(TProp.Action)] := ptruint(@DoAction); 
    PropertyOffset2[ord(TProp.Action)] := PtrInt(ActionEnum); 

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties();
end;

function TTShape.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    obj: TObj;
begin
    obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := obj;
    obj.ClassIndex := AddObjectToList(obj, Activate);
    Result := obj;
end;

procedure TTShapeObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
begin
    case Idx of 
        ord(TProp.csvfile):
            DoCSVFile(DSS, Hours, TValues, numPoints, (Interval <> 0.0), csvfile, ParentClass.Name);
        ord(TProp.sngfile):
            DoSngFile(DSS, Hours, TValues, numPoints, (Interval <> 0.0), sngfile, ParentClass.Name);
        ord(TProp.dblfile):
            DoDblFile(DSS, Hours, TValues, numPoints, (Interval <> 0.0), dblfile, ParentClass.Name);
    end;
    case Idx of
        ord(TProp.npts):
            // Force as the always first property when saving in a later point
            PrpSequence[Idx] := -10;
        3, 7, 8, 9:
        begin
            FStdDevCalculated := FALSE;   // now calculated on demand
            PropertySideEffects(ord(TProp.npts), 0, setterFlags);
        end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

function TTShape.Find(const ObjName: String; const ChangeActive: Boolean): Pointer;
begin
    if (Length(ObjName) = 0) or (CompareText(ObjName, 'none') = 0) then
        Result := NIL
    else
        Result := inherited Find(ObjName, ChangeActive);
end;

procedure TTShapeObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    numPoints := Other.numPoints;
    Interval := Other.Interval;
    ReallocMem(TValues, SizeOf(TValues[1]) * numPoints);
    for i := 1 to numPoints do
        TValues[i] := Other.TValues[i];
    if Interval > 0.0 then
        ReallocMem(Hours, 0)
    else
    begin
        ReallocMem(Hours, SizeOf(Hours[1]) * numPoints);
        for i := 1 to numPoints do
            Hours[i] := Other.Hours[i];
    end;
end;

constructor TTShapeObj.Create(ParClass: TDSSClass; const TShapeName: String);
begin
    inherited Create(ParClass, TShapeName);
    DSSObjType := ParClass.DSSClassType;

    numPoints := 0;
    Interval := 1.0;  // hr
    Hours := NIL;
    TValues := NIL;
    FStdDevCalculated := FALSE;  // calculate on demand

    csvfile := '';
    dblfile := '';
    sngfile := '';
end;

destructor TTShapeObj.Destroy;
begin
    ReallocMem(Hours, 0);
    if Assigned(TValues) then
        ReallocMem(TValues, 0);
    inherited destroy;
end;

function TTShapeObj.GetTemperatureAtHour(Hr: Double): Double;
// This FUNCTION returns the Temperature for the given hour.
// If no points exist in the curve, the result is  0.0
// If there are fewer points than requested, the curve is simply assumed to repeat
// Thus a daily load curve can suffice for a yearly load curve:  You just get the
// same day over and over again.
// The value returned is the nearest to the interval requested.  Thus if you request
// hour=12.25 and the interval is 1.0, you will get interval 12.
var
    Index, i: Integer;
begin
    Result := 0.0;    // default return value if no points in curve

    if numPoints <= 0 then         // Handle Exceptional cases
        Exit;

    if numPoints = 1 then
    begin
        Result := TValues[1];
        Exit;
    end;

    if Interval > 0.0 then
    begin
        Index := round(Hr / Interval);
        if Index > numPoints then
            Index := Index mod numPoints;  // Wrap around using remainder
        if Index = 0 then
            Index := numPoints;
        Result := TValues[Index];
        Exit;
    end;

    // For random interval

    // Normalize Hr to max hour in curve to get wraparound
    if (Hr > Hours[numPoints]) then
    begin
        Hr := Hr - Trunc(Hr / Hours[numPoints]) * Hours[numPoints];
    end;

    for i := 1 to numPoints do
    begin
        if (Abs(Hours[i] - Hr) < 0.00001) then  // If close to an actual point, just use it.
        begin
            Result := TValues[i];
            Exit;
        end
        else
        if (Hours[i] > Hr) then      // Interpolate for temperature
        begin
            Result := TValues[i - 1] +
                (Hr - Hours[i - 1]) / (Hours[i] - Hours[i - 1]) *
                (TValues[i] - TValues[i - 1]);
            Exit;
        end;
    end;
    // If we fall through the loop, just use last value
    Result := TValues[numPoints];
end;

procedure TTShapeObj.CalcMeanandStdDev();
begin
    if numPoints > 0 then
        if Interval > 0.0 then
            RCDMeanandStdDev(TValues, numPoints, FMean, FStdDev)
        else
            CurveMeanAndStdDev(TValues, Hours, numPoints, FMean, FStdDev);

    FStdDevCalculated := TRUE;
end;

function TTShapeObj.GetMean(): Double;
begin
    if not FStdDevCalculated then
        CalcMeanandStdDev;
    Result := FMean;
end;

function TTShapeObj.GetStdDev(): Double;
begin
    if not FStdDevCalculated then
        CalcMeanandStdDev;
    Result := FStdDev;
end;

procedure TTShapeObj.SaveToDblFile();
var
    F: TStream = nil;
    Fname: String;
begin
    if not Assigned(TValues) then
    begin
        DoSimpleMsg('%s Temperatures not defined.', [FullName()], 57622);
        Exit;
    end;
    try
        FName := DSS.OutputDirectory + Format('%s.dbl', [Name]); // CurrentDSSDir
        F := DSS.GetOutputStreamEx(FName, fmCreate);
        F.WriteBuffer(TValues[1], numPoints * SizeOf(Double));
        DSS.GlobalResult := 'Temp=[dblfile=' + FName + ']';
    finally
        FreeAndNil(F);
    end;
end;

procedure TTShapeObj.SaveToSngFile();
var
    F: TStream = nil;
    i: Integer;
    Fname: String;
    Temp: Single;
begin
    if not Assigned(TValues) then
    begin
        DoSimpleMsg('%s Temperatures not defined.', [FullName()], 57623);
        Exit;
    end;
    try
        FName := DSS.OutputDirectory + Format('%s.sng', [Name]); // CurrentDSSDir
        F := DSS.GetOutputStreamEx(FName, fmCreate);
        for i := 1 to numPoints do
        begin
            Temp := TValues[i];
            F.WriteBuffer(Temp, SizeOf(Temp));
        end;
        DSS.GlobalResult := 'Temp=[sngfile=' + FName + ']';
    finally
        FreeAndNil(F);
    end;
end;

procedure TTShapeObj.SetMean(const Value: Double);
begin
    FStdDevCalculated := TRUE;
    FMean := Value;
end;

procedure TTShapeObj.SetStdDev(const Value: Double);
begin
    FStdDevCalculated := TRUE;
    FStdDev := Value;
end;

finalization
    ActionEnum.Free;        
end.
