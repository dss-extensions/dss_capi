unit TempShape;

{
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

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
    TTShapeProp = (
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
{$SCOPEDENUMS OFF}

    TTShape = class(TDSSClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
        function Find(const ObjName: String; const ChangeActive: Boolean=True): Pointer; OVERRIDE;  // Find an obj of this class by name
    end;

    TTShapeObj = class(TDSSObject)
    PRIVATE
        LastValueAccessed,
        FNumPoints: Integer;  // Number of points in curve

        FStdDevCalculated: Boolean;
        FMean,
        FStdDev: Double;

        function Get_Interval: Double;
        procedure SaveToDblFile;
        procedure SaveToSngFile;
        procedure CalcMeanandStdDev;
        function Get_Mean: Double;
        function Get_StdDev: Double;
        procedure Set_Mean(const Value: Double);
        procedure Set_StdDev(const Value: Double);  // Normalize the curve presently in memory

    PUBLIC
        Interval: Double;  //=0.0 then random interval     (hr)
        Hours,          // Time values (hr) if Interval > 0.0  Else nil
        TValues: pDoubleArray;  // Temperatures
        csvfile, dblfile, sngfile: String;

        constructor Create(ParClass: TDSSClass; const TShapeName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        function GetTemperature(hr: Double): Double;  // Get Temperatures at specified time, hr
        function Temperature(i: Integer): Double;  // get Temperatures by index
        function Hour(i: Integer): Double;  // get hour corresponding to point index

        property NumPoints: Integer READ FNumPoints;
        property PresentInterval: Double READ Get_Interval;
        property Mean: Double READ Get_Mean WRITE Set_Mean;
        property StdDev: Double READ Get_StdDev WRITE Set_StdDev;
    end;

implementation

uses
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
    PropInfo: Pointer;    
    ActionEnum: TDSSEnum;

constructor TTShape.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
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

procedure DoAction(Obj: TObj; action: TTShapeAction);
begin
    case action of
        TTShapeAction.DblSave:
            Obj.SaveToDblFile;
        TTShapeAction.SngSave:
            Obj.SaveToSngFile;
    end;
end;

function GetMean(obj: TObj): Double;
begin
    Result := obj.Mean;
end;

function GetStdDev(obj: TObj): Double;
begin
    Result := obj.StdDev;
end;

procedure TTShape.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // doubles
    PropertyOffset[ord(TProp.interval)] := ptruint(@obj.Interval);

    PropertyOffset[ord(TProp.mean)] := ptruint(@obj.FMean);
    PropertyReadFunction[ord(TProp.mean)] := @GetMean;
    PropertyFlags[ord(TProp.mean)] := [TPropertyFlag.ReadByFunction];
    
    PropertyOffset[ord(TProp.stddev)] := ptruint(@obj.FStdDev);
    PropertyReadFunction[ord(TProp.stddev)] := @GetStdDev;
    PropertyFlags[ord(TProp.stddev)] := [TPropertyFlag.ReadByFunction];

    // advanced doubles
    PropertyOffset[ord(TProp.sinterval)] := ptruint(@obj.Interval);
    PropertyScale[ord(TProp.sinterval)] := 1 / 3600.0;
    PropertyFlags[ord(TProp.sinterval)] := [TPropertyFlag.Redundant];

    PropertyOffset[ord(TProp.minterval)] := ptruint(@obj.Interval);
    PropertyScale[ord(TProp.minterval)] := 1 / 60.0;
    PropertyFlags[ord(TProp.minterval)] := [TPropertyFlag.Redundant];

    // strings
    PropertyType[ord(TProp.csvfile)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.csvfile)] := ptruint(@obj.csvfile);
    PropertyFlags[ord(TProp.csvfile)] := [TPropertyFlag.IsFilename];

    PropertyType[ord(TProp.dblfile)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.dblfile)] := ptruint(@obj.dblfile);
    PropertyFlags[ord(TProp.dblfile)] := [TPropertyFlag.IsFilename];

    PropertyType[ord(TProp.sngfile)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.sngfile)] := ptruint(@obj.sngfile);
    PropertyFlags[ord(TProp.sngfile)] := [TPropertyFlag.IsFilename];

    // integer
    PropertyType[ord(TProp.Npts)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Npts)] := ptruint(@obj.FNumPoints);

    // double arrays
    PropertyType[ord(TProp.hour)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.hour)] := ptruint(@obj.Hours);
    PropertyOffset2[ord(TProp.hour)] := ptruint(@obj.FNumPoints);

    PropertyType[ord(TProp.temp)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.temp)] := ptruint(@obj.TValues);
    PropertyOffset2[ord(TProp.temp)] := ptruint(@obj.FNumPoints);

    // enum action
    PropertyType[ord(TProp.Action)] := TPropertyType.StringEnumActionProperty;
    PropertyOffset[ord(TProp.Action)] := ptruint(@DoAction); 
    PropertyOffset2[ord(TProp.Action)] := PtrInt(ActionEnum); 

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TTShape.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TTShapeObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of 
        ord(TProp.csvfile):
            DoCSVFile(DSS, Hours, TValues, FNumPoints, (Interval <> 0.0), csvfile, ParentClass.Name);
        ord(TProp.sngfile):
            DoSngFile(DSS, Hours, TValues, FNumPoints, (Interval <> 0.0), sngfile, ParentClass.Name);
        ord(TProp.dblfile):
            DoDblFile(DSS, Hours, TValues, FNumPoints, (Interval <> 0.0), dblfile, ParentClass.Name);
    end;
    case Idx of
        ord(TProp.npts):
            // Force as the always first property when saving in a later point
            PrpSequence[Idx] := -10;
        3, 7, 8, 9:
        begin
            FStdDevCalculated := FALSE;   // now calculated on demand
            PropertySideEffects(ord(TProp.npts));
        end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
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
    FNumPoints := Other.NumPoints;
    Interval := Other.Interval;
    ReallocMem(TValues, SizeOf(TValues^[1]) * NumPoints);
    for i := 1 to NumPoints do
        TValues^[i] := Other.TValues^[i];
    if Interval > 0.0 then
        ReallocMem(Hours, 0)
    else
    begin
        ReallocMem(Hours, SizeOf(Hours^[1]) * NumPoints);
        for i := 1 to NumPoints do
            Hours^[i] := Other.Hours^[i];
    end;
end;

constructor TTShapeObj.Create(ParClass: TDSSClass; const TShapeName: String);
begin
    inherited Create(ParClass);
    Name := LowerCase(TShapeName);
    DSSObjType := ParClass.DSSClassType;

    LastValueAccessed := 1;

    FNumPoints := 0;
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

function TTShapeObj.GetTemperature(hr: Double): Double;
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

    if FNumPoints <= 0 then         // Handle Exceptional cases
        Exit;

    if FNumPoints = 1 then
    begin
        Result := TValues^[1];
        Exit;
    end;

    if Interval > 0.0 then
    begin
        Index := round(hr / Interval);
        if Index > FNumPoints then
            Index := Index mod FNumPoints;  // Wrap around using remainder
        if Index = 0 then
            Index := FNumPoints;
        Result := TValues^[Index];
        Exit;
    end;

    // For random interval

    // Start with previous value accessed under the assumption that most
    // of the time, this FUNCTION will be called sequentially

    // Normalize Hr to max hour in curve to get wraparound
    if (Hr > Hours^[FNumPoints]) then
    begin
        Hr := Hr - Trunc(Hr / Hours^[FNumPoints]) * Hours^[FNumPoints];
    end;

    if (Hours^[LastValueAccessed] > Hr) then
        LastValueAccessed := 1;  // Start over from Beginning
    for i := LastValueAccessed + 1 to FNumPoints do
    begin
        if (Abs(Hours^[i] - Hr) < 0.00001) then  // If close to an actual point, just use it.
        begin
            Result := TValues^[i];
            LastValueAccessed := i;
            Exit;
        end
        else
        if (Hours^[i] > Hr) then      // Interpolate for temperature
        begin
            LastValueAccessed := i - 1;
            Result := TValues^[LastValueAccessed] +
                (Hr - Hours^[LastValueAccessed]) / (Hours^[i] - Hours^[LastValueAccessed]) *
                (TValues^[i] - TValues^[LastValueAccessed]);
            Exit;
        end;
    end;
    // If we fall through the loop, just use last value
    LastValueAccessed := FNumPoints - 1;
    Result := TValues^[FNumPoints];
end;

procedure TTShapeObj.CalcMeanandStdDev;
begin
    if FNumPoints > 0 then
        if Interval > 0.0 then
            RCDMeanandStdDev(TValues, FNumPoints, FMean, FStdDev)
        else
            CurveMeanAndStdDev(TValues, Hours, FNumPoints, FMean, FStdDev);

    FStdDevCalculated := TRUE;
end;

function TTShapeObj.Get_Interval: Double;
begin
    if Interval > 0.0 then
        Result := Interval
    else
    begin
        if LastValueAccessed > 1 then
            Result := Hours^[LastValueAccessed] - Hours^[LastValueAccessed - 1]
        else
            Result := 0.0;
    end;
end;

function TTShapeObj.Get_Mean: Double;
begin
    if not FStdDevCalculated then
        CalcMeanandStdDev;
    Result := FMean;
end;

function TTShapeObj.Get_StdDev: Double;
begin
    if not FStdDevCalculated then
        CalcMeanandStdDev;
    Result := FStdDev;
end;

function TTShapeObj.Temperature(i: Integer): Double;
begin
    if (i <= FNumPoints) and (i > 0) then
    begin
        Result := TValues^[i];
        LastValueAccessed := i;
    end
    else
        Result := 0.0;
end;

function TTShapeObj.Hour(i: Integer): Double;
begin
    if Interval = 0 then
    begin
        if (i <= FNumPoints) and (i > 0) then
        begin
            Result := Hours^[i];
            LastValueAccessed := i;
        end
        else
            Result := 0.0;
    end
    else
    begin
        Result := Hours^[i] * Interval;
        LastValueAccessed := i;
    end;
end;

procedure TTShapeObj.SaveToDblFile;
var
    F: TFileStream = nil;
    Fname: String;
begin
    if not Assigned(TValues) then
    begin
        DoSimpleMsg('%s Temperatures not defined.', [FullName], 57622);
        Exit;
    end;
    try
        FName := DSS.OutputDirectory {CurrentDSSDir} + Format('%s.dbl', [Name]);
        F := TFileStream.Create(FName, fmCreate);
        F.WriteBuffer(TValues^[1], NumPoints * SizeOf(Double));
        DSS.GlobalResult := 'Temp=[dblfile=' + FName + ']';
    finally
        FreeAndNil(F);
    end;
end;

procedure TTShapeObj.SaveToSngFile;
var
    F: TFileStream = nil;
    i: Integer;
    Fname: String;
    Temp: Single;
begin
    if not Assigned(TValues) then
    begin
        DoSimpleMsg('%s Temperatures not defined.', [FullName], 57623);
        Exit;
    end;
    try
        FName := DSS.OutputDirectory {CurrentDSSDir} + Format('%s.sng', [Name]);
        F := TFileStream.Create(FName, fmCreate);
        for i := 1 to NumPoints do
        begin
            Temp := TValues^[i];
            F.WriteBuffer(Temp, SizeOf(Temp));
        end;
        DSS.GlobalResult := 'Temp=[sngfile=' + FName + ']';
    finally
        FreeAndNil(F);
    end;
end;

procedure TTShapeObj.Set_Mean(const Value: Double);
begin
    FStdDevCalculated := TRUE;
    FMean := Value;
end;

procedure TTShapeObj.Set_StdDev(const Value: Double);
begin
    FStdDevCalculated := TRUE;
    FStdDev := Value;
end;

initialization
    PropInfo := NIL;
finalization
    ActionEnum.Free;        
end.
