unit XYcurve;

// ----------------------------------------------------------
// Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

// The XYcurve object is a general DSS object used by all circuit elements
// as a reference for obtaining yearly, daily, and other Temperature shapes.
//
// The values are set by the normal New and Edit PROCEDUREs for any DSS object.
//
// The values may be retrieved by setting the Code Property in the XYCurve Class.
// This sets the active XYCurve object to be the one referenced by the Code Property;
//
// Then the values of that code can be retrieved via the public variables.  Or you
// can pick up the ActiveTXYcurveObj object and save the direct reference to the object.
//
// The user may place the curve data in CSV or binary files as well as passing through the
// command interface. Obviously, for large amounts of data such as 8760 load curves, the
// command interface is cumbersome.  CSV files are text separated by commas, or white space
// one point to a line.
//
// There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.

uses
    Classes,
    Command,
    DSSClass,
    DSSObject,
    Arraydef;

type
{$SCOPEDENUMS ON}
    TXYcurvePropLegacy = (
        INVALID = 0,
        npts = 1, // Number of points to expect
        Points = 2,
        Yarray = 3, // vector of Y values
        Xarray = 4, // vector of X values corresponding to Y values
        csvfile = 5, // Switch input to a csvfile
        sngfile = 6, // switch input to a binary file of singles
        dblfile = 7, // switch input to a binary file of singles
        x = 8,
        y = 9,
        Xshift = 10,
        Yshift = 11,
        Xscale = 12,
        Yscale = 13 
    );
    TXYcurveProp = (
        INVALID = 0,
        NPts = 1, // Number of points to expect
        Points = 2,
        YArray = 3, // vector of Y values
        XArray = 4, // vector of X values corresponding to Y values
        CSVFile = 5, // Switch input to a csvfile
        SngFile = 6, // switch input to a binary file of singles
        DblFile = 7, // switch input to a binary file of singles
        X = 8,
        Y = 9,
        XShift = 10,
        YShift = 11,
        XScale = 12,
        YScale = 13 
    );
{$SCOPEDENUMS OFF}

    TCoeff = array[1..2] of Double;

    TXYcurve = class(TDSSClass)
    PROTECTED
        procedure DefineProperties(); override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
        function Find(const ObjName: String; const ChangeActive: Boolean=True): Pointer; OVERRIDE;  // Find an obj of this class by name
    end;

    TXYcurveObj = class(TDSSObject)
    PUBLIC
        XValues,
        YValues: pDoubleArray;
    PRIVATE
        FX,
        FY: Double;

        function InterpolatePoints(i, j: Integer; X: Double; Xarray, Yarray: pDoubleArray): Double;
    PUBLIC
        numPoints: Integer;  // Number of points in curve

        FXshift,
        FYshift,
        FXscale,
        FYscale: Double;
        
        csvfile, dblfile, sngfile: String;

        constructor Create(ParClass: TDSSClass; const XYCurveName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        function GetYValue(X: Double): Double;  // Get Y value at specified X Value
        function GetXValue(Y: Double): Double;  // Get X value at specified Y Value
        function GetCoefficients(X: Double): TCoeff;

        function YValue_pt(i: Integer): Double;  // get Y Value by index
        function XValue_pt(i: Integer): Double;  // get X Value corresponding to point index

        function GetX(): Double;
        function GetY(): Double;
        procedure SetX(Value: Double);
        procedure SetY(Value: Double);
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
    TypInfo,
    CAPI_Types,
    CAPI_Utils;

type
    TObj = TXYcurveObj;
    TProp = TXYcurveProp;
    TPropLegacy = TXYcurvePropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;    

constructor TXYcurve.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
    end;

    inherited Create(dssContext, DSS_OBJECT, 'XYcurve');
end;

destructor TXYcurve.Destroy;
begin
    inherited Destroy;
end;

procedure SetX(Obj: TObj; Value: Double);
begin
    Obj.SetX(Value);
end;

procedure SetY(Obj: TObj; Value: Double);
begin
    Obj.SetY(Value);
end;

function GetX(Obj: TObj): Double;
begin
    Result := Obj.GetX();
end;

function GetY(Obj: TObj): Double;
begin
    Result := Obj.GetY();
end;

function Get2xNumPoints(Obj: TObj): Integer;
begin
    Result := Obj.numPoints * 2;
end;

procedure SetPoints(obj: TObj; Values: PDouble; ValueCount: Integer);
var
    i: Integer;
begin
    // Allow possible Resetting (to a lower value) of num points when specifying temperatures not Hours
    obj.numPoints := ValueCount div 2;
    ReAllocmem(obj.YValues, Sizeof(Double) * obj.numPoints);
    ReAllocmem(obj.XValues, Sizeof(Double) * obj.numPoints);
    for i := 1 to obj.numPoints do
    begin
        obj.XValues[i] := Values^;
        Inc(Values);
        obj.YValues[i] := Values^;
        Inc(Values);
    end;
    obj.SetX(obj.Xvalues[1]);
    obj.SetY(obj.Yvalues[1]);
end;

procedure GetPoints(obj: TObj; var ResultPtr: PDouble; ResultCount: PAPISize);
var
    i: Integer;
    Result: PDoubleArray0;
begin
    if (obj.XValues <> NIL) and (obj.YValues <> NIL) then
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, obj.numPoints * 2);
        for i := 1 to obj.numPoints do
        begin
            Result[2 * (i - 1)] := obj.XValues[i];
            Result[2 * (i - 1) + 1] := obj.YValues[i];
        end;
        Exit;
    end;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
    Result[0] := 0;
    Result[1] := 0;
end;

procedure TXYcurve.DefineProperties();
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    PropertyStructArrayCountOffset := ptruint(@obj.numPoints);

    SpecSetNames := ArrayOfString.Create(
        'Xarray, Yarray',
        'Points',
        'CSVFile',
        'SngFile',
        'DblFile'
    );
    SpecSets := TSpecSets.Create(
        TSpecSet.Create(ord(TProp.Xarray), ord(TProp.Yarray)),
        TSpecSet.Create(ord(TProp.Points)),
        TSpecSet.Create(ord(TProp.CSVFile)),
        TSpecSet.Create(ord(TProp.SngFile)),
        TSpecSet.Create(ord(TProp.DblFile))
    );

    // integer properties
    PropertyType[ord(TProp.Npts)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Npts)] := ptruint(@obj.numPoints);
    PropertyFlags[ord(TProp.Npts)] := [TPropertyFlag.SuppressJSON];
          
    // double arrays
    PropertyType[ord(TProp.Xarray)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.Xarray)] := ptruint(@obj.XValues);
    PropertyOffset2[ord(TProp.Xarray)] := ptruint(@obj.numPoints);
    PropertyFlags[ord(TProp.Xarray)] := [TPropertyFlag.RequiredInSpecSet];

    PropertyType[ord(TProp.Yarray)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.Yarray)] := ptruint(@obj.YValues);
    PropertyOffset2[ord(TProp.Yarray)] := ptruint(@obj.numPoints);
    PropertyFlags[ord(TProp.Yarray)] := [TPropertyFlag.RequiredInSpecSet];

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

    // doubles
    PropertyOffset[ord(TProp.Xshift)] := ptruint(@obj.FXshift);
    PropertyOffset[ord(TProp.Yshift)] := ptruint(@obj.FYshift);
    PropertyOffset[ord(TProp.Xscale)] := ptruint(@obj.FXscale);
    PropertyOffset[ord(TProp.Yscale)] := ptruint(@obj.FYscale);

    // doubles with setter and getters
    PropertyType[ord(TProp.X)] := TPropertyType.DoubleProperty;
    PropertyOffset[ord(TProp.X)] := 1; // dummy
    PropertyWriteFunction[ord(TProp.X)] := @SetX;
    PropertyReadFunction[ord(TProp.X)] := @GetX;
    PropertyFlags[ord(TProp.X)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction, TPropertyFlag.Util, TPropertyFlag.SuppressJSON];
    
    PropertyType[ord(TProp.Y)] := TPropertyType.DoubleProperty;
    PropertyOffset[ord(TProp.Y)] := 1; // dummy
    PropertyWriteFunction[ord(TProp.Y)] := @SetY;
    PropertyReadFunction[ord(TProp.Y)] := @GetY;
    PropertyFlags[ord(TProp.Y)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction, TPropertyFlag.Util, TPropertyFlag.SuppressJSON];

    // ...and just mark Points as custom
    PropertyType[ord(TProp.Points)] := TPropertyType.DoubleDArrayProperty;
    PropertyOffset[ord(TProp.Points)] := 1; // dummy
    PropertyWriteFunction[ord(TProp.Points)] := @SetPoints;
    PropertyReadFunction[ord(TProp.Points)] := @GetPoints;
    PropertyOffset3[ord(TProp.Points)] := ptruint(@Get2xNumPoints);
    PropertyFlags[ord(TProp.Points)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction, TPropertyFlag.SizeIsFunction, TPropertyFlag.Redundant, TPropertyFlag.RequiredInSpecSet];
    PropertyRedundantWith[ord(TProp.Points)] := ord(TProp.Xarray);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties();
end;

function TXYcurve.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TXYcurveObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
begin
    case Idx of
        ord(TProp.csvfile):
            DoCSVFile(DSS, Xvalues, Yvalues, numPoints, False, csvfile, ParentClass.Name); // file of x,y points, one to a line
        ord(TProp.sngfile):
            DoSngFile(DSS, Xvalues, Yvalues, numPoints, False, sngfile, ParentClass.Name);
        ord(TProp.dblfile):
            DoDblFile(DSS, Xvalues, Yvalues, numPoints, False, dblfile, ParentClass.Name);
    end;

    case Idx of
        ord(TProp.npts):
        begin
            // Force as the always first property when saving in a later point
            PrpSequence[Idx] := -10;
            ReAllocmem(YValues, Sizeof(YValues[1]) * numPoints);
            ReAllocmem(XValues, Sizeof(XValues[1]) * numPoints);
        end;
        ord(TProp.Yarray):
            if (YValues <> NIL) then
                SetY(YValues[1]);
        ord(TProp.Xarray):
            if (XValues <> NIL) then
                SetX(XValues[1]);
        ord(TProp.csvfile), ord(TProp.sngfile), ord(TProp.dblfile): 
        begin
            if (XValues <> NIL) then
                SetX(XValues[1]);
            if (YValues <> NIL) then
                SetY(YValues[1]);
        end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

function TXYcurve.Find(const ObjName: String; const ChangeActive: Boolean): Pointer;
begin
    if (Length(ObjName) = 0) or (CompareText(ObjName, 'none') = 0) then
        Result := NIL
    else
        Result := inherited Find(ObjName, ChangeActive);
end;

procedure TXYcurveObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    numPoints := Other.numPoints;
    ReAllocmem(XValues, Sizeof(XValues[1]) * numPoints);
    ReAllocmem(YValues, Sizeof(YValues[1]) * numPoints);
    for i := 1 to numPoints do
        XValues[i] := Other.XValues[i];
    for i := 1 to numPoints do
        YValues[i] := Other.YValues[i];

    FXshift := Other.FXshift;
    FYshift := Other.FYshift;
    FXscale := Other.FXscale;
    FYscale := Other.FYscale;
end;

constructor TXYcurveObj.Create(ParClass: TDSSClass; const XYCurveName: String);
begin
    inherited Create(ParClass, XYCurveName);
    DSSObjType := ParClass.DSSClassType;

    numPoints := 0;
    XValues := NIL;
    YValues := NIL;

    FX := 0.0;
    FY := 0.0;
    FXshift := 0.0;
    FYshift := 0.0;
    FXscale := 1.0;
    FYscale := 1.0;

    csvfile := '';
    dblfile := '';
    sngfile := '';
end;

destructor TXYcurveObj.Destroy;
begin
    if Assigned(XValues) then
        ReallocMem(XValues, 0);
    if Assigned(YValues) then
        ReallocMem(YValues, 0);
    inherited destroy;
end;

function TXYcurveObj.GetYValue(X: Double): Double;
// This function returns the interpolated Y value for the given X.
// If no points exist in the curve, the result is  0.0
// If Xvalue is outside the range of defined X values,
// the curve is extrapolated from the Ends.
var
    i: Integer;
begin
    Result := 0.0;    // default return value if no points in curve

    if numPoints <= 0 then         // Handle Exceptional cases
        Exit;

    if numPoints = 1 then
    begin
        Result := YValues[1];
        Exit;
    end;

    // if off the curve for the first point, extrapolate from the first two points
    if XValues[1] > X then
    begin
        Result := InterpolatePoints(1, 2, X, XValues, YValues);
        Exit;
    end;

    // In the middle of the arrays
    for i := 1 to numPoints do
    begin
        if (Abs(XValues[i] - X) < 0.00001) then  // If close to an actual point, just use it.
        begin
            Result := YValues[i];
            Exit;
        end
        else
        if (XValues[i] > X) then
        // INTERPOLATE between two values
        begin
            Result := InterpolatePoints(i, i - 1, X, XValues, YValues);
            Exit;
        end;
    end;

    // If we fall through the loop, Extrapolate from last two points
    Result := InterpolatePoints(numPoints, numPoints - 1, X, XValues, YValues);
end;

function TXYcurveObj.GetCoefficients(X: Double): TCoeff;
// This function returns the coefficients of the line interpolated line for the given X (a*X + b).
// If no points exist in the curve (or just a single point), the result is  (a = 0, b = 0)
// If Xvalue is outside the range of defined X values,
// the curve is extrapolated from the Ends (a = 0, b = extrapolated value)
var
    i: Integer;
//   coef: pDoubleArray;
    coef: TCoeff;
begin
    // default return value if no points in curve
    coef[1] := 0.0;
    coef[2] := 0.0;
    Result := coef;

    if numPoints <= 0 then         // Handle Exceptional cases
        Exit;

    if numPoints = 1 then
    begin
        Result := coef;
        Exit;
    end;

    // if off the curve for the first point, extrapolate from the first two points
    if XValues[1] > X then
    begin
        // Assume the same coefficients determined by the first two points. Necessary to keep
        // consistency with TXYcurveObj.GetYValue function.
        coef[1] := (YValues[2] - YValues[1]) / (XValues[2] - XValues[1]);
        coef[2] := YValues[2] - coef[1] * XValues[2];
        Result := coef;
        Exit;
    end;

    // In the middle of the arrays
    for i := 1 to numPoints do
    begin
        if (XValues[i] > X) then
        // INTERPOLATE between two values
        begin
            coef[1] := (YValues[i] - YValues[i - 1]) / (XValues[i] - XValues[i - 1]);
            coef[2] := YValues[i] - coef[1] * XValues[i];
            Result := coef;
            Exit;
        end;
    end;

    // Assume the same coefficients determined by the last two points. Necessary to keep
    // consistency with TXYcurveObj.GetYValue function.
    coef[1] := (YValues[numPoints] - YValues[numPoints - 1]) / (XValues[numPoints] - XValues[numPoints - 1]);
    coef[2] := YValues[numPoints] - coef[1] * XValues[numPoints];
    Result := coef;
end;

function TXYcurveObj.GetY(): Double;
begin
    Result := FY * FYscale + FYshift;
end;

function TXYcurveObj.YValue_pt(i: Integer): Double;
begin
    if (i <= numPoints) and (i > 0) then
    begin
        Result := YValues[i];
    end
    else
        Result := 0.0;
end;

function TXYcurveObj.GetX(): Double;
begin
    Result := FX * FXscale + FXshift;
end;

function TXYcurveObj.XValue_pt(i: Integer): Double;
begin
    if (i <= numPoints) and (i > 0) then
    begin
        Result := XValues[i];
    end
    else
        Result := 0.0;
end;

function TXYcurveObj.GetXValue(Y: Double): Double;
// This FUNCTION returns the interpolated X value for the given Y.
// If no points exist in the curve, the result is  0.0
// If Xvalue is outside the range of defined X values,
// the curve is extrapolated from the Ends.
// TEMc: change to relax assumption that Y values are increasing monotonically
//       if Y is not monotonic (increasing or decreasing) then X is not unique
var
    i: Integer;
begin
    Result := 0.0;    // default return value if no points in curve

    if numPoints <= 0 then
        Exit;

    if numPoints = 1 then
    begin
        Result := XValues[1];
        Exit;
    end;

    for i := 2 to numPoints do
    begin
        if ((Y >= YValues[i - 1]) and (Y <= YValues[i])) then
        begin
            Result := InterpolatePoints(i - 1, i, Y, YValues, XValues);
            Exit;
        end;
        if ((Y <= YValues[i - 1]) and (Y >= YValues[i])) then
        begin
            Result := InterpolatePoints(i - 1, i, Y, YValues, XValues);
            Exit;
        end;
    end;

    // Y is out of range, need to determine which end to extrapolate from
    if YValues[1] <= YValues[numPoints] then
    begin // increasing Y values
        if Y <= YValues[1] then
            Result := InterpolatePoints(1, 2, Y, YValues, XValues)
        else
            Result := InterpolatePoints(numPoints - 1, numPoints, Y, YValues, XValues);
    end
    else
    begin // decreasing Y values
        if Y >= YValues[1] then
            Result := InterpolatePoints(1, 2, Y, YValues, XValues)
        else
            Result := InterpolatePoints(numPoints - 1, numPoints, Y, YValues, XValues);
    end;
end;

function TXYcurveObj.InterpolatePoints(i, j: Integer; X: Double; Xarray, Yarray: pDoubleArray): Double;
var
    Den: Double;
begin
    Den := (Xarray[i] - Xarray[j]);
    if Den <> 0.0 then
        Result := Yarray[j] + (X - Xarray[j]) / Den * (Yarray[i] - Yarray[j])
    else
        Result := Yarray[i]; // Y is undefined, return ith value
end;

procedure TXYcurveObj.SetX(Value: Double);
begin
    FX := (Value - FXshift) / FXscale;
    FY := GetYValue(FX); //Keep In synch
end;

procedure TXYcurveObj.SetY(Value: Double);
begin
    FY := (Value - FYshift) / FYscale;
    FX := GetXValue(FY); //Keep In synch
end;

end.
