unit XYcurve;

{
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

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
    TXYcurveProp = (
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
{$SCOPEDENUMS OFF}

    TCoeff = array[1..2] of Double;

    TXYcurve = class(TDSSClass)
    PROTECTED
        procedure DefineProperties; override;
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
        LastValueAccessed: Integer;
        FX,
        FY: Double;

        function InterpolatePoints(i, j: Integer; X: Double; Xarray, Yarray: pDoubleArray): Double;
        function Get_YValue(i: Integer): Double;  // get Y Value by index
        function Get_XValue(i: Integer): Double;  // get X Value corresponding to point index
        procedure Set_XValue(Index: Integer; Value: Double);
        procedure Set_YValue(Index: Integer; Value: Double);

        function Get_X: Double;
        function Get_Y: Double;
        procedure Set_X(Value: Double);
        procedure Set_Y(Value: Double);

    PUBLIC
        FNumPoints: Integer;  // Number of points in curve

        FXshift,
        FYshift,
        FXscale,
        FYscale: Double;
        
        csvfile, dblfile, sngfile: String;

        constructor Create(ParClass: TDSSClass; const XYCurveName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        function GetYValue(X: Double): Double;  // Get Y value at specified X Value
        function GetXValue(Y: Double): Double;  // Get X value at specified Y Value
        function GetCoefficients(X: Double): TCoeff;

        property NumPoints: Integer READ FNumPoints;
        property XValue_pt[Index: Integer]: Double READ Get_XValue WRITE Set_XValue;
        property YValue_pt[Index: Integer]: Double READ Get_YValue WRITE Set_YValue;

        property X: Double READ Get_X WRITE Set_X;
        property Y: Double READ Get_Y WRITE Set_Y;
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
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TXYcurve.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, DSS_OBJECT, 'XYcurve');
end;

destructor TXYcurve.Destroy;
begin
    inherited Destroy;
end;

procedure SetX(Obj: TObj; Value: Double);
begin
    Obj.X := Value;
end;

procedure SetY(Obj: TObj; Value: Double);
begin
    Obj.Y := Value;
end;

function GetX(Obj: TObj): Double;
begin
    Result := Obj.X;
end;

function GetY(Obj: TObj): Double;
begin
    Result := Obj.Y;
end;

function Get2xNumPoints(Obj: TObj): Integer;
begin
    Result := Obj.FNumPoints * 2;
end;

procedure SetPoints(obj: TObj; Values: PDouble; ValueCount: Integer);
var
    i: Integer;
begin
    with obj do
    begin
        // Allow possible Resetting (to a lower value) of num points when specifying temperatures not Hours
        FNumPoints := ValueCount div 2;
        ReAllocmem(YValues, Sizeof(Double) * FNumPoints);
        ReAllocmem(XValues, Sizeof(Double) * FNumPoints);
        for i := 1 to FNumPoints do
        begin
            XValues[i] := Values^;
            Inc(Values);
            YValues[i] := Values^;
            Inc(Values);
        end;
        X := Xvalues[1];
        Y := Yvalues[1];
    end;
end;

procedure GetPoints(obj: TObj; var ResultPtr: PDouble; ResultCount: PAPISize);
var
    i: Integer;
    Result: PDoubleArray0;
begin
    with Obj do
        if (XValues <> NIL) and (YValues <> NIL) then
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, FNumPoints * 2);
            for i := 1 to FNumPoints do
            begin
                Result[2 * (i - 1)] := XValues^[i];
                Result[2 * (i - 1) + 1] := YValues^[i];
            end;
            Exit;
        end;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
    Result[0] := 0;
    Result[1] := 0;
end;

procedure TXYcurve.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // integer properties
    PropertyType[ord(TProp.Npts)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Npts)] := ptruint(@obj.FNumPoints);
          
    // double arrays
    PropertyType[ord(TProp.Xarray)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.Xarray)] := ptruint(@obj.XValues);
    PropertyOffset2[ord(TProp.Xarray)] := ptruint(@obj.FNumPoints);

    PropertyType[ord(TProp.Yarray)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.Yarray)] := ptruint(@obj.YValues);
    PropertyOffset2[ord(TProp.Yarray)] := ptruint(@obj.FNumPoints);

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
    PropertyFlags[ord(TProp.X)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction];
    
    PropertyType[ord(TProp.Y)] := TPropertyType.DoubleProperty;
    PropertyOffset[ord(TProp.Y)] := 1; // dummy
    PropertyWriteFunction[ord(TProp.Y)] := @SetY;
    PropertyReadFunction[ord(TProp.Y)] := @GetY;
    PropertyFlags[ord(TProp.Y)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction];

    // ...and just mark Points as custom
    PropertyType[ord(TProp.Points)] := TPropertyType.DoubleDArrayProperty;
    PropertyOffset[ord(TProp.Points)] := 1; // dummy
    PropertyWriteFunction[ord(TProp.Points)] := @SetPoints;
    PropertyReadFunction[ord(TProp.Points)] := @GetPoints;
    PropertyOffset3[ord(TProp.Points)] := ptruint(@Get2xNumPoints);
    PropertyFlags[ord(TProp.Points)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction, TPropertyFlag.SizeIsFunction, TPropertyFlag.Redundant];

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
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

procedure TXYcurveObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of
        ord(TProp.csvfile):
            DoCSVFile(DSS, Xvalues, Yvalues, FNumPoints, False, csvfile, ParentClass.Name); // file of x,y points, one to a line
        ord(TProp.sngfile):
            DoSngFile(DSS, Xvalues, Yvalues, FNumPoints, False, sngfile, ParentClass.Name);
        ord(TProp.dblfile):
            DoDblFile(DSS, Xvalues, Yvalues, FNumPoints, False, dblfile, ParentClass.Name);
    end;

    case Idx of
        ord(TProp.npts):
        begin
            // Force as the always first property when saving in a later point
            PrpSequence[Idx] := -10;
            ReAllocmem(YValues, Sizeof(YValues^[1]) * FNumPoints);
            ReAllocmem(XValues, Sizeof(XValues^[1]) * FNumPoints);
        end;
        ord(TProp.Yarray):
            Y := Yvalues^[1];
        ord(TProp.Xarray):
            X := Xvalues^[1];
        ord(TProp.csvfile), ord(TProp.sngfile), ord(TProp.dblfile): 
        begin
            X := Xvalues^[1];
            Y := Yvalues^[1];
        end;
    end;

    case Idx of
        2..7:
        begin
            LastValueAccessed := 1;
        end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
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
    FNumPoints := Other.NumPoints;
    ReAllocmem(XValues, Sizeof(XValues^[1]) * NumPoints);
    ReAllocmem(YValues, Sizeof(YValues^[1]) * NumPoints);
    for i := 1 to NumPoints do
        XValues^[i] := Other.XValues^[i];
    for i := 1 to NumPoints do
        YValues^[i] := Other.YValues^[i];

    FXshift := Other.FXshift;
    FYshift := Other.FYshift;
    FXscale := Other.FXscale;
    FYscale := Other.FYscale;
end;

constructor TXYcurveObj.Create(ParClass: TDSSClass; const XYCurveName: String);
begin
    inherited Create(ParClass);
    Name := LowerCase(XYCurveName);
    DSSObjType := ParClass.DSSClassType;

    LastValueAccessed := 1;

    FNumPoints := 0;
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

    if FNumPoints <= 0 then         // Handle Exceptional cases
        Exit;

    if FNumPoints = 1 then
    begin
        Result := YValues^[1];
        Exit;
    end;

    // Start with previous value accessed under the assumption that most
    // of the time, the values won't change much
    if (XValues^[LastValueAccessed] > X) then
        LastValueAccessed := 1; // Start over from Beginning

    // if off the curve for the first point, extrapolate from the first two points
    if (LastValueAccessed = 1) and (XValues[1] > X) then
    begin
        Result := InterpolatePoints(1, 2, X, XValues, YValues);
        Exit;
    end;

    // In the middle of the arrays
    for i := LastValueAccessed + 1 to FNumPoints do
    begin
        if (Abs(XValues^[i] - X) < 0.00001) then  // If close to an actual point, just use it.
        begin
            Result := YValues^[i];
            LastValueAccessed := i;
            Exit;
        end
        else
        if (XValues^[i] > X) then
        // INTERPOLATE between two values
        begin
            LastValueAccessed := i - 1;
            Result := InterpolatePoints(i, LastValueAccessed, X, XValues, YValues);
            Exit;
        end;
    end;

    // If we fall through the loop, Extrapolate from last two points
    LastValueAccessed := FNumPoints - 1;
    Result := InterpolatePoints(FNumPoints, LastValueAccessed, X, XValues, YValues);
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

    if FNumPoints <= 0 then         // Handle Exceptional cases
        Exit;

    if FNumPoints = 1 then
    begin
        Result := coef;
        Exit;
    end;

    // Start with previous value accessed under the assumption that most
    // of the time, the values won't change much
    if (XValues^[LastValueAccessed] > X) then
        LastValueAccessed := 1; // Start over from Beginning

    // if off the curve for the first point, extrapolate from the first two points
    if (LastValueAccessed = 1) and (XValues[1] > X) then
    begin
        // Assume the same coefficients determined by the first two points. Necessary to keep
        // consistency with TXYcurveObj.GetYValue function.
        coef[1] := (YValues^[2] - YValues^[1]) / (XValues^[2] - XValues^[1]);
        coef[2] := YValues^[2] - coef[1] * XValues^[2];

        Result := coef;
        Exit;
    end;

    // In the middle of the arrays
    for i := LastValueAccessed + 1 to FNumPoints do
        if (XValues^[i] > X) then
        // INTERPOLATE between two values
        begin
            LastValueAccessed := i - 1;
            coef[1] := (YValues^[i] - YValues^[i - 1]) / (XValues^[i] - XValues^[i - 1]);
            coef[2] := YValues^[i] - coef[1] * XValues^[i];
            Result := coef;
            Exit;
        end;

    // Assume the same coefficients determined by the last two points. Necessary to keep
    // consistency with TXYcurveObj.GetYValue function.
    coef[1] := (YValues^[FNumPoints] - YValues^[FNumPoints - 1]) / (XValues^[FNumPoints] - XValues^[FNumPoints - 1]);
    coef[2] := YValues^[FNumPoints] - coef[1] * XValues^[FNumPoints];
    Result := coef;
end;

function TXYcurveObj.Get_Y: Double;
begin
    Result := FY * FYscale + FYshift;
end;

function TXYcurveObj.Get_YValue(i: Integer): Double;
begin
    if (i <= FNumPoints) and (i > 0) then
    begin
        Result := YValues^[i];
        LastValueAccessed := i;
    end
    else
        Result := 0.0;
end;

function TXYcurveObj.Get_X: Double;
begin
    Result := FX * FXscale + FXshift;
end;

function TXYcurveObj.Get_XValue(i: Integer): Double;
begin
    if (i <= FNumPoints) and (i > 0) then
    begin
        Result := XValues^[i];
        LastValueAccessed := i;
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

    if FNumPoints <= 0 then
        Exit;

    if FNumPoints = 1 then
    begin
        Result := XValues^[1];
        Exit;
    end;

    for i := 2 to FNumPoints do
    begin
        if ((Y >= YValues^[i - 1]) and (Y <= YValues^[i])) then
        begin
            Result := InterpolatePoints(i - 1, i, Y, YValues, XValues);
            Exit;
        end;
        if ((Y <= YValues^[i - 1]) and (Y >= YValues^[i])) then
        begin
            Result := InterpolatePoints(i - 1, i, Y, YValues, XValues);
            Exit;
        end;
    end;

    // Y is out of range, need to determine which end to extrapolate from
    if YValues^[1] <= YValues^[FNumPoints] then
    begin // increasing Y values
        if Y <= YValues^[1] then
            Result := InterpolatePoints(1, 2, Y, YValues, XValues)
        else
            Result := InterpolatePoints(FNumPoints - 1, FNumPoints, Y, YValues, XValues);
    end
    else
    begin // decreasing Y values
        if Y >= YValues^[1] then
            Result := InterpolatePoints(1, 2, Y, YValues, XValues)
        else
            Result := InterpolatePoints(FNumPoints - 1, FNumPoints, Y, YValues, XValues);
    end;
end;

function TXYcurveObj.InterpolatePoints(i, j: Integer; X: Double; Xarray, Yarray: pDoubleArray): Double;
var
    Den: Double;
begin
    Den := (Xarray^[i] - Xarray^[j]);
    if Den <> 0.0 then
        Result := Yarray^[j] + (X - Xarray^[j]) / Den * (Yarray^[i] - Yarray^[j])
    else
        Result := Yarray^[i]; // Y is undefined, return ith value
end;

procedure TXYcurveObj.Set_X(Value: Double);
begin
    FX := (Value - FXshift) / FXscale;
    FY := GetYValue(FX); //Keep In synch
end;

procedure TXYCurveObj.Set_XValue(Index: Integer; Value: Double);
begin
    if Index <= FNumPoints then
        XValues^[Index] := Value;
end;

procedure TXYcurveObj.Set_Y(Value: Double);
begin
    FY := (Value - FYshift) / FYscale;
    FX := GetXValue(FY); //Keep In synch
end;

procedure TXYCurveObj.Set_YValue(Index: Integer; Value: Double);
begin
    if Index <= FNumPoints then
        YValues^[Index] := Value;
end;

end.