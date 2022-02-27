unit PriceShape;

{
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

// The PriceShape object is a general DSS object used by all circuits
// as a reference for obtaining yearly, daily, and other Price shapes.
//
// The values are set by the normal New and Edit procedures for any DSS object.
//
// The values may be retrieved by setting the Code Property in the PriceShape Class.
// This sets the active PriceShape object to be the one referenced by the Code Property;
//
// Then the values of that code can be retrieved via the public variables.  Or you
// can pick up the ActivePriceShapeObj object and save the direct reference to the object.
//
// PriceShapes default to fixed interval data (like Loadshapes).  If the Interval is specified to be 0.0,
// then both time and price data are expected.  If the Interval is  greater than 0.0,
// the user specifies only the prices.  The Hour command is ignored and the files are
// assumed to contain only the price data.
//
// The Interval may also be specified in seconds (sinterval) or minutes (minterval).
//
// The user may place the data in CSV or binary files as well as passing through the
// command interface. Obviously, for large amounts of data such as 8760 load curves, the
// command interface is cumbersome.  CSV files are text separated by commas, one interval to a line.
// There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.
//
// For fixed interval data, only the price values are expected.  Therefore, the CSV format would
// contain only one number per line.  The two binary formats are packed.
//
// For variable interval data, (hour, price) pairs are expected in both formats.
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
    TPriceShapeProp = (
        INVALID = 0,
        npts = 1, // Number of points to expect
        interval = 2, // default = 1.0;
        price = 3, // vector of price values
        hour = 4, // vector of hour values
        mean = 5, // set the mean Price (otherwise computed)
        stddev = 6, // set the std dev of the Price (otherwise computed)
        csvfile = 7, // Switch input to a csvfile
        sngfile = 8, // switch input to a binary file of singles
        dblfile = 9, // switch input to a binary file of doubles
        sinterval = 10, // Interval in seconds
        minterval = 11, // Interval in minutes
        action = 12
    );
{$SCOPEDENUMS OFF}

    TPriceShape = class(TDSSClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
        function Find(const ObjName: String; const ChangeActive: Boolean=True): Pointer; OVERRIDE;  // Find an obj of this class by name
    end;

    TPriceShapeObj = class(TDSSObject)
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
    PUBLIC
        Interval: Double;  //=0.0 then random interval     (hr)
        Hours,          // Time values (hr) if Interval > 0.0  Else nil
        PriceValues: pDoubleArray;  // Prices
        csvfile, dblfile, sngfile: String;

        constructor Create(ParClass: TDSSClass; const PriceShapeName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        function GetPrice(hr: Double): Double;  // Get Prices at specified time, hr
        function Price(i: Integer): Double;  // get Prices by index
        function Hour(i: Integer): Double;  // get hour corresponding to point index

        property NumPoints: Integer READ FNumPoints;
        property PresentInterval: Double READ Get_Interval;
        property Mean: Double READ Get_Mean;
        property StdDev: Double READ Get_StdDev;
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
    TObj = TPriceShapeObj;
    TProp = TPriceShapeProp;
{$PUSH}
{$Z4} // keep enums as int32 values
    TPriceShapeAction = (
        DblSave = 0,
        SngSave = 1
    );
{$POP}

const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer;    
    ActionEnum: TDSSEnum;

constructor TPriceShape.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        ActionEnum := TDSSEnum.Create('PriceShape: Action', True, 1, 1, 
            ['DblSave', 'SngSave'], 
            [ord(TPriceShapeAction.DblSave), ord(TPriceShapeAction.SngSave)]);
    end;

    inherited Create(dssContext, DSS_OBJECT, 'PriceShape');
end;

destructor TPriceShape.Destroy;
begin
    inherited Destroy;
end;

procedure DoAction(Obj: TObj; action: TPriceShapeAction);
begin
    case action of
        TPriceShapeAction.DblSave:
            Obj.SaveToDblFile;
        TPriceShapeAction.SngSave:
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

procedure TPriceShape.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

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

    // integer properties
    PropertyType[ord(TProp.Npts)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Npts)] := ptruint(@obj.FNumPoints);

    // advanced doubles
    PropertyOffset[ord(TProp.sinterval)] := ptruint(@obj.Interval);
    PropertyScale[ord(TProp.sinterval)] := 1 / 3600.0;
    PropertyFlags[ord(TProp.sinterval)] := [TPropertyFlag.Redundant];

    PropertyOffset[ord(TProp.minterval)] := ptruint(@obj.Interval);
    PropertyScale[ord(TProp.minterval)] := 1 / 60.0;
    PropertyFlags[ord(TProp.minterval)] := [TPropertyFlag.Redundant];

    // double properties
    PropertyOffset[ord(TProp.Interval)] := ptruint(@obj.Interval);
    
    PropertyOffset[ord(TProp.mean)] := ptruint(@obj.FMean);
    PropertyReadFunction[ord(TProp.mean)] := @GetMean;
    PropertyFlags[ord(TProp.mean)] := [TPropertyFlag.ReadByFunction];
    
    PropertyOffset[ord(TProp.stddev)] := ptruint(@obj.FStdDev);
    PropertyReadFunction[ord(TProp.stddev)] := @GetStdDev;
    PropertyFlags[ord(TProp.stddev)] := [TPropertyFlag.ReadByFunction];

    // double arrays
    PropertyType[ord(TProp.price)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.price)] := ptruint(@obj.PriceValues);
    PropertyOffset2[ord(TProp.price)] := ptruint(@obj.FNumPoints);

    PropertyType[ord(TProp.hour)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.hour)] := ptruint(@obj.Hours);
    PropertyOffset2[ord(TProp.hour)] := ptruint(@obj.FNumPoints);

    // enum action
    PropertyType[ord(TProp.Action)] := TPropertyType.StringEnumActionProperty;
    PropertyOffset[ord(TProp.Action)] := ptruint(@DoAction); 
    PropertyOffset2[ord(TProp.Action)] := PtrInt(ActionEnum); 
    
    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TPriceShape.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TPriceShapeObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of 
        ord(TProp.csvfile):
            DoCSVFile(DSS, Hours, PriceValues, FNumPoints, (Interval <> 0.0), csvfile, ParentClass.Name);
        ord(TProp.sngfile):
            DoSngFile(DSS, Hours, PriceValues, FNumPoints, (Interval <> 0.0), sngfile, ParentClass.Name);
        ord(TProp.dblfile):
            DoDblFile(DSS, Hours, PriceValues, FNumPoints, (Interval <> 0.0), dblfile, ParentClass.Name);
    end;
    case Idx of
        ord(TProp.npts):
        begin
            // Force as the always first property when saving in a later point
            PrpSequence[Idx] := -10;

            ReAllocmem(PriceValues, Sizeof(PriceValues^[1]) * FNumPoints);
            if Interval > 0.0 then
                ReallocMem(Hours, 0) //TODO: check if required
            else
                ReAllocmem(Hours, Sizeof(Hours^[1]) * FNumPoints);
        end;
        ord(TProp.interval):
            if Interval > 0.0 then
                ReallocMem(Hours, 0); //TODO: check if required
        ord(TProp.hour):
            Interval := 0;

        ord(TProp.mean), 
        ord(TProp.stddev):
            FStdDevCalculated := TRUE;

        3, 7, 8, 9:
        begin
            FStdDevCalculated := FALSE;   // now calculated on demand
            //TODO: check if needed after full migration: NumPoints := FNumPoints;  // Keep Properties in order for save command
        end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TPriceShape.Find(const ObjName: String; const ChangeActive: Boolean): Pointer;
begin
    if (Length(ObjName) = 0) or (CompareText(ObjName, 'none') = 0) then
        Result := NIL
    else
        Result := inherited Find(ObjName, ChangeActive);
end;

procedure TPriceShapeObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNumPoints := Other.FNumPoints;
    Interval := Other.Interval;
    ReallocMem(PriceValues, SizeOf(PriceValues^[1]) * FNumPoints);
    for i := 1 to FNumPoints do
        PriceValues^[i] := Other.PriceValues^[i];
    if Interval > 0.0 then
        ReallocMem(Hours, 0)
    else
    begin
        ReallocMem(Hours, SizeOf(Hours^[1]) * FNumPoints);
        for i := 1 to FNumPoints do
            Hours^[i] := Other.Hours^[i];
    end;
end;

constructor TPriceShapeObj.Create(ParClass: TDSSClass; const PriceShapeName: String);
begin
    inherited Create(ParClass);
    Name := LowerCase(PriceShapeName);
    DSSObjType := ParClass.DSSClassType;

    LastValueAccessed := 1;

    FNumPoints := 0;
    Interval := 1.0;  // hr
    Hours := NIL;
    PriceValues := NIL;
    FStdDevCalculated := FALSE;  // calculate on demand

    csvfile := '';
    dblfile := '';
    sngfile := '';
end;

destructor TPriceShapeObj.Destroy;
begin
    ReallocMem(Hours, 0);
    if Assigned(PriceValues) then
        ReallocMem(PriceValues, 0);
    inherited destroy;
end;

function TPriceShapeObj.GetPrice(hr: Double): Double;
// This FUNCTION returns the Price for the given hour.
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
        Result := PriceValues^[1];
        Exit;
    end;

    if Interval > 0.0 then
    begin
        Index := round(hr / Interval);
        if Index > FNumPoints then
            Index := Index mod FNumPoints;  // Wrap around using remainder
        if Index = 0 then
            Index := FNumPoints;
        Result := PriceValues^[Index];
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
            Result := PriceValues^[i];
            LastValueAccessed := i;
            Exit;
        end
        else
        if (Hours^[i] > Hr) then      // Interpolate for Price
        begin
            LastValueAccessed := i - 1;
            Result := PriceValues^[LastValueAccessed] +
                (Hr - Hours^[LastValueAccessed]) / (Hours^[i] - Hours^[LastValueAccessed]) *
                (PriceValues^[i] - PriceValues^[LastValueAccessed]);
            Exit;
        end;
    end;

    // If we fall through the loop, just use last value
    LastValueAccessed := FNumPoints - 1;
    Result := PriceValues^[FNumPoints];
end;

procedure TPriceShapeObj.CalcMeanandStdDev;
begin
    if FNumPoints > 0 then
        if Interval > 0.0 then
            RCDMeanandStdDev(PriceValues, FNumPoints, FMean, FStdDev)
        else
            CurveMeanAndStdDev(PriceValues, Hours, FNumPoints, FMean, FStdDev);

    FStdDevCalculated := TRUE;
end;

function TPriceShapeObj.Get_Interval: Double;
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

function TPriceShapeObj.Get_Mean: Double;
begin
    if not FStdDevCalculated then
        CalcMeanandStdDev;
    Result := FMean;
end;

function TPriceShapeObj.Get_StdDev: Double;
begin
    if not FStdDevCalculated then
        CalcMeanandStdDev;
    Result := FStdDev;
end;

function TPriceShapeObj.Price(i: Integer): Double;
begin
    if (i <= FNumPoints) and (i > 0) then
    begin
        Result := PriceValues^[i];
        LastValueAccessed := i;
    end
    else
        Result := 0.0;
end;

function TPriceShapeObj.Hour(i: Integer): Double;
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

procedure TPriceShapeObj.SaveToDblFile;
var
    F: TFileStream = nil;
    Fname: String;
begin
    if not Assigned(PriceValues) then
    begin
        DoSimpleMsg('%s Prices not defined.', [FullName], 58622);
        Exit;
    end;

    try
        FName := DSS.OutputDirectory {CurrentDSSDir} + Format('%s.dbl', [Name]);
        F := TFileStream.Create(FName, fmCreate);
        F.WriteBuffer(PriceValues^[1], NumPoints * SizeOf(Double));
        DSS.GlobalResult := 'Price=[dblfile=' + FName + ']';
    finally
        FreeAndNil(F);
    end;
end;

procedure TPriceShapeObj.SaveToSngFile;
var
    F: TFileStream = nil;
    i: Integer;
    Fname: String;
    sngPrice: Single;
begin
    if not Assigned(PriceValues) then
    begin
        DoSimpleMsg('%s Prices not defined.', [FullName], 58623);
        Exit;
    end;

    try
        FName := DSS.OutputDirectory {CurrentDSSDir} + Format('%s.sng', [Name]);
        F := TFileStream.Create(FName, fmCreate);
        for i := 1 to NumPoints do
        begin
            sngPrice := PriceValues^[i];
            F.Write(sngPrice, sizeof(sngPrice));
        end;
        DSS.GlobalResult := 'Price=[sngfile=' + FName + ']';
    finally
        FreeAndNil(F);
    end;
end;

initialization
    PropInfo := NIL;
finalization
    ActionEnum.Free;        
end.
