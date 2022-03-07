unit GrowthShape;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

// The GrowthShape object is a general DSS object used by all circuits
// as a reference for obtaining yearly growth curves.
//
// The values are set by the normal New and Edit procedures as for any DSS object.
//
// The values are retrieved by setting the Code Property in the GrowthShape Class.
// This sets the active GrowthShapeObj object to be the one referenced by the Code Property;
//
// Then the values of that code can be retrieved via the public variables.  Or you
// can pick up the ActiveGrowthShapeObj object and save the direct reference to the object.
//
// Growth shapes are entered as multipliers for the previous year's load.  If the
// load grows by 2.5% in a year, the multiplier is entered as 1.025.  You do not need
// to enter subsequent years if the multiplier remains the same.  You need only enter
// the years in which the growth rate is assumed to have changed.
//
// The user may place the data in CSV or binary files as well as passing through the
// command interface. The rules are the same as for LoadShapes except that the year
// is always entered.  CSV files are text separated by commas, one interval to a line.
// There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.
//
// (Year, multiplier) pairs are expected in all formats.  Through the COM interface,
// supply separate arrays of Year and Mult.
//
// Edit growthshape.allisonsub npts=5
// ~   year="1999 2000 2001 2005 2010"
// ~   mult="1.10 1.07 1.05 1.025 1.01"
//
// This example describes a growth curve that start off relatively fast (10%) and after
// 10 years tapers off to 1%

uses
    Classes,
    Command,
    DSSClass,
    DSSObject,
    UcMatrix,
    Arraydef;

type
{$SCOPEDENUMS ON}
    TGrowthShapeProp = (
        INVALID = 0,
        npts = 1, // Number of points to expect
        year = 2, // vector of year values
        mult = 3, // vector of multiplier values corresponding to years
        csvfile = 4, // Switch input to a csvfile (year, mult)
        sngfile = 5, // switch input to a binary file of singles  (year, mult)
        dblfile = 6 // switch input to a binary file of doubles (year, mult)
    );
{$SCOPEDENUMS OFF}

    TGrowthShape = class(TDSSClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TGrowthShapeObj = class(TDSSObject)
    PRIVATE
        Npts: Integer;  // Number of points in curve
        NYears: Integer;    // Number of years presently allocated in look up table
        Year, // Year values
        YearMult,
        Multiplier: pDoubleArray;  // Multipliers
        csvfile, dblfile, sngfile: String;

        procedure ReCalcYearMult;
    PUBLIC
        constructor Create(ParClass: TDSSClass; const GrowthShapeName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;
        function GetMult(Yr: Integer): Double;  // Get multiplier for Specified Year
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    UComplex, DSSUcomplex,
    MathUtil,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TGrowthShapeObj;
    TProp = TGrowthShapeProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TGrowthShape.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, DSS_OBJECT, 'GrowthShape');
end;

destructor TGrowthShape.Destroy;
begin
    inherited Destroy;
end;

procedure TGrowthShape.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    PropertyType[ord(TProp.year)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.year)] := ptruint(@obj.Year);
    PropertyOffset2[ord(TProp.year)] := ptruint(@obj.Npts);
    PropertyFlags[ord(TProp.year)] := [TPropertyFlag.IntegerToDouble];

    PropertyType[ord(TProp.mult)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.mult)] := ptruint(@obj.Multiplier);
    PropertyOffset2[ord(TProp.mult)] := ptruint(@obj.Npts);

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
    PropertyOffset[ord(TProp.Npts)] := ptruint(@obj.Npts);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;

    CommandList.Abbrev := FALSE; //TODO: why only this class?
end;

function TGrowthShape.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TGrowthShapeObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of 
        ord(TProp.Npts):
        begin
            ReAllocmem(Year, Sizeof(Double) * Npts);
            ReAllocmem(Multiplier, Sizeof(Double) * Npts);
        end;
        ord(TProp.csvfile):
            DoCSVFile(DSS, Year, Multiplier, Npts, False, csvfile, ParentClass.Name, True);
        ord(TProp.sngfile):
            DoSngFile(DSS, Year, Multiplier, Npts, False, sngfile, ParentClass.Name, True);
        ord(TProp.dblfile):
            DoDblFile(DSS, Year, Multiplier, Npts, False, dblfile, ParentClass.Name, True);
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TGrowthShape.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    TObj(ptr).ReCalcYearMult;
    Exclude(TObj(ptr).Flags, Flg.EditionActive);
    Result := True;
end;

procedure TGrowthShapeObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    Npts := Other.Npts;
    ReallocMem(Multiplier, SizeOf(Double) * Npts);
    for i := 1 to Npts do
        Multiplier^[i] := Other.Multiplier^[i];
    ReallocMem(Year, SizeOf(Double) * Npts);
    for i := 1 to Npts do
        Year^[i] := Other.Year^[i];
end;

constructor TGrowthShapeObj.Create(ParClass: TDSSClass; const GrowthShapeName: String);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(GrowthShapeName);
    DSSObjType := ParClass.DSSClassType;

    Npts := 0;
    Year := NIL;
    Multiplier := NIL;
    NYears := 30;
    YearMult := AllocMem(SizeOf(yearMult^[1]) * NYears);
    csvfile := '';
    dblfile := '';
    sngfile := '';
end;

destructor TGrowthShapeObj.Destroy;
begin
    ReallocMem(Year, 0);
    ReallocMem(Multiplier, 0);
    ReallocMem(YearMult, 0);
    inherited destroy;
end;

function TGrowthShapeObj.GetMult(Yr: Integer): Double;
// This function returns the multiplier to use for a load in the given year.
// The first year specified in the curve is the base year.  The Base value
// is the beginning of the first year.
var
    Index: Integer;
begin
    Result := 1.0;    // default return value if no points in curve

    if NPts > 0 then
    begin         // Handle Exceptional cases
        Index := Yr - Round(Year^[1]);
        if Index > 0 then
        begin     // Returns 1.0 for base year or any year previous
            if Index > Nyears then
            begin  // Make some more space
                NYears := Index + 10;
                ReallocMem(YearMult, SizeOf(YearMult^[1]) * NYears);
                ReCalcYearMult;
            end;
            Result := YearMult^[Index];
        end;
    end;
end;

procedure TGrowthShapeObj.ReCalcYearMult;
var
    i, DataPtr, Yr: Integer;
    Mult, MultInc: Double;
begin
  // Fill up the YearMult array with total yearly multiplier from base year
    Mult := Multiplier^[1];
    MultInc := Mult;
    YearMult^[1] := Mult;
    DataPtr := 1;
    Yr := Round(Year^[1]);
    for i := 2 to NYears do
    begin
        Inc(Yr);
        if DataPtr < Npts then
        begin
            if Year^[DataPtr + 1] = Yr then
            begin
                INC(DataPtr);
                MultInc := Multiplier^[DataPtr];
            end;
        end;
        Mult := Mult * MultInc;
        YearMult^[i] := Mult;
    end;
end;

end.