unit GrowthShape;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{  8-18-00 Added call to InterpretDblArrayto allow File=Syntax }

interface

{The GrowthShape object is a general DSS object used by all circuits
 as a reference for obtaining yearly growth curves.

 The values are set by the normal New and Edit procedures as for any DSS object.

 The values are retrieved by setting the Code Property in the GrowthShape Class.
 This sets the active GrowthShapeObj object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.  Or you
 can pick up the ActiveGrowthShapeObj object and save the direct reference to the object.

 Growth shapes are entered as multipliers for the previous year's load.  If the
 load grows by 2.5% in a year, the multiplier is entered as 1.025.  You do not need
 to enter subsequent years if the multiplier remains the same.  You need only enter
 the years in which the growth rate is assumed to have changed.

 The user may place the data in CSV or binary files as well as passing through the
 command interface. The rules are the same as for LoadShapes except that the year
 is always entered.  CSV files are text separated by commas, one interval to a line.
 There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.

 (Year, multiplier) pairs are expected in all formats.  Through the COM interface,
 supply separate arrays of Year and Mult.

 Edit growthshape.allisonsub npts=5
 ~   year="1999 2000 2001 2005 2010"
 ~   mult="1.10 1.07 1.05 1.025 1.01"

 This example describes a growth curve that start off relatively fast (10%) and after
 10 years tapers off to 1%

 }

uses
    Command,
    DSSClass,
    DSSObject,
    UcMatrix,
    Arraydef;

type

    TGrowthShape = class(TDSSClass)
    PRIVATE

        function Get_Code: String;  // Returns active GrowthShape string
        procedure Set_Code(const Value: String);  // sets the  active GrowthShape

        procedure DoCSVFile(const FileName: String);
        procedure DoSngFile(const FileName: String);
        procedure DoDblFile(const FileName: String);
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const ShapeName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

       // Set this property to point ActiveGrowthShapeObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;

    end;

    TGrowthShapeObj = class(TDSSObject)
    PRIVATE
        Npts: Integer;  // Number of points in curve
        NYears: Integer;    // Number of years presently allocated in look up table
        BaseYear: Integer;

        Year: pIntegerArray;          // Year values
        YearMult,
        Multiplier: pDoubleArray;  // Multipliers

        procedure ReCalcYearMult;
    PUBLIC

        constructor Create(ParClass: TDSSClass; const GrowthShapeName: String);
        destructor Destroy; OVERRIDE;

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        function GetMult(Yr: Integer): Double;  // Get multiplier for Specified Year
    end;

var
    ActiveGrowthShapeObj: TGrowthShapeObj;


implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Ucomplex,
    MathUtil,
    Utilities;

const
    NumPropsThisClass = 6;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TGrowthShape.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    Class_Name := 'GrowthShape';
    DSSClassType := DSS_OBJECT;

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := FALSE;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TGrowthShape.Destroy;

begin

    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGrowthShape.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count

    AllocatePropertyArrays;


     // Define Property names

    PropertyName[1] := 'npts';     // Number of points to expect
    PropertyName[2] := 'year';     // vextor of year values
    PropertyName[3] := 'mult';     // vector of multiplier values corresponding to years
    PropertyName[4] := 'csvfile';   // Switch input to a csvfile                 (year, mult)
    PropertyName[5] := 'sngfile';  // switch input to a binary file of singles  (year, mult)
    PropertyName[6] := 'dblfile';   // switch input to a binary file of doubles (year, mult)

    PropertyHelp[1] := 'Number of points to expect in subsequent vector.';
    PropertyHelp[2] := 'Array of year values, or a text file spec, corresponding to the multipliers. ' +
        'Enter only those years where the growth changes. ' +
        'May be any integer sequence -- just so it is consistent. See help on Mult.';
    PropertyHelp[3] := 'Array of growth multiplier values, or a text file spec, corresponding to the year values. ' +
        'Enter the multiplier by which you would multiply the previous year''s load to get the present year''s.' +
        CRLF + CRLF + 'Examples:' + CRLF + CRLF +
        '  Year = [1, 2, 5]   Mult=[1.05, 1.025, 1.02].' + CRLF +
        '  Year= (File=years.txt) Mult= (file=mults.txt).' + CRLF + CRLF +
        'Text files contain one value per line.';
    PropertyHelp[4] := 'Switch input of growth curve data to a csv file containing (year, mult) points, one per line.';
    PropertyHelp[5] := 'Switch input of growth curve data to a binary file of singles ' +
        'containing (year, mult) points, packed one after another.';
    PropertyHelp[6] := 'Switch input of growth curve data to a binary file of doubles ' +
        'containing (year, mult) points, packed one after another.';


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TGrowthShape.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    with ActiveCircuit do
    begin
        ActiveDSSObject := TGrowthShapeObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TGrowthShape.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    YrBuffer: pDoubleArray;
    i: Integer;

begin
    Result := 0;
  // continue parsing with contents of Parser
    ActiveGrowthShapeObj := ElementList.Active;
    ActiveDSSObject := ActiveGrowthShapeObj;

    with ActiveGrowthShapeObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 600);
                1:
                    Npts := Parser.Intvalue;
                2:
                begin
                    ReAllocmem(Year, Sizeof(Year^[1]) * Npts);
                    YrBuffer := Allocmem(Sizeof(Double) * Npts);
                    InterpretDblArray(Param, Npts, YrBuffer);  // Parser.ParseAsVector(Npts, Yrbuffer);

                    for i := 1 to Npts do
                        Year^[i] := Round(YrBuffer^[i]);
                    BaseYear := Year^[1];
                    FreeMem(YrBuffer, Sizeof(Double) * Npts);
                end;
                3:
                begin
                    ReAllocmem(Multiplier, Sizeof(Multiplier^[1]) * Npts);
                    InterpretDblArray(Param, Npts, Multiplier);   //Parser.ParseAsVector(Npts, Multiplier);

                end;
                4:
                    DoCSVFile(Param);
                5:
                    DoSngFile(Param);
                6:
                    DoDblFile(Param);
            else
           // Inherited parameters
                ClassEdit(ActiveGrowthShapeObj, ParamPointer - NumPropsThisClass)
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end; {WHILE}

        ReCalcYearMult;
    end; {WITH}
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TGrowthShape.MakeLike(const ShapeName: String): Integer;
var
    OtherGrowthShape: TGrowthShapeObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this line code in the present collection}
    OtherGrowthShape := Find(ShapeName);
    if OtherGrowthShape <> NIL then
        with ActiveGrowthShapeObj do
        begin
            Npts := OtherGrowthShape.Npts;
            ReallocMem(Multiplier, SizeOf(Multiplier^[1]) * Npts);
            for i := 1 to Npts do
                Multiplier^[i] := OtherGrowthShape.Multiplier^[i];
            ReallocMem(Year, SizeOf(Year^[1]) * Npts);
            for i := 1 to Npts do
                Year^[i] := OtherGrowthShape.Year^[i];

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherGrowthShape.PropertyValue[i];

        end
    else
        DoSimpleMsg('Error in GrowthShape MakeLike: "' + ShapeName + '" Not Found.', 601);


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TGrowthShape.Init(Handle: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TGrowthShape.Init', -1);
    REsult := 0;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TGrowthShape.Get_Code: String;  // Returns active line code string
var
    GrowthShapeObj: TGrowthShapeObj;

begin

    GrowthShapeObj := ElementList.Active;
    Result := GrowthShapeObj.Name;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGrowthShape.Set_Code(const Value: String);  // sets the  active GrowthShape

var
    GrowthShapeObj: TGrowthShapeObj;

begin

    ActiveGrowthShapeObj := NIL;
    GrowthShapeObj := ElementList.First;
    while GrowthShapeObj <> NIL do
    begin

        if CompareText(GrowthShapeObj.Name, Value) = 0 then
        begin
            ActiveGrowthShapeObj := GrowthShapeObj;
            Exit;
        end;

        GrowthShapeObj := ElementList.Next;
    end;

    DoSimpleMsg('GrowthShape: "' + Value + '" not Found.', 602);

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGrowthShape.DoCSVFile(const FileName: String);

var
    F: Textfile;
    i: Integer;
    s: String;

begin
    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 603);
        CloseFile(F);
        Exit;
    end;

    try
        with ActiveGrowthShapeObj do
        begin
            i := 0;
            while (not EOF(F)) and (i < Npts) do
            begin
                Inc(i);
                Readln(F, s);  {Use AuxParser to allow flexible formats}
                with AuxParser do
                begin
             // Readln(F,Year^[i], Multiplier^[i]);
                    CmdString := S;
                    NextParam;
                    Year^[i] := IntValue;
                    NextParam;
                    Multiplier^[i] := DblValue;
                end;
            end;
            CloseFile(F);
        end;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error Processing CSV File: "' + FileName + '. ' + E.Message, 604);
            CloseFile(F);
            Exit;
        end;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGrowthShape.DoSngFile(const FileName: String);
var
    F: file of Single;
    Y, M: Single;
    i: Integer;

begin
    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 605);
        CloseFile(F);
        Exit;
    end;

    try
        with ActiveGrowthShapeObj do
        begin
            i := 0;
            while (not EOF(F)) and (i < Npts) do
            begin
                Inc(i);
                Read(F, Y, M);
                Year^[i] := Round(Y);
                Multiplier^[i] := M;
            end;
            CloseFile(F);
        end;
    except
        DoSimpleMsg('Error Processing GrowthShape File: "' + FileName, 606);
        CloseFile(F);
        Exit;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGrowthShape.DoDblFile(const FileName: String);
var
    F: file of Double;
    i: Integer;
    Yr: Double;

begin
    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 607);
        CloseFile(F);
        Exit;
    end;

    try
        with ActiveGrowthShapeObj do
        begin
            i := 0;
            while (not EOF(F)) and (i < Npts) do
            begin
                Inc(i);
                Read(F, Yr, Multiplier^[i]);
                Year^[i] := Round(Yr);
            end;
            CloseFile(F);
        end;
    except
        DoSimpleMsg('Error Processing GrowthShape File: "' + FileName, 608);
        CloseFile(F);
        Exit;
    end;


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TGrowthShape Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TGrowthShapeObj.Create(ParClass: TDSSClass; const GrowthShapeName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(GrowthShapeName);
    DSSObjType := ParClass.DSSClassType;

    Npts := 0;
    Year := NIL;
    Multiplier := NIL;
    NYears := 30;
    YearMult := AllocMem(SizeOf(yearMult^[1]) * NYears);

    InitPropertyValues(0);

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TGrowthShapeObj.Destroy;
begin

    ReallocMem(Year, 0);
    ReallocMem(Multiplier, 0);
    ReallocMem(YearMult, 0);
    inherited destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
        Index := Yr - BaseYear;
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


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
    Yr := BaseYear;
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

procedure TGrowthShapeObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);


    with ParentClass do
    begin
        for i := 1 to NumProperties do
        begin
            case i of
                2, 3:
                    Writeln(F, '~ ', PropertyName^[i], '=(', PropertyValue[i], ')');
            else
                Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
            end;
        end;

    end;

end;


function TGrowthShapeObj.GetPropertyValue(Index: Integer): String;
var
    i: Integer;
begin
    case Index of
        2, 3:
            Result := '(';
    else
        Result := '';
    end;

    case Index of
        2:
            for i := 1 to Npts do
                Result := Result + Format('%-d, ', [Year^[i]]);
        3:
            for i := 1 to Npts do
                Result := Result + Format('%-g, ', [Multiplier^[i]]);
    else
        Result := inherited GetPropertyValue(index);
    end;

    case Index of
        2, 3:
            Result := Result + ')';
    else
    end;

end;

procedure TGrowthShapeObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := '0';     // Number of points to expect
    PropertyValue[2] := '';     // vextor of year values
    PropertyValue[3] := '';     // vector of multiplier values corresponding to years
    PropertyValue[4] := '';   // Switch input to a csvfile                 (year, mult)
    PropertyValue[5] := '';  // switch input to a binary file of singles  (year, mult)
    PropertyValue[6] := '';   // switch input to a binary file of doubles (year, mult)

    inherited InitPropertyValues(NumPropsThisClass);
end;


end.
