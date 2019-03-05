unit XYcurve;

{
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{  2-15-2011 Converted from TempShape.

   General X-Y Curve Data Support Class

}

interface

{

 The XYcurve object is a general DSS object used by all circuit elements
 as a reference for obtaining yearly, daily, and other Temperature shapes.

 The values are set by the normal New and Edit PROCEDUREs for any DSS object.

 The values may be retrieved by setting the Code Property in the XYCurve Class.
 This sets the active XYCurve object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.  Or you
 can pick up the ActiveTXYcurveObj object and save the direct reference to the object.

 The user may place the curve data in CSV or binary files as well as passing through the
 command interface. Obviously, for large amounts of data such as 8760 load curves, the
 command interface is cumbersome.  CSV files are text separated by commas, or white space
 one point to a line.

 There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.



 }

uses
    Command,
    DSSClass,
    DSSObject,
    Arraydef;

type

    TXYcurve = class(TDSSClass)
    PRIVATE
        TempPointsBuffer: pDoubleArray;
        function Get_Code: String;  // Returns active TShape string
        procedure Set_Code(const Value: String);  // sets the  active TShape

        procedure DoCSVFile(const FileName: String);
        procedure DoSngFile(const FileName: String);
        procedure DoDblFile(const FileName: String);
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const CurveName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

        function Find(const ObjName: String): Pointer; OVERRIDE;  // Find an obj of this class by name


       // Set this property to point ActiveTShapeObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;

    end;

    TXYcurveObj = class(TDSSObject)
    PRIVATE
        LastValueAccessed,
        FNumPoints: Integer;  // Number of points in curve
        ArrayPropertyIndex: Integer;
        FX,
        FY: Double;
        XValues,
        YValues: pDoubleArray;

        procedure Set_NumPoints(const Value: Integer);
        function InterpolatePoints(i, j: Integer; X: Double; Xarray, Yarray: pDoubleArray): Double;
       // PROCEDURE SaveToDblFile;
       // PROCEDURE SaveToSngFile;
        function Get_YValue(i: Integer): Double;  // get Y Value by index
        function Get_XValue(i: Integer): Double;  // get X Value corresponding to point index
        procedure Set_XValue(Index: Integer; Value: Double);
        procedure Set_YValue(Index: Integer; Value: Double);

        function Get_X: Double;
        function Get_Y: Double;
        procedure Set_X(Value: Double);
        procedure Set_Y(Value: Double);

    PUBLIC

       // Make these vars available to COM interface
        FXshift,
        FYshift,
        FXscale,
        FYscale: Double;

        constructor Create(ParClass: TDSSClass; const XYCurveName: String);
        destructor Destroy; OVERRIDE;

        function GetYValue(X: Double): Double;  // Get Y value at specified X Value
        function GetXValue(Y: Double): Double;  // Get X value at specified Y Value


        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        procedure SaveWrite(var F: TextFile); OVERRIDE;

        property NumPoints: Integer READ FNumPoints WRITE Set_NumPoints;
        property XValue_pt[Index: Integer]: Double READ Get_XValue WRITE Set_XValue;
        property YValue_pt[Index: Integer]: Double READ Get_YValue WRITE Set_YValue;

        property X: Double READ Get_X WRITE Set_X;
        property Y: Double READ Get_Y WRITE Set_Y;

    end;

var
    ActiveXYcurveObj: TXYcurveObj;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    MathUtil,
    Utilities,
    Classes,
    Math,
    PointerList;

const
    NumPropsThisClass = 13;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TXYcurve.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    Class_Name := 'XYcurve';
    DSSClassType := DSS_OBJECT;

    ActiveElement := 0;
    TempPointsBuffer := NIL;  // Has to start off Nil for Reallocmem call

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TXYcurve.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TXYcurve.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names
    PropertyName[1] := 'npts';     // Number of points to expect
    PropertyName[2] := 'Points';
    PropertyName[3] := 'Yarray';     // vector of Y values
    PropertyName[4] := 'Xarray';     // vector of X values corresponding to Y values
    PropertyName[5] := 'csvfile';  // Switch input to a csvfile
    PropertyName[6] := 'sngfile';  // switch input to a binary file of singles
    PropertyName[7] := 'dblfile';    // switch input to a binary file of singles
    PropertyName[8] := 'x';
    PropertyName[9] := 'y';
    PropertyName[10] := 'Xshift';
    PropertyName[11] := 'Yshift';
    PropertyName[12] := 'Xscale';
    PropertyName[13] := 'Yscale';

     // define Property help values

    PropertyHelp[1] := 'Max number of points to expect in curve. This could get reset to the actual number of points defined ' +
        'if less than specified.';     // Number of points to expect
    PropertyHelp[2] := 'One way to enter the points in a curve. Enter x and y values as one array ' +
        'in the order [x1, y1, x2, y2, ...]. For example:' + CRLF + CRLF +
        'Points=[1,100 2,200 3, 300] ' + CRLF + CRLF +
        'Values separated by commas or white space. Zero fills arrays if insufficient number of values.';
    PropertyHelp[3] := 'Alternate way to enter Y values. Enter an array of Y values corresponding to the X values.  ' +
        'You can also use the syntax: ' + CRLF +
        'Yarray = (file=filename)     !for text file one value per line' + CRLF +
        'Yarray = (dblfile=filename)  !for packed file of doubles' + CRLF +
        'Yarray = (sngfile=filename)  !for packed file of singles ' + CRLF + CRLF +
        'Note: this property will reset Npts to a smaller value if the  number of values in the files are fewer.';     // vextor of hour values
    PropertyHelp[4] := 'Alternate way to enter X values. Enter an array of X values corresponding to the Y values.  ' +
        'You can also use the syntax: ' + CRLF +
        'Xarray = (file=filename)     !for text file one value per line' + CRLF +
        'Xarray = (dblfile=filename)  !for packed file of doubles' + CRLF +
        'Xarray = (sngfile=filename)  !for packed file of singles ' + CRLF + CRLF +
        'Note: this property will reset Npts to a smaller value if the  number of values in the files are fewer.';     // vextor of hour values
    PropertyHelp[5] := 'Switch input of  X-Y curve data to a CSV file ' +
        'containing X, Y points one per line. ' +
        'NOTE: This action may reset the number of points to a lower value.';   // Switch input to a csvfile
    PropertyHelp[6] := 'Switch input of  X-Y curve data to a binary file of SINGLES ' +
        'containing X, Y points packed one after another. ' +
        'NOTE: This action may reset the number of points to a lower value.';  // switch input to a binary file of singles
    PropertyHelp[7] := 'Switch input of  X-Y  curve data to a binary file of DOUBLES ' +
        'containing X, Y points packed one after another. ' +
        'NOTE: This action may reset the number of points to a lower value.';   // switch input to a binary file of singles
    PropertyHelp[8] := 'Enter a value and then retrieve the interpolated Y value from the Y property. On input shifted then scaled to original curve. Scaled then shifted on output.';
    PropertyHelp[9] := 'Enter a value and then retrieve the interpolated X value from the X property. On input shifted then scaled to original curve. Scaled then shifted on output.';
    PropertyHelp[10] := 'Shift X property values (in/out) by this amount of offset. Default = 0. Does not change original definition of arrays.';
    PropertyHelp[11] := 'Shift Y property values (in/out) by this amount of offset. Default = 0. Does not change original definition of arrays.';
    PropertyHelp[12] := 'Scale X property values (in/out) by this factor. Default = 1.0. Does not change original definition of arrays.';
    PropertyHelp[13] := 'Scale Y property values (in/out) by this factor. Default = 1.0. Does not change original definition of arrays.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TXYcurve.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    with ActiveCircuit do
    begin
        ActiveDSSObject := TXYcurveObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TXYcurve.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

    i: Integer;

begin
    Result := 0;
  // continue parsing with contents of Parser
    ActiveXYcurveObj := ElementList.Active;
    ActiveDSSObject := ActiveXYcurveObj;

    with ActiveXYcurveObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 610);
                1:
                    NumPoints := Parser.Intvalue;
                2:
                begin
                    ReAllocmem(TempPointsBuffer, Sizeof(TempPointsBuffer^[1]) * FNumPoints * 2);
                 // Allow possible Resetting (to a lower value) of num points when specifying temperatures not Hours
                    NumPoints := InterpretDblArray(Param, (FNumPoints * 2), TempPointsBuffer) div 2;  // Parser.ParseAsVector(Npts, Temperatures);
                    ReAllocmem(YValues, Sizeof(YValues^[1]) * FNumPoints);
                    ReAllocmem(XValues, Sizeof(XValues^[1]) * FNumPoints);
                    for i := 1 to FNumPoints do
                    begin
                        XValues^[i] := TempPointsBuffer^[2 * i - 1];
                        YValues^[i] := TempPointsBuffer^[2 * i];
                    end;
                    X := Xvalues^[1];
                    Y := Yvalues^[1];
                    ReAllocmem(TempPointsBuffer, 0);  // Throw away temp array
                end;
                3:
                begin
                    ReAllocmem(YValues, Sizeof(YValues^[1]) * NumPoints);
                    NumPoints := InterpretDblArray(Param, NumPoints, YValues);
                    Y := Yvalues^[1];
                end;
                4:
                begin
                    ReAllocmem(XValues, Sizeof(XValues^[1]) * NumPoints);
                    NumPoints := InterpretDblArray(Param, NumPoints, XValues);
                    X := Xvalues^[1];
                end;
                5:
                    DoCSVFile(Param);   // file of x,y points, one to a line
                6:
                    DoSngFile(Param);
                7:
                    DoDblFile(Param);
                8:
                    X := Parser.dblvalue;
                9:
                    Y := Parser.dblvalue;
                10:
                    FXshift := Parser.dblvalue;
                11:
                    FYshift := Parser.dblvalue;
                12:
                    FXscale := Parser.dblvalue;
                13:
                    FYscale := Parser.dblvalue;
            else
           // Inherited parameters
                ClassEdit(ActiveXYcurveObj, ParamPointer - NumPropsThisClass)
            end;

            case ParamPointer of
                5..7:
                begin
                    X := Xvalues^[1];
                    Y := Yvalues^[1];
                end;
            end;

            case ParamPointer of
                2..7:
                begin
                    ArrayPropertyIndex := ParamPointer;
                    NumPoints := FNumPoints;  // Keep Properties in order for save command
                    LastValueAccessed := 1;
                end;
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end; {While}

    end; {WITH}
end;

function TXYcurve.Find(const ObjName: String): Pointer;
begin
    if (Length(ObjName) = 0) or (CompareText(ObjName, 'none') = 0) then
        Result := NIL
    else
        Result := inherited Find(ObjName);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TXYcurve.MakeLike(const CurveName: String): Integer;
var
    OtherXYCurve: TXYcurveObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this curve in the present collection}
    OtherXYCurve := Find(CurveName);
    if OtherXYCurve <> NIL then
        with ActiveXYcurveObj do
        begin
            NumPoints := OtherXYCurve.NumPoints;
            ReAllocmem(XValues, Sizeof(XValues^[1]) * NumPoints);
            ReAllocmem(YValues, Sizeof(YValues^[1]) * NumPoints);
            for i := 1 to NumPoints do
                XValues^[i] := OtherXYCurve.XValues^[i];
            for i := 1 to NumPoints do
                YValues^[i] := OtherXYCurve.YValues^[i];

            FXshift := OtherXYCurve.FXshift;
            FYshift := OtherXYCurve.FYshift;
            FXscale := OtherXYCurve.FXscale;
            FYscale := OtherXYCurve.FYscale;

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherXYCurve.PropertyValue[i];
        end
    else
        DoSimpleMsg('Error in XYCurve MakeLike: "' + CurveName + '" Not Found.', 611);


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TXYcurve.Init(Handle: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TXYcurve.Init', -1);
    Result := 0;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TXYcurve.Get_Code: String;  // Returns active line code string
var
    XYCurveObj: TXYcurveObj;

begin

    XYCurveObj := ElementList.Active;
    Result := XYCurveObj.Name;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TXYcurve.Set_Code(const Value: String);  // sets the  active TShape

var
    XYCurveObj: TXYcurveObj;

begin

    ActiveXYcurveObj := NIL;
    XYCurveObj := ElementList.First;
    while XYCurveObj <> NIL do
    begin

        if CompareText(XYCurveObj.Name, Value) = 0 then
        begin
            ActiveXYcurveObj := XYCurveObj;
            Exit;
        end;

        XYCurveObj := ElementList.Next;
    end;

    DoSimpleMsg('XYCurve: "' + Value + '" not Found.', 612);

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TXYcurve.DoCSVFile(const FileName: String);

var
    F: Textfile;
    i: Integer;
    s: String;

begin
    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 613);
        CloseFile(F);
        Exit;
    end;

    try

        with ActiveXYcurveObj do
        begin
            ReAllocmem(XValues, Sizeof(XValues^[1]) * NumPoints);
            ReAllocmem(YValues, Sizeof(YValues^[1]) * NumPoints);
            i := 0;
            while (not EOF(F)) and (i < FNumPoints) do
            begin
                Inc(i);
                Readln(F, s); // read entire line  and parse with AuxParser
            {AuxParser allows commas or white space}
                with AuxParser do
                begin
                    CmdString := s;
                    NextParam;
                    XValues^[i] := DblValue;
                    NextParam;
                    YValues^[i] := DblValue;
                end;
            end;
            CloseFile(F);
            if i <> FNumPoints then
                NumPoints := i;
        end;

    except
        On E: Exception do
        begin
            DoSimpleMsg('Error Processing XYCurve CSV File: "' + FileName + '. ' + E.Message, 614);
            CloseFile(F);
            Exit;
        end;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TXYcurve.DoSngFile(const FileName: String);
var
    F: file of Single;
    sX,
    sY: Single;
    i: Integer;

begin
    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 615);
        CloseFile(F);
        Exit;
    end;

    try
        with ActiveXYcurveObj do
        begin
            ReAllocmem(XValues, Sizeof(XValues^[1]) * NumPoints);
            ReAllocmem(YValues, Sizeof(YValues^[1]) * NumPoints);
            i := 0;
            while (not EOF(F)) and (i < FNumPoints) do
            begin
                Inc(i);
                Read(F, sX);
                XValues^[i] := sX;
                Read(F, sY);
                YValues^[i] := sY;
            end;
            CloseFile(F);
            if i <> FNumPoints then
                NumPoints := i;
        end;
    except
        DoSimpleMsg('Error Processing binary (single) XYCurve File: "' + FileName, 616);
        CloseFile(F);
        Exit;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TXYcurve.DoDblFile(const FileName: String);
var
    F: file of Double;
    i: Integer;

begin

    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 617);
        CloseFile(F);
        Exit;
    end;

    try
        with ActiveXYcurveObj do
        begin
            ReAllocmem(XValues, Sizeof(XValues^[1]) * NumPoints);
            ReAllocmem(YValues, Sizeof(YValues^[1]) * NumPoints);
            i := 0;
            while (not EOF(F)) and (i < FNumPoints) do
            begin
                Inc(i);
                Read(F, XValues^[i]);
                Read(F, YValues^[i]);
            end;
            CloseFile(F);
            if i <> FNumPoints then
                NumPoints := i;
        end;
    except
        DoSimpleMsg('Error Processing binary (double) XYCurve File: "' + FileName, 618);
        CloseFile(F);
        Exit;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TTShape Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

    ArrayPropertyIndex := 0;

    InitPropertyValues(0);

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TXYcurveObj.Destroy;
begin

    if Assigned(XValues) then
        ReallocMem(XValues, 0);
    if Assigned(YValues) then
        ReallocMem(YValues, 0);
    inherited destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TXYcurveObj.GetYValue(X: Double): Double;

// This function returns the interpolated Y value for the given X.
// If no points exist in the curve, the result is  0.0
// If Xvalue is outside the range of defined X values,
// the curve is extrapolated from the Ends.

var
    i: Integer;

begin

    Result := 0.0;    // default return value if no points in curve

    if FNumPoints > 0 then         // Handle Exceptional cases
        if FNumPoints = 1 then
            Result := YValues^[1]
        else
        begin

    { Start with previous value accessed under the assumption that most
      of the time, the values won't change much}

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


procedure TXYcurveObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            case i of
                3, 4:
                    Writeln(F, '~ ', PropertyName^[i], '=(', PropertyValue[i], ')');
            else
                Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
            end;
        end;


end;

function TXYcurveObj.GetPropertyValue(Index: Integer): String;
var
    i: Integer;
begin
    case Index of
        2..4:
            Result := '[';
    else
        Result := '';
    end;

    case Index of
        2:
            if (XValues <> NIL) and (YValues <> NIL) then
                for i := 1 to FNumPoints do
                    Result := Result + Format('%.8g, %.8g ', [XValues^[i], YValues^[i]])
            else
                Result := '0, 0';
        3:
            if (YValues <> NIL) then
                for i := 1 to FNumPoints do
                    Result := Result + Format('%-g, ', [YValues^[i]])
            else
                Result := '0';
        4:
            if (XValues <> NIL) then
                for i := 1 to FNumPoints do
                    Result := Result + Format('%-g, ', [XValues^[i]])
            else
                Result := '0';
        8:
            Result := Format('%.8g', [Get_X]);
        9:
            Result := Format('%.8g', [Get_Y]);
        10:
            Result := Format('%.8g', [FXshift]);
        11:
            Result := Format('%.8g', [FYshift]);
        12:
            Result := Format('%.8g', [FXscale]);
        13:
            Result := Format('%.8g', [FYscale]);
    else
        Result := inherited GetPropertyValue(index);
    end;

    case Index of
        2..4:
            Result := Result + ']';
    else
    end;
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

    if FNumPoints > 0 then
        if FNumPoints = 1 then
            Result := XValues^[1]
        else
        begin
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
                begin
                    Result := InterpolatePoints(1, 2, Y, YValues, XValues);
                end
                else
                begin
                    Result := InterpolatePoints(FNumPoints - 1, FNumPoints, Y, YValues, XValues);
                end
            end
            else
            begin // decreasing Y values
                if Y >= YValues^[1] then
                begin
                    Result := InterpolatePoints(1, 2, Y, YValues, XValues);
                end
                else
                begin
                    Result := InterpolatePoints(FNumPoints - 1, FNumPoints, Y, YValues, XValues);
                end
            end;
        end;
    {
  IF FNumPoints>0 Then         // Handle Exceptional cases
  IF FNumPoints=1 Then Result := XValues^[1]
  ELSE
    Begin

     IF (YValues^[LastValueAccessed] > Y) Then LastValueAccessed := 1; // Start over from Beginning

     // if off the curve for the first point, extrapolate from the first two points
     IF (LastValueAccessed = 1) AND (YValues[1] > Y) Then Begin
         Result := InterpolatePoints(1, 2, Y, YValues, XValues);
         Exit;
     End;

     FOR i := LastValueAccessed+1 TO FNumPoints do
       Begin
         IF (Abs(YValues^[i]-Y) < 0.00001) Then  // If close to an actual point, just use it.
           Begin
               Result := XValues^[i];
               LastValueAccessed := i;
               Exit;
           End
         ELSE IF (YValues^[i] > Y) Then
// INTERPOLATE
           Begin
             LastValueAccessed := i-1;
             Result := InterpolatePoints(i, LastValueAccessed, Y, YValues, XValues);
             Exit ;
           End;
       End;

     // If we fall through the loop, Extrapolate from last two points
     LastValueAccessed := FNumPoints-1;
     Result := InterpolatePoints(FNumPoints, LastValueAccessed,  Y, YValues, XValues);
    End;
    }
end;

procedure TXYcurveObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := '0';     // Number of points to expect
    PropertyValue[2] := '';
    PropertyValue[3] := '';
    PropertyValue[4] := '';
    PropertyValue[5] := '';
    PropertyValue[6] := '';
    PropertyValue[7] := '';
    PropertyValue[8] := '';
    PropertyValue[9] := '';
    PropertyValue[10] := '0';
    PropertyValue[11] := '0';
    PropertyValue[12] := '1';
    PropertyValue[13] := '1';

    inherited  InitPropertyValues(NumPropsThisClass);

end;


function TXYcurveObj.InterpolatePoints(i, j: Integer; X: Double; Xarray,
    Yarray: pDoubleArray): Double;

var
    Den: Double;
begin
    Den := (Xarray^[i] - Xarray^[j]);
    if Den <> 0.0 then
        Result := Yarray^[j] + (X - Xarray^[j]) / Den * (Yarray^[i] - Yarray^[j])
    else
        Result := Yarray^[i]; // Y is undefined, return ith value
end;

(*************************************************

PROCEDURE TXYcurveObj.SaveToDblFile;

Var
   F:File of Double;
   i:Integer;
   Fname :String;
Begin
   If Assigned(TValues) then  Begin
    TRY
      FName := Format('%s.dbl',[Name]);
      AssignFile(F, Fname);
      Rewrite(F);
      FOR i := 1 to NumPoints Do  Write(F, TValues^[i]);
      GlobalResult := 'Temp=[dblfile='+FName+']';
    FINALLY
      CloseFile(F);
    End;

   End
   ELSE DoSimpleMsg('Tshape.'+Name + ' Temperatures not defined.', 622);
End;

PROCEDURE TXYcurveObj.SaveToSngFile;

Var
   F:File of Single;
   i:Integer;
   Fname :String;
   Temp  :Single;

Begin
   If Assigned(TValues) then  Begin
    TRY
        FName := Format('%s.sng',[Name]);
        AssignFile(F, Fname);
        Rewrite(F);
        FOR i := 1 to NumPoints Do  Begin
            Temp := TValues^[i] ;
            Write(F, Temp);
        End;
        GlobalResult := 'Temp=[sngfile='+FName+']';
    FINALLY
      CloseFile(F);
    End;


   End
   ELSE DoSimpleMsg('Tshape.'+Name + ' Temperatures not defined.', 623);


End;

****************************************************)
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

procedure TXYcurveObj.SaveWrite(var F: TextFile);

{Override standard SaveWrite}
{Transformer structure not conducive to standard means of saving}
var
    iprop: Integer;
begin
   {Write only properties that were explicitly set in the final order they were actually set}

   {Write Npts out first so that arrays get allocated properly}
    Write(F, Format(' Npts=%d', [NumPoints]));
    iProp := GetNextPropertySet(0); // Works on ActiveDSSObject
    while iProp > 0 do
    begin
        with ParentClass do
       {Trap npts= and write out array properties instead}
            case RevPropertyIdxMap[iProp] of
                1:
{Ignore Npts};

            else
                Write(F, Format(' %s=%s', [PropertyName^[RevPropertyIdxMap[iProp]], CheckForBlanks(PropertyValue[iProp])]));
            end;
        iProp := GetNextPropertySet(iProp);
    end;

end;

procedure TXYcurveObj.Set_NumPoints(const Value: Integer);
begin
    PropertyValue[1] := IntToStr(Value);   // Update property list variable

    // Reset array property values to keep them in propoer order in Save

    if ArrayPropertyIndex > 0 then
        PropertyValue[ArrayPropertyIndex] := PropertyValue[ArrayPropertyIndex];

    FNumPoints := Value;   // Now assign the value

    // reallocate the curve memory
    ReAllocmem(YValues, Sizeof(YValues^[1]) * FNumPoints);
    ReAllocmem(XValues, Sizeof(XValues^[1]) * FNumPoints);

end;


end.
