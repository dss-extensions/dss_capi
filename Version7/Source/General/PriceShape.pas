unit PriceShape;

{
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{  2-16-2011 Converted from TempShape.
   Price shapes would generally be defined to correspond to loadshapes

}

interface

{The PriceShape object is a general DSS object used by all circuits
 as a reference for obtaining yearly, daily, and other Price shapes.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values may be retrieved by setting the Code Property in the PriceShape Class.
 This sets the active PriceShape object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.  Or you
 can pick up the ActivePriceShapeObj object and save the direct reference to the object.

 PriceShapes default to fixed interval data (like Loadshapes).  If the Interval is specified to be 0.0,
 then both time and price data are expected.  If the Interval is  greater than 0.0,
 the user specifies only the prices.  The Hour command is ignored and the files are
 assumed to contain only the price data.

 The Interval may also be specified in seconds (sinterval) or minutes (minterval).

 The user may place the data in CSV or binary files as well as passing through the
 command interface. Obviously, for large amounts of data such as 8760 load curves, the
 command interface is cumbersome.  CSV files are text separated by commas, one interval to a line.
 There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.

 For fixed interval data, only the price values are expected.  Therefore, the CSV format would
 contain only one number per line.  The two binary formats are packed.

 For variable interval data, (hour, price) pairs are expected in both formats.

 The Mean and Std Deviation are automatically computed upon demand when new series of points is entered.



 }

uses
    Command,
    DSSClass,
    DSSObject,
    Arraydef;

type

    TPriceShape = class(TDSSClass)
    PRIVATE

        function Get_Code: String;  // Returns active PriceShape string
        procedure Set_Code(const Value: String);  // sets the  active PriceShape

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

        function Find(const ObjName: String): Pointer; OVERRIDE;  // Find an obj of this class by name

        procedure TOPExport(ObjName: String); // can export this to top for plotting

       // Set this property to point ActivePriceShapeObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;

    end;

    TPriceShapeObj = class(TDSSObject)
    PRIVATE
        LastValueAccessed,
        FNumPoints: Integer;  // Number of points in curve
        ArrayPropertyIndex: Integer;

        FStdDevCalculated: Boolean;
        FMean,
        FStdDev: Double;

        function Get_Interval: Double;
        procedure Set_NumPoints(const Value: Integer);
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
        PriceValues: pDoubleArray;  // Prices

        constructor Create(ParClass: TDSSClass; const PriceShapeName: String);
        destructor Destroy; OVERRIDE;

        function GetPrice(hr: Double): Double;  // Get Prices at specified time, hr
        function Price(i: Integer): Double;  // get Prices by index
        function Hour(i: Integer): Double;  // get hour corresponding to point index


        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

        property NumPoints: Integer READ FNumPoints WRITE Set_NumPoints;
        property PresentInterval: Double READ Get_Interval;
        property Mean: Double READ Get_Mean WRITE Set_Mean;
        property StdDev: Double READ Get_StdDev WRITE Set_StdDev;

    end;

var
    ActivePriceShapeObj: TPriceShapeObj;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    MathUtil,
    Utilities,
    Classes,
    TOPExport,
    Math,
    PointerList;

const
    NumPropsThisClass = 12;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TPriceShape.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    Class_Name := 'PriceShape';
    DSSClassType := DSS_OBJECT;

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TPriceShape.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TPriceShape.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names
    PropertyName[1] := 'npts';     // Number of points to expect
    PropertyName[2] := 'interval'; // default = 1.0;
    PropertyName[3] := 'price';     // vector of price values
    PropertyName[4] := 'hour';     // vector of hour values
    PropertyName[5] := 'mean';     // set the mean Price (otherwise computed)
    PropertyName[6] := 'stddev';   // set the std dev of the Price (otherwise computed)
    PropertyName[7] := 'csvfile';  // Switch input to a csvfile
    PropertyName[8] := 'sngfile';  // switch input to a binary file of singles
    PropertyName[9] := 'dblfile';    // switch input to a binary file of singles
    PropertyName[10] := 'sinterval'; // Interval in seconds
    PropertyName[11] := 'minterval'; // Interval in minutes
    PropertyName[12] := 'action';    //

     // define Property help values

    PropertyHelp[1] := 'Max number of points to expect in price shape vectors. This gets reset to the number of Price values ' +
        'found if less than specified.';     // Number of points to expect
    PropertyHelp[2] := 'Time interval for fixed interval data, hrs. Default = 1. ' +
        'If Interval = 0 then time data (in hours) may be at irregular intervals and time value must be specified using either the Hour property or input files. ' +
        'Then values are interpolated when Interval=0, but not for fixed interval data.  ' + CRLF + CRLF +
        'See also "sinterval" and "minterval".'; // default = 1.0;
    PropertyHelp[3] := 'Array of Price values.  Units should be compatible with the object using the data. ' +
        'You can also use the syntax: ' + CRLF +
        'Price = (file=filename)     !for text file one value per line' + CRLF +
        'Price = (dblfile=filename)  !for packed file of doubles' + CRLF +
        'Price = (sngfile=filename)  !for packed file of singles ' + CRLF + CRLF +
        'Note: this property will reset Npts if the  number of values in the files are fewer.';     // vextor of hour values
    PropertyHelp[4] := 'Array of hour values. Only necessary to define this property for variable interval data.' +
        ' If the data are fixed interval, do not use this property. ' +
        'You can also use the syntax: ' + CRLF +
        'hour = (file=filename)     !for text file one value per line' + CRLF +
        'hour = (dblfile=filename)  !for packed file of doubles' + CRLF +
        'hour = (sngfile=filename)  !for packed file of singles ';     // vextor of hour values
    PropertyHelp[5] := 'Mean of the Price curve values.  This is computed on demand the first time a ' +
        'value is needed.  However, you may set it to another value independently. ' +
        'Used for Monte Carlo load simulations.';     // set the mean (otherwise computed)
    PropertyHelp[6] := 'Standard deviation of the Prices.  This is computed on demand the first time a ' +
        'value is needed.  However, you may set it to another value independently.' +
        'Is overwritten if you subsequently read in a curve' + CRLF + CRLF +
        'Used for Monte Carlo load simulations.';   // set the std dev (otherwise computed)
    PropertyHelp[7] := 'Switch input of  Price curve data to a csv file ' +
        'containing (hour, Price) points, or simply (Price) values for fixed time interval data, one per line. ' +
        'NOTE: This action may reset the number of points to a lower value.';   // Switch input to a csvfile
    PropertyHelp[8] := 'Switch input of  Price curve data to a binary file of singles ' +
        'containing (hour, Price) points, or simply (Price) values for fixed time interval data, packed one after another. ' +
        'NOTE: This action may reset the number of points to a lower value.';  // switch input to a binary file of singles
    PropertyHelp[9] := 'Switch input of  Price curve data to a binary file of doubles ' +
        'containing (hour, Price) points, or simply (Price) values for fixed time interval data, packed one after another. ' +
        'NOTE: This action may reset the number of points to a lower value.';   // switch input to a binary file of singles
    PropertyHelp[10] := 'Specify fixed interval in SECONDS. Alternate way to specify Interval property.';
    PropertyHelp[11] := 'Specify fixed interval in MINUTES. Alternate way to specify Interval property.';
    PropertyHelp[12] := '{DblSave | SngSave} After defining Price curve data... ' +
        'Setting action=DblSave or SngSave will cause the present "Price" values to be written to ' +
        'either a packed file of double or single. The filename is the PriceShape name. '; // Action

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TPriceShape.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    with ActiveCircuit do
    begin
        ActiveDSSObject := TPriceShapeObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TPriceShape.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin
    Result := 0;
  // continue parsing with contents of Parser
    ActivePriceShapeObj := ElementList.Active;
    ActiveDSSObject := ActivePriceShapeObj;

    with ActivePriceShapeObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 58610);
                1:
                    NumPoints := Parser.Intvalue;
                2:
                    Interval := Parser.DblValue;
                3:
                begin
                    ReAllocmem(PriceValues, Sizeof(PriceValues^[1]) * NumPoints);
                 // Allow possible Resetting (to a lower value) of num points when specifying Prices not Hours
                    NumPoints := InterpretDblArray(Param, NumPoints, PriceValues);   // Parser.ParseAsVector(Npts, Prices);
                end;
                4:
                begin
                    ReAllocmem(Hours, Sizeof(Hours^[1]) * NumPoints);
                    InterpretDblArray(Param, NumPoints, Hours);   // Parser.ParseAsVector(Npts, Hours);
                end;
                5:
                    Mean := Parser.DblValue;
                6:
                    StdDev := Parser.DblValue;
                7:
                    DoCSVFile(Param);
                8:
                    DoSngFile(Param);
                9:
                    DoDblFile(Param);
                10:
                    Interval := Parser.DblValue / 3600.0;  // Convert seconds to hr
                11:
                    Interval := Parser.DblValue / 60.0;  // Convert minutes to hr
                12:
                    case lowercase(Param)[1] of
                        'd':
                            SaveToDblFile;
                        's':
                            SaveToSngFile;
                    end;
            else
           // Inherited parameters
                ClassEdit(ActivePriceShapeObj, ParamPointer - NumPropsThisClass)
            end;

            case ParamPointer of
                3, 7, 8, 9:
                begin
                    FStdDevCalculated := FALSE;   // now calculated on demand
                    ArrayPropertyIndex := ParamPointer;
                    NumPoints := FNumPoints;  // Keep Properties in order for save command
                end;

            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end; {While}

    end; {WITH}
end;

function TPriceShape.Find(const ObjName: String): Pointer;
begin
    if (Length(ObjName) = 0) or (CompareText(ObjName, 'none') = 0) then
        Result := NIL
    else
        Result := inherited Find(ObjName);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TPriceShape.MakeLike(const ShapeName: String): Integer;
var
    OtherPriceShape: TPriceShapeObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this line code in the present collection}
    OtherPriceShape := Find(ShapeName);
    if OtherPriceShape <> NIL then
        with ActivePriceShapeObj do
        begin
            NumPoints := OtherPriceShape.NumPoints;
            Interval := OtherPriceShape.Interval;
            ReallocMem(PriceValues, SizeOf(PriceValues^[1]) * NumPoints);
            for i := 1 to NumPoints do
                PriceValues^[i] := OtherPriceShape.PriceValues^[i];
            if Interval > 0.0 then
                ReallocMem(Hours, 0)
            else
            begin
                ReallocMem(Hours, SizeOf(Hours^[1]) * NumPoints);
                for i := 1 to NumPoints do
                    Hours^[i] := OtherPriceShape.Hours^[i];
            end;

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherPriceShape.PropertyValue[i];
        end
    else
        DoSimpleMsg('Error in PriceShape MakeLike: "' + ShapeName + '" Not Found.', 58611);


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TPriceShape.Init(Handle: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TPriceShape.Init', -1);
    Result := 0;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TPriceShape.Get_Code: String;  // Returns active line code string
var
    PriceShapeObj: TPriceShapeObj;

begin

    PriceShapeObj := ElementList.Active;
    Result := PriceShapeObj.Name;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TPriceShape.Set_Code(const Value: String);  // sets the  active PriceShape

var
    PriceShapeObj: TPriceShapeObj;

begin

    ActivePriceShapeObj := NIL;
    PriceShapeObj := ElementList.First;
    while PriceShapeObj <> NIL do
    begin

        if CompareText(PriceShapeObj.Name, Value) = 0 then
        begin
            ActivePriceShapeObj := PriceShapeObj;
            Exit;
        end;

        PriceShapeObj := ElementList.Next;
    end;

    DoSimpleMsg('PriceShape: "' + Value + '" not Found.', 58612);

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TPriceShape.DoCSVFile(const FileName: String);

var
    F: Textfile;
    i: Integer;
    s: String;

begin
    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 58613);
        CloseFile(F);
        Exit;
    end;

    try

        with ActivePriceShapeObj do
        begin
            ReAllocmem(PriceValues, Sizeof(PriceValues^[1]) * NumPoints);
            if Interval = 0.0 then
                ReAllocmem(Hours, Sizeof(Hours^[1]) * NumPoints);
            i := 0;
            while (not EOF(F)) and (i < FNumPoints) do
            begin
                Inc(i);
                Readln(F, s); // read entire line  and parse with AuxParser
            {AuxParser allows commas or white space}
                with AuxParser do
                begin
                    CmdString := s;
                    if Interval = 0.0 then
                    begin
                        NextParam;
                        Hours^[i] := DblValue;
                    end;
                    NextParam;
                    PriceValues^[i] := DblValue;
                end;
            end;
            CloseFile(F);
            if i <> FNumPoints then
                NumPoints := i;
        end;

    except
        On E: Exception do
        begin
            DoSimpleMsg('Error Processing CSV File: "' + FileName + '. ' + E.Message, 58614);
            CloseFile(F);
            Exit;
        end;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TPriceShape.DoSngFile(const FileName: String);
var
    F: file of Single;
    Hr,
    M: Single;
    i: Integer;

begin
    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 58615);
        CloseFile(F);
        Exit;
    end;

    try
        with ActivePriceShapeObj do
        begin
            ReAllocmem(PriceValues, Sizeof(PriceValues^[1]) * NumPoints);
            if Interval = 0.0 then
                ReAllocmem(Hours, Sizeof(Hours^[1]) * NumPoints);
            i := 0;
            while (not EOF(F)) and (i < FNumPoints) do
            begin
                Inc(i);
                if Interval = 0.0 then
                begin
                    Read(F, Hr);
                    Hours^[i] := Hr;
                end;
                Read(F, M);
                PriceValues^[i] := M;
            end;
            CloseFile(F);
            if i <> FNumPoints then
                NumPoints := i;
        end;
    except
        DoSimpleMsg('Error Processing PriceShape File: "' + FileName, 58616);
        CloseFile(F);
        Exit;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TPriceShape.DoDblFile(const FileName: String);
var
    F: file of Double;
    i: Integer;

begin
    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 58617);
        CloseFile(F);
        Exit;
    end;

    try
        with ActivePriceShapeObj do
        begin
            ReAllocmem(PriceValues, Sizeof(PriceValues^[1]) * NumPoints);
            if Interval = 0.0 then
                ReAllocmem(Hours, Sizeof(Hours^[1]) * NumPoints);
            i := 0;
            while (not EOF(F)) and (i < FNumPoints) do
            begin
                Inc(i);
                if Interval = 0.0 then
                    Read(F, Hours^[i]);
                Read(F, PriceValues^[i]);
            end;
            CloseFile(F);
            if i <> FNumPoints then
                NumPoints := i;
        end;
    except
        DoSimpleMsg('Error Processing PriceShape File: "' + FileName, 58618);
        CloseFile(F);
        Exit;
    end;


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TPriceShape Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

    ArrayPropertyIndex := 0;

    InitPropertyValues(0);

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TPriceShapeObj.Destroy;
begin

    ReallocMem(Hours, 0);
    if Assigned(PriceValues) then
        ReallocMem(PriceValues, 0);
    inherited destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

    if FNumPoints > 0 then         // Handle Exceptional cases
        if FNumPoints = 1 then
        begin
            Result := PriceValues^[1];
        end
        else
        begin
            if Interval > 0.0 then
            begin
                Index := round(hr / Interval);
                if Index > FNumPoints then
                    Index := Index mod FNumPoints;  // Wrap around using remainder
                if Index = 0 then
                    Index := FNumPoints;
                Result := PriceValues^[Index];
            end
            else
            begin
          // For random interval

        { Start with previous value accessed under the assumption that most
          of the time, this FUNCTION will be called sequentially}

          {Normalize Hr to max hour in curve to get wraparound}
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
        end;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TPriceShapeObj.CalcMeanandStdDev;

begin

    if FNumPoints > 0 then
        if Interval > 0.0 then
            RCDMeanandStdDev(PriceValues, FNumPoints, FMean, FStdDev)
        else
            CurveMeanAndStdDev(PriceValues, Hours, FNumPoints, FMean, FStdDev);

    PropertyValue[5] := Format('%.8g', [FMean]);
    PropertyValue[6] := Format('%.8g', [FStdDev]);

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


procedure TPriceShapeObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;


end;

function TPriceShapeObj.GetPropertyValue(Index: Integer): String;
begin
    Result := '';

    case Index of
        2:
            Result := Format('%.8g', [Interval]);
        3:
            Result := GetDSSArray_Real(FNumPoints, PriceValues);
        4:
            if Hours <> NIL then
                Result := GetDSSArray_Real(FNumPoints, Hours);
        5:
            Result := Format('%.8g', [Mean]);
        6:
            Result := Format('%.8g', [StdDev]);
        10:
            Result := Format('%.8g', [Interval * 3600.0]);
        11:
            Result := Format('%.8g', [Interval * 60.0]);
    else
        Result := inherited GetPropertyValue(index);
    end;

end;

procedure TPriceShapeObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := '0';     // Number of points to expect
    PropertyValue[2] := '1'; // default = 1.0 hr;
    PropertyValue[3] := '';     // vector of multiplier values
    PropertyValue[4] := '';     // vextor of hour values
    PropertyValue[5] := '0';     // set the mean (otherwise computed)
    PropertyValue[6] := '0';   // set the std dev (otherwise computed)
    PropertyValue[7] := '';   // Switch input to a csvfile
    PropertyValue[8] := '';  // switch input to a binary file of singles
    PropertyValue[9] := '';   // switch input to a binary file of singles
    PropertyValue[10] := '3600';   // seconds
    PropertyValue[11] := '60';     // minutes
    PropertyValue[12] := ''; // action option .


    inherited  InitPropertyValues(NumPropsThisClass);

end;


procedure TPriceShape.TOPExport(ObjName: String);

var
    NameList, CNames: TStringList;
    Vbuf, CBuf: pDoubleArray;
    Obj: TPriceShapeObj;
    MaxPts, i, j: Integer;
    MaxTime, MinInterval, Hr_Time: Double;
    ObjList: TPointerList;

begin
    TOPTransferFile.FileName := GetOutputDirectory + 'TOP_PriceShape.STO';
    try
        TOPTransferFile.Open;
    except
        ON E: Exception do
        begin
            DoSimpleMsg('TOP Transfer File Error: ' + E.message, 58619);
            try
                TopTransferFile.Close;
            except
              {OK if Error}
            end;
            Exit;
        end;
    end;

     {Send only fixed interval data}

    ObjList := TPointerList.Create(10);
    NameList := TStringList.Create;
    CNames := TStringList.Create;

     {Make a List of fixed interval data where the interval is greater than 1 minute}
    if CompareText(ObjName, 'ALL') = 0 then
    begin
        Obj := ElementList.First;
        while Obj <> NIL do
        begin
            if Obj.Interval > (1.0 / 60.0) then
                ObjList.Add(Obj);
            Obj := ElementList.Next;
        end;
    end
    else
    begin
        Obj := Find(ObjName);
        if Obj <> NIL then
        begin
            if Obj.Interval > (1.0 / 60.0) then
                ObjList.Add(Obj)
            else
                DoSimpleMsg('PriceShape.' + ObjName + ' is not hourly fixed interval.', 58620);
        end
        else
        begin
            DoSimpleMsg('PriceShape.' + ObjName + ' not found.', 58621);
        end;

    end;

     {If none found, exit}
    if ObjList.ListSize > 0 then
    begin

       {Find Max number of points}
        MaxTime := 0.0;
        MinInterval := 1.0;
        Obj := ObjList.First;
        while Obj <> NIL do
        begin
            MaxTime := Max(MaxTime, Obj.NumPoints * Obj.Interval);
            MinInterval := Min(MinInterval, Obj.Interval);
            NameList.Add(Obj.Name);
            Obj := ObjList.Next;
        end;
      // SetLength(Xarray, maxPts);
        MaxPts := Round(MaxTime / MinInterval);

        TopTransferFile.WriteHeader(0.0, MaxTime, MinInterval, ObjList.ListSize, 0, 16, 'DSS (TM), Electrotek Concepts (R)');
        TopTransferFile.WriteNames(NameList, CNames);

        Hr_Time := 0.0;

        VBuf := AllocMem(Sizeof(Double) * ObjList.ListSize);
        CBuf := AllocMem(Sizeof(Double) * 1);   // just a dummy -- Cbuf is ignored here

        for i := 1 to MaxPts do
        begin
            for j := 1 to ObjList.ListSize do
            begin
                Obj := ObjList.Get(j);
                VBuf^[j] := Obj.GetPrice(Hr_Time);
            end;
            TopTransferFile.WriteData(HR_Time, Vbuf, Cbuf);
            HR_Time := HR_Time + MinInterval;
        end;

        TopTransferFile.Close;
        TopTransferFile.SendToTop;
        Reallocmem(Vbuf, 0);
        Reallocmem(Cbuf, 0);
    end;

    ObjList.Free;
    NameList.Free;
    CNames.Free;
end;


procedure TPriceShapeObj.SaveToDblFile;

var
    F: file of Double;
    i: Integer;
    Fname: String;
begin
    if Assigned(PriceValues) then
    begin
        try
            FName := Format('%s.dbl', [Name]);
            AssignFile(F, Fname);
            Rewrite(F);
            for i := 1 to NumPoints do
                Write(F, PriceValues^[i]);
            GlobalResult := 'Price=[dblfile=' + FName + ']';
        finally
            CloseFile(F);
        end;

    end
    else
        DoSimpleMsg('PriceShape.' + Name + ' Prices not defined.', 58622);
end;

procedure TPriceShapeObj.SaveToSngFile;

var
    F: file of Single;
    i: Integer;
    Fname: String;
    sngPrice: Single;

begin
    if Assigned(PriceValues) then
    begin
        try
            FName := Format('%s.sng', [Name]);
            AssignFile(F, Fname);
            Rewrite(F);
            for i := 1 to NumPoints do
            begin
                sngPrice := PriceValues^[i];
                Write(F, sngPrice);
            end;
            GlobalResult := 'Price=[sngfile=' + FName + ']';
        finally
            CloseFile(F);
        end;


    end
    else
        DoSimpleMsg('PriceShape.' + Name + ' Prices not defined.', 58623);


end;


procedure TPriceShapeObj.Set_Mean(const Value: Double);
begin
    FStdDevCalculated := TRUE;
    FMean := Value;
end;

procedure TPriceShapeObj.Set_NumPoints(const Value: Integer);
begin
    PropertyValue[1] := IntToStr(Value);   // Update property list variable

      // Reset array property values to keep them in propoer order in Save

    if ArrayPropertyIndex > 0 then
        PropertyValue[ArrayPropertyIndex] := PropertyValue[ArrayPropertyIndex];

    FNumPoints := Value;   // Now assign the value
end;

procedure TPriceShapeObj.Set_StdDev(const Value: Double);
begin
    FStdDevCalculated := TRUE;
    FStdDev := Value;
end;

end.
