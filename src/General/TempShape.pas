unit TempShape;

{
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{  2-13-2011 Converted from Loadshape.
   Temperature shapes (Tshapes) would generally be defined to correspond to loadshapes

}

interface

{The Tshape object is a general DSS object used by all circuits
 as a reference for obtaining yearly, daily, and other Temperature shapes.

 The values are set by the normal New and Edit PROCEDUREs for any DSS object.

 The values may be retrieved by setting the Code Property in the Tshape Class.
 This sets the active Tshape object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.  Or you
 can pick up the ActiveTShapeObj object and save the direct reference to the object.

 Tshapes default to fixed interval data (like Loadshapes).  If the Interval is specified to be 0.0,
 then both time and temperature data are expected.  If the Interval is  greater than 0.0,
 the user specifies only the Temperatures.  The Hour command is ignored and the files are
 assumed to contain only the temperature data.

 The Interval may also be specified in seconds (sinterval) or minutes (minterval).

 The user may place the data in CSV or binary files as well as passing through the
 command interface. Obviously, for large amounts of data such as 8760 load curves, the
 command interface is cumbersome.  CSV files are text separated by commas, one interval to a line.
 There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.

 For fixed interval data, only the Temperature values are expected.  Therefore, the CSV format would
 contain only one number per line.  The two binary formats are packed.

 For variable interval data, (hour, Temperature) pairs are expected in both formats.

 The Mean and Std Deviation are automatically computed upon demand when new series of points is entered.



 }

uses
    Classes,
    Command,
    DSSClass,
    DSSObject,
    Arraydef;

type

    TTShape = class(TDSSClass)
    PRIVATE

        function Get_Code: String;  // Returns active TShape string
        procedure Set_Code(const Value: String);  // sets the  active TShape

        procedure DoCSVFile(const FileName: String);
        procedure DoSngFile(const FileName: String);
        procedure DoDblFile(const FileName: String);
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const ShapeName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

        function Find(const ObjName: String; const ChangeActive: Boolean=True): Pointer; OVERRIDE;  // Find an obj of this class by name

       // Set this property to point ActiveTShapeObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;

    end;

    TTShapeObj = class(TDSSObject)
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
        TValues: pDoubleArray;  // Temperatures

        constructor Create(ParClass: TDSSClass; const TShapeName: String);
        destructor Destroy; OVERRIDE;

        function GetTemperature(hr: Double): Double;  // Get Temperatures at specified time, hr
        function Temperature(i: Integer): Double;  // get Temperatures by index
        function Hour(i: Integer): Double;  // get hour corresponding to point index


        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(F: TFileStream; Complete: Boolean); OVERRIDE;

        property NumPoints: Integer READ FNumPoints WRITE Set_NumPoints;
        property PresentInterval: Double READ Get_Interval;
        property Mean: Double READ Get_Mean WRITE Set_Mean;
        property StdDev: Double READ Get_StdDev WRITE Set_StdDev;

    end;


implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    MathUtil,
    Utilities,
    Math,
    DSSPointerList,
    BufStream,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

const
    NumPropsThisClass = 12;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TTShape.Create(dssContext: TDSSContext);  // Creates superstructure for all Line objects
begin
    inherited Create(dssContext);
    Class_Name := 'TShape';
    DSSClassType := DSS_OBJECT;

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TTShape.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTShape.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names
    PropertyName[1] := 'npts';     // Number of points to expect
    PropertyName[2] := 'interval'; // default = 1.0;
    PropertyName[3] := 'temp';     // vector of temperature values
    PropertyName[4] := 'hour';     // vector of hour values
    PropertyName[5] := 'mean';     // set the mean temp (otherwise computed)
    PropertyName[6] := 'stddev';   // set the std dev of the temp (otherwise computed)
    PropertyName[7] := 'csvfile';  // Switch input to a csvfile
    PropertyName[8] := 'sngfile';  // switch input to a binary file of singles
    PropertyName[9] := 'dblfile';    // switch input to a binary file of singles
    PropertyName[10] := 'sinterval'; // Interval in seconds
    PropertyName[11] := 'minterval'; // Interval in minutes
    PropertyName[12] := 'action';    // Interval in minutes

     // define Property help values

    PropertyHelp[1] := 'Max number of points to expect in temperature shape vectors. This gets reset to the number of Temperature values ' +
        'found if less than specified.';     // Number of points to expect
    PropertyHelp[2] := 'Time interval for fixed interval data, hrs. Default = 1. ' +
        'If Interval = 0 then time data (in hours) may be at irregular intervals and time value must be specified using either the Hour property or input files. ' +
        'Then values are interpolated when Interval=0, but not for fixed interval data.  ' + CRLF + CRLF +
        'See also "sinterval" and "minterval".'; // default = 1.0;
    PropertyHelp[3] := 'Array of temperature values.  Units should be compatible with the object using the data. ' +
        'You can also use the syntax: ' + CRLF +
        'Temp = (file=filename)     !for text file one value per line' + CRLF +
        'Temp = (dblfile=filename)  !for packed file of doubles' + CRLF +
        'Temp = (sngfile=filename)  !for packed file of singles ' + CRLF + CRLF +
        'Note: this property will reset Npts if the  number of values in the files are fewer.';     // vextor of hour values
    PropertyHelp[4] := 'Array of hour values. Only necessary to define this property for variable interval data.' +
        ' If the data are fixed interval, do not use this property. ' +
        'You can also use the syntax: ' + CRLF +
        'hour = (file=filename)     !for text file one value per line' + CRLF +
        'hour = (dblfile=filename)  !for packed file of doubles' + CRLF +
        'hour = (sngfile=filename)  !for packed file of singles ';     // vextor of hour values
    PropertyHelp[5] := 'Mean of the temperature curve values.  This is computed on demand the first time a ' +
        'value is needed.  However, you may set it to another value independently. ' +
        'Used for Monte Carlo load simulations.';     // set the mean (otherwise computed)
    PropertyHelp[6] := 'Standard deviation of the temperatures.  This is computed on demand the first time a ' +
        'value is needed.  However, you may set it to another value independently.' +
        'Is overwritten if you subsequently read in a curve' + CRLF + CRLF +
        'Used for Monte Carlo load simulations.';   // set the std dev (otherwise computed)
    PropertyHelp[7] := 'Switch input of  temperature curve data to a csv file ' +
        'containing (hour, Temp) points, or simply (Temp) values for fixed time interval data, one per line. ' +
        'NOTE: This action may reset the number of points to a lower value.';   // Switch input to a csvfile
    PropertyHelp[8] := 'Switch input of  temperature curve data to a binary file of singles ' +
        'containing (hour, Temp) points, or simply (Temp) values for fixed time interval data, packed one after another. ' +
        'NOTE: This action may reset the number of points to a lower value.';  // switch input to a binary file of singles
    PropertyHelp[9] := 'Switch input of  temperature curve data to a binary file of doubles ' +
        'containing (hour, Temp) points, or simply (Temp) values for fixed time interval data, packed one after another. ' +
        'NOTE: This action may reset the number of points to a lower value.';   // switch input to a binary file of singles
    PropertyHelp[10] := 'Specify fixed interval in SECONDS. Alternate way to specify Interval property.';
    PropertyHelp[11] := 'Specify fixed interval in MINUTES. Alternate way to specify Interval property.';
    PropertyHelp[12] := '{DblSave | SngSave} After defining temperature curve data... ' +
        'Setting action=DblSave or SngSave will cause the present "Temp" values to be written to ' +
        'either a packed file of double or single. The filename is the Tshape name. '; // Action

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TTShape.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    DSS.ActiveDSSObject := TTShapeObj.Create(Self, ObjName);
    Result := AddObjectToList(DSS.ActiveDSSObject);
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TTShape.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin
    Result := 0;
  // continue parsing with contents of Parser
    DSS.ActiveTShapeObj := ElementList.Active;
    DSS.ActiveDSSObject := DSS.ActiveTShapeObj;

    with DSS.ActiveTShapeObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 57610);
                1:
                    NumPoints := Parser.Intvalue;
                2:
                    Interval := Parser.DblValue;
                3:
                begin
                    ReAllocmem(TValues, Sizeof(TValues^[1]) * NumPoints);
                 // Allow possible Resetting (to a lower value) of num points when specifying temperatures not Hours
                    NumPoints := InterpretDblArray(Param, NumPoints, TValues);   // Parser.ParseAsVector(Npts, Temperatures);
                end;
                4:
                begin
                    ReAllocmem(Hours, Sizeof(Hours^[1]) * NumPoints);
                    NumPoints := InterpretDblArray(Param, NumPoints, Hours);   // Parser.ParseAsVector(Npts, Hours);
                end;
                5:
                    Mean := Parser.DblValue;
                6:
                    StdDev := Parser.DblValue;
                7:
                    DoCSVFile(AdjustInputFilePath(Param));
                8:
                    DoSngFile(AdjustInputFilePath(Param));
                9:
                    DoDblFile(AdjustInputFilePath(Param));
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
                ClassEdit(DSS.ActiveTShapeObj, ParamPointer - NumPropsThisClass)
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

function TTShape.Find(const ObjName: String; const ChangeActive: Boolean): Pointer;
begin
    if (Length(ObjName) = 0) or (CompareText(ObjName, 'none') = 0) then
        Result := NIL
    else
        Result := inherited Find(ObjName, ChangeActive);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TTShape.MakeLike(const ShapeName: String): Integer;
var
    OtherTShape: TTShapeObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this line code in the present collection}
    OtherTShape := Find(ShapeName);
    if OtherTShape <> NIL then
        with DSS.ActiveTShapeObj do
        begin
            NumPoints := OtherTShape.NumPoints;
            Interval := OtherTShape.Interval;
            ReallocMem(TValues, SizeOf(TValues^[1]) * NumPoints);
            for i := 1 to NumPoints do
                TValues^[i] := OtherTShape.TValues^[i];
            if Interval > 0.0 then
                ReallocMem(Hours, 0)
            else
            begin
                ReallocMem(Hours, SizeOf(Hours^[1]) * NumPoints);
                for i := 1 to NumPoints do
                    Hours^[i] := OtherTShape.Hours^[i];
            end;

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherTShape.PropertyValue[i];
        end
    else
        DoSimpleMsg('Error in TShape MakeLike: "' + ShapeName + '" Not Found.', 57611);


end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TTShape.Get_Code: String;  // Returns active line code string
var
    TShapeObj: TTShapeObj;

begin

    TShapeObj := ElementList.Active;
    Result := TShapeObj.Name;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTShape.Set_Code(const Value: String);  // sets the  active TShape

var
    TShapeObj: TTShapeObj;

begin

    DSS.ActiveTShapeObj := NIL;
    TShapeObj := ElementList.First;
    while TShapeObj <> NIL do
    begin

        if CompareText(TShapeObj.Name, Value) = 0 then
        begin
            DSS.ActiveTShapeObj := TShapeObj;
            Exit;
        end;

        TShapeObj := ElementList.Next;
    end;

    DoSimpleMsg('TShape: "' + Value + '" not Found.', 57612);

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTShape.DoCSVFile(const FileName: String);

var
    F: TBufferedFileStream = nil;
    i: Integer;
    s: String;

begin
    try
        F := TBufferedFileStream.Create(FileName, fmOpenRead);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 57613);
        FreeAndNil(F);
        Exit;
    end;

    try

        with DSS.ActiveTShapeObj do
        begin
            ReAllocmem(TValues, Sizeof(TValues^[1]) * NumPoints);
            if Interval = 0.0 then
                ReAllocmem(Hours, Sizeof(Hours^[1]) * NumPoints);
            i := 0;
            while ((F.Position + 1) < F.Size) and (i < FNumPoints) do
            begin
                Inc(i);
                FSReadln(F, s); // read entire line  and parse with AuxParser
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
                    TValues^[i] := DblValue;
                end;
            end;
            FreeAndNil(F);
            if i <> FNumPoints then
                NumPoints := i;
        end;

    except
        On E: Exception do
        begin
            DoSimpleMsg('Error Processing CSV File: "' + FileName + '. ' + E.Message, 57614);
            FreeAndNil(F);
            Exit;
        end;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTShape.DoSngFile(const FileName: String);
var
    F: TFileStream = nil;
    Hr,
    M: Single;
    i: Integer;

begin
    try
        F := TFileStream.Create(FileName, fmOpenRead);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 57615);
        FreeAndNil(F);
        Exit;
    end;

    try
        with DSS.ActiveTShapeObj do
        begin
            ReAllocmem(TValues, Sizeof(TValues^[1]) * NumPoints);
            if Interval = 0.0 then
                ReAllocmem(Hours, Sizeof(Hours^[1]) * NumPoints);
            i := 0;
            while ((F.Position + 1) < F.Size) and (i < FNumPoints) do
            begin
                Inc(i);
                if Interval = 0.0 then
                begin
                    if F.Read(Hr, SizeOf(Hr)) <> SizeOf(Hr) then 
                        Break;
                    Hours^[i] := Hr;
                end;
                if F.Read(M, SizeOf(M)) <> SizeOf(M) then 
                    Break;
                TValues^[i] := M;
            end;
            FreeAndNil(F);
            if i <> FNumPoints then
                NumPoints := i;
        end;
    except
        DoSimpleMsg('Error Processing TShape File: "' + FileName, 57616);
        FreeAndNil(F);
        Exit;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTShape.DoDblFile(const FileName: String);
var
    F: TFileStream = nil;
    i: Integer;

begin
    try
        F := TFileStream.Create(FileName, fmOpenRead);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 57617);
        FreeAndNil(F);
        Exit;
    end;

    try
        with DSS.ActiveTShapeObj do
        begin
            ReAllocmem(TValues, Sizeof(TValues^[1]) * NumPoints);
            if Interval = 0.0 then
                ReAllocmem(Hours, Sizeof(Hours^[1]) * NumPoints);
            i := 0;
            while ((F.Position + 1) < F.Size) and (i < FNumPoints) do
            begin
                Inc(i);
                if Interval = 0.0 then
                    if F.Read(Hours^[i], SizeOf(Double)) <> SizeOf(Double) then 
                        Break;
                
                if F.Read(TValues^[i], SizeOf(Double)) <> SizeOf(Double) then 
                    Break;
            end;
            FreeAndNil(F);
            if i <> FNumPoints then
                NumPoints := i;
        end;
    except
        DoSimpleMsg('Error Processing Tshape File: "' + FileName, 57618);
        FreeAndNil(F);
        Exit;
    end;


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TTShape Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

    ArrayPropertyIndex := 0;

    InitPropertyValues(0);

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TTShapeObj.Destroy;
begin

    ReallocMem(Hours, 0);
    if Assigned(TValues) then
        ReallocMem(TValues, 0);
    inherited destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

    if FNumPoints > 0 then         // Handle Exceptional cases
        if FNumPoints = 1 then
        begin
            Result := TValues^[1];
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
                Result := TValues^[Index];
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
        end;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TTShapeObj.CalcMeanandStdDev;

begin

    if FNumPoints > 0 then
        if Interval > 0.0 then
            RCDMeanandStdDev(TValues, FNumPoints, FMean, FStdDev)
        else
            CurveMeanAndStdDev(TValues, Hours, FNumPoints, FMean, FStdDev);

    PropertyValue[5] := Format('%.8g', [FMean]);
    PropertyValue[6] := Format('%.8g', [FStdDev]);

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


procedure TTShapeObj.DumpProperties(F: TFileStream; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[i]);
        end;


end;

function TTShapeObj.GetPropertyValue(Index: Integer): String;
begin
    Result := '';

    case Index of
        2:
            Result := Format('%.8g', [Interval]);
        3:
            Result := GetDSSArray_Real(FNumPoints, TValues);
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

procedure TTShapeObj.InitPropertyValues(ArrayOffset: Integer);
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

procedure TTShapeObj.SaveToDblFile;

var
    F: TFileStream = nil;
    i: Integer;
    Fname: String;
begin
    if Assigned(TValues) then
    begin
        try
            FName := DSS.OutputDirectory {CurrentDSSDir} + Format('%s.dbl', [Name]);
            F := TFileStream.Create(FName, fmCreate);
            F.WriteBuffer(TValues^[1], NumPoints * SizeOf(Double));
            DSS.GlobalResult := 'Temp=[dblfile=' + FName + ']';
        finally
            FreeAndNil(F);
        end;

    end
    else
        DoSimpleMsg('Tshape.' + Name + ' Temperatures not defined.', 57622);
end;

procedure TTShapeObj.SaveToSngFile;

var
    F: TFileStream = nil;
    i: Integer;
    Fname: String;
    Temp: Single;

begin
    if Assigned(TValues) then
    begin
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


    end
    else
        DoSimpleMsg('Tshape.' + Name + ' Temperatures not defined.', 57623);


end;


procedure TTShapeObj.Set_Mean(const Value: Double);
begin
    FStdDevCalculated := TRUE;
    FMean := Value;
end;

procedure TTShapeObj.Set_NumPoints(const Value: Integer);
begin
    PropertyValue[1] := IntToStr(Value);   // Update property list variable

      // Reset array property values to keep them in propoer order in Save

    if ArrayPropertyIndex > 0 then
        PropertyValue[ArrayPropertyIndex] := PropertyValue[ArrayPropertyIndex];

    FNumPoints := Value;   // Now assign the value
end;

procedure TTShapeObj.Set_StdDev(const Value: Double);
begin
    FStdDevCalculated := TRUE;
    FStdDev := Value;
end;

end.
