unit LoadShape;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{  8-18-00 Added call to InterpretDblArrayto allow File=Syntax }

interface

{The LoadShape object is a general DSS object used by all circuits
 as a reference for obtaining yearly, daily, and other load shapes.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values are retrieved by setting the Code Property in the LoadShape Class.
 This sets the active LoadShape object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.  Or you
 can pick up the ActiveLoadShapeObj object and save the direct reference to the object.

 Loadshapes default to fixed interval data.  If the Interval is specified to be 0.0,
 then both time and multiplier data are expected.  If the Interval is  greater than 0.0,
 the user specifies only the multipliers.  The Hour command is ignored and the files are
 assumed to contain only the multiplier data.

 The user may place the data in CSV or binary files as well as passing through the
 command interface. Obviously, for large amounts of data such as 8760 load curves, the
 command interface is cumbersome.  CSV files are text separated by commas, one interval to a line.
 There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.

 For fixed interval data, only the multiplier is expected.  Therefore, the CSV format would
 contain only one number per line.  The two binary formats are packed.

 For variable interval data, (hour, multiplier) pairs are expected in both formats.

 The Mean and Std Deviation are automatically computed upon demand when new series of points is entered.

 The data may also be entered in unnormalized form.  The normalize=Yes command will force normalization.  That
 is, the multipliers are scaled so that the maximum value is 1.0.

 }

uses
    Command,
    DSSClass,
    DSSObject,
    UcMatrix,
    ucomplex,
    Arraydef;

type
{$SCOPEDENUMS ON}
    TLoadShapeProp = (
        INVALID = 0,
        npts = 1,     // Number of points to expect
        interval = 2, // default = 1.0
        mult = 3,     // vector of power multiplier values
        hour = 4,     // vextor of hour values
        mean = 5,     // set the mean (otherwise computed)
        stddev = 6,   // set the std dev (otherwise computed)
        csvfile = 7,  // Switch input to a csvfile
        sngfile = 8,  // switch input to a binary file of singles
        dblfile = 9,  // switch input to a binary file of singles
        action = 10,  // actions  Normalize
        qmult = 11,   // Q multiplier
        UseActual = 12, // Flag to signify to use actual value
        Pmax = 13,    // MaxP value
        Qmax = 14,    // MaxQ
        sinterval = 15, // Interval in seconds
        minterval = 16, // Interval in minutes
        Pbase = 17,   // for normalization, use peak if 0
        Qbase = 18,   // for normalization, use peak if 0
        Pmult = 19,   // synonym for Mult
        PQCSVFile = 20 // Redirect to a file with p, q pairs
    );
{$SCOPEDENUMS OFF}

    TLoadShape = class(TDSSClass)
    PRIVATE

        function Get_Code: String;  // Returns active LoadShape string
        procedure Set_Code(const Value: String);  // sets the  active LoadShape

        procedure DoCSVFile(const FileName: String);
        procedure Do2ColCSVFile(const FileName: String);  // for P and Q pairs
        procedure DoSngFile(const FileName: String);
        procedure DoDblFile(const FileName: String);
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const ShapeName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create(dss: TDSSContext);
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

        function Find(const ObjName: String): Pointer; OVERRIDE;  // Find an obj of this class by name

        procedure TOPExport(ObjName: String);

       // Set this property to point ActiveLoadShapeObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;

    end;

    TLoadShapeObj = class(TDSSObject)
    PRIVATE
        LastValueAccessed,
        FNumPoints: Integer;  // Number of points in curve
        ArrayPropertyIndex: Integer;

        iMaxP: Integer;

        FStdDevCalculated: Boolean;
        MaxQSpecified: Boolean;

        FMean,
        FStdDev: Double;

        // Function Get_FirstMult:Double;
        // Function Get_NextMult :Double;
        function Get_Interval: Double;
        procedure SaveToDblFile;
        procedure SaveToSngFile;
        procedure CalcMeanandStdDev;
        function Get_Mean: Double;
        function Get_StdDev: Double;
        procedure Set_Mean(const Value: Double);
        procedure Set_StdDev(const Value: Double);  // Normalize the curve presently in memory
        function GetMultSingle(hr: Double): Complex;
    PUBLIC

        Interval: Double;  //=0.0 then random interval     (hr)
        // Double data
        dblHours,          // Time values (hr) if Interval > 0.0  Else nil
        dblPMultipliers,
        dblQMultipliers: pDoubleArray;  // Multipliers

        // Single data
        sngHours,
        sngPMultipliers,
        sngQMultipliers: pSingleArray;

        MaxP,
        MaxQ,
        BaseP,
        BaseQ: Double;

        UseActual: Boolean;
        ExternalMemory: Boolean;

        PQCSVFilename: String;
        CSVFilename: String;
        SngFilename: String;
        DblFilename: String;

        constructor Create(ParClass: TDSSClass; const LoadShapeName: String);
        destructor Destroy; OVERRIDE;

        function GetMult(hr: Double): Complex;  // Get multiplier at specified time
        function Mult(i: Integer): Double;  // get multiplier by index
        function Hour(i: Integer): Double;  // get hour corresponding to point index
        procedure Normalize;
        procedure SetMaxPandQ;


        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

        property NumPoints: Integer READ FNumPoints WRITE FNumPoints;
        property PresentInterval: Double READ Get_Interval;
        property Mean: Double READ Get_Mean WRITE Set_Mean;
        property StdDev: Double READ Get_StdDev WRITE Set_StdDev;
        {Property FirstMult :Double Read Get_FirstMult;}
        {Property NextMult  :Double Read Get_NextMult;}

        procedure SetDataPointers(HoursPtr: PDouble; PMultPtr: PDouble; QMultPtr: PDouble);
        procedure SetDataPointersSingle(HoursPtr: PSingle; PMultPtr: PSingle; QMultPtr: PSingle);
        procedure UseFloat32();
        procedure UseFloat64();
    end;

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
    PointerList,
    DSSHelper,
    TypInfo;

type
    ELoadShapeError = class(Exception);  // Raised to abort solution

const
    NumPropsThisClass = Ord(High(TLoadShapeProp));

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TLoadShape.Create(dss: TDSSContext);  // Creates superstructure for all Line objects
begin
    inherited Create(dss);
    Class_Name := 'LoadShape';
    DSSClassType := DSS_OBJECT;

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLoadShape.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShape.DefineProperties;
var
    i: Integer;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

    // Define Property names
    for i := 1 to Ord(High(TLoadShapeProp)) do
    begin
        PropertyName[i] := GetEnumName(TypeInfo(TLoadShapeProp), Ord(i));;
    end;

    // define Property help values
    PropertyHelp[ord(TLoadShapeProp.npts)] := 'Max number of points to expect in load shape vectors. This gets reset to the number of multiplier values found (in files only) if less than specified.';     // Number of points to expect
    PropertyHelp[ord(TLoadShapeProp.interval)] := 'Time interval for fixed interval data, hrs. Default = 1. ' +
        'If Interval = 0 then time data (in hours) may be at either regular or  irregular intervals and time value must be specified using either the Hour property or input files. ' +
        'Then values are interpolated when Interval=0, but not for fixed interval data.  ' + CRLF + CRLF +
        'See also "sinterval" and "minterval".'; // default = 1.0;
    PropertyHelp[ord(TLoadShapeProp.mult)] := 'Array of multiplier values for active power (P) or other key value (such as pu V for Vsource). ' + CRLF + CRLF +
        'You can also use the syntax: ' + CRLF + CRLF +
        'mult = (file=filename)     !for text file one value per line' + CRLF +
        'mult = (dblfile=filename)  !for packed file of doubles' + CRLF +
        'mult = (sngfile=filename)  !for packed file of singles ' + CRLF +
        'mult = (file=MyCSVFile.CSV, col=3, header=yes)  !for multicolumn CSV files ' + CRLF + CRLF +
        'Note: this property will reset Npts if the  number of values in the files are fewer.' + CRLF + CRLF +
        'Same as Pmult';     // vector of power multiplier values
    PropertyHelp[ord(TLoadShapeProp.hour)] := 'Array of hour values. Only necessary to define for variable interval data (Interval=0).' +
        ' If you set Interval>0 to denote fixed interval data, DO NOT USE THIS PROPERTY. ' +
        'You can also use the syntax: ' + CRLF +
        'hour = (file=filename)     !for text file one value per line' + CRLF +
        'hour = (dblfile=filename)  !for packed file of doubles' + CRLF +
        'hour = (sngfile=filename)  !for packed file of singles ';     // vextor of hour values
    PropertyHelp[ord(TLoadShapeProp.mean)] := 'Mean of the active power multipliers.  This is computed on demand the first time a ' +
        'value is needed.  However, you may set it to another value independently. ' +
        'Used for Monte Carlo load simulations.';     // set the mean (otherwise computed)
    PropertyHelp[ord(TLoadShapeProp.stddev)] := 'Standard deviation of active power multipliers.  This is computed on demand the first time a ' +
        'value is needed.  However, you may set it to another value independently.' +
        'Is overwritten if you subsequently read in a curve' + CRLF + CRLF +
        'Used for Monte Carlo load simulations.';   // set the std dev (otherwise computed)
    PropertyHelp[ord(TLoadShapeProp.csvfile)] := 'Switch input of active power load curve data to a CSV text file ' +
        'containing (hour, mult) points, or simply (mult) values for fixed time interval data, one per line. ' +
        'NOTE: This action may reset the number of points to a lower value.';   // Switch input to a csvfile
    PropertyHelp[ord(TLoadShapeProp.sngfile)] := 'Switch input of active power load curve data to a binary file of singles ' +
        'containing (hour, mult) points, or simply (mult) values for fixed time interval data, packed one after another. ' +
        'NOTE: This action may reset the number of points to a lower value.';  // switch input to a binary file of singles
    PropertyHelp[ord(TLoadShapeProp.dblfile)] := 'Switch input of active power load curve data to a binary file of doubles ' +
        'containing (hour, mult) points, or simply (mult) values for fixed time interval data, packed one after another. ' +
        'NOTE: This action may reset the number of points to a lower value.';   // switch input to a binary file of singles
    PropertyHelp[ord(TLoadShapeProp.action)] := '{NORMALIZE | DblSave | SngSave} After defining load curve data, setting action=normalize ' +
        'will modify the multipliers so that the peak is 1.0. ' +
        'The mean and std deviation are recomputed.' + CRLF + CRLF +
        'Setting action=DblSave or SngSave will cause the present mult and qmult values to be written to ' +
        'either a packed file of double or single. The filename is the loadshape name. The mult array will have a ' +
        '"_P" appended on the file name and the qmult array, if it exists, will have "_Q" appended.'; // Action
    PropertyHelp[ord(TLoadShapeProp.qmult)] := 'Array of multiplier values for reactive power (Q).  You can also use the syntax: ' + CRLF +
        'qmult = (file=filename)     !for text file one value per line' + CRLF +
        'qmult = (dblfile=filename)  !for packed file of doubles' + CRLF +
        'qmult = (sngfile=filename)  !for packed file of singles ' + CRLF +     // vector of qmultiplier values
        'qmult = (file=MyCSVFile.CSV, col=4, header=yes)  !for multicolumn CSV files ';
    PropertyHelp[ord(TLoadShapeProp.UseActual)] := '{Yes | No* | True | False*} If true, signifies to Load, Generator, Vsource, or other objects to ' +
        'use the return value as the actual kW, kvar, kV, or other value rather than a multiplier. ' +
        'Nominally for AMI Load data but may be used for other functions.';
    PropertyHelp[ord(TLoadShapeProp.Pmax)] := 'kW value at the time of max power. Is automatically set upon reading in a loadshape. ' +
        'Use this property to override the value automatically computed or to retrieve the value computed.';
    PropertyHelp[ord(TLoadShapeProp.Qmax)] := 'kvar value at the time of max kW power. Is automatically set upon reading in a loadshape. ' +
        'Use this property to override the value automatically computed or to retrieve the value computed.';
    PropertyHelp[ord(TLoadShapeProp.sinterval)] := 'Specify fixed interval in SECONDS. Alternate way to specify Interval property.';
    PropertyHelp[ord(TLoadShapeProp.minterval)] := 'Specify fixed interval in MINUTES. Alternate way to specify Interval property.';
    PropertyHelp[ord(TLoadShapeProp.Pbase)] := 'Base P value for normalization. Default is zero, meaning the peak will be used.';
    PropertyHelp[ord(TLoadShapeProp.Qbase)] := 'Base Q value for normalization. Default is zero, meaning the peak will be used.';
    PropertyHelp[ord(TLoadShapeProp.Pmult)] := 'Synonym for "mult".';
    PropertyHelp[ord(TLoadShapeProp.PQCSVFile)] := 'Switch input to a CSV text file containing (active, reactive) power (P, Q) multiplier pairs, one per row. ' + CRLF +
        'If the interval=0, there should be 3 items on each line: (hour, Pmult, Qmult)';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLoadShape.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    DSS.ActiveDSSObject := TLoadShapeObj.Create(Self, ObjName);
    Result := AddObjectToList(DSS.ActiveDSSObject);
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLoadShape.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin
    Result := 0;
  // continue parsing with contents of Parser
    DSS.ActiveLoadShapeObj := ElementList.Active;
    DSS.ActiveDSSObject := DSS.ActiveLoadShapeObj;

    with DSS.ActiveLoadShapeObj do
    begin

        ParamPointer := 0;
        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
        while Length(Param) > 0 do
        begin
            if Length(ParamName) = 0 then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer <= NumPropsThisClass) then case TLoadShapeProp(ParamPointer) of
                TLoadShapeProp.INVALID:
                    DoSimpleMsg(DSS, 'Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 610);
                TLoadShapeProp.npts:
                    if DSS.ActiveLoadShapeObj.ExternalMemory then
                    begin
                        DoSimpleMsg(DSS, 'Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
                        Exit;
                    end
                    else
                        NumPoints := DSS.Parser.Intvalue;
                TLoadShapeProp.interval:
                    Interval := DSS.Parser.DblValue;
                TLoadShapeProp.Pmult, TLoadShapeProp.mult:
                begin
                    if DSS.ActiveLoadShapeObj.ExternalMemory then
                    begin
                        DoSimpleMsg(DSS, 'Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
                        Exit;
                    end;
                    UseFloat64();
                    ReAllocmem(dblPMultipliers, Sizeof(Double) * NumPoints);
                    // Allow possible Resetting (to a lower value) of num points when specifying multipliers not Hours
                    NumPoints := InterpretDblArray(DSS, Param, NumPoints, dblPMultipliers);   // Parser.ParseAsVector(Npts, Multipliers);
                end;
                TLoadShapeProp.hour:
                begin
                    UseFloat64();
                    if DSS.ActiveLoadShapeObj.ExternalMemory then
                    begin
                        DoSimpleMsg(DSS, 'Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
                        Exit;
                    end;
                    ReAllocmem(dblHours, Sizeof(dblHours^[1]) * NumPoints);
                    InterpretDblArray(DSS, Param, NumPoints, dblHours);   // Parser.ParseAsVector(Npts, Hours);
                    Interval := 0.0;
                end;
                TLoadShapeProp.mean:
                    Mean := DSS.Parser.DblValue;
                TLoadShapeProp.stddev:
                    StdDev := DSS.Parser.DblValue;
                TLoadShapeProp.csvfile:
                    DoCSVFile(Param);
                TLoadShapeProp.sngfile:
                    DoSngFile(Param);
                TLoadShapeProp.dblfile:
                    DoDblFile(Param);
                TLoadShapeProp.action:
                    case lowercase(Param)[1] of
                        'n':
                            Normalize;
                        'd':
                            SaveToDblFile;
                        's':
                            SaveToSngFile;
                    end;
                TLoadShapeProp.qmult:
                begin
                    if DSS.ActiveLoadShapeObj.ExternalMemory then
                    begin
                        DoSimpleMsg(DSS, 'Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61105);
                        Exit;
                    end;
                    UseFloat64();
                    ReAllocmem(dblQMultipliers, Sizeof(dblQMultipliers^[1]) * NumPoints);
                    InterpretDblArray(DSS, Param, NumPoints, dblQMultipliers);   // Parser.ParseAsVector(Npts, Multipliers);
                end;
                TLoadShapeProp.UseActual:
                    UseActual := InterpretYesNo(Param);
                TLoadShapeProp.Pmax:
                    MaxP := DSS.Parser.DblValue;
                TLoadShapeProp.Qmax:
                    MaxQ := DSS.Parser.DblValue;
                TLoadShapeProp.sinterval:
                    Interval := DSS.Parser.DblValue / 3600.0;  // Convert seconds to hr
                TLoadShapeProp.minterval:
                    Interval := DSS.Parser.DblValue / 60.0;  // Convert minutes to hr
                TLoadShapeProp.Pbase:
                    BaseP := DSS.Parser.DblValue;
                TLoadShapeProp.Qbase:
                    BaseQ := DSS.Parser.DblValue;
                TLoadShapeProp.PQCSVFile:
                    Do2ColCSVFile(Param);
            end
            else
            begin
                // Inherited parameters

                //TODO: Remove after all classes are handled without PropertyValue.
                //      Note that if the value is invalid, this would save it anyway.
                PropertyValue[ParamPointer] := Param;

                ClassEdit(DSS.ActiveLoadShapeObj, ParamPointer - NumPropsThisClass);
            end;

            if (ParamPointer <= NumPropsThisClass) then case TLoadShapeProp(ParamPointer) of
                TLoadShapeProp.mult, TLoadShapeProp.csvfile, TLoadShapeProp.sngfile, TLoadShapeProp.dblfile:
                begin
                    FStdDevCalculated := FALSE;   // now calculated on demand
                    ArrayPropertyIndex := ParamPointer;
                    NumPoints := FNumPoints;  // Keep Properties in order for save command
                end;
                TLoadShapeProp.Qmax:
                    MaxQSpecified := TRUE;
            end;

            ParamName := DSS.Parser.NextParam;
            Param := DSS.Parser.StrValue;
        end; {WHILE}

        if Assigned(dblPMultipliers) or Assigned(sngPMultipliers) then
            SetMaxPandQ;
    end; {WITH}
end;

function TLoadShape.Find(const ObjName: String): Pointer;
begin
    if (Length(ObjName) = 0) or (CompareText(ObjName, 'none') = 0) then
        Result := NIL
    else
        Result := inherited Find(ObjName);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLoadShape.MakeLike(const ShapeName: String): Integer;
var
    OtherLoadShape: TLoadShapeObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this line code in the present collection}
    OtherLoadShape := Find(ShapeName);
    if OtherLoadShape <> NIL then
        with DSS.ActiveLoadShapeObj do
        begin
            NumPoints := OtherLoadShape.NumPoints;
            Interval := OtherLoadShape.Interval;
            PQCSVFilename := OtherLoadShape.PQCSVFilename;
            CSVFilename := OtherLoadShape.CSVFilename;
            SngFilename := OtherLoadShape.SngFilename;
            DblFilename := OtherLoadShape.DblFilename;

            if ExternalMemory then
            begin
                // There is no point in copying a static loadshape,
                // so we assume the user would want to modify the data
                dblPMultipliers := nil;
                dblQMultipliers := nil;
                dblHours:= nil;
                sngPMultipliers := nil;
                sngQMultipliers := nil;
                sngHours:= nil;
                ExternalMemory := False;
            end;

            // Double versions
            if Assigned(OtherLoadShape.dblPMultipliers) then
            begin
                ReallocMem(dblPMultipliers, SizeOf(Double) * NumPoints);
                Move(OtherLoadShape.dblPMultipliers[1], dblPMultipliers[1], SizeOf(Double) * NumPoints);
            end
            else
                ReallocMem(dblPMultipliers, 0);

            if Assigned(OtherLoadShape.dblQmultipliers) then
            begin
                ReallocMem(dblQMultipliers, SizeOf(Double) * NumPoints);
                Move(OtherLoadShape.dblQMultipliers[1], dblQMultipliers[1], SizeOf(Double) * NumPoints);
            end;

            if Interval > 0.0 then
                ReallocMem(dblHours, 0)
            else
            begin
                ReallocMem(dblHours, SizeOf(Double) * NumPoints);
                Move(OtherLoadShape.dblHours[1], dblHours[1], SizeOf(Double) * NumPoints);
            end;

            // Single versions
            if Assigned(OtherLoadShape.sngPMultipliers) then
            begin
                ReallocMem(sngPMultipliers, SizeOf(Single) * NumPoints);
                Move(OtherLoadShape.sngPMultipliers[1], sngPMultipliers[1], SizeOf(Single) * NumPoints);
            end
            else
                ReallocMem(sngPMultipliers, 0);

            if Assigned(OtherLoadShape.sngQmultipliers) then
            begin
                ReallocMem(sngQMultipliers, SizeOf(Single) * NumPoints);
                Move(OtherLoadShape.sngQMultipliers[1], sngQMultipliers[1], SizeOf(Single) * NumPoints);
            end;

            if Interval > 0.0 then
                ReallocMem(sngHours, 0)
            else
            begin
                ReallocMem(sngHours, SizeOf(Single) * NumPoints);
                Move(OtherLoadShape.sngHours[1], sngHours[1], SizeOf(Single) * NumPoints);
            end;

            SetMaxPandQ;
            UseActual := OtherLoadShape.UseActual;
            BaseP := OtherLoadShape.BaseP;
            BaseQ := OtherLoadShape.BaseQ;


       { MaxP :=  OtherLoadShape.MaxP;
        MaxQ :=  OtherLoadShape.MaxQ;
        Mean :=  OtherLoadShape.Mean;
        StdDev := OtherLoadShape.StdDev;
       }

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherLoadShape.PropertyValue[i];
        end
    else
        DoSimpleMsg(DSS, 'Error in LoadShape MakeLike: "' + ShapeName + '" Not Found.', 611);


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLoadShape.Init(Handle: Integer): Integer;

begin
    DoSimpleMsg(DSS, 'Need to implement TLoadShape.Init', -1);
    REsult := 0;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLoadShape.Get_Code: String;  // Returns active line code string
var
    LoadShapeObj: TLoadShapeObj;

begin

    LoadShapeObj := ElementList.Active;
    Result := LoadShapeObj.Name;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShape.Set_Code(const Value: String);  // sets the  active LoadShape

var
    LoadShapeObj: TLoadShapeObj;

begin

    DSS.ActiveLoadShapeObj := NIL;
    LoadShapeObj := ElementList.First;
    while LoadShapeObj <> NIL do
    begin

        if CompareText(LoadShapeObj.Name, Value) = 0 then
        begin
            DSS.ActiveLoadShapeObj := LoadShapeObj;
            Exit;
        end;

        LoadShapeObj := ElementList.Next;
    end;

    DoSimpleMsg(DSS, 'LoadShape: "' + Value + '" not Found.', 612);

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShape.Do2ColCSVFile(const FileName: String);
{
   Process 2-column CSV file (3-col if time expected)
}
var
    F: Textfile;
    i: Integer;
    s: String;

begin
    if DSS.ActiveLoadShapeObj.ExternalMemory then
    begin
        DoSimpleMsg(DSS, 'Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
        Exit;
    end;

    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg(DSS, 'Error Opening File: "' + FileName, 613);
        CloseFile(F);
        Exit;
    end;

    try

        with DSS.ActiveLoadShapeObj do
        begin
         // Allocate both P and Q multipliers
            UseFloat64();
            ReAllocmem(dblPMultipliers, Sizeof(dblPMultipliers^[1]) * NumPoints);
            ReAllocmem(dblQMultipliers, Sizeof(dblQMultipliers^[1]) * NumPoints);
            if Interval = 0.0 then
                ReAllocmem(dblHours, Sizeof(dblHours^[1]) * NumPoints);
            i := 0;
            while (not EOF(F)) and (i < FNumPoints) do
            begin
                Inc(i);
                Readln(F, s); // read entire line  and parse with DSS.AuxParser
            {DSS.AuxParser allows commas or white space}
                with DSS.AuxParser do
                begin
                    CmdString := s;
                    if Interval = 0.0 then
                    begin
                        NextParam;
                        dblHours^[i] := DblValue;
                    end;
                    NextParam;
                    dblPMultipliers[i - 1] := DblValue;  // first parm
                    NextParam;
                    dblQMultipliers[i - 1] := DblValue;  // second parm
                end;
            end;
            CloseFile(F);
            if i <> FNumPoints then
                NumPoints := i;
        end;

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error Processing CSV File: "' + FileName + '. ' + E.Message, 614);
            CloseFile(F);
            Exit;
        end;
    end;

    DSS.ActiveLoadShapeObj.PQCSVFilename := FileName;
end;

procedure TLoadShape.DoCSVFile(const FileName: String);

var
    F: Textfile;
    i: Integer;
    s: String;

begin
    if DSS.ActiveLoadShapeObj.ExternalMemory then
    begin
        DoSimpleMsg(DSS, 'Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
        Exit;
    end;

    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg(DSS, 'Error Opening File: "' + FileName, 613);
        CloseFile(F);
        Exit;
    end;

    try

        with DSS.ActiveLoadShapeObj do
        begin
            UseFloat64();
            ReAllocmem(dblPMultipliers, Sizeof(dblPMultipliers^[1]) * NumPoints);
            if Interval = 0.0 then
                ReAllocmem(dblHours, Sizeof(dblHours^[1]) * NumPoints);
            i := 0;
            while (not EOF(F)) and (i < FNumPoints) do
            begin
                Inc(i);
                Readln(F, s); // read entire line  and parse with DSS.AuxParser
            {DSS.AuxParser allows commas or white space}
                with DSS.AuxParser do
                begin
                    CmdString := s;
                    if Interval = 0.0 then
                    begin
                        NextParam;
                        dblHours^[i] := DblValue;
                    end;
                    NextParam;
                    dblPMultipliers^[i] := DblValue;
                end;
            end;
            CloseFile(F);
            if i <> FNumPoints then
                NumPoints := i;
        end;

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error Processing CSV File: "' + FileName + '. ' + E.Message, 614);
            CloseFile(F);
            Exit;
        end;
    end;

    DSS.ActiveLoadShapeObj.CSVFilename := FileName;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShape.DoSngFile(const FileName: String);
var
    F: file of Single;
    Hr, M: Single;
    i: Integer;

begin
    if DSS.ActiveLoadShapeObj.ExternalMemory then
    begin
        DoSimpleMsg(DSS, 'Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
        Exit;
    end;

    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg(DSS, 'Error Opening File: "' + FileName, 615);
        CloseFile(F);
        Exit;
    end;

    try
        with DSS.ActiveLoadShapeObj do
        begin
            UseFloat64();
            ReAllocmem(dblPMultipliers, Sizeof(dblPMultipliers^[1]) * NumPoints);
            if Interval = 0.0 then
                ReAllocmem(dblHours, Sizeof(dblHours^[1]) * NumPoints);
            i := 0;
            while (not EOF(F)) and (i < FNumPoints) do
            begin
                Inc(i);
                if Interval = 0.0 then
                begin
                    Read(F, Hr);
                    dblHours^[i] := Hr;
                end;
                Read(F, M);
                dblPMultipliers^[i] := M;
            end;
            CloseFile(F);
            if i <> FNumPoints then
                NumPoints := i;
        end;
    except
        DoSimpleMsg(DSS, 'Error Processing LoadShape File: "' + FileName, 616);
        CloseFile(F);
        Exit;
    end;

    DSS.ActiveLoadShapeObj.SngFilename := FileName;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShape.DoDblFile(const FileName: String);
var
    F: file of Double;
    i: Integer;

begin
    if DSS.ActiveLoadShapeObj.ExternalMemory then
    begin
        DoSimpleMsg(DSS, 'Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
        Exit;
    end;

    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg(DSS, 'Error Opening File: "' + FileName, 617);
        CloseFile(F);
        Exit;
    end;

    try
        with DSS.ActiveLoadShapeObj do
        begin
            UseFloat64();
            ReAllocmem(dblPMultipliers, Sizeof(dblPMultipliers^[1]) * NumPoints);
            if Interval = 0.0 then
                ReAllocmem(dblHours, Sizeof(dblHours^[1]) * NumPoints);
            i := 0;
            while (not EOF(F)) and (i < FNumPoints) do
            begin
                Inc(i);
                if Interval = 0.0 then
                    Read(F, dblHours^[i]);
                Read(F, dblPMultipliers^[i]);
            end;
            CloseFile(F);
            if i <> FNumPoints then
                NumPoints := i;
        end;
    except
        DoSimpleMsg(DSS, 'Error Processing LoadShape File: "' + FileName, 618);
        CloseFile(F);
        Exit;
    end;

    DSS.ActiveLoadShapeObj.DblFilename := FileName;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLoadShape Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TLoadShapeObj.Create(ParClass: TDSSClass; const LoadShapeName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(LoadShapeName);
    DSSObjType := ParClass.DSSClassType;

    ExternalMemory := False;
    LastValueAccessed := 1;

    FNumPoints := 0;
    Interval := 1.0;  // hr
    dblHours := NIL;
    dblPMultipliers := NIL;
    dblQMultipliers := NIL;
    sngHours := NIL;
    sngPMultipliers := NIL;
    sngQMultipliers := NIL;
    MaxP := 1.0;
    MaxQ := 0.0;
    BaseP := 0.0;
    BaseQ := 0.0;
    UseActual := FALSE;
    MaxQSpecified := FALSE;
    FStdDevCalculated := FALSE;  // calculate on demand
    PQCSVFilename := '';
    FMean := 0;
    FStdDev := 0;
    ArrayPropertyIndex := 0;

    InitPropertyValues(0);

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLoadShapeObj.Destroy;
begin
    if not ExternalMemory then
    begin
        if Assigned(dblHours) then
            ReallocMem(dblHours, 0);
        if Assigned(dblPMultipliers) then
            ReallocMem(dblPMultipliers, 0);
        if Assigned(dblQMultipliers) then
            ReallocMem(dblQMultipliers, 0);
        if Assigned(sngHours) then
            ReallocMem(sngHours, 0);
        if Assigned(sngPMultipliers) then
            ReallocMem(sngPMultipliers, 0);
        if Assigned(sngQMultipliers) then
            ReallocMem(sngQMultipliers, 0);
    end;

    inherited destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLoadShapeObj.GetMult(hr: Double): Complex;

// This function returns a multiplier for the given hour.
// If no points exist in the curve, the result is  1.0
// If there are fewer points than requested, the curve is simply assumed to repeat
// Thus a daily load curve can suffice for a yearly load curve:  You just get the
// same day over and over again.
// The value returned is the nearest to the interval requested.  Thus if you request
// hour=12.25 and the interval is 1.0, you will get interval 12.

var
    Index, i: Integer;

    function Set_Result_im(const realpart: Double): Double;
   {Set imaginary part of Result when Qmultipliers not defined}
    begin
        if UseActual then
            Set_Result_im := 0.0       // if actual, assume zero
        else
            Set_Result_im := realpart; // same as real otherwise
    end;

begin
    if Assigned(sngPMultipliers) then
    begin
        Result := GetMultSingle(hr);
        exit;
    end;

    Result.re := 1.0;
    Result.im := 1.0;    // default return value if no points in curve

    if FNumPoints > 0 then         // Handle Exceptional cases
        if FNumPoints = 1 then
        begin
            Result.re := dblPMultipliers^[1];
            if Assigned(dblQMultipliers) then
                Result.im := dblQMultipliers^[1]
            else
                Result.im := Set_Result_im(Result.re);
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
                Result.Re := dblPMultipliers^[Index];
                if Assigned(dblQMultipliers) then
                    Result.im := dblQMultipliers^[Index]
                else
                    Result.im := Set_Result_im(Result.re);
            end
            else
            begin
          // For random interval

        { Start with previous value accessed under the assumption that most
          of the time, this function will be called sequentially}

          {Normalize Hr to max hour in curve to get wraparound}
                if Hr > dblHours^[FNumPoints] then
                begin
                    Hr := Hr - Trunc(Hr / dblHours^[FNumPoints]) * dblHours^[FNumPoints];
                end;

                if dblHours^[LastValueAccessed] > Hr then
                    LastValueAccessed := 1;  // Start over from beginning
                for i := LastValueAccessed + 1 to FNumPoints do
                begin
                    if Abs(dblHours^[i] - Hr) < 0.00001 then  // If close to an actual point, just use it.
                    begin
                        Result.re := dblPMultipliers^[i];
                        if Assigned(dblQMultipliers) then
                            Result.im := dblQMultipliers^[i]
                        else
                            Result.im := Set_Result_im(Result.re);
                        LastValueAccessed := i;
                        Exit;
                    end
                    else
                    if dblHours^[i] > Hr then      // Interpolate for multiplier
                    begin
                        LastValueAccessed := i - 1;
                        Result.re := dblPMultipliers^[LastValueAccessed] +
                            (Hr - dblHours^[LastValueAccessed]) / (dblHours^[i] - dblHours^[LastValueAccessed]) *
                            (dblPMultipliers^[i] - dblPMultipliers^[LastValueAccessed]);
                        if Assigned(dblQMultipliers) then
                            Result.im := dblQMultipliers^[LastValueAccessed] +
                                (Hr - dblHours^[LastValueAccessed]) / (dblHours^[i] - dblHours^[LastValueAccessed]) *
                                (dblQMultipliers^[i] - dblQMultipliers^[LastValueAccessed])
                        else
                            Result.im := Set_Result_im(Result.re);
                        Exit;
                    end;
                end;

           // If we fall through the loop, just use last value
                LastValueAccessed := FNumPoints - 1;
                Result.re := dblPMultipliers^[FNumPoints];
                if Assigned(dblQMultipliers) then
                    Result.im := dblQMultipliers^[FNumPoints]
                else
                    Result.im := Set_Result_im(Result.re);
            end;
        end;
end;

function TLoadShapeObj.GetMultSingle(hr: Double): Complex;
var
    Index, i: Integer;

    function Set_Result_im(const realpart: Double): Double;
   {Set imaginary part of Result when Qmultipliers not defined}
    begin
        if UseActual then
            Set_Result_im := 0.0       // if actual, assume zero
        else
            Set_Result_im := realpart; // same as real otherwise
    end;

begin
    Result.re := 1.0;
    Result.im := 1.0;    // default return value if no points in curve

    if FNumPoints > 0 then         // Handle Exceptional cases
        if FNumPoints = 1 then
        begin
            Result.re := sngPMultipliers^[1];
            if Assigned(sngQMultipliers) then
                Result.im := sngQMultipliers^[1]
            else
                Result.im := Set_Result_im(Result.re);
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
                Result.Re := sngPMultipliers^[Index];
                if Assigned(sngQMultipliers) then
                    Result.im := sngQMultipliers^[Index]
                else
                    Result.im := Set_Result_im(Result.re);
            end
            else
            begin
          // For random interval

        { Start with previous value accessed under the assumption that most
          of the time, this function will be called sequentially}

          {Normalize Hr to max hour in curve to get wraparound}
                if Hr > sngHours^[FNumPoints] then
                begin
                    Hr := Hr - Trunc(Hr / sngHours^[FNumPoints]) * sngHours^[FNumPoints];
                end;

                if sngHours^[LastValueAccessed] > Hr then
                    LastValueAccessed := 1;  // Start over from beginning
                for i := LastValueAccessed + 1 to FNumPoints do
                begin
                    if Abs(sngHours^[i] - Hr) < 0.00001 then  // If close to an actual point, just use it.
                    begin
                        Result.re := sngPMultipliers^[i];
                        if Assigned(sngQMultipliers) then
                            Result.im := sngQMultipliers^[i]
                        else
                            Result.im := Set_Result_im(Result.re);
                        LastValueAccessed := i;
                        Exit;
                    end
                    else
                    if sngHours^[i] > Hr then      // Interpolate for multiplier
                    begin
                        LastValueAccessed := i - 1;
                        Result.re := sngPMultipliers^[LastValueAccessed] +
                            (Hr - sngHours^[LastValueAccessed]) / (sngHours^[i] - sngHours^[LastValueAccessed]) *
                            (sngPMultipliers^[i] - sngPMultipliers^[LastValueAccessed]);
                        if Assigned(sngQMultipliers) then
                            Result.im := sngQMultipliers^[LastValueAccessed] +
                                (Hr - sngHours^[LastValueAccessed]) / (sngHours^[i] - sngHours^[LastValueAccessed]) *
                                (sngQMultipliers^[i] - sngQMultipliers^[LastValueAccessed])
                        else
                            Result.im := Set_Result_im(Result.re);
                        Exit;
                    end;
                end;

           // If we fall through the loop, just use last value
                LastValueAccessed := FNumPoints - 1;
                Result.re := sngPMultipliers^[FNumPoints];
                if Assigned(sngQMultipliers) then
                    Result.im := sngQMultipliers^[FNumPoints]
                else
                    Result.im := Set_Result_im(Result.re);
            end;
        end;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShapeObj.Normalize;
// normalize this load shape
var
    MaxMult: Double;

    procedure DoNormalize(Multipliers: pDoubleArray);
    var
        i: Integer;
    begin
        if FNumPoints > 0 then
        begin
            if MaxMult <= 0.0 then
            begin
                MaxMult := Abs(Multipliers^[1]);
                for i := 2 to FNumPoints do
                    MaxMult := Max(MaxMult, Abs(Multipliers^[i]));
            end;
            if MaxMult = 0.0 then
                MaxMult := 1.0; // Avoid divide by zero
            for i := 1 to FNumPoints do
                Multipliers^[i] := Multipliers^[i] / MaxMult;
        end;
    end;

    procedure DoNormalizeSingle(Multipliers: pSingleArray);
    var
        i: Integer;
    begin
        if FNumPoints > 0 then
        begin
            if MaxMult <= 0.0 then
            begin
                MaxMult := Abs(Multipliers^[1]);
                for i := 2 to FNumPoints do
                    MaxMult := Max(MaxMult, Abs(Multipliers^[i]));
            end;
            if MaxMult = 0.0 then
                MaxMult := 1.0; // Avoid divide by zero
            for i := 1 to FNumPoints do
                Multipliers^[i] := Multipliers^[i] / MaxMult;
        end;
    end;

begin
    if ExternalMemory then
    begin
        DoSimpleMsg(DSS, 'Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
        Exit;
    end;

    MaxMult := BaseP;
    if Assigned(dblPMultipliers) then
    begin
        DoNormalize(dblPMultipliers);
        if Assigned(dblQMultipliers) then
        begin
            MaxMult := BaseQ;
            DoNormalize(dblQMultipliers);
        end;
    end
    else
    begin
        DoNormalizeSingle(sngPMultipliers);
        if Assigned(sngQMultipliers) then
        begin
            MaxMult := BaseQ;
            DoNormalizeSingle(sngQMultipliers);
        end;
    end;
    UseActual := FALSE;  // not likely that you would want to use the actual if you normalized it.
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShapeObj.CalcMeanandStdDev;
begin
    if FNumPoints > 0 then
    begin
        if Assigned(dblPMultipliers) then
        begin
            if Interval > 0.0 then
                RCDMeanandStdDev(dblPMultipliers, FNumPoints, FMean, FStdDev)
            else
                CurveMeanAndStdDev(dblPMultipliers, dblHours, FNumPoints, FMean, FStdDev);
        end
        else
        begin
            if Interval > 0.0 then
                RCDMeanandStdDevSingle(sngPMultipliers, FNumPoints, FMean, FStdDev)
            else
                CurveMeanAndStdDevSingle(sngPMultipliers, sngHours, FNumPoints, FMean, FStdDev);
        end;
    end;
    PropertyValue[ord(TLoadShapeProp.mean)] := Format('%.8g', [FMean]);
    PropertyValue[ord(TLoadShapeProp.stddev)] := Format('%.8g', [FStdDev]);

    FStdDevCalculated := TRUE;
   { No Action is taken on Q multipliers}
end;

function TLoadShapeObj.Get_Interval: Double;
begin

    if Interval > 0.0 then
        Result := Interval
    else
    begin
        if LastValueAccessed > 1 then
        begin
            if dblHours <> nil then
                Result := dblHours^[LastValueAccessed] - dblHours^[LastValueAccessed - 1]
            else
                Result := sngHours^[LastValueAccessed] - sngHours^[LastValueAccessed - 1]
        end
        else
            Result := 0.0;
    end;


end;

function TLoadShapeObj.Get_Mean: Double;
begin
    if not FStdDevCalculated then
        CalcMeanandStdDev;
    Result := FMean;
end;

function TLoadShapeObj.Get_StdDev: Double;
begin
    if not FStdDevCalculated then
        CalcMeanandStdDev;
    Result := FStdDev;
end;

function TLoadShapeObj.Mult(i: Integer): Double;
begin
    if (i <= FNumPoints) and (i > 0) then
    begin
        if dblPMultipliers <> nil then
            Result := dblPMultipliers^[i]
        else
            Result := sngPMultipliers^[i];

        LastValueAccessed := i;
    end
    else
        Result := 0.0;

end;

function TLoadShapeObj.Hour(i: Integer): Double;
begin
    if Interval = 0 then
    begin
        if (i <= FNumPoints) and (i > 0) then
        begin
            if dblHours <> nil then
                Result := dblHours^[i]
            else
                Result := sngHours^[i];

            LastValueAccessed := i;
        end
        else
            Result := 0.0;
    end
    else
    begin
        if dblHours <> nil then
            Result := dblHours^[i] * Interval
        else
            Result := sngHours^[i] * Interval;

        LastValueAccessed := i;
    end;

end;


procedure TLoadShapeObj.DumpProperties(var F: TextFile; Complete: Boolean);

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

function TLoadShapeObj.GetPropertyValue(Index: Integer): String;
begin
    if (Index > NumPropsThisClass) then
    begin
        Result := inherited GetPropertyValue(index);
        Exit;
    end;

    Result := '';

    case TLoadShapeProp(Index) of
        TLoadShapeProp.npts:
            Result := IntToStr(FNumPoints);
        TLoadShapeProp.interval:
            Result := Format('%.8g', [Interval]);
        TLoadShapeProp.Pmult, TLoadShapeProp.mult:
            if dblPMultipliers <> NIL then
                Result := GetDSSArray_Real(FNumPoints, pDoubleArray(dblPMultipliers))
            else if sngPMultipliers <> NIL then
                Result := GetDSSArray_Single(FNumPoints, pSingleArray(sngPMultipliers));
        TLoadShapeProp.hour:
            if dblHours <> NIL then
                Result := GetDSSArray_Real(FNumPoints, pDoubleArray(dblHours))
            else if sngHours <> NIL then
                Result := GetDSSArray_Single(FNumPoints, pSingleArray(sngHours));
        TLoadShapeProp.mean:
            Result := Format('%.8g', [Mean]);
        TLoadShapeProp.stddev:
            Result := Format('%.8g', [StdDev]);
        TLoadShapeProp.qmult:
            if Assigned(dblQMultipliers) then
                Result := GetDSSArray_Real(FNumPoints, pDoubleArray(dblQMultipliers))
            else if Assigned(sngQMultipliers) then
                Result := GetDSSArray_Single(FNumPoints, pSingleArray(sngQMultipliers));
        TLoadShapeProp.UseActual:
            if UseActual then
                Result := 'Yes'
            else
                Result := 'No';
        TLoadShapeProp.Pmax:
            Result := Format('%.8g', [MaxP]);
        TLoadShapeProp.Qmax:
            Result := Format('%.8g', [MaxQ]);
        TLoadShapeProp.sinterval:
            Result := Format('%.8g', [Interval * 3600.0]);
        TLoadShapeProp.minterval:
            Result := Format('%.8g', [Interval * 60.0]);
        TLoadShapeProp.Pbase:
            Result := Format('%.8g', [BaseP]);
        TLoadShapeProp.Qbase:
            Result := Format('%.8g', [BaseQ]);
        TLoadShapeProp.PQCSVFile:
            Result := PQCSVFilename;
        TLoadShapeProp.CSVFile:
            Result := CSVFilename;
        TLoadShapeProp.SngFile:
            Result := SngFilename;
        TLoadShapeProp.DblFile:
            Result := DblFilename;
        TLoadShapeProp.action:
            Result := '';
    else
        begin
            Result := 'ERROR';
            DoSimpleMsg(DSS, 'Property number "' + IntToStr(Index) + '" is not handled for LoadShape', 541);
        end
    end;

end;

procedure TLoadShapeObj.InitPropertyValues(ArrayOffset: Integer);
begin
    inherited  InitPropertyValues(NumPropsThisClass);
end;

procedure TLoadShape.TOPExport(ObjName: String);

var
    NameList, CNames: TStringList;
    Vbuf, CBuf: pDoubleArray;
    Obj: TLoadShapeObj;
    MaxPts, i, j: Integer;
    MaxTime, MinInterval, Hr_Time: Double;
    ObjList: TPointerList;
    TOPTransferFile: TOutFile32;
begin
    TOPTransferFile.FileName := DSS.OutputDirectory + 'TOP_LoadShape.STO';
    try
        TOPTransferFile.Open;
    except
        ON E: Exception do
        begin
            DoSimpleMsg(DSS, 'TOP Transfer File Error: ' + E.message, 619);
            try
                TopTransferFile.Close;
                {OK if Error}
            finally
                TOPTransferFile.Free;
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
                DoSimpleMsg(DSS, 'Loadshape.' + ObjName + ' is not hourly fixed interval.', 620);
        end
        else
        begin
            DoSimpleMsg(DSS, 'Loadshape.' + ObjName + ' not found.', 621);
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

        TopTransferFile.WriteHeader(0.0, MaxTime, MinInterval, ObjList.ListSize, 0, 16, 'DSS (TM), Electrotek Concepts (R)', DSS.DefaultBaseFreq);
        TopTransferFile.WriteNames(NameList, CNames);

        Hr_Time := 0.0;

        VBuf := AllocMem(Sizeof(Double) * ObjList.ListSize);
        CBuf := AllocMem(Sizeof(Double) * 1);   // just a dummy -- Cbuf is ignored here

        for i := 1 to MaxPts do
        begin
            for j := 1 to ObjList.ListSize do
            begin
                Obj := ObjList.Get(j);
                VBuf^[j] := Obj.GetMult(Hr_Time).Re;
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
    TOPTransferFile.Free;
end;

procedure TLoadShapeObj.SaveToDblFile;
var
    F: file of Double;
    i: Integer;
    Fname: String;
    Temp: Double;
begin
    UseFloat64();
    if Assigned(dblPMultipliers) then
    begin
        try
            FName := Format('%s_P.dbl', [Name]);
            AssignFile(F, Fname);
            Rewrite(F);
            for i := 1 to NumPoints do
                Write(F, dblPMultipliers^[i]);
            DSS.GlobalResult := 'mult=[dblfile=' + FName + ']';
        finally
            CloseFile(F);
        end;

        if Assigned(dblQMultipliers) then
        begin
            try
                FName := Format('%s_Q.dbl', [Name]);
                AssignFile(F, Fname);
                Rewrite(F);
                for i := 1 to NumPoints do
                    Write(F, dblQMultipliers^[i]);
                AppendGlobalResult(DSS, ' Qmult=[dblfile=' + FName + ']');
            finally
                CloseFile(F);
            end;
        end;
    end
    else
        DoSimpleMsg(DSS, 'Loadshape.' + Name + ' P multipliers not defined.', 622);
end;

procedure TLoadShapeObj.SaveToSngFile;
var
    F: file of Single;
    i: Integer;
    Fname: String;
    Temp: Single;

begin
    UseFloat64();
    if Assigned(dblPMultipliers) then
    begin
        try
            FName := Format('%s_P.sng', [Name]);
            AssignFile(F, Fname);
            Rewrite(F);
            for i := 1 to NumPoints do
            begin
                Temp := dblPMultipliers^[i];
                Write(F, Temp);
            end;
            DSS.GlobalResult := 'mult=[sngfile=' + FName + ']';
        finally
            CloseFile(F);
        end;

        if Assigned(dblQMultipliers) then
        begin
            try
                FName := Format('%s_Q.sng', [Name]);
                AssignFile(F, Fname);
                Rewrite(F);
                for i := 1 to NumPoints do
                begin
                    Temp := dblQMultipliers^[i];
                    Write(F, Temp);
                end;
                AppendGlobalResult(DSS, ' Qmult=[sngfile=' + FName + ']');
            finally
                CloseFile(F);
            end;
        end;
    end
    else
        DoSimpleMsg(DSS, 'Loadshape.' + Name + ' P multipliers not defined.', 623);
end;

procedure TLoadShapeObj.SetMaxPandQ;
begin
    if Assigned(dblPMultipliers) then
    begin
        iMaxP := iMaxAbsdblArrayValue(NumPoints, dblPMultipliers);
        if iMaxP > 0 then
        begin
            MaxP := dblPMultipliers^[iMaxP];
            if not MaxQSpecified then
                if Assigned(dblQMultipliers) then
                    MaxQ := dblQMultipliers^[iMaxP]
                else
                    MaxQ := 0.0;
        end;
    end
    else
    begin
        iMaxP := iMaxAbssngArrayValue(NumPoints, sngPMultipliers);
        if iMaxP > 0 then
        begin
            MaxP := sngPMultipliers^[iMaxP];
            if not MaxQSpecified then
                if Assigned(sngQMultipliers) then
                    MaxQ := sngQMultipliers^[iMaxP]
                else
                    MaxQ := 0.0;
        end;
    end;
end;

procedure TLoadShapeObj.Set_Mean(const Value: Double);
begin
    FStdDevCalculated := TRUE;
    FMean := Value;
end;

procedure TLoadShapeObj.Set_StdDev(const Value: Double);
begin
    FStdDevCalculated := TRUE;
    FStdDev := Value;
end;

procedure TLoadShapeObj.SetDataPointers(HoursPtr: PDouble; PMultPtr: PDouble; QMultPtr: PDouble);
begin
    if not ExternalMemory then
    begin
        if Assigned(dblHours) then
            ReallocMem(dblHours, 0);
        if Assigned(dblPMultipliers) then
            ReallocMem(dblPMultipliers, 0);
        if Assigned(dblQMultipliers) then
            ReallocMem(dblQMultipliers, 0);
        if Assigned(sngHours) then
            ReallocMem(sngHours, 0);
        if Assigned(sngPMultipliers) then
            ReallocMem(sngPMultipliers, 0);
        if Assigned(sngQMultipliers) then
            ReallocMem(sngQMultipliers, 0);
    end;
    sngHours := nil;
    sngPMultipliers := nil;
    sngQMultipliers := nil;
    dblHours := ArrayDef.PDoubleArray(HoursPtr);
    dblPMultipliers := ArrayDef.PDoubleArray(PMultPtr);
    dblQMultipliers := ArrayDef.PDoubleArray(QMultPtr);
    if Assigned(dblPMultipliers) then
        SetMaxPandQ;
end;

procedure TLoadShapeObj.SetDataPointersSingle(HoursPtr: PSingle; PMultPtr: PSingle; QMultPtr: PSingle);
begin
    if not ExternalMemory then
    begin
        if Assigned(dblHours) then
            ReallocMem(dblHours, 0);
        if Assigned(dblPMultipliers) then
            ReallocMem(dblPMultipliers, 0);
        if Assigned(dblQMultipliers) then
            ReallocMem(dblQMultipliers, 0);
        if Assigned(sngHours) then
            ReallocMem(sngHours, 0);
        if Assigned(sngPMultipliers) then
            ReallocMem(sngPMultipliers, 0);
        if Assigned(sngQMultipliers) then
            ReallocMem(sngQMultipliers, 0);
    end;
    dblHours := nil;
    dblPMultipliers := nil;
    dblQMultipliers := nil;
    sngHours := ArrayDef.PSingleArray(HoursPtr);
    sngPMultipliers := ArrayDef.PSingleArray(PMultPtr);
    sngQMultipliers := ArrayDef.PSingleArray(QMultPtr);
    if Assigned(sngPMultipliers) then
        SetMaxPandQ;
end;

procedure TLoadShapeObj.UseFloat32();
var
    i: Integer;
begin
    if DSS.ActiveLoadShapeObj.ExternalMemory then
    begin
        DoSimpleMsg(DSS, 'Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61103);
        Exit;
    end;

    if Assigned(dblHours) then
    begin
        ReallocMem(sngHours, FNumPoints * SizeOf(Single));
        for i := 1 to FNumPoints do
            sngHours[i] := dblHours[i];
        FreeMem(dblHours);
        dblHours := nil;
    end;
    if Assigned(dblPMultipliers) then
    begin
        ReallocMem(sngPMultipliers, FNumPoints * SizeOf(Single));
        for i := 1 to FNumPoints do
            sngPMultipliers[i] := dblPMultipliers[i];
        FreeMem(dblPMultipliers);
        dblPMultipliers := nil;
    end;
    if Assigned(dblQMultipliers) then
    begin
        ReallocMem(sngQMultipliers, FNumPoints * SizeOf(Single));
        for i := 1 to FNumPoints do
            sngQMultipliers[i] := dblQMultipliers[i];
        FreeMem(dblQMultipliers);
        dblQMultipliers := nil;
    end;

end;

procedure TLoadShapeObj.UseFloat64();
var 
    i: Integer;
begin
    if DSS.ActiveLoadShapeObj.ExternalMemory then
    begin
        DoSimpleMsg(DSS, 'Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61104);
        Exit;
    end;

    if Assigned(sngHours) then
    begin
        ReallocMem(dblHours, FNumPoints * SizeOf(Double));
        for i := 1 to FNumPoints do
            dblHours[i] := sngHours[i];
        FreeMem(sngHours);
        sngHours := nil;
    end;
    if Assigned(sngPMultipliers) then
    begin
        ReallocMem(dblPMultipliers, FNumPoints * SizeOf(Double));
        for i := 1 to FNumPoints do
            dblPMultipliers[i] := sngPMultipliers[i];
        FreeMem(sngPMultipliers);
        sngPMultipliers := nil;
    end;
    if Assigned(sngQMultipliers) then
    begin
        ReallocMem(dblQMultipliers, FNumPoints * SizeOf(Double));
        for i := 1 to FNumPoints do
            dblQMultipliers[i] := sngQMultipliers[i];
        FreeMem(sngQMultipliers);
        sngQMultipliers := nil;
    end;
end;

end.
