unit LoadShape;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  Copyright (c) 2019-2021, Paulo Meira
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
    Arraydef,
    Utilities,
{$IFDEF WINDOWS}
    Windows;
{$ELSE}
    BaseUnix,
    Unix;
{$ENDIF}

type
{$SCOPEDENUMS ON}
    TMMShapeType = (
        P = 0,
        Q = 1
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
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

        function Find(const ObjName: String): Pointer; OVERRIDE;  // Find an obj of this class by name

        procedure TOPExport(ObjName: String);
        function CreateMMF(const S: String; Destination: TMMShapeType): Boolean;
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
        function GetMultAtHourSingle(hr: Double): Complex;
    PUBLIC
        Interval: Double;  //=0.0 then random interval     (hr)

        // Double data
        dH: pDoubleArray0; // Time values (hr) if Interval > 0.0 else nil
        dP, dQ: pDoubleArray0;  // Multipliers, zero based
        // Single data
        sH, sP, sQ: pSingleArray0; //zero based

        MaxP, MaxQ, BaseP, BaseQ: Double;

        Enabled, UseActual, ExternalMemory: Boolean;
        Stride: Integer;

        // Memory mapping variables
        UseMMF: Boolean;            // Flag to indicated that the user wants to use MMF
        mmMMF,                      // Handle for the memory map (P)
        mmFile,                     // Handle for the file to be mapped (P)
        mmQMMF,                     // Handle for the memory map (Q)
        mmQFile: THandle;           // Handle for the file to be mapped (Q)
        mmFileSizeQ,                // File size of the file opened (P)
        mmFileSize: Cardinal;       // File size of the file opened (P)
        mmFileCmdQ,
        mmFileCmd: String;          // The file definition added by the user (for moving the data window)
        mmViewQ,                    // Current view of the file mapped (Bytes - Q)
        mmView: pByte;              // Current view of the file mapped (Bytes - P)
        mmFileType,                 // The file type (P)
        mmFileTypeQ: TLSFileType;   // The file type (Q)
        mmColumn,                   // The column to read (P)
        mmColumnQ,                  // The column to read (Q)
        mmLineLen,                  // The size of the char line (P)
        mmLineLenQ,                 // The size of the char line (Q)
        mmDataSize,                 // The total data size expected (P)
        mmDataSizeQ,                // The total data size expected (Q)
        mmViewLenQ,                 // Memory View size in bytes (Q)
        mmViewLen: Integer;         // Memory View size in bytes (P)

        constructor Create(ParClass: TDSSClass; const LoadShapeName: String);
        destructor Destroy; OVERRIDE;

        function GetMultAtHour(hr: Double): Complex;  // Get multiplier at specified time
        function Mult(i: Integer): Double;  // get multiplier by index -- used in SolutionAlgs, updates LastValueAccessed
        function PMult(i: Integer): Double;  // get multiplier by index -- used in SolutionAlgs, doesn't update LastValueAccessed 
        function QMult(i: Integer; var m: Double): Boolean;  // get multiplier by index
        function Hour(i: Integer): Double;  // get hour corresponding to point index
        procedure Normalize;
        procedure SetMaxPandQ;

        procedure LoadMMFView(const Parmname: String; MMF: THandle; Destination: TMMShapeType);
        procedure LoadFileFeatures(ShapeType: TMMShapeType);

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

        property NumPoints: Integer READ FNumPoints WRITE FNumPoints;
        property PresentInterval: Double READ Get_Interval;
        property Mean: Double READ Get_Mean WRITE Set_Mean;
        property StdDev: Double READ Get_StdDev WRITE Set_StdDev;

        procedure SetDataPointers(HoursPtr: PDouble; PMultPtr: PDouble; QMultPtr: PDouble; DStride: Integer);
        procedure SetDataPointersSingle(HoursPtr: PSingle; PMultPtr: PSingle; QMultPtr: PSingle; SStride: Integer);
        procedure UseFloat32;
        procedure UseFloat64;
    end;

var
    ActiveLoadShapeObj: TLoadShapeObj;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    MathUtil,
    Classes,
    TOPExport,
    Math,
    PointerList;

type
    ELoadShapeError = class(Exception);  // Raised to abort solution

const
    NumPropsThisClass = 21;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TLoadShape.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
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
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names
    PropertyName[1] := 'npts';     // Number of points to expect
    PropertyName[2] := 'interval'; // default = 1.0;
    PropertyName[3] := 'mult';     // vector of power multiplier values
    PropertyName[4] := 'hour';     // vextor of hour values
    PropertyName[5] := 'mean';     // set the mean (otherwise computed)
    PropertyName[6] := 'stddev';   // set the std dev (otherwise computed)
    PropertyName[7] := 'csvfile';  // Switch input to a csvfile
    PropertyName[8] := 'sngfile';  // switch input to a binary file of singles
    PropertyName[9] := 'dblfile';  // switch input to a binary file of singles
    PropertyName[10] := 'action';  // actions  Normalize
    PropertyName[11] := 'qmult';   // Q multiplier
    PropertyName[12] := 'UseActual'; // Flag to signify to use actual value
    PropertyName[13] := 'Pmax'; // MaxP value
    PropertyName[14] := 'Qmax'; // MaxQ
    PropertyName[15] := 'sinterval'; // Interval in seconds
    PropertyName[16] := 'minterval'; // Interval in minutes
    PropertyName[17] := 'Pbase'; // for normalization, use peak if 0
    PropertyName[18] := 'Qbase'; // for normalization, use peak if 0
    PropertyName[19] := 'Pmult'; // synonym for Mult
    PropertyName[20] := 'PQCSVFile'; // Redirect to a file with p, q pairs
    PropertyName[21] := 'MemoryMapping'; // Enable/disable using Memory mapping for this shape

     // define Property help values

    PropertyHelp[1] := 'Max number of points to expect in load shape vectors. This gets reset to the number of multiplier values found (in files only) if less than specified.';     // Number of points to expect
    PropertyHelp[2] := 'Time interval for fixed interval data, hrs. Default = 1. ' +
        'If Interval = 0 then time data (in hours) may be at either regular or  irregular intervals and time value must be specified using either the Hour property or input files. ' +
        'Then values are interpolated when Interval=0, but not for fixed interval data.  ' + CRLF + CRLF +
        'See also "sinterval" and "minterval".'; // default = 1.0;
    PropertyHelp[3] := 'Array of multiplier values for active power (P) or other key value (such as pu V for Vsource). ' + CRLF + CRLF +
        'You can also use the syntax: ' + CRLF + CRLF +
        'mult = (file=filename)     !for text file one value per line' + CRLF +
        'mult = (dblfile=filename)  !for packed file of doubles' + CRLF +
        'mult = (sngfile=filename)  !for packed file of singles ' + CRLF +
        'mult = (file=MyCSVFile.CSV, col=3, header=yes)  !for multicolumn CSV files ' + CRLF + CRLF +
        'Note: this property will reset Npts if the  number of values in the files are fewer.' + CRLF + CRLF +
        'Same as Pmult';     // vector of power multiplier values
    PropertyHelp[4] := 'Array of hour values. Only necessary to define for variable interval data (Interval=0).' +
        ' If you set Interval>0 to denote fixed interval data, DO NOT USE THIS PROPERTY. ' +
        'You can also use the syntax: ' + CRLF +
        'hour = (file=filename)     !for text file one value per line' + CRLF +
        'hour = (dblfile=filename)  !for packed file of doubles' + CRLF +
        'hour = (sngfile=filename)  !for packed file of singles ';     // vextor of hour values
    PropertyHelp[5] := 'Mean of the active power multipliers.  This is computed on demand the first time a ' +
        'value is needed.  However, you may set it to another value independently. ' +
        'Used for Monte Carlo load simulations.';     // set the mean (otherwise computed)
    PropertyHelp[6] := 'Standard deviation of active power multipliers.  This is computed on demand the first time a ' +
        'value is needed.  However, you may set it to another value independently.' +
        'Is overwritten if you subsequently read in a curve' + CRLF + CRLF +
        'Used for Monte Carlo load simulations.';   // set the std dev (otherwise computed)
    PropertyHelp[7] := 'Switch input of active power load curve data to a CSV text file ' +
        'containing (hour, mult) points, or simply (mult) values for fixed time interval data, one per line. ' +
        'NOTE: This action may reset the number of points to a lower value.';   // Switch input to a csvfile
    PropertyHelp[8] := 'Switch input of active power load curve data to a binary file of singles ' +
        'containing (hour, mult) points, or simply (mult) values for fixed time interval data, packed one after another. ' +
        'NOTE: This action may reset the number of points to a lower value.';  // switch input to a binary file of singles
    PropertyHelp[9] := 'Switch input of active power load curve data to a binary file of doubles ' +
        'containing (hour, mult) points, or simply (mult) values for fixed time interval data, packed one after another. ' +
        'NOTE: This action may reset the number of points to a lower value.';   // switch input to a binary file of singles
    PropertyHelp[10] := '{NORMALIZE | DblSave | SngSave} After defining load curve data, setting action=normalize ' +
        'will modify the multipliers so that the peak is 1.0. ' +
        'The mean and std deviation are recomputed.' + CRLF + CRLF +
        'Setting action=DblSave or SngSave will cause the present mult and qmult values to be written to ' +
        'either a packed file of double or single. The filename is the loadshape name. The mult array will have a ' +
        '"_P" appended on the file name and the qmult array, if it exists, will have "_Q" appended.'; // Action
    PropertyHelp[11] := 'Array of multiplier values for reactive power (Q).  You can also use the syntax: ' + CRLF +
        'qmult = (file=filename)     !for text file one value per line' + CRLF +
        'qmult = (dblfile=filename)  !for packed file of doubles' + CRLF +
        'qmult = (sngfile=filename)  !for packed file of singles ' + CRLF +     // vector of qmultiplier values
        'qmult = (file=MyCSVFile.CSV, col=4, header=yes)  !for multicolumn CSV files ';
    PropertyHelp[12] := '{Yes | No* | True | False*} If true, signifies to Load, Generator, Vsource, or other objects to ' +
        'use the return value as the actual kW, kvar, kV, or other value rather than a multiplier. ' +
        'Nominally for AMI Load data but may be used for other functions.';
    PropertyHelp[13] := 'kW value at the time of max power. Is automatically set upon reading in a loadshape. ' +
        'Use this property to override the value automatically computed or to retrieve the value computed.';
    PropertyHelp[14] := 'kvar value at the time of max kW power. Is automatically set upon reading in a loadshape. ' +
        'Use this property to override the value automatically computed or to retrieve the value computed.';
    PropertyHelp[15] := 'Specify fixed interval in SECONDS. Alternate way to specify Interval property.';
    PropertyHelp[16] := 'Specify fixed interval in MINUTES. Alternate way to specify Interval property.';
    PropertyHelp[17] := 'Base P value for normalization. Default is zero, meaning the peak will be used.';
    PropertyHelp[18] := 'Base Q value for normalization. Default is zero, meaning the peak will be used.';
    PropertyHelp[19] := 'Synonym for "mult".';
    PropertyHelp[20] := 'Switch input to a CSV text file containing (active, reactive) power (P, Q) multiplier pairs, one per row. ' + CRLF +
        'If the interval=0, there should be 3 items on each line: (hour, Pmult, Qmult)';
    PropertyHelp[21] := '{Yes | No* | True | False*} Enables the memory mapping functionality for dealing with large amounts of load shapes. ' + CRLF +
        'By default is False. Use it to accelerate the model loading when the containing a large number of load shapes.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLoadShape.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    with ActiveCircuit do
    begin
        ActiveDSSObject := TLoadShapeObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

// Loads the mapped file features into local variables for further use     *
procedure TLoadShapeObj.LoadFileFeatures(ShapeType: TMMShapeType);
var
    LocalCol: Integer;
    fileType: TLSFileType;
    ParmName,
    Param: String;
begin
    AuxParser.CmdString := mmFileCmd;
    ParmName := AuxParser.NextParam;
    LocalCol := 1;

    if CompareText(Parmname, 'file') = 0 then
    begin
        fileType := TLSFileType.PlainText;

        // Look for other options  (may be in either order)
        ParmName := AuxParser.NextParam;
        Param := AuxParser.StrValue;
        while Length(Param) > 0 do
        begin
            if CompareTextShortest(ParmName, 'column') = 0 then
                LocalCol := AuxParser.IntValue;
            ParmName := AuxParser.NextParam;
            Param := AuxParser.StrValue;
        end;
    end
    else if CompareText(Parmname, 'dblfile') = 0 then
        fileType := TLSFileType.Float64
    else if CompareText(Parmname, 'sngfile') = 0 then
        fileType := TLSFileType.Float32;

    if ShapeType = TMMShapeType.P then
    begin
        mmFileType := fileType;
        mmColumn := LocalCol;
    end
    else
    begin
        mmFileTypeQ := fileType;
        mmColumnQ := LocalCol;
    end;
end;

// Loads the active MMF view into memory for further use
procedure TLoadShapeObj.LoadMMFView(const Parmname: String; MMF: THandle; Destination: TMMShapeType);
var
    FirstPos: Integer;
    lastCh: Byte;
begin
    // processes the view depending on the file type
    FirstPos := 1;
    if Destination = TMMShapeType.P then
    begin
        mmView := PByte(MapViewOfFile(MMF, FILE_MAP_READ, 0, 0, mmViewLen));
        if CompareText(Parmname, 'file') = 0 then // standard csv file
        begin
            lastCh := mmView[FirstPos];
            while lastCh <> $0A do
            begin
                inc(FirstPos);
                lastCh := mmView[FirstPos];
            end;
            mmLineLen := FirstPos + 1;
        end
        // DBL file
        else if (Length(Parmname) > 0) and (CompareTextShortest(Parmname, 'dblfile') = 0) then
            mmLineLen := sizeof(Double)
        // SGL file
        else if (Length(Parmname) > 0) and (CompareTextShortest(Parmname, 'sngfile') = 0) then
            mmLineLen := sizeof(Single);
    end
    else
    begin
        mmViewQ := PByte(MapViewOfFile(MMF, FILE_MAP_READ, 0, 0, mmViewLen));
        if CompareText(Parmname, 'file') = 0 then // standard csv file
        begin
            lastCh := mmViewQ[FirstPos];
            while lastCh <> $0A do
            begin
                inc(FirstPos);
                lastCh := mmViewQ[FirstPos];
            end;
            mmLineLenQ := FirstPos + 1;
        end
        // DBL file
        else if (Length(Parmname) > 0) and (CompareTextShortest(Parmname, 'dblfile') = 0) then
            mmLineLenQ := sizeof(Double)
        // SGL file
        else if (Length(Parmname) > 0) and (CompareTextShortest(Parmname, 'sngfile') = 0) then
            mmLineLenQ := sizeof(Single);
    end;
end;

// Creates the Memory mapping for the file specified
function TLoadShape.CreateMMF(const S: String; Destination: TMMShapeType): Boolean;
var
    ParmName,
    Param: String;
    localMMF: THandle;
begin
    with ActiveLoadShapeObj do
    try
        AuxParser.CmdString := S;
        ParmName := AuxParser.NextParam;
        Param := AdjustInputFilePath(AuxParser.StrValue);
        if not FileExists(Param) then
        begin
            DoSimpleMsg(Format('The file "%s" does not exist. Process cancelled.', [Param]), 800002);
            Result := False;
        end;
        
        if Destination = TMMShapeType.P then
        begin
            // Opens the file for this instance
            mmFile := CreateFile(Pchar(Param), GENERIC_READ, FILE_SHARE_READ, NIL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
            // Creates the memory map for the file
            mmMMF := CreateFileMapping(mmFile, NIL, PAGE_READONLY, 0, 0, NIL);
            localMMF := mmMMF;  // Assignment for working locally
            mmFileCmd := S;
            mmFileSize := GetFileSize(mmFile, NIL);
            mmViewLen := mmFileSize;
        end
        else
        begin
            // Creating mapping for Q
            // Opens the file for this instance
            mmQFile := CreateFile(Pchar(Param), GENERIC_READ, FILE_SHARE_READ, NIL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
            // Creates the memory map for the file
            mmQMMF := CreateFileMapping(mmQFile, NIL, PAGE_READONLY, 0, 0, NIL);
            localMMF := mmQMMF; // Assignment for working locally
            mmFileCmdQ := S;
            mmFileSizeQ := GetFileSize(mmFile, NIL);
            mmViewLenQ := mmFileSizeQ;
        end;

        LoadMMFView(ParmName, localMMF, Destination);
        Result := True;
    except
        DoSimpleMsg(Format('There was a problem mapping file "%s". Process cancelled.', [Param]), 800001);
        Result := False;
    end;
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
    ActiveLoadShapeObj := ElementList.Active;
    ActiveDSSObject := ActiveLoadShapeObj;

    with ActiveLoadShapeObj do
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
                1: // npts:
                    if ActiveLoadShapeObj.ExternalMemory then
                    begin
                        DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
                        Exit;
                    end
                    else
                    begin
                        NumPoints := Parser.Intvalue;
                        // Force as the always first property when saving in a later point
                        PrpSequence[ParamPointer] := -10;
                    end;
                2: // interval:
                    Interval := Parser.DblValue;
                3, 19: // Pmult, mult:
                begin
                    if ActiveLoadShapeObj.ExternalMemory then
                    begin
                        DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
                        Exit;
                    end;                
                    if UseMMF then
                    begin
                        if not CreateMMF(Param, TMMShapeType.P) then
                            Exit; // CreateMMF throws an error message already
                        LoadFileFeatures(TMMShapeType.P);
                        mmDataSize := NumPoints;
                        ReAllocmem(dP, sizeof(Double) * 2);
                        Exit;
                    end;

                    // Otherwise, follow the traditional technique for loading up load shapes
                	UseFloat64;
                    ReAllocmem(dP, Sizeof(Double) * NumPoints);
                    // Allow possible Resetting (to a lower value) of num points when specifying multipliers not Hours
                    NumPoints := InterpretDblArray(Param, NumPoints, PDoubleArray(dP));   // Parser.ParseAsVector(Npts, Multipliers);
                end;
                4: // hour:
                begin
                    if ActiveLoadShapeObj.ExternalMemory then
                    begin
                        DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
                        Exit;
                    end;
                    UseFloat64;
                    ReAllocmem(dH, Sizeof(Double) * NumPoints);
                    InterpretDblArray(Param, NumPoints, PDoubleArray(dH));   // Parser.ParseAsVector(Npts, Hours);
                    Interval := 0.0;
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
                    case lowercase(Param)[1] of
                        'n':
                            Normalize;
                        'd':
                            SaveToDblFile;
                        's':
                            SaveToSngFile;
                    end;
                11: // qmult:
                begin
                    if ActiveLoadShapeObj.ExternalMemory then
                    begin
                        DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61105);
                        Exit;
                    end;
                    if UseMMF then
                    begin
                        if not CreateMMF(Param, TMMShapeType.Q) then
                            Exit; // CreateMMF throws an error message already
                        LoadFileFeatures(TMMShapeType.Q);
                        if Assigned(dP) then
                            mmDataSizeQ := mmDataSize
                        else
                            mmDataSizeQ := NumPoints;
                        ReAllocmem(dQ, sizeof(dQ[1]) * 2);
                        Exit;
                    end;
					// Otherwise, follow the traditional technique for loading up load shapes                    
                    UseFloat64;
                    ReAllocmem(dQ, Sizeof(dQ[1]) * NumPoints);
                    InterpretDblArray(Param, NumPoints, PDoubleArray(dQ));   // Parser.ParseAsVector(Npts, Multipliers);
                end;
                12: // UseActual:
                    UseActual := InterpretYesNo(Param);
                13:
                    MaxP := Parser.DblValue;
                14:
                    MaxQ := Parser.DblValue;
                15:
                    Interval := Parser.DblValue / 3600.0;  // Convert seconds to hr
                16:
                    Interval := Parser.DblValue / 60.0;  // Convert minutes to hr
                17:
                    BaseP := Parser.DblValue;
                18:
                    BaseQ := Parser.DblValue;
                20:
                    Do2ColCSVFile(AdjustInputFilePath(Param));
                21:
                begin
                    UseMMF := InterpretYesNo(Param);
                    if UseMMF then
                    begin
                        UseFloat64;
                    end;
                end;

            else
           // Inherited parameters
                ClassEdit(ActiveLoadShapeObj, ParamPointer - NumPropsThisClass)
            end;

            case ParamPointer of
                3, 7, 8, 9, 11:
                begin
                    FStdDevCalculated := FALSE;   // now calculated on demand
                    ArrayPropertyIndex := ParamPointer;
                    NumPoints := FNumPoints;  // Keep Properties in order for save command
                end;
                14:
                    MaxQSpecified := TRUE;

            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end; {WHILE}

        if Assigned(dP) or Assigned(sP) then
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
        with ActiveLoadShapeObj do
        begin
            if ExternalMemory then
            begin
                // There is no point in copying a static loadshape,
                // so we assume the user would want to modify the data
                dP := nil;
                dQ := nil;
                dH:= nil;
                sP := nil;
                sQ := nil;
                sH:= nil;
                ExternalMemory := False;
            end;

            NumPoints := OtherLoadShape.NumPoints;
            Interval := OtherLoadShape.Interval;
            Stride := 1;

            // Double versions
            if Assigned(OtherLoadShape.dP) then
            begin
                ReallocMem(dP, SizeOf(Double) * NumPoints);
                //Move(OtherLoadShape.dP[0], dP[0], SizeOf(Double) * NumPoints);
                for i := 1 to NumPoints do
                    dP[i] := OtherLoadShape.dP[Stride * i];
            end
            else
                ReallocMem(dP, 0);

            if Assigned(OtherLoadShape.dQ) then
            begin
                ReallocMem(dQ, SizeOf(Double) * NumPoints);
                //Move(OtherLoadShape.dQ[0], dQ[0], SizeOf(Double) * NumPoints);
                for i := 1 to NumPoints do
                    dQ[i] := OtherLoadShape.dQ[Stride * i];
                
            end;

            if Interval > 0.0 then
                ReallocMem(dH, 0)
            else
            begin
                ReallocMem(dH, SizeOf(Double) * NumPoints);
                // Move(OtherLoadShape.dH[0], dH[0], SizeOf(Double) * NumPoints);
                for i := 1 to NumPoints do
                    dH[i] := OtherLoadShape.dH[Stride * i];
            end;

            // Single versions
            if Assigned(OtherLoadShape.sP) then
            begin
                ReallocMem(sP, SizeOf(Single) * NumPoints);
                // Move(OtherLoadShape.sP[0], sP[0], SizeOf(Single) * NumPoints);
                for i := 1 to NumPoints do
                    sP[i] := OtherLoadShape.sP[Stride * i];
            end
            else
                ReallocMem(sP, 0);

            if Assigned(OtherLoadShape.sQ) then
            begin
                ReallocMem(sQ, SizeOf(Single) * NumPoints);
                // Move(OtherLoadShape.sQ[0], sQ[0], SizeOf(Single) * NumPoints);
                for i := 1 to NumPoints do
                    sQ[i] := OtherLoadShape.sQ[Stride * i];
            end;

            if Interval > 0.0 then
                ReallocMem(sH, 0)
            else
            begin
                ReallocMem(sH, SizeOf(Single) * NumPoints);
                // Move(OtherLoadShape.sH[0], sH[0], SizeOf(Single) * NumPoints);
                for i := 1 to NumPoints do
                    sH[i] := OtherLoadShape.sH[Stride * i];
            end;

            SetMaxPandQ;
            UseActual := OtherLoadShape.UseActual;
            UseMMF := OtherLoadShape.UseMMF;
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
        DoSimpleMsg('Error in LoadShape MakeLike: "' + ShapeName + '" Not Found.', 611);
end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLoadShape.Init(Handle: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TLoadShape.Init', -1);
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
    ActiveLoadShapeObj := NIL;
    LoadShapeObj := ElementList.First;
    while LoadShapeObj <> NIL do
    begin
        if CompareText(LoadShapeObj.Name, Value) = 0 then
        begin
            ActiveLoadShapeObj := LoadShapeObj;
            Exit;
        end;
        LoadShapeObj := ElementList.Next;
    end;
    DoSimpleMsg('LoadShape: "' + Value + '" not Found.', 612);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShape.Do2ColCSVFile(const FileName: String);
//   Process 2-column CSV file (3-col if time expected)
var
    F: Textfile;
    i: Integer;
    s: String;
begin
    if ActiveLoadShapeObj.ExternalMemory then
    begin
        DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
        Exit;
    end;

    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 613);
        CloseFile(F);
        Exit;
    end;

    with ActiveLoadShapeObj do
    try
        if UseMMF then
        begin
            CloseFile(F);
            mmDataSize := NumPoints;
            mmFileCmd := 'file=' + FileName + ' column=1';      // Command for P
            if not CreateMMF(mmFileCmd, TMMShapeType.P) then  // Creates MMF for the whole file
                Exit; // CreateMMF throws an error message already
            
            mmViewQ := mmView;
            LoadFileFeatures(TMMShapeType.P);
            mmFileCmd := 'file=' + FileName + ' column=2';      // Command for Q
            LoadFileFeatures(TMMShapeType.Q);
            mmDataSize := NumPoints;
            mmLineLenQ := mmLineLen;
            ReAllocmem(dP, sizeof(Double) * 2);
            ReAllocmem(dQ, sizeof(Double) * 2);
            Exit;
        end;

        // Allocate both P and Q multipliers
        UseFloat64;
        ReAllocmem(dP, sizeof(Double) * NumPoints);
        ReAllocmem(dQ, Sizeof(Double) * NumPoints);
        if Interval = 0.0 then
            ReAllocmem(dH, Sizeof(Double) * NumPoints);
        i := -1;
        while (not EOF(F)) and (i < (FNumPoints - 1)) do
        begin
            Inc(i);
            Readln(F, s); // read entire line and parse with AuxParser
            {AuxParser allows commas or white space}
            with AuxParser do
            begin
                CmdString := s;
                if Interval = 0.0 then
                begin
                    NextParam;
                    dH[i] := DblValue;
                end;
                NextParam;
                dP[i] := DblValue;  // first parm
                NextParam;
                dQ[i] := DblValue;  // second parm
            end;
        end;
        CloseFile(F);
        inc(i);
        if i <> FNumPoints then
            NumPoints := i;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error Processing CSV File: "' + FileName + '. ' + E.Message, 614);
            CloseFile(F);
            Exit;
        end;
    end;
end;

procedure TLoadShape.DoCSVFile(const FileName: String);
var
    F: Textfile;
    i: Integer;
    s: String;
begin
    if ActiveLoadShapeObj.ExternalMemory then
    begin
        DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
        Exit;
    end;

    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 613);
        CloseFile(F);
        Exit;
    end;

    with ActiveLoadShapeObj do
    try
        if UseMMF then
        begin
            CloseFile(F);
            s := 'file=' + FileName;
            if CreateMMF(s, TMMShapeType.P) then
                Exit; // CreateMMF throws an error message already

            LoadFileFeatures(TMMShapeType.P);
            mmDataSize := NumPoints;
            ReAllocmem(dP, sizeof(Double) * 2);
            Exit;
        end;

        UseFloat64;
        ReAllocmem(dP, sizeof(Double) * NumPoints);
        if Interval = 0.0 then
            ReAllocmem(dH, Sizeof(Double) * NumPoints);
        i := -1;
        while (not EOF(F)) and (i < (FNumPoints - 1)) do
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
                    dH[i] := DblValue;
                end;
                NextParam;
                dP[i] := DblValue;
            end;
        end;
        CloseFile(F);
        inc(i);
        if i <> FNumPoints then
            NumPoints := i;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error Processing CSV File: "' + FileName + '. ' + E.Message, 614);
            CloseFile(F);
            Exit;
        end;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShape.DoSngFile(const FileName: String);
var
    s: String;
    F: file of Single;
    Hr, M: Single;
    i: Integer;
begin
    if ActiveLoadShapeObj.ExternalMemory then
    begin
        DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
        Exit;
    end;

    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 615);
        CloseFile(F);
        Exit;
    end;

    with ActiveLoadShapeObj do
    try
        if UseMMF then
        begin
            CloseFile(F);
            s := 'sngfile=' + FileName;
            if not CreateMMF(s, TMMShapeType.P) then
                Exit; // CreateMMF throws an error message already

            LoadFileFeatures(TMMShapeType.P);
            mmDataSize := NumPoints;
            ReAllocmem(dP, sizeof(Double) * 2);
            Exit;
        end;

        UseFloat64;
        ReAllocmem(dP, sizeof(Double) * NumPoints);
        if Interval = 0.0 then
            ReAllocmem(dH, Sizeof(Double) * NumPoints);
        i := -1;
        while (not EOF(F)) and (i < (FNumPoints - 1)) do
        begin
            Inc(i);
            if Interval = 0.0 then
            begin
                Read(F, Hr);
                dH[i] := Hr;
            end;
            Read(F, M);
            dP[i] := M;
        end;
        CloseFile(F);
        inc(i);
        if i <> FNumPoints then
            NumPoints := i;
    except
        DoSimpleMsg('Error Processing LoadShape File: "' + FileName, 616);
        CloseFile(F);
        Exit;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShape.DoDblFile(const FileName: String);
var
    s: String;
    F: file of Double;
    i: Integer;
begin
    if ActiveLoadShapeObj.ExternalMemory then
    begin
        DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
        Exit;
    end;

    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 617);
        CloseFile(F);
        Exit;
    end;

    with ActiveLoadShapeObj do
    try
        if UseMMF then
        begin
            CloseFile(F);
            s := 'dblfile=' + FileName;
            if not CreateMMF(s, TMMShapeType.P) then
                Exit; // CreateMMF throws an error message already
            
            LoadFileFeatures(TMMShapeType.P);
            mmDataSize := NumPoints;
            ReAllocmem(dP, sizeof(Double) * 2);
            Exit;
        end;

        UseFloat64;
        ReAllocmem(dP, sizeof(Double) * NumPoints);
        if Interval = 0.0 then
            ReAllocmem(dH, Sizeof(Double) * NumPoints);
        i := -1;
        while (not EOF(F)) and (i < (FNumPoints - 1)) do
        begin
            Inc(i);
            if Interval = 0.0 then
                Read(F, dH[i]);
            Read(F, dP[i]);
        end;
        CloseFile(F);
        inc(i);
        if i <> FNumPoints then
            NumPoints := i;
    except
        DoSimpleMsg('Error Processing LoadShape File: "' + FileName, 618);
        CloseFile(F);
        Exit;
    end;
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
    Stride := 1;
    LastValueAccessed := 1;

    FNumPoints := 0;
    Interval := 1.0;  // hr
    dH := NIL;
    dP := NIL;
    dQ := NIL;
    sH := NIL;
    sP := NIL;
    sQ := NIL;
    MaxP := 1.0;
    MaxQ := 0.0;
    BaseP := 0.0;
    BaseQ := 0.0;
    UseActual := FALSE;
    UseMMF := FALSE;  // No memory mapping by default
    MaxQSpecified := FALSE;
    FStdDevCalculated := FALSE;  // calculate on demand
    Enabled := True;

    mmViewLen := 1000;   // 1kB by default, it may change for not missing a row

    ArrayPropertyIndex := 0;

    InitPropertyValues(0);

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLoadShapeObj.Destroy;
begin
    if not ExternalMemory then
    begin
        if Assigned(dH) then
            ReallocMem(dH, 0);
        if Assigned(dP) then
            ReallocMem(dP, 0);
        if Assigned(dQ) then
            ReallocMem(dQ, 0);
        if Assigned(sH) then
            ReallocMem(sH, 0);
        if Assigned(sP) then
            ReallocMem(sP, 0);
        if Assigned(sQ) then
            ReallocMem(sQ, 0);
    end;
    if UseMMF then
    begin
        UnmapViewOfFile(mmView);
        CloseHandle(mmMMF);
        CloseHandle(mmFile);
        CloseHandle(mmQMMF);
        CloseHandle(mmQFile);
    end;
    inherited destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLoadShapeObj.GetMultAtHour(hr: Double): Complex;
// This function returns a multiplier for the given hour.
// If no points exist in the curve, the result is  1.0
// If there are fewer points than requested, the curve is simply assumed to repeat
// Thus a daily load curve can suffice for a yearly load curve:  You just get the
// same day over and over again.
// The value returned is the nearest to the interval requested.  Thus if you request
// hour=12.25 and the interval is 1.0, you will get interval 12.
var
    i, 
    offset, // index including stride
    poffset: Integer; // previous index including stride
    
    function Set_Result_im(const realpart: Double): Double;
    {Set imaginary part of Result when Qmultipliers not defined}
    begin
        if UseActual then
            Set_Result_im := 0.0       // if actual, assume zero
        else
            Set_Result_im := realpart; // same as real otherwise
    end;
    
begin
    if Assigned(sP) then
    begin
        Result := GetMultAtHourSingle(hr);
        exit;
    end;

    Result.re := 1.0;
    Result.im := 1.0;    // default return value if no points in curve

    if FNumPoints <= 0 then
        Exit;
        
    if FNumPoints = 1 then
    begin
        Result.re := dP[0];
        if Assigned(dQ) then
            Result.im := dQ[0]
        else
            Result.im := Set_Result_im(Result.re);
        Exit;
    end;

    if Interval > 0.0 then
    begin
        i := round(hr / Interval) - 1;
        if UseMMF then
        begin
            i := i mod mmDataSize;  // Wrap around using remainder
            Result.re := InterpretDblArrayMMF(mmView, mmFileType, mmColumn, i + 1, mmLineLen);
            if Assigned(dQ) then
                Result.im := InterpretDblArrayMMF(mmViewQ, mmFileTypeQ, mmColumnQ, i + 1, mmLineLenQ)
            else
                Result.im := Set_Result_im(Result.re);
            
            Exit;
        end;
        
        offset := i * Stride;
        i := i mod FNumPoints;  // Wrap around using remainder
        Result.Re := dP[offset];
        if Assigned(dQ) then
            Result.im := dQ[offset]
        else
            Result.im := Set_Result_im(Result.re);

        Exit;
    end;

    // For random interval

    // Start with previous value accessed under the assumption that most
    //  of the time, this function will be called sequentially

    // Normalize Hr to max hour in curve to get wraparound
    if Hr > dH[Stride * (FNumPoints - 1)] then
    begin
        offset := Stride * (FNumPoints - 1);
        Hr := Hr - Trunc(Hr / dH[offset]) * dH[offset];
    end;
    
    if dH[Stride * LastValueAccessed] > Hr then
        LastValueAccessed := 0;  // Start over from beginning
        
    for i := LastValueAccessed to FNumPoints - 1 do
    begin
        offset := Stride * i;
        if Abs(dH[offset] - Hr) < 0.00001 then  // If close to an actual point, just use it.
        begin
            if UseMMF then
            begin
                Result.re := InterpretDblArrayMMF(mmView, mmFileType, mmColumn, i + 1, mmLineLen);
                if Assigned(dQ) then
                    Result.im := InterpretDblArrayMMF(mmViewQ, mmFileTypeQ, mmColumnQ, i + 1, mmLineLenQ)
                else
                    Result.im := Set_Result_im(Result.re);
            
                LastValueAccessed := i;
                Exit;
            end;
            
            Result.re := dP[offset];
            if Assigned(dQ) then
                Result.im := dQ[offset]
            else
                Result.im := Set_Result_im(Result.re);
            LastValueAccessed := i;
            Exit;
        end;
        
        if dH[offset] > Hr then      // Interpolate for multiplier
        begin
            LastValueAccessed := i - 1;
            poffset := offset - Stride;
            if UseMMF then
            begin
                Result.re := InterpretDblArrayMMF(mmView, mmFileType, mmColumn, LastValueAccessed + 1, mmLineLen) +
                    (Hr - dH[LastValueAccessed]) / (dH[i] - dH[LastValueAccessed]) *
                    (InterpretDblArrayMMF(mmView, mmFileType, mmColumn, i, mmLineLen) -
                    InterpretDblArrayMMF(mmView, mmFileType, mmColumn, LastValueAccessed, mmLineLen));
                if Assigned(dQ) then
                    Result.im := InterpretDblArrayMMF(mmViewQ, mmFileTypeQ, mmColumnQ, LastValueAccessed + 1, mmLineLenQ) +
                        (Hr - dH[LastValueAccessed]) / (dH[i] - dH[LastValueAccessed]) *
                        (InterpretDblArrayMMF(mmViewQ, mmFileTypeQ, mmColumnQ, i, mmLineLenQ) -
                        InterpretDblArrayMMF(mmViewQ, mmFileTypeQ, mmColumnQ, LastValueAccessed, mmLineLenQ))
                else
                    Result.im := Set_Result_im(Result.re);
                    
                Exit;
            end;

            Result.re := dP[poffset] + (Hr - dH[poffset]) / (dH[i] - dH[poffset]) * (dP[i] - dP[poffset]);
            if Assigned(dQ) then
                Result.im := dQ[poffset] + (Hr - dH[poffset]) / (dH[i] - dH[poffset]) * (dQ[i] - dQ[poffset])
            else
                Result.im := Set_Result_im(Result.re);
            Exit;
        end;
    end;

// If we fall through the loop, just use last value
    LastValueAccessed := FNumPoints - 2;
    Result.re := dP[Stride * LastValueAccessed];
    if Assigned(dQ) then
        Result.im := dQ[Stride * LastValueAccessed]
    else
        Result.im := Set_Result_im(Result.re);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShapeObj.Normalize;
// normalize this load shape
var
    MaxMult: Double;

    procedure DoNormalize(Multipliers: pDoubleArray0);
    var
        i: Integer;
    begin
        if FNumPoints > 0 then
        begin
            if MaxMult <= 0.0 then
            begin
                MaxMult := Abs(Multipliers[0]);
                for i := 1 to FNumPoints - 1 do
                    MaxMult := Max(MaxMult, Abs(Multipliers[i]));
            end;
            if MaxMult = 0.0 then
                MaxMult := 1.0; // Avoid divide by zero
            for i := 0 to FNumPoints - 1 do
                Multipliers[i] := Multipliers[i] / MaxMult;
        end;
    end;

    procedure DoNormalizeSingle(Multipliers: pSingleArray0);
    var
        i: Integer;
    begin
        if FNumPoints > 0 then
        begin
            if MaxMult <= 0.0 then
			begin
                MaxMult := Abs(Multipliers[0]);
                for i := 1 to FNumPoints - 1 do
                    MaxMult := Max(MaxMult, Abs(Multipliers[i]));
            end;
            if MaxMult = 0.0 then
                MaxMult := 1.0; // Avoid divide by zero
            for i := 0 to FNumPoints - 1 do
                Multipliers[i] := Multipliers[i] / MaxMult;
        end;
    end;

begin
    if UseMMF or ExternalMemory  then //TODO: disallow MMF?
    begin
        DoSimpleMsg('Data cannot be changed for LoadShapes with external memory or memory-mapped files! Reset the data first.', 61102);
        Exit;
    end;

    MaxMult := BaseP;
    if Assigned(dP) then
    begin
        DoNormalize(dP);
        if Assigned(dQ) then
        begin
            MaxMult := BaseQ;
            DoNormalize(dQ);
        end;
    end
    else
    begin
        DoNormalizeSingle(sP);
        if Assigned(sQ) then
    	begin
        	MaxMult := BaseQ;
            DoNormalizeSingle(sQ);
        end;
    end;
    UseActual := FALSE;  // not likely that you would want to use the actual if you normalized it.
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShapeObj.CalcMeanandStdDev;
begin
    if FNumPoints > 0 then
    begin
        if Assigned(dP) then
        begin
            if Interval > 0.0 then
                RCDMeanandStdDev(dP, FNumPoints, FMean, FStdDev)
            else
                CurveMeanAndStdDev(PDoubleArray(dP), PDoubleArray(dH), FNumPoints, FMean, FStdDev);
        end
        else
        Begin
            if Interval > 0.0 then
                RCDMeanandStdDevSingle(sP, FNumPoints, FMean, FStdDev)
            else
                CurveMeanAndStdDevSingle(PSingleArray(sP), PSingleArray(sH), FNumPoints, FMean, FStdDev);
        End;
    end;
    PropertyValue[5] := Format('%.8g', [FMean]);
    PropertyValue[6] := Format('%.8g', [FStdDev]);

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
            if dH <> nil then
                Result := dH[Stride * LastValueAccessed] - dH[(LastValueAccessed - 1) * Stride]
            else
                Result := sH[Stride * LastValueAccessed] - sH[(LastValueAccessed - 1) * Stride]
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
        if UseMMF then
            Result := InterpretDblArrayMMF(mmView, mmFileType, mmColumn, i, mmLineLen)
        else if dP <> nil then
            Result := dP[Stride * i]
        else
            Result := sP[Stride * i];

        LastValueAccessed := i;
    end
    else
        Result := 0.0;
end;

function TLoadShapeObj.PMult(i: Integer): Double;
begin
    if (i <= FNumPoints) and (i > 0) then
    begin
        if UseMMF then
            Result := InterpretDblArrayMMF(mmView, mmFileType, mmColumn, i, mmLineLen)
        else if dP <> nil then
            Result := dP[Stride * i]
        else
            Result := sP[Stride * i];
    end
    else
        Result := 0.0;
end;

function TLoadShapeObj.QMult(i: Integer; var m: Double): Boolean;
begin
    Result := False;
    if (dQ = nil) and (sQ = nil) then
        Exit;
    Result := True;
    
    if (i <= FNumPoints) and (i > 0) then
    begin
        if UseMMF then
            m := InterpretDblArrayMMF(mmViewQ, mmFileTypeQ, mmColumnQ, i, mmLineLenQ)
        else if dQ <> nil then
            m := dQ[Stride * i]
        else
            m := sQ[Stride * i];
    end
    else
        m := 0.0;
end;


function TLoadShapeObj.Hour(i: Integer): Double;
begin

    if Interval = 0 then
    begin
        if (i <= FNumPoints) and (i > 0) then
        begin
            if dH <> nil then
                Result := dH[Stride * i]
            else
                Result := sH[Stride * i];

            LastValueAccessed := i;
        end
        else
            Result := 0.0;
    end
    else
    begin
        if dH <> nil then
            Result := dH[Stride * i] * Interval
        else
            Result := sH[Stride * i] * Interval;

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
    Result := '';

    case Index of
        1:
            Result := IntToStr(FNumPoints);
        2:
            Result := Format('%.8g', [Interval]);
        3, 19:
        begin
            if UseMMF then
            begin
                Result := '(' + mmFileCmd + ')';
                Exit;
            end;
            if dP <> NIL then
                Result := GetDSSArray_Real(FNumPoints, pDoubleArray(dP))
            else if sP <> NIL then
                Result := GetDSSArray_Single(FNumPoints, pSingleArray(sP));
        end;
        4:
            if dH <> NIL then
                Result := GetDSSArray_Real(FNumPoints, pDoubleArray(dH))
            else if sH <> NIL then
                Result := GetDSSArray_Single(FNumPoints, pSingleArray(sH));
        5:
            Result := Format('%.8g', [Mean]);
        6:
            Result := Format('%.8g', [StdDev]);
        11:
        begin
            if UseMMF then
            begin
                Result := '(' + mmFileCmdQ + ')';
                Exit;
            end;
            if Assigned(dQ) then
                Result := GetDSSArray_Real(FNumPoints, pDoubleArray(dQ))
            else if Assigned(sQ) then
                Result := GetDSSArray_Single(FNumPoints, pSingleArray(sQ));
        end;
        12:
            if UseActual then
                Result := 'Yes'
            else
                Result := 'No';
        13:
            Result := Format('%.8g', [MaxP]);
        14:
            Result := Format('%.8g', [MaxQ]);
        15:
            Result := Format('%.8g', [Interval * 3600.0]);
        16:
            Result := Format('%.8g', [Interval * 60.0]);
        17:
            Result := Format('%.8g', [BaseP]);
        18:
            Result := Format('%.8g', [BaseQ]);
        21:
            if UseMMF then
                Result := 'Yes'
            else
                Result := 'No';
    else
        Result := inherited GetPropertyValue(index);
    end;

end;

procedure TLoadShapeObj.InitPropertyValues(ArrayOffset: Integer);
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
    PropertyValue[10] := ''; // action option .
    PropertyValue[11] := ''; // Qmult.
    PropertyValue[12] := 'No';
    PropertyValue[13] := '0';
    PropertyValue[14] := '0';
    PropertyValue[15] := '3600';   // seconds
    PropertyValue[16] := '60';     // minutes
    PropertyValue[17] := '0';
    PropertyValue[18] := '0';
    PropertyValue[19] := '';   // same as 3
    PropertyValue[20] := '';  // switch input to csv file of P, Q pairs
    PropertyValue[21] := 'No';  // memory mapped load shape


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

begin
    TOPTransferFile.FileName := GetOutputDirectory + 'TOP_LoadShape.STO';
    try
        TOPTransferFile.Open;
    except
        ON E: Exception do
        begin
            DoSimpleMsg('TOP Transfer File Error: ' + E.message, 619);
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
                DoSimpleMsg('Loadshape.' + ObjName + ' is not hourly fixed interval.', 620);
        end
        else
        begin
            DoSimpleMsg('Loadshape.' + ObjName + ' not found.', 621);
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
                VBuf^[j] := Obj.GetMultAtHour(Hr_Time).Re;
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

procedure TLoadShapeObj.SaveToDblFile;

var
    myDBL: Double;
    F: file of Double;
    i: Integer;
    Fname: String;
begin
    UseFloat64;
    if Assigned(dP) then
    begin
        try
            FName := OutputDirectory {CurrentDSSDir} + Format('%s_P.dbl', [Name]);
            AssignFile(F, Fname);
            Rewrite(F);
            if UseMMF then
            begin
                for i := 1 to NumPoints do
                begin
                    myDBL := InterpretDblArrayMMF(mmView, mmFileType, mmColumn, i, mmLineLen);
                    Write(F, myDBL);
                end;
            end
            else
                for i := 1 to NumPoints do
                    Write(F, dP[Stride * i]);
            GlobalResult := 'mult=[dblfile=' + FName + ']';
        finally
            CloseFile(F);
        end;

        if Assigned(dQ) then
        begin
            try
                FName := OutputDirectory {CurrentDSSDir} + Format('%s_Q.dbl', [Name]);
                AssignFile(F, Fname);
                Rewrite(F);
                if UseMMF then
                begin
                    for i := 1 to NumPoints do
                    begin
                        myDBL := InterpretDblArrayMMF(mmViewQ, mmFileTypeQ, mmColumnQ, i, mmLineLenQ);
                        Write(F, myDBL);
                    end;
                end
                else
                    for i := 1 to NumPoints do
                        Write(F, dQ[Stride * i]);
                AppendGlobalResult(' Qmult=[dblfile=' + FName + ']');
            finally
                CloseFile(F);
            end;
        end;

    end
    else
        DoSimpleMsg('Loadshape.' + Name + ' P multipliers not defined.', 622);
end;

procedure TLoadShapeObj.SaveToSngFile;

var
    F: file of Single;
    i: Integer;
    Fname: String;
    Temp: Single;

begin
    UseFloat64;
    if Assigned(dP) then
    begin
        try
            FName := OutputDirectory {CurrentDSSDir} + Format('%s_P.sng', [Name]);
            AssignFile(F, Fname);
            Rewrite(F);
            for i := 1 to NumPoints do
            begin
                if UseMMF then
                    Temp := InterpretDblArrayMMF(mmView, mmFileType, mmColumn, i, mmLineLen)
                else
                    Temp := dP[Stride * i];
                Write(F, Temp);
            end;
            GlobalResult := 'mult=[sngfile=' + FName + ']';
        finally
            CloseFile(F);
        end;

        if Assigned(dQ) then
        begin
            try
                FName := OutputDirectory {CurrentDSSDir} + Format('%s_Q.sng', [Name]);
                AssignFile(F, Fname);
                Rewrite(F);
                for i := 1 to NumPoints do
                begin
                    if UseMMF then
                        Temp := InterpretDblArrayMMF(mmViewQ, mmFileTypeQ, mmColumnQ, i, mmLineLenQ)
                    else
                        Temp := dQ[Stride * i];
                    Write(F, Temp);
                end;
                AppendGlobalResult(' Qmult=[sngfile=' + FName + ']');
            finally
                CloseFile(F);
            end;
        end;
    end
    else
        DoSimpleMsg('Loadshape.' + Name + ' P multipliers not defined.', 623);
end;

procedure TLoadShapeObj.SetMaxPandQ;
begin
    if UseMMF then
        Exit;

    if Assigned(dP) then
    begin
        iMaxP := iMaxAbsdblArrayValue(NumPoints, PDoubleArray(dP));
        if iMaxP > 0 then
        begin
            MaxP := dP[Stride * iMaxP];
            if not MaxQSpecified then
                if Assigned(dQ) then
                    MaxQ := dQ[Stride * iMaxP]
                else
                    MaxQ := 0.0;
        end;
    end
    else
    begin
        iMaxP := iMaxAbssngArrayValue(NumPoints, PSingleArray(sP));
        if iMaxP > 0 then
        begin
            MaxP := sP[Stride * iMaxP];
            if not MaxQSpecified then
                if Assigned(sQ) then
                    MaxQ := sQ[Stride * iMaxP]
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

procedure TLoadShapeObj.SetDataPointers(HoursPtr: PDouble; PMultPtr: PDouble; QMultPtr: PDouble; DStride: Integer);
begin
    if not ExternalMemory then
    begin
        if Assigned(dH) then
            ReallocMem(dH, 0);
        if Assigned(dP) then
            ReallocMem(dP, 0);
        if Assigned(dQ) then
            ReallocMem(dQ, 0);
        if Assigned(sH) then
            ReallocMem(sH, 0);
        if Assigned(sP) then
            ReallocMem(sP, 0);
        if Assigned(sQ) then
            ReallocMem(sQ, 0);
    end;
    sH := nil;
    sP := nil;
    sQ := nil;
    dH := ArrayDef.PDoubleArray0(HoursPtr);
    dP := ArrayDef.PDoubleArray0(PMultPtr);
    dQ := ArrayDef.PDoubleArray0(QMultPtr);
    
    if ExternalMemory then
    begin
        if (DStride <> 0) then
            Stride := DStride
        else
            Stride := 1;
    end
    else
        Stride := 1;
    
    if Assigned(dP) then
        SetMaxPandQ;
end;

procedure TLoadShapeObj.SetDataPointersSingle(HoursPtr: PSingle; PMultPtr: PSingle; QMultPtr: PSingle; SStride: Integer);
begin
    if not ExternalMemory then
    begin
        if Assigned(dH) then
            ReallocMem(dH, 0);
        if Assigned(dP) then
            ReallocMem(dP, 0);
        if Assigned(dQ) then
            ReallocMem(dQ, 0);
        if Assigned(sH) then
            ReallocMem(sH, 0);
        if Assigned(sP) then
            ReallocMem(sP, 0);
        if Assigned(sQ) then
            ReallocMem(sQ, 0);
    end;
    dH := nil;
    dP := nil;
    dQ := nil;
    sH := ArrayDef.PSingleArray0(HoursPtr);
    sP := ArrayDef.PSingleArray0(PMultPtr);
    sQ := ArrayDef.PSingleArray0(QMultPtr);
    
    if ExternalMemory then
    begin
        if (SStride <> 0) then
            Stride := SStride
        else
            Stride := 1;
    end
    else
        Stride := 1;
    
    if Assigned(sP) then
        SetMaxPandQ;
end;

procedure TLoadShapeObj.UseFloat32;
var
    i: Integer;
begin
    if ActiveLoadShapeObj.UseMMF then
    begin
        DoSimpleMsg('Data cannot be toggled to 32-bit floats when memory-mapping is enabled.', 61106);
        Exit;
    end;

    if ActiveLoadShapeObj.ExternalMemory then
    begin
        DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61103);
        Exit;
    end;

    if Assigned(dH) then
    begin
        ReallocMem(sH, FNumPoints * SizeOf(Single));
        for i := 1 to FNumPoints do
            sH[i] := dH[i];
        FreeMem(dH);
        dH := nil;
    end;
    if Assigned(dP) then
    begin
        ReallocMem(sP, FNumPoints * SizeOf(Single));
        for i := 1 to FNumPoints do
            sP[i] := dP[i];
        FreeMem(dP);
        dP := nil;
    end;
    if Assigned(dQ) then
    begin
        ReallocMem(sQ, FNumPoints * SizeOf(Single));
        for i := 1 to FNumPoints do
            sQ[i] := dQ[i];
        FreeMem(dQ);
        dQ := nil;
    end;

end;

procedure TLoadShapeObj.UseFloat64;
var 
    i: Integer;
begin
    if UseMMF then // data has to be already using float64, we can skip this
        Exit;
        
    if ExternalMemory then
    begin
        DoSimpleMsg('Data cannot be changed for LoadShapes with external memory or memory-mapped files! Reset the data first.', 61104);
        Exit;
    end;

    if Assigned(sH) then
    begin
        ReallocMem(dH, FNumPoints * SizeOf(Double));
        for i := 1 to FNumPoints do
            dH[i] := sH[i];
        FreeMem(sH);
        sH := nil;
    end;
    if Assigned(sP) then
    begin
        ReallocMem(dP, FNumPoints * SizeOf(Double));
        for i := 1 to FNumPoints do
            dP[i] := sP[i];
        FreeMem(sP);
        sP := nil;
    end;
    if Assigned(sQ) then
    begin
        ReallocMem(dQ, FNumPoints * SizeOf(Double));
        for i := 1 to FNumPoints do
            dQ[i] := sQ[i];
        FreeMem(sQ);
        sQ := nil;
    end;
end;

function TLoadShapeObj.GetMultAtHourSingle(hr: Double): Complex;
var
    i, 
    offset, // index including stride
    poffset: Integer; // previous index including stride
    
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

    if FNumPoints <= 0 then         // Handle Exceptional cases
        Exit;

    if FNumPoints = 1 then
    begin
        Result.re := sP[0];
        if Assigned(sQ) then
            Result.im := sQ[0]
        else
            Result.im := Set_Result_im(Result.re);
        Exit;
    end;

    if Interval > 0.0 then
    begin
        i := round(hr / Interval) - 1;
        offset := i * Stride;
        i := i mod FNumPoints;  // Wrap around using remainder
        Result.Re := sP[offset];
        if Assigned(sQ) then
            Result.im := sQ[offset]
        else
            Result.im := Set_Result_im(Result.re);

        Exit;
    end;

    // For random interval

    // Start with previous value accessed under the assumption that most
    //  of the time, this function will be called sequentially

    // Normalize Hr to max hour in curve to get wraparound
    if Hr > sH[Stride * (FNumPoints - 1)] then
    begin
        offset := Stride * (FNumPoints - 1);
        Hr := Hr - Trunc(Hr / sH[offset]) * sH[offset];
    end;

    if sH[Stride * LastValueAccessed] > Hr then
        LastValueAccessed := 0;  // Start over from beginning
    
    for i := LastValueAccessed to FNumPoints - 1 do
    begin
        offset := Stride * i;
        if Abs(sH[offset] - Hr) < 0.00001 then  // If close to an actual point, just use it.
        begin
            Result.re := sP[offset];
            if Assigned(sQ) then
                Result.im := sQ[offset]
            else
                Result.im := Set_Result_im(Result.re);
            LastValueAccessed := i;
            Exit;
        end;
        
        if sH[offset] > Hr then      // Interpolate for multiplier
        begin
            LastValueAccessed := i - 1;
            poffset := offset - Stride;
            Result.re := sP[poffset] + (Hr - sH[poffset]) / (sH[offset] - sH[poffset]) * (sP[offset] - sP[poffset]);
            if Assigned(sQ) then
                Result.im := sQ[poffset] + (Hr - sH[poffset]) / (sH[offset] - sH[poffset]) * (sQ[offset] - sQ[poffset])
            else
                Result.im := Set_Result_im(Result.re);
            Exit;
        end;
    end;

    // If we fall through the loop, just use last value
    LastValueAccessed := FNumPoints - 2;
    Result.re := sP[Stride * LastValueAccessed];
    if Assigned(sQ) then
        Result.im := sQ[Stride * LastValueAccessed]
    else
        Result.im := Set_Result_im(Result.re);
end;

end.
