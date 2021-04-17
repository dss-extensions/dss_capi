unit LoadShape;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
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
    Windows;

type

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

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

        function Find(const ObjName: String): Pointer; OVERRIDE;  // Find an obj of this class by name

        procedure TOPExport(ObjName: String);
        function CreateMMF(const S: String; Destination: Integer): Integer;
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
        procedure Set_NumPoints(const Value: Integer);
        procedure SaveToDblFile;
        procedure SaveToSngFile;
        procedure CalcMeanandStdDev;
        function Get_Mean: Double;
        function Get_StdDev: Double;
        procedure Set_Mean(const Value: Double);
        procedure Set_StdDev(const Value: Double);  // Normalize the curve presently in memory
        procedure SetMaxPandQ;

    PUBLIC

        Interval: Double;  //=0.0 then random interval     (hr)
        Hours,          // Time values (hr) if Interval > 0.0  Else nil
        PMultipliers,
        QMultipliers: pDoubleArray;  // Multipliers

        MaxP,
        MaxQ,
        BaseP,
        BaseQ: Double;

        Enabled,
        UseActual: Boolean;

        {***********************************************************************
        *                    Memory mapping variables                          *
        ************************************************************************}

        UseMMF: Boolean;            // Flag to indicated that the user wants to use MMF
        myMMF,                                    // Handle for the memory map (P)
        myFile,                                   // Handle for the file to be mapped (P)
        myQMMF,                                   // Handle for the memory map (Q)
        myQFile: THandle;            // Handle for the file to be mapped (Q)
        myFileSizeQ,                              // File size of the file opened (P)
        myFileSize: Cardinal;           // File size of the file opened (P)
        myFileCmdQ,
        myFileCmd: String;             // The file definition added by the user (for moving the data window)
        myViewQ,                                  // Current view of the file mapped (Bytes - Q)
        myView: pByte;              // Current view of the file mapped (Bytes - P)
        myFileType,                               // The file type (P)
        myFileTypeQ,                              // The file type (Q)
        myColumn,                                 // The column to read (P)
        myColumnQ,                                // The column to read (Q)
        myLineLen,                                // The size of the char line (P)
        myLineLenQ,                               // The size of the char line (Q)
        myDataSize,                               // The total data size expected (P)
        myDataSizeQ,                              // The total data size expected (Q)
        MyViewLenQ,                               // Memory View size in bytes (Q)
        MyViewLen: Integer;            // Memory View size in bytes (P)

        //**********************************************************************

        constructor Create(ParClass: TDSSClass; const LoadShapeName: String);
        destructor Destroy; OVERRIDE;

        function GetMult(hr: Double): Complex;           // Get multiplier at specified time
        function Mult(i: Integer): Double;               // get multiplier by index
        function Hour(i: Integer): Double;               // get hour corresponding to point index
        procedure Normalize;
        // Loads the current view of the MMF into memory for further use
        procedure LoadMMFView(const Parmname: String; MMF: THandle; Destination: Integer);
        procedure LoadFileFeatures(ShapeType: Integer);


        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

        property NumPoints: Integer READ FNumPoints WRITE Set_NumPoints;
        property PresentInterval: Double READ Get_Interval;
        property Mean: Double READ Get_Mean WRITE Set_Mean;
        property StdDev: Double READ Get_StdDev WRITE Set_StdDev;

        {Property FirstMult :Double Read Get_FirstMult;}
        {Property NextMult  :Double Read Get_NextMult;}

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
    Utilities,
    Classes,
    TOPExport,
    Math,
    PointerList;

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
    PropertyName[1] := 'npts';           // Number of points to expect
    PropertyName[2] := 'interval';       // default = 1.0;
    PropertyName[3] := 'mult';           // vector of power multiplier values
    PropertyName[4] := 'hour';           // vextor of hour values
    PropertyName[5] := 'mean';           // set the mean (otherwise computed)
    PropertyName[6] := 'stddev';         // set the std dev (otherwise computed)
    PropertyName[7] := 'csvfile';        // Switch input to a csvfile
    PropertyName[8] := 'sngfile';        // switch input to a binary file of singles
    PropertyName[9] := 'dblfile';        // switch input to a binary file of singles
    PropertyName[10] := 'action';        // actions  Normalize
    PropertyName[11] := 'qmult';         // Q multiplier
    PropertyName[12] := 'UseActual';     // Flag to signify to use actual value
    PropertyName[13] := 'Pmax';          // MaxP value
    PropertyName[14] := 'Qmax';          // MaxQ
    PropertyName[15] := 'sinterval';     // Interval in seconds
    PropertyName[16] := 'minterval';     // Interval in minutes
    PropertyName[17] := 'Pbase';         // for normalization, use peak if 0
    PropertyName[18] := 'Qbase';         // for normalization, use peak if 0
    PropertyName[19] := 'Pmult';         // synonym for Mult
    PropertyName[20] := 'PQCSVFile';     // Redirect to a file with p, q pairs
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
        'By defaul is False. Use it to accelerate the model loading when the containing a large number of load shapes.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLoadShape.NewObject(const ObjName: String): Integer;
begin
  // create a new object of this class and add to list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveDSSObject[ActiveActor] := TLoadShapeObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;


{*******************************************************************************
*      Loads the mapped file features into local variables for further use     *
********************************************************************************}
procedure TLoadShapeObj.LoadFileFeatures(ShapeType: Integer);
var
    LocalCol,
    myType: Integer;
    ParmName,
    Param: String;
begin

    AuxParser[ActiveActor].CmdString := myFileCmd;
    ParmName := AuxParser[ActiveActor].NextParam;
    Param := AuxParser[ActiveActor].StrValue;
    LocalCol := 1;

    if CompareText(Parmname, 'file') = 0 then
    begin
     {Default values}
        myType := 0;
     // Default options

    // Look for other options  (may be in either order)
        ParmName := AuxParser[ActiveActor].NextParam;
        Param := AuxParser[ActiveActor].StrValue;
        while Length(Param) > 0 do
        begin
            if CompareTextShortest(ParmName, 'column') = 0 then
                LocalCol := AuxParser[ActiveActor].IntValue;
            ParmName := AuxParser[ActiveActor].NextParam;
            Param := AuxParser[ActiveActor].StrValue;
        end;
    end

    else
    if CompareText(Parmname, 'dblfile') = 0 then
        myType := 1
    else
    if CompareText(Parmname, 'sngfile') = 0 then
        myType := 2;
    if ShapeType = 0 then // P
    begin
        myFileType := myType;
        myColumn := LocalCol;
    end
    else
    begin
        myFileTypeQ := myType;
        myColumnQ := LocalCol;
    end;

end;

{*******************************************************************************
*         Uploads the active MMF view into memory for further use              *
********************************************************************************}
procedure TLoadShapeObj.LoadMMFView(const Parmname: String; MMF: THandle; Destination: Integer);
// Destination
//  0   : P
//  1   : Q
var
    FirstPos: Integer;
    myLastCh: Byte;

begin
  // processes the view depending on the file type
    FirstPos := 1;
    if Destination = 0 then
    begin
        myView := PByte(MapViewOfFile(MMF, FILE_MAP_READ, 0, 0, myViewlen));
        if CompareText(Parmname, 'file') = 0 then // starndard csv file
        begin
            myLastCh := myView[FirstPos];
            while myLastCh <> $0A do
            begin
                inc(FirstPos);
                myLastCh := myView[FirstPos];
            end;
            myLineLen := FirstPos + 1;
        end
    // DBL file
        else
        if (Length(Parmname) > 0) and (CompareTextShortest(Parmname, 'dblfile') = 0) then
            myLineLen := sizeof(Double)
    // SGL file
        else
        if (Length(Parmname) > 0) and (CompareTextShortest(Parmname, 'sngfile') = 0) then
            myLineLen := sizeof(Single);
    end
    else
    begin
        myViewQ := PByte(MapViewOfFile(MMF, FILE_MAP_READ, 0, 0, myViewlen));
        if CompareText(Parmname, 'file') = 0 then // starndard csv file
        begin
            myLastCh := myViewQ[FirstPos];
            while myLastCh <> $0A do
            begin
                inc(FirstPos);
                myLastCh := myViewQ[FirstPos];
            end;
            myLineLenQ := FirstPos + 1;
        end
    // DBL file
        else
        if (Length(Parmname) > 0) and (CompareTextShortest(Parmname, 'dblfile') = 0) then
            myLineLenQ := sizeof(Double)
    // SGL file
        else
        if (Length(Parmname) > 0) and (CompareTextShortest(Parmname, 'sngfile') = 0) then
            myLineLenQ := sizeof(Single);
    end;

end;

{*******************************************************************************
*   Creates the Memory mapping for the file specified, Destination is used to  *
*   Indicate the destinaton (0 = P, 1 = Q)                                     *
********************************************************************************}

function TLoadShape.CreateMMF(const S: String; Destination: Integer): Integer;
var

    ParmName,
    Param: String;
    myLastCh: Byte;
    i: Integer;
    myLocalMMF: THandle;

begin

    with ActiveLoadShapeObj do
    begin
        try

            AuxParser[ActiveActor].CmdString := S;
            ParmName := AuxParser[ActiveActor].NextParam;
            Param := AuxParser[ActiveActor].StrValue;
            if fileexists(Pchar(Param)) then
            begin
                if Destination = 0 then
                begin
          // Creating mapping for P
          // Opens the file for this instance
                    myFile := CreateFile(Pchar(Param), GENERIC_READ, FILE_SHARE_READ, NIL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
          // Creates the memory map for the file
                    myMMF := CreateFileMapping(myFile, NIL, PAGE_READONLY, 0, 0, NIL);
                    myLocalMMF := myMMF;  // Assignment for working locally
                    myFileCmd := S;
                    myFileSize := GetFileSize(myFile, NIL);
                    myViewLen := myFileSize;
                end
                else
                begin
          // Creating mapping for Q
          // Opens the file for this instance
                    myQFile := CreateFile(Pchar(Param), GENERIC_READ, FILE_SHARE_READ, NIL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
          // Creates the memory map for the file
                    myQMMF := CreateFileMapping(myQFile, NIL, PAGE_READONLY, 0, 0, NIL);
                    myLocalMMF := myQMMF; // Assignment for working locally
                    myFileCmdQ := S;
                    myFileSizeQ := GetFileSize(myFile, NIL);
                    myViewLenQ := myFileSizeQ;
                end;

                LoadMMFView(ParmName, myLocalMMF, Destination);
                Result := 0;
            end
            else
            begin
                DoSimpleMsg(Format('The file "%s" does not exist. Process cancelled.', [Param]), 800002);
                Result := -1;
            end;
        except
            DoSimpleMsg(Format('There was a proble mapping file "%s". Process cancelled.', [Param]), 800001);
            Result := -1;
        end;

    end;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLoadShape.Edit(ActorID: Integer): Integer;
var
    MMFError,
    ParamPointer: Integer;
    ParamName,
    Param: String;

begin
    Result := 0;
  // continue parsing with contents of Parser
    ActiveLoadShapeObj := ElementList.Active;
    ActiveDSSObject[ActorID] := ActiveLoadShapeObj;

    with ActiveLoadShapeObj do
    begin

        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;
        Param := Parser[ActorID].StrValue;
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
                    NumPoints := Parser[ActorID].Intvalue;
                2:
                    Interval := Parser[ActorID].DblValue;
                3:
                begin
                    if UseMMF then      // A different procedure if the user is working with MMF
                    begin
                        MMFError := CreateMMF(Param, 0); // Creates MMF for P
                        if MMFError = 0 then
                        begin
                            LoadFileFeatures(0);
                            myDataSize := NumPoints;
                            ReAllocmem(PMultipliers, sizeof(PMultipliers^[1]) * 2);
                        end;
                    end
                    else
                    begin               // Otherwise, follow the traditional technique for loading up load shapes
                        ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1]) * NumPoints);
                   // Allow possible Resetting (to a lower value) of num points when specifying multipliers not Hours
                        NumPoints := InterpretDblArray(Param, NumPoints, PMultipliers);   // Parser.ParseAsVector(Npts, Multipliers);
                    end;
                end;
                4:
                begin
                    ReAllocmem(Hours, Sizeof(Hours^[1]) * NumPoints);
                    NumPoints := InterpretDblArray(Param, NumPoints, Hours);   // Parser.ParseAsVector(Npts, Hours);
                    Interval := 0.0;
                end;
                5:
                    Mean := Parser[ActorID].DblValue;
                6:
                    StdDev := Parser[ActorID].DblValue;
                7:
                    DoCSVFile(Param);
                8:
                    DoSngFile(Param);
                9:
                    DoDblFile(Param);
                10:
                    case lowercase(Param)[1] of
                        'n':
                            Normalize;
                        'd':
                            SaveToDblFile;
                        's':
                            SaveToSngFile;
                    end;
                11:
                begin
                    if UseMMF then      // A different procedure if the user is working with MMF
                    begin
                        MMFError := CreateMMF(Param, 1);  // Creates MMF for Q
                        if MMFError = 0 then
                        begin
                            LoadFileFeatures(1);
                            if Assigned(PMultipliers) then
                                myDataSizeQ := myDataSize
                            else
                                myDataSizeQ := NumPoints;
                            ReAllocmem(QMultipliers, sizeof(QMultipliers^[1]) * 2);
                        end;
                    end
                    else
                    begin               // Otherwise, follow the traditional technique for loading up load shapes
                        ReAllocmem(QMultipliers, Sizeof(QMultipliers^[1]) * NumPoints);
                        NumPoints := InterpretDblArray(Param, NumPoints, QMultipliers);   // Parser.ParseAsVector(Npts, Multipliers);
                    end;
                end;
                12:
                    UseActual := InterpretYesNo(Param);
                13:
                    MaxP := Parser[ActorID].DblValue;
                14:
                    MaxQ := Parser[ActorID].DblValue;
                15:
                    Interval := Parser[ActorID].DblValue / 3600.0;  // Convert seconds to hr
                16:
                    Interval := Parser[ActorID].DblValue / 60.0;  // Convert minutes to hr
                17:
                    BaseP := Parser[ActorID].DblValue;
                18:
                    BaseQ := Parser[ActorID].DblValue;
                19:
                begin   // same as mult
                    if UseMMF then      // A different procedure if the user is working with MMF
                    begin
                        MMFError := CreateMMF(Param, 0); // Creates MMF for P
                        if MMFError = 0 then
                        begin
                            LoadFileFeatures(0);
                            myDataSize := NumPoints;
                            ReAllocmem(PMultipliers, sizeof(PMultipliers^[1]) * 2);
                        end;
                    end
                    else
                    begin               // Otherwise, follow the traditional technique for loading up load shapes
                        ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1]) * NumPoints);
                   // Allow possible Resetting (to a lower value) of num points when specifying multipliers not Hours
                        NumPoints := InterpretDblArray(Param, NumPoints, PMultipliers);   // Parser.ParseAsVector(Npts, Multipliers);
                    end;
                end;
                20:
                    Do2ColCSVFile(Param);
                21:
                    UseMMF := InterpretYesNo(Param);

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

            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end; {WHILE}

        if Assigned(PMultipliers) then
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
            NumPoints := OtherLoadShape.NumPoints;
            Interval := OtherLoadShape.Interval;
            ReallocMem(PMultipliers, SizeOf(PMultipliers^[1]) * NumPoints);
            for i := 1 to NumPoints do
                PMultipliers^[i] := OtherLoadShape.PMultipliers^[i];
            if Assigned(OtherLoadShape.Qmultipliers) then
            begin
                ReallocMem(QMultipliers, SizeOf(QMultipliers^[1]) * NumPoints);
                for i := 1 to NumPoints do
                    QMultipliers^[i] := OtherLoadShape.QMultipliers^[i];
            end;
            if Interval > 0.0 then
                ReallocMem(Hours, 0)
            else
            begin
                ReallocMem(Hours, SizeOf(Hours^[1]) * NumPoints);
                for i := 1 to NumPoints do
                    Hours^[i] := OtherLoadShape.Hours^[i];
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
function TLoadShape.Init(Handle: Integer; ActorID: Integer): Integer;

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
{
   Process 2-column CSV file (3-col if time expected)
}
var
    F: Textfile;
    MMFError,
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

    with ActiveLoadShapeObj do
    begin
        if UseMMF then      // A different procedure if the user is working with MMF
        begin
            CloseFile(F);
            myDataSize := NumPoints;
            myFileCmd := 'file=' + FileName + ' column=1';      // Command for P
            MMFError := CreateMMF(myFileCmd, 0);               // Creates MMF for the whole file
            myViewQ := myView;
            if MMFError = 0 then
            begin
                LoadFileFeatures(0);                                             // Features for P
                myFileCmd := 'file=' + FileName + ' column=2';      // Command for Q
                LoadFileFeatures(1);                                             // Features for Q
                myDataSize := NumPoints;
                myLineLenQ := myLineLen;
                ReAllocmem(PMultipliers, sizeof(PMultipliers^[1]) * 2);
                ReAllocmem(QMultipliers, sizeof(QMultipliers^[1]) * 2);
            end;
        end

        else
        begin
            try
        // Allocate both P and Q multipliers
                ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1]) * NumPoints);
                ReAllocmem(QMultipliers, Sizeof(QMultipliers^[1]) * NumPoints);
                if Interval = 0.0 then
                    ReAllocmem(Hours, Sizeof(Hours^[1]) * NumPoints);
                i := 0;
                while (not EOF(F)) and (i < FNumPoints) do
                begin
                    Inc(i);
                    Readln(F, s); // read entire line  and parse with AuxParser
          {AuxParser allows commas or white space}
                    with AuxParser[ActiveActor] do
                    begin
                        CmdString := s;
                        if Interval = 0.0 then
                        begin
                            NextParam;
                            Hours^[i] := DblValue;
                        end;
                        NextParam;
                        PMultipliers^[i] := DblValue;  // first parm
                        NextParam;
                        QMultipliers^[i] := DblValue;  // second parm
                    end;
                end;
                CloseFile(F);
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

    end;
end;

procedure TLoadShape.DoCSVFile(const FileName: String);

var

    MMFError,
    i: Integer;
    s: String;
    F: Textfile;

begin

    try
        AssignFile(F, FileName);
        Reset(F);
    except
        DoSimpleMsg('Error Opening File: "' + FileName, 613);
        CloseFile(F);
        Exit;
    end;
    with ActiveLoadShapeObj do
    begin
        if UseMMF then      // A different procedure if the user is working with MMF
        begin
            CloseFile(F);
            s := 'file=' + FileName;
            MMFError := CreateMMF(s, 0); // Creates MMF for P
            if MMFError = 0 then
            begin
                LoadFileFeatures(0);
                myDataSize := NumPoints;
                ReAllocmem(PMultipliers, sizeof(PMultipliers^[1]) * 2);
            end;
        end
        else
        begin
            try
                ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1]) * NumPoints);
                if Interval = 0.0 then
                    ReAllocmem(Hours, Sizeof(Hours^[1]) * NumPoints);
                i := 0;
                while (not EOF(F)) and (i < FNumPoints) do
                begin
                    Inc(i);
                    Readln(F, s); // read entire line  and parse with AuxParser
          {AuxParser allows commas or white space}
                    with AuxParser[ActiveActor] do
                    begin
                        CmdString := s;
                        if Interval = 0.0 then
                        begin
                            NextParam;
                            Hours^[i] := DblValue;
                        end;
                        NextParam;
                        PMultipliers^[i] := DblValue;
                    end;
                end;
                CloseFile(F);
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
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShape.DoSngFile(const FileName: String);
var
    s: String;
    F: file of Single;
    Hr, M: Single;
    mMFError,
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

    with ActiveLoadShapeObj do
    begin
        if UseMMF then      // A different procedure if the user is working with MMF
        begin
            CloseFile(F);
            s := 'sngfile=' + FileName;
            MMFError := CreateMMF(s, 0); // Creates MMF for P
            if MMFError = 0 then
            begin
                LoadFileFeatures(0);
                myDataSize := NumPoints;
                ReAllocmem(PMultipliers, sizeof(PMultipliers^[1]) * 2);
            end;
        end
        else
        begin
            try
                ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1]) * NumPoints);
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
                    PMultipliers^[i] := M;
                end;
                CloseFile(F);
                if i <> FNumPoints then
                    NumPoints := i;
            except
                DoSimpleMsg('Error Processing LoadShape File: "' + FileName, 616);
                CloseFile(F);
                Exit;
            end;
        end;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShape.DoDblFile(const FileName: String);
var
    s: String;
    F: file of Double;
    MMFError,
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

    with ActiveLoadShapeObj do
    begin
        if UseMMF then      // A different procedure if the user is working with MMF
        begin
            CloseFile(F);
            s := 'dblfile=' + FileName;
            MMFError := CreateMMF(s, 0); // Creates MMF for P
            if MMFError = 0 then
            begin
                LoadFileFeatures(0);
                myDataSize := NumPoints;
                ReAllocmem(PMultipliers, sizeof(PMultipliers^[1]) * 2);
            end;
        end
        else
        begin
            try
                ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1]) * NumPoints);
                if Interval = 0.0 then
                    ReAllocmem(Hours, Sizeof(Hours^[1]) * NumPoints);
                i := 0;
                while (not EOF(F)) and (i < FNumPoints) do
                begin
                    Inc(i);
                    if Interval = 0.0 then
                        Read(F, Hours^[i]);
                    Read(F, PMultipliers^[i]);
                end;
                CloseFile(F);
                if i <> FNumPoints then
                    NumPoints := i;
            except
                DoSimpleMsg('Error Processing LoadShape File: "' + FileName, 618);
                CloseFile(F);
                Exit;
            end;
        end;
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

    LastValueAccessed := 1;

    FNumPoints := 0;
    Interval := 1.0;  // hr
    Hours := NIL;
    PMultipliers := NIL;
    QMultipliers := NIL;
    MaxP := 1.0;
    MaxQ := 0.0;
    BaseP := 0.0;
    BaseQ := 0.0;
    UseActual := FALSE;
    UseMMF := FALSE;  // No memory mapping by default
    MaxQSpecified := FALSE;
    FStdDevCalculated := FALSE;  // calculate on demand
    Enabled := TRUE;

    myViewLen := 1000;   // 1kB by default, it may change for not missing a row

    ArrayPropertyIndex := 0;

    InitPropertyValues(0);

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLoadShapeObj.Destroy;
begin

    if Assigned(Hours) then
        ReallocMem(Hours, 0);
    if Assigned(PMultipliers) then
        ReallocMem(PMultipliers, 0);
    if Assigned(QMultipliers) then
        ReallocMem(QMultipliers, 0);
    if UseMMF then
    begin
        UnmapViewOfFile(myView);
        CloseHandle(myMMF);
        CloseHandle(myFile);
        CloseHandle(myQMMF);
        CloseHandle(myQFile);
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
    FileType: String;
    MMFound: Boolean;
    LocalPage,
    UpLimit,
    LowLimit,
    Index,
    j,
    i: Integer;

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
            Result.re := PMultipliers^[1];
            if Assigned(QMultipliers) then
                Result.im := QMultipliers^[1]
            else
                Result.im := Set_Result_im(Result.re);
        end
        else
        begin
            if Interval > 0.0 then                                      // Using Interval
            begin
                Index := round(hr / Interval);
                if UseMMF then
                begin
                    if Index > myDataSize then
                        Index := Index mod myDataSize;  // Wrap around using remainder
                    if Index = 0 then
                        Index := myDataSize;
                    Result.re := InterpretDblArrayMMF(myView, myFileType, myColumn, Index, myLineLen);
                    if Assigned(QMultipliers) then
                        Result.im := InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, Index, myLineLenQ)
                    else
                        Result.im := Set_Result_im(Result.re);
                end
                else
                begin
                    if Index > FNumPoints then
                        Index := Index mod FNumPoints;  // Wrap around using remainder
                    if Index = 0 then
                        Index := FNumPoints;
                    Result.re := PMultipliers^[Index];
                    if Assigned(QMultipliers) then
                        Result.im := QMultipliers^[Index]
                    else
                        Result.im := Set_Result_im(Result.re);
                end;
            end
            else
            begin
      // For random interval
        { Start with previous value accessed under the assumption that most
          of the time, this function will be called sequentially}
        {Normalize Hr to max hour in curve to get wraparound}
                if Hr > Hours^[FNumPoints] then
                    Hr := Hr - Trunc(Hr / Hours^[FNumPoints]) * Hours^[FNumPoints];


                if Hours^[LastValueAccessed] > Hr then
                    LastValueAccessed := 1;  // Start over from beginning
                for i := LastValueAccessed + 1 to FNumPoints do
                begin
                    if Abs(Hours^[i] - Hr) < 0.00001 then  // If close to an actual point, just use it.
                    begin
                        if UseMMF then
                        begin
                            Result.re := InterpretDblArrayMMF(myView, myFileType, myColumn, Index, myLineLen);
                            if Assigned(QMultipliers) then
                                Result.im := InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, Index, myLineLenQ)
                            else
                                Result.im := Set_Result_im(Result.re);
                        end
                        else
                        begin
                            Result.re := PMultipliers^[i];
                            if Assigned(QMultipliers) then
                                Result.im := QMultipliers^[i]
                            else
                                Result.im := Set_Result_im(Result.re);
                        end;
                        LastValueAccessed := i;
                        Exit;
                    end
                    else
                    if Hours^[i] > Hr then      // Interpolate for multiplier
                    begin
                        LastValueAccessed := i - 1;
                        if UseMMF then
                        begin
                            Result.re := InterpretDblArrayMMF(myView, myFileType, myColumn, LastValueAccessed, myLineLen) +
                                (Hr - Hours^[LastValueAccessed]) / (Hours^[i] - Hours^[LastValueAccessed]) *
                                (InterpretDblArrayMMF(myView, myFileType, myColumn, i, myLineLen) -
                                InterpretDblArrayMMF(myView, myFileType, myColumn, LastValueAccessed, myLineLen));
                            if Assigned(QMultipliers) then
                                Result.im := InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, LastValueAccessed, myLineLenQ) +
                                    (Hr - Hours^[LastValueAccessed]) / (Hours^[i] - Hours^[LastValueAccessed]) *
                                    (InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, i, myLineLenQ) -
                                    InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, LastValueAccessed, myLineLenQ))
                            else
                                Result.im := Set_Result_im(Result.re);
                        end
                        else
                        begin
                            Result.re := PMultipliers^[LastValueAccessed] +
                                (Hr - Hours^[LastValueAccessed]) / (Hours^[i] - Hours^[LastValueAccessed]) *
                                (PMultipliers^[i] - PMultipliers^[LastValueAccessed]);
                            if Assigned(QMultipliers) then
                                Result.im := QMultipliers^[LastValueAccessed] +
                                    (Hr - Hours^[LastValueAccessed]) / (Hours^[i] - Hours^[LastValueAccessed]) *
                                    (QMultipliers^[i] - QMultipliers^[LastValueAccessed])
                            else
                                Result.im := Set_Result_im(Result.re);
                        end;
                        Exit;
                    end;
                end;
        // If we fall through the loop, just use last value
                LastValueAccessed := FNumPoints - 1;
                Result.re := PMultipliers^[FNumPoints];
                if Assigned(QMultipliers) then
                    Result.im := QMultipliers^[FNumPoints]
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

begin
    MaxMult := BaseP;
    DoNormalize(PMultipliers);
    if Assigned(QMultipliers) then
    begin
        MaxMult := BaseQ;
        DoNormalize(QMultipliers);
    end;
    UseActual := FALSE;  // not likely that you would want to use the actual if you normalized it.
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShapeObj.CalcMeanandStdDev;

begin

    if FNumPoints > 0 then
        if Interval > 0.0 then
            RCDMeanandStdDev(PMultipliers, FNumPoints, FMean, FStdDev)
        else
            CurveMeanAndStdDev(PMultipliers, Hours, FNumPoints, FMean, FStdDev);

    PropertyValue[5] := Format('%.8g', [FMean]);
    PropertyValue[6] := Format('%.8g', [FStdDev]);

    FStdDevCalculated := TRUE;
   { No Action is taken on Q multipliers}
end;

(*
Function TLoadShapeObj.Get_FirstMult:Double;
Begin

  If Npts>0 Then Begin
     Result :=  Multipliers^[1];
     LastValueAccessed := 1;
  End
  Else
      Result := 0.0;

End;

Function TLoadShapeObj.Get_NextMult :Double;
Begin

  If Npts>0 Then Begin
     Inc(LastValueAccessed);
     If LastValueAccessed>Npts Then Begin
         Result := 0.0;
         Dec(LastValueAccessed);
     End
     Else Begin
          Result :=  Multipliers^[LastValueAccessed];
     End;
  End Else
      Result := 0.0;

End;
*)
function TLoadShapeObj.Get_Interval: Double;
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
            Result := InterpretDblArrayMMF(myView, myFileType, myColumn, i, myLineLen)
        else
            Result := PMultipliers^[i];
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
        2:
            Result := Format('%.8g', [Interval]);
        3:
        begin
            if UseMMF then
                Result := '(' + myFileCmd + ')'
            else
                Result := GetDSSArray_Real(FNumPoints, PMultipliers);
        end;
        4:
            if Hours <> NIL then
                Result := GetDSSArray_Real(FNumPoints, Hours);
        5:
            Result := Format('%.8g', [Mean]);
        6:
            Result := Format('%.8g', [StdDev]);
        11:
        begin
            if Assigned(QMultipliers) then
                if UseMMF then
                    Result := '(' + myFileCmdQ + ')'
                else
                    Result := GetDSSArray_Real(FNumPoints, QMultipliers);
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
        19:
        begin
            if UseMMF then
                Result := '(' + myFileCmd + ')'
            else
                Result := GetDSSArray_Real(FNumPoints, PMultipliers);
        end;
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

        TopTransferFile.WriteHeader(0.0, MaxTime, MinInterval, ObjList.ListSize, 0, 16, 'OpenDSS(TM), EPRI (R)');
        TopTransferFile.WriteNames(NameList, CNames);

        Hr_Time := 0.0;

        VBuf := AllocMem(Sizeof(VBuf^[1]) * ObjList.ListSize);
        CBuf := AllocMem(Sizeof(VBuf^[1]) * 1);   // just a dummy -- Cbuf is ignored here

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
end;

procedure TLoadShapeObj.SaveToDblFile;

var
    myDBL: Double;
    F: file of Double;
    i: Integer;
    Fname: String;

begin
    if Assigned(PMultipliers) then
    begin
        try
            FName := Format('%s_P.dbl', [Name]);
            AssignFile(F, Fname);
            Rewrite(F);
            if UseMMF then
            begin
                for i := 1 to NumPoints do
                begin
                    myDBL := InterpretDblArrayMMF(myView, myFileType, myColumn, i, myLineLen);
                    Write(F, myDBL);
                end;
            end
            else
                for i := 1 to NumPoints do
                    Write(F, PMultipliers^[i]);
            GlobalResult := 'mult=[dblfile=' + FName + ']';
        finally
            CloseFile(F);
        end;

        if Assigned(QMultipliers) then
        begin
            try
                FName := Format('%s_Q.dbl', [Name]);
                AssignFile(F, Fname);
                Rewrite(F);
                if UseMMF then
                begin
                    for i := 1 to NumPoints do
                    begin
                        myDBL := InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, i, myLineLenQ);
                        Write(F, myDBL);
                    end;
                end
                else
                    for i := 1 to NumPoints do
                        Write(F, QMultipliers^[i]);
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
    if Assigned(PMultipliers) then
    begin
        try
            FName := Format('%s_P.sng', [Name]);
            AssignFile(F, Fname);
            Rewrite(F);
            for i := 1 to NumPoints do
            begin
                if UseMMF then
                    Temp := InterpretDblArrayMMF(myView, myFileType, myColumn, i, myLineLen)
                else
                    Temp := PMultipliers^[i];
                Write(F, Temp);
            end;
            GlobalResult := 'mult=[sngfile=' + FName + ']';
        finally
            CloseFile(F);
        end;

        if Assigned(QMultipliers) then
        begin
            try
                FName := Format('%s_Q.sng', [Name]);
                AssignFile(F, Fname);
                Rewrite(F);
                for i := 1 to NumPoints do
                begin
                    if UseMMF then
                        Temp := InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, i, myLineLenQ)
                    else
                        Temp := QMultipliers^[i];
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
    if not UseMMF then
    begin
        iMaxP := iMaxAbsdblArrayValue(NumPoints, PMultipliers);
        if iMaxP > 0 then
        begin
            MaxP := PMultipliers^[iMaxP];
            if not MaxQSpecified then
                if Assigned(QMultipliers) then
                    MaxQ := QMultipliers^[iMaxP]
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

procedure TLoadShapeObj.Set_NumPoints(const Value: Integer);
begin

    PropertyValue[1] := IntToStr(Value);   // Update property list variable

        // Reset array property values to keep them in propoer order in Save

    if ArrayPropertyIndex > 0 then
        PropertyValue[ArrayPropertyIndex] := PropertyValue[ArrayPropertyIndex];
    if Assigned(Qmultipliers) then
        PropertyValue[11] := PropertyValue[11];

    FNumPoints := Value;   // Now assign the value

end;

procedure TLoadShapeObj.Set_StdDev(const Value: Double);
begin
    FStdDevCalculated := TRUE;
    FStdDev := Value;
end;

end.
