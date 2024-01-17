unit LoadShape;

// ----------------------------------------------------------
// Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
// Copyright (c) 2019-2022, Paulo Meira, DSS-Extensions contributors
// All rights reserved.
// ----------------------------------------------------------

interface

// The LoadShape object is a general DSS object used by all circuits
// as a reference for obtaining yearly, daily, and other load shapes.
//
// The values are set by the normal New and Edit procedures for any DSS object.
//
// The values are retrieved by setting the Code Property in the LoadShape Class.
// This sets the active LoadShape object to be the one referenced by the Code Property;
//
// Then the values of that code can be retrieved via the public variables.  Or you
// can pick up the ActiveLoadShapeObj object and save the direct reference to the object.
//
// Loadshapes default to fixed interval data.  If the Interval is specified to be 0.0,
// then both time and multiplier data are expected.  If the Interval is  greater than 0.0,
// the user specifies only the multipliers.  The Hour command is ignored and the files are
// assumed to contain only the multiplier data.
//
// The user may place the data in CSV or binary files as well as passing through the
// command interface. Obviously, for large amounts of data such as 8760 load curves, the
// command interface is cumbersome.  CSV files are text separated by commas, one interval to a line.
// There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.
//
// For fixed interval data, only the multiplier is expected.  Therefore, the CSV format would
// contain only one number per line.  The two binary formats are packed.
//
// For variable interval data, (hour, multiplier) pairs are expected in both formats.
//
// The Mean and Std Deviation are automatically computed upon demand when new series of points is entered.
//
// The data may also be entered in unnormalized form.  The normalize=Yes command will force normalization.  That
// is, the multipliers are scaled so that the maximum value is 1.0.

uses
    Classes,
    ParserDel,
    Command,
    DSSClass,
    DSSObject,
    UcMatrix,
    UComplex, DSSUcomplex,
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
    TLoadShapePropLegacy = (
        INVALID = 0,
        npts = 1,     // Number of points to expect
        interval = 2, // default = 1.0
        mult = 3,     // vector of power multiplier values
        hour = 4,     // vector of hour values
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
        PQCSVFile = 20, // Redirect to a file with p, q pairs
        MemoryMapping = 21, // Enable/disable using Memory mapping for this shape
        Interpolation = 22
    );
    TLoadShapeProp = (
        INVALID = 0,
        NPts = 1,     // Number of points to expect
        Interval = 2, // default = 1.0
        Mult = 3,     // vector of power multiplier values
        Hour = 4,     // vector of hour values
        Mean = 5,     // set the mean (otherwise computed)
        StdDev = 6,   // set the std dev (otherwise computed)
        CSVFile = 7,  // Switch input to a csvfile
        SngFile = 8,  // switch input to a binary file of singles
        DblFile = 9,  // switch input to a binary file of singles
        Action = 10,  // actions  Normalize
        QMult = 11,   // Q multiplier
        UseActual = 12, // Flag to signify to use actual value
        PMax = 13,    // MaxP value
        QMax = 14,    // MaxQ
        SInterval = 15, // Interval in seconds
        MInterval = 16, // Interval in minutes
        PBase = 17,   // for normalization, use peak if 0
        QBase = 18,   // for normalization, use peak if 0
        PMult = 19,   // synonym for Mult
        PQCSVFile = 20, // Redirect to a file with p, q pairs
        MemoryMapping = 21, // Enable/disable using Memory mapping for this shape
        Interpolation = 22
    );

    TMMShapeType = (
        P = 0,
        Q = 1
    );

{$PUSH}
{$Z4} // keep enums as int32 values
    TLoadShapeInterp = (
        Avg = 0,
        Edge = 1
    );
{$POP}
    TLSFileType = (
        PlainText = 0,
        Float64 = 1,
        Float32 = 2
    );
{$SCOPEDENUMS OFF}

    TLoadShape = class(TDSSClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;

        function Find(const ObjName: String; const ChangeActive: Boolean=True): Pointer; OVERRIDE;  // Find an obj of this class by name
    end;

    TLoadShapeObj = class(TDSSObject)
    PRIVATE
        LastValueAccessed: Integer;

        FStdDevCalculated: Boolean;
        FMean,
        FStdDev: Double;

        // Function Get_FirstMult:Double;
        // Function Get_NextMult :Double;
        function Get_Interval: Double;
        procedure SaveToDblFile();
        procedure SaveToSngFile();
        procedure CalcMeanandStdDev;
        function Get_Mean: Double;
        function Get_StdDev: Double;
        procedure Set_Mean(const Value: Double);
        procedure Set_StdDev(const Value: Double);  // Normalize the curve presently in memory
        function GetMultAtHourSingle(hr: Double): Complex;
        function HasData(): Boolean;
    PUBLIC
        NumPoints: Integer;  // Number of points in curve -- TODO: int64
        Interval: Double;  //=0.0 then random interval     (hr)

        // Double data
        dH: pDoubleArray0; // Time values (hr) if Interval > 0.0 else nil
        dP, dQ: pDoubleArray0;  // Multipliers, zero based
        // Single data
        sH, sP, sQ: pSingleArray0; //zero based

        MaxP, MaxQ, BaseP, BaseQ: Double;
        MaxQSpecified: Boolean;

        Enabled: Boolean;
        ExternalMemory: LongBool;
        UseActual: LongBool;
        interpolation: TLoadShapeInterp;
        Stride: Integer;

        // Memory mapping variables
        UseMMF: LongBool; // Flag to indicated that the user wants to use MMF
        {$IFDEF WINDOWS}
        mmMMF, mmQMMF: THandle; // Handle for the memory map (P, Q)
        mmFile, mmQFile: THandle; // Handle for the file to be mapped (P, Q)
        {$ELSE}
        mmFile, mmQFile: CInt; // Handle for the file to be mapped (P, Q)
        {$ENDIF}
        mmFileSize, mmFileSizeQ: Cardinal; // File size of the file opened (P, Q)
        mmFileCmd, mmFileCmdQ: String; // The file definition added by the user (for moving the data window)
        mmView, mmViewQ: pByte; // Current view of the file mapped (Bytes - P, Q)
        mmFileType, mmFileTypeQ: TLSFileType; // The file type (P, Q)
        mmColumn, mmColumnQ, // The column to read (P, Q)
        mmLineLen, mmLineLenQ, // The size of the char line (P, Q)
        mmDataSize, mmDataSizeQ, // The total data size expected (P, Q)
        mmViewLen, mmViewLenQ: Int64; // Memory View size in bytes (P)

        csvfile, dblfile, sngfile, pqcsvfile: String;

        constructor Create(ParClass: TDSSClass; const LoadShapeName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;
        procedure CustomSetRaw(Idx: Integer; Value: String); override;
        procedure SaveWrite(F: TStream); override;

        function GetMultAtHour(hr: Double): Complex;  // Get multiplier at specified time
        function Mult(i: Integer): Double;  // get multiplier by index -- used in SolutionAlgs, updates LastValueAccessed
        function PMult(i: Integer): Double;  // get multiplier by index -- used in SolutionAlgs, doesn't update LastValueAccessed 
        function QMult(i: Integer; var m: Double): Boolean;  // get multiplier by index
        function Hour(i: Integer): Double;  // get hour corresponding to point index
        procedure Normalize;
        procedure SetMaxPandQ;

        procedure LoadMMFView(const Parmname: String; Destination: TMMShapeType);
        procedure LoadFileFeatures(ShapeType: TMMShapeType);

        function GetPropertyValue(Index: Integer): String; OVERRIDE;

        property PresentInterval: Double READ Get_Interval;
        property Mean: Double READ Get_Mean WRITE Set_Mean;
        property StdDev: Double READ Get_StdDev WRITE Set_StdDev;

        procedure SetDataPointers(HoursPtr: PDouble; PMultPtr: PDouble; QMultPtr: PDouble; DStride: Integer);
        procedure SetDataPointersSingle(HoursPtr: PSingle; PMultPtr: PSingle; QMultPtr: PSingle; SStride: Integer);
        procedure UseFloat32;
        procedure UseFloat64;
        procedure ReadDblFile(const FileName: String);
        procedure ReadSngFile(const FileName: String);
        procedure ReadCSVFile(const FileName: String);
        procedure Read2ColCSVFile(const FileName: String);
        function CreateMMF(const S: String; Destination: TMMShapeType): Boolean;        
    end;

implementation

uses
    BufStream,
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    MathUtil,
    Math,
    DSSPointerList,
    DSSHelper,
    DSSObjectHelper,
    TypInfo,
    CAPI_Utils,
    CAPI_Types;

type
    ELoadShapeError = class(Exception);  // Raised to abort solution

type
    TObj = TLoadShapeObj;
    TProp = TLoadShapeProp;
    TPropLegacy = TLoadShapePropLegacy;

{$SCOPEDENUMS ON}
{$PUSH}
{$Z4} // keep enums as int32 values
    TLoadShapeAction = (
        Normalize = 0,
        DblSave = 1,
        SngSave = 2
    );
{$POP}
{$SCOPEDENUMS OFF}

const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;    
    ActionEnum, InterpEnum: TDSSEnum;

procedure Do2ColCSVFile(Obj: TObj; const FileName: String);forward;
procedure DoDblFile(Obj: TObj; const FileName: String);forward;
procedure DoSngFile(Obj: TObj; const FileName: String);forward;
procedure DoCSVFile(Obj: TObj; const FileName: String);forward;

constructor TLoadShape.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
        ActionEnum := TDSSEnum.Create('LoadShape: Action', True, 1, 1, 
            ['Normalize', 'DblSave', 'SngSave'], 
            [ord(TLoadShapeAction.Normalize), ord(TLoadShapeAction.DblSave), ord(TLoadShapeAction.SngSave)]);
        InterpEnum := TDSSEnum.Create('LoadShape: Interpolation', True, 1, 1, 
            ['Avg', 'Edge'], 
            [ord(TLoadShapeInterp.Avg), ord(TLoadShapeInterp.Edge)]);
    end;

    inherited Create(dssContext, DSS_OBJECT, 'LoadShape');
end;

destructor TLoadShape.Destroy;
begin
    inherited Destroy;
end;

function GetMean(obj: TObj): Double;
begin
    Result := obj.Mean;
end;

procedure SetMean(obj: TObj; value: Double);
begin
    obj.Mean := value;
end;

function GetStdDev(obj: TObj): Double;
begin
    Result := obj.StdDev;
end;

procedure SetStdDev(obj: TObj; value: Double);
begin
    obj.StdDev := value;
end;

procedure DoAction(obj: TObj; action: TLoadShapeAction);
begin
    case action of
        TLoadShapeAction.Normalize:
            obj.Normalize();
        TLoadShapeAction.DblSave:
            obj.SaveToDblFile();
        TLoadShapeAction.SngSave:
            obj.SaveToSngFile();
    end;
end;

procedure SetNumPoints(obj: TObj; Value: Integer);
begin
    if obj.ExternalMemory then
    begin
        obj.DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
        Exit;
    end
    else
        obj.NumPoints := Value;
end;

procedure getLSArray(
    npts: Integer; d: PDoubleArray0; s: PSingleArray0; var ResultPtr: PDouble; ResultCount: PAPISize
);
var
    i: Integer;
    inPtr: PSingle;
    outPtr: PDouble;
begin
    ResultCount[0] := 0;
    if npts = 0 then
        Exit;

    if d <> NIL then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, npts);
        Move(d[0], ResultPtr[0], npts * SizeOf(Double));
        Exit;
    end;
    if s <> NIL then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, npts);
        outPtr := PDouble(ResultPtr);
        inPtr := PSingle(s);
        for i := 0 to npts - 1 do
        begin
            outPtr^ := inPtr^;
            inc(outPtr);
            inc(inPtr);
        end;
        Exit;
    end;
end;

procedure getHour(obj: TObj; var ResultPtr: PDouble; ResultCount: PAPISize);
begin
    getLSArray(obj.NumPoints, obj.dH, obj.sH, ResultPtr, ResultCount);
end;

procedure getPMult(obj: TObj; var ResultPtr: PDouble; ResultCount: PAPISize);
begin
    getLSArray(obj.NumPoints, obj.dP, obj.sP, ResultPtr, ResultCount);
end;

procedure getQMult(obj: TObj; var ResultPtr: PDouble; ResultCount: PAPISize);
begin
    getLSArray(obj.NumPoints, obj.dQ, obj.sQ, ResultPtr, ResultCount);
end;


procedure TLoadShape.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    PropertyStructArrayCountOffset := ptruint(@obj.NumPoints);

    SpecSetNames := ArrayOfString.Create(
        'PMult, QMult, Hour',
        'PMult, QMult, Interval',
        'PQCSVFile',
        'CSVFile',
        'SngFile',
        'DblFile'
    );
    SpecSets := TSpecSets.Create(
        TSpecSet.Create(ord(TProp.Pmult), ord(TProp.Qmult), ord(TProp.Hour)),
        TSpecSet.Create(ord(TProp.Pmult), ord(TProp.Qmult), ord(TProp.Interval)),
        TSpecSet.Create(ord(TProp.PQCSVFile)),
        TSpecSet.Create(ord(TProp.CSVFile)),
        TSpecSet.Create(ord(TProp.SngFile)),
        TSpecSet.Create(ord(TProp.DblFile))
    );
    

    // boolean properties
    PropertyType[ord(TProp.MemoryMapping)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.MemoryMapping)] := ptruint(@obj.UseMMF);
    PropertyFlags[ord(TProp.MemoryMapping)] := [TPropertyFlag.Ordering_First];

    PropertyType[ord(TProp.UseActual)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.UseActual)] := ptruint(@obj.UseActual);

    // advanced doubles
    PropertyOffset[ord(TProp.sinterval)] := ptruint(@obj.Interval);
    PropertyScale[ord(TProp.sinterval)] := 1 / 3600.0;
    PropertyFlags[ord(TProp.sinterval)] := [TPropertyFlag.Redundant, TPropertyFlag.NonNegative, TPropertyFlag.Units_s];
    PropertyRedundantWith[ord(TProp.sinterval)] := ord(TProp.interval);

    PropertyOffset[ord(TProp.minterval)] := ptruint(@obj.Interval);
    PropertyScale[ord(TProp.minterval)] := 1 / 60.0;
    PropertyFlags[ord(TProp.minterval)] := [TPropertyFlag.Redundant, TPropertyFlag.NonNegative, TPropertyFlag.Units_minute];
    PropertyRedundantWith[ord(TProp.minterval)] := ord(TProp.interval);

    // double properties
    PropertyOffset[ord(TProp.interval)] := ptruint(@obj.Interval);
    PropertyFlags[ord(TProp.interval)] := [TPropertyFlag.NonNegative, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_hour];

    PropertyOffset[ord(TProp.Pmax)] := ptruint(@obj.MaxP);
    PropertyFlags[ord(TProp.Pmax)] := [TPropertyFlag.Units_kW];

    PropertyOffset[ord(TProp.Qmax)] := ptruint(@obj.MaxQ);
    PropertyFlags[ord(TProp.Qmax)] := [TPropertyFlag.Units_kvar];

    PropertyOffset[ord(TProp.Pbase)] := ptruint(@obj.BaseP);
    PropertyFlags[ord(TProp.Pbase)] := [TPropertyFlag.Units_kW];

    PropertyOffset[ord(TProp.Qbase)] := ptruint(@obj.BaseQ);
    PropertyFlags[ord(TProp.Qbase)] := [TPropertyFlag.Units_kvar];


    PropertyOffset[ord(TProp.mean)] := ptruint(@obj.FMean);
    PropertyReadFunction[ord(TProp.mean)] := @GetMean;
    PropertyWriteFunction[ord(TProp.mean)] := @SetMean;
    PropertyFlags[ord(TProp.mean)] := [TPropertyFlag.ReadByFunction, TPropertyFlag.WriteByFunction, TPropertyFlag.DynamicDefault];
    
    PropertyOffset[ord(TProp.stddev)] := ptruint(@obj.FStdDev);
    PropertyReadFunction[ord(TProp.stddev)] := @GetStdDev;
    PropertyWriteFunction[ord(TProp.stddev)] := @SetStdDev;
    PropertyFlags[ord(TProp.stddev)] := [TPropertyFlag.ReadByFunction, TPropertyFlag.WriteByFunction, TPropertyFlag.DynamicDefault];

    // double arrays, special
    PropertyType[ord(TProp.hour)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.hour)] := ptruint(@obj.dH);
    PropertyOffset2[ord(TProp.hour)] := ptruint(@obj.NumPoints);
    PropertyOffset3[ord(TProp.hour)] := ptruint(@obj.ExternalMemory);
    PropertyFlags[ord(TProp.hour)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.ReadByFunction, TPropertyFlag.CustomGet, TPropertyFlag.ConditionalReadOnly, TPropertyFlag.RequiredInSpecSet];
    PropertyReadFunction[ord(TProp.hour)] := @getHour;

    PropertyType[ord(TProp.mult)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.mult)] := ptruint(@obj.dP);
    PropertyOffset2[ord(TProp.mult)] := ptruint(@obj.NumPoints);
    PropertyOffset3[ord(TProp.mult)] := ptruint(@obj.ExternalMemory);
    PropertyFlags[ord(TProp.mult)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.ReadByFunction, TPropertyFlag.CustomGet, TPropertyFlag.ConditionalReadOnly, TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.mult)] := ord(TProp.pmult);
    PropertyReadFunction[ord(TProp.mult)] := @getPMult;

    PropertyType[ord(TProp.Pmult)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.Pmult)] := ptruint(@obj.dP);
    PropertyOffset2[ord(TProp.Pmult)] := ptruint(@obj.NumPoints);
    PropertyOffset3[ord(TProp.Pmult)] := ptruint(@obj.ExternalMemory);
    PropertyFlags[ord(TProp.Pmult)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.ReadByFunction, TPropertyFlag.CustomGet, TPropertyFlag.ConditionalReadOnly, TPropertyFlag.RequiredInSpecSet];
    PropertyReadFunction[ord(TProp.Pmult)] := @getPMult;

    PropertyType[ord(TProp.Qmult)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.Qmult)] := ptruint(@obj.dQ);
    PropertyOffset2[ord(TProp.Qmult)] := ptruint(@obj.NumPoints);
    PropertyOffset3[ord(TProp.Qmult)] := ptruint(@obj.ExternalMemory);
    PropertyFlags[ord(TProp.Qmult)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.ReadByFunction, TPropertyFlag.CustomGet, TPropertyFlag.ConditionalReadOnly];
    PropertyReadFunction[ord(TProp.Qmult)] := @getQMult;

    // integer
    Propertytype[ord(TProp.npts)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.npts)] := ptruint(@obj.NumPoints);
    PropertyWriteFunction[ord(TProp.npts)] := @SetNumPoints;
    PropertyFlags[ord(TProp.npts)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.SuppressJSON];

    // enums
    // enum action
    PropertyType[ord(TProp.Action)] := TPropertyType.StringEnumActionProperty;
    PropertyOffset[ord(TProp.Action)] := ptruint(@DoAction); 
    PropertyOffset2[ord(TProp.Action)] := PtrInt(ActionEnum); 

    // plain enum
    PropertyType[ord(TProp.Interpolation)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Interpolation)] := ptruint(@obj.interpolation); 
    PropertyOffset2[ord(TProp.Interpolation)] := PtrInt(InterpEnum); 

    // strings
    PropertyType[ord(TProp.csvfile)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.csvfile)] := ptruint(@obj.csvfile);
    PropertyFlags[ord(TProp.csvfile)] := [TPropertyFlag.IsFilename, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.GlobalCount];

    PropertyType[ord(TProp.dblfile)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.dblfile)] := ptruint(@obj.dblfile);
    PropertyFlags[ord(TProp.dblfile)] := [TPropertyFlag.IsFilename, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.GlobalCount];

    PropertyType[ord(TProp.sngfile)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.sngfile)] := ptruint(@obj.sngfile);
    PropertyFlags[ord(TProp.sngfile)] := [TPropertyFlag.IsFilename, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.GlobalCount];

    PropertyType[ord(TProp.PQCSVFile)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.PQCSVFile)] := ptruint(@obj.pqcsvfile);
    PropertyFlags[ord(TProp.PQCSVFile)] := [TPropertyFlag.IsFilename, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.GlobalCount];

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TLoadShape.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

// Loads the mapped file features into local variables for further use
procedure TLoadShapeObj.LoadFileFeatures(ShapeType: TMMShapeType);
var
    LocalCol: Integer;
    fileType: TLSFileType = TLSFileType.PlainText;
    ParmName,
    Param: String;
begin
    DSS.AuxParser.CmdString := mmFileCmd;
    ParmName := DSS.AuxParser.NextParam();
    LocalCol := 1;

    if CompareText(Parmname, 'file') = 0 then
    begin
        fileType := TLSFileType.PlainText;

        // Look for other options  (may be in either order)
        ParmName := DSS.AuxParser.NextParam();
        Param := DSS.AuxParser.StrValue;
        while Length(Param) > 0 do
        begin
            if CompareTextShortest(ParmName, 'column') = 0 then
                LocalCol := DSS.AuxParser.IntValue;
            ParmName := DSS.AuxParser.NextParam();
            Param := DSS.AuxParser.StrValue;
        end;
    end
    else if CompareText(Parmname, 'dblfile') = 0 then
        fileType := TLSFileType.Float64
    else if CompareText(Parmname, 'sngfile') = 0 then
        fileType := TLSFileType.Float32;

    // TODO: fileType could be uninitialized!

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
procedure TLoadShapeObj.LoadMMFView(const Parmname: String; Destination: TMMShapeType);
var
    FirstPos: Integer;
    lastCh: Byte;
begin
    // processes the view depending on the file type
    FirstPos := 1;
    if Destination = TMMShapeType.P then
    begin
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
function TLoadShapeObj.CreateMMF(const S: String; Destination: TMMShapeType): Boolean;
var
    ParmName,
    Param: String;
begin
    try
        DSS.AuxParser.CmdString := S;
        ParmName := DSS.AuxParser.NextParam();
        Param := AdjustInputFilePath(DSS.AuxParser.StrValue);
        if not FileExists(Param) then
        begin
            DoSimpleMsg('The file "%s" does not exist. Process cancelled.', [Param], 800002);
            Result := False;
        end;
        
        if Destination = TMMShapeType.P then
        begin
            // Creating mapping for P
{$IFDEF WINDOWS}
            mmFile := CreateFile(Pchar(Param), GENERIC_READ, FILE_SHARE_READ, NIL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
            mmMMF := CreateFileMapping(mmFile, NIL, PAGE_READONLY, 0, 0, NIL);
            mmFileSize := GetFileSize(mmFile, NIL);
            mmViewLen := mmFileSize;
            mmView := PByte(MapViewOfFile(mmMMF, FILE_MAP_READ, 0, 0, mmViewLen));
{$ELSE}
            mmFile := fpOpen(Pchar(Param), O_rdOnly, 0);
            mmFileSize := fpLSeek(mmFile, 0, Seek_End);
            fpLSeek(mmFile, 0, Seek_Set);
            mmViewLen := mmFileSize;
            mmView := PByte(fpMMap(nil, mmFileSize, PROT_READ, MAP_SHARED, mmFile, 0));
{$ENDIF}
            mmFileCmd := S;
        end
        else
        begin
            // Creating mapping for Q
{$IFDEF WINDOWS}
            mmQFile := CreateFile(Pchar(Param), GENERIC_READ, FILE_SHARE_READ, NIL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
            mmQMMF := CreateFileMapping(mmQFile, NIL, PAGE_READONLY, 0, 0, NIL);
            mmFileSizeQ := GetFileSize(mmQFile, NIL);
            mmViewLenQ := mmFileSizeQ;
            mmViewQ := PByte(MapViewOfFile(mmQMMF, FILE_MAP_READ, 0, 0, mmViewLenQ));
{$ELSE}
            mmQFile := fpOpen(Pchar(Param), O_rdOnly, 0);
            mmFileSizeQ := fpLSeek(mmQFile, 0, Seek_End);
            fpLSeek(mmQFile, 0, Seek_Set);
            mmViewLenQ := mmFileSizeQ;
            mmViewQ := PByte(fpMMap(nil, mmFileSizeQ, PROT_READ, MAP_SHARED, mmQFile, 0));
{$ENDIF}
            mmFileCmdQ := S;
        end;

        LoadMMFView(ParmName, Destination);
        Result := True;
    except
        DoSimpleMsg('There was a problem mapping file "%s". Process cancelled.', [Param], 800001);
        Result := False;
    end;
end;

procedure TLoadShapeObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
begin
    case Idx of 
        ord(TProp.csvfile):
            DoCSVFile(self, csvfile);
        ord(TProp.sngfile):
            DoSngFile(self, sngfile);
        ord(TProp.dblfile):
            DoDblFile(self, dblfile);
        ord(TProp.PQCSVFile):
            Do2ColCSVFile(self, pqcsvfile);
        ord(TProp.Interval):
            if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.NoPropertyTracking)) = 0 then
            begin
                PrpSequence[ord(TProp.Hour)] := 0;
            end;
        ord(TProp.Hour):
            if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.NoPropertyTracking)) = 0 then
            begin
                PrpSequence[ord(TProp.Interval)] := 0;
            end;
    end;
    case Idx of
        ord(TProp.npts):
            // Force as the always first property when saving in a later point
            // Doing this here we don't need to force the order everywhere later,
            // especially since in DSS C-API's implementation we don't keep a string
            // copy of every field.
            PrpSequence[Idx] := -10;
        ord(TProp.mult), ord(TProp.Pmult), ord(TProp.csvfile), ord(TProp.sngfile), ord(TProp.dblfile), ord(TProp.qmult):
            FStdDevCalculated := FALSE;   // now calculated on demand
        ord(TProp.Qmax):
            MaxQSpecified := TRUE;
        ord(TProp.MemoryMapping):
            if UseMMF then
                UseFloat64;
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

procedure TLoadShapeObj.CustomSetRaw(Idx: Integer; Value: String);
begin
    case Idx of
        ord(TProp.mult), ord(TProp.Pmult):
        begin
            if ExternalMemory then
            begin
                DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
                Exit;
            end;
            if UseMMF then
            begin
                if not CreateMMF(Value, TMMShapeType.P) then
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
            NumPoints := InterpretDblArray(DSS, Value, NumPoints, PDoubleArray(dP)); // TODO: different from the rest and conditional
        end;
        ord(TProp.hour):
        begin
            if ExternalMemory then
            begin
                DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
                Exit;
            end;
            UseFloat64;
            ReAllocmem(dH, Sizeof(Double) * NumPoints);
            InterpretDblArray(DSS, Value, NumPoints, PDoubleArray(dH)); // TODO: different from the rest and conditional
            Interval := 0.0;
        end;
        ord(TProp.qmult):
        begin
            if ExternalMemory then
            begin
                DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61105);
                Exit;
            end;
            if UseMMF then
            begin
                if not CreateMMF(Value, TMMShapeType.Q) then
                    Exit; // CreateMMF throws an error message already
                LoadFileFeatures(TMMShapeType.Q);
                if Assigned(dP) then
                    mmDataSizeQ := mmDataSize
                else
                    mmDataSizeQ := NumPoints;
                ReAllocmem(dQ, sizeof(Double) * 2);
                Exit;
            end;
            // Otherwise, follow the traditional technique for loading up load shapes                    
            UseFloat64;
            ReAllocmem(dQ, Sizeof(Double) * NumPoints);
            InterpretDblArray(DSS, Value, NumPoints, PDoubleArray(dQ));   // Parser.ParseAsVector(Npts, Multipliers);
        end;
    else
        inherited CustomSetRaw(Idx, Value);
    end;
end;

function TLoadShape.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
var
    obj: TObj;
begin
    obj := TObj(ptr);
    if (obj.dP <> NIL) or (obj.sP <> NIL) then
        obj.SetMaxPandQ();

    Exclude(obj.Flags, Flg.EditingActive);
    Result := True;
end;

function TLoadShape.Find(const ObjName: String; const ChangeActive: Boolean): Pointer;
begin
    if (Length(ObjName) = 0) or (CompareText(ObjName, 'none') = 0) then
        Result := NIL
    else
        Result := inherited Find(ObjName, ChangeActive);
end;

procedure TLoadShapeObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
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

    NumPoints := Other.NumPoints;
    Interval := Other.Interval;
    Stride := 1;

    // Double versions
    if Assigned(Other.dP) then
    begin
        ReallocMem(dP, SizeOf(Double) * NumPoints);
        //Move(Other.dP[0], dP[0], SizeOf(Double) * NumPoints);
        for i := 0 to NumPoints - 1do
            dP[i] := Other.dP[Stride * i];
    end
    else
        ReallocMem(dP, 0);

    if Assigned(Other.dQ) then
    begin
        ReallocMem(dQ, SizeOf(Double) * NumPoints);
        //Move(Other.dQ[0], dQ[0], SizeOf(Double) * NumPoints);
        for i := 0 to NumPoints - 1 do
            dQ[i] := Other.dQ[Stride * i];
        
    end;

    if Interval > 0.0 then
        ReallocMem(dH, 0)
    else
    begin
        ReallocMem(dH, SizeOf(Double) * NumPoints);
        // Move(Other.dH[0], dH[0], SizeOf(Double) * NumPoints);
        for i := 0 to NumPoints - 1 do
            dH[i] := Other.dH[Stride * i];
    end;

    // Single versions
    if Assigned(Other.sP) then
    begin
        ReallocMem(sP, SizeOf(Single) * NumPoints);
        // Move(Other.sP[0], sP[0], SizeOf(Single) * NumPoints);
        for i := 0 to NumPoints - 1 do
            sP[i] := Other.sP[Stride * i];
    end
    else
        ReallocMem(sP, 0);

    if Assigned(Other.sQ) then
    begin
        ReallocMem(sQ, SizeOf(Single) * NumPoints);
        // Move(Other.sQ[0], sQ[0], SizeOf(Single) * NumPoints);
        for i := 0 to NumPoints - 1 do
            sQ[i] := Other.sQ[Stride * i];
    end;

    if Interval > 0.0 then
        ReallocMem(sH, 0)
    else
    begin
        ReallocMem(sH, SizeOf(Single) * NumPoints);
        // Move(Other.sH[0], sH[0], SizeOf(Single) * NumPoints);
        for i := 0 to NumPoints - 1 do
            sH[i] := Other.sH[Stride * i];
    end;

    UseActual := Other.UseActual;
    UseMMF := Other.UseMMF;
    BaseP := Other.BaseP;
    BaseQ := Other.BaseQ;
    SetMaxPandQ;
end;

procedure Do2ColCSVFile(obj: TObj; const FileName: String);
begin
    obj.Read2ColCSVFile(FileName);
end;

procedure TLoadShapeObj.Read2ColCSVFile(const FileName: String);
//   Process 2-column CSV file (3-col if time expected)
var
    F: TStream = nil;
    i: Integer;
    s: String;
begin
    if ExternalMemory then
    begin
        DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
        Exit;
    end;

    try
        F := DSS.GetInputStreamEx(FileName);
    except
        DoSimpleMsg('Error opening file: "%s"', [FileName], 613);
        FreeAndNil(F);
        Exit;
    end;

    try
        if UseMMF then
        begin
            FreeAndNil(F);
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
        while ((F.Position + 1) < F.Size) and (i < (NumPoints - 1)) do
        begin
            Inc(i);
            FSReadln(F, s); // read entire line and parse with AuxParser
            // AuxParser allows commas or white space
            DSS.AuxParser.CmdString := s;
            if Interval = 0.0 then
            begin
                DSS.AuxParser.NextParam();
                dH[i] := DSS.AuxParser.DblValue;
            end;
            DSS.AuxParser.NextParam();
            dP[i] := DSS.AuxParser.DblValue;  // first parm
            DSS.AuxParser.NextParam();
            dQ[i] := DSS.AuxParser.DblValue;  // second parm
        end;
        FreeAndNil(F);
        inc(i);
        if i <> NumPoints then
            NumPoints := i;
    except
        On E: Exception do
        begin
            DoSimpleMsg(_('Error Processing CSV File: "%s". %s'), [FileName, E.Message], 614);
            FreeAndNil(F);
            Exit;
        end;
    end;
end;

procedure DoCSVFile(obj: TObj; const FileName: String);
begin
    obj.ReadCSVFile(FileName);
end;

procedure TLoadShapeObj.ReadCSVFile(const FileName: String);
var
    F: TStream = nil;
    i: Integer;
    s: String;
begin
    if ExternalMemory then
    begin
        DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
        Exit;
    end;
    try
        F := DSS.GetInputStreamEx(FileName);
    except
        DoSimpleMsg('Error opening file: "%s"', [FileName], 613);
        FreeAndNil(F);
        Exit;
    end;

    try
        if UseMMF then
        begin
            FreeAndNil(F);
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
        while ((F.Position + 1) < F.Size) and (i < (NumPoints - 1)) do
        begin
            Inc(i);
            FSReadln(F, s); // read entire line  and parse with AuxParser
            // AuxParser allows commas or white space
            DSS.AuxParser.CmdString := s;
            if Interval = 0.0 then
            begin
                DSS.AuxParser.NextParam();
                dH[i] := DSS.AuxParser.DblValue;
            end;
            DSS.AuxParser.NextParam();
            dP[i] := DSS.AuxParser.DblValue;
        end;
        FreeAndNil(F);
        inc(i);
        if i <> NumPoints then
            NumPoints := i;
    except
        On E: Exception do
        begin
            DoSimpleMsg(_('Error Processing CSV File: "%s". %s'), [FileName, E.Message], 614);
            FreeAndNil(F);
            Exit;
        end;
    end;
end;

procedure DoSngFile(obj: TObj; const FileName: String);
begin
    obj.ReadSngFile(FileName);
end;

procedure TLoadShapeObj.ReadSngFile(const FileName: String);
var
    s: String;
    F: TStream = NIL;
    Hr, M: Single;
    i: Integer;
    bytesRead: Int64;
begin
    if ExternalMemory then
    begin
        DoSimpleMsg(_('Data cannot be changed for LoadShapes with external memory! Reset the data first.'), 61102);
        Exit;
    end;
    try
        F := DSS.GetInputStreamEx(FileName);
    except
        DoSimpleMsg('Error opening file: "%s"', [FileName], 615);
        Exit;
    end;

    try
        if UseMMF then
        begin
            FreeAndNil(F);
            s := 'sngfile=' + FileName;
            if not CreateMMF(s, TMMShapeType.P) then
                Exit; // CreateMMF throws an error message already

            LoadFileFeatures(TMMShapeType.P);
            mmDataSize := NumPoints;
            ReAllocmem(dP, sizeof(Double) * 2);
            Exit;
        end;

        if (dQ = NIL) then
        begin
            // Take the opportunity to use float32 data
            UseFloat32;
            if sP = nil then
                ReallocMem(sP, NumPoints * SizeOf(Single));
            i := -1;
            
            if Interval = 0.0 then
            begin
                if sH = nil then
                    ReallocMem(sH, NumPoints * SizeOf(Single));

                while i < (NumPoints - 1) do
                begin
                    Inc(i);
                    if F.Read(sH[i], 4) <> 4 then break;
                    if F.Read(sP[i], 4) <> 4 then break;
                end;
            end
            else
            begin
                bytesRead := F.Read(sP[0], NumPoints * sizeof(Single));
                NumPoints := min(bytesRead div 4, NumPoints);
            end;
            FreeAndNil(F);
            Exit;
        end;

        UseFloat64;
        ReAllocmem(dP, sizeof(Double) * NumPoints);
        if Interval = 0.0 then
            ReAllocmem(dH, Sizeof(Double) * NumPoints);
        i := -1;
        
        if Interval = 0.0 then 
        begin
            while i < (NumPoints - 1) do
            begin
                Inc(i);
                if F.Read(Hr, sizeof(Single)) <> sizeof(Single) then break;
                if F.Read(M, sizeof(Single)) <> sizeof(Single) then break;
                dH[i] := Hr;
                dP[i] := M;
            end;
            inc(i);
            if i <> NumPoints then
                NumPoints := i;
        end
        else 
        begin
            ReallocMem(sP, NumPoints * SizeOf(Single));
            bytesRead := F.Read(sP[0], NumPoints * sizeof(Single));
            NumPoints := min(bytesRead div sizeof(Single), NumPoints);
            for i := 0 to NumPoints - 1 do
                dP[i] := sP[i];
            ReallocMem(sP, 0);
        end;
        FreeAndNil(F);
    except
        DoSimpleMsg('Error Processing LoadShape File: "%s"', [FileName], 616);
        if F <> nil then
            F.Free();
    end;
end;

procedure DoDblFile(obj: TObj; const FileName: String);
begin
    obj.ReadDblFile(FileName);
end;

procedure TLoadShapeObj.ReadDblFile(const FileName: String);
var
    s: String;
    F: TStream = NIL;
    i: Integer;
    bytesRead: Int64;
begin
    if ExternalMemory then
    begin
        DoSimpleMsg(_('Data cannot be changed for LoadShapes with external memory! Reset the data first.'), 61102);
        Exit;
    end;
    try
        F := DSS.GetInputStreamEx(FileName);
    except
        DoSimpleMsg('Error opening file: "%s"', [FileName], 617);
        Exit;
    end;

    try
        if UseMMF then
        begin
            FreeAndNil(F);
            s := 'dblfile=' + FileName;
            if not CreateMMF(s, TMMShapeType.P) then
                Exit; // CreateMMF throws an error message already
            
            LoadFileFeatures(TMMShapeType.P);
            mmDataSize := NumPoints;
            ReAllocmem(dP, sizeof(Double) * 2);
            Exit;
        end;

        UseFloat64();
        ReAllocmem(dP, sizeof(Double) * NumPoints);
        if Interval = 0.0 then
            ReAllocmem(dH, Sizeof(Double) * NumPoints);
        i := -1;
        
        if Interval = 0.0 then 
        begin
            while i < (NumPoints - 1) do
            begin
                Inc(i);
                if F.Read(dH[i], sizeof(Double)) <> sizeof(Double) then break;
                if F.Read(dP[i], sizeof(Double)) <> sizeof(Double) then break;
            end;
            inc(i);
            if i <> NumPoints then
                NumPoints := i;
        end
        else 
        begin
            bytesRead := F.Read(dP[0], NumPoints * sizeof(Double));
            NumPoints := min(bytesRead div sizeof(Double), NumPoints);
        end;
        FreeAndNil(F);
        if F <> nil then
            F.Free();
    except
        DoSimpleMsg('Error Processing LoadShape File: "%s"', [FileName], 618);
    end;
end;

constructor TLoadShapeObj.Create(ParClass: TDSSClass; const LoadShapeName: String);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(LoadShapeName);
    DSSObjType := ParClass.DSSClassType;

    ExternalMemory := False;
    Stride := 1;
    LastValueAccessed := 1;

    NumPoints := 0;
    Interval := 1.0;  // hr
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.NoPropertyTracking)) = 0 then
    begin
        SetAsNextSeq(ord(TProp.interval));
    end;
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

{$IFDEF WINDOWS}
    mmMMF := 0;
    mmQMMF := 0;
{$ENDIF}
    mmView := NIL;
    mmViewQ := NIL;
    mmFile := 0;
    mmQFile := 0;

    csvfile := '';
    dblfile := '';
    sngfile := '';
    pqcsvfile := '';

    mmViewLen := 1000;   // 1kB by default, it may change for not missing a row

    interpolation := TLoadShapeInterp.Avg;
end;

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
{$IFDEF WINDOWS}    
        UnmapViewOfFile(mmView);
        UnmapViewOfFile(mmViewQ);
        CloseHandle(mmMMF);
        CloseHandle(mmFile);
        CloseHandle(mmQMMF);
        CloseHandle(mmQFile);
{$ELSE}
        if (mmViewQ <> NIL) and (mmViewQ <> mmView) then
            fpMUnMap(mmViewQ, mmFileSize);
        if (mmView <> NIL) then
            fpMUnMap(mmView, mmFileSizeQ);

        if (mmQFile <> 0) and (mmQFile <> mmFile) then
            fpclose(mmQFile);
        if (mmFile <> 0) then
            fpclose(mmFile);
{$ENDIF}
    end;
    inherited destroy;
end;

function InterpretDblArrayMMF(DSS: TDSSContext; mmPtr: pByte; FileType: TLSFileType; Column, INDEX, DataSize: Integer): double;
//  Gets the value for the value at INDEX within the file mapped (mmPtr)
//  Considers the flags FileType, Column for locating the data within the file
//  FileType :
//    0 - normal file (ANSI char)
//    1 - dblfile
//    2 - sngfile
var
   DBLByteArray: array[0..7] of byte;
   SGLByteArray: array[0..3] of byte;
   // InputLIne, 
   content: String;
   byteValue : Byte;
   OffSet, i, j : Integer;
begin
    Result := 1.0; // Default Return Value;
    OffSet := INDEX * DataSize;

    if FileType = TLSFileType.PlainText then  // Normal file (CSV, txt, ASCII based file)
    begin
        content := '';
        byteValue := 0;
        i := OffSet;
        if mmPtr[i] = $0A then 
            inc(i); // in case we are at the end of the previous line
            
        j := 0;
        while byteValue <> $0A do
        begin
            byteValue := mmPtr[i];
            // Concatenates avoiding special chars (EOL)
            if (byteValue >= 46) and (byteValue < 58) then 
                content := content + AnsiChar(byteValue);

            if byteValue = 44 then // a comma char was found
            begin               // If we are at the column, exit, otherwise, keep looking
                inc(j);         // discarding the previous number (not needed anyway)
                if j = Column then 
                    break
                else 
                    content := '';
            end;
            inc(i);
        end;
        try
            // checks if the extraction was OK, othwerwise, forces the default value
            if content = '' then 
                content := '1.0';
            Result := strtofloat(content);
        except
            on E:Exception Do
            begin
                DoSimpleMsg(DSS, 'Error reading %d-th numeric array value. Error is:', [i, E.message], 785);
                Result := i - 1;
            end;
        end;
        Exit;
    end;

    if (FileType = TLSFileType.Float64) then // DBL files
    begin
        // load the list from a file of doubles (no checking done on type of data)
        for i := 0 to (DataSize - 1) do 
            DBLByteArray[i] :=  mmPtr[i + OffSet]; // Load data into the temporary buffer
        Result := double(DBLByteArray); // returns the number (double)
        Exit;
    end;

    if (FileType = TLSFileType.Float32) then // SGL files
    begin
        // load the list from a file of doubles (no checking done on type of data)
        for i := 0 to (DataSize - 1) do 
            SGLByteArray[i] := mmPtr[i + OffSet]; // Load data into the temporary buffer
        Result := single(SGLByteArray); // returns the number formatted as double
        Exit;
    end;
end;

function TLoadShapeObj.GetMultAtHour(hr: Double): Complex;
// This function returns a multiplier for the given hour.
// If no points exist in the curve, the result is  1.0
// If there are fewer points than requested, the curve is simply assumed to repeat
// Thus a daily load curve can suffice for a yearly load curve:  You just get the
// same day over and over again.
// The value returned is the nearest to the interval requested.  Thus if you request
// hour=12.25 and the interval is 1.0, you will get interval 12.
var
    i, k: Integer;
    koffset, 
    offset, // index including stride
    poffset: Int64; // previous index including stride
    
    function Set_Result_im(const realpart: Double): Double;
    // Set imaginary part of Result when Qmultipliers not defined
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

    if NumPoints <= 0 then
        Exit;
        
    if NumPoints = 1 then
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
        if interpolation = TLoadShapeInterp.Edge then
            i := floor(hr / Interval)
        else
            i := round(hr / Interval);

        if UseMMF then
        begin
            if i > mmDataSize then 
                i := i mod mmDataSize;  // Wrap around using remainder
            if i = 0 then 
                i := mmDataSize;
            Result.re := InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, i - 1, mmLineLen);
            if Assigned(dQ) then
                Result.im := InterpretDblArrayMMF(DSS, mmViewQ, mmFileTypeQ, mmColumnQ, i - 1, mmLineLenQ)
            else
                Result.im := Set_Result_im(Result.re);
            
            Exit;
        end;
        
        if i > NumPoints then 
            i := i mod NumPoints;  // Wrap around using remainder
        if i = 0 then 
            i := NumPoints;
        
        dec(i);
        offset := i * Stride;
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
    if Hr > dH[Stride * (NumPoints - 1)] then
    begin
        offset := Stride * (NumPoints - 1);
        Hr := Hr - Trunc(Hr / dH[offset]) * dH[offset];
    end;
    
    if dH[Stride * LastValueAccessed] > Hr then
        LastValueAccessed := 0;  // Start over from beginning
        
    for i := LastValueAccessed to NumPoints - 1 do
    begin
        offset := Stride * i;
        if Abs(dH[offset] - Hr) < 0.00001 then  // If close to an actual point, just use it.
        begin
            if UseMMF then
            begin
                Result.re := InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, i, mmLineLen);
                if Assigned(dQ) then
                    Result.im := InterpretDblArrayMMF(DSS, mmViewQ, mmFileTypeQ, mmColumnQ, i, mmLineLenQ)
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
        
        if dH[offset] > Hr then
        begin
            if Interpolation = TLoadShapeInterp.Edge then
            begin
                // Use the edge values
                Result := 0;
                //TODO: after we have more tests, rewrite this to walk back from i instead of this loop
                for k := 1 to NumPoints do
                begin
                    koffset := Stride * (k - 1);
                    if dH[koffset] <= Hr then
                    begin
                        Result.re := dP[koffset];
                        if dQ <> NIL then
                            Result.im := dQ[koffset];
                    end
                    else
                        Exit;
                end;
            end
            else
            begin
                // Interpolate for multiplier
                LastValueAccessed := i - 1;
                poffset := offset - Stride;
                if UseMMF then
                begin
                    Result.re := InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, LastValueAccessed, mmLineLen) +
                        (Hr - dH[LastValueAccessed]) / (dH[i] - dH[LastValueAccessed]) *
                        (InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, i - 1, mmLineLen) -
                        InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, LastValueAccessed - 1, mmLineLen));
                    if Assigned(dQ) then
                        Result.im := InterpretDblArrayMMF(DSS, mmViewQ, mmFileTypeQ, mmColumnQ, LastValueAccessed, mmLineLenQ) +
                            (Hr - dH[LastValueAccessed]) / (dH[i] - dH[LastValueAccessed]) *
                            (InterpretDblArrayMMF(DSS, mmViewQ, mmFileTypeQ, mmColumnQ, i - 1, mmLineLenQ) -
                            InterpretDblArrayMMF(DSS, mmViewQ, mmFileTypeQ, mmColumnQ, LastValueAccessed - 1, mmLineLenQ))
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
    end;

    // If we fall through the loop, just use last value
    LastValueAccessed := NumPoints - 2;
    Result.re := dP[Stride * LastValueAccessed];
    if Assigned(dQ) then
        Result.im := dQ[Stride * LastValueAccessed]
    else
        Result.im := Set_Result_im(Result.re);
end;

function TLoadShapeObj.HasData(): Boolean;
begin
    Result := True;
    if (sP <> NIL) or (dP <> NIL) then
        Exit;

    // "Silently" ignore if there's an error message pending already
    if (DSS.ErrorNumber = 0) then
        DoSimpleMsg(_('LoadShape has no data to be normalized. Check for previous errors.'), 61107);

    Result := False;
end;

procedure TLoadShapeObj.Normalize;
// normalize this load shape
var
    MaxMult: Double;

    procedure DoNormalize(Multipliers: pDoubleArray0);
    var
        i: Integer;
    begin
        if NumPoints > 0 then
        begin
            if MaxMult <= 0.0 then
            begin
                MaxMult := Abs(Multipliers[0]);
                for i := 1 to NumPoints - 1 do
                    MaxMult := Max(MaxMult, Abs(Multipliers[i]));
            end;
            if MaxMult = 0.0 then
                MaxMult := 1.0; // Avoid divide by zero
            for i := 0 to NumPoints - 1 do
                Multipliers[i] := Multipliers[i] / MaxMult;
        end;
    end;

    procedure DoNormalizeSingle(Multipliers: pSingleArray0);
    var
        i: Integer;
    begin
        if NumPoints > 0 then
        begin
            if MaxMult <= 0.0 then
			begin
                MaxMult := Abs(Multipliers[0]);
                for i := 1 to NumPoints - 1 do
                    MaxMult := Max(MaxMult, Abs(Multipliers[i]));
            end;
            if MaxMult = 0.0 then
                MaxMult := 1.0; // Avoid divide by zero
            for i := 0 to NumPoints - 1 do
                Multipliers[i] := Multipliers[i] / MaxMult;
        end;
    end;

begin
    if UseMMF or ExternalMemory then //TODO: disallow MMF?
    begin
        DoSimpleMsg(_('Data cannot be changed for LoadShapes with external memory or memory-mapped files! Reset the data first.'), 61102);
        Exit;
    end;
    if not HasData() then
        Exit;

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
    else if Assigned(sP) then
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

procedure TLoadShapeObj.CalcMeanandStdDev;
begin
    if UseMMF or ExternalMemory then
        Exit;

    if not HasData() then
        Exit;

    if NumPoints > 0 then
    begin
        if Assigned(dP) then
        begin
            if Interval > 0.0 then
                RCDMeanandStdDev(dP, NumPoints, FMean, FStdDev)
            else
                CurveMeanAndStdDev(PDoubleArray(dP), PDoubleArray(dH), NumPoints, FMean, FStdDev);
        end
        else
        Begin
            if Interval > 0.0 then
                RCDMeanandStdDevSingle(sP, NumPoints, FMean, FStdDev)
            else
                CurveMeanAndStdDevSingle(PSingleArray(sP), PSingleArray(sH), NumPoints, FMean, FStdDev);
        End;
    end;

    FStdDevCalculated := TRUE;
    // No Action is taken on Q multipliers
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
    dec(i);
    if (i < NumPoints) and (i >= 0) then
    begin
        if UseMMF then
            Result := InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, i, mmLineLen)
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
    dec(i);
    if (i < NumPoints) and (i >= 0) then
    begin
        if UseMMF then
            Result := InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, i, mmLineLen)
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
    dec(i);
    Result := False;
    if (dQ = nil) and (sQ = nil) then
        Exit;
    Result := True;
    
    if (i < NumPoints) and (i >= 0) then
    begin
        if UseMMF then
            m := InterpretDblArrayMMF(DSS, mmViewQ, mmFileTypeQ, mmColumnQ, i, mmLineLenQ)
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
    dec(i);
    if Interval = 0 then
    begin
        if (i < NumPoints) and (i >= 0) then
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

function TLoadShapeObj.GetPropertyValue(Index: Integer): String;
begin
    Result := '';

    case Index of
        ord(TProp.mult), ord(TProp.Pmult):
        begin
            if UseMMF then
            begin
                Result := '(' + mmFileCmd + ')';
                Exit;
            end;
            if dP <> NIL then
                Result := GetDSSArray(NumPoints, pDoubleArray(dP))
            else if sP <> NIL then
                Result := GetDSSArray(NumPoints, pSingleArray(sP));
        end;
        ord(TProp.hour):
        begin
            if dH <> NIL then
                Result := GetDSSArray(NumPoints, pDoubleArray(dH))
            else if sH <> NIL then
                Result := GetDSSArray(NumPoints, pSingleArray(sH));
        end;
        ord(TProp.qmult):
        begin
            if UseMMF then
            begin
                Result := '(' + mmFileCmdQ + ')';
                Exit;
            end;
            if Assigned(dQ) then
                Result := GetDSSArray(NumPoints, pDoubleArray(dQ))
            else if Assigned(sQ) then
                Result := GetDSSArray(NumPoints, pSingleArray(sQ));
        end;
    else
        Result := inherited GetPropertyValue(index);
    end;
end;

procedure TLoadShapeObj.SaveToDblFile;
var
    myDBL: Double;
    F: TStream = nil;
    i: Integer;
    Fname: String;
begin
    //TODO: disallow when ExternalMemory?
    UseFloat64;
    if not Assigned(dP) then
    begin
        DoSimpleMsg('%s P multipliers not defined.', [FullName], 622);
        Exit;
    end;

    try
        FName := DSS.OutputDirectory + Format('%s_P.dbl', [Name]); // CurrentDSSDir
        F := DSS.GetOutputStreamEx(FName, fmCreate);
        if UseMMF then
        begin
            for i := 0 to NumPoints - 1 do
            begin
                myDBL := InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, i, mmLineLen);
                F.Write(myDBL, sizeOf(myDBL));
            end;
        end
        else
        begin
            for i := 0 to NumPoints - 1 do
                F.Write(dP[Stride * i], sizeOf(Double)); 
        end;
        DSS.GlobalResult := 'mult=[dblfile=' + FName + ']';
    finally
        FreeAndNil(F);
    end;

    if Assigned(dQ) then
    begin
        try
            FName := DSS.OutputDirectory + Format('%s_Q.dbl', [Name]); // CurrentDSSDir
            F := DSS.GetOutputStreamEx(FName, fmCreate);
            if UseMMF then
            begin
                for i := 0 to NumPoints - 1 do
                begin
                    myDBL := InterpretDblArrayMMF(DSS, mmViewQ, mmFileTypeQ, mmColumnQ, i, mmLineLenQ);
                    F.Write(myDBL, sizeOf(myDBL));
                end;
            end
            else
                for i := 0 to NumPoints - 1 do
                    F.Write(dQ[Stride * i], sizeOf(Double)); 
            AppendGlobalResult(DSS, ' Qmult=[dblfile=' + FName + ']');
        finally
            FreeAndNil(F);
        end;
    end;
end;

procedure TLoadShapeObj.SaveToSngFile;
var
    F: TStream = nil;
    i: Integer;
    Fname: String;
    Temp: Single;
begin
    UseFloat64;
    if not Assigned(dP) then
    begin
        DoSimpleMsg('%s P multipliers not defined.', [FullName], 623);
        Exit;
    end;

    try
        FName := DSS.OutputDirectory + Format('%s_P.sng', [Name]); // CurrentDSSDir
        F := DSS.GetOutputStreamEx(FName, fmCreate);
        if UseMMF then
        begin
            for i := 0 to NumPoints - 1 do
            begin
                Temp := InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, i, mmLineLen);
                F.Write(Temp, SizeOf(Temp));
            end;
        end
        else
        begin
            for i := 0 to NumPoints - 1 do
            begin
                Temp := dP[Stride * i];
                F.Write(Temp, SizeOf(Temp));
            end;
        end;
        DSS.GlobalResult := 'mult=[sngfile=' + FName + ']';
    finally
        FreeAndNil(F);
    end;

    if Assigned(dQ) then
    begin
        try
            FName := DSS.OutputDirectory + Format('%s_Q.sng', [Name]); // CurrentDSSDir
            F := DSS.GetOutputStreamEx(FName, fmCreate);
            if UseMMF then
            begin
                for i := 0 to NumPoints - 1 do
                begin
                    Temp := InterpretDblArrayMMF(DSS, mmViewQ, mmFileTypeQ, mmColumnQ, i, mmLineLenQ);
                    F.Write(Temp, SizeOf(Temp));
                end;
            end
            else
            begin
                for i := 0 to NumPoints - 1 do
                begin
                    Temp := dQ[Stride * i];
                    F.Write(Temp, SizeOf(Temp));
                end;
            end;
            AppendGlobalResult(DSS, ' Qmult=[sngfile=' + FName + ']');
        finally
            FreeAndNil(F);
        end;
    end;
end;

function iMaxAbsArrayValue(npts: Integer; dbls: pDoubleArray): Integer; overload;
// Returns index of max array value  in abs value
var
    i: Integer;
    MaxValue: Double;
begin
    Result := 0;
    if npts = 0 then
        exit;

    Result := 1;
    MaxValue := abs(dbls[1]);
    for i := 2 to npts do
        if abs(dbls[i]) > Maxvalue then
        begin
            Maxvalue := abs(dbls[i]);
            Result := i;   // save index
        end;
end;

function iMaxAbsArrayValue(npts: Integer; sngs: pSingleArray): Integer; overload;
// Returns index of max array value  in abs value
var
    i: Integer;
    MaxValue: Single;
begin
    Result := 0;
    if npts = 0 then
        exit;

    Result := 1;
    MaxValue := abs(sngs[1]);
    for i := 2 to npts do
        if abs(sngs[i]) > Maxvalue then
        begin
            Maxvalue := abs(sngs[i]);
            Result := i;   // save index
        end;
end;
procedure TLoadShapeObj.SetMaxPandQ;
var
    iMaxP: Integer;
begin
    if UseMMF or ExternalMemory then
        Exit;

    if not HasData() then
        Exit;

    if Assigned(dP) then
    begin
        iMaxP := iMaxAbsArrayValue(NumPoints, PDoubleArray(dP)) - 1;
        if iMaxP >= 0 then
        begin
            MaxP := dP[iMaxP]; // Stride *
            if not MaxQSpecified then
                if Assigned(dQ) then
                    MaxQ := dQ[iMaxP] // Stride *
                else
                    MaxQ := 0.0;
        end;
    end
    else
    begin
        iMaxP := iMaxAbsArrayValue(NumPoints, PSingleArray(sP)) - 1;
        if iMaxP >= 0 then
        begin
            MaxP := sP[iMaxP]; // Stride *
            if not MaxQSpecified then
                if Assigned(sQ) then
                    MaxQ := sQ[iMaxP] // Stride *
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
        ReallocMem(dH, 0);
        ReallocMem(dP, 0);
        ReallocMem(dQ, 0);
        ReallocMem(sH, 0);
        ReallocMem(sP, 0);
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
        ReallocMem(dH, 0);
        ReallocMem(dP, 0);
        ReallocMem(dQ, 0);
        ReallocMem(sH, 0);
        ReallocMem(sP, 0);
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
    if UseMMF then
    begin
        DoSimpleMsg(_('Data cannot be toggled to 32-bit floats when memory-mapping is enabled.'), 61106);
        Exit;
    end;

    if ExternalMemory then
    begin
        DoSimpleMsg(_('Data cannot be changed for LoadShapes with external memory! Reset the data first.'), 61103);
        Exit;
    end;

    if Assigned(dH) then
    begin
        ReallocMem(sH, NumPoints * SizeOf(Single));
        for i := 0 to NumPoints - 1 do
            sH[i] := dH[i];
        FreeMem(dH);
        dH := nil;
    end;
    if Assigned(dP) then
    begin
        ReallocMem(sP, NumPoints * SizeOf(Single));
        for i := 0 to NumPoints - 1 do
            sP[i] := dP[i];
        FreeMem(dP);
        dP := nil;
    end;
    if Assigned(dQ) then
    begin
        ReallocMem(sQ, NumPoints * SizeOf(Single));
        for i := 0 to NumPoints - 1 do
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
        DoSimpleMsg(_('Data cannot be changed for LoadShapes with external memory or memory-mapped files! Reset the data first.'), 61104);
        Exit;
    end;

    if Assigned(sH) then
    begin
        if dH = NIL then
        begin
            ReallocMem(dH, NumPoints * SizeOf(Double));
            for i := 0 to NumPoints - 1 do
                dH[i] := sH[i];
        end;
        FreeMem(sH);
        sH := nil;
    end;
    if Assigned(sP) then
    begin
        if dP = NIL then
        begin
            ReallocMem(dP, NumPoints * SizeOf(Double));
            for i := 0 to NumPoints - 1 do
                dP[i] := sP[i];
        end;
        FreeMem(sP);
        sP := nil;
    end;
    if Assigned(sQ) then
    begin
        if dQ = NIL then
        begin
            ReallocMem(dQ, NumPoints * SizeOf(Double));
            for i := 0 to NumPoints - 1 do
                dQ[i] := sQ[i];
        end;
        FreeMem(sQ);
        sQ := nil;
    end;
end;

function TLoadShapeObj.GetMultAtHourSingle(hr: Double): Complex;
var
    i, k: Integer;
    koffset, 
    offset, // index including stride
    poffset: Int64; // previous index including stride
    
    function Set_Result_im(const realpart: Double): Double;
    // Set imaginary part of Result when Qmultipliers not defined
    begin
        if UseActual then
            Set_Result_im := 0.0       // if actual, assume zero
        else
            Set_Result_im := realpart; // same as real otherwise
    end;

begin
    Result.re := 1.0;
    Result.im := 1.0;    // default return value if no points in curve

    if NumPoints <= 0 then         // Handle Exceptional cases
        Exit;

    if NumPoints = 1 then
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
        if interpolation = TLoadShapeInterp.Edge then
            i := floor(hr / Interval)
        else
            i := round(hr / Interval);

        if i > NumPoints then 
            i := i mod NumPoints;  // Wrap around using remainder
        if i = 0 then 
            i := NumPoints;

        dec(i);
        offset := i * Stride;
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
    if Hr > sH[Stride * (NumPoints - 1)] then
    begin
        offset := Stride * (NumPoints - 1);
        Hr := Hr - Trunc(Hr / sH[offset]) * sH[offset];
    end;

    if sH[Stride * LastValueAccessed] > Hr then
        LastValueAccessed := 0;  // Start over from beginning
    
    for i := LastValueAccessed to NumPoints - 1 do
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
            if Interpolation = TLoadShapeInterp.Edge then
            begin
                // Use the edge values
                Result := 0;
                //TODO: after we have more tests, rewrite this to walk back from i instead of this loop
                for k := 1 to NumPoints do
                begin
                    koffset := Stride * (k - 1);
                    if sH[koffset] <= Hr then
                    begin
                        Result.re := sP[koffset];
                        if dQ <> NIL then
                            Result.im := sQ[koffset];
                    end
                    else
                        Exit;
                end;
            end
            else
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
    end;

    // If we fall through the loop, just use last value
    LastValueAccessed := NumPoints - 2;
    Result.re := sP[Stride * LastValueAccessed];
    if Assigned(sQ) then
        Result.im := sQ[Stride * LastValueAccessed]
    else
        Result.im := Set_Result_im(Result.re);
end;

procedure TLoadShapeObj.SaveWrite(F: TStream);
begin
    PrpSequence[ord(TProp.npts)] := -999; // make sure Npts prop is first
    inherited SaveWrite(F);
end;

finalization
    ActionEnum.Free;
    InterpEnum.Free;
end.
