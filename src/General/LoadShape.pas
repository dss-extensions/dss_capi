unit LoadShape;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  Copyright (c) 2019-2021, Paulo Meira
  All rights reserved.
  ----------------------------------------------------------
}

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
    TLoadShapeProp = (
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
        MemoryMapping = 21 // Enable/disable using Memory mapping for this shape
    );

    TMMShapeType = (
        P = 0,
        Q = 1
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
        LastValueAccessed,
        FNumPoints: Integer;  // Number of points in curve -- TODO: int64

        FStdDevCalculated: Boolean;
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
        MaxQSpecified: Boolean;

        Enabled, UseActual, ExternalMemory: Boolean;
        Stride: Integer;

        // Memory mapping variables
        UseMMF: Boolean; // Flag to indicated that the user wants to use MMF
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
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;
        procedure CustomSetRaw(Idx: Integer; Value: String); override;

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

        property NumPoints: Integer READ FNumPoints WRITE FNumPoints;
        property PresentInterval: Double READ Get_Interval;
        property Mean: Double READ Get_Mean WRITE Set_Mean;
        property StdDev: Double READ Get_StdDev WRITE Set_StdDev;

        procedure SetDataPointers(HoursPtr: PDouble; PMultPtr: PDouble; QMultPtr: PDouble; DStride: Integer);
        procedure SetDataPointersSingle(HoursPtr: PSingle; PMultPtr: PSingle; QMultPtr: PSingle; SStride: Integer);
        procedure UseFloat32;
        procedure UseFloat64;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    MathUtil,
    Math,
    DSSPointerList,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    ELoadShapeError = class(Exception);  // Raised to abort solution

type
    TObj = TLoadShapeObj;
    TProp = TLoadShapeProp;
{$PUSH}
{$Z4} // keep enums as int32 values
    TLoadShapeAction = (
        Normalize = 0,
        DblSave = 1,
        SngSave = 2
    );
{$POP}

const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer;    
    ActionEnum: TDSSEnum;

procedure Do2ColCSVFile(Obj: TObj; const FileName: String);forward;
procedure DoDblFile(Obj: TObj; const FileName: String);forward;
procedure DoSngFile(Obj: TObj; const FileName: String);forward;
procedure DoCSVFile(Obj: TObj; const FileName: String);forward;

constructor TLoadShape.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        ActionEnum := TDSSEnum.Create('LoadShape: Action', True, 1, 1, 
            ['Normalize', 'DblSave', 'SngSave'], 
            [ord(TLoadShapeAction.Normalize), ord(TLoadShapeAction.DblSave), ord(TLoadShapeAction.SngSave)]);
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

function GetStdDev(obj: TObj): Double;
begin
    Result := obj.StdDev;
end;

procedure DoAction(Obj: TObj; action: TLoadShapeAction);
begin
    case action of
        TLoadShapeAction.Normalize:
            Obj.Normalize;
        TLoadShapeAction.DblSave:
            Obj.SaveToDblFile;
        TLoadShapeAction.SngSave:
            Obj.SaveToSngFile;
    end;
end;

procedure SetNumPoints(Obj: TObj; Value: Integer);
begin
    with Obj do 
        if ExternalMemory then
        begin
            DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
            Exit;
        end
        else
            NumPoints := Value;
end;

procedure TLoadShape.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // boolean properties
    PropertyType[ord(TProp.MemoryMapping)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.UseActual)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.MemoryMapping)] := ptruint(@obj.UseMMF);
    PropertyOffset[ord(TProp.UseActual)] := ptruint(@obj.UseActual);

    // advanced doubles
    PropertyOffset[ord(TProp.sinterval)] := ptruint(@obj.Interval);
    PropertyScale[ord(TProp.sinterval)] := 1 / 3600.0;
    PropertyFlags[ord(TProp.sinterval)] := [TPropertyFlag.Redundant];

    PropertyOffset[ord(TProp.minterval)] := ptruint(@obj.Interval);
    PropertyScale[ord(TProp.minterval)] := 1 / 60.0;
    PropertyFlags[ord(TProp.minterval)] := [TPropertyFlag.Redundant];

    // double properties
    PropertyOffset[ord(TProp.interval)] := ptruint(@obj.Interval);
    PropertyOffset[ord(TProp.Pmax)] := ptruint(@obj.MaxP);
    PropertyOffset[ord(TProp.Qmax)] := ptruint(@obj.MaxQ);
    PropertyOffset[ord(TProp.Pbase)] := ptruint(@obj.BaseP);
    PropertyOffset[ord(TProp.Qbase)] := ptruint(@obj.BaseQ);

    PropertyOffset[ord(TProp.mean)] := ptruint(@obj.FMean);
    PropertyReadFunction[ord(TProp.mean)] := @GetMean;
    PropertyFlags[ord(TProp.mean)] := [TPropertyFlag.ReadByFunction];
    
    PropertyOffset[ord(TProp.stddev)] := ptruint(@obj.FStdDev);
    PropertyReadFunction[ord(TProp.stddev)] := @GetStdDev;
    PropertyFlags[ord(TProp.stddev)] := [TPropertyFlag.ReadByFunction];

    // double arrays, special
    PropertyType[ord(TProp.hour)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.hour)] := ptruint(@obj.dH);
    PropertyOffset2[ord(TProp.hour)] := ptruint(@obj.FNumPoints);
    PropertyOffset3[ord(TProp.hour)] := ptruint(@obj.ExternalMemory);
    PropertyFlags[ord(TProp.hour)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.CustomGet, TPropertyFlag.ConditionalReadOnly];

    PropertyType[ord(TProp.mult)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.mult)] := ptruint(@obj.dP);
    PropertyOffset2[ord(TProp.mult)] := ptruint(@obj.FNumPoints);
    PropertyOffset3[ord(TProp.mult)] := ptruint(@obj.ExternalMemory);
    PropertyFlags[ord(TProp.mult)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.CustomGet, TPropertyFlag.ConditionalReadOnly, TPropertyFlag.Redundant];

    PropertyType[ord(TProp.Pmult)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.Pmult)] := ptruint(@obj.dP);
    PropertyOffset2[ord(TProp.Pmult)] := ptruint(@obj.FNumPoints);
    PropertyOffset3[ord(TProp.Pmult)] := ptruint(@obj.ExternalMemory);
    PropertyFlags[ord(TProp.Pmult)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.CustomGet, TPropertyFlag.ConditionalReadOnly];

    PropertyType[ord(TProp.Qmult)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.Qmult)] := ptruint(@obj.dQ);
    PropertyOffset2[ord(TProp.Qmult)] := ptruint(@obj.FNumPoints);
    PropertyOffset3[ord(TProp.Qmult)] := ptruint(@obj.ExternalMemory);
    PropertyFlags[ord(TProp.Qmult)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.CustomGet, TPropertyFlag.ConditionalReadOnly];

    // integer
    Propertytype[ord(TProp.npts)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.npts)] := ptruint(@obj.FNumPoints);
    PropertyWriteFunction[ord(TProp.npts)] := @SetNumPoints;
    PropertyFlags[ord(TProp.npts)] := [TPropertyFlag.WriteByFunction];

    // enum action
    PropertyType[ord(TProp.Action)] := TPropertyType.StringEnumActionProperty;
    PropertyOffset[ord(TProp.Action)] := ptruint(@DoAction); 
    PropertyOffset2[ord(TProp.Action)] := PtrInt(ActionEnum); 

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

    PropertyType[ord(TProp.PQCSVFile)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.PQCSVFile)] := ptruint(@obj.pqcsvfile);
    PropertyFlags[ord(TProp.PQCSVFile)] := [TPropertyFlag.IsFilename];

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
    ParmName := DSS.AuxParser.NextParam;
    LocalCol := 1;

    if CompareText(Parmname, 'file') = 0 then
    begin
        fileType := TLSFileType.PlainText;

        // Look for other options  (may be in either order)
        ParmName := DSS.AuxParser.NextParam;
        Param := DSS.AuxParser.StrValue;
        while Length(Param) > 0 do
        begin
            if CompareTextShortest(ParmName, 'column') = 0 then
                LocalCol := DSS.AuxParser.IntValue;
            ParmName := DSS.AuxParser.NextParam;
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
function CreateMMF(Obj: TObj; const S: String; Destination: TMMShapeType): Boolean;
var
    ParmName,
    Param: String;
begin
    with Obj do
    try
        DSS.AuxParser.CmdString := S;
        ParmName := DSS.AuxParser.NextParam;
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

procedure TLoadShapeObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
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
    end;
    case Idx of
        ord(TProp.npts):
            // Force as the always first property when saving in a later point
            PrpSequence[Idx] := -10;
        ord(TProp.mult), ord(TProp.Pmult), ord(TProp.csvfile), ord(TProp.sngfile), ord(TProp.dblfile), ord(TProp.qmult):
        begin
            FStdDevCalculated := FALSE;   // now calculated on demand
            NumPoints := FNumPoints;  // Keep Properties in order for save command
        end;
        ord(TProp.Qmax):
            MaxQSpecified := TRUE;
        ord(TProp.MemoryMapping):
            if UseMMF then
                UseFloat64;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
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
                if not CreateMMF(self, Value, TMMShapeType.P) then
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
                if not CreateMMF(self, Value, TMMShapeType.Q) then
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
begin
    with TObj(ptr) do
    begin
        if Assigned(dP) or Assigned(sP) then
            SetMaxPandQ;

        Exclude(Flags, Flg.EditionActive);
    end;
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
        for i := 1 to NumPoints do
            dP[i] := Other.dP[Stride * i];
    end
    else
        ReallocMem(dP, 0);

    if Assigned(Other.dQ) then
    begin
        ReallocMem(dQ, SizeOf(Double) * NumPoints);
        //Move(Other.dQ[0], dQ[0], SizeOf(Double) * NumPoints);
        for i := 1 to NumPoints do
            dQ[i] := Other.dQ[Stride * i];
        
    end;

    if Interval > 0.0 then
        ReallocMem(dH, 0)
    else
    begin
        ReallocMem(dH, SizeOf(Double) * NumPoints);
        // Move(Other.dH[0], dH[0], SizeOf(Double) * NumPoints);
        for i := 1 to NumPoints do
            dH[i] := Other.dH[Stride * i];
    end;

    // Single versions
    if Assigned(Other.sP) then
    begin
        ReallocMem(sP, SizeOf(Single) * NumPoints);
        // Move(Other.sP[0], sP[0], SizeOf(Single) * NumPoints);
        for i := 1 to NumPoints do
            sP[i] := Other.sP[Stride * i];
    end
    else
        ReallocMem(sP, 0);

    if Assigned(Other.sQ) then
    begin
        ReallocMem(sQ, SizeOf(Single) * NumPoints);
        // Move(Other.sQ[0], sQ[0], SizeOf(Single) * NumPoints);
        for i := 1 to NumPoints do
            sQ[i] := Other.sQ[Stride * i];
    end;

    if Interval > 0.0 then
        ReallocMem(sH, 0)
    else
    begin
        ReallocMem(sH, SizeOf(Single) * NumPoints);
        // Move(Other.sH[0], sH[0], SizeOf(Single) * NumPoints);
        for i := 1 to NumPoints do
            sH[i] := Other.sH[Stride * i];
    end;

    UseActual := Other.UseActual;
    UseMMF := Other.UseMMF;
    BaseP := Other.BaseP;
    BaseQ := Other.BaseQ;
    SetMaxPandQ;
end;

procedure Do2ColCSVFile(Obj: TObj; const FileName: String);
//   Process 2-column CSV file (3-col if time expected)
var
    F: TStream = nil;
    i: Integer;
    s: String;
begin
    if Obj.ExternalMemory then
    begin
        DoSimpleMsg(Obj.DSS, 'Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
        Exit;
    end;

    try
        F := Obj.DSS.GetROFileStream(FileName);
    except
        DoSimpleMsg(Obj.DSS, 'Error opening file: "%s"', [FileName], 613);
        FreeAndNil(F);
        Exit;
    end;

    with Obj do
    try
        if UseMMF then
        begin
            FreeAndNil(F);
            mmDataSize := NumPoints;
            mmFileCmd := 'file=' + FileName + ' column=1';      // Command for P
            if not CreateMMF(Obj, mmFileCmd, TMMShapeType.P) then  // Creates MMF for the whole file
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
        while ((F.Position + 1) < F.Size) and (i < (FNumPoints - 1)) do
        begin
            Inc(i);
            FSReadln(F, s); // read entire line and parse with AuxParser
            // AuxParser allows commas or white space
            with DSS.AuxParser do
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
        FreeAndNil(F);
        inc(i);
        if i <> FNumPoints then
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

procedure DoCSVFile(Obj: TObj; const FileName: String);
var
    F: TStream = nil;
    i: Integer;
    s: String;
begin
    if Obj.ExternalMemory then
    begin
        DoSimpleMsg(Obj.DSS, 'Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61102);
        Exit;
    end;
    try
        F := Obj.DSS.GetROFileStream(FileName);
    except
        DoSimpleMsg(Obj.DSS, 'Error opening file: "%s"', [FileName], 613);
        FreeAndNil(F);
        Exit;
    end;

    with Obj do
    try
        if UseMMF then
        begin
            FreeAndNil(F);
            s := 'file=' + FileName;
            if CreateMMF(Obj, s, TMMShapeType.P) then
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
        while ((F.Position + 1) < F.Size) and (i < (FNumPoints - 1)) do
        begin
            Inc(i);
            FSReadln(F, s); // read entire line  and parse with AuxParser
            // AuxParser allows commas or white space
            with DSS.AuxParser do
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
        FreeAndNil(F);
        inc(i);
        if i <> FNumPoints then
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

procedure DoSngFile(Obj: TObj; const FileName: String);
var
    s: String;
    F: TStream = NIL;
    Hr, M: Single;
    i: Integer;
    bytesRead: Int64;
begin
    if Obj.ExternalMemory then
    begin
        DoSimpleMsg(Obj.DSS, _('Data cannot be changed for LoadShapes with external memory! Reset the data first.'), 61102);
        Exit;
    end;
    try
        F := Obj.DSS.GetROFileStream(FileName);
    except
        DoSimpleMsg(Obj.DSS, 'Error opening file: "%s"', [FileName], 615);
        Exit;
    end;

    with Obj do
    try
        if UseMMF then
        begin
            FreeAndNil(F);
            s := 'sngfile=' + FileName;
            if not CreateMMF(Obj, s, TMMShapeType.P) then
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
                ReallocMem(sP, FNumPoints * SizeOf(Single));
            i := -1;
            
            if Interval = 0.0 then
            begin
                if sH = nil then
                    ReallocMem(sH, FNumPoints * SizeOf(Single));

                while i < (FNumPoints - 1) do
                begin
                    Inc(i);
                    if F.Read(sH[i], 4) <> 4 then break;
                    if F.Read(sP[i], 4) <> 4 then break;
                end;
            end
            else
            begin
                bytesRead := F.Read(sP[0], FNumPoints * sizeof(Single));
                FNumPoints := min(bytesRead div 4, FNumPoints);
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
            while i < (FNumPoints - 1) do
            begin
                Inc(i);
                if F.Read(Hr, sizeof(Single)) <> sizeof(Single) then break;
                if F.Read(M, sizeof(Single)) <> sizeof(Single) then break;
                dH[i] := Hr;
                dP[i] := M;
            end;
            inc(i);
            if i <> FNumPoints then
                NumPoints := i;
        end
        else 
        begin
            ReallocMem(sP, FNumPoints * SizeOf(Single));
            bytesRead := F.Read(sP[0], FNumPoints * sizeof(Single));
            FNumPoints := min(bytesRead div sizeof(Single), FNumPoints);
            for i := 0 to FNumPoints - 1 do
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

procedure DoDblFile(Obj: TObj; const FileName: String);
var
    s: String;
    F: TStream = NIL;
    i: Integer;
    bytesRead: Int64;
begin
    if Obj.ExternalMemory then
    begin
        DoSimpleMsg(Obj.DSS, _('Data cannot be changed for LoadShapes with external memory! Reset the data first.'), 61102);
        Exit;
    end;
    try
        F := Obj.DSS.GetROFileStream(FileName);
    except
        DoSimpleMsg(Obj.DSS, 'Error opening file: "%s"', [FileName], 617);
        Exit;
    end;

    with Obj do
    try
        if UseMMF then
        begin
            FreeAndNil(F);
            s := 'dblfile=' + FileName;
            if not CreateMMF(Obj, s, TMMShapeType.P) then
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
        
        if Interval = 0.0 then 
        begin
            while i < (FNumPoints - 1) do
            begin
                Inc(i);
                if F.Read(dH[i], sizeof(Double)) <> sizeof(Double) then break;
                if F.Read(dP[i], sizeof(Double)) <> sizeof(Double) then break;
            end;
            inc(i);
            if i <> FNumPoints then
                NumPoints := i;
        end
        else 
        begin
            bytesRead := F.Read(dP[0], FNumPoints * sizeof(Double));
            FNumPoints := min(bytesRead div sizeof(Double), FNumPoints);
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

function TLoadShapeObj.GetMultAtHour(hr: Double): Complex;
// This function returns a multiplier for the given hour.
// If no points exist in the curve, the result is  1.0
// If there are fewer points than requested, the curve is simply assumed to repeat
// Thus a daily load curve can suffice for a yearly load curve:  You just get the
// same day over and over again.
// The value returned is the nearest to the interval requested.  Thus if you request
// hour=12.25 and the interval is 1.0, you will get interval 12.
var
    i: Integer;
    offset, // index including stride
    poffset: Int64; // previous index including stride
    
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
        i := round(hr / Interval);
        if UseMMF then
        begin
            if i > mmDataSize then 
                i := i mod mmDataSize;  // Wrap around using remainder
            if i = 0 then 
                i := mmDataSize;
            Result.re := InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, i, mmLineLen);
            if Assigned(dQ) then
                Result.im := InterpretDblArrayMMF(DSS, mmViewQ, mmFileTypeQ, mmColumnQ, i, mmLineLenQ)
            else
                Result.im := Set_Result_im(Result.re);
            
            Exit;
        end;
        
        if i > FNumPoints then 
            i := i mod FNumPoints;  // Wrap around using remainder
        if i = 0 then 
            i := FNumPoints;
        
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
                Result.re := InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, i + 1, mmLineLen);
                if Assigned(dQ) then
                    Result.im := InterpretDblArrayMMF(DSS, mmViewQ, mmFileTypeQ, mmColumnQ, i + 1, mmLineLenQ)
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
                Result.re := InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, LastValueAccessed + 1, mmLineLen) +
                    (Hr - dH[LastValueAccessed]) / (dH[i] - dH[LastValueAccessed]) *
                    (InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, i, mmLineLen) -
                    InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, LastValueAccessed, mmLineLen));
                if Assigned(dQ) then
                    Result.im := InterpretDblArrayMMF(DSS, mmViewQ, mmFileTypeQ, mmColumnQ, LastValueAccessed + 1, mmLineLenQ) +
                        (Hr - dH[LastValueAccessed]) / (dH[i] - dH[LastValueAccessed]) *
                        (InterpretDblArrayMMF(DSS, mmViewQ, mmFileTypeQ, mmColumnQ, i, mmLineLenQ) -
                        InterpretDblArrayMMF(DSS, mmViewQ, mmFileTypeQ, mmColumnQ, LastValueAccessed, mmLineLenQ))
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
    if UseMMF or ExternalMemory then //TODO: disallow MMF?
    begin
        DoSimpleMsg(_('Data cannot be changed for LoadShapes with external memory or memory-mapped files! Reset the data first.'), 61102);
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

procedure TLoadShapeObj.CalcMeanandStdDev;
begin
    if UseMMF or ExternalMemory then
        Exit;

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
    if (i < FNumPoints) and (i >= 0) then
    begin
        if UseMMF then
            Result := InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, i + 1, mmLineLen)
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
    if (i < FNumPoints) and (i >= 0) then
    begin
        if UseMMF then
            Result := InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, i + 1, mmLineLen)
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
    
    if (i < FNumPoints) and (i >= 0) then
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
        if (i < FNumPoints) and (i >= 0) then
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
                Result := GetDSSArray_Real(FNumPoints, pDoubleArray(dP))
            else if sP <> NIL then
                Result := GetDSSArray_Single(FNumPoints, pSingleArray(sP));
        end;
        ord(TProp.hour):
            if dH <> NIL then
                Result := GetDSSArray_Real(FNumPoints, pDoubleArray(dH))
            else if sH <> NIL then
                Result := GetDSSArray_Single(FNumPoints, pSingleArray(sH));
        ord(TProp.qmult):
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
    else
        Result := inherited GetPropertyValue(index);
    end;
end;

procedure TLoadShapeObj.SaveToDblFile;
var
    myDBL: Double;
    F: TFileStream = nil;
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
        FName := DSS.OutputDirectory {CurrentDSSDir} + Format('%s_P.dbl', [Name]);
        F := TFileStream.Create(FName, fmCreate);
        if UseMMF then
        begin
            for i := 1 to NumPoints do
            begin
                myDBL := InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, i, mmLineLen);
                F.Write(myDBL, sizeOf(myDBL));
            end;
        end
        else
        begin
            for i := 1 to NumPoints do
                F.Write(dP[Stride * i], sizeOf(Double)); 
        end;
        DSS.GlobalResult := 'mult=[dblfile=' + FName + ']';
    finally
        FreeAndNil(F);
    end;

    if Assigned(dQ) then
    begin
        try
            FName := DSS.OutputDirectory {CurrentDSSDir} + Format('%s_Q.dbl', [Name]);
            F := TFileStream.Create(FName, fmCreate);
            if UseMMF then
            begin
                for i := 1 to NumPoints do
                begin
                    myDBL := InterpretDblArrayMMF(DSS, mmViewQ, mmFileTypeQ, mmColumnQ, i, mmLineLenQ);
                    F.Write(myDBL, sizeOf(myDBL));
                end;
            end
            else
                for i := 1 to NumPoints do
                    F.Write(dQ[Stride * i], sizeOf(Double)); 
            AppendGlobalResult(DSS, ' Qmult=[dblfile=' + FName + ']');
        finally
            FreeAndNil(F);
        end;
    end;
end;

procedure TLoadShapeObj.SaveToSngFile;
var
    F: TFileStream = nil;
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
        FName := DSS.OutputDirectory {CurrentDSSDir} + Format('%s_P.sng', [Name]);
        F := TFileStream.Create(FName, fmCreate);
        for i := 1 to NumPoints do
        begin
            if UseMMF then
                Temp := InterpretDblArrayMMF(DSS, mmView, mmFileType, mmColumn, i, mmLineLen)
            else
                Temp := dP[Stride * i];
            F.Write(Temp, SizeOf(Temp));
        end;
        DSS.GlobalResult := 'mult=[sngfile=' + FName + ']';
    finally
        FreeAndNil(F);
    end;

    if Assigned(dQ) then
    begin
        try
            FName := DSS.OutputDirectory {CurrentDSSDir} + Format('%s_Q.sng', [Name]);
            F := TFileStream.Create(FName, fmCreate);
            for i := 1 to NumPoints do
            begin
                if UseMMF then
                    Temp := InterpretDblArrayMMF(DSS, mmViewQ, mmFileTypeQ, mmColumnQ, i, mmLineLenQ)
                else
                    Temp := dQ[Stride * i];
                F.Write(Temp, SizeOf(Temp));
            end;
            AppendGlobalResult(DSS, ' Qmult=[sngfile=' + FName + ']');
        finally
            FreeAndNil(F);
        end;
    end;
end;

procedure TLoadShapeObj.SetMaxPandQ;
var
    iMaxP: Integer;
begin
    if UseMMF or ExternalMemory then
        Exit;

    if Assigned(dP) then
    begin
        iMaxP := iMaxAbsdblArrayValue(NumPoints, PDoubleArray(dP)) - 1;
        if iMaxP >= 0 then
        begin
            MaxP := dP[{Stride *} iMaxP];
            if not MaxQSpecified then
                if Assigned(dQ) then
                    MaxQ := dQ[{Stride *} iMaxP]
                else
                    MaxQ := 0.0;
        end;
    end
    else
    begin
        iMaxP := iMaxAbssngArrayValue(NumPoints, PSingleArray(sP)) - 1;
        if iMaxP >= 0 then
        begin
            MaxP := sP[{Stride *} iMaxP];
            if not MaxQSpecified then
                if Assigned(sQ) then
                    MaxQ := sQ[{Stride *} iMaxP]
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
        DoSimpleMsg(_('Data cannot be changed for LoadShapes with external memory or memory-mapped files! Reset the data first.'), 61104);
        Exit;
    end;

    if Assigned(sH) then
    begin
        if dH = NIL then
        begin
            ReallocMem(dH, FNumPoints * SizeOf(Double));
            for i := 0 to FNumPoints - 1 do
                dH[i] := sH[i];
        end;
        FreeMem(sH);
        sH := nil;
    end;
    if Assigned(sP) then
    begin
        if dP = NIL then
        begin
            ReallocMem(dP, FNumPoints * SizeOf(Double));
            for i := 0 to FNumPoints - 1 do
                dP[i] := sP[i];
        end;
        FreeMem(sP);
        sP := nil;
    end;
    if Assigned(sQ) then
    begin
        if dQ = NIL then
        begin
            ReallocMem(dQ, FNumPoints * SizeOf(Double));
            for i := 0 to FNumPoints - 1 do
                dQ[i] := sQ[i];
        end;
        FreeMem(sQ);
        sQ := nil;
    end;
end;

function TLoadShapeObj.GetMultAtHourSingle(hr: Double): Complex;
var
    i: Integer;
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
        i := round(hr / Interval);
        if i > FNumPoints then 
            i := i mod FNumPoints;  // Wrap around using remainder
        if i = 0 then 
            i := FNumPoints;

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

initialization
    PropInfo := NIL;
finalization
    ActionEnum.Free;
end.
