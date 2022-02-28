
unit DSSClass;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

USES
    Command, 
    Arraydef, 
    Hashlist, 
    Classes, 
    DSSPointerList, 
    NamedObject, 
    ParserDel, 
{$IFDEF DSS_CAPI_PM}
    SyncObjs, 
{$ENDIF}    
    UComplex, DSSUcomplex, 
    contnrs,
    CAPI_Types,
    gettext;

type
{$SCOPEDENUMS ON}
    TDSSObjectFlag = (
        EditionActive, 
        HasBeenSaved, // originally from TDSSObject

        // Originally from TDSSCktElement
        Checked,
        Flag, // General purpose Flag for each object  don't assume inited
        HasEnergyMeter,
        HasSensorObj,
        IsIsolated,
        HasControl,
        IsMonitored, // indicates some control is monitoring this element
        // IsPartofFeeder,  -- UNUSED
        // Drawn,  // Flag used in tree searches etc  -- UNUSED
        HasOCPDevice, // Fuse, Relay, or Recloser
        HasAutoOCPDevice, // Relay or Recloser only
        // HasSwtControl // Has a remotely-controlled Switch -- UNUSED
        NeedsRecalc // Used for Edit command loops
    );
    TDSSObjectFlags = set of TDSSObjectFlag;
    Flg = TDSSObjectFlag;

    TActorStatus = (
        Busy = 0,
        Idle = 1
    );
    TDSSObjectProp = (
        INVALID = 0,
        like = 1
    );
    
    TPropertyFlag = ( // Currently targets advanced double properties
        // CustomParse,
        CustomSet, // Implemented only for a few types -- parse and pass value to the object.
        CustomSetRaw, // Only for some LoadShape props -- pass string instead of parsing first.
        CustomGet,
        IsFilename, // for strings
        IgnoreInvalid,
        NonPositive,
        NonNegative,
        NonZero,
        Transform_Abs,
        Transform_LowerCase,
        ScaledByFunction,
        WriteByFunction,
        ReadByFunction,
        RealPart,
        ImagPart,
        GreaterThanOne,
        IntegerStructIndex,
        IntegerOnArray,
        IntervalUnits,
        AltIndex,
        SizeIsFunction,
        SilentReadOnly, //TODO: SilentRO=ignore writes. We might want to change this in the future to error out instead of ignoring
        ConditionalReadOnly, // only implemented for MappedStringEnumProperty
        IntegerToDouble, // for double arrays -- read integer, convert to double
        CheckForVar, // for object references
        AllowNone, // for arrays
        ArrayMaxSize, // for arrays
        ValueOffset, // only implemented for integers
        Redundant,
        Unused
    );

    TPropertyFlags = set of TPropertyFlag;

    TPropertyType = (
        DoubleProperty = 0,
        EnabledProperty,
        MakeLikeProperty,
        BooleanActionProperty,
        StringEnumActionProperty,

        //TODO: use Flags for the following (i.e. transform them into DoubleProperty)

        // For (double,string,integer)-on-array, 
        // offset is the offset pointer to the pointer/array
        // offset2 is the offset pointer to the element index (Integer)
        DoubleOnArrayProperty, //1-based
        // For (double,string,integer)-on-array, 
        // offset is the offset pointer to the pointer/array
        // offset2 is the offset pointer to the element index (Integer)
        // step is the size of the step used in offset2 (direct integer)
        DoubleOnStructArrayProperty,
        
        StringSilentROFunctionProperty, //TODO: SilentRO=ignore writes. We might want to change this in the future to error out instead of ignoring

        DoubleArrayProperty,
        DoubleDArrayProperty, // -> For dynamic arrays
        DoubleVArrayProperty, // -> Use ParseAsVector
        DoubleFArrayProperty, // -> For fixed-size arrays, with size in offset2
        ComplexPartSymMatrixProperty,
        DoubleSymMatrixProperty,

        IntegerArrayProperty,
        StringListProperty,

        // A string with a name of an object
        // offset is the offset pointer to the pointer to the object
        // offset2 is the pointer to the TDSSClass; if NIL, assumes any CktElement
        DSSObjectReferenceProperty,

        // List of strings with names of objects
        // offset is the offset pointer to the pointer to the object
        // offset2 is the pointer to the TDSSClass; if NIL, assumes any CktElement
        DSSObjectReferenceArrayProperty,

        DoubleArrayOnStructArrayProperty,

        IntegerProperty,
        StringProperty,
        ComplexProperty,
        BooleanProperty,
        BusProperty,
        ComplexPartsProperty,

        MappedStringEnumProperty,
        MappedIntEnumProperty,
        MappedStringEnumArrayProperty,
        MappedStringEnumOnStructArrayProperty,
        MappedStringEnumArrayOnStructArrayProperty,

        // For (double,string,integer)-on-array, 
        // offset is the offset pointer to the pointer/array
        // offset2 is the offset pointer to the element index (Integer)
        StringOnArrayProperty,  //1-based

        // For (double,string,integer)-on-array, 
        // offset is the offset pointer to the pointer/array
        // offset2 is the offset pointer to the element index (Integer)
        // step is the size of the step used in offset2 (direct integer)
        IntegerOnStructArrayProperty,
        StringOnStructArrayProperty,

        BusOnStructArrayProperty,
        BusesOnStructArrayProperty

        // OtherProperty
    );
{$SCOPEDENUMS OFF}

    ArrayOfDouble = Array of Double;
    ArrayOfInteger = Array of Integer;
    ArrayOfString = Array of String;
    ArrayOfPointer = Array of Pointer;

    PropertyTypeArray = Array[1..100] of TPropertyType;
    pPropertyTypeArray = ^PropertyTypeArray;

    TDoublePropertyFunction = function (obj: Pointer): Double;
    TIntegerPropertyFunction = function (obj: Pointer): Integer;
    TStringPropertyFunction = function (obj: Pointer): String;
    TStringListPropertyFunction = function (obj: Pointer): TStringList;
    TDoublesPropertyFunction = procedure (obj: Pointer; var ResultPtr: PDouble; ResultCount: PAPISize);
    TObjRefsPropertyFunction = procedure (obj: Pointer; var ResultPtr: PPointer; ResultCount: PAPISize);

    // TDoubleArrayPropertyFunction = function (obj: Pointer): Array of Double;
    TWriteDoublePropertyFunction = procedure (obj: Pointer; Value: double);
    TWriteObjRefPropertyFunction = procedure (obj: Pointer; Value: Pointer);
    TWriteIntegerPropertyFunction = procedure (obj: Pointer; Value: Integer);
    TWriteStringListPropertyFunction = procedure (obj: Pointer; Value: TStringList);
    TWriteObjRefsPropertyFunction = procedure (obj: Pointer; Values: PPointer; ValueCount: Integer);
    TWriteDoublesPropertyFunction = procedure (obj: Pointer; Values: PDouble; ValueCount: Integer);
    TEnumActionProcedure = TWriteIntegerPropertyFunction;
    TActionProcedure = procedure (obj: Pointer);

    BooleanArray = Array[1..100] of Boolean;
    pBooleanArray = ^BooleanArray;

    TDSSEnum = class(TObject)
    private
        Sequential: Boolean; // are the main ordinals (without aliases) sequential/contiguous?
        MinOrdinal: Integer;
        MaxOrdinal: Integer;
        MinChars, MaxChars: Integer; // minimum and maximum number of chars that are required to disambiguate strings
        Names, LowerNames: Array of String;
        Ordinals: Array of Integer;
        Name: String;
    public
        DefaultValue: Integer;
        UseFirstFound, AllowLonger: Boolean;
        Hybrid: Boolean;

        constructor Create(EnumName: String; IsSequential: Boolean; MinCh, MaxCh: Integer; EnumNames: Array of String; EnumOrds: Array of Integer);
        destructor Destroy; override;
        function OrdinalToString(Value: Integer): String;
        function StringToOrdinal(Value: String): Integer;
        function IsOrdinalValid(Value: Integer): Boolean;
    end;

    TAction = record
        ActionCode: Integer;
        DeviceHandle: Integer;
    end;
    
    pAction = ^TAction;

    TDSSContext = class;

    dss_callback_plot_t = function (DSS: TDSSContext; jsonParams: PChar): Integer; CDECL;
    dss_callback_message_t = function (DSS: TDSSContext; messageStr: PChar; messageType: Integer): Integer; CDECL;

    // Base for all collection classes
    TDSSClass = class(TObject)
    type 
        THashListType = {$IFDEF DSS_CAPI_HASHLIST}TAltHashList;{$ELSE}THashList;{$ENDIF}
     private

        procedure Set_Active(value:Integer);
        function Get_ElementCount: Integer;
        function Get_First: Integer;
        function Get_Next: Integer;

        procedure ResynchElementNameList;

    Protected
        ClassParents: TStringList;
        Class_Name: String;
        ActiveElement: Integer;   // index of present ActiveElement
        ActiveProperty: Integer;
        ElementNameList: THashListType;

        Function AddObjectToList(Obj:Pointer; Activate: Boolean = True):Integer;  // Used by NewObject
        Function Get_FirstPropertyName:String;
        Function Get_NextPropertyName:String;
        Procedure CountPropertiesAndAllocate;virtual;
        procedure DefineProperties;virtual;

        procedure PopulatePropertyNames(PropOffset: Integer; NumProps: Integer; EnumInfo: Pointer; ReplacePct: Boolean = True);
     public
        DSS: TDSSContext;
        CommandList: TCommandlist;
        NumProperties: Integer;

        // TODO: move to array of records
        PropertyName: pStringArray;
        PropertyScale, PropertyValueOffset: pDoubleArray;
        PropertyTrapZero: pDoubleArray;
        PropertyInverse: pBooleanArray;
        PropertyType: pPropertyTypeArray;
        PropertyWriteFunction, PropertyReadFunction: PPointerArray;
        PropertyOffset: pPtrIntArray; // For most simple properties
        PropertyOffset2: pPtrIntArray; // For separate complex quantities, double-on-array, ...
        PropertyOffset3: pPtrIntArray; // For setters in e.g. object refs
        
        PropertyStructArrayIndexOffset, PropertyStructArrayIndexOffset2,
        PropertyStructArrayOffset, 
        PropertyStructArrayStep, 
        PropertyStructArrayCountOffset: PtrUint;

        PropertyFlags: Array of TPropertyFlags; //TODO: 0 is unused until things are migrated later

        DSSClassType: Integer;

        ElementList: TDSSPointerList;
        ElementNamesOutOfSynch: Boolean;     // When device gets renamed

        Saved: Boolean;

        constructor Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
        destructor Destroy; override;
        
        Procedure ReallocateElementNameList;

        // function CustomParse(ptr: Pointer; Idx: Integer; Param: String): Boolean; virtual;

        function BeginEdit(ptr: Pointer; SetActive: Boolean=True): Pointer; virtual;
        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; virtual;
        function Edit(Parser: TDSSParser): Integer;

        function NewObject(const ObjName: String; Activate: Boolean = True):Pointer; Virtual; overload;
        function NewObject(const ObjName: String; Activate: Boolean; out Idx: Integer):Pointer; overload; // for compatibility, when the index is required

        Function SetActive(const ObjName:String):Boolean;
        Function GetActiveObj:Pointer; // Get address of active obj of this class
        Function Find(const ObjName:String; const ChangeActive: Boolean=True): Pointer; virtual;  // Find an obj of this class by name

        Function PropertyIndex(Const Prop:String):Integer;
        Property FirstPropertyName:String read Get_FirstPropertyName;
        Property NextPropertyName:String read Get_NextPropertyName;
        function GetPropertyHelp(idx: Integer): String;

        Property Active:Integer read ActiveElement write Set_Active;
        Property ElementCount:Integer read Get_ElementCount;
        Property First:Integer read Get_First;
        Property Next:Integer read Get_Next;
        Property Name:String read Class_Name;
        
    protected
        // DSSContext convenience functions
        procedure DoErrorMsg(Const S, Emsg, ProbCause: String; ErrNum: Integer);inline;
        procedure DoSimpleMsg(Const S: String; ErrNum:Integer);inline;overload;
        procedure DoSimpleMsg(Const S: String; fmtArgs: Array of Const; ErrNum:Integer);inline;overload;
    end;

    TProxyClass = class(TDSSClass) // use for the property system (object references with multiple options)
    public
        TargetClasses: Array Of TDSSClass;
        TargetClassNames: Array Of String;

        constructor Create(dssContext: TDSSContext; Targets: Array Of String);
        destructor Destroy; override;
        procedure DefineProperties; override;
        function Find(const ObjName: String; const ChangeActive: Boolean): Pointer; override;
    end;

    TDSSContext = class(TObject)
    protected
        FLoadShapeClass: TDSSClass;
        FTShapeClass: TDSSClass;
        FPriceShapeClass: TDSSClass;
        FXYCurveClass: TDSSClass;
        FGrowthShapeClass: TDSSClass;
        FSpectrumClass: TDSSClass;
        FEnergyMeterClass: TDSSClass;
        FMonitorClass: TDSSClass;
        FSensorClass: TDSSClass;
        FTCC_CurveClass: TDSSClass;
        FWireDataClass: TDSSClass;
        FCNDataClass: TDSSClass;
        FTSDataClass: TDSSClass;
        FLineGeometryClass: TDSSClass;
        FLineSpacingClass: TDSSClass;
        FLineCodeClass: TDSSClass;
        FStorageClass: TDSSClass;
        FStorage2Class: TDSSClass;
        FPVSystemClass: TDSSClass;
        FPVSystem2Class: TDSSClass;
        FInvControlClass: TDSSClass;
        FInvControl2Class: TDSSClass;
        FExpControlClass: TDSSClass;
        FLineClass: TDSSClass;
        FVSourceClass: TDSSClass;
        FISourceClass: TDSSClass;
        FVCSSClass: TDSSClass;
        FLoadClass: TDSSClass;
        FTransformerClass: TDSSClass;
        FRegControlClass: TDSSClass;
        FCapacitorClass: TDSSClass;
        FReactorClass: TDSSClass;
        FCapControlClass: TDSSClass;
        FFaultClass: TDSSClass;
        FGeneratorClass: TDSSClass;
        FGenDispatcherClass: TDSSClass;
        FStorageControllerClass: TDSSClass;
        FStorageController2Class: TDSSClass;
        FRelayClass: TDSSClass;
        FRecloserClass: TDSSClass;
        FFuseClass: TDSSClass;
        FSwtControlClass: TDSSClass;
        FUPFCClass: TDSSClass;
        FUPFCControlClass: TDSSClass;
        FESPVLControlClass: TDSSClass;
        FIndMach012Class: TDSSClass;
        FGICsourceClass: TDSSClass;
        FAutoTransClass: TDSSClass;
        FVSConverterClass: TDSSClass;
        FXfmrCodeClass: TDSSClass;
        FGICLineClass: TDSSClass;
        FGICTransformerClass: TDSSClass;

        FActiveFeederObj: TObject;
        FActiveSolutionObj: TObject;
        FActiveCapControlObj: TObject;
        FActiveESPVLControlObj: TObject;
        FActiveExpControlObj: TObject;
        FActiveGenDispatcherObj: TObject;
        FActiveInvControlObj: TObject;
        FActiveInvControl2Obj: TObject;
        FActiveRecloserObj: TObject;
        FActiveRegControlObj: TObject;
        FActiveRelayObj: TObject;
        FActiveStorageControllerObj: TObject;
        FActiveStorageController2Obj: TObject;
        FActiveSwtControlObj: TObject;
        FActiveUPFCControlObj: TObject;
        // FActiveVVCControlObj: TObject;
        FActiveConductorDataObj: TObject;
        FActiveGrowthShapeObj: TObject;
        FActiveLineCodeObj: TObject;
        FActiveLineGeometryObj: TObject;
        FActiveLineSpacingObj: TObject;
        FActiveLoadShapeObj: TObject;
        FActivePriceShapeObj: TObject;
        FActiveSpectrumObj: TObject;
        FActiveTCC_CurveObj: TObject;
        FActiveTShapeObj: TObject;
        FActiveXfmrCodeObj: TObject;
        FActiveXYcurveObj: TObject;
        FActiveEnergyMeterObj: TObject;
        // FActiveFMonitorObj: TObject;
        FActiveMonitorObj: TObject;
        FActiveSensorObj: TObject;
        FActiveEquivalentObj: TObject;
        FActiveGeneratorObj: TObject;
        // FActiveGeneric5Obj: TObject;
        FActiveGICLineObj: TObject;
        FActiveGICsourceObj: TObject;
        FActiveIndMach012Obj: TObject;
        FActiveIsourceObj: TObject;
        FActiveLoadObj: TObject;
        FActivePVsystemObj: TObject;
        FActivePVsystem2Obj: TObject;
        FActiveStorageObj: TObject;
        FActiveStorage2Obj: TObject;
        FActiveUPFCObj: TObject;
        FActiveVCCSObj: TObject;
        FActiveVSConverterObj: TObject;
        FActiveVsourceObj: TObject;
        FActiveAutoTransObj: TObject;
        FActiveCapacitorObj: TObject;
        FActiveFaultObj: TObject;
        FActiveFuseObj: TObject;
        FActiveGICTransformerObj: TObject;
        FActiveLineObj: TObject;
        FActiveReactorObj: TObject;
        FActiveTransfObj: TObject;
    
        FDSSExecutive: TObject;
        FCIMExporter: TObject;
    
        FActiveCircuit: TNamedObject;
        FActiveDSSObject :TNamedObject;
{$IFDEF DSS_CAPI_PM}
        FActorThread: TThread; //TODO: Currently only for solution, extend later (send redirect command to the other thread, etc.)
{$ENDIF}

        CurrentDSSDir_internal: String;
        FSolutionAbort: LongInt; // changed to LongInt to enable InterLockedIncrement and others

        function get_SolutionAbort(): Boolean;
        procedure set_SolutionAbort(val: Boolean);
    public
        Parent: TDSSContext;
    
        DSSPlotCallback: dss_callback_plot_t;
        DSSMessageCallback: dss_callback_message_t;
    
        // Parallel Machine state
{$IFDEF DSS_CAPI_PM}
        Children: array of TDSSContext;
        ActiveChild: TDSSContext;
        ActiveChildIndex: Integer;
        CPU: Integer;

        IsSolveAll: Boolean;
        AllActors: Boolean;
        Parallel_enabled: Boolean;
        ConcatenateReports: Boolean;
        ConcatenateReportsLock: TCriticalSection;
        ActorPctProgress: Integer;
        ActorStatus: TActorStatus;
        ThreadStatusEvent: TEvent;
{$ENDIF}
        _Name: String;
    
        // C-API pointer data (GR mode)
        GR_DataPtr_PPAnsiChar: PPAnsiChar;
        GR_DataPtr_PDouble: PDouble;
        GR_DataPtr_PInteger: PInteger;
        GR_DataPtr_PByte: PByte;

        GR_Counts_PPAnsiChar: Array[0..1] of TAPISize;
        GR_Counts_PDouble: Array[0..1] of TAPISize;
        GR_Counts_PInteger: Array[0..1] of TAPISize;
        GR_Counts_PByte: Array[0..1] of TAPISize;

        // Original global state
        ClassNames: TClassNamesHashListType;
        DSSClassList    :TDSSPointerList; // pointers to the base class types
        Circuits        :TDSSPointerList;
        DSSObjs         :TDSSPointerList;

        NumIntrinsicClasses,
        NumUserClasses: Integer;

        ActiveDSSClass: TDSSClass;
        AuxParser: TDSSParser;  // Auxiliary parser for use by anybody for reparsing values
        PropParser: TDSSParser;  // Parser dedicated for parsing in SetObjPropertyValue
        Parser: TDSSParser;
        ParserVars: TParserVar;

        LastClassReferenced:Integer;  // index of class of last thing edited
        NumCircuits     :Integer;
        MaxAllocationIterations :Integer;
        ErrorPending       :Boolean;
        CmdResult,
        ErrorNumber        :Integer;
        LastErrorMessage   :String;
        DefaultEarthModel  :Integer;
        ActiveEarthModel   :Integer;
        LastFileCompiled   :String;
        LastCommandWasCompile :Boolean;
        InShowResults      :Boolean;
        Redirect_Abort     :Boolean;
        In_Redirect        :Boolean;
        DIFilesAreOpen     :Boolean;
        AutoShowExport     :Boolean;
        SolutionWasAttempted :Boolean;

        GlobalHelpString   :String;
        GlobalPropertyValue:String;
        GlobalResult       :String;
        LastResultFile     :String;

        LogQueries         :Boolean;
        QueryFirstTime     :Boolean;
        QueryLogFileName   :String;
        QueryLogFile       :TFileStream;

        DataDirectory    :String;     // used to be DSSDataDirectory
        OutputDirectory  :String;     // output files go here, same as DataDirectory if writable
        CircuitName_     :String;     // Name of Circuit with a "_" appended

        DefaultBaseFreq  :Double;
        DaisySize        :Double;
        
        EventStrings: TStringList;
        SavedFileList:TStringList;
        ErrorStrings: TStringList;

        IncMat_Ordered     : Boolean;

        //***********************Seasonal QSTS variables********************************
        SeasonalRating         : Boolean;    // Tells the energy meter if the seasonal rating feature is active
        SeasonSignal           : String;     // Stores the name of the signal for selecting the rating dynamically

        LastCmdLine: String;   // always has last command processed
        RedirFile: String;
        
        IsPrime: Boolean; // Indicates whether this instance is the first/main DSS instance

        // For external APIs
        FPropIndex: Integer;  
        FPropClass: TDSSClass;
        
        // Previously C-API or COM globals
        tempBuffer: AnsiString; // CAPI_Utils.pas
        ComParser: TDSSParser; // CAPI_Parser.pas
        ReduceEditString: String; // CAPI_ReduceCkt.pas
        EnergyMeterName: String; // CAPI_ReduceCkt.pas
        FirstPDelement: String;  // Full name -- CAPI_ReduceCkt.pas
        FControlProxyObj: TObject; // CAPI_CtrlQueue.pas
        ActiveAction: pAction; // CAPI_CtrlQueue.pas
        
        Enums: TObjectList;
        UnitsEnum, ScanTypeEnum, SequenceEnum, ConnectionEnum, LeadLagEnum, CoreTypeEnum,
        LineTypeEnum, EarthModelEnum, DefaultLoadModelEnum, RandomModeEnum, ControlModeEnum,
        SolveModeEnum, SolveAlgEnum, CktModelEnum, AddTypeEnum, LoadShapeClassEnum, MonPhaseEnum: TDSSENum;

        // ZIP file state
        unzipper: TObject;
        inZipPath: String;

        constructor Create(_Parent: TDSSContext = nil; _IsPrime: Boolean = False);
        destructor Destroy; override;
        function GetPrime(): TDSSContext;
        function CurrentDSSDir(): String;
        procedure SetCurrentDSSDir(dir: String);
        property SolutionAbort: Boolean READ get_SolutionAbort WRITE set_SolutionAbort;
        function GetROFileStream(fn: String): TStream;
        procedure NewDSSClass(Value: Pointer);
    End;

VAR
    DSSPrime: TDSSContext;

implementation

USES 
    DSSGlobals, 
    SysUtils, 
    DSSObject, 
    CktElement, 
    DSSHelper, 
    DSSObjectHelper, 
    Executive,
    ExecHelper,
    ControlProxy, 
    Utilities, 
    ExportCIMXML,
    TypInfo,
    StrUtils,
    Math,
    Transformer,
    LineUnits,
    Load,
    uCMatrix,
    Dynamics,
    BufStream,
    Solution;

type
    TProp = TDSSObjectProp;
    PBoolean = ^Boolean;
    PPDouble = ^PDouble;
    PPString= ^PString;
    PPByte = ^PByte;
    TDSSObjectPtrPtr = ^TDSSObjectPtr;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;

function TDSSContext.GetROFileStream(fn: String): TStream;
begin
    if DSSExecutive.InZip then
    begin
        Result := DSSExecutive.GetZipStream(fn);
        Exit;
    end;
    fn := AdjustInputFilePath(self, fn);
    Result := TBufferedFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
end;

function TDSSContext.GetPrime(): TDSSContext;
begin
    if IsPrime or (Parent = nil) then 
        Result := self
    else
        Result := Parent.GetPrime();
end;

function TDSSContext.get_SolutionAbort(): Boolean;
begin
    Result := FSolutionAbort <> 0;
end;

procedure TDSSContext.set_SolutionAbort(val: Boolean);
begin
{$IFDEF DSS_CAPI_PM}
    if val then
        InterlockedExchange(FSolutionAbort, 1)
    else
        InterlockedExchange(FSolutionAbort, 0);
{$ELSE}
    if val then
        FSolutionAbort := 1
    else
        FSolutionAbort := 0;
{$ENDIF}
end;

constructor TDSSContext.Create(_Parent: TDSSContext; _IsPrime: Boolean);
begin
    inherited Create;

    Parent := _Parent;
    IsPrime := _IsPrime;
    if IsPrime and (DSSMessages = NIL) then
    begin
        try
            DSSMessages := TMOFile.Create('locale/messages.mo');
        except
            DSSMessages := NIL;
        end;

        try
            DSSPropertyHelp := TMOFile.Create('locale/en_US.mo');
        except
            DSSPropertyHelp := NIL;
        end;
    end;

    Enums := TObjectList.Create();

    // Populate enum info
    EarthModelEnum := TDSSEnum.Create('Earth Model', True, 1, 1,
        ['Carson', 'FullCarson', 'Deri'], [1, 2, 3]);
    EarthModelEnum.DefaultValue := 1;
    Enums.Add(EarthModelEnum);

    LineTypeEnum := TDSSEnum.Create('Line Type', True, 2, 4,
        ['oh', 'ug', 'ug_ts', 'ug_cn', 'swt_ldbrk', 'swt_fuse', 'swt_sect', 'swt_rec', 'swt_disc', 'swt_brk', 'swt_elbow'],
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]);
    LineTypeEnum.DefaultValue := 1;
    Enums.Add(LineTypeEnum);

    UnitsEnum := TDSSEnum.Create('Dimension Units', True, 1, 2, 
        ['none', 'mi', 'kft', 'km', 'm', 'ft', 'in', 'cm', 'mm', 'meter', 'miles'],
        [0, 1, 2, 3, 4, 5, 6, 7, 8, 4, 1]);
    UnitsEnum.DefaultValue := 0;
    Enums.Add(UnitsEnum);

    ScanTypeEnum := TDSSEnum.Create('Scan Type', True, 1, 1, ['None', 'Zero', 'Positive'], [-1, 0, 1]);
    Enums.Add(ScanTypeEnum);

    SequenceEnum := TDSSEnum.Create('Sequence Type', True, 1, 1, ['Negative', 'Zero', 'Positive'], [-1, 0, 1]);
    Enums.Add(SequenceEnum);

    ConnectionEnum := TDSSEnum.Create('Connection', True, 1, 2,
        ['wye', 'delta', 'y', 'ln', 'll'],
        [0, 1, 0, 0, 1]);
    Enums.Add(ConnectionEnum);

    CoreTypeEnum := TDSSEnum.Create('Core Type', False, 1, 1,
        ['shell', '1-phase', '3-leg', '4-leg', '5-leg', 'core-1-phase'],
        [0, 1, 3, 4, 5, 9]);
    Enums.Add(CoreTypeEnum);

    LeadLagEnum := TDSSEnum.Create('Phase Sequence', True, 1, 1,
        ['Lag', 'Lead', 'ANSI', 'Euro'],
        [0, 1, 0, 1]);
    Enums.Add(LeadLagEnum);

    DefaultLoadModelEnum := TDSSEnum.Create('Load Solution Model', True, 1, 1,
        ['PowerFlow', 'Admittance'],
        [POWERFLOW, ADMITTANCE]);
    DefaultLoadModelEnum.DefaultValue := ADMITTANCE;
    Enums.Add(DefaultLoadModelEnum);

    RandomModeEnum := TDSSEnum.Create('Random Type', True, 1, 1,
        ['None', 'Gaussian', 'Uniform', 'LogNormal'],
        [0, GAUSSIAN, UNIFORM, LOGNORMAL]);
    RandomModeEnum.DefaultValue := 0;
    Enums.Add(RandomModeEnum);

    ControlModeEnum := TDSSEnum.Create('Control Mode', True, 1, 1,
        ['Off', 'Static', 'Event', 'Time', 'MultiRate'],
        [CONTROLSOFF, CTRLSTATIC, EVENTDRIVEN, TIMEDRIVEN, MULTIRATE]);
    ControlModeEnum.DefaultValue := CTRLSTATIC;
    Enums.Add(ControlModeEnum);

    SolveModeEnum := TDSSEnum.Create('Solution Mode', True, 1, 9,
        ['Snap', 'Daily', 'Yearly', 'M1', 'LD1', 'PeakDay', 'DutyCycle', 'Direct', 'MF', 'FaultStudy', 'M2', 'M3', 'LD2', 'AutoAdd', 'Dynamic', 'Harmonic', 'Time', 'HarmonicT', 'Snapshot'],
        [Ord(TSolveMode.SNAPSHOT), Ord(TSolveMode.DAILYMODE), Ord(TSolveMode.YEARLYMODE), Ord(TSolveMode.MONTECARLO1), Ord(TSolveMode.LOADDURATION1), Ord(TSolveMode.PEAKDAY), Ord(TSolveMode.DUTYCYCLE), Ord(TSolveMode.DIRECT), Ord(TSolveMode.MONTEFAULT), Ord(TSolveMode.FAULTSTUDY), Ord(TSolveMode.MONTECARLO2), Ord(TSolveMode.MONTECARLO3), Ord(TSolveMode.LOADDURATION2), Ord(TSolveMode.AUTOADDFLAG), Ord(TSolveMode.DYNAMICMODE), Ord(TSolveMode.HARMONICMODE), Ord(TSolveMode.GENERALTIME), Ord(TSolveMode.HARMONICMODET), Ord(TSolveMode.SNAPSHOT)]);
    SolveModeEnum.DefaultValue := Ord(TSolveMode.SNAPSHOT);
    SolveModeEnum.UseFirstFound := True; // Some example/test files use just "Harm", which is ambiguous
    Enums.Add(SolveModeEnum);

    SolveAlgEnum := TDSSEnum.Create('Solution Algorithm', True, 2, 2,
        ['Normal', 'Newton'],
        [NORMALSOLVE, NEWTONSOLVE]);
    SolveAlgEnum.DefaultValue := Ord(NORMALSOLVE);
    Enums.Add(SolveAlgEnum);

    CktModelEnum := TDSSEnum.Create('Circuit Model', True, 1, 1,
        ['Multiphase', 'Positive'],
        [Integer(False), Integer(True)]);
    CktModelEnum.DefaultValue := Integer(False);
    Enums.Add(CktModelEnum);

    AddTypeEnum := TDSSEnum.Create('AutoAdd Device Type', True, 1, 1,
        ['Generator', 'Capacitor'],
        [GENADD, CAPADD]);
    AddTypeEnum.DefaultValue := CAPADD;
    Enums.Add(AddTypeEnum);

    LoadShapeClassEnum := TDSSEnum.Create('Load Shape Class', True, 1, 2,
        ['None', 'Daily', 'Yearly', 'Duty'],
        [USENONE, USEDAILY, USEYEARLY, USEDUTY]);
    LoadShapeClassEnum.DefaultValue := USENONE;
    Enums.Add(LoadShapeClassEnum);

    MonPhaseEnum := TDSSEnum.Create('Monitored Phase', True, 1, 2, 
        ['min', 'max', 'avg'], [-3, -2, -1]);
    MonPhaseEnum.Hybrid := True;
    Enums.Add(MonPhaseEnum);

    // GR (global result) counters: Initialize to zero
    FillByte(GR_Counts_PPAnsiChar, sizeof(TAPISize) * 2, 0);
    FillByte(GR_Counts_PDouble, sizeof(TAPISize) * 2, 0);
    FillByte(GR_Counts_PInteger, sizeof(TAPISize) * 2, 0);
    FillByte(GR_Counts_PByte, sizeof(TAPISize) * 2, 0);
    
    DSSPlotCallback := nil;
    DSSMessageCallback := nil;

    ClassNames := NIL;
    DSSClassList := NIL;
    Circuits := NIL;
    DSSObjs := NIL;

{$IFDEF DSS_CAPI_PM}
    ActorStatus := TActorStatus.Idle;
    ThreadStatusEvent := nil;

    ActiveChildIndex := 0;
    Children := nil;
    
    IsSolveAll := False;
    AllActors := False;
    ConcatenateReports := False;
    ConcatenateReportsLock := TCriticalSection.Create();
    Parallel_enabled := False;
    ActorPctProgress := 0;

    if IsPrime then
    begin
        SetLength(Children, 1);
        Children[0] := Self;
        ActiveChild := Self;
        _Name := '_1';
    end
    else
    begin
        ActiveChild := Self;
        _Name := '_';
    end;
    CPU := -1; // left at -1 = doesn't change affinity
{$ELSE}
    _Name := '';
{$ENDIF} // DSS_CAPI_PM

    
    LastCmdLine := '';
    RedirFile := '';

    // Use the current working directory as the initial datapath when using DSS_CAPI
    SetDataPath(self, StartupDirectory);
    
    ParserVars := TParserVar.Create(100);  // start with space for 100 variables
    Parser := TDSSParser.Create;
    PropParser := TDSSParser.Create;
    AuxParser := TDSSParser.Create;
    
    // Share parser variables
    Parser.SetVars(ParserVars);
    AuxParser.SetVars(ParserVars);
    PropParser.SetVars(ParserVars);
    
    SeasonalRating         :=  False;
    SeasonSignal           :=  '';
    
    CmdResult             := 0;
    DIFilesAreOpen        := FALSE;
    ErrorNumber           := 0;
    ErrorPending          := FALSE;
    GlobalHelpString      := '';
    GlobalPropertyValue   := '';
    LastResultFile        := '';
    In_Redirect           := FALSE;
    InShowResults         := FALSE;
    LastCommandWasCompile := FALSE;
    LastErrorMessage      := '';
    MaxAllocationIterations := 2;
    FSolutionAbort := 0;
    AutoShowExport        := FALSE;
    SolutionWasAttempted  := FALSE;

    DefaultBaseFreq       := GlobalDefaultBaseFreq;
    DaisySize             := 1.0;
    DefaultEarthModel     := DERI;
    ActiveEarthModel      := DefaultEarthModel;

    LogQueries       := FALSE;
    QueryLogFileName := '';   
    QueryLogFile := nil;

    EventStrings     := TStringList.Create;
    SavedFileList    := TStringList.Create;
    ErrorStrings     := TStringList.Create;
    // ErrorStrings.Clear;
    
    FPropIndex := 0;
    FPropClass := NIL;
    
    // From ReduceCkt interface initialization
    ReduceEditString := ''; // Init to null string
    EnergyMeterName := '';
    FirstPDelement := '';
    
    ComParser := ParserDel.TDSSParser.Create;  // create COM Parser object
    ActiveAction := NIL;
    FControlProxyObj := TControlProxyObj.Create(self);
    
    DSSExecutive := TExecutive.Create(self);
    DSSExecutive.CreateDefaultDSSItems;
    
    CIMExporter := TCIMExporter.Create(self);

    unzipper := NIL;
end;

destructor TDSSContext.Destroy;
var
    i: Integer;
begin
    if unzipper <> NIL then
        unzipper.Free;

    CIMExporter.Free;

    DSSExecutive.Clear(False);
    DSSExecutive.Free;
    
    if FControlProxyObj <> nil then
        TControlProxyObj(FControlProxyObj).Free;

    // No need to free ActiveAction, it only points to the action
    PropParser.Free;
    AuxParser.Free;
    EventStrings.Free;
    SavedFileList.Free;
    ErrorStrings.Free;
    ParserVars.Free;
    Parser.Free;
    ComParser.Free;

    Enums.Free;

    if IsPrime then
    begin
        FreeAndNil(DSSMessages);
        FreeAndNil(DSSPropertyHelp);
    end;
{$IFDEF DSS_CAPI_PM}
    ConcatenateReportsLock.Free();
{$ENDIF}
    inherited Destroy;
end;

function TDSSContext.CurrentDSSDir(): String;
begin
    if DSS_CAPI_ALLOW_CHANGE_DIR then
    begin
        Result := GetCurrentDir();
        If Result[Length(Result)] <> PathDelim Then 
            Result := Result + PathDelim;
    end
    else
    begin
        Result := CurrentDSSDir_internal
    end;
end;

procedure TDSSContext.SetCurrentDSSDir(dir: String);
begin
    if DSS_CAPI_ALLOW_CHANGE_DIR then
    begin
        SetCurrentDir(dir);
        Exit;
    end;

    If dir[Length(dir)] <> PathDelim Then 
        CurrentDSSDir_internal := dir + PathDelim
    else
        CurrentDSSDir_internal := dir;
end;

procedure TDSSContext.NewDSSClass(Value:Pointer);
begin
    DSSClassList.Add(Value); // Add to pointer list
    ActiveDSSClass := Value;   // Declare to be active
    ClassNames.Add(ActiveDSSClass.Name); // Add to classname list
end;

Constructor TDSSClass.Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
BEGIN
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    Inherited Create;

    DSSClassType := DSSClsType;
    ClassParents := TStringList.Create(); // for easier property help with inheritance
    Class_Name := DSSClsName;
    ClassParents.Add('DSSClass');
    DSS := dssContext;
    ElementList := TDSSPointerList.Create(20);  // Init size and increment
    PropertyName := nil;
    PropertyScale := nil;
    PropertyValueOffset := nil;
    PropertyTrapZero := nil;
    PropertyInverse := nil;
    PropertyType := nil;
    PropertyOffset := nil;
    PropertyOffset2 := nil;
    PropertyOffset3 := nil;
    PropertyWriteFunction := nil;
    PropertyReadFunction := nil;
    // PropertyStep := nil;
    PropertyStructArrayIndexOffset := 0;
    PropertyStructArrayIndexOffset2 := 0;

    ActiveElement := 0;
    ActiveProperty := 0;

    ElementNameList := THashListType.Create(100);
    ElementNamesOutOfSynch := FALSE;

    DefineProperties();
end;

destructor TDSSClass.Destroy;
var
   i: Integer;
   obj: TDSSObject;
begin
    // if ElementList <> NIL then
    // begin
    //     for i := 1 to ElementList.Count do
    //     begin
    //         obj := ElementList.At(i);
    //         obj.Free();
    //     end;
    //     ElementList.Clear();
    // end;

    // Get rid of space occupied by strings
    for i := 1 to NumProperties do
        PropertyName[i] := '';

    Reallocmem(PropertyName, 0);
    Reallocmem(PropertyScale, 0);
    Reallocmem(PropertyValueOffset, 0);
    Reallocmem(PropertyType, 0);
    Reallocmem(PropertyOffset, 0);
    Reallocmem(PropertyOffset2, 0);
    Reallocmem(PropertyOffset3, 0);
    Reallocmem(PropertyReadFunction, 0);
    Reallocmem(PropertyWriteFunction, 0);
    Reallocmem(PropertyTrapZero, 0);
    Reallocmem(PropertyInverse, 0);
    SetLength(PropertyFlags, 0);

    ElementList.Free;
    ElementNameList.Free;
    CommandList.Free;
    ClassParents.Free;
    Inherited Destroy;
end;

function TDSSClass.NewObject(const ObjName: String; Activate: Boolean): Pointer;
begin
    Result := NIL;
    DoErrorMsg(Format('Reached base class of TDSSClass for device "%s"', [ObjName]),
        'N/A',
        'Should be overridden.', 780);
end;

function TDSSClass.NewObject(const ObjName: String; Activate: Boolean; out Idx: Integer): Pointer;
begin
    Result := NewObject(ObjName, Activate);
    Idx := ElementList.Count;
end;

Procedure TDSSClass.Set_Active(value:Integer);
BEGIN
    If (Value > 0) and (Value<= ElementList.Count) THEN
    Begin
        ActiveElement := Value;
        DSS.ActiveDSSObject := ElementList.Get(ActiveElement);
        // Make sure Active Ckt Element agrees if is a ckt element
        // So COM interface will work
        if DSS.ActiveDSSObject is TDSSCktElement then
            ActiveCircuit.ActiveCktElement := TDSSCktElement(DSS.ActiveDSSObject);
    End;
END;

function TDSSClass.BeginEdit(ptr: Pointer; SetActive: Boolean): Pointer;
type
    TObj = TDSSObject;
var
    Obj: TObj;
begin
    Result := NIL;
    if ptr <> NIL then
        Obj := TObj(ptr)
    else
        Obj := ElementList.Active;

    if (Obj <> NIL) and (Flg.EditionActive in Obj.Flags) then
    begin
        DosimpleMsg('%s: Object already being edited!', [Obj.FullName], 37737);
        Exit;
    end;

    if SetActive then
    begin
        //TODO: e.g. DSS.ActiveConductorDataObj := Obj; -- if ever required later
        DSS.ActiveDSSObject := Obj;
    end;

    Include(Obj.Flags, Flg.EditionActive);
    Result := Obj;
end;

function TDSSClass.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    Exclude(TDSSObject(ptr).Flags, Flg.EditionActive);
    Result := True;
end;

Function TDSSClass.Edit(Parser: TDSSParser): Integer;
var
    ParamPointer: Integer;
    ParamName, Param: String;
    Obj: TDSSObject;
    prevInt: Integer;
begin
    Result := 0;

    // Get the target object and initialize the edition
    Obj := TDSSObject(BeginEdit(NIL, True));

    //TODO: error handling? if Obj = NIL then...

    // Previous Edit loop
    ParamPointer := 0;
    ParamName := Parser.NextParam;
    Param := Parser.StrValue;
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            ParamPointer := CommandList.GetCommand(ParamName);

        if (ParamPointer <= 0) or (ParamPointer > NumProperties) then
        begin
            if Length(ParamName) > 0 then
                DoSimpleMsg('Unknown parameter "%s" (value "%s") for object "%s"', [ParamName, Param, TDSSObject(Obj).FullName], 110)
            else
                DoSimpleMsg('Unknown parameter for value "%s" in object "%s"', [Param, TDSSObject(Obj).FullName], 110);

            if DSS_CAPI_EARLY_ABORT then
            begin
                Result := -1;
                Exit;
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
            continue;
        end;

        Inc(Result);

        if not ParseObjPropertyValue(Obj, ParamPointer, Param, prevInt) then
        begin
            if DSS_CAPI_EARLY_ABORT then
            begin
                Result := -1;
                Exit;
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
            continue;
        end;
        
        Obj.SetAsNextSeq(ParamPointer);
        Obj.PropertySideEffects(ParamPointer, prevInt);

//            GetObjPropertyValue(Obj, ParamPointer, tmp);
//            WriteLn(TDSSObject(Obj).FullName, '.', PropertyName[ParamPointer], ' = ', tmp);

        ParamName := Parser.NextParam;
        Param := Parser.StrValue;
    end;

    // Finalize it
    EndEdit(Obj, Result);
end;

function TDSSClass.AddObjectToList(Obj:Pointer; Activate: Boolean): Integer;
begin
    ElementList.Add(Obj); // Stuff it in this collection's element list
    ElementNameList.Add(TDSSObject(Obj).Name);
{$IFNDEF DSS_CAPI_HASHLIST}
    If Cardinal(ElementList.Count) > 2 * ElementNameList.InitialAllocation Then ReallocateElementNameList;
{$ENDIF}
    if Activate then
    begin
        ActiveElement := ElementList.Count;
        Result := ActiveElement; // Return index of object in list
    end
    else
        Result := ElementList.Count;
end;

Function TDSSClass.SetActive(const ObjName:String): Boolean;
var
    idx: Integer;
begin
    Result := False;
    // Faster to look in hash list 7/7/03
    If ElementNamesOutOfSynch Then ResynchElementNameList;
    idx := ElementNameList.Find(ObjName);
    
    if idx>0 then
    begin
        ActiveElement := idx;
        DSS.ActiveDSSObject := ElementList.get(idx);
        Result := TRUE;
    End;
end;

Function TDSSClass.Find(const ObjName: String; const ChangeActive: Boolean): Pointer;
VAR
    idx: Integer;
BEGIN
    Result := Nil;
    If ElementNamesOutOfSynch Then ResynchElementNameList;
    // Faster to look in hash list 7/7/03
    idx := ElementNameList.Find(ObjName);
    
    If idx>0 Then
    Begin
        Result := ElementList.Get(idx);
        if ChangeActive then 
            ActiveElement := idx;
    End;
END;

Function TDSSClass.GetActiveObj:Pointer; // Get address of active obj of this class
BEGIN
    ActiveElement := ElementList.ActiveIndex;
    Result := ElementList.Active;
END;

Function TDSSClass.Get_FirstPropertyName:String;
BEGIN
    ActiveProperty := 0;
    Result := Get_NextPropertyName;
END;

Function TDSSClass.Get_NextPropertyName:String;
BEGIN
    Inc(ActiveProperty);
    IF ActiveProperty<=NumProperties THEN
        Result := PropertyName^[ActiveProperty]
    ELSE 
        Result := '';
END;

Function TDSSClass.PropertyIndex(Const Prop:String):Integer;
// find property value by string
var
    i: Integer;
begin
    Result := 0;  // Default result if not found
    For i := 1 to NumProperties DO 
    BEGIN
        IF CompareText(Prop, PropertyName[i])=0 THEN 
        BEGIN
            Result := i;
            Break;
        END;
    END;
END;

Procedure TDSSClass.CountPropertiesAndAllocate;
var 
    i: Integer;
begin
    NumProperties := NumProperties + 1;

    PropertyName := Allocmem(SizeOf(String) * NumProperties);
    PropertyScale := Allocmem(SizeOf(Double) * NumProperties);
    PropertyValueOffset := Allocmem(SizeOf(Double) * NumProperties);
    PropertyTrapZero := Allocmem(SizeOf(Double) * NumProperties);
    PropertyInverse := Allocmem(SizeOf(Boolean) * NumProperties);
    PropertyType := Allocmem(SizeOf(TPropertyType) * NumProperties);
    PropertyOffset := Allocmem(SizeOf(PtrInt) * NumProperties);
    PropertyOffset2 := Allocmem(SizeOf(PtrInt) * NumProperties);
    PropertyOffset3 := Allocmem(SizeOf(PtrInt) * NumProperties);
    PropertyReadFunction := Allocmem(SizeOf(Pointer) * NumProperties);
    PropertyWriteFunction := Allocmem(SizeOf(Pointer) * NumProperties);

    SetLength(PropertyFlags, NumProperties + 1);
    
    for i := 1 to NumProperties do
    begin
        // This defaults all properties to simple doubles, 
        // but offset still needs to be set later
        PropertyType[i] := TPropertyType.DoubleProperty;
        PropertyScale[i] := 1;
        PropertyValueOffset[i] := 0;
        PropertyTrapZero[i] := 0;
        PropertyInverse[i] := False;
        PropertyOffset[i] := -1;
        PropertyOffset2[i] := 0;
        PropertyOffset3[i] := 0;
        PropertyFlags[i] := [];
        // PropertyStep[i] := -1;
        PropertyReadFunction[i] := NIL;
        PropertyWriteFunction[i] := NIL;
    end;

    ActiveProperty := 0;    // initialize for AddPropert
End;

Procedure TDSSClass.DefineProperties;
Begin
    PopulatePropertyNames(ActiveProperty, NumPropsThisClass, PropInfo, False);

    PropertyType[ActiveProperty + ord(TProp.Like)] := TPropertyType.MakeLikeProperty;
    PropertyOffset[ActiveProperty + ord(TProp.Like)] := 1; // dummy value

    ActiveProperty := ActiveProperty + NumPropsThisClass;

    CommandList := TCommandList.Create(SliceProps(PropertyName, NumProperties), True);
End;

function TDSSClass.Get_ElementCount: Integer;
begin
    Result := ElementList.Count;
end;

function TDSSClass.Get_First: Integer;
begin
    IF ElementList.Count=0   THEN Result := 0

    ELSE Begin
        ActiveElement := 1;
        DSS.ActiveDSSObject := ElementList.First;
        // Make sure Active Ckt Element agrees if is a ckt element
        // So COM interface will work
        if DSS.ActiveDSSObject is TDSSCktElement then
            ActiveCircuit.ActiveCktElement := TDSSCktElement(DSS.ActiveDSSObject);
        Result := ActiveElement;
    End;
end;

function TDSSClass.Get_Next: Integer;
begin
    Inc(ActiveElement);
    IF ActiveElement > ElementList.Count THEN 
        Result := 0
    ELSE 
    Begin
        DSS.ActiveDSSObject := ElementList.Next;
        // Make sure Active Ckt Element agrees if is a ckt element
        // So COM interface will work
        if DSS.ActiveDSSObject is TDSSCktElement then
            ActiveCircuit.ActiveCktElement := TDSSCktElement(DSS.ActiveDSSObject);
        Result := ActiveElement;
    End;
end;

procedure TDSSClass.ReallocateElementNameList;
Var
    i: Integer;
begin
    // Reallocate the device name list to improve the performance of searches
    ElementNameList.Free; // Throw away the old one.
    ElementNameList := THashListType.Create(2*ElementList.Count); // make a new one

    // Do this using the Names of the Elements rather than the old list because it might be
    // messed up if an element gets renamed

    For i := 1 to ElementList.Count Do ElementNameList.Add(TDSSObject(ElementList.Get(i)).Name);
end;

procedure TDSSClass.ResynchElementNameList;
begin
    ReallocateElementNameList;
    ElementNamesOutOfSynch := False;
end;

procedure TDSSClass.PopulatePropertyNames(PropOffset: Integer; NumProps: Integer; EnumInfo: Pointer; ReplacePct: Boolean = True);
var
    i: Integer;
    propName: String;
begin
    for i := 1 to NumProps do
    begin
        propName := GetEnumName(EnumInfo, i);
        if LeftStr(propName, 2) = '__' then
            propName := Copy(propName, 3, Length(propName));

        if ReplacePct then
            propName := ReplaceStr(propName, 'pct', '%');

        propName := ReplaceStr(propName, '__', '-');

        if propName = 'cls' then
            propName := 'class'
        else if LowerCase(propName) = 'typ' then
            propName := propName + 'e';
            
        PropertyName[PropOffset + i] := propName;
    end;
end;

procedure TDSSClass.DoErrorMsg(Const S, Emsg, ProbCause: String; ErrNum: Integer);inline;
begin
    DSSGlobals.DoErrorMsg(DSS, S, Emsg, ProbCause, ErrNum)
end;

procedure TDSSClass.DoSimpleMsg(Const S: String; ErrNum:Integer);inline;
begin
    DSSGlobals.DoSimpleMsg(DSS, S, ErrNum)
end;

procedure TDSSClass.DoSimpleMsg(Const S: String; fmtArgs: Array of Const; ErrNum:Integer);inline;
begin
    DSSGlobals.DoSimpleMsg(DSS, DSSTranslate(S), fmtArgs, ErrNum)
end;

function TDSSClass.GetPropertyHelp(idx: Integer): String;
var
    altkey, key: String;
    i: Integer;
begin
    if (idx <= 0) or (idx > NumProperties) then
    begin
        Result := 'INVALID_PROPERTY';
        Exit;
    end;

    key := Class_Name + '.' + PropertyName[idx];

    if DSSPropertyHelp = NIL then
    begin
        // Catalog is not loaded
        Result := key;
        Exit;
    end;

    Result := DSSHelp(key);
    if Result <> key then
        Exit; // Found a string

    // Try parents
    for i := ClassParents.Count downto 1 do
    begin
        altkey := ClassParents.Strings[i - 1] + '.' + PropertyName[idx];
        Result := DSSHelp(altkey);
        if Result <> altkey then
            Exit; // Found a string
    end;

    // Nothing found
    Result := key;
end;

constructor TDSSEnum.Create(EnumName: String; IsSequential: Boolean; MinCh, MaxCh: Integer; EnumNames: Array of String; EnumOrds: Array of Integer);
var
    i: Integer;
    n: Integer;
begin
    inherited Create;

    n := Length(EnumNames);
    Name := EnumName;
    Names := NIL;
    LowerNames := NIL;
    Ordinals := NIL;
   
    SetLength(Names, n);
    SetLength(LowerNames, n);
    for i := 0 to n - 1 do
    begin
        Names[i] := EnumNames[i];
        LowerNames[i] := LowerCase(EnumNames[i]);
    end;

    if High(EnumNames) <> High(EnumOrds) then
        raise Exception.Create(Format('Could not initialize enum ("%s").', [EnumName]));

    SetLength(Ordinals, n);
    for i := 0 to n - 1 do
        Ordinals[i] := EnumOrds[i];

    Sequential := IsSequential;

    //TODO: fill these automatically
    MinChars := MinCh;
    MaxChars := MaxCh;

    DefaultValue := -9999999;
    AllowLonger := False;
    UseFirstFound := False;
    Hybrid := False;

    MinOrdinal := 9999999;
    MaxOrdinal := -9999999;
    for i := 0 to High(Ordinals) do
    begin
        MinOrdinal := Min(MinOrdinal, Ordinals[i]);
        MaxOrdinal := Max(MaxOrdinal, Ordinals[i]);
    end;
end;

destructor TDSSEnum.Destroy;
begin
    SetLength(Names, 0);
    SetLength(LowerNames, 0);
    SetLength(Ordinals, 0);
    inherited Destroy;
end;

function TDSSEnum.OrdinalToString(Value: Integer): String;
var
    i: Integer;
begin
    if (Value < MinOrdinal) or (Value > MaxOrdinal) then
    begin
        Result := ''; //TODO: error? Usually on purpose though, may need a flag
        Exit;
    end;

    if Sequential then
    begin
        Result := Names[Value - MinOrdinal];
        Exit;
    end;

    for i := 0 to High(Ordinals) do
        if Ordinals[i] = Value then
        begin
            Result := Names[i];
            Exit;
        end;

    if not Hybrid then
    begin
        Result := ''; //TODO: error?
        Exit;
    end;

    Result := IntToStr(Value);
end;

function TDSSEnum.IsOrdinalValid(Value: Integer): Boolean;
var 
    i: Integer;
begin
    if Hybrid and (Value > 0) then
    begin
        Result := True;
        Exit;
    end;

    for i := 0 to High(Ordinals) do
        if Ordinals[i] = Value then
        begin
            Result := True;
            Exit;
        end;
    Result := False;
end;

function TDSSEnum.StringToOrdinal(Value: String): Integer; // Naive version for testing
var
    i: Integer;
    minch, nch: Integer;
    found: Integer;
    s: String;
    errCode: Word;
begin
    if (MinChars <> 0) and (MinChars > Length(Value)) then
    begin
        Result := DefaultValue;
        if DefaultValue = -9999999 then
            raise Exception.Create(Format('Could not match enum ("%s") value "%s"', [Name, Value]));
        Exit;
        //TODO: error
    end;

    minch := Max(1, MinChars);
    for nch := minch to Min(Length(Value), MaxChars) do
    begin
        found := 0;
        s := Copy(Value, 1, nch);
        for i := 0 to High(LowerNames) do
        begin
            if (not AllowLonger) and (Length(LowerNames[i]) < length(Value)) then
                continue;

            if (nch = minch) and (Value = LowerNames[i]) then
            begin
                Result := Ordinals[i];
                Exit;
            end;

            if CompareTextShortest(s, LowerNames[i]) = 0 then
            begin
                Result := Ordinals[i];

                if (nch = Length(Value)) and (UseFirstFound) then
                    Exit;

                Inc(found);
                if found > 1 then
                    break;
            end;
        end;
        
        if found = 1 then
            exit; // Found the match, can exit safely
    end;

    if Hybrid then
    begin
        Val(Value, Result, errCode);
        if errCode <> 0 then
            raise EParserProblem.Create(Format('Integer number conversion error for string: "%s"', [Value]));

        Result := Max(1, Result);
        Exit;
    end;

    // TODO: Error handling or do nothing
    if DefaultValue = -9999999 then
       raise Exception.Create(Format('Could not match enum ("%s") value "%s"', [Name, Value]));
    Result := DefaultValue;    
end;

constructor TProxyClass.Create(dssContext: TDSSContext; Targets: Array Of String);
var
    s: String;
    i: Integer;
begin
    TargetClasses := NIL;
    s := '(';

    // To avoid missing references, copy the names here and find the classes later
    SetLength(TargetClassNames, Length(Targets));
    for i := 0 to High(Targets) do
    begin
        if i <> 0 then
            s := s + '|';

        s := s + Targets[i];
        TargetClassNames[i] := Targets[i];
    end;
    s := s + ')';

    inherited Create(dssContext, 0, s);
end;

function TProxyClass.Find(const ObjName: String; const ChangeActive: Boolean): Pointer;
var
    i: Integer;
begin
    Result := Nil;

    if Length(TargetClasses) = 0 then
    begin
        SetLength(TargetClasses, Length(TargetClassNames));
        with DSS do
            for i := 0 to High(TargetClassNames) do
                TargetClasses[i] := DSSClassList.Get(ClassNames.Find(TargetClassNames[i]));
    end;

    for i := 0 to High(TargetClasses) do
    begin
        Result := TargetClasses[i].Find(ObjName, ChangeActive);
        if Result <> NIL then
            Exit;
    end;
end;

procedure TProxyClass.DefineProperties;
begin
    // Empty
end;

destructor TProxyClass.Destroy;
begin
    inherited Destroy;
end;

end.