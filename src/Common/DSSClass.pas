
unit DSSClass;
{
    ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
 Base Class for all DSS collection classes.
 Keeps track of objects of each class, dispatches edits, etc
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
    SyncObjs, 
    UComplex, 
    CAPI_Types;

type
{$SCOPEDENUMS ON}
    TActorStatus = (
        Busy = 0,
        Idle = 1
    );
{$SCOPEDENUMS OFF}
    TAction = record
        ActionCode: Integer;
        DeviceHandle: Integer;
    end;
    
    pAction = ^TAction;

    TDSSContext = class;

    dss_callback_plot_t = function (DSS: TDSSContext; jsonParams: PChar): Integer; CDECL;
    dss_callback_message_t = function (DSS: TDSSContext; messageStr: PChar; messageType: Integer): Integer; CDECL;

    // Collection of all DSS Classes
    TDSSClasses = class(TObject)
    public
        DSS: TDSSContext;

        constructor Create(dssContext: TDSSContext);
        destructor Destroy; override;
        PROCEDURE New(Value:Pointer);
   End;

   // Base for all collection classes
    TDSSClass = class(TObject)
    type 
        THashListType = {$IFDEF DSS_CAPI_HASHLIST}TAltHashList;{$ELSE}THashList;{$ENDIF}
     private

          Procedure Set_Active(value:Integer);
        function Get_ElementCount: Integer;
        function Get_First: Integer;
        function Get_Next: Integer;

          Procedure ResynchElementNameList;

    Protected
        Class_Name: String;
        ActiveElement: Integer;   // index of present ActiveElement
        CommandList: TCommandlist;
        ActiveProperty: Integer;
        ElementNameList: THashListType;

        Function AddObjectToList(Obj:Pointer):Integer;  // Used by NewObject
        Function Get_FirstPropertyName:String;
        Function Get_NextPropertyName:String;
        Function MakeLike(Const ObjName:String):Integer; Virtual;

        Procedure CountProperties;  // Add no. of intrinsic properties
        Procedure AllocatePropertyArrays;
        Procedure DefineProperties;  // Add Properties of this class to propName
        procedure ClassEdit(Const ActiveObj:Pointer; Const ParamPointer:Integer);

    
     public
        DSS: TDSSContext;
        
        NumProperties: Integer;
        PropertyName,
        PropertyHelp: pStringArray;
        PropertyIdxMap,
        RevPropertyIdxMap: pIntegerArray;    // maps property to internal command number

        DSSClassType: Integer;

        ElementList: TDSSPointerList;
        ElementNamesOutOfSynch: Boolean;     // When device gets renamed

        Saved: Boolean;

        constructor Create(dssContext: TDSSContext);
        destructor Destroy; override;

        Procedure AddProperty(const PropName:String; CmdMapIndex:Integer; const HelpString:String);
        Procedure ReallocateElementNameList;
        
        Function Edit:Integer;Virtual;      // uses global parser
        Function NewObject(const ObjName:String):Integer; Virtual;

        Function SetActive(const ObjName:String):Boolean;
         Function GetActiveObj:Pointer; // Get address of active obj of this class
        Function Find(const ObjName:String; const ChangeActive: Boolean=True): Pointer; virtual;  // Find an obj of this class by name

        Function PropertyIndex(Const Prop:String):Integer;
        Property FirstPropertyName:String read Get_FirstPropertyName;
        Property NextPropertyName:String read Get_NextPropertyName;

        Property Active:Integer read ActiveElement write Set_Active;
        Property ElementCount:Integer read Get_ElementCount;
        Property First:Integer read Get_First;
        Property Next:Integer read Get_Next;
        Property Name:String read Class_Name;
        
    protected
        // DSSContext convenience functions
        procedure DoErrorMsg(Const S, Emsg, ProbCause: String; ErrNum: Integer);inline;
        procedure DoSimpleMsg(Const S: String; ErrNum:Integer);inline;
        function InterpretDblArray(const s: String; MaxValues: Integer; ResultArray: pDoubleArray): Integer;inline;
        function InterpretIntArray(const s: String; MaxValues: Integer; ResultArray: pIntegerArray): Integer;inline;
        procedure InterpretAndAllocStrArray(const s: String; var Size: Integer; var ResultArray: pStringArray);inline;
        procedure InterpretTStringListArray(const s: String; var ResultList: TStringList);inline;
        function InterpretTimeStepSize(const s: String): Double;inline;
        function InterpretColorName(const s: String): Integer;inline;
        function InterpretComplex(const s: String): Complex;inline;
        function GetCktElementIndex(const FullObjName: String): Integer;inline;
        function AdjustInputFilePath(const param: String): String;inline;
   END;

    TDSSContext = class(TObject)
    protected
        FLoadShapeClass: TDSSClass;
        FTShapeClass: TDSSClass;
        FPriceShapeClass: TDSSClass;
        FXYCurveClass: TDSSClass;
        FGrowthShapeClass: TDSSClass;
        FSpectrumClass: TDSSClass;
        FSolutionClass: TDSSClass;
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
        FActorThread: TThread; //TODO: Currently only for solution, extend later
{$ENDIF}

        CurrentDSSDir_internal: String;

    public
        Parent: TDSSContext;
    
        DSSPlotCallback: dss_callback_plot_t;
        DSSMessageCallback: dss_callback_message_t;
    
        // Parallel Machine state
        ADiakoptics: Boolean;
{$IFDEF DSS_CAPI_PM}
        Children: array of TDSSContext;
        ActiveChild: TDSSContext;
        ActiveChildIndex: Integer;
        CPU: Integer;
        
        IsSolveAll: Boolean;
        AllActors: Boolean;
        Parallel_enabled: Boolean;
        ConcatenateReports: Boolean;
        ActorPctProgress: Integer;
        
        ActorMsg: TEvent;

        ActorStatus: TActorStatus;
        ActorMA_Msg: TEvent;
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
        DSSClasses: TDSSClasses;
        ClassNames         :TClassNamesHashListType;
        DSSClassList    :TDSSPointerList; // pointers to the base class types
        Circuits        :TDSSPointerList;
        DSSObjs         :TDSSPointerList;

        NumIntrinsicClasses,
        NumUserClasses: Integer;

        ActiveDSSClass  :TDSSClass;
        AuxParser       :TParser;  // Auxiliary parser for use by anybody for reparsing values
        Parser: TParser;
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
        SolutionAbort      :Boolean;
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
        ComParser: TParser; // CAPI_Parser.pas
        ReduceEditString: String; // CAPI_ReduceCkt.pas
        EnergyMeterName: String; // CAPI_ReduceCkt.pas
        FirstPDelement: String;  // Full name -- CAPI_ReduceCkt.pas
        FControlProxyObj: TObject; // CAPI_CtrlQueue.pas
        ActiveAction: pAction; // CAPI_CtrlQueue.pas
        
        constructor Create(_Parent: TDSSContext = nil; _IsPrime: Boolean = False);
        destructor Destroy; override;
        function GetPrime(): TDSSContext;
        function CurrentDSSDir(): String;
        procedure SetCurrentDSSDir(dir: String);        
    End;


VAR
    DSSPrime: TDSSContext;

implementation

USES DSSGlobals, SysUtils, DSSObject, CktElement, DSSHelper, DSSObjectHelper, Executive, ControlProxy, Utilities, ExportCIMXML;

function TDSSContext.GetPrime(): TDSSContext;
begin
    if IsPrime or (Parent = nil) then 
        Result := self
    else
        Result := Parent.GetPrime();
end;

constructor TDSSContext.Create(_Parent: TDSSContext; _IsPrime: Boolean);
begin
    inherited Create;

    // GR (global result) counters: Initialize to zero
    FillByte(GR_Counts_PPAnsiChar, sizeof(TAPISize) * 2, 0);
    FillByte(GR_Counts_PDouble, sizeof(TAPISize) * 2, 0);
    FillByte(GR_Counts_PInteger, sizeof(TAPISize) * 2, 0);
    FillByte(GR_Counts_PByte, sizeof(TAPISize) * 2, 0);

    IsPrime := _IsPrime;
    Parent := _Parent;

    DSSPlotCallback := nil;
    DSSMessageCallback := nil;

    ADiakoptics := False;
{$IFDEF DSS_CAPI_PM}
    ActorStatus := TActorStatus.Idle;
    ActorMA_Msg := nil;

    ActiveChildIndex := 0;
    Children := nil;
    
    IsSolveAll := False;
    AllActors := False;
    ConcatenateReports := False;
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
        ActiveChild := nil;
        _Name := '_';
    end;
    CPU := 0; //TODO: unused for now, can be useful even on single thread later
{$ELSE}
    _Name := '';
{$ENDIF} // DSS_CAPI_PM

    
    LastCmdLine := '';
    RedirFile := '';

{$IFDEF DSS_CAPI}
    // Use the current working directory as the initial datapath when using DSS_CAPI
    SetDataPath(self, StartupDirectory);
{$ENDIF}
    
    
    
    ParserVars := TParserVar.Create(100);  // start with space for 100 variables
    Parser := TParser.Create;
    AuxParser := TParser.Create;
    Parser.SetVars(ParserVars);
    AuxParser.SetVars(ParserVars);
    
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
    SolutionAbort         := FALSE;
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
    
    ComParser := ParserDel.TParser.Create;  // create COM Parser object
    ActiveAction := NIL;
    FControlProxyObj := TControlProxyObj.Create(self);
    
    DSSExecutive := TExecutive.Create(self);
    DSSExecutive.CreateDefaultDSSItems;
    
    CIMExporter := TCIMExporter.Create(self);
end;

destructor TDSSContext.Destroy;
begin
    // DSSExecutive.Free?
    
    if FControlProxyObj <> nil then
        TControlProxyObj(FControlProxyObj).Free;

    // No need to free ActiveAction, it only points to the action
    
    AuxParser.Free;
    EventStrings.Free;
    SavedFileList.Free;
    ErrorStrings.Free;
    ParserVars.Free;
    Parser.Free;
    ComParser.Free;

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

{--------------------------------------------------------------}
{ DSSClasses Implementation
{--------------------------------------------------------------}
Constructor TDSSClasses.Create(dssContext: TDSSContext);
Begin
     Inherited Create;
     
     DSS := dssContext;
End;

{--------------------------------------------------------------}
Destructor TDSSClasses.Destroy;
Begin
     Inherited Destroy;
End;

{--------------------------------------------------------------}
PROCEDURE TDSSClasses.New(Value:Pointer);

Begin
    DSS.DSSClassList.New := Value; // Add to pointer list
    DSS.ActiveDSSClass := Value;   // Declare to be active
    DSS.ClassNames.Add(DSS.ActiveDSSClass.Name); // Add to classname list
End;

{--------------------------------------------------------------}
{  DSSClass Implementation
{--------------------------------------------------------------}
Constructor TDSSClass.Create(dssContext: TDSSContext);

BEGIN
    Inherited Create;
    DSS := dssContext;
    ElementList := TDSSPointerList.Create(20);  // Init size and increment
    PropertyName := nil;
    PropertyHelp := Nil;
    PropertyIdxMap  := Nil;
    RevPropertyIdxMap := Nil;

    ActiveElement := 0;
    ActiveProperty := 0;


    ElementNameList := THashListType.Create(100);
    ElementNamesOutOfSynch := FALSE;

END;

{--------------------------------------------------------------}
Destructor TDSSClass.Destroy;

VAR
   i:INTEGER;

BEGIN
    // Get rid of space occupied by strings
    For i := 1 to NumProperties DO PropertyName[i] := '';
    For i := 1 to NumProperties DO PropertyHelp[i] := '';
    Reallocmem(PropertyName, 0);
    Reallocmem(PropertyHelp, 0);
    Reallocmem(PropertyIdxMap, 0);
    Reallocmem(RevPropertyIdxMap, 0);
    ElementList.Free;
    ElementNameList.Free;
    CommandList.Free;
    Inherited Destroy;
END;


{--------------------------------------------------------------}
Function TDSSClass.NewObject(const ObjName:String):Integer;
BEGIN
    Result := 0;
    DoErrorMsg('Reached base class of TDSSClass for device "' + ObjName + '"',
        'N/A',
        'Should be overridden.', 780);
END;

Procedure TDSSClass.Set_Active(value:Integer);
BEGIN
     If (Value > 0) and (Value<= ElementList.Count)
     THEN
       Begin
        ActiveElement := Value;
        DSS.ActiveDSSObject := ElementList.Get(ActiveElement);
         // Make sure Active Ckt Element agrees if is a ckt element
         // So COM interface will work
        if ActiveDSSObject is TDSSCktElement then
            ActiveCircuit.ActiveCktElement := TDSSCktElement(ActiveDSSObject);
       End;
END;

Function TDSSClass.Edit:Integer;
BEGIN
    Result := 0;
    DoSimpleMsg('virtual function TDSSClass.Edit called.  Should be overriden.', 781);
END;

Function TDSSClass.AddObjectToList(Obj:Pointer):Integer;
BEGIN
    ElementList.New := Obj; // Stuff it in this collection's element list
    ElementNameList.Add(TDSSObject(Obj).Name);
{$IFNDEF DSS_CAPI_HASHLIST}
    If Cardinal(ElementList.Count) > 2 * ElementNameList.InitialAllocation Then ReallocateElementNameList;
{$ENDIF}
    ActiveElement := ElementList.Count;
    Result := ActiveElement; // Return index of object in list
END;

Function TDSSClass.SetActive(const ObjName:String):Boolean;
VAR
    idx: Integer;

BEGIN
    Result := False;
    // Faster to look in hash list 7/7/03
    If ElementNamesOutOfSynch Then ResynchElementNameList;
    idx := ElementNameList.Find(ObjName);
    
    If idx>0 Then
    Begin
        ActiveElement := idx;
        DSS.ActiveDSSObject := ElementList.get(idx);
        Result := TRUE;
    End;

END;

Function TDSSClass.Find(const ObjName:String; const ChangeActive: Boolean):Pointer;
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
    ELSE Result := '';
END;

Function TDSSClass.PropertyIndex(Const Prop:String):Integer;
// find property value by string

VAR
    i: Integer;
BEGIN

    Result := 0;  // Default result if not found
     For i := 1 to NumProperties DO BEGIN
        IF CompareText(Prop, PropertyName[i])=0 THEN BEGIN
            Result := PropertyIdxMap[i];
            Break;
        END;
     END;
END;

Procedure TDSSClass.CountProperties;
Begin
    NumProperties := NumProperties + 1;
End;

Procedure TDSSClass.DefineProperties;
Begin
    ActiveProperty := ActiveProperty + 1;
    PropertyName^[ActiveProperty] := 'like';
    PropertyHelp^[ActiveProperty] := 'Make like another object, e.g.:' + CRLF + CRLF +
        'New Capacitor.C2 like=c1  ...';
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSClass.ClassEdit(Const ActiveObj:Pointer; Const ParamPointer:Integer);
BEGIN
  // continue parsing with contents of Parser
  If ParamPointer > 0 Then
  WITH TDSSObject(ActiveObj) DO BEGIN
      CASE ParamPointer OF
       1: MakeLike(Parser.StrValue);    // Like command (virtual)
      END;
  End;
End;

Function  TDSSClass.MakeLike(Const ObjName:String):Integer;
Begin
    Result := 0;
    DoSimpleMsg('virtual function TDSSClass.MakeLike called.  Should be overriden.', 784);
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
        if ActiveDSSObject is TDSSCktElement then
            ActiveCircuit.ActiveCktElement := TDSSCktElement(ActiveDSSObject);
        Result := ActiveElement;
    End;
end;

function TDSSClass.Get_Next: Integer;
begin
    Inc(ActiveElement);
    IF ActiveElement > ElementList.Count
    THEN Result := 0
    ELSE Begin
        DSS.ActiveDSSObject := ElementList.Next;
      // Make sure Active Ckt Element agrees if is a ckt element
      // So COM interface will work
        if ActiveDSSObject is TDSSCktElement then
            ActiveCircuit.ActiveCktElement := TDSSCktElement(ActiveDSSObject);
        Result := ActiveElement;
    End;

end;

procedure TDSSClass.AddProperty(const PropName: String; CmdMapIndex: Integer; const HelpString: String);

begin
    Inc(ActiveProperty);
    PropertyName[ActiveProperty] := PropName;
    PropertyHelp[ActiveProperty] := HelpString;
    PropertyIdxMap[ActiveProperty] := CmdMapIndex;   // Maps to internal object property index
    RevPropertyIdxMap[CmdMapIndex] := ActiveProperty;
end;

procedure TDSSClass.AllocatePropertyArrays;
Var 
    i:Integer;
begin
    PropertyName := Allocmem(SizeOf(PropertyName^[1]) * NumProperties);
    PropertyHelp := Allocmem(SizeOf(PropertyHelp^[1]) * NumProperties);
    PropertyIdxMap := Allocmem(SizeOf(PropertyIdxMap^[1]) * NumProperties);
    RevPropertyIdxMap := Allocmem(SizeOf(RevPropertyIdxMap^[1]) * NumProperties);
    ActiveProperty := 0;    // initialize for AddPropert
     {initialize PropertyIdxMap to take care of legacy items}
     For i := 1 to NumProperties Do PropertyIDXMap^[i] := i;
     For i := 1 to NumProperties Do RevPropertyIDXMap^[i] := i;
end;

procedure TDSSClass.ReallocateElementNameList;
Var
    i: Integer;
begin
  {Reallocate the device name list to improve the performance of searches}
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


procedure TDSSClass.DoErrorMsg(Const S, Emsg, ProbCause: String; ErrNum: Integer);inline;
begin
    DSSGlobals.DoErrorMsg(DSS, S, Emsg, ProbCause, ErrNum)
end;

procedure TDSSClass.DoSimpleMsg(Const S: String; ErrNum:Integer);inline;
begin
    DSSGlobals.DoSimpleMsg(DSS, S, ErrNum)
end;

function TDSSClass.InterpretDblArray(const s: String; MaxValues: Integer; ResultArray: pDoubleArray): Integer;inline;
begin
    Result := Utilities.InterpretDblArray(DSS, s, MaxValues, ResultArray)
end;

function TDSSClass.InterpretIntArray(const s: String; MaxValues: Integer; ResultArray: pIntegerArray): Integer;inline;
begin
    Result := Utilities.InterpretIntArray(DSS, s, MaxValues, ResultArray)
end;

procedure TDSSClass.InterpretAndAllocStrArray(const s: String; var Size: Integer; var ResultArray: pStringArray);inline;
begin
    Utilities.InterpretAndAllocStrArray(DSS, s, Size, ResultArray)
end;

procedure TDSSClass.InterpretTStringListArray(const s: String; var ResultList: TStringList);inline;
begin
    Utilities.InterpretTStringListArray(DSS, s, ResultList)
end;

function TDSSClass.InterpretTimeStepSize(const s: String): Double;inline;
begin
    Result := Utilities.InterpretTimeStepSize(DSS, s)
end;

function TDSSClass.InterpretColorName(const s: String): Integer;inline;
begin
    Result := Utilities.InterpretColorName(DSS, s)
end;

function TDSSClass.InterpretComplex(const s: String): Complex;inline;
begin
    Result := Utilities.InterpretComplex(DSS, s)
end;

function TDSSClass.GetCktElementIndex(const FullObjName: String): Integer;inline;
begin
    Result := Utilities.GetCktElementIndex(DSS, FullObjName)
end;

function TDSSClass.AdjustInputFilePath(const param: String): String;
begin
    Result := Utilities.AdjustInputFilePath(DSS, param)
end;


end.
