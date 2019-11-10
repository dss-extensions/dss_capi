
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
    Command,  Arraydef, Hashlist, {$IFDEF DSS_CAPI_HASHLIST}Contnrs,{$ENDIF} Classes, PointerList, NamedObject, ParserDel;

TYPE
    TDSS = class;

    // Collection of all DSS Classes
    TDSSClasses = class(TObject)
    public
        DSS: TDSS;
    
        constructor Create(dss: TDSS);
        destructor Destroy; override;
        PROCEDURE New(Value:Pointer);
   End;

   // Base for all collection classes
    TDSSClass = class(TObject)
    private
        
        Procedure Set_Active(value:Integer);
        function Get_ElementCount: Integer;
        function Get_First: Integer;
        function Get_Next: Integer;

        Procedure ResynchElementNameList;

    protected
        Class_Name: String;
        ActiveElement: Integer;   // index of present ActiveElement
        CommandList: TCommandlist;
        ActiveProperty: Integer;
        ElementNameList:{$IFDEF DSS_CAPI_HASHLIST}TFPHashList;{$ELSE}THashList;{$ENDIF}

        Function AddObjectToList(Obj:Pointer):Integer;  // Used by NewObject
        Function Get_FirstPropertyName:String;
        Function Get_NextPropertyName:String;
        Function MakeLike(Const ObjName:String):Integer; Virtual;

        Procedure CountProperties;  // Add no. of intrinsic properties
        Procedure AllocatePropertyArrays;
        Procedure DefineProperties;  // Add Properties of this class to propName
        procedure ClassEdit(Const ActiveObj:Pointer; Const ParamPointer:Integer);

     public
        DSS: TDSS;
        
        NumProperties: Integer;
        PropertyName,
        PropertyHelp: pStringArray;
        PropertyIdxMap,
        RevPropertyIdxMap: pIntegerArray;    // maps property to internal command number

        DSSClassType: Integer;


        ElementList: TPointerList;
        ElementNamesOutOfSynch: Boolean;     // When device gets renamed

        Saved: Boolean;

        constructor Create(dss: TDSS);
        destructor Destroy; override;

        {Helper routine for building Property strings}
        Procedure AddProperty(const PropName:String; CmdMapIndex:Integer; const HelpString:String);
        Procedure ReallocateElementNameList;
        
        Function Edit:Integer;Virtual;      // uses global parser
        Function Init(Handle:Integer):Integer; Virtual;
        Function NewObject(const ObjName:String):Integer; Virtual;

        Function SetActive(const ObjName:String):Boolean; Virtual;
        Function GetActiveObj:Pointer; // Get address of active obj of this class
        Function Find(const ObjName:String):Pointer; Virtual;  // Find an obj of this class by name

        Function PropertyIndex(Const Prop:String):Integer;
        Property FirstPropertyName:String read Get_FirstPropertyName;
        Property NextPropertyName:String read Get_NextPropertyName;

        Property Active:Integer read ActiveElement write Set_Active;
        Property ElementCount:Integer read Get_ElementCount;
        Property First:Integer read Get_First;
        Property Next:Integer read Get_Next;
        Property Name:String read Class_Name;
   END;

    TDSS = class(TObject)
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
        FPVSystemClass: TDSSClass;
        FInvControlClass: TDSSClass;
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
        FActiveRecloserObj: TObject;
        FActiveRegControlObj: TObject;
        FActiveRelayObj: TObject;
        FActiveStorageControllerObj: TObject;
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
        FActiveStorageObj: TObject;
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
    
        FActiveCircuit: TNamedObject;
        FActiveDSSObject :TNamedObject;
    public
        DSSClasses: TDSSClasses;
        ClassNames         :THashList;
        DSSClassList    :TPointerList; // pointers to the base class types
        Circuits        :TPointerList;
        DSSObjs         :TPointerList;

        NumIntrinsicClasses,
        NumUserClasses: Integer;

        ActiveDSSClass  :TDSSClass;
        AuxParser       :TParser;  // Auxiliary parser for use by anybody for reparsing values
    
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
        QueryLogFile       :TextFile;

        DataDirectory    :String;     // used to be DSSDataDirectory
        OutputDirectory  :String;     // output files go here, same as DataDirectory if writable
        CircuitName_     :String;     // Name of Circuit with a "_" appended

        DefaultBaseFreq  :Double;
        DaisySize        :Double;
        
        EventStrings: TStringList;
        SavedFileList:TStringList;
        ErrorStrings: TStringList;

        IncMat_Ordered     : Boolean;
        //***********************A-Diakoptics Variables*********************************
        ADiakoptics             : Boolean;

        //***********************Seasonal QSTS variables********************************
        SeasonalRating         : Boolean;    // Tells the energy meter if the seasonal rating feature is active
        SeasonSignal           : String;     // Stores the name of the signal for selecting the rating dynamically

        
        IsPrime: Boolean; // Indicates whether this instance is the first/main DSS instance

        // For external APIs
        FPropIndex: Integer;  
        FPropClass: TDSSClass;
        
        constructor Create(_IsPrime: Boolean = False);
        destructor Destroy; override;
    End;


var
    DSSPrime: TDSS;

implementation

USES DSSGlobals, SysUtils, DSSObject, CktElement, DSSHelper, Executive;

constructor TDSS.Create(_IsPrime: Boolean);
begin
    inherited Create;
    
    IsPrime := _IsPrime;

    AuxParser        := TParser.Create;
    
    ADiakoptics      :=    False;  // Disabled by default

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

    EventStrings     := TStringList.Create;
    SavedFileList    := TStringList.Create;
    ErrorStrings     := TStringList.Create;
    // ErrorStrings.Clear;
    
    FPropIndex := 0;
    FPropClass := NIL;
    
    DSSExecutive := TExecutive(self);
    DSSExecutive.CreateDefaultDSSItems;
end;

destructor TDSS.Destroy;
begin
    AuxParser.Free;
    EventStrings.Free;
    SavedFileList.Free;
    ErrorStrings.Free;

    inherited Destroy;
end;

{--------------------------------------------------------------}
{ DSSClasses Implementation
{--------------------------------------------------------------}
Constructor TDSSClasses.Create(dss: TDSS);
Begin
     Inherited Create;
     
     DSS := dss;
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
Constructor TDSSClass.Create(dss: TDSS);

BEGIN
    Inherited Create;
    DSS := dss;
    ElementList := TPointerList.Create(20);  // Init size and increment
    PropertyName := nil;
    PropertyHelp := Nil;
    PropertyIdxMap  := Nil;
    RevPropertyIdxMap := Nil;

    ActiveElement := 0;
    ActiveProperty := 0;


    ElementNameList := {$IFDEF DSS_CAPI_HASHLIST}TFPHashList.Create();{$ELSE}THashList.Create(100);{$ENDIF}
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
     If (Value > 0) and (Value<= ElementList.ListSize)
     THEN
       Begin
        ActiveElement := Value;
        DSS.ActiveDSSObject := ElementList.Get(ActiveElement);
         // Make sure Active Ckt Element agrees if is a ckt element
         // So COM interface will work
        if DSS.ActiveDSSObject is TDSSCktElement then
            DSS.ActiveCircuit.ActiveCktElement := TDSSCktElement(DSS.ActiveDSSObject);
       End;
END;

Function TDSSClass.Edit:Integer;
BEGIN
    Result := 0;
    DoSimpleMsg('virtual function TDSSClass.Edit called.  Should be overriden.', 781);
END;


Function TDSSClass.Init(Handle:Integer):Integer;
BEGIN
    Result := 0;
    DoSimpleMsg('virtual function TDSSClass.Init called.  Should be overriden.', 782);
END;

Function TDSSClass.AddObjectToList(Obj:Pointer):Integer;
BEGIN
    ElementList.New := Obj; // Stuff it in this collection's element list
{$IFNDEF DSS_CAPI_HASHLIST}
    ElementNameList.Add(TDSSObject(Obj).Name);
    If Cardinal(ElementList.ListSize) > 2* ElementNameList.InitialAllocation Then ReallocateElementNameList;
{$ELSE}    
    ElementNameList.Add(LowerCase(TDSSObject(Obj).Name), Pointer(ElementList.ListSize));
{$ENDIF}
    ActiveElement := ElementList.ListSize;
    Result := ActiveElement; // Return index of object in list
END;

Function TDSSClass.SetActive(const ObjName:String):Boolean;
VAR
    idx: Integer;

BEGIN
    Result := False;
    // Faster to look in hash list 7/7/03
    If ElementNamesOutOfSynch Then ResynchElementNameList;
    {$IFDEF DSS_CAPI_HASHLIST}
    idx := LongInt(ElementNameList.Find(LowerCase(ObjName)));
    {$ELSE}
    idx := ElementNameList.Find(ObjName);
    {$ENDIF}
    
    If idx>0 Then
    Begin
        ActiveElement := idx;
        DSS.ActiveDSSObject := ElementList.get(idx);
        Result := TRUE;
    End;

END;

Function TDSSClass.Find(const ObjName:String):Pointer;
VAR
    idx: Integer;

BEGIN
    Result := Nil;
    If ElementNamesOutOfSynch Then ResynchElementNameList;
    // Faster to look in hash list 7/7/03
    {$IFDEF DSS_CAPI_HASHLIST}
    idx := LongInt(ElementNameList.Find(LowerCase(ObjName)));
    {$ELSE}
    idx := ElementNameList.Find(ObjName);
    {$ENDIF}
    
    If idx>0 Then
    Begin
        ActiveElement := idx;
        Result := ElementList.get(idx);
    End;
END;

Function TDSSClass.GetActiveObj:Pointer; // Get address of active obj of this class
BEGIN
    ActiveElement := ElementList.ActiveIndex;
    If ActiveElement>0 THEN
        Result := ElementList.Get(ActiveElement)
    Else
       Result := Nil;
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
    Result := ElementList.ListSize;
end;

function TDSSClass.Get_First: Integer;
begin
    IF ElementList.ListSize=0   THEN Result := 0

    ELSE Begin
        ActiveElement := 1;
        DSS.ActiveDSSObject := ElementList.First;
      // Make sure Active Ckt Element agrees if is a ckt element
      // So COM interface will work
        if DSS.ActiveDSSObject is TDSSCktElement then
            DSS.ActiveCircuit.ActiveCktElement := TDSSCktElement(DSS.ActiveDSSObject);
        Result := ActiveElement;
    End;
end;

function TDSSClass.Get_Next: Integer;
begin
    Inc(ActiveElement);
    IF ActiveElement > ElementList.ListSize
    THEN Result := 0
    ELSE Begin
        DSS.ActiveDSSObject := ElementList.Next;
      // Make sure Active Ckt Element agrees if is a ckt element
      // So COM interface will work
        if DSS.ActiveDSSObject is TDSSCktElement then
            DSS.ActiveCircuit.ActiveCktElement := TDSSCktElement(DSS.ActiveDSSObject);
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
    ElementNameList := {$IFDEF DSS_CAPI_HASHLIST}TFPHashList.Create();{$ELSE}THashList.Create(2*ElementList.ListSize);{$ENDIF} // make a new one

    // Do this using the Names of the Elements rather than the old list because it might be
    // messed up if an element gets renamed

    {$IFDEF DSS_CAPI_HASHLIST}
    For i := 1 to ElementList.ListSize Do ElementNameList.Add(LowerCase(TDSSObject(ElementList.Get(i)).Name), Pointer(i));
    {$ELSE}
    For i := 1 to ElementList.ListSize Do ElementNameList.Add(TDSSObject(ElementList.Get(i)).Name);
    {$ENDIF}

end;

procedure TDSSClass.ResynchElementNameList;
begin
    ReallocateElementNameList;
    ElementNamesOutOfSynch := False;
end;

end.
