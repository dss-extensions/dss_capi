unit DSSClassDefs;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    DSSClass,
    PointerList,
    HashList;

const

    BASECLASSMASK: Cardinal = $00000007;
    CLASSMASK: Cardinal = $FFFFFFF8;

      {Basic element types}
    NON_PCPD_ELEM = 1;  // A circuit Element we don't want enumerated in PD and PC Elements
    PD_ELEMENT = 2;
    PC_ELEMENT = 3;
    CTRL_ELEMENT = 4;
    METER_ELEMENT = 5;
    HIDDEN_ELEMENT = 6;

      {Specific element Types}
    MON_ELEMENT = 1 * 8;
    DSS_OBJECT = 2 * 8;   // Just a general DSS object, accessible to all circuits
    SOURCE = 3 * 8;
    XFMR_ELEMENT = 4 * 8;
    SUBSTATION = 5 * 8;  // not used
    LINE_ELEMENT = 6 * 8;
    LOAD_ELEMENT = 7 * 8;
    FAULTOBJECT = 8 * 8;
    ENERGY_METER = 9 * 8;
    GEN_ELEMENT = 10 * 8;
    CAP_CONTROL = 11 * 8;
    REG_CONTROL = 12 * 8;
    CAP_ELEMENT = 13 * 8;
    RELAY_CONTROL = 14 * 8;
    RECLOSER_CONTROL = 15 * 8;
    FUSE_CONTROL = 16 * 8;
    REACTOR_ELEMENT = 17 * 8;
    FEEDER_ELEMENT = 18 * 8;
    GEN_CONTROL = 19 * 8;
    SENSOR_ELEMENT = 20 * 8;
    STORAGE_ELEMENT = 21 * 8;
    STORAGE_CONTROL = 22 * 8;
    SWT_CONTROL = 23 * 8;
    PVSYSTEM_ELEMENT = 24 * 8;
      // Deleted --- VV_CONTROL       = 25 * 8;
    GIC_Line = 26 * 8;
    GIC_Transformer = 27 * 8;
    INV_CONTROL = 28 * 8;
    VS_CONVERTER = 29 * 8;
    EXP_CONTROL = 30 * 8;
    UPFC_ELEMENT = 31 * 8;
    UPFC_CONTROL = 32 * 8;
    VCCS_ELEMENT = 33 * 8;
    ESPVL_CONTROL = 34 * 8;
    INDMACH012_ELEMENT = 35 * 8;
    GIC_SOURCE = 36 * 8;
    AUTOTRANS_ELEMENT = 37 * 8;

var
    NumIntrinsicClasses,
    NumUserClasses: Integer;

procedure CreateDSSClasses;
procedure DisposeDSSClasses;
function GetDSSClassPtr(const ClassName: String): TDSSClass;
function SetObjectClass(const ObjType: String): Boolean;


implementation

uses
    SysUtils,
    DSSGlobals,
    DSSObject,
    ParserDel,
    MyDSSClassDefs,
    Solution,
    Bus,
    Line,
    VSource,
    Isource,
    VCCS,
    LineCode,
    Spectrum,
    WireData,
    CNData,
    TSData,
    LineGeometry,
    LineSpacing,
    Load,
    LoadShape,
    TempShape,
    PriceShape,
    XYCurve,
    Monitor,
    EnergyMeter,
    GrowthShape,
    TCC_Curve,
    Transformer,
    Capacitor,
    Reactor,
    Fault,
    Generator,
    RegControl,
    CapControl,
    GenDispatcher,
    Relay,
    Recloser,
    Fuse,
    Sensor,
    Feeder,
    XfmrCode,
    Storage,
    StorageController,
    SwtControl,
    PVSystem,
    InvControl,
    GICLine,
    GICTransformer,
    VSConverter,
    ExpControl,
    UPFC,
    UPFCControl,
    ESPVLControl,
    IndMach012,
    GICSource,
    AutoTrans;


{--------------------------------------------------------------}
procedure CreateDSSClasses;


begin

    Classnames := THashList.Create(25);   // Makes 5 sub lists
    DSSClassList := TPointerList.Create(10);  // 10 is initial size and increment
    DSSClasses := TDSSClasses.Create;  // class to handle junk for defining DSS classes

     {General DSS objects, not circuit elements}
    DSSObjs := TPointerList.Create(25);  // 25 is initial size and increment

     {instantiate all Intrinsic Object Classes}

     {Generic Object classes first in case others refer to them}
    DSSClasses.New := TDSSSolution.Create;
    SolutionClass := ActiveDSSClass;     // this is a special class

    LineCodeClass := TLineCode.Create;
    DSSClasses.New := LineCodeClass;

    LoadShapeClass := TLoadShape.Create;
    DSSClasses.New := LoadShapeClass;

    TShapeClass := TTShape.Create;
    DSSClasses.New := TShapeClass;

    PriceShapeClass := TPriceShape.Create;
    DSSClasses.New := PriceShapeClass;

    XYCurveClass := TXYCurve.Create;
    DSSClasses.New := XYCurveClass;

    GrowthShapeClass := TGrowthShape.Create;
    DSSClasses.New := GrowthShapeClass;

    TCC_CurveClass := TTCC_Curve.Create;
    DSSClasses.New := TCC_CurveClass;

    SpectrumClass := TSpectrum.Create;
    DSSClasses.New := SpectrumClass;

    WireDataClass := TWireData.Create;
    DSSClasses.New := WireDataClass;

    CNDataClass := TCNData.Create;
    DSSClasses.New := CNDataClass;

    TSDataClass := TTSData.Create;
    DSSClasses.New := TSDataClass;

    DSSClasses.New := TLineGeometry.Create;
    LineSpacingClass := TLineSpacing.Create;

    DSSClasses.New := LineSpacingClass;
    DSSClasses.New := TXfmrCode.Create;

     {Circuit Element Classes}
    LineClass := TLine.Create;
    DSSClasses.New := LineClass;

    VSourceClass := TVSource.Create;    // 2-terminal Vsource
    DSSClasses.New := VSourceClass;

    ISourceClass := TISource.Create;    // 2-terminal Isource
    DSSClasses.New := ISourceClass;

    VCSSClass := TVCCS.Create;
    DSSClasses.New := VCSSClass;

    LoadClass := TLoad.Create;
    DSSClasses.New := LoadClass;

    TransformerClass := TTransf.Create;
    DSSClasses.New := TransformerClass;

    RegControlClass := TRegControl.Create;
    DSSClasses.New := RegControlClass;

    CapacitorClass := TCapacitor.Create;
    DSSClasses.New := CapacitorClass;

    ReactorClass := TReactor.Create;
    DSSClasses.New := ReactorClass;

    CapControlClass := TCapControl.Create;
    DSSClasses.New := CapControlClass;

    FaultClass := TFault.Create;
    DSSClasses.New := FaultClass;

    GeneratorClass := TGenerator.Create;
    DSSClasses.New := GeneratorClass;

    GenDispatcherClass := TGenDispatcher.Create;
    DSSClasses.New := GenDispatcherClass;

    StorageClass := TStorage.Create;
    DSSClasses.New := StorageClass;

    StorageControllerClass := TStorageController.Create;
    DSSClasses.New := StorageControllerClass;

    RelayClass := TRelay.Create;
    DSSClasses.New := RelayClass;

    RecloserClass := TRecloser.Create;
    DSSClasses.New := RecloserClass;

    FuseClass := TFuse.Create;
    DSSClasses.New := FuseClass;

//     FeederClass    := TFeeder.Create;
//     DSSClasses.New := FeederClass;

    SwtControlClass := TSwtControl.Create;
    DSSClasses.New := SwtControlClass;

    PVSystemClass := TPVSystem.Create;
    DSSClasses.New := PVSystemClass;

    UPFCClass := TUPFC.Create;
    DSSClasses.New := UPFCClass;

    UPFCControlClass := TUPFCControl.Create;
    DSSClasses.New := UPFCControlClass;

    ESPVLControlClass := TESPVLControl.Create;
    DSSClasses.New := ESPVLControlClass;

    IndMach012Class := TIndMach012.Create;
    DSSClasses.New := IndMach012Class;

    GICsourceClass := TGICsource.Create; // GIC source
    DSSClasses.New := GICsourceClass;

    AutoTransClass := TAutoTrans.Create; // Auto Transformer
    DSSClasses.New := AutoTransClass;

    InvControlClass := TInvControl.Create;
    DSSClasses.New := InvControlClass;

    ExpControlClass := TExpControl.Create;
    DSSClasses.New := ExpControlClass;

    DSSClasses.New := TGICLine.Create;
    DSSClasses.New := TGICTransformer.Create;

    VSConverterClass := TVSConverter.Create;
    DSSClasses.New := VSConverterClass;

    MonitorClass := TDSSMonitor.Create;  // Have to do this AFTER Generator
    DSSClasses.New := MonitorClass;

    EnergyMeterClass := TEnergyMeter.Create;  // Have to do this AFTER Generator
    DSSClasses.New := EnergyMeterClass;

    SensorClass := TSensor.Create;      // Create state estimation sensors
    DSSClasses.New := SensorClass;


 { Create Classes for custom implementations }
    CreateMyDSSClasses;

    NumIntrinsicClasses := DSSClassList.ListSize;
    NumUserClasses := 0;

   {Add user-defined objects}


   {This feature has been disabled - doesn't work in IIS}

   // Check all DLLs in present directory and home DSS directory to see if they
   // are a user-defined DSS class

   //**** LoadUserClasses;


end;

//----------------------------------------------------------------------------
procedure DisposeDSSClasses;

var
    i: Integer;
    DSSObj: TDSSObject;
    TraceName: String;
    SuccessFree: String;

begin

    try
        SuccessFree := 'First Object';
        for i := 1 to DSSObjs.ListSize do
        begin
            DSSObj := DSSObjs.Get(i);
            TraceName := DSSObj.ParentClass.Name + '.' + DSSObj.Name;
            DSSObj.Free;
            SuccessFree := TraceName;
        end;
        TraceName := '(DSSObjs Class)';
        DSSObjs.Free;
    except
        On E: Exception do
            Dosimplemsg('Exception disposing of DSS Obj "' + TraceName + '". ' + CRLF +
                'Last Successful dispose was for object "' + SuccessFree + '" ' + CRLF +
                E.Message, 901);
    end;

    try
        for i := 1 to DSSClassList.ListSize do
            TDSSClass(DSSClassList.Get(i)).Free;
        TraceName := '(DSS Class List)';
        DSSClassList.Free;
        TraceName := '(DSS Classes)';
        DSSClasses.Free;
        TraceName := '(ClassNames)';
        ClassNames.Free;
    except
        On E: Exception do
            Dosimplemsg('Exception disposing of DSS Class"' + TraceName + '". ' + CRLF + E.Message, 902);
    end;

end;


{--------------------------------------------------------------}
procedure AddUserClass;

begin
      // ***** ADD STUFF HERE ****

      {Assumes DLL has been loaded by call to LoadLibrary and the Handle is stored
       in LastUserDLLHandle.  Also, assumes DSSRegisterProc has the address of
       the user.}


     { ***** Needs to be re-done ****** }


end;

{--------------------------------------------------------------}
procedure LoadUserClasses;
var
    F: TSearchRec;
begin

{  Rework This !!!!}

    // Check All DLLs in present directory
    if FindFirst('*.dll', 0, F) = 0 then
    begin
        repeat
            if IsDSSDLL(F.Name) then
                AddUserclass; // Attempt to add (ignored if classname already exists)
        until FindNext(F) <> 0;
    end;

    // Check All DLLs in DSS Directory   unless that is the directory we just checked
    if comparetext(StartupDirectory, DSSDirectory) <> 0 then
        if FindFirst(DSSDirectory + '*.dll', 0, F) = 0 then
        begin
            repeat
                if IsDSSDLL(F.Name) then
                    AddUserclass; // Attempt to add (ignored if classname already exists)
            until FindNext(F) <> 0;
        end;

end;

//----------------------------------------------------------------------------
function SetObjectClass(const ObjType: String): Boolean;

// set LastClassReferenced variable by class name

var
    Classref: Integer;

begin

    Classref := ClassNames.Find(ObjType);

    case Classref of
        0:
        begin
            DoSimpleMsg('Error! Object Class "' + ObjType + '" not found.' + CRLF + parser.CmdString, 903);
            Result := FALSE;
            Exit;
        end;{Error}
    else
        LastClassReferenced := Classref;
    end;

    Result := TRUE;

end;

//----------------------------------------------------------------------------
function GetDSSClassPtr(const ClassName: String): TDSSClass;
begin
    Result := TDSSClass(DSSClassList.Get(ClassNames.Find(lowercase(ClassName))));
end;


end.
