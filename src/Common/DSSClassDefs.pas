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
    DSSPointerList,
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
    // FEEDER_ELEMENT = 18 * 8;
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
    NumIntrinsicClasses: Integer;

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
    XfmrCode,
    Storage,
    Storage2,
    StorageController,
    StorageController2,
    SwtControl,
    PVSystem,
    PVSystem2,
    InvControl,
    InvControl2,
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
    DSS_CAPI_LEGACY_MODELS_PREV := DSS_CAPI_LEGACY_MODELS;
    
    Classnames := THashList.Create(25);   // Makes 5 sub lists
    DSSClassList := TDSSPointerList.Create(10);  // 10 is initial size and increment
    DSSClasses := TDSSClasses.Create;  // class to handle junk for defining DSS classes

     {General DSS objects, not circuit elements}
    DSSObjs := TDSSPointerList.Create(25);  // 25 is initial size and increment

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

    LineGeometryClass := TLineGeometry.Create;
    DSSClasses.New := LineGeometryClass;
    
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

    if DSS_CAPI_LEGACY_MODELS then
    begin
        StorageClass := TStorage.Create;
        Storage2Class := NIL;
        DSSClasses.New := StorageClass;
    end
    else
    begin
    	StorageClass := NIL;
        Storage2Class := TStorage2.Create;
        DSSClasses.New := Storage2Class;
    end;

    if DSS_CAPI_LEGACY_MODELS then
    begin
        StorageControllerClass := TStorageController.Create;
        StorageController2Class := NIL;
        DSSClasses.New := StorageControllerClass;
    end
    else
    begin
    	StorageControllerClass := NIL;
        StorageController2Class := TStorageController2.Create;
        DSSClasses.New := StorageController2Class;
    end;

    RelayClass := TRelay.Create;
    DSSClasses.New := RelayClass;

    RecloserClass := TRecloser.Create;
    DSSClasses.New := RecloserClass;

    FuseClass := TFuse.Create;
    DSSClasses.New := FuseClass;

    SwtControlClass := TSwtControl.Create;
    DSSClasses.New := SwtControlClass;

    if DSS_CAPI_LEGACY_MODELS then
    begin
        PVSystemClass := TPVSystem.Create;
        PVSystem2Class := NIL;
        DSSClasses.New := PVSystemClass;
    end
    else
    begin
    	PVSystemClass := NIL;
        PVSystem2Class := TPVSystem2.Create;
        DSSClasses.New := PVSystem2Class;
    end;

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

    if DSS_CAPI_LEGACY_MODELS then
    begin
        InvControlClass := TInvControl.Create;
        InvControl2Class := NIL;
        DSSClasses.New := InvControlClass;
    end
    else
    begin
    	InvControlClass := NIL;
        InvControl2Class := TInvControl2.Create;
        DSSClasses.New := InvControl2Class;
    end;
    
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
