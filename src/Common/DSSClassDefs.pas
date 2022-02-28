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

procedure CreateDSSClasses(DSS: TDSSContext);
procedure DisposeDSSClasses(DSS: TDSSContext);
function GetDSSClassPtr(DSS: TDSSContext; const ClassName: String): TDSSClass;
function SetObjectClass(DSS: TDSSContext; const ObjType: String): Boolean;


implementation

uses
    SysUtils,
    DSSGlobals,
    DSSObject,
    ParserDel,
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
    AutoTrans,
        
    DSSHelper;

procedure CreateDSSClasses(DSS: TDSSContext);
begin
    DSS_CAPI_LEGACY_MODELS_PREV := DSS_CAPI_LEGACY_MODELS;
    
    DSS.Classnames := TClassNamesHashListType.Create(40);   // Makes 5 sub lists
    DSS.DSSClassList := TDSSPointerList.Create(40);  // 40 is initial size and increment

    // General DSS objects, not circuit elements
    DSS.DSSObjs := TDSSPointerList.Create(1024);

    // instantiate all Intrinsic Object Classes

    // Generic Object classes first in case others refer to them

    DSS.LineCodeClass := TLineCode.Create(DSS);
    DSS.NewDSSClass(DSS.LineCodeClass);

    DSS.LoadShapeClass := TLoadShape.Create(DSS);
    DSS.NewDSSClass(DSS.LoadShapeClass);

    DSS.TShapeClass := TTShape.Create(DSS);
    DSS.NewDSSClass(DSS.TShapeClass);

    DSS.PriceShapeClass := TPriceShape.Create(DSS);
    DSS.NewDSSClass(DSS.PriceShapeClass);

    DSS.XYCurveClass := TXYCurve.Create(DSS);
    DSS.NewDSSClass(DSS.XYCurveClass);

    DSS.GrowthShapeClass := TGrowthShape.Create(DSS);
    DSS.NewDSSClass(DSS.GrowthShapeClass);

    DSS.TCC_CurveClass := TTCC_Curve.Create(DSS);
    DSS.NewDSSClass(DSS.TCC_CurveClass);

    DSS.SpectrumClass := TSpectrum.Create(DSS);
    DSS.NewDSSClass(DSS.SpectrumClass);

    DSS.WireDataClass := TWireData.Create(DSS);
    DSS.NewDSSClass(DSS.WireDataClass);

    DSS.CNDataClass := TCNData.Create(DSS);
    DSS.NewDSSClass(DSS.CNDataClass);

    DSS.TSDataClass := TTSData.Create(DSS);
    DSS.NewDSSClass(DSS.TSDataClass);

    DSS.LineSpacingClass := TLineSpacing.Create(DSS);
    DSS.NewDSSClass(DSS.LineSpacingClass);

    DSS.LineGeometryClass := TLineGeometry.Create(DSS);
    DSS.NewDSSClass(DSS.LineGeometryClass);
    
    DSS.XfmrCodeClass := TXfmrCode.Create(DSS);
    DSS.NewDSSClass(DSS.XfmrCodeClass);

    // Circuit Element Classes
    DSS.LineClass := TLine.Create(DSS);
    DSS.NewDSSClass(DSS.LineClass);

    DSS.VSourceClass := TVSource.Create(DSS);    // 2-terminal Vsource
    DSS.NewDSSClass(DSS.VSourceClass);




    DSS.ISourceClass := TISource.Create(DSS);    // 2-terminal Isource
    DSS.NewDSSClass(DSS.ISourceClass);

    DSS.VCSSClass := TVCCS.Create(DSS);
    DSS.NewDSSClass(DSS.VCSSClass);




    DSS.LoadClass := TLoad.Create(DSS);
    DSS.NewDSSClass(DSS.LoadClass);

    DSS.TransformerClass := TTransf.Create(DSS);
    DSS.NewDSSClass(DSS.TransformerClass);




    DSS.RegControlClass := TRegControl.Create(DSS);
    DSS.NewDSSClass(DSS.RegControlClass);

    DSS.CapacitorClass := TCapacitor.Create(DSS);
    DSS.NewDSSClass(DSS.CapacitorClass);

    DSS.ReactorClass := TReactor.Create(DSS);
    DSS.NewDSSClass(DSS.ReactorClass);

    DSS.CapControlClass := TCapControl.Create(DSS);
    DSS.NewDSSClass(DSS.CapControlClass);

    DSS.FaultClass := TFault.Create(DSS);
    DSS.NewDSSClass(DSS.FaultClass);

    DSS.GeneratorClass := TGenerator.Create(DSS);
    DSS.NewDSSClass(DSS.GeneratorClass);

    DSS.GenDispatcherClass := TGenDispatcher.Create(DSS);
    DSS.NewDSSClass(DSS.GenDispatcherClass);

    if DSS_CAPI_LEGACY_MODELS then
    begin
        DSS.StorageClass := TStorage.Create(DSS);
        DSS.Storage2Class := NIL;
        DSS.NewDSSClass(DSS.StorageClass);
    end
    else
    begin
    	DSS.StorageClass := NIL;
        DSS.Storage2Class := TStorage2.Create(DSS);
        DSS.NewDSSClass(DSS.Storage2Class);
    end;

    if DSS_CAPI_LEGACY_MODELS then
    begin
        DSS.StorageControllerClass := TStorageController.Create(DSS);
        DSS.StorageController2Class := NIL;
        DSS.NewDSSClass(DSS.StorageControllerClass);
    end
    else
    begin
    	DSS.StorageControllerClass := NIL;
        DSS.StorageController2Class := TStorageController2.Create(DSS);
        DSS.NewDSSClass(DSS.StorageController2Class);
    end;

    DSS.RelayClass := TRelay.Create(DSS);
    DSS.NewDSSClass(DSS.RelayClass);

    DSS.RecloserClass := TRecloser.Create(DSS);
    DSS.NewDSSClass(DSS.RecloserClass);

    DSS.FuseClass := TFuse.Create(DSS);
    DSS.NewDSSClass(DSS.FuseClass);

    DSS.SwtControlClass := TSwtControl.Create(DSS);
    DSS.NewDSSClass(DSS.SwtControlClass);

    if DSS_CAPI_LEGACY_MODELS then
    begin
        DSS.PVSystemClass := TPVSystem.Create(DSS);
        DSS.PVSystem2Class := NIL;
        DSS.NewDSSClass(DSS.PVSystemClass);
    end
    else
    begin
    	DSS.PVSystemClass := NIL;
        DSS.PVSystem2Class := TPVSystem2.Create(DSS);
        DSS.NewDSSClass(DSS.PVSystem2Class);
    end;

    DSS.UPFCClass := TUPFC.Create(DSS);
    DSS.NewDSSClass(DSS.UPFCClass);

    DSS.UPFCControlClass := TUPFCControl.Create(DSS);
    DSS.NewDSSClass(DSS.UPFCControlClass);

    DSS.ESPVLControlClass := TESPVLControl.Create(DSS);
    DSS.NewDSSClass(DSS.ESPVLControlClass);

    DSS.IndMach012Class := TIndMach012.Create(DSS);
    DSS.NewDSSClass(DSS.IndMach012Class);

    DSS.GICsourceClass := TGICsource.Create(DSS); // GIC source
    DSS.NewDSSClass(DSS.GICsourceClass);

    DSS.AutoTransClass := TAutoTrans.Create(DSS); // Auto Transformer
    DSS.NewDSSClass(DSS.AutoTransClass);

    if DSS_CAPI_LEGACY_MODELS then
    begin
        DSS.InvControlClass := TInvControl.Create(DSS);
        DSS.InvControl2Class := NIL;
        DSS.NewDSSClass(DSS.InvControlClass);
    end
    else
    begin
    	DSS.InvControlClass := NIL;
        DSS.InvControl2Class := TInvControl2.Create(DSS);
        DSS.NewDSSClass(DSS.InvControl2Class);
    end;

    DSS.ExpControlClass := TExpControl.Create(DSS);
    DSS.NewDSSClass(DSS.ExpControlClass);

    DSS.GICLineClass := TGICLine.Create(DSS);
    DSS.NewDSSClass(DSS.GICLineClass);
    
    DSS.GICTransformerClass := TGICTransformer.Create(DSS);
    DSS.NewDSSClass(DSS.GICTransformerClass);

    DSS.VSConverterClass := TVSConverter.Create(DSS);
    DSS.NewDSSClass(DSS.VSConverterClass);

    DSS.MonitorClass := TDSSMonitor.Create(DSS);  // Have to do this AFTER Generator
    DSS.NewDSSClass(DSS.MonitorClass);

    DSS.EnergyMeterClass := TEnergyMeter.Create(DSS);  // Have to do this AFTER Generator
    DSS.NewDSSClass(DSS.EnergyMeterClass);

    DSS.SensorClass := TSensor.Create(DSS);      // Create state estimation sensors
    DSS.NewDSSClass(DSS.SensorClass);
    DSS.NumIntrinsicClasses := DSS.DSSClassList.Count;
end;

procedure DisposeDSSClasses(DSS: TDSSContext);
var
    i: Integer;
    DSSObj: TDSSObject;
    TraceName: String;
    SuccessFree: String;
begin
    try
        if DSS.DSSObjs <> NIL then
        begin
            SuccessFree := 'First Object';
            for i := 1 to DSS.DSSObjs.Count do
            begin
                DSSObj := DSS.DSSObjs.At(i);
                TraceName := DSSObj.FullName;
                DSSObj.Free;
                SuccessFree := TraceName;
            end;
            TraceName := '(DSSObjs Class)';
            FreeAndNil(DSS.DSSObjs);
        end;
    except
        On E: Exception do
            DoSimpleMsg(DSS, 'Exception disposing of DSS Obj "%s". Last Successful dispose was for object "%s". %s', 
                [TraceName, SuccessFree, CRLF + E.Message],
                901);
    end;

    try
        if DSS.DSSClassList <> NIL then
        begin
            for i := 1 to DSS.DSSClassList.Count do
            begin
                TDSSClass(DSS.DSSClassList.Get(i)).Free;
            end;
            TraceName := '(DSS Class List)';
            FreeAndNil(DSS.DSSClassList);
        end;
        if DSS.ClassNames <> NIL then
        begin
            TraceName := '(ClassNames)';
            FreeAndNil(DSS.ClassNames);
        end;
    except
        On E: Exception do
            DoSimpleMsg(DSS, Format(_('Exception disposing of DSS Class "%s".'), [TraceName]) + CRLF + E.Message, 902);
    end;
end;


function SetObjectClass(DSS: TDSSContext; const ObjType: String): Boolean;

// set LastClassReferenced variable by class name

var
    Classref: Integer;

begin
    Classref := DSS.ClassNames.Find(ObjType);

    case Classref of
        0:
        begin
            DoSimpleMsg(DSS, Format(_('Error! Object Class "%s" not found.'), [ObjType]) + CRLF + DSS.Parser.CmdString, 903);
            Result := FALSE;
            Exit;
        end;{Error}
    else
        DSS.LastClassReferenced := Classref;
    end;

    Result := TRUE;
end;

function GetDSSClassPtr(DSS: TDSSContext; const ClassName: String): TDSSClass;
begin
    Result := TDSSClass(DSS.DSSClassList.Get(DSS.ClassNames.Find(lowercase(ClassName))));
end;


end.
