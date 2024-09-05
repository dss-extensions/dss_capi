unit DSSClassDefs;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

uses
    DSSClass,
    DSSPointerList,
    HashList;

const

    BASECLASSMASK: Cardinal = $00000007;
    CLASSMASK: Cardinal = $FFFFFFF8;

    // Basic element types
    NON_PCPD_ELEM = 1;  // A circuit Element we don't want enumerated in PD and PC Elements
    PD_ELEMENT = 2;
    PC_ELEMENT = 3;
    CTRL_ELEMENT = 4;
    METER_ELEMENT = 5;
    HIDDEN_ELEMENT = 6;

    // Specific element Types
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
    FMON_ELEMENT = 38*8; // BY Dahei UCF
    GENERIC5ORDERMACH_ELEMENT = 39 * 8; // BY Dahei UCF
    
    WINDGEN_ELEMENT = 43 * 8;
    GEN_CONTROLLER = 44 * 8;


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
    AutoTrans,
    DynamicExp,
    Generic5OrderMach,
    fMonitor,
    WindGen,

    DSSHelper;

procedure CreateDSSClasses(DSS: TDSSContext);
begin
    DSS.Classnames := TClassNamesHashListType.Create(40);   // Makes 5 sub lists
    DSS.DSSClassList := TDSSPointerList.Create(40);  // 40 is initial size and increment

    // General DSS objects, not circuit elements
    DSS.DSSObjs := TDSSPointerList.Create(1024);

    // instantiate all Intrinsic Object Classes

    // Generic Object classes first in case others refer to them

    DSS.LineCodeClass := TLineCode.Create(DSS);
    DSS.LoadShapeClass := TLoadShape.Create(DSS);
    DSS.TShapeClass := TTShape.Create(DSS);
    DSS.PriceShapeClass := TPriceShape.Create(DSS);
    DSS.XYCurveClass := TXYCurve.Create(DSS);
    DSS.GrowthShapeClass := TGrowthShape.Create(DSS);
    DSS.TCC_CurveClass := TTCC_Curve.Create(DSS);
    DSS.SpectrumClass := TSpectrum.Create(DSS);
    DSS.WireDataClass := TWireData.Create(DSS);
    DSS.CNDataClass := TCNData.Create(DSS);
    DSS.TSDataClass := TTSData.Create(DSS);
    DSS.LineSpacingClass := TLineSpacing.Create(DSS);
    DSS.LineGeometryClass := TLineGeometry.Create(DSS);
    DSS.XfmrCodeClass := TXfmrCode.Create(DSS);

    // Circuit Element Classes
    DSS.LineClass := TLine.Create(DSS);
    DSS.VSourceClass := TVSource.Create(DSS);    // 2-terminal Vsource

    DSS.ISourceClass := TISource.Create(DSS);    // 2-terminal Isource
    DSS.VCSSClass := TVCCS.Create(DSS);
    DSS.LoadClass := TLoad.Create(DSS);
    DSS.TransformerClass := TTransf.Create(DSS);
    DSS.RegControlClass := TRegControl.Create(DSS);
    DSS.CapacitorClass := TCapacitor.Create(DSS);
    DSS.ReactorClass := TReactor.Create(DSS);
    DSS.CapControlClass := TCapControl.Create(DSS);
    DSS.FaultClass := TFault.Create(DSS);
    DSS.DynamicExpClass := TDynamicExp.Create(DSS); // This needs to be before Generator, PVsystem, Storage
    DSS.GeneratorClass := TGenerator.Create(DSS);
    DSS.WindGenClass := TWindGen.Create(DSS);
    DSS.GenDispatcherClass := TGenDispatcher.Create(DSS);
    DSS.StorageClass := TStorage.Create(DSS);
    DSS.StorageControllerClass := TStorageController.Create(DSS);
    DSS.RelayClass := TRelay.Create(DSS);
    DSS.RecloserClass := TRecloser.Create(DSS);
    DSS.FuseClass := TFuse.Create(DSS);
    DSS.SwtControlClass := TSwtControl.Create(DSS);
    DSS.PVSystemClass := TPVSystem.Create(DSS);
    DSS.UPFCClass := TUPFC.Create(DSS);
    DSS.UPFCControlClass := TUPFCControl.Create(DSS);
    DSS.ESPVLControlClass := TESPVLControl.Create(DSS);
    DSS.IndMach012Class := TIndMach012.Create(DSS);
    DSS.GICsourceClass := TGICsource.Create(DSS); // GIC source
    DSS.AutoTransClass := TAutoTrans.Create(DSS); // Auto Transformer
    DSS.InvControlClass := TInvControl.Create(DSS);
    DSS.ExpControlClass := TExpControl.Create(DSS);
    DSS.GICLineClass := TGICLine.Create(DSS);
    DSS.GICTransformerClass := TGICTransformer.Create(DSS);
    DSS.VSConverterClass := TVSConverter.Create(DSS);
    DSS.MonitorClass := TDSSMonitor.Create(DSS);  // Have to do this AFTER Generator
    DSS.EnergyMeterClass := TEnergyMeter.Create(DSS);  // Have to do this AFTER Generator
    DSS.SensorClass := TSensor.Create(DSS);      // Create state estimation sensors
    DSS.FMonitorClass := TFMonitor.Create(DSS);
    DSS.Generic5Class := TGeneric5.Create(DSS);

    DSS.NumIntrinsicClasses := DSS.DSSClassList.Count;

    DSS.SetPropertyNameStyle(PropNameStyle);
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
                TraceName := DSSObj.FullName();
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

    if Classref = 0 then
    begin
        DoSimpleMsg(DSS, Format(_('Error! Object Class "%s" not found.'), [ObjType]) + CRLF + DSS.Parser.CmdString(), 903);
        Result := FALSE;
        Exit;
    end;
    DSS.LastClassReferenced := Classref;
    Result := TRUE;
end;

function GetDSSClassPtr(DSS: TDSSContext; const ClassName: String): TDSSClass;
begin
    Result := TDSSClass(DSS.DSSClassList.Get(DSS.ClassNames.Find(AnsiLowerCase(ClassName))));
end;


end.
