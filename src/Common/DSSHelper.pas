unit DSSHelper;

interface 

uses 
    DSSClass,
    Circuit,
    Spectrum,
    LoadShape,
    TempShape,
    PriceShape,
    XYCurve,
    GrowthShape,
    Monitor,
    EnergyMeter,
    Sensor,
    TCC_Curve,
    WireData,
    CNData,
    TSData,
    LineSpacing,
    Storage,
    Storage2,
    PVSystem,
    PVSystem2,
    InvControl,
    InvControl2,
    ExpControl,
    LineCode,
    LineGeometry,
    Line,
    VSource,
    ISource,
    VCCS,
    Load,
    Transformer,
    RegControl,
    Capacitor,
    Reactor,
    CapControl,
    Fault,
    Generator,
    GenDispatcher,
    StorageController,
    StorageController2,
    Relay,
    Recloser,
    Fuse,
    SwtControl,
    UPFC,
    UPFCControl,
    ESPVLControl,
    IndMach012,
    GICsource, 
    AutoTrans, 
    VSConverter,
    XfmrCode,
    GICLine,
    GICTransformer,
    Solution, 
    // VVControl, 
    ConductorData,
    DSSObject,
    Executive,
    ControlProxy,
    ExportCIMXML;
    
type
   
    TDSSGlobalHelper = class helper for TDSSContext
    private
{$IFDEF DSS_CAPI_PM}
        function GetNumOfActors: Integer; inline;
        function GetActorThread: TSolver; inline;
{$ENDIF}
        function GetControlProxyObj: TControlProxyObj; inline;
        function GetDSSExecutive: TExecutive; inline;
        function GetCIMExporter: TCIMExporter; inline;
        function GetActiveDSSObject: TDSSObject; inline;
        function GetActiveCircuit: TDSSCircuit; inline;
        function GetLoadShapeClass: TLoadShape; inline;
        function GetTShapeClass: TTshape; inline;
        function GetPriceShapeClass: TPriceShape; inline;
        function GetXYCurveClass: TXYCurve; inline;
        function GetGrowthShapeClass: TGrowthShape; inline;
        function GetSpectrumClass: TSpectrum; inline;
        function GetEnergyMeterClass: TEnergyMeter; inline;
        function GetMonitorClass: TDSSMonitor; inline;
        function GetSensorClass: TSensor; inline;
        function GetTCC_CurveClass: TTCC_Curve; inline;
        function GetWireDataClass: TWireData; inline;
        function GetCNDataClass: TCNData; inline;
        function GetTSDataClass: TTSData; inline;
        function GetLineGeometryClass: TLineGeometry; inline;
        function GetLineSpacingClass: TLineSpacing; inline;
        function GetLineCodeClass: TLineCode; inline;
        function GetStorageClass: TStorage; inline;
        function GetStorage2Class: TStorage2; inline;
        function GetPVSystemClass: TPVSystem; inline;
        function GetPVSystem2Class: TPVSystem2; inline;
        function GetInvControlClass: TInvControl; inline;
        function GetInvControl2Class: TInvControl2; inline;
        function GetExpControlClass: TExpControl; inline;
        function GetLineClass: TLine; inline;
        function GetVSourceClass: TVSource; inline;
        function GetISourceClass: TISource; inline;
        function GetVCSSClass: TVCCS; inline;
        function GetLoadClass: TLoad; inline;
        function GetTransformerClass: TTransf; inline;
        function GetRegControlClass: TRegControl; inline;
        function GetCapacitorClass: TCapacitor; inline;
        function GetReactorClass: TReactor; inline;
        function GetCapControlClass: TCapControl; inline;
        function GetFaultClass: TFault; inline;
        function GetGeneratorClass: TGenerator; inline;
        function GetGenDispatcherClass: TGenDispatcher; inline;
        function GetStorageControllerClass: TStorageController; inline;
        function GetStorageController2Class: TStorageController2; inline;
        function GetRelayClass: TRelay; inline;
        function GetRecloserClass: TRecloser; inline;
        function GetFuseClass: TFuse; inline;
        function GetSwtControlClass: TSwtControl; inline;
        function GetUPFCClass: TUPFC; inline;
        function GetUPFCControlClass: TUPFCControl; inline;
        function GetESPVLControlClass: TESPVLControl; inline;
        function GetIndMach012Class: TIndMach012; inline;
        function GetGICsourceClass: TGICsource; inline;
        function GetAutoTransClass: TAutoTrans; inline;
        function GetVSConverterClass: TVSConverter; inline;
        function GetXfmrCodeClass: TXfmrCode; inline;
        function GetGICLineClass: TGICLine; inline;
        function GetGICTransformerClass:TGICTransformer; inline;
        
{$IFDEF DSS_CAPI_PM}
        procedure SetActorThread(val: TSolver); inline;
{$ENDIF}
        procedure SetDSSExecutive(val: TExecutive); inline;
        procedure SetCIMExporter(val: TCIMExporter); inline;
        procedure SetActiveDSSObject(val: TDSSObject); inline;
        procedure SetActiveCircuit(val: TDSSCircuit); inline;
        procedure SetLoadShapeClass(val: TLoadShape); inline;
        procedure SetTShapeClass(val: TTshape); inline;
        procedure SetPriceShapeClass(val: TPriceShape); inline;
        procedure SetXYCurveClass(val: TXYCurve); inline;
        procedure SetGrowthShapeClass(val: TGrowthShape); inline;
        procedure SetSpectrumClass(val: TSpectrum); inline;
        procedure SetEnergyMeterClass(val: TEnergyMeter); inline;
        procedure SetMonitorClass(val: TDSSMonitor); inline;
        procedure SetSensorClass(val: TSensor); inline;
        procedure SetTCC_CurveClass(val: TTCC_Curve); inline;
        procedure SetWireDataClass(val: TWireData); inline;
        procedure SetCNDataClass(val: TCNData); inline;
        procedure SetTSDataClass(val: TTSData); inline;
        procedure SetLineGeometryClass(val: TLineGeometry); inline;
        procedure SetLineSpacingClass(val: TLineSpacing); inline;
        procedure SetLineCodeClass(val: TLineCode); inline;
        procedure SetStorageClass(val: TStorage); inline;
        procedure SetStorage2Class(val: TStorage2); inline;
        procedure SetPVSystemClass(val: TPVSystem); inline;
        procedure SetPVSystem2Class(val: TPVSystem2); inline;
        procedure SetInvControlClass(val: TInvControl); inline;
        procedure SetInvControl2Class(val: TInvControl2); inline;
        procedure SetExpControlClass(val: TExpControl); inline;
        procedure SetLineClass(val: TLine); inline;
        procedure SetVSourceClass(val: TVSource); inline;
        procedure SetISourceClass(val: TISource); inline;
        procedure SetVCSSClass(val: TVCCS); inline;
        procedure SetLoadClass(val:  TLoad); inline;
        procedure SetTransformerClass(val: TTransf); inline;
        procedure SetRegControlClass(val: TRegControl); inline;
        procedure SetCapacitorClass(val: TCapacitor); inline;
        procedure SetReactorClass(val: TReactor); inline;
        procedure SetCapControlClass(val: TCapControl); inline;
        procedure SetFaultClass(val: TFault); inline;
        procedure SetGeneratorClass(val: TGenerator); inline;
        procedure SetGenDispatcherClass(val: TGenDispatcher); inline;
        procedure SetStorageControllerClass(val: TStorageController); inline;
        procedure SetStorageController2Class(val: TStorageController2); inline;
        procedure SetRelayClass(val: TRelay); inline;
        procedure SetRecloserClass(val: TRecloser); inline;
        procedure SetFuseClass(val: TFuse); inline;
        procedure SetSwtControlClass(val: TSwtControl); inline;
        procedure SetUPFCClass(val: TUPFC); inline;
        procedure SetUPFCControlClass(val: TUPFCControl); inline;
        procedure SetESPVLControlClass(val: TESPVLControl); inline;
        procedure SetIndMach012Class(val: TIndMach012); inline;
        procedure SetGICsourceClass(val: TGICsource); inline;
        procedure SetAutoTransClass(val: TAutoTrans); inline;
        procedure SetVSConverterClass(val: TVSConverter); inline;
        procedure SetXfmrCodeClass(val: TXfmrCode); inline;
        procedure SetGICLineClass(val: TGICLine); inline;
        procedure SetGICTransformerClass(val:TGICTransformer); inline;

        function GetActiveEnergyMeterObj: TEnergyMeterObj; inline;
        function GetActiveFaultObj: TFaultObj; inline;

        procedure SetActiveEnergyMeterObj(val: TEnergyMeterObj); inline;
        procedure SetActiveFaultObj(val: TFaultObj); inline;
        
    public
{$IFDEF DSS_CAPI_PM}
        property NumOfActors: Integer read GetNumOfActors;
        property ActorThread: TSolver read GetActorThread write SetActorThread;
{$ENDIF}
        property ControlProxyObj: TControlProxyObj read GetControlProxyObj;
        property DSSExecutive: TExecutive read GetDSSExecutive write SetDSSExecutive;
        property CIMExporter: TCIMExporter read GetCIMExporter write SetCIMExporter;

        property ActiveCircuit: TDSSCircuit read GetActiveCircuit write SetActiveCircuit;
        property ActiveDSSObject: TDSSObject read GetActiveDSSObject write SetActiveDSSObject;
        property ActiveFaultObj: TFaultObj read GetActiveFaultObj write SetActiveFaultObj;
        property ActiveEnergyMeterObj: TEnergyMeterObj read GetActiveEnergyMeterObj write SetActiveEnergyMeterObj;

        property LoadShapeClass: TLoadShape read GetLoadShapeClass write SetLoadShapeClass;
        property TShapeClass: TTshape read GetTShapeClass write SetTShapeClass;
        property PriceShapeClass: TPriceShape read GetPriceShapeClass write SetPriceShapeClass;
        property XYCurveClass: TXYCurve read GetXYCurveClass write SetXYCurveClass;
        property GrowthShapeClass: TGrowthShape read GetGrowthShapeClass write SetGrowthShapeClass;
        property SpectrumClass: TSpectrum read GetSpectrumClass write SetSpectrumClass;
        property EnergyMeterClass: TEnergyMeter read GetEnergyMeterClass write SetEnergyMeterClass;
        property MonitorClass: TDSSMonitor read GetMonitorClass write SetMonitorClass;
        property SensorClass: TSensor read GetSensorClass write SetSensorClass;
        property TCC_CurveClass: TTCC_Curve read GetTCC_CurveClass write SetTCC_CurveClass;
        property WireDataClass: TWireData read GetWireDataClass write SetWireDataClass;
        property CNDataClass: TCNData read GetCNDataClass write SetCNDataClass;
        property TSDataClass: TTSData read GetTSDataClass write SetTSDataClass;
        property LineGeometryClass: TLineGeometry read GetLineGeometryClass write SetLineGeometryClass;
        property LineSpacingClass: TLineSpacing read GetLineSpacingClass write SetLineSpacingClass;
        property LineCodeClass: TLineCode read GetLineCodeClass write SetLineCodeClass;
        property StorageClass: TStorage read GetStorageClass write SetStorageClass;
        property Storage2Class: TStorage2 read GetStorage2Class write SetStorage2Class;
        property PVSystemClass: TPVSystem read GetPVSystemClass write SetPVSystemClass;
        property PVSystem2Class: TPVSystem2 read GetPVSystem2Class write SetPVSystem2Class;
        property InvControlClass: TInvControl read GetInvControlClass write SetInvControlClass;
        property InvControl2Class: TInvControl2 read GetInvControl2Class write SetInvControl2Class;
        property ExpControlClass: TExpControl read GetExpControlClass write SetExpControlClass;
        property LineClass: TLine read GetLineClass write SetLineClass;
        property VSourceClass: TVSource read GetVSourceClass write SetVSourceClass;
        property ISourceClass: TISource read GetISourceClass write SetISourceClass;
        property VCSSClass: TVCCS read GetVCSSClass write SetVCSSClass;
        property LoadClass:  TLoad read GetLoadClass write SetLoadClass;
        property TransformerClass: TTransf read GetTransformerClass write SetTransformerClass;
        property RegControlClass: TRegControl read GetRegControlClass write SetRegControlClass;
        property CapacitorClass: TCapacitor read GetCapacitorClass write SetCapacitorClass;
        property ReactorClass: TReactor read GetReactorClass write SetReactorClass;
        property CapControlClass: TCapControl read GetCapControlClass write SetCapControlClass;
        property FaultClass: TFault read GetFaultClass write SetFaultClass;
        property GeneratorClass: TGenerator read GetGeneratorClass write SetGeneratorClass;
        property GenDispatcherClass: TGenDispatcher read GetGenDispatcherClass write SetGenDispatcherClass;
        property StorageControllerClass: TStorageController read GetStorageControllerClass write SetStorageControllerClass;
        property StorageController2Class: TStorageController2 read GetStorageController2Class write SetStorageController2Class;
        property RelayClass: TRelay read GetRelayClass write SetRelayClass;
        property RecloserClass: TRecloser read GetRecloserClass write SetRecloserClass;
        property FuseClass: TFuse read GetFuseClass write SetFuseClass;
        property SwtControlClass: TSwtControl read GetSwtControlClass write SetSwtControlClass;
        property UPFCClass: TUPFC read GetUPFCClass write SetUPFCClass;
        property UPFCControlClass: TUPFCControl read GetUPFCControlClass write SetUPFCControlClass;
        property ESPVLControlClass: TESPVLControl read GetESPVLControlClass write SetESPVLControlClass;
        property IndMach012Class: TIndMach012 read GetIndMach012Class write SetIndMach012Class;
        property GICsourceClass: TGICsource read GetGICsourceClass write SetGICsourceClass;
        property AutoTransClass: TAutoTrans read GetAutoTransClass write SetAutoTransClass;
        property VSConverterClass: TVSConverter read GetVSConverterClass write SetVSConverterClass;
        property XfmrCodeClass: TXfmrCode read GetXfmrCodeClass write SetXfmrCodeClass;
        property GICLineClass: TGICLine read GetGICLineClass write SetGICLineClass;
        property GICTransformerClass: TGICTransformer read GetGICTransformerClass write SetGICTransformerClass;
    end;    
    
implementation

{$IFDEF DSS_CAPI_PM}
function TDSSGlobalHelper.GetNumOfActors: Integer; begin Result := High(Children) + 1; end;
function TDSSGlobalHelper.GetActorThread: TSolver; begin Result := TSolver(FActorThread); end;
{$ENDIF}
function TDSSGlobalHelper.GetControlProxyObj: TControlProxyObj; begin Result := TControlProxyObj(FControlProxyObj); end;
function TDSSGlobalHelper.GetDSSExecutive: TExecutive; begin Result := TExecutive(FDSSExecutive); end;
function TDSSGlobalHelper.GetCIMExporter: TCIMExporter; begin Result := TCIMExporter(FCIMExporter); end;
function TDSSGlobalHelper.GetActiveDSSObject: TDSSObject; begin Result := TDSSObject(FActiveDSSObject); end;
function TDSSGlobalHelper.GetActiveCircuit: TDSSCircuit; begin Result := TDSSCircuit(FActiveCircuit); end;
function TDSSGlobalHelper.GetLoadShapeClass: TLoadShape; begin Result := TLoadShape(FLoadShapeClass); end;
function TDSSGlobalHelper.GetTShapeClass: TTshape; begin Result := TTshape(FTShapeClass); end;
function TDSSGlobalHelper.GetPriceShapeClass: TPriceShape; begin Result := TPriceShape(FPriceShapeClass); end;
function TDSSGlobalHelper.GetXYCurveClass: TXYCurve; begin Result := TXYCurve(FXYCurveClass); end;
function TDSSGlobalHelper.GetGrowthShapeClass: TGrowthShape; begin Result := TGrowthShape(FGrowthShapeClass); end;
function TDSSGlobalHelper.GetSpectrumClass: TSpectrum; begin Result := TSpectrum(FSpectrumClass); end;
function TDSSGlobalHelper.GetEnergyMeterClass: TEnergyMeter; begin Result := TEnergyMeter(FEnergyMeterClass); end;
function TDSSGlobalHelper.GetMonitorClass: TDSSMonitor; begin Result := TDSSMonitor(FMonitorClass); end;
function TDSSGlobalHelper.GetSensorClass: TSensor; begin Result := TSensor(FSensorClass); end;
function TDSSGlobalHelper.GetTCC_CurveClass: TTCC_Curve; begin Result := TTCC_Curve(FTCC_CurveClass); end;
function TDSSGlobalHelper.GetWireDataClass: TWireData; begin Result := TWireData(FWireDataClass); end;
function TDSSGlobalHelper.GetCNDataClass: TCNData; begin Result := TCNData(FCNDataClass); end;
function TDSSGlobalHelper.GetTSDataClass: TTSData; begin Result := TTSData(FTSDataClass); end;
function TDSSGlobalHelper.GetLineGeometryClass: TLineGeometry; begin Result := TLineGeometry(FLineGeometryClass); end;
function TDSSGlobalHelper.GetLineSpacingClass: TLineSpacing; begin Result := TLineSpacing(FLineSpacingClass); end;
function TDSSGlobalHelper.GetLineCodeClass: TLineCode; begin Result := TLineCode(FLineCodeClass); end;
function TDSSGlobalHelper.GetStorageClass: TStorage; begin Result := TStorage(FStorageClass); end;
function TDSSGlobalHelper.GetStorage2Class: TStorage2; begin Result := TStorage2(FStorage2Class); end;
function TDSSGlobalHelper.GetPVSystemClass: TPVSystem; begin Result := TPVSystem(FPVSystemClass); end;
function TDSSGlobalHelper.GetPVSystem2Class: TPVSystem2; begin Result := TPVSystem2(FPVSystem2Class); end;
function TDSSGlobalHelper.GetInvControlClass: TInvControl; begin Result := TInvControl(FInvControlClass); end;
function TDSSGlobalHelper.GetInvControl2Class: TInvControl2; begin Result := TInvControl2(FInvControl2Class); end;
function TDSSGlobalHelper.GetExpControlClass: TExpControl; begin Result := TExpControl(FExpControlClass); end;
function TDSSGlobalHelper.GetLineClass: TLine; begin Result := TLine(FLineClass); end;
function TDSSGlobalHelper.GetVSourceClass: TVSource; begin Result := TVSource(FVSourceClass); end;
function TDSSGlobalHelper.GetISourceClass: TISource; begin Result := TISource(FISourceClass); end;
function TDSSGlobalHelper.GetVCSSClass: TVCCS; begin Result := TVCCS(FVCSSClass); end;
function TDSSGlobalHelper.GetLoadClass:  TLoad; begin Result :=  TLoad(FLoadClass); end;
function TDSSGlobalHelper.GetTransformerClass: TTransf; begin Result := TTransf(FTransformerClass); end;
function TDSSGlobalHelper.GetRegControlClass: TRegControl; begin Result := TRegControl(FRegControlClass); end;
function TDSSGlobalHelper.GetCapacitorClass: TCapacitor; begin Result := TCapacitor(FCapacitorClass); end;
function TDSSGlobalHelper.GetReactorClass: TReactor; begin Result := TReactor(FReactorClass); end;
function TDSSGlobalHelper.GetCapControlClass: TCapControl; begin Result := TCapControl(FCapControlClass); end;
function TDSSGlobalHelper.GetFaultClass: TFault; begin Result := TFault(FFaultClass); end;
function TDSSGlobalHelper.GetGeneratorClass: TGenerator; begin Result := TGenerator(FGeneratorClass); end;
function TDSSGlobalHelper.GetGenDispatcherClass: TGenDispatcher; begin Result := TGenDispatcher(FGenDispatcherClass); end;
function TDSSGlobalHelper.GetStorageControllerClass: TStorageController; begin Result := TStorageController(FStorageControllerClass); end;
function TDSSGlobalHelper.GetStorageController2Class: TStorageController2; begin Result := TStorageController2(FStorageController2Class); end;
function TDSSGlobalHelper.GetRelayClass: TRelay; begin Result := TRelay(FRelayClass); end;
function TDSSGlobalHelper.GetRecloserClass: TRecloser; begin Result := TRecloser(FRecloserClass); end;
function TDSSGlobalHelper.GetFuseClass: TFuse; begin Result := TFuse(FFuseClass); end;
function TDSSGlobalHelper.GetSwtControlClass: TSwtControl; begin Result := TSwtControl(FSwtControlClass); end;
function TDSSGlobalHelper.GetUPFCClass: TUPFC; begin Result := TUPFC(FUPFCClass); end;
function TDSSGlobalHelper.GetUPFCControlClass: TUPFCControl; begin Result := TUPFCControl(FUPFCControlClass); end;
function TDSSGlobalHelper.GetESPVLControlClass: TESPVLControl; begin Result := TESPVLControl(FESPVLControlClass); end;
function TDSSGlobalHelper.GetIndMach012Class: TIndMach012; begin Result := TIndMach012(FIndMach012Class); end;
function TDSSGlobalHelper.GetGICsourceClass: TGICsource; begin Result := TGICsource(FGICsourceClass); end;
function TDSSGlobalHelper.GetAutoTransClass: TAutoTrans; begin Result := TAutoTrans(FAutoTransClass); end;
function TDSSGlobalHelper.GetVSConverterClass: TVSConverter; begin Result := TVSConverter(FVSConverterClass); end;
function TDSSGlobalHelper.GetXfmrCodeClass: TXfmrCode; begin Result := TXfmrCode(FXfmrCodeClass); end;
function TDSSGlobalHelper.GetGICLineClass: TGICLine; begin Result := TGICLine(FGICLineClass); end;
function TDSSGlobalHelper.GetGICTransformerClass: TGICTransformer; begin Result := TGICTransformer(FGICTransformerClass); end;

function TDSSGlobalHelper.GetActiveEnergyMeterObj: TEnergyMeterObj; begin Result := TEnergyMeterObj(FActiveEnergyMeterObj); end;
function TDSSGlobalHelper.GetActiveFaultObj: TFaultObj; begin Result := TFaultObj(FActiveFaultObj); end;


{$IFDEF DSS_CAPI_PM}
procedure TDSSGlobalHelper.SetActorThread(val: TSolver); begin FActorThread := val; end;
{$ENDIF}
procedure TDSSGlobalHelper.SetDSSExecutive(val: TExecutive); begin FDSSExecutive := val; end;
procedure TDSSGlobalHelper.SetCIMExporter(val: TCIMExporter); begin FCIMExporter := val; end;
procedure TDSSGlobalHelper.SetActiveDSSObject(val: TDSSObject); begin FActiveDSSObject := val; end;
procedure TDSSGlobalHelper.SetActiveCircuit(val: TDSSCircuit); begin FActiveCircuit := val; end;
procedure TDSSGlobalHelper.SetLoadShapeClass(val: TLoadShape); begin FLoadShapeClass := val; end;
procedure TDSSGlobalHelper.SetTShapeClass(val: TTshape); begin FTShapeClass := val; end;
procedure TDSSGlobalHelper.SetPriceShapeClass(val: TPriceShape); begin FPriceShapeClass := val; end;
procedure TDSSGlobalHelper.SetXYCurveClass(val: TXYCurve); begin FXYCurveClass := val; end;
procedure TDSSGlobalHelper.SetGrowthShapeClass(val: TGrowthShape); begin FGrowthShapeClass := val; end;
procedure TDSSGlobalHelper.SetSpectrumClass(val: TSpectrum); begin FSpectrumClass := val; end;
procedure TDSSGlobalHelper.SetEnergyMeterClass(val: TEnergyMeter); begin FEnergyMeterClass := val; end;
procedure TDSSGlobalHelper.SetMonitorClass(val: TDSSMonitor); begin FMonitorClass := val; end;
procedure TDSSGlobalHelper.SetSensorClass(val: TSensor); begin FSensorClass := val; end;
procedure TDSSGlobalHelper.SetTCC_CurveClass(val: TTCC_Curve); begin FTCC_CurveClass := val; end;
procedure TDSSGlobalHelper.SetWireDataClass(val: TWireData); begin FWireDataClass := val; end;
procedure TDSSGlobalHelper.SetCNDataClass(val: TCNData); begin FCNDataClass := val; end;
procedure TDSSGlobalHelper.SetTSDataClass(val: TTSData); begin FTSDataClass := val; end;
procedure TDSSGlobalHelper.SetLineGeometryClass(val: TLineGeometry); begin FLineGeometryClass := val; end;
procedure TDSSGlobalHelper.SetLineSpacingClass(val: TLineSpacing); begin FLineSpacingClass := val; end;
procedure TDSSGlobalHelper.SetLineCodeClass(val: TLineCode); begin FLineCodeClass := val; end;
procedure TDSSGlobalHelper.SetStorageClass(val: TStorage); begin FStorageClass := val; end;
procedure TDSSGlobalHelper.SetStorage2Class(val: TStorage2); begin FStorage2Class := val; end;
procedure TDSSGlobalHelper.SetPVSystemClass(val: TPVSystem); begin FPVSystemClass := val; end;
procedure TDSSGlobalHelper.SetPVSystem2Class(val: TPVSystem2); begin FPVSystem2Class := val; end;
procedure TDSSGlobalHelper.SetInvControlClass(val: TInvControl); begin FInvControlClass := val; end;
procedure TDSSGlobalHelper.SetInvControl2Class(val: TInvControl2); begin FInvControl2Class := val; end;
procedure TDSSGlobalHelper.SetExpControlClass(val: TExpControl); begin FExpControlClass := val; end;
procedure TDSSGlobalHelper.SetLineClass(val: TLine); begin FLineClass := val; end;
procedure TDSSGlobalHelper.SetVSourceClass(val: TVSource); begin FVSourceClass := val; end;
procedure TDSSGlobalHelper.SetISourceClass(val: TISource); begin FISourceClass := val; end;
procedure TDSSGlobalHelper.SetVCSSClass(val: TVCCS); begin FVCSSClass := val; end;
procedure TDSSGlobalHelper.SetLoadClass(val: TLoad); begin FLoadClass := val; end;
procedure TDSSGlobalHelper.SetTransformerClass(val: TTransf); begin FTransformerClass := val; end;
procedure TDSSGlobalHelper.SetRegControlClass(val: TRegControl); begin FRegControlClass := val; end;
procedure TDSSGlobalHelper.SetCapacitorClass(val: TCapacitor); begin FCapacitorClass := val; end;
procedure TDSSGlobalHelper.SetReactorClass(val: TReactor); begin FReactorClass := val; end;
procedure TDSSGlobalHelper.SetCapControlClass(val: TCapControl); begin FCapControlClass := val; end;
procedure TDSSGlobalHelper.SetFaultClass(val: TFault); begin FFaultClass := val; end;
procedure TDSSGlobalHelper.SetGeneratorClass(val: TGenerator); begin FGeneratorClass := val; end;
procedure TDSSGlobalHelper.SetGenDispatcherClass(val: TGenDispatcher); begin FGenDispatcherClass := val; end;
procedure TDSSGlobalHelper.SetStorageControllerClass(val: TStorageController); begin FStorageControllerClass := val; end;
procedure TDSSGlobalHelper.SetStorageController2Class(val: TStorageController2); begin FStorageController2Class := val; end;
procedure TDSSGlobalHelper.SetRelayClass(val: TRelay); begin FRelayClass := val; end;
procedure TDSSGlobalHelper.SetRecloserClass(val: TRecloser); begin FRecloserClass := val; end;
procedure TDSSGlobalHelper.SetFuseClass(val: TFuse); begin FFuseClass := val; end;
procedure TDSSGlobalHelper.SetSwtControlClass(val: TSwtControl); begin FSwtControlClass := val; end;
procedure TDSSGlobalHelper.SetUPFCClass(val: TUPFC); begin FUPFCClass := val; end;
procedure TDSSGlobalHelper.SetUPFCControlClass(val: TUPFCControl); begin FUPFCControlClass := val; end;
procedure TDSSGlobalHelper.SetESPVLControlClass(val: TESPVLControl); begin FESPVLControlClass := val; end;
procedure TDSSGlobalHelper.SetIndMach012Class(val: TIndMach012); begin FIndMach012Class := val; end;
procedure TDSSGlobalHelper.SetGICsourceClass(val: TGICsource); begin FGICsourceClass := val; end;
procedure TDSSGlobalHelper.SetAutoTransClass(val: TAutoTrans); begin FAutoTransClass := val; end;
procedure TDSSGlobalHelper.SetVSConverterClass(val: TVSConverter); begin FVSConverterClass := val; end;
procedure TDSSGlobalHelper.SetXfmrCodeClass(val: TXfmrCode); begin FXfmrCodeClass := val; end;
procedure TDSSGlobalHelper.SetGICLineClass(val: TGICLine); begin FGICLineClass := val; end;
procedure TDSSGlobalHelper.SetGICTransformerClass(val:TGICTransformer); begin FGICTransformerClass := val; end;

procedure TDSSGlobalHelper.SetActiveEnergyMeterObj(val: TEnergyMeterObj); begin FActiveEnergyMeterObj := val; end;
procedure TDSSGlobalHelper.SetActiveFaultObj(val: TFaultObj); begin FActiveFaultObj := val; end;


end.