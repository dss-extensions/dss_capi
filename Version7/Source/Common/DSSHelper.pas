unit DSSHelper;

interface 

uses 
    DSSClass,
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
    Feeder,
    WireData,
    CNData,
    TSData,
    LineSpacing,
    Storage,
    PVSystem,
    InvControl,
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
    GICTransformer;
    
type
    TDSSGlobalHelper = class helper for TDSS
    private
        function GetLoadShapeClass: TLoadShape; inline;
        function GetTShapeClass: TTshape; inline;
        function GetPriceShapeClass: TPriceShape; inline;
        function GetXYCurveClass: TXYCurve; inline;
        function GetGrowthShapeClass: TGrowthShape; inline;
        function GetSpectrumClass: TSpectrum; inline;
        function GetSolutionClass: TDSSClass; inline;
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
        function GetPVSystemClass: TPVSystem; inline;
        function GetInvControlClass: TInvControl; inline;
        function GetExpControlClass: TExpControl; inline;
        function GetLineClass: TLine; inline;
        function GetVSourceClass: TVSource; inline;
        function GetISourceClass: TISource; inline;
        function GetVCSSClass: TVCCS; inline;
        function GetLoadClass:  TLoad; inline;
        function GetTransformerClass: TTransf; inline;
        function GetRegControlClass: TRegControl; inline;
        function GetCapacitorClass: TCapacitor; inline;
        function GetReactorClass: TReactor; inline;
        function GetCapControlClass: TCapControl; inline;
        function GetFaultClass: TFault; inline;
        function GetGeneratorClass: TGenerator; inline;
        function GetGenDispatcherClass: TGenDispatcher; inline;
        function GetStorageControllerClass: TStorageController; inline;
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

        procedure SetLoadShapeClass(val: TLoadShape); inline;
        procedure SetTShapeClass(val: TTshape); inline;
        procedure SetPriceShapeClass(val: TPriceShape); inline;
        procedure SetXYCurveClass(val: TXYCurve); inline;
        procedure SetGrowthShapeClass(val: TGrowthShape); inline;
        procedure SetSpectrumClass(val: TSpectrum); inline;
        procedure SetSolutionClass(val: TDSSClass); inline;
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
        procedure SetPVSystemClass(val: TPVSystem); inline;
        procedure SetInvControlClass(val: TInvControl); inline;
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
        
    public
        property LoadShapeClass: TLoadShape read GetLoadShapeClass write SetLoadShapeClass;
        property TShapeClass: TTshape read GetTShapeClass write SetTShapeClass;
        property PriceShapeClass: TPriceShape read GetPriceShapeClass write SetPriceShapeClass;
        property XYCurveClass: TXYCurve read GetXYCurveClass write SetXYCurveClass;
        property GrowthShapeClass: TGrowthShape read GetGrowthShapeClass write SetGrowthShapeClass;
        property SpectrumClass: TSpectrum read GetSpectrumClass write SetSpectrumClass;
        property SolutionClass: TDSSClass read GetSolutionClass write SetSolutionClass;
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
        property PVSystemClass: TPVSystem read GetPVSystemClass write SetPVSystemClass;
        property InvControlClass: TInvControl read GetInvControlClass write SetInvControlClass;
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

function TDSSGlobalHelper.GetLoadShapeClass: TLoadShape; begin Result := TLoadShape(FLoadShapeClass); end;
function TDSSGlobalHelper.GetTShapeClass: TTshape; begin Result := TTshape(FTShapeClass); end;
function TDSSGlobalHelper.GetPriceShapeClass: TPriceShape; begin Result := TPriceShape(FPriceShapeClass); end;
function TDSSGlobalHelper.GetXYCurveClass: TXYCurve; begin Result := TXYCurve(FXYCurveClass); end;
function TDSSGlobalHelper.GetGrowthShapeClass: TGrowthShape; begin Result := TGrowthShape(FGrowthShapeClass); end;
function TDSSGlobalHelper.GetSpectrumClass: TSpectrum; begin Result := TSpectrum(FSpectrumClass); end;
function TDSSGlobalHelper.GetSolutionClass: TDSSClass; begin Result := TDSSClass(FSolutionClass); end;
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
function TDSSGlobalHelper.GetPVSystemClass: TPVSystem; begin Result := TPVSystem(FPVSystemClass); end;
function TDSSGlobalHelper.GetInvControlClass: TInvControl; begin Result := TInvControl(FInvControlClass); end;
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

procedure TDSSGlobalHelper.SetLoadShapeClass(val: TLoadShape); begin FLoadShapeClass := val; end;
procedure TDSSGlobalHelper.SetTShapeClass(val: TTshape); begin FTShapeClass := val; end;
procedure TDSSGlobalHelper.SetPriceShapeClass(val: TPriceShape); begin FPriceShapeClass := val; end;
procedure TDSSGlobalHelper.SetXYCurveClass(val: TXYCurve); begin FXYCurveClass := val; end;
procedure TDSSGlobalHelper.SetGrowthShapeClass(val: TGrowthShape); begin FGrowthShapeClass := val; end;
procedure TDSSGlobalHelper.SetSpectrumClass(val: TSpectrum); begin FSpectrumClass := val; end;
procedure TDSSGlobalHelper.SetSolutionClass(val: TDSSClass); begin FSolutionClass := val; end;
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
procedure TDSSGlobalHelper.SetPVSystemClass(val: TPVSystem); begin FPVSystemClass := val; end;
procedure TDSSGlobalHelper.SetInvControlClass(val: TInvControl); begin FInvControlClass := val; end;
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

end.