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
    GICTransformer,
    Solution, 
    // VVControl, 
    ConductorData,
    DSSObject,
    Executive;
    
type
    TDSSGlobalHelper = class helper for TDSS
    private
        function GetDSSExecutive: TExecutive; inline;
        function GetActiveDSSObject: TDSSObject; inline;
        function GetActiveCircuit: TDSSCircuit; inline;
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
        
        procedure SetDSSExecutive(val: TExecutive); inline;
        procedure SetActiveDSSObject(val: TDSSObject); inline;
        procedure SetActiveCircuit(val: TDSSCircuit); inline;
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

        function GetActiveFeederObj: TFeederObj; inline;
        function GetActiveSolutionObj: TSolutionObj; inline;
        function GetActiveCapControlObj: TCapControlObj; inline;
        function GetActiveESPVLControlObj: TESPVLControlObj; inline;
        function GetActiveExpControlObj: TExpControlObj; inline;
        function GetActiveGenDispatcherObj: TGenDispatcherObj; inline;
        function GetActiveInvControlObj: TInvControlObj; inline;
        function GetActiveRecloserObj: TRecloserObj; inline;
        function GetActiveRegControlObj: TRegControlObj; inline;
        function GetActiveRelayObj: TRelayObj; inline;
        function GetActiveStorageControllerObj: TStorageControllerObj; inline;
        function GetActiveSwtControlObj: TSwtControlObj; inline;
        function GetActiveUPFCControlObj: TUPFCControlObj; inline;
        // function GetActiveVVCControlObj: TVVControlObj; inline;
        function GetActiveConductorDataObj: TConductorDataObj; inline;
        function GetActiveGrowthShapeObj: TGrowthShapeObj; inline;
        function GetActiveLineCodeObj: TLineCodeObj; inline;
        function GetActiveLineGeometryObj: TLineGeometryObj; inline;
        function GetActiveLineSpacingObj: TLineSpacingObj; inline;
        function GetActiveLoadShapeObj: TLoadShapeObj; inline;
        function GetActivePriceShapeObj: TPriceShapeObj; inline;
        function GetActiveSpectrumObj: TSpectrumObj; inline;
        function GetActiveTCC_CurveObj: TTCC_CurveObj; inline;
        function GetActiveTShapeObj: TTShapeObj; inline;
        function GetActiveXfmrCodeObj: TXfmrCodeObj; inline;
        function GetActiveXYcurveObj: TXYcurveObj; inline;
        function GetActiveEnergyMeterObj: TEnergyMeterObj; inline;
        // function GetActiveFMonitorObj: TFMonitorObj; inline;
        function GetActiveMonitorObj: TMonitorObj; inline;
        function GetActiveSensorObj: TSensorObj; inline;
        // function GetActiveEquivalentObj: TEquivalentObj; inline;
        function GetActiveGeneratorObj: TGeneratorObj; inline;
        // function GetActiveGeneric5Obj: TGeneric5Obj; inline;
        function GetActiveGICLineObj: TGICLineObj; inline;
        function GetActiveGICsourceObj: TGICSourceObj; inline;
        function GetActiveIndMach012Obj: TIndMach012Obj; inline;
        function GetActiveIsourceObj: TIsourceObj; inline;
        function GetActiveLoadObj: TLoadObj; inline;
        function GetActivePVsystemObj: TPVsystemObj; inline;
        function GetActiveStorageObj: TStorageObj; inline;
        function GetActiveUPFCObj: TUPFCObj; inline;
        function GetActiveVCCSObj: TVCCSObj; inline;
        function GetActiveVSConverterObj: TVSConverterObj; inline;
        function GetActiveVsourceObj: TVsourceObj; inline;
        function GetActiveAutoTransObj: TAutoTransObj; inline;
        function GetActiveCapacitorObj: TCapacitorObj; inline;
        function GetActiveFaultObj: TFaultObj; inline;
        function GetActiveFuseObj: TFuseObj; inline;
        function GetActiveGICTransformerObj: TGICTransformerObj; inline;
        function GetActiveLineObj: TLineObj; inline;
        function GetActiveReactorObj: TReactorObj; inline;
        function GetActiveTransfObj: TTransfObj; inline;

        procedure SetActiveFeederObj(val: TFeederObj); inline;
        procedure SetActiveSolutionObj(val: TSolutionObj); inline;
        procedure SetActiveCapControlObj(val: TCapControlObj); inline;
        procedure SetActiveESPVLControlObj(val: TESPVLControlObj); inline;
        procedure SetActiveExpControlObj(val: TExpControlObj); inline;
        procedure SetActiveGenDispatcherObj(val: TGenDispatcherObj); inline;
        procedure SetActiveInvControlObj(val: TInvControlObj); inline;
        procedure SetActiveRecloserObj(val: TRecloserObj); inline;
        procedure SetActiveRegControlObj(val: TRegControlObj); inline;
        procedure SetActiveRelayObj(val: TRelayObj); inline;
        procedure SetActiveStorageControllerObj(val: TStorageControllerObj); inline;
        procedure SetActiveSwtControlObj(val: TSwtControlObj); inline;
        procedure SetActiveUPFCControlObj(val: TUPFCControlObj); inline;
        // procedure SetActiveVVCControlObj(val: TVVControlObj); inline;
        procedure SetActiveConductorDataObj(val: TConductorDataObj); inline;
        procedure SetActiveGrowthShapeObj(val: TGrowthShapeObj); inline;
        procedure SetActiveLineCodeObj(val: TLineCodeObj); inline;
        procedure SetActiveLineGeometryObj(val: TLineGeometryObj); inline;
        procedure SetActiveLineSpacingObj(val: TLineSpacingObj); inline;
        procedure SetActiveLoadShapeObj(val: TLoadShapeObj); inline;
        procedure SetActivePriceShapeObj(val: TPriceShapeObj); inline;
        procedure SetActiveSpectrumObj(val: TSpectrumObj); inline;
        procedure SetActiveTCC_CurveObj(val: TTCC_CurveObj); inline;
        procedure SetActiveTShapeObj(val: TTShapeObj); inline;
        procedure SetActiveXfmrCodeObj(val: TXfmrCodeObj); inline;
        procedure SetActiveXYcurveObj(val: TXYcurveObj); inline;
        procedure SetActiveEnergyMeterObj(val: TEnergyMeterObj); inline;
        // procedure SetActiveFMonitorObj(val: TFMonitorObj); inline;
        procedure SetActiveMonitorObj(val: TMonitorObj); inline;
        procedure SetActiveSensorObj(val: TSensorObj); inline;
        // procedure SetActiveEquivalentObj(val: TEquivalentObj); inline;
        procedure SetActiveGeneratorObj(val: TGeneratorObj); inline;
        // procedure SetActiveGeneric5Obj(val: TGeneric5Obj); inline;
        procedure SetActiveGICLineObj(val: TGICLineObj); inline;
        procedure SetActiveGICsourceObj(val: TGICSourceObj); inline;
        procedure SetActiveIndMach012Obj(val: TIndMach012Obj); inline;
        procedure SetActiveIsourceObj(val: TIsourceObj); inline;
        procedure SetActiveLoadObj(val: TLoadObj); inline;
        procedure SetActivePVsystemObj(val: TPVsystemObj); inline;
        procedure SetActiveStorageObj(val: TStorageObj); inline;
        procedure SetActiveUPFCObj(val: TUPFCObj); inline;
        procedure SetActiveVCCSObj(val: TVCCSObj); inline;
        procedure SetActiveVSConverterObj(val: TVSConverterObj); inline;
        procedure SetActiveVsourceObj(val: TVsourceObj); inline;
        procedure SetActiveAutoTransObj(val: TAutoTransObj); inline;
        procedure SetActiveCapacitorObj(val: TCapacitorObj); inline;
        procedure SetActiveFaultObj(val: TFaultObj); inline;
        procedure SetActiveFuseObj(val: TFuseObj); inline;
        procedure SetActiveGICTransformerObj(val: TGICTransformerObj); inline;
        procedure SetActiveLineObj(val: TLineObj); inline;
        procedure SetActiveReactorObj(val: TReactorObj); inline;
        procedure SetActiveTransfObj(val: TTransfObj); inline;
        
    public
        property DSSExecutive: TExecutive read GetDSSExecutive write SetDSSExecutive;
        property ActiveCircuit: TDSSCircuit read GetActiveCircuit write SetActiveCircuit;
        property ActiveDSSObject: TDSSObject read GetActiveDSSObject write SetActiveDSSObject;
        
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
        
        property ActiveFeederObj: TFeederObj read GetActiveFeederObj write SetActiveFeederObj;
        property ActiveSolutionObj: TSolutionObj read GetActiveSolutionObj write SetActiveSolutionObj;
        property ActiveCapControlObj: TCapControlObj read GetActiveCapControlObj write SetActiveCapControlObj;
        property ActiveESPVLControlObj: TESPVLControlObj read GetActiveESPVLControlObj write SetActiveESPVLControlObj;
        property ActiveExpControlObj: TExpControlObj read GetActiveExpControlObj write SetActiveExpControlObj;
        property ActiveGenDispatcherObj: TGenDispatcherObj read GetActiveGenDispatcherObj write SetActiveGenDispatcherObj;
        property ActiveInvControlObj: TInvControlObj read GetActiveInvControlObj write SetActiveInvControlObj;
        property ActiveRecloserObj: TRecloserObj read GetActiveRecloserObj write SetActiveRecloserObj;
        property ActiveRegControlObj: TRegControlObj read GetActiveRegControlObj write SetActiveRegControlObj;
        property ActiveRelayObj: TRelayObj read GetActiveRelayObj write SetActiveRelayObj;
        property ActiveStorageControllerObj: TStorageControllerObj read GetActiveStorageControllerObj write SetActiveStorageControllerObj;
        property ActiveSwtControlObj: TSwtControlObj read GetActiveSwtControlObj write SetActiveSwtControlObj;
        property ActiveUPFCControlObj: TUPFCControlObj read GetActiveUPFCControlObj write SetActiveUPFCControlObj;
        // property ActiveVVCControlObj: TVVControlObj read GetActiveVVCControlObj write SetActiveVVCControlObj;
        property ActiveConductorDataObj: TConductorDataObj read GetActiveConductorDataObj write SetActiveConductorDataObj;
        property ActiveGrowthShapeObj: TGrowthShapeObj read GetActiveGrowthShapeObj write SetActiveGrowthShapeObj;
        property ActiveLineCodeObj: TLineCodeObj read GetActiveLineCodeObj write SetActiveLineCodeObj;
        property ActiveLineGeometryObj: TLineGeometryObj read GetActiveLineGeometryObj write SetActiveLineGeometryObj;
        property ActiveLineSpacingObj: TLineSpacingObj read GetActiveLineSpacingObj write SetActiveLineSpacingObj;
        property ActiveLoadShapeObj: TLoadShapeObj read GetActiveLoadShapeObj write SetActiveLoadShapeObj;
        property ActivePriceShapeObj: TPriceShapeObj read GetActivePriceShapeObj write SetActivePriceShapeObj;
        property ActiveSpectrumObj: TSpectrumObj read GetActiveSpectrumObj write SetActiveSpectrumObj;
        property ActiveTCC_CurveObj: TTCC_CurveObj read GetActiveTCC_CurveObj write SetActiveTCC_CurveObj;
        property ActiveTShapeObj: TTShapeObj read GetActiveTShapeObj write SetActiveTShapeObj;
        property ActiveXfmrCodeObj: TXfmrCodeObj read GetActiveXfmrCodeObj write SetActiveXfmrCodeObj;
        property ActiveXYcurveObj: TXYcurveObj read GetActiveXYcurveObj write SetActiveXYcurveObj;
        property ActiveEnergyMeterObj: TEnergyMeterObj read GetActiveEnergyMeterObj write SetActiveEnergyMeterObj;
        // property ActiveFMonitorObj: TFMonitorObj read GetActiveFMonitorObj write SetActiveFMonitorObj;
        property ActiveMonitorObj: TMonitorObj read GetActiveMonitorObj write SetActiveMonitorObj;
        property ActiveSensorObj: TSensorObj read GetActiveSensorObj write SetActiveSensorObj;
        // property ActiveEquivalentObj: TEquivalentObj read GetActiveEquivalentObj write SetActiveEquivalentObj;
        property ActiveGeneratorObj: TGeneratorObj read GetActiveGeneratorObj write SetActiveGeneratorObj;
        // property ActiveGeneric5Obj: TGeneric5Obj read GetActiveGeneric5Obj write SetActiveGeneric5Obj;
        property ActiveGICLineObj: TGICLineObj read GetActiveGICLineObj write SetActiveGICLineObj;
        property ActiveGICsourceObj: TGICSourceObj read GetActiveGICsourceObj write SetActiveGICsourceObj;
        property ActiveIndMach012Obj: TIndMach012Obj read GetActiveIndMach012Obj write SetActiveIndMach012Obj;
        property ActiveIsourceObj: TIsourceObj read GetActiveIsourceObj write SetActiveIsourceObj;
        property ActiveLoadObj: TLoadObj read GetActiveLoadObj write SetActiveLoadObj;
        property ActivePVsystemObj: TPVsystemObj read GetActivePVsystemObj write SetActivePVsystemObj;
        property ActiveStorageObj: TStorageObj read GetActiveStorageObj write SetActiveStorageObj;
        property ActiveUPFCObj: TUPFCObj read GetActiveUPFCObj write SetActiveUPFCObj;
        property ActiveVCCSObj: TVCCSObj read GetActiveVCCSObj write SetActiveVCCSObj;
        property ActiveVSConverterObj: TVSConverterObj read GetActiveVSConverterObj write SetActiveVSConverterObj;
        property ActiveVsourceObj: TVsourceObj read GetActiveVsourceObj write SetActiveVsourceObj;
        property ActiveAutoTransObj: TAutoTransObj read GetActiveAutoTransObj write SetActiveAutoTransObj;
        property ActiveCapacitorObj: TCapacitorObj read GetActiveCapacitorObj write SetActiveCapacitorObj;
        property ActiveFaultObj: TFaultObj read GetActiveFaultObj write SetActiveFaultObj;
        property ActiveFuseObj: TFuseObj read GetActiveFuseObj write SetActiveFuseObj;
        property ActiveGICTransformerObj: TGICTransformerObj read GetActiveGICTransformerObj write SetActiveGICTransformerObj;
        property ActiveLineObj: TLineObj read GetActiveLineObj write SetActiveLineObj;
        property ActiveReactorObj: TReactorObj read GetActiveReactorObj write SetActiveReactorObj;
        property ActiveTransfObj: TTransfObj read GetActiveTransfObj write SetActiveTransfObj;
        
    end;    
    
implementation

function TDSSGlobalHelper.GetDSSExecutive: TExecutive; begin Result := TExecutive(FDSSExecutive); end;
function TDSSGlobalHelper.GetActiveDSSObject: TDSSObject; begin Result := TDSSObject(FActiveDSSObject); end;
function TDSSGlobalHelper.GetActiveCircuit: TDSSCircuit; begin Result := TDSSCircuit(FActiveCircuit); end;
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

function TDSSGlobalHelper.GetActiveFeederObj: TFeederObj; begin Result := TFeederObj(FActiveFeederObj); end;
function TDSSGlobalHelper.GetActiveSolutionObj: TSolutionObj; begin Result := TSolutionObj(FActiveSolutionObj); end;
function TDSSGlobalHelper.GetActiveCapControlObj: TCapControlObj; begin Result := TCapControlObj(FActiveCapControlObj); end;
function TDSSGlobalHelper.GetActiveESPVLControlObj: TESPVLControlObj; begin Result := TESPVLControlObj(FActiveESPVLControlObj); end;
function TDSSGlobalHelper.GetActiveExpControlObj: TExpControlObj; begin Result := TExpControlObj(FActiveExpControlObj); end;
function TDSSGlobalHelper.GetActiveGenDispatcherObj: TGenDispatcherObj; begin Result := TGenDispatcherObj(FActiveGenDispatcherObj); end;
function TDSSGlobalHelper.GetActiveInvControlObj: TInvControlObj; begin Result := TInvControlObj(FActiveInvControlObj); end;
function TDSSGlobalHelper.GetActiveRecloserObj: TRecloserObj; begin Result := TRecloserObj(FActiveRecloserObj); end;
function TDSSGlobalHelper.GetActiveRegControlObj: TRegControlObj; begin Result := TRegControlObj(FActiveRegControlObj); end;
function TDSSGlobalHelper.GetActiveRelayObj: TRelayObj; begin Result := TRelayObj(FActiveRelayObj); end;
function TDSSGlobalHelper.GetActiveStorageControllerObj: TStorageControllerObj; begin Result := TStorageControllerObj(FActiveStorageControllerObj); end;
function TDSSGlobalHelper.GetActiveSwtControlObj: TSwtControlObj; begin Result := TSwtControlObj(FActiveSwtControlObj); end;
function TDSSGlobalHelper.GetActiveUPFCControlObj: TUPFCControlObj; begin Result := TUPFCControlObj(FActiveUPFCControlObj); end;
// function TDSSGlobalHelper.ActiveGetVVCControlObj: TVVControlObj; begin Result := TVVCControlObj(FActiveVVCControlObj); end;
function TDSSGlobalHelper.GetActiveConductorDataObj: TConductorDataObj; begin Result := TConductorDataObj(FActiveConductorDataObj); end;
function TDSSGlobalHelper.GetActiveGrowthShapeObj: TGrowthShapeObj; begin Result := TGrowthShapeObj(FActiveGrowthShapeObj); end;
function TDSSGlobalHelper.GetActiveLineCodeObj: TLineCodeObj; begin Result := TLineCodeObj(FActiveLineCodeObj); end;
function TDSSGlobalHelper.GetActiveLineGeometryObj: TLineGeometryObj; begin Result := TLineGeometryObj(FActiveLineGeometryObj); end;
function TDSSGlobalHelper.GetActiveLineSpacingObj: TLineSpacingObj; begin Result := TLineSpacingObj(FActiveLineSpacingObj); end;
function TDSSGlobalHelper.GetActiveLoadShapeObj: TLoadShapeObj; begin Result := TLoadShapeObj(FActiveLoadShapeObj); end;
function TDSSGlobalHelper.GetActivePriceShapeObj: TPriceShapeObj; begin Result := TPriceShapeObj(FActivePriceShapeObj); end;
function TDSSGlobalHelper.GetActiveSpectrumObj: TSpectrumObj; begin Result := TSpectrumObj(FActiveSpectrumObj); end;
function TDSSGlobalHelper.GetActiveTCC_CurveObj: TTCC_CurveObj; begin Result := TTCC_CurveObj(FActiveTCC_CurveObj); end;
function TDSSGlobalHelper.GetActiveTShapeObj: TTShapeObj; begin Result := TTShapeObj(FActiveTShapeObj); end;
function TDSSGlobalHelper.GetActiveXfmrCodeObj: TXfmrCodeObj; begin Result := TXfmrCodeObj(FActiveXfmrCodeObj); end;
function TDSSGlobalHelper.GetActiveXYcurveObj: TXYcurveObj; begin Result := TXYcurveObj(FActiveXYcurveObj); end;
function TDSSGlobalHelper.GetActiveEnergyMeterObj: TEnergyMeterObj; begin Result := TEnergyMeterObj(FActiveEnergyMeterObj); end;
// function TDSSGlobalHelper.GetActiveFMonitorObj: TFMonitorObj; begin Result := TFMonitorObj(FActiveFMonitorObj); end;
function TDSSGlobalHelper.GetActiveMonitorObj: TMonitorObj; begin Result := TMonitorObj(FActiveMonitorObj); end;
function TDSSGlobalHelper.GetActiveSensorObj: TSensorObj; begin Result := TSensorObj(FActiveSensorObj); end;
// function TDSSGlobalHelper.ActiveGetEquivalentObj: TEquivalentObj; begin Result := TEquivalentObj(FActiveEquivalentObj); end;
function TDSSGlobalHelper.GetActiveGeneratorObj: TGeneratorObj; begin Result := TGeneratorObj(FActiveGeneratorObj); end;
// function TDSSGlobalHelper.ActiveGetGeneric5Obj: TGeneric5Obj; begin Result := TGeneric5Obj(FActiveGeneric5Obj); end;
function TDSSGlobalHelper.GetActiveGICLineObj: TGICLineObj; begin Result := TGICLineObj(FActiveGICLineObj); end;
function TDSSGlobalHelper.GetActiveGICsourceObj: TGICSourceObj; begin Result := TGICsourceObj(FActiveGICsourceObj); end;
function TDSSGlobalHelper.GetActiveIndMach012Obj: TIndMach012Obj; begin Result := TIndMach012Obj(FActiveIndMach012Obj); end;
function TDSSGlobalHelper.GetActiveIsourceObj: TIsourceObj; begin Result := TIsourceObj(FActiveIsourceObj); end;
function TDSSGlobalHelper.GetActiveLoadObj: TLoadObj; begin Result := TLoadObj(FActiveLoadObj); end;
function TDSSGlobalHelper.GetActivePVsystemObj: TPVsystemObj; begin Result := TPVsystemObj(FActivePVsystemObj); end;
function TDSSGlobalHelper.GetActiveStorageObj: TStorageObj; begin Result := TStorageObj(FActiveStorageObj); end;
function TDSSGlobalHelper.GetActiveUPFCObj: TUPFCObj; begin Result := TUPFCObj(FActiveUPFCObj); end;
function TDSSGlobalHelper.GetActiveVCCSObj: TVCCSObj; begin Result := TVCCSObj(FActiveVCCSObj); end;
function TDSSGlobalHelper.GetActiveVSConverterObj: TVSConverterObj; begin Result := TVSConverterObj(FActiveVSConverterObj); end;
function TDSSGlobalHelper.GetActiveVsourceObj: TVsourceObj; begin Result := TVsourceObj(FActiveVsourceObj); end;
function TDSSGlobalHelper.GetActiveAutoTransObj: TAutoTransObj; begin Result := TAutoTransObj(FActiveAutoTransObj); end;
function TDSSGlobalHelper.GetActiveCapacitorObj: TCapacitorObj; begin Result := TCapacitorObj(FActiveCapacitorObj); end;
function TDSSGlobalHelper.GetActiveFaultObj: TFaultObj; begin Result := TFaultObj(FActiveFaultObj); end;
function TDSSGlobalHelper.GetActiveFuseObj: TFuseObj; begin Result := TFuseObj(FActiveFuseObj); end;
function TDSSGlobalHelper.GetActiveGICTransformerObj: TGICTransformerObj; begin Result := TGICTransformerObj(FActiveGICTransformerObj); end;
function TDSSGlobalHelper.GetActiveLineObj: TLineObj; begin Result := TLineObj(FActiveLineObj); end;
function TDSSGlobalHelper.GetActiveReactorObj: TReactorObj; begin Result := TReactorObj(FActiveReactorObj); end;
function TDSSGlobalHelper.GetActiveTransfObj: TTransfObj; begin Result := TTransfObj(FActiveTransfObj); end;


procedure TDSSGlobalHelper.SetDSSExecutive(val: TExecutive); begin FDSSExecutive := val; end;
procedure TDSSGlobalHelper.SetActiveDSSObject(val: TDSSObject); begin FActiveDSSObject := val; end;
procedure TDSSGlobalHelper.SetActiveCircuit(val: TDSSCircuit); begin FActiveCircuit := val; end;
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

procedure TDSSGlobalHelper.SetActiveFeederObj(val: TFeederObj); begin FActiveFeederObj := val; end;
procedure TDSSGlobalHelper.SetActiveSolutionObj(val: TSolutionObj); begin FActiveSolutionObj := val; end;
procedure TDSSGlobalHelper.SetActiveCapControlObj(val: TCapControlObj); begin FActiveCapControlObj := val; end;
procedure TDSSGlobalHelper.SetActiveESPVLControlObj(val: TESPVLControlObj); begin FActiveESPVLControlObj := val; end;
procedure TDSSGlobalHelper.SetActiveExpControlObj(val: TExpControlObj); begin FActiveExpControlObj := val; end;
procedure TDSSGlobalHelper.SetActiveGenDispatcherObj(val: TGenDispatcherObj); begin FActiveGenDispatcherObj := val; end;
procedure TDSSGlobalHelper.SetActiveInvControlObj(val: TInvControlObj); begin FActiveInvControlObj := val; end;
procedure TDSSGlobalHelper.SetActiveRecloserObj(val: TRecloserObj); begin FActiveRecloserObj := val; end;
procedure TDSSGlobalHelper.SetActiveRegControlObj(val: TRegControlObj); begin FActiveRegControlObj := val; end;
procedure TDSSGlobalHelper.SetActiveRelayObj(val: TRelayObj); begin FActiveRelayObj := val; end;
procedure TDSSGlobalHelper.SetActiveStorageControllerObj(val: TStorageControllerObj); begin FActiveStorageControllerObj := val; end;
procedure TDSSGlobalHelper.SetActiveSwtControlObj(val: TSwtControlObj); begin FActiveSwtControlObj := val; end;
procedure TDSSGlobalHelper.SetActiveUPFCControlObj(val: TUPFCControlObj); begin FActiveUPFCControlObj := val; end;
// procedure TDSSGlobalHelper.SetActiveVVCControlObj(val: TVVControlObj); begin FActiveVVCControlObj := val; end;
procedure TDSSGlobalHelper.SetActiveConductorDataObj(val: TConductorDataObj); begin FActiveConductorDataObj := val; end;
procedure TDSSGlobalHelper.SetActiveGrowthShapeObj(val: TGrowthShapeObj); begin FActiveGrowthShapeObj := val; end;
procedure TDSSGlobalHelper.SetActiveLineCodeObj(val: TLineCodeObj); begin FActiveLineCodeObj := val; end;
procedure TDSSGlobalHelper.SetActiveLineGeometryObj(val: TLineGeometryObj); begin FActiveLineGeometryObj := val; end;
procedure TDSSGlobalHelper.SetActiveLineSpacingObj(val: TLineSpacingObj); begin FActiveLineSpacingObj := val; end;
procedure TDSSGlobalHelper.SetActiveLoadShapeObj(val: TLoadShapeObj); begin FActiveLoadShapeObj := val; end;
procedure TDSSGlobalHelper.SetActivePriceShapeObj(val: TPriceShapeObj); begin FActivePriceShapeObj := val; end;
procedure TDSSGlobalHelper.SetActiveSpectrumObj(val: TSpectrumObj); begin FActiveSpectrumObj := val; end;
procedure TDSSGlobalHelper.SetActiveTCC_CurveObj(val: TTCC_CurveObj); begin FActiveTCC_CurveObj := val; end;
procedure TDSSGlobalHelper.SetActiveTShapeObj(val: TTShapeObj); begin FActiveTShapeObj := val; end;
procedure TDSSGlobalHelper.SetActiveXfmrCodeObj(val: TXfmrCodeObj); begin FActiveXfmrCodeObj := val; end;
procedure TDSSGlobalHelper.SetActiveXYcurveObj(val: TXYcurveObj); begin FActiveXYcurveObj := val; end;
procedure TDSSGlobalHelper.SetActiveEnergyMeterObj(val: TEnergyMeterObj); begin FActiveEnergyMeterObj := val; end;
// procedure TDSSGlobalHelper.SetActiveFMonitorObj(val: TFMonitorObj); begin FActiveFMonitorObj := val; end;
procedure TDSSGlobalHelper.SetActiveMonitorObj(val: TMonitorObj); begin FActiveMonitorObj := val; end;
procedure TDSSGlobalHelper.SetActiveSensorObj(val: TSensorObj); begin FActiveSensorObj := val; end;
// procedure TDSSGlobalHelper.SetActiveEquivalentObj(val: TEquivalentObj); begin FActiveEquivalentObj := val; end;
procedure TDSSGlobalHelper.SetActiveGeneratorObj(val: TGeneratorObj); begin FActiveGeneratorObj := val; end;
// procedure TDSSGlobalHelper.SetActiveGeneric5Obj(val: TGeneric5Obj); begin FActiveGeneric5Obj := val; end;
procedure TDSSGlobalHelper.SetActiveGICLineObj(val: TGICLineObj); begin FActiveGICLineObj := val; end;
procedure TDSSGlobalHelper.SetActiveGICsourceObj(val: TGICSourceObj); begin FActiveGICsourceObj := val; end;
procedure TDSSGlobalHelper.SetActiveIndMach012Obj(val: TIndMach012Obj); begin FActiveIndMach012Obj := val; end;
procedure TDSSGlobalHelper.SetActiveIsourceObj(val: TIsourceObj); begin FActiveIsourceObj := val; end;
procedure TDSSGlobalHelper.SetActiveLoadObj(val: TLoadObj); begin FActiveLoadObj := val; end;
procedure TDSSGlobalHelper.SetActivePVsystemObj(val: TPVsystemObj); begin FActivePVsystemObj := val; end;
procedure TDSSGlobalHelper.SetActiveStorageObj(val: TStorageObj); begin FActiveStorageObj := val; end;
procedure TDSSGlobalHelper.SetActiveUPFCObj(val: TUPFCObj); begin FActiveUPFCObj := val; end;
procedure TDSSGlobalHelper.SetActiveVCCSObj(val: TVCCSObj); begin FActiveVCCSObj := val; end;
procedure TDSSGlobalHelper.SetActiveVSConverterObj(val: TVSConverterObj); begin FActiveVSConverterObj := val; end;
procedure TDSSGlobalHelper.SetActiveVsourceObj(val: TVsourceObj); begin FActiveVsourceObj := val; end;
procedure TDSSGlobalHelper.SetActiveAutoTransObj(val: TAutoTransObj); begin FActiveAutoTransObj := val; end;
procedure TDSSGlobalHelper.SetActiveCapacitorObj(val: TCapacitorObj); begin FActiveCapacitorObj := val; end;
procedure TDSSGlobalHelper.SetActiveFaultObj(val: TFaultObj); begin FActiveFaultObj := val; end;
procedure TDSSGlobalHelper.SetActiveFuseObj(val: TFuseObj); begin FActiveFuseObj := val; end;
procedure TDSSGlobalHelper.SetActiveGICTransformerObj(val: TGICTransformerObj); begin FActiveGICTransformerObj := val; end;
procedure TDSSGlobalHelper.SetActiveLineObj(val: TLineObj); begin FActiveLineObj := val; end;
procedure TDSSGlobalHelper.SetActiveReactorObj(val: TReactorObj); begin FActiveReactorObj := val; end;
procedure TDSSGlobalHelper.SetActiveTransfObj(val: TTransfObj); begin FActiveTransfObj := val; end;


end.