unit OpenDSSengine_TLB;

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// $Rev: 52393 $
// File generated on 1/4/2019 5:37:56 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\OpenDSS\Version8\Source\DLL\OpenDSSengine (1)
// LIBID: {8BFDE413-245A-4514-B151-B16DCC243796}
// LCID: 0
// Helpfile:
// HelpString: OpenDSS Engine
// DepndLst:
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
//   (2) v1.0 stdole, (stdole32.tlb)
// SYS_KIND: SYS_WIN32
// Errors:
//   Hint: Member 'Class' of 'ILoads' changed to 'Class_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses
    Winapi.Windows,
    System.Classes,
    System.Variants,
    System.Win.StdVCL,
    Vcl.Graphics,
    Vcl.OleServer,
    Winapi.ActiveX;


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
    OpenDSSengineMajorVersion = 1;
    OpenDSSengineMinorVersion = 0;

    LIBID_OpenDSSengine: TGUID = '{8BFDE413-245A-4514-B151-B16DCC243796}';

    IID_IText: TGUID = '{0513A8DC-2C0D-4648-8BD7-2130B82C05FA}';
    CLASS_Text: TGUID = '{6E20BC4C-67C0-4AD3-9E12-BF90C478A1CC}';
    IID_IDSSProperty: TGUID = '{1298D126-0750-4B2A-8462-62EFE7310DF2}';
    CLASS_DSSProperty: TGUID = '{F8410F14-7E85-44A9-B42F-F900DF5F596E}';
    IID_ICktElement: TGUID = '{F20E8327-5B60-478E-8DBD-5EFC75EB929B}';
    CLASS_CktElement: TGUID = '{BC5F55A3-7A0F-4923-B218-098A91F482D8}';
    IID_IError: TGUID = '{B521E339-8ED2-4BD6-9AEB-FD349CA8D8E3}';
    CLASS_Error: TGUID = '{0038D0EB-28ED-42B0-A247-E212E05ADF4B}';
    IID_ICircuit: TGUID = '{32441C6D-7A27-4164-B5B0-FA054300C217}';
    CLASS_Circuit: TGUID = '{B5B695B1-A1F5-444F-ABC5-836B7EF1DF0D}';
    IID_IBus: TGUID = '{E5B78C35-88F8-495F-8CD1-EBB5D90ED228}';
    CLASS_Bus: TGUID = '{A14C32E4-846B-444D-9070-F7A31E9F5FF9}';
    IID_IDSS: TGUID = '{14644AD0-4909-48FF-B624-24E8C38D1AED}';
    CLASS_DSS: TGUID = '{6FE9D1B8-C064-4877-94C0-F13882ADBDB6}';
    IID_ISolution: TGUID = '{F2332365-962A-4DF4-9D1E-218E0B0F2CEF}';
    CLASS_Solution: TGUID = '{F799E1DE-E7BF-4F86-BCED-6DD01FD00419}';
    IID_IMonitors: TGUID = '{5C339E44-C583-445C-91D1-3B1E49CAD6B0}';
    CLASS_Monitors: TGUID = '{7FF93D6F-4258-40CB-9558-0792422309F3}';
    IID_IMeters: TGUID = '{86705B6C-352A-47F8-A24B-78B750EC3859}';
    CLASS_Meters: TGUID = '{F869D5BB-A023-48AB-A459-01444585B7C1}';
    IID_IGenerators: TGUID = '{2D9B7548-D03E-478A-9FEA-9FC4033C793E}';
    CLASS_Generators: TGUID = '{65F232C9-7D95-4E45-B9FA-40F518CFBB64}';
    IID_IDSSProgress: TGUID = '{315C0C38-929C-4942-BDF8-6DA12D001B47}';
    CLASS_DSSProgress: TGUID = '{4CB900D9-DD2F-41AF-9E48-B999E0AED0A7}';
    IID_ISettings: TGUID = '{4E3928A0-8B75-4127-885F-F4AD6B3F4323}';
    CLASS_Settings: TGUID = '{9D910AA4-0CB3-4907-AEEF-8DD79A58C0AD}';
    IID_ILines: TGUID = '{E1616BDB-589B-4E5D-A7CE-828ACD73E5D4}';
    CLASS_Lines: TGUID = '{A1352870-9D53-4E48-B83A-6DB0C8FED65B}';
    IID_ICtrlQueue: TGUID = '{55055001-5EEC-4667-9CCA-63F3A60F31F3}';
    CLASS_CtrlQueue: TGUID = '{19DD7174-7FEE-4E59-97ED-C54F16EDC3F0}';
    IID_ILoads: TGUID = '{9A3FFA05-5B82-488C-B08D-FCA2FDB23101}';
    CLASS_Loads: TGUID = '{1302A34B-A554-4C32-BCED-4AF0A94FF114}';
    IID_IDSSElement: TGUID = '{C22D4922-6DC2-4283-93AB-4F2138C4B922}';
    CLASS_DSSElement: TGUID = '{09D4B4AB-DF58-4F8F-A3F0-72F32830B337}';
    IID_IActiveClass: TGUID = '{8E73B64C-0D99-4D19-AB90-170DBBD06FA0}';
    CLASS_ActiveClass: TGUID = '{2A02BB33-50A4-4C87-86E0-59EF7738F86C}';
    IID_ICapacitors: TGUID = '{3C171A69-40AB-46AA-B037-9C4EBB9FBFCD}';
    CLASS_Capacitors: TGUID = '{F733F571-4CEC-45CC-922D-16C2BEEBA5BC}';
    IID_ITransformers: TGUID = '{94E9CACF-A548-4DC2-B460-E2642B501387}';
    IID_ISwtControls: TGUID = '{112AB9E6-C112-46BE-A8A3-F72C5FA3A657}';
    IID_ICapControls: TGUID = '{4C132096-4161-4D9B-A701-E6CCCFF1D5AE}';
    IID_IRegControls: TGUID = '{3F983AD2-B658-4CE8-B4C1-DE0A9EDD47FD}';
    CLASS_Transformers: TGUID = '{3A3E2154-1249-4DBB-AEDC-C4C14300D332}';
    CLASS_SwtControls: TGUID = '{7D8F53AE-0D61-4B87-9BEE-12D54052F689}';
    CLASS_CapControls: TGUID = '{7D95304E-B0A8-4531-8D1B-F438287EEA6E}';
    CLASS_RegControls: TGUID = '{D3DBDE53-6397-4C36-8C87-9BEA061FBC78}';
    IID_ITopology: TGUID = '{03FADB98-4F30-416E-ACD2-9BD987A0CBC3}';
    CLASS_Topology: TGUID = '{5B1B5AB3-0595-4E46-B64B-CF8877ED0857}';
    IID_IDSS_Executive: TGUID = '{DD7B80E9-5EFB-4E79-96CA-9C88F5A8A11C}';
    CLASS_DSS_Executive: TGUID = '{D00898D0-6CC7-4A3B-BF89-DED9593579E7}';
    IID_IDSSEvents: TGUID = '{3F5A5530-4E67-44BF-AE6D-561584C6BF47}';
    DIID_IDSSEventsEvents: TGUID = '{AE501F77-F7F0-4201-A9AD-6AB385262203}';
    CLASS_DSSEvents: TGUID = '{B734843A-08E4-42D3-9E24-C0D5F7BF6487}';
    IID_ISensors: TGUID = '{E7444ECD-B491-4D8E-A1E3-E5804BD571E2}';
    CLASS_Sensors: TGUID = '{FC54E9AA-1C6A-4CF8-837D-82B257D98E5A}';
    IID_IXYCurves: TGUID = '{97AA7680-E994-4A0C-BAC3-9B67BA49825C}';
    CLASS_XYCurves: TGUID = '{9594F37D-E47E-4701-892B-52BE7E576E87}';
    IID_IPDElements: TGUID = '{05D4E15E-1588-4ABB-8339-3527420C668B}';
    CLASS_PDElements: TGUID = '{4DDCDADD-A1D3-40BB-98E7-B023BD3947BE}';
    IID_IReclosers: TGUID = '{21001789-9F46-4323-93B0-8B31395FD6E4}';
    CLASS_Reclosers: TGUID = '{B92B059F-FEFD-4554-8F07-AFDCEFBBEA7B}';
    IID_IRelays: TGUID = '{76956697-6055-4E8E-B4D6-650805D3F90D}';
    CLASS_Relays: TGUID = '{9D887EEA-7454-4214-BC56-AC42F5A3318E}';
    IID_ICmathLib: TGUID = '{2B649EC0-FA89-45ED-A937-E7CB47806A3A}';
    CLASS_CmathLib: TGUID = '{76847D49-B650-4850-9486-E08B48F87E39}';
    IID_IParser: TGUID = '{9714FED4-9D39-4692-B76B-9A18F206A934}';
    CLASS_Parser: TGUID = '{2245AD88-CB0E-4426-9DF2-5B2F89B2A08D}';
    IID_ILoadShapes: TGUID = '{196861FB-38C6-4FB4-B8A5-B2DDA3DDA663}';
    CLASS_LoadShapes: TGUID = '{4FCBE090-AA15-4E31-A8C7-E5F42D41C90C}';
    IID_IFuses: TGUID = '{AABE4DA8-3D5A-447F-AFFB-78946BA68DA5}';
    CLASS_Fuses: TGUID = '{ABED90F5-3908-408A-87EF-D0582FD2FFD5}';
    IID_IISources: TGUID = '{CB2C7310-1717-4C6E-A7B8-DA54CF1722CD}';
    CLASS_ISources: TGUID = '{CE35EBD2-BDD4-4B01-AE88-1D90DC82F619}';
    IID_IDSSimComs: TGUID = '{25C5373D-5888-4A0C-974B-77EBD57ED0D1}';
    CLASS_DSSimComs: TGUID = '{2104B607-8D58-4BBD-85B8-4E5F1C8BD6BE}';
    IID_IPVSystems: TGUID = '{FAF19717-5887-43F6-8DC3-D0337E1081AD}';
    CLASS_PVSystems: TGUID = '{D8D7592D-D5CD-4E27-870D-00D654DF2D3C}';
    IID_IVsources: TGUID = '{8DCD1962-268B-40E1-B49E-B7C01C3E07CD}';
    CLASS_Vsources: TGUID = '{0823B8BD-AD34-452B-974A-F46BA25D49EA}';
    IID_IParallel: TGUID = '{A0351633-A988-4A5B-B551-A7E2ADDD4984}';
    CLASS_Parallel: TGUID = '{D967764D-CD38-41ED-B1FD-7D79DC78EFCD}';
    IID_ILineCodes: TGUID = '{519DBEAC-F1D5-4770-A890-3C8A7BB5E54D}';
    CLASS_LineCodes: TGUID = '{0657EE75-F8CF-41D0-8672-2ADACD195591}';
    IID_IGICSources: TGUID = '{9CD30253-86C0-4339-B86E-745C912E8B15}';
    CLASS_GICSources: TGUID = '{D8715F4F-25F5-4EF8-A6F8-8E790628702C}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library
// *********************************************************************//
// Constants for enum MonitorModes
type
    MonitorModes = TOleEnum;

const
    dssVI = $00000000;
    dssPower = $00000001;
    dssSequence = $00000010;
    dssMagnitude = $00000020;
    dssPosOnly = $00000040;
    dssTaps = $00000002;
    dssStates = $00000003;

// Constants for enum SolveModes
type
    SolveModes = TOleEnum;

const
    dssSnapShot = $00000000;
    dssDutyCycle = $00000006;
    dssDirect = $00000007;
    dssDaily = $00000001;
    dssMonte1 = $00000003;
    dssMonte2 = $0000000A;
    dssMonte3 = $0000000B;
    dssFaultStudy = $00000009;
    dssYearly = $00000002;
    dssMonteFault = $00000008;
    dssPeakDay = $00000005;
    dssLD1 = $00000004;
    dssLD2 = $0000000C;
    dssAutoAdd = $0000000D;
    dssHarmonic = $0000000F;
    dssDynamic = $0000000E;

// Constants for enum Options
type
    Options = TOleEnum;

const
    dssPowerFlow = $00000001;
    dssAdmittance = $00000002;
    dssNormalSolve = $00000000;
    dssNewtonSolve = $00000001;
    dssStatic = $00000000;
    dssEvent = $00000001;
    dssTime = $00000002;
    dssMultiphase = $00000000;
    dssPositiveSeq = $00000001;
    dssGaussian = $00000001;
    dssUniform = $00000002;
    dssLogNormal = $00000003;
    dssAddGen = $00000001;
    dssAddCap = $00000002;
    dssControlOFF = $FFFFFFFF;

// Constants for enum CapControlModes
type
    CapControlModes = TOleEnum;

const
    dssCapControlVoltage = $00000001;
    dssCapControlKVAR = $00000002;
    dssCapControlCurrent = $00000000;
    dssCapControlPF = $00000004;
    dssCapControlTime = $00000003;

// Constants for enum ActionCodes
type
    ActionCodes = TOleEnum;

const
    dssActionNone = $00000000;
    dssActionOpen = $00000001;
    dssActionClose = $00000002;
    dssActionReset = $00000003;
    dssActionLock = $00000004;
    dssActionUnlock = $00000005;
    dssActionTapUp = $00000006;
    dssActionTapDown = $00000007;

// Constants for enum LoadStatus
type
    LoadStatus = TOleEnum;

const
    dssLoadVariable = $00000000;
    dssLoadFixed = $00000001;
    dssLoadExempt = $00000002;

// Constants for enum LoadModels
type
    LoadModels = TOleEnum;

const
    dssLoadConstPQ = $00000001;
    dssLoadConstZ = $00000002;
    dssLoadMotor = $00000003;
    dssLoadCVR = $00000004;
    dssLoadConstI = $00000005;
    dssLoadConstPFixedQ = $00000006;
    dssLoadConstPFixedX = $00000007;
    dssLoadZIPV = $00000008;

// Constants for enum LineUnits
type
    LineUnits = TOleEnum;

const
    dssLineUnitsNone = $00000000;
    dssLineUnitsMiles = $00000001;
    dssLineUnitskFt = $00000002;
    dssLineUnitskm = $00000003;
    dssLineUnitsmeter = $00000004;
    dssLineUnitsft = $00000005;
    dssLineUnitsinch = $00000006;
    dssLineUnitscm = $00000007;
    dssLineUnitsmm = $00000008;
    dssLineUnitsMaxnum = $00000009;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
    IText = interface;
    ITextDisp = dispinterface;
    IDSSProperty = interface;
    IDSSPropertyDisp = dispinterface;
    ICktElement = interface;
    ICktElementDisp = dispinterface;
    IError = interface;
    IErrorDisp = dispinterface;
    ICircuit = interface;
    ICircuitDisp = dispinterface;
    IBus = interface;
    IBusDisp = dispinterface;
    IDSS = interface;
    IDSSDisp = dispinterface;
    ISolution = interface;
    ISolutionDisp = dispinterface;
    IMonitors = interface;
    IMonitorsDisp = dispinterface;
    IMeters = interface;
    IMetersDisp = dispinterface;
    IGenerators = interface;
    IGeneratorsDisp = dispinterface;
    IDSSProgress = interface;
    IDSSProgressDisp = dispinterface;
    ISettings = interface;
    ISettingsDisp = dispinterface;
    ILines = interface;
    ILinesDisp = dispinterface;
    ICtrlQueue = interface;
    ICtrlQueueDisp = dispinterface;
    ILoads = interface;
    ILoadsDisp = dispinterface;
    IDSSElement = interface;
    IDSSElementDisp = dispinterface;
    IActiveClass = interface;
    IActiveClassDisp = dispinterface;
    ICapacitors = interface;
    ICapacitorsDisp = dispinterface;
    ITransformers = interface;
    ITransformersDisp = dispinterface;
    ISwtControls = interface;
    ISwtControlsDisp = dispinterface;
    ICapControls = interface;
    ICapControlsDisp = dispinterface;
    IRegControls = interface;
    IRegControlsDisp = dispinterface;
    ITopology = interface;
    ITopologyDisp = dispinterface;
    IDSS_Executive = interface;
    IDSS_ExecutiveDisp = dispinterface;
    IDSSEvents = interface;
    IDSSEventsDisp = dispinterface;
    IDSSEventsEvents = dispinterface;
    ISensors = interface;
    ISensorsDisp = dispinterface;
    IXYCurves = interface;
    IXYCurvesDisp = dispinterface;
    IPDElements = interface;
    IPDElementsDisp = dispinterface;
    IReclosers = interface;
    IReclosersDisp = dispinterface;
    IRelays = interface;
    IRelaysDisp = dispinterface;
    ICmathLib = interface;
    ICmathLibDisp = dispinterface;
    IParser = interface;
    IParserDisp = dispinterface;
    ILoadShapes = interface;
    ILoadShapesDisp = dispinterface;
    IFuses = interface;
    IFusesDisp = dispinterface;
    IISources = interface;
    IISourcesDisp = dispinterface;
    IDSSimComs = interface;
    IDSSimComsDisp = dispinterface;
    IPVSystems = interface;
    IPVSystemsDisp = dispinterface;
    IVsources = interface;
    IVsourcesDisp = dispinterface;
    IParallel = interface;
    IParallelDisp = dispinterface;
    ILineCodes = interface;
    ILineCodesDisp = dispinterface;
    IGICSources = interface;
    IGICSourcesDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
    Text = IText;
    DSSProperty = IDSSProperty;
    CktElement = ICktElement;
    Error = IError;
    Circuit = ICircuit;
    Bus = IBus;
    DSS = IDSS;
    Solution = ISolution;
    Monitors = IMonitors;
    Meters = IMeters;
    Generators = IGenerators;
    DSSProgress = IDSSProgress;
    Settings = ISettings;
    Lines = ILines;
    CtrlQueue = ICtrlQueue;
    Loads = ILoads;
    DSSElement = IDSSElement;
    ActiveClass = IActiveClass;
    Capacitors = ICapacitors;
    Transformers = ITransformers;
    SwtControls = ISwtControls;
    CapControls = ICapControls;
    RegControls = IRegControls;
    Topology = ITopology;
    DSS_Executive = IDSS_Executive;
    DSSEvents = IDSSEvents;
    Sensors = ISensors;
    XYCurves = IXYCurves;
    PDElements = IPDElements;
    Reclosers = IReclosers;
    Relays = IRelays;
    CmathLib = ICmathLib;
    Parser = IParser;
    LoadShapes = ILoadShapes;
    Fuses = IFuses;
    ISources = IISources;
    DSSimComs = IDSSimComs;
    PVSystems = IPVSystems;
    Vsources = IVsources;
    Parallel = IParallel;
    LineCodes = ILineCodes;
    GICSources = IGICSources;


// *********************************************************************//
// Interface: IText
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {0513A8DC-2C0D-4648-8BD7-2130B82C05FA}
// *********************************************************************//
    IText = interface(IDispatch)
        ['{0513A8DC-2C0D-4648-8BD7-2130B82C05FA}']
        function Get_Command: Widestring; SAFECALL;
        procedure Set_Command(const Command: Widestring); SAFECALL;
        function Get_Result: Widestring; SAFECALL;
        property Command: Widestring READ Get_Command WRITE Set_Command;
        property Result: Widestring READ Get_Result;
    end;

// *********************************************************************//
// DispIntf:  ITextDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {0513A8DC-2C0D-4648-8BD7-2130B82C05FA}
// *********************************************************************//
    ITextDisp = dispinterface
        ['{0513A8DC-2C0D-4648-8BD7-2130B82C05FA}']
        property Command: Widestring DISPID 1;
        property Result: Widestring READONLY DISPID 2;
    end;

// *********************************************************************//
// Interface: IDSSProperty
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {1298D126-0750-4B2A-8462-62EFE7310DF2}
// *********************************************************************//
    IDSSProperty = interface(IDispatch)
        ['{1298D126-0750-4B2A-8462-62EFE7310DF2}']
        function Get_Name: Widestring; SAFECALL;
        function Get_Description: Widestring; SAFECALL;
        function Get_Val: Widestring; SAFECALL;
        procedure Set_Val(const Value: Widestring); SAFECALL;
        property Name: Widestring READ Get_Name;
        property Description: Widestring READ Get_Description;
        property Val: Widestring READ Get_Val WRITE Set_Val;
    end;

// *********************************************************************//
// DispIntf:  IDSSPropertyDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {1298D126-0750-4B2A-8462-62EFE7310DF2}
// *********************************************************************//
    IDSSPropertyDisp = dispinterface
        ['{1298D126-0750-4B2A-8462-62EFE7310DF2}']
        property Name: Widestring READONLY DISPID 1;
        property Description: Widestring READONLY DISPID 3;
        property Val: Widestring DISPID 2;
    end;

// *********************************************************************//
// Interface: ICktElement
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F20E8327-5B60-478E-8DBD-5EFC75EB929B}
// *********************************************************************//
    ICktElement = interface(IDispatch)
        ['{F20E8327-5B60-478E-8DBD-5EFC75EB929B}']
        function Get_Name: Widestring; SAFECALL;
        function Get_NumTerminals: Integer; SAFECALL;
        function Get_NumConductors: Integer; SAFECALL;
        function Get_NumPhases: Integer; SAFECALL;
        function Get_BusNames: Olevariant; SAFECALL;
        procedure Set_BusNames(Value: Olevariant); SAFECALL;
        function Get_Properties(Indx: Olevariant): IDSSProperty; SAFECALL;
        function Get_Voltages: Olevariant; SAFECALL;
        function Get_Currents: Olevariant; SAFECALL;
        function Get_Powers: Olevariant; SAFECALL;
        function Get_Losses: Olevariant; SAFECALL;
        function Get_PhaseLosses: Olevariant; SAFECALL;
        function Get_SeqVoltages: Olevariant; SAFECALL;
        function Get_SeqCurrents: Olevariant; SAFECALL;
        function Get_SeqPowers: Olevariant; SAFECALL;
        function Get_Enabled: Wordbool; SAFECALL;
        procedure Set_Enabled(Value: Wordbool); SAFECALL;
        function Get_NormalAmps: Double; SAFECALL;
        procedure Set_NormalAmps(Value: Double); SAFECALL;
        function Get_EmergAmps: Double; SAFECALL;
        procedure Set_EmergAmps(Value: Double); SAFECALL;
        procedure Open(Term: Integer; Phs: Integer); SAFECALL;
        procedure Close(Term: Integer; Phs: Integer); SAFECALL;
        function IsOpen(Term: Integer; Phs: Integer): Wordbool; SAFECALL;
        function Get_NumProperties: Integer; SAFECALL;
        function Get_AllPropertyNames: Olevariant; SAFECALL;
        function Get_Residuals: Olevariant; SAFECALL;
        function Get_Yprim: Olevariant; SAFECALL;
        function Get_DisplayName: Widestring; SAFECALL;
        procedure Set_DisplayName(const Value: Widestring); SAFECALL;
        function Get_Handle: Integer; SAFECALL;
        function Get_GUID: Widestring; SAFECALL;
        function Get_HasSwitchControl: Wordbool; SAFECALL;
        function Get_HasVoltControl: Wordbool; SAFECALL;
        function Get_EnergyMeter: Widestring; SAFECALL;
        function Get_Controller(idx: Integer): Widestring; SAFECALL;
        function Get_CplxSeqVoltages: Olevariant; SAFECALL;
        function Get_CplxSeqCurrents: Olevariant; SAFECALL;
        function Get_AllVariableNames: Olevariant; SAFECALL;
        function Get_AllVariableValues: Olevariant; SAFECALL;
        function Get_Variable(const MyVarName: Widestring; out Code: Integer): Double; SAFECALL;
        function Get_Variablei(Idx: Integer; out Code: Integer): Double; SAFECALL;
        function Get_NodeOrder: Olevariant; SAFECALL;
        function Get_HasOCPDevice: Wordbool; SAFECALL;
        function Get_NumControls: Integer; SAFECALL;
        function Get_OCPDevIndex: Integer; SAFECALL;
        function Get_OCPDevType: Integer; SAFECALL;
        function Get_CurrentsMagAng: Olevariant; SAFECALL;
        function Get_VoltagesMagAng: Olevariant; SAFECALL;
        property Name: Widestring READ Get_Name;
        property NumTerminals: Integer READ Get_NumTerminals;
        property NumConductors: Integer READ Get_NumConductors;
        property NumPhases: Integer READ Get_NumPhases;
        property BusNames: Olevariant READ Get_BusNames WRITE Set_BusNames;
        property Properties[Indx: Olevariant]: IDSSProperty READ Get_Properties;
        property Voltages: Olevariant READ Get_Voltages;
        property Currents: Olevariant READ Get_Currents;
        property Powers: Olevariant READ Get_Powers;
        property Losses: Olevariant READ Get_Losses;
        property PhaseLosses: Olevariant READ Get_PhaseLosses;
        property SeqVoltages: Olevariant READ Get_SeqVoltages;
        property SeqCurrents: Olevariant READ Get_SeqCurrents;
        property SeqPowers: Olevariant READ Get_SeqPowers;
        property Enabled: Wordbool READ Get_Enabled WRITE Set_Enabled;
        property NormalAmps: Double READ Get_NormalAmps WRITE Set_NormalAmps;
        property EmergAmps: Double READ Get_EmergAmps WRITE Set_EmergAmps;
        property NumProperties: Integer READ Get_NumProperties;
        property AllPropertyNames: Olevariant READ Get_AllPropertyNames;
        property Residuals: Olevariant READ Get_Residuals;
        property Yprim: Olevariant READ Get_Yprim;
        property DisplayName: Widestring READ Get_DisplayName WRITE Set_DisplayName;
        property Handle: Integer READ Get_Handle;
        property GUID: Widestring READ Get_GUID;
        property HasSwitchControl: Wordbool READ Get_HasSwitchControl;
        property HasVoltControl: Wordbool READ Get_HasVoltControl;
        property EnergyMeter: Widestring READ Get_EnergyMeter;
        property Controller[idx: Integer]: Widestring READ Get_Controller;
        property CplxSeqVoltages: Olevariant READ Get_CplxSeqVoltages;
        property CplxSeqCurrents: Olevariant READ Get_CplxSeqCurrents;
        property AllVariableNames: Olevariant READ Get_AllVariableNames;
        property AllVariableValues: Olevariant READ Get_AllVariableValues;
        property Variable[const MyVarName: Widestring; out Code: Integer]: Double READ Get_Variable;
        property Variablei[Idx: Integer; out Code: Integer]: Double READ Get_Variablei;
        property NodeOrder: Olevariant READ Get_NodeOrder;
        property HasOCPDevice: Wordbool READ Get_HasOCPDevice;
        property NumControls: Integer READ Get_NumControls;
        property OCPDevIndex: Integer READ Get_OCPDevIndex;
        property OCPDevType: Integer READ Get_OCPDevType;
        property CurrentsMagAng: Olevariant READ Get_CurrentsMagAng;
        property VoltagesMagAng: Olevariant READ Get_VoltagesMagAng;
    end;

// *********************************************************************//
// DispIntf:  ICktElementDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F20E8327-5B60-478E-8DBD-5EFC75EB929B}
// *********************************************************************//
    ICktElementDisp = dispinterface
        ['{F20E8327-5B60-478E-8DBD-5EFC75EB929B}']
        property Name: Widestring READONLY DISPID 1;
        property NumTerminals: Integer READONLY DISPID 2;
        property NumConductors: Integer READONLY DISPID 3;
        property NumPhases: Integer READONLY DISPID 4;
        property BusNames: Olevariant DISPID 5;
        property Properties[Indx: Olevariant]: IDSSProperty READONLY DISPID 6;
        property Voltages: Olevariant READONLY DISPID 7;
        property Currents: Olevariant READONLY DISPID 8;
        property Powers: Olevariant READONLY DISPID 9;
        property Losses: Olevariant READONLY DISPID 10;
        property PhaseLosses: Olevariant READONLY DISPID 11;
        property SeqVoltages: Olevariant READONLY DISPID 12;
        property SeqCurrents: Olevariant READONLY DISPID 13;
        property SeqPowers: Olevariant READONLY DISPID 14;
        property Enabled: Wordbool DISPID 15;
        property NormalAmps: Double DISPID 16;
        property EmergAmps: Double DISPID 17;
        procedure Open(Term: Integer; Phs: Integer); DISPID 18;
        procedure Close(Term: Integer; Phs: Integer); DISPID 19;
        function IsOpen(Term: Integer; Phs: Integer): Wordbool; DISPID 20;
        property NumProperties: Integer READONLY DISPID 21;
        property AllPropertyNames: Olevariant READONLY DISPID 22;
        property Residuals: Olevariant READONLY DISPID 23;
        property Yprim: Olevariant READONLY DISPID 24;
        property DisplayName: Widestring DISPID 201;
        property Handle: Integer READONLY DISPID 202;
        property GUID: Widestring READONLY DISPID 203;
        property HasSwitchControl: Wordbool READONLY DISPID 204;
        property HasVoltControl: Wordbool READONLY DISPID 205;
        property EnergyMeter: Widestring READONLY DISPID 206;
        property Controller[idx: Integer]: Widestring READONLY DISPID 207;
        property CplxSeqVoltages: Olevariant READONLY DISPID 208;
        property CplxSeqCurrents: Olevariant READONLY DISPID 209;
        property AllVariableNames: Olevariant READONLY DISPID 210;
        property AllVariableValues: Olevariant READONLY DISPID 211;
        property Variable[const MyVarName: Widestring; out Code: Integer]: Double READONLY DISPID 212;
        property Variablei[Idx: Integer; out Code: Integer]: Double READONLY DISPID 213;
        property NodeOrder: Olevariant READONLY DISPID 214;
        property HasOCPDevice: Wordbool READONLY DISPID 215;
        property NumControls: Integer READONLY DISPID 216;
        property OCPDevIndex: Integer READONLY DISPID 217;
        property OCPDevType: Integer READONLY DISPID 218;
        property CurrentsMagAng: Olevariant READONLY DISPID 219;
        property VoltagesMagAng: Olevariant READONLY DISPID 220;
    end;

// *********************************************************************//
// Interface: IError
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {B521E339-8ED2-4BD6-9AEB-FD349CA8D8E3}
// *********************************************************************//
    IError = interface(IDispatch)
        ['{B521E339-8ED2-4BD6-9AEB-FD349CA8D8E3}']
        function Get_Number: Integer; SAFECALL;
        function Get_Description: Widestring; SAFECALL;
        property Number: Integer READ Get_Number;
        property Description: Widestring READ Get_Description;
    end;

// *********************************************************************//
// DispIntf:  IErrorDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {B521E339-8ED2-4BD6-9AEB-FD349CA8D8E3}
// *********************************************************************//
    IErrorDisp = dispinterface
        ['{B521E339-8ED2-4BD6-9AEB-FD349CA8D8E3}']
        property Number: Integer READONLY DISPID 1;
        property Description: Widestring READONLY DISPID 2;
    end;

// *********************************************************************//
// Interface: ICircuit
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {32441C6D-7A27-4164-B5B0-FA054300C217}
// *********************************************************************//
    ICircuit = interface(IDispatch)
        ['{32441C6D-7A27-4164-B5B0-FA054300C217}']
        function Get_Name: Widestring; SAFECALL;
        function Get_NumCktElements: Integer; SAFECALL;
        function Get_NumBuses: Integer; SAFECALL;
        function Get_NumNodes: Integer; SAFECALL;
        function Get_Buses(Index: Olevariant): IBus; SAFECALL;
        function Get_CktElements(Idx: Olevariant): ICktElement; SAFECALL;
        function Get_Losses: Olevariant; SAFECALL;
        function Get_LineLosses: Olevariant; SAFECALL;
        function Get_SubstationLosses: Olevariant; SAFECALL;
        function Get_TotalPower: Olevariant; SAFECALL;
        function Get_AllBusVolts: Olevariant; SAFECALL;
        function Get_AllBusVmag: Olevariant; SAFECALL;
        function Get_AllElementNames: Olevariant; SAFECALL;
        function Get_ActiveElement: ICktElement; SAFECALL;
        procedure Disable(const Name: Widestring); SAFECALL;
        procedure Enable(const Name: Widestring); SAFECALL;
        function Get_Solution: ISolution; SAFECALL;
        function Get_ActiveBus: IBus; SAFECALL;
        function FirstPCElement: Integer; SAFECALL;
        function NextPCElement: Integer; SAFECALL;
        function FirstPDElement: Integer; SAFECALL;
        function NextPDElement: Integer; SAFECALL;
        function Get_AllBusNames: Olevariant; SAFECALL;
        function Get_AllElementLosses: Olevariant; SAFECALL;
        procedure Sample; SAFECALL;
        procedure SaveSample; SAFECALL;
        function Get_Monitors: IMonitors; SAFECALL;
        function Get_Meters: IMeters; SAFECALL;
        function Get_Generators: IGenerators; SAFECALL;
        function Get_Settings: ISettings; SAFECALL;
        function Get_Lines: ILines; SAFECALL;
        function SetActiveElement(const FullName: Widestring): Integer; SAFECALL;
        function Capacity(Start: Double; Increment: Double): Double; SAFECALL;
        function SetActiveBus(const BusName: Widestring): Integer; SAFECALL;
        function SetActiveBusi(BusIndex: Integer): Integer; SAFECALL;
        function Get_AllBusVmagPu: Olevariant; SAFECALL;
        function Get_AllNodeNames: Olevariant; SAFECALL;
        function Get_SystemY: Olevariant; SAFECALL;
        function Get_CtrlQueue: ICtrlQueue; SAFECALL;
        function Get_AllBusDistances: Olevariant; SAFECALL;
        function Get_AllNodeDistances: Olevariant; SAFECALL;
        function Get_AllNodeVmagByPhase(Phase: Integer): Olevariant; SAFECALL;
        function Get_AllNodeVmagPUByPhase(Phase: Integer): Olevariant; SAFECALL;
        function Get_AllNodeDistancesByPhase(Phase: Integer): Olevariant; SAFECALL;
        function Get_AllNodeNamesByPhase(Phase: Integer): Olevariant; SAFECALL;
        function Get_Loads: ILoads; SAFECALL;
        function FirstElement: Integer; SAFECALL;
        function NextElement: Integer; SAFECALL;
        function SetActiveClass(const ClassName: Widestring): Integer; SAFECALL;
        function Get_ActiveDSSElement: IDSSElement; SAFECALL;
        function Get_ActiveCktElement: ICktElement; SAFECALL;
        function Get_ActiveClass: IActiveClass; SAFECALL;
        function Get_Transformers: ITransformers; SAFECALL;
        function Get_SwtControls: ISwtControls; SAFECALL;
        function Get_CapControls: ICapControls; SAFECALL;
        function Get_RegControls: IRegControls; SAFECALL;
        function Get_Capacitors: ICapacitors; SAFECALL;
        function Get_Topology: ITopology; SAFECALL;
        function Get_Sensors: ISensors; SAFECALL;
        procedure UpdateStorage; SAFECALL;
        function Get_ParentPDElement: Integer; SAFECALL;
        function Get_XYCurves: IXYCurves; SAFECALL;
        function Get_PDElements: IPDElements; SAFECALL;
        function Get_Reclosers: IReclosers; SAFECALL;
        function Get_Relays: IRelays; SAFECALL;
        function Get_LoadShapes: ILoadShapes; SAFECALL;
        function Get_Fuses: Fuses; SAFECALL;
        function Get_Isources: IISources; SAFECALL;
        function Get_YNodeVarray: Olevariant; SAFECALL;
        procedure EndOfTimeStepUpdate; SAFECALL;
        function Get_DSSim_Coms: IDSSimComs; SAFECALL;
        function Get_YNodeOrder: Olevariant; SAFECALL;
        function Get_YCurrents: Olevariant; SAFECALL;
        function Get_PVSystems: IPVSystems; SAFECALL;
        function Get_Vsources: IVsources; SAFECALL;
        function Get_Parallel: IParallel; SAFECALL;
        function Get_LineCodes: ILineCodes; SAFECALL;
        property Name: Widestring READ Get_Name;
        property NumCktElements: Integer READ Get_NumCktElements;
        property NumBuses: Integer READ Get_NumBuses;
        property NumNodes: Integer READ Get_NumNodes;
        property Buses[Index: Olevariant]: IBus READ Get_Buses;
        property CktElements[Idx: Olevariant]: ICktElement READ Get_CktElements;
        property Losses: Olevariant READ Get_Losses;
        property LineLosses: Olevariant READ Get_LineLosses;
        property SubstationLosses: Olevariant READ Get_SubstationLosses;
        property TotalPower: Olevariant READ Get_TotalPower;
        property AllBusVolts: Olevariant READ Get_AllBusVolts;
        property AllBusVmag: Olevariant READ Get_AllBusVmag;
        property AllElementNames: Olevariant READ Get_AllElementNames;
        property ActiveElement: ICktElement READ Get_ActiveElement;
        property Solution: ISolution READ Get_Solution;
        property ActiveBus: IBus READ Get_ActiveBus;
        property AllBusNames: Olevariant READ Get_AllBusNames;
        property AllElementLosses: Olevariant READ Get_AllElementLosses;
        property Monitors: IMonitors READ Get_Monitors;
        property Meters: IMeters READ Get_Meters;
        property Generators: IGenerators READ Get_Generators;
        property Settings: ISettings READ Get_Settings;
        property Lines: ILines READ Get_Lines;
        property AllBusVmagPu: Olevariant READ Get_AllBusVmagPu;
        property AllNodeNames: Olevariant READ Get_AllNodeNames;
        property SystemY: Olevariant READ Get_SystemY;
        property CtrlQueue: ICtrlQueue READ Get_CtrlQueue;
        property AllBusDistances: Olevariant READ Get_AllBusDistances;
        property AllNodeDistances: Olevariant READ Get_AllNodeDistances;
        property AllNodeVmagByPhase[Phase: Integer]: Olevariant READ Get_AllNodeVmagByPhase;
        property AllNodeVmagPUByPhase[Phase: Integer]: Olevariant READ Get_AllNodeVmagPUByPhase;
        property AllNodeDistancesByPhase[Phase: Integer]: Olevariant READ Get_AllNodeDistancesByPhase;
        property AllNodeNamesByPhase[Phase: Integer]: Olevariant READ Get_AllNodeNamesByPhase;
        property Loads: ILoads READ Get_Loads;
        property ActiveDSSElement: IDSSElement READ Get_ActiveDSSElement;
        property ActiveCktElement: ICktElement READ Get_ActiveCktElement;
        property ActiveClass: IActiveClass READ Get_ActiveClass;
        property Transformers: ITransformers READ Get_Transformers;
        property SwtControls: ISwtControls READ Get_SwtControls;
        property CapControls: ICapControls READ Get_CapControls;
        property RegControls: IRegControls READ Get_RegControls;
        property Capacitors: ICapacitors READ Get_Capacitors;
        property Topology: ITopology READ Get_Topology;
        property Sensors: ISensors READ Get_Sensors;
        property ParentPDElement: Integer READ Get_ParentPDElement;
        property XYCurves: IXYCurves READ Get_XYCurves;
        property PDElements: IPDElements READ Get_PDElements;
        property Reclosers: IReclosers READ Get_Reclosers;
        property Relays: IRelays READ Get_Relays;
        property LoadShapes: ILoadShapes READ Get_LoadShapes;
        property Fuses: Fuses READ Get_Fuses;
        property Isources: IISources READ Get_Isources;
        property YNodeVarray: Olevariant READ Get_YNodeVarray;
        property DSSim_Coms: IDSSimComs READ Get_DSSim_Coms;
        property YNodeOrder: Olevariant READ Get_YNodeOrder;
        property YCurrents: Olevariant READ Get_YCurrents;
        property PVSystems: IPVSystems READ Get_PVSystems;
        property Vsources: IVsources READ Get_Vsources;
        property Parallel: IParallel READ Get_Parallel;
        property LineCodes: ILineCodes READ Get_LineCodes;
    end;

// *********************************************************************//
// DispIntf:  ICircuitDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {32441C6D-7A27-4164-B5B0-FA054300C217}
// *********************************************************************//
    ICircuitDisp = dispinterface
        ['{32441C6D-7A27-4164-B5B0-FA054300C217}']
        property Name: Widestring READONLY DISPID 1;
        property NumCktElements: Integer READONLY DISPID 2;
        property NumBuses: Integer READONLY DISPID 3;
        property NumNodes: Integer READONLY DISPID 4;
        property Buses[Index: Olevariant]: IBus READONLY DISPID 5;
        property CktElements[Idx: Olevariant]: ICktElement READONLY DISPID 6;
        property Losses: Olevariant READONLY DISPID 7;
        property LineLosses: Olevariant READONLY DISPID 8;
        property SubstationLosses: Olevariant READONLY DISPID 9;
        property TotalPower: Olevariant READONLY DISPID 10;
        property AllBusVolts: Olevariant READONLY DISPID 11;
        property AllBusVmag: Olevariant READONLY DISPID 12;
        property AllElementNames: Olevariant READONLY DISPID 13;
        property ActiveElement: ICktElement READONLY DISPID 14;
        procedure Disable(const Name: Widestring); DISPID 15;
        procedure Enable(const Name: Widestring); DISPID 16;
        property Solution: ISolution READONLY DISPID 17;
        property ActiveBus: IBus READONLY DISPID 18;
        function FirstPCElement: Integer; DISPID 19;
        function NextPCElement: Integer; DISPID 20;
        function FirstPDElement: Integer; DISPID 21;
        function NextPDElement: Integer; DISPID 22;
        property AllBusNames: Olevariant READONLY DISPID 23;
        property AllElementLosses: Olevariant READONLY DISPID 24;
        procedure Sample; DISPID 25;
        procedure SaveSample; DISPID 26;
        property Monitors: IMonitors READONLY DISPID 27;
        property Meters: IMeters READONLY DISPID 28;
        property Generators: IGenerators READONLY DISPID 29;
        property Settings: ISettings READONLY DISPID 30;
        property Lines: ILines READONLY DISPID 31;
        function SetActiveElement(const FullName: Widestring): Integer; DISPID 32;
        function Capacity(Start: Double; Increment: Double): Double; DISPID 33;
        function SetActiveBus(const BusName: Widestring): Integer; DISPID 34;
        function SetActiveBusi(BusIndex: Integer): Integer; DISPID 36;
        property AllBusVmagPu: Olevariant READONLY DISPID 35;
        property AllNodeNames: Olevariant READONLY DISPID 37;
        property SystemY: Olevariant READONLY DISPID 38;
        property CtrlQueue: ICtrlQueue READONLY DISPID 201;
        property AllBusDistances: Olevariant READONLY DISPID 202;
        property AllNodeDistances: Olevariant READONLY DISPID 203;
        property AllNodeVmagByPhase[Phase: Integer]: Olevariant READONLY DISPID 204;
        property AllNodeVmagPUByPhase[Phase: Integer]: Olevariant READONLY DISPID 205;
        property AllNodeDistancesByPhase[Phase: Integer]: Olevariant READONLY DISPID 206;
        property AllNodeNamesByPhase[Phase: Integer]: Olevariant READONLY DISPID 207;
        property Loads: ILoads READONLY DISPID 208;
        function FirstElement: Integer; DISPID 209;
        function NextElement: Integer; DISPID 210;
        function SetActiveClass(const ClassName: Widestring): Integer; DISPID 211;
        property ActiveDSSElement: IDSSElement READONLY DISPID 212;
        property ActiveCktElement: ICktElement READONLY DISPID 213;
        property ActiveClass: IActiveClass READONLY DISPID 214;
        property Transformers: ITransformers READONLY DISPID 215;
        property SwtControls: ISwtControls READONLY DISPID 216;
        property CapControls: ICapControls READONLY DISPID 217;
        property RegControls: IRegControls READONLY DISPID 218;
        property Capacitors: ICapacitors READONLY DISPID 219;
        property Topology: ITopology READONLY DISPID 220;
        property Sensors: ISensors READONLY DISPID 221;
        procedure UpdateStorage; DISPID 222;
        property ParentPDElement: Integer READONLY DISPID 223;
        property XYCurves: IXYCurves READONLY DISPID 224;
        property PDElements: IPDElements READONLY DISPID 225;
        property Reclosers: IReclosers READONLY DISPID 226;
        property Relays: IRelays READONLY DISPID 227;
        property LoadShapes: ILoadShapes READONLY DISPID 228;
        property Fuses: Fuses READONLY DISPID 229;
        property Isources: IISources READONLY DISPID 230;
        property YNodeVarray: Olevariant READONLY DISPID 231;
        procedure EndOfTimeStepUpdate; DISPID 232;
        property DSSim_Coms: IDSSimComs READONLY DISPID 233;
        property YNodeOrder: Olevariant READONLY DISPID 234;
        property YCurrents: Olevariant READONLY DISPID 235;
        property PVSystems: IPVSystems READONLY DISPID 236;
        property Vsources: IVsources READONLY DISPID 237;
        property Parallel: IParallel READONLY DISPID 238;
        property LineCodes: ILineCodes READONLY DISPID 239;
    end;

// *********************************************************************//
// Interface: IBus
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {E5B78C35-88F8-495F-8CD1-EBB5D90ED228}
// *********************************************************************//
    IBus = interface(IDispatch)
        ['{E5B78C35-88F8-495F-8CD1-EBB5D90ED228}']
        function Get_Name: Widestring; SAFECALL;
        function Get_NumNodes: Integer; SAFECALL;
        function Get_Voltages: Olevariant; SAFECALL;
        function Get_SeqVoltages: Olevariant; SAFECALL;
        function Get_Nodes: Olevariant; SAFECALL;
        function Get_Voc: Olevariant; SAFECALL;
        function Get_Isc: Olevariant; SAFECALL;
        function Get_puVoltages: Olevariant; SAFECALL;
        function Get_kVBase: Double; SAFECALL;
        function Get_ZscMatrix: Olevariant; SAFECALL;
        function Get_Zsc1: Olevariant; SAFECALL;
        function Get_Zsc0: Olevariant; SAFECALL;
        function ZscRefresh: Wordbool; SAFECALL;
        function Get_YscMatrix: Olevariant; SAFECALL;
        function Get_Coorddefined: Wordbool; SAFECALL;
        function Get_x: Double; SAFECALL;
        procedure Set_x(Value: Double); SAFECALL;
        function Get_y: Double; SAFECALL;
        procedure Set_y(Value: Double); SAFECALL;
        function Get_Distance: Double; SAFECALL;
        function GetUniqueNodeNumber(StartNumber: Integer): Integer; SAFECALL;
        function Get_CplxSeqVoltages: Olevariant; SAFECALL;
        function Get_Lambda: Double; SAFECALL;
        function Get_N_interrupts: Double; SAFECALL;
        function Get_Int_Duration: Double; SAFECALL;
        function Get_Cust_Interrupts: Double; SAFECALL;
        function Get_Cust_Duration: Double; SAFECALL;
        function Get_N_Customers: Integer; SAFECALL;
        function Get_VLL: Olevariant; SAFECALL;
        function Get_puVLL: Olevariant; SAFECALL;
        function Get_VMagAngle: Olevariant; SAFECALL;
        function Get_puVmagAngle: Olevariant; SAFECALL;
        function Get_TotalMiles: Double; SAFECALL;
        function Get_SectionID: Integer; SAFECALL;
        property Name: Widestring READ Get_Name;
        property NumNodes: Integer READ Get_NumNodes;
        property Voltages: Olevariant READ Get_Voltages;
        property SeqVoltages: Olevariant READ Get_SeqVoltages;
        property Nodes: Olevariant READ Get_Nodes;
        property Voc: Olevariant READ Get_Voc;
        property Isc: Olevariant READ Get_Isc;
        property puVoltages: Olevariant READ Get_puVoltages;
        property kVBase: Double READ Get_kVBase;
        property ZscMatrix: Olevariant READ Get_ZscMatrix;
        property Zsc1: Olevariant READ Get_Zsc1;
        property Zsc0: Olevariant READ Get_Zsc0;
        property YscMatrix: Olevariant READ Get_YscMatrix;
        property Coorddefined: Wordbool READ Get_Coorddefined;
        property x: Double READ Get_x WRITE Set_x;
        property y: Double READ Get_y WRITE Set_y;
        property Distance: Double READ Get_Distance;
        property CplxSeqVoltages: Olevariant READ Get_CplxSeqVoltages;
        property Lambda: Double READ Get_Lambda;
        property N_interrupts: Double READ Get_N_interrupts;
        property Int_Duration: Double READ Get_Int_Duration;
        property Cust_Interrupts: Double READ Get_Cust_Interrupts;
        property Cust_Duration: Double READ Get_Cust_Duration;
        property N_Customers: Integer READ Get_N_Customers;
        property VLL: Olevariant READ Get_VLL;
        property puVLL: Olevariant READ Get_puVLL;
        property VMagAngle: Olevariant READ Get_VMagAngle;
        property puVmagAngle: Olevariant READ Get_puVmagAngle;
        property TotalMiles: Double READ Get_TotalMiles;
        property SectionID: Integer READ Get_SectionID;
    end;

// *********************************************************************//
// DispIntf:  IBusDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {E5B78C35-88F8-495F-8CD1-EBB5D90ED228}
// *********************************************************************//
    IBusDisp = dispinterface
        ['{E5B78C35-88F8-495F-8CD1-EBB5D90ED228}']
        property Name: Widestring READONLY DISPID 1;
        property NumNodes: Integer READONLY DISPID 2;
        property Voltages: Olevariant READONLY DISPID 3;
        property SeqVoltages: Olevariant READONLY DISPID 4;
        property Nodes: Olevariant READONLY DISPID 5;
        property Voc: Olevariant READONLY DISPID 6;
        property Isc: Olevariant READONLY DISPID 7;
        property puVoltages: Olevariant READONLY DISPID 8;
        property kVBase: Double READONLY DISPID 9;
        property ZscMatrix: Olevariant READONLY DISPID 10;
        property Zsc1: Olevariant READONLY DISPID 11;
        property Zsc0: Olevariant READONLY DISPID 12;
        function ZscRefresh: Wordbool; DISPID 13;
        property YscMatrix: Olevariant READONLY DISPID 14;
        property Coorddefined: Wordbool READONLY DISPID 201;
        property x: Double DISPID 202;
        property y: Double DISPID 203;
        property Distance: Double READONLY DISPID 204;
        function GetUniqueNodeNumber(StartNumber: Integer): Integer; DISPID 205;
        property CplxSeqVoltages: Olevariant READONLY DISPID 206;
        property Lambda: Double READONLY DISPID 207;
        property N_interrupts: Double READONLY DISPID 208;
        property Int_Duration: Double READONLY DISPID 209;
        property Cust_Interrupts: Double READONLY DISPID 210;
        property Cust_Duration: Double READONLY DISPID 211;
        property N_Customers: Integer READONLY DISPID 212;
        property VLL: Olevariant READONLY DISPID 213;
        property puVLL: Olevariant READONLY DISPID 214;
        property VMagAngle: Olevariant READONLY DISPID 215;
        property puVmagAngle: Olevariant READONLY DISPID 216;
        property TotalMiles: Double READONLY DISPID 217;
        property SectionID: Integer READONLY DISPID 218;
    end;

// *********************************************************************//
// Interface: IDSS
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {14644AD0-4909-48FF-B624-24E8C38D1AED}
// *********************************************************************//
    IDSS = interface(IDispatch)
        ['{14644AD0-4909-48FF-B624-24E8C38D1AED}']
        function Get_NumCircuits: Integer; SAFECALL;
        function Get_Circuits(Idx: Olevariant): ICircuit; SAFECALL;
        function Get_ActiveCircuit: ICircuit; SAFECALL;
        function Get_Text: IText; SAFECALL;
        function Get_Error: IError; SAFECALL;
        function NewCircuit(const Name: Widestring): ICircuit; SAFECALL;
        procedure ClearAll; SAFECALL;
        procedure ShowPanel; SAFECALL;
        function Start(code: Integer): Wordbool; SAFECALL;
        function Get_Version: Widestring; SAFECALL;
        function Get_DSSProgress: IDSSProgress; SAFECALL;
        function Get_Classes: Olevariant; SAFECALL;
        function Get_UserClasses: Olevariant; SAFECALL;
        function Get_NumClasses: Integer; SAFECALL;
        function Get_NumUserClasses: Integer; SAFECALL;
        function Get_DataPath: Widestring; SAFECALL;
        procedure Set_DataPath(const Value: Widestring); SAFECALL;
        procedure Reset; SAFECALL;
        function Get_AllowForms: Wordbool; SAFECALL;
        procedure Set_AllowForms(Value: Wordbool); SAFECALL;
        function Get_DefaultEditor: Widestring; SAFECALL;
        function Get_ActiveClass: IActiveClass; SAFECALL;
        function SetActiveClass(const ClassName: Widestring): Integer; SAFECALL;
        function Get_Executive: IDSS_Executive; SAFECALL;
        function Get_Events: IDSSEvents; SAFECALL;
        function Get_CmathLib: ICmathLib; SAFECALL;
        function Get_Parser: IParser; SAFECALL;
        function Get_DSSim_Coms: IDSSimComs; SAFECALL;
        property NumCircuits: Integer READ Get_NumCircuits;
        property Circuits[Idx: Olevariant]: ICircuit READ Get_Circuits;
        property ActiveCircuit: ICircuit READ Get_ActiveCircuit;
        property Text: IText READ Get_Text;
        property Error: IError READ Get_Error;
        property Version: Widestring READ Get_Version;
        property DSSProgress: IDSSProgress READ Get_DSSProgress;
        property Classes: Olevariant READ Get_Classes;
        property UserClasses: Olevariant READ Get_UserClasses;
        property NumClasses: Integer READ Get_NumClasses;
        property NumUserClasses: Integer READ Get_NumUserClasses;
        property DataPath: Widestring READ Get_DataPath WRITE Set_DataPath;
        property AllowForms: Wordbool READ Get_AllowForms WRITE Set_AllowForms;
        property DefaultEditor: Widestring READ Get_DefaultEditor;
        property ActiveClass: IActiveClass READ Get_ActiveClass;
        property Executive: IDSS_Executive READ Get_Executive;
        property Events: IDSSEvents READ Get_Events;
        property CmathLib: ICmathLib READ Get_CmathLib;
        property Parser: IParser READ Get_Parser;
        property DSSim_Coms: IDSSimComs READ Get_DSSim_Coms;
    end;

// *********************************************************************//
// DispIntf:  IDSSDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {14644AD0-4909-48FF-B624-24E8C38D1AED}
// *********************************************************************//
    IDSSDisp = dispinterface
        ['{14644AD0-4909-48FF-B624-24E8C38D1AED}']
        property NumCircuits: Integer READONLY DISPID 1;
        property Circuits[Idx: Olevariant]: ICircuit READONLY DISPID 2;
        property ActiveCircuit: ICircuit READONLY DISPID 3;
        property Text: IText READONLY DISPID 4;
        property Error: IError READONLY DISPID 5;
        function NewCircuit(const Name: Widestring): ICircuit; DISPID 6;
        procedure ClearAll; DISPID 7;
        procedure ShowPanel; DISPID 8;
        function Start(code: Integer): Wordbool; DISPID 9;
        property Version: Widestring READONLY DISPID 10;
        property DSSProgress: IDSSProgress READONLY DISPID 11;
        property Classes: Olevariant READONLY DISPID 12;
        property UserClasses: Olevariant READONLY DISPID 13;
        property NumClasses: Integer READONLY DISPID 14;
        property NumUserClasses: Integer READONLY DISPID 15;
        property DataPath: Widestring DISPID 17;
        procedure Reset; DISPID 18;
        property AllowForms: Wordbool DISPID 20;
        property DefaultEditor: Widestring READONLY DISPID 201;
        property ActiveClass: IActiveClass READONLY DISPID 202;
        function SetActiveClass(const ClassName: Widestring): Integer; DISPID 203;
        property Executive: IDSS_Executive READONLY DISPID 205;
        property Events: IDSSEvents READONLY DISPID 206;
        property CmathLib: ICmathLib READONLY DISPID 204;
        property Parser: IParser READONLY DISPID 207;
        property DSSim_Coms: IDSSimComs READONLY DISPID 208;
    end;

// *********************************************************************//
// Interface: ISolution
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F2332365-962A-4DF4-9D1E-218E0B0F2CEF}
// *********************************************************************//
    ISolution = interface(IDispatch)
        ['{F2332365-962A-4DF4-9D1E-218E0B0F2CEF}']
        procedure Solve; SAFECALL;
        function Get_Mode: Integer; SAFECALL;
        procedure Set_Mode(Mode: Integer); SAFECALL;
        function Get_Frequency: Double; SAFECALL;
        procedure Set_Frequency(Frequency: Double); SAFECALL;
        function Get_Hour: Integer; SAFECALL;
        procedure Set_Hour(Hour: Integer); SAFECALL;
        function Get_Seconds: Double; SAFECALL;
        procedure Set_Seconds(Seconds: Double); SAFECALL;
        function Get_StepSize: Double; SAFECALL;
        procedure Set_StepSize(StepSize: Double); SAFECALL;
        function Get_Year: Integer; SAFECALL;
        procedure Set_Year(Year: Integer); SAFECALL;
        function Get_LoadMult: Double; SAFECALL;
        procedure Set_LoadMult(LoadMult: Double); SAFECALL;
        function Get_Iterations: Integer; SAFECALL;
        function Get_MaxIterations: Integer; SAFECALL;
        procedure Set_MaxIterations(MaxIterations: Integer); SAFECALL;
        function Get_Tolerance: Double; SAFECALL;
        procedure Set_Tolerance(Tolerance: Double); SAFECALL;
        function Get_Number: Integer; SAFECALL;
        procedure Set_Number(Number: Integer); SAFECALL;
        function Get_Random: Integer; SAFECALL;
        procedure Set_Random(Random: Integer); SAFECALL;
        function Get_ModeID: Widestring; SAFECALL;
        function Get_LoadModel: Integer; SAFECALL;
        procedure Set_LoadModel(Value: Integer); SAFECALL;
        function Get_LDCurve: Widestring; SAFECALL;
        procedure Set_LDCurve(const Value: Widestring); SAFECALL;
        function Get_pctGrowth: Double; SAFECALL;
        procedure Set_pctGrowth(Value: Double); SAFECALL;
        function Get_AddType: Integer; SAFECALL;
        procedure Set_AddType(Value: Integer); SAFECALL;
        function Get_GenkW: Double; SAFECALL;
        procedure Set_GenkW(Value: Double); SAFECALL;
        function Get_GenPF: Double; SAFECALL;
        procedure Set_GenPF(Value: Double); SAFECALL;
        function Get_Capkvar: Double; SAFECALL;
        procedure Set_Capkvar(Value: Double); SAFECALL;
        function Get_Algorithm: Integer; SAFECALL;
        procedure Set_Algorithm(Value: Integer); SAFECALL;
        function Get_ControlMode: Integer; SAFECALL;
        procedure Set_ControlMode(Value: Integer); SAFECALL;
        function Get_GenMult: Double; SAFECALL;
        procedure Set_GenMult(Value: Double); SAFECALL;
        function Get_DefaultDaily: Widestring; SAFECALL;
        procedure Set_DefaultDaily(const Value: Widestring); SAFECALL;
        function Get_DefaultYearly: Widestring; SAFECALL;
        procedure Set_DefaultYearly(const Value: Widestring); SAFECALL;
        function Get_EventLog: Olevariant; SAFECALL;
        function Get_dblHour: Double; SAFECALL;
        procedure Set_dblHour(Value: Double); SAFECALL;
        procedure Set_StepsizeMin(Param1: Double); SAFECALL;
        procedure Set_StepsizeHr(Param1: Double); SAFECALL;
        function Get_ControlIterations: Integer; SAFECALL;
        procedure Set_ControlIterations(Value: Integer); SAFECALL;
        function Get_MaxControlIterations: Integer; SAFECALL;
        procedure Set_MaxControlIterations(Value: Integer); SAFECALL;
        procedure Sample_DoControlActions; SAFECALL;
        procedure CheckFaultStatus; SAFECALL;
        procedure SolveSnap; SAFECALL;
        procedure SolveDirect; SAFECALL;
        procedure SolvePflow; SAFECALL;
        procedure SolveNoControl; SAFECALL;
        procedure SolvePlusControl; SAFECALL;
        procedure InitSnap; SAFECALL;
        procedure CheckControls; SAFECALL;
        procedure SampleControlDevices; SAFECALL;
        procedure DoControlActions; SAFECALL;
        procedure BuildYMatrix(BuildOption: Integer; AllocateVI: Integer); SAFECALL;
        function Get_SystemYChanged: Wordbool; SAFECALL;
        function Get_Converged: Wordbool; SAFECALL;
        procedure Set_Converged(Value: Wordbool); SAFECALL;
        function Get_Totaliterations: Integer; SAFECALL;
        function Get_MostIterationsDone: Integer; SAFECALL;
        function Get_ControlActionsDone: Wordbool; SAFECALL;
        procedure Set_ControlActionsDone(Value: Wordbool); SAFECALL;
        procedure FinishTimeStep; SAFECALL;
        procedure Cleanup; SAFECALL;
        function Get_Total_Time: Double; SAFECALL;
        procedure Set_Total_Time(Value: Double); SAFECALL;
        function Get_Process_Time: Double; SAFECALL;
        function Get_Time_of_Step: Double; SAFECALL;
        procedure SolveAll; SAFECALL;
        function Get_IncMatrix: Olevariant; SAFECALL;
        function Get_IncMatrixRows: Olevariant; SAFECALL;
        function Get_IncMatrixCols: Olevariant; SAFECALL;
        function Get_BusLevels: Olevariant; SAFECALL;
        function Get_Laplacian: Olevariant; SAFECALL;
        property Mode: Integer READ Get_Mode WRITE Set_Mode;
        property Frequency: Double READ Get_Frequency WRITE Set_Frequency;
        property Hour: Integer READ Get_Hour WRITE Set_Hour;
        property Seconds: Double READ Get_Seconds WRITE Set_Seconds;
        property StepSize: Double READ Get_StepSize WRITE Set_StepSize;
        property Year: Integer READ Get_Year WRITE Set_Year;
        property LoadMult: Double READ Get_LoadMult WRITE Set_LoadMult;
        property Iterations: Integer READ Get_Iterations;
        property MaxIterations: Integer READ Get_MaxIterations WRITE Set_MaxIterations;
        property Tolerance: Double READ Get_Tolerance WRITE Set_Tolerance;
        property Number: Integer READ Get_Number WRITE Set_Number;
        property Random: Integer READ Get_Random WRITE Set_Random;
        property ModeID: Widestring READ Get_ModeID;
        property LoadModel: Integer READ Get_LoadModel WRITE Set_LoadModel;
        property LDCurve: Widestring READ Get_LDCurve WRITE Set_LDCurve;
        property pctGrowth: Double READ Get_pctGrowth WRITE Set_pctGrowth;
        property AddType: Integer READ Get_AddType WRITE Set_AddType;
        property GenkW: Double READ Get_GenkW WRITE Set_GenkW;
        property GenPF: Double READ Get_GenPF WRITE Set_GenPF;
        property Capkvar: Double READ Get_Capkvar WRITE Set_Capkvar;
        property Algorithm: Integer READ Get_Algorithm WRITE Set_Algorithm;
        property ControlMode: Integer READ Get_ControlMode WRITE Set_ControlMode;
        property GenMult: Double READ Get_GenMult WRITE Set_GenMult;
        property DefaultDaily: Widestring READ Get_DefaultDaily WRITE Set_DefaultDaily;
        property DefaultYearly: Widestring READ Get_DefaultYearly WRITE Set_DefaultYearly;
        property EventLog: Olevariant READ Get_EventLog;
        property dblHour: Double READ Get_dblHour WRITE Set_dblHour;
        property StepsizeMin: Double WRITE Set_StepsizeMin;
        property StepsizeHr: Double WRITE Set_StepsizeHr;
        property ControlIterations: Integer READ Get_ControlIterations WRITE Set_ControlIterations;
        property MaxControlIterations: Integer READ Get_MaxControlIterations WRITE Set_MaxControlIterations;
        property SystemYChanged: Wordbool READ Get_SystemYChanged;
        property Converged: Wordbool READ Get_Converged WRITE Set_Converged;
        property Totaliterations: Integer READ Get_Totaliterations;
        property MostIterationsDone: Integer READ Get_MostIterationsDone;
        property ControlActionsDone: Wordbool READ Get_ControlActionsDone WRITE Set_ControlActionsDone;
        property Total_Time: Double READ Get_Total_Time WRITE Set_Total_Time;
        property Process_Time: Double READ Get_Process_Time;
        property Time_of_Step: Double READ Get_Time_of_Step;
        property IncMatrix: Olevariant READ Get_IncMatrix;
        property IncMatrixRows: Olevariant READ Get_IncMatrixRows;
        property IncMatrixCols: Olevariant READ Get_IncMatrixCols;
        property BusLevels: Olevariant READ Get_BusLevels;
        property Laplacian: Olevariant READ Get_Laplacian;
    end;

// *********************************************************************//
// DispIntf:  ISolutionDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F2332365-962A-4DF4-9D1E-218E0B0F2CEF}
// *********************************************************************//
    ISolutionDisp = dispinterface
        ['{F2332365-962A-4DF4-9D1E-218E0B0F2CEF}']
        procedure Solve; DISPID 1;
        property Mode: Integer DISPID 2;
        property Frequency: Double DISPID 3;
        property Hour: Integer DISPID 4;
        property Seconds: Double DISPID 5;
        property StepSize: Double DISPID 6;
        property Year: Integer DISPID 7;
        property LoadMult: Double DISPID 8;
        property Iterations: Integer READONLY DISPID 9;
        property MaxIterations: Integer DISPID 10;
        property Tolerance: Double DISPID 11;
        property Number: Integer DISPID 12;
        property Random: Integer DISPID 13;
        property ModeID: Widestring READONLY DISPID 14;
        property LoadModel: Integer DISPID 15;
        property LDCurve: Widestring DISPID 16;
        property pctGrowth: Double DISPID 17;
        property AddType: Integer DISPID 18;
        property GenkW: Double DISPID 19;
        property GenPF: Double DISPID 20;
        property Capkvar: Double DISPID 21;
        property Algorithm: Integer DISPID 22;
        property ControlMode: Integer DISPID 23;
        property GenMult: Double DISPID 24;
        property DefaultDaily: Widestring DISPID 25;
        property DefaultYearly: Widestring DISPID 26;
        property EventLog: Olevariant READONLY DISPID 27;
        property dblHour: Double DISPID 201;
        property StepsizeMin: Double WRITEONLY DISPID 202;
        property StepsizeHr: Double WRITEONLY DISPID 203;
        property ControlIterations: Integer DISPID 204;
        property MaxControlIterations: Integer DISPID 205;
        procedure Sample_DoControlActions; DISPID 206;
        procedure CheckFaultStatus; DISPID 207;
        procedure SolveSnap; DISPID 208;
        procedure SolveDirect; DISPID 209;
        procedure SolvePflow; DISPID 210;
        procedure SolveNoControl; DISPID 211;
        procedure SolvePlusControl; DISPID 212;
        procedure InitSnap; DISPID 213;
        procedure CheckControls; DISPID 214;
        procedure SampleControlDevices; DISPID 215;
        procedure DoControlActions; DISPID 216;
        procedure BuildYMatrix(BuildOption: Integer; AllocateVI: Integer); DISPID 217;
        property SystemYChanged: Wordbool READONLY DISPID 218;
        property Converged: Wordbool DISPID 219;
        property Totaliterations: Integer READONLY DISPID 220;
        property MostIterationsDone: Integer READONLY DISPID 221;
        property ControlActionsDone: Wordbool DISPID 222;
        procedure FinishTimeStep; DISPID 223;
        procedure Cleanup; DISPID 224;
        property Total_Time: Double DISPID 225;
        property Process_Time: Double READONLY DISPID 226;
        property Time_of_Step: Double READONLY DISPID 227;
        procedure SolveAll; DISPID 228;
        property IncMatrix: Olevariant READONLY DISPID 229;
        property IncMatrixRows: Olevariant READONLY DISPID 230;
        property IncMatrixCols: Olevariant READONLY DISPID 231;
        property BusLevels: Olevariant READONLY DISPID 232;
        property Laplacian: Olevariant READONLY DISPID 233;
    end;

// *********************************************************************//
// Interface: IMonitors
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5C339E44-C583-445C-91D1-3B1E49CAD6B0}
// *********************************************************************//
    IMonitors = interface(IDispatch)
        ['{5C339E44-C583-445C-91D1-3B1E49CAD6B0}']
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        procedure Reset; SAFECALL;
        procedure ResetAll; SAFECALL;
        procedure Sample; SAFECALL;
        procedure Save; SAFECALL;
        procedure Show; SAFECALL;
        function Get_FileName: Widestring; SAFECALL;
        function Get_Mode: Integer; SAFECALL;
        procedure Set_Mode(Value: Integer); SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_ByteStream: Olevariant; SAFECALL;
        function Get_SampleCount: Integer; SAFECALL;
        procedure SampleAll; SAFECALL;
        procedure SaveAll; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        procedure Process; SAFECALL;
        procedure ProcessAll; SAFECALL;
        function Get_FileVersion: Integer; SAFECALL;
        function Get_RecordSize: Integer; SAFECALL;
        function Get_Header: Olevariant; SAFECALL;
        function Get_dblHour: Olevariant; SAFECALL;
        function Get_dblFreq: Olevariant; SAFECALL;
        function Get_Channel(Index: Integer): Olevariant; SAFECALL;
        function Get_NumChannels: Integer; SAFECALL;
        function Get_Element: Widestring; SAFECALL;
        procedure Set_Element(const Value: Widestring); SAFECALL;
        function Get_Terminal: Integer; SAFECALL;
        procedure Set_Terminal(Value: Integer); SAFECALL;
        property AllNames: Olevariant READ Get_AllNames;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property FileName: Widestring READ Get_FileName;
        property Mode: Integer READ Get_Mode WRITE Set_Mode;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property ByteStream: Olevariant READ Get_ByteStream;
        property SampleCount: Integer READ Get_SampleCount;
        property Count: Integer READ Get_Count;
        property FileVersion: Integer READ Get_FileVersion;
        property RecordSize: Integer READ Get_RecordSize;
        property Header: Olevariant READ Get_Header;
        property dblHour: Olevariant READ Get_dblHour;
        property dblFreq: Olevariant READ Get_dblFreq;
        property Channel[Index: Integer]: Olevariant READ Get_Channel;
        property NumChannels: Integer READ Get_NumChannels;
        property Element: Widestring READ Get_Element WRITE Set_Element;
        property Terminal: Integer READ Get_Terminal WRITE Set_Terminal;
    end;

// *********************************************************************//
// DispIntf:  IMonitorsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5C339E44-C583-445C-91D1-3B1E49CAD6B0}
// *********************************************************************//
    IMonitorsDisp = dispinterface
        ['{5C339E44-C583-445C-91D1-3B1E49CAD6B0}']
        property AllNames: Olevariant READONLY DISPID 2;
        property First: Integer READONLY DISPID 3;
        property Next: Integer READONLY DISPID 4;
        procedure Reset; DISPID 5;
        procedure ResetAll; DISPID 6;
        procedure Sample; DISPID 7;
        procedure Save; DISPID 8;
        procedure Show; DISPID 9;
        property FileName: Widestring READONLY DISPID 10;
        property Mode: Integer DISPID 11;
        property Name: Widestring DISPID 1;
        property ByteStream: Olevariant READONLY DISPID 12;
        property SampleCount: Integer READONLY DISPID 13;
        procedure SampleAll; DISPID 201;
        procedure SaveAll; DISPID 202;
        property Count: Integer READONLY DISPID 203;
        procedure Process; DISPID 204;
        procedure ProcessAll; DISPID 205;
        property FileVersion: Integer READONLY DISPID 206;
        property RecordSize: Integer READONLY DISPID 207;
        property Header: Olevariant READONLY DISPID 208;
        property dblHour: Olevariant READONLY DISPID 209;
        property dblFreq: Olevariant READONLY DISPID 210;
        property Channel[Index: Integer]: Olevariant READONLY DISPID 211;
        property NumChannels: Integer READONLY DISPID 212;
        property Element: Widestring DISPID 213;
        property Terminal: Integer DISPID 214;
    end;

// *********************************************************************//
// Interface: IMeters
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {86705B6C-352A-47F8-A24B-78B750EC3859}
// *********************************************************************//
    IMeters = interface(IDispatch)
        ['{86705B6C-352A-47F8-A24B-78B750EC3859}']
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_RegisterNames: Olevariant; SAFECALL;
        function Get_RegisterValues: Olevariant; SAFECALL;
        procedure Reset; SAFECALL;
        procedure ResetAll; SAFECALL;
        procedure Sample; SAFECALL;
        procedure Save; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Totals: Olevariant; SAFECALL;
        function Get_Peakcurrent: Olevariant; SAFECALL;
        procedure Set_Peakcurrent(Value: Olevariant); SAFECALL;
        function Get_CalcCurrent: Olevariant; SAFECALL;
        procedure Set_CalcCurrent(Value: Olevariant); SAFECALL;
        function Get_AllocFactors: Olevariant; SAFECALL;
        procedure Set_AllocFactors(Value: Olevariant); SAFECALL;
        function Get_MeteredElement: Widestring; SAFECALL;
        procedure Set_MeteredElement(const Value: Widestring); SAFECALL;
        function Get_MeteredTerminal: Integer; SAFECALL;
        procedure Set_MeteredTerminal(Value: Integer); SAFECALL;
        function Get_DIFilesAreOpen: Wordbool; SAFECALL;
        procedure SampleAll; SAFECALL;
        procedure SaveAll; SAFECALL;
        procedure OpenAllDIFiles; SAFECALL;
        procedure CloseAllDIFiles; SAFECALL;
        function Get_CountEndElements: Integer; SAFECALL;
        function Get_AllEndElements: Olevariant; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_AllBranchesInZone: Olevariant; SAFECALL;
        function Get_CountBranches: Integer; SAFECALL;
        function Get_SAIFI: Double; SAFECALL;
        function Get_SequenceIndex: Integer; SAFECALL;
        procedure Set_SequenceIndex(Value: Integer); SAFECALL;
        function Get_SAIFIKW: Double; SAFECALL;
        procedure DoReliabilityCalc(AssumeRestoration: Wordbool); SAFECALL;
        function Get_SeqListSize: Integer; SAFECALL;
        function Get_TotalCustomers: Integer; SAFECALL;
        function Get_SAIDI: Double; SAFECALL;
        function Get_CustInterrupts: Double; SAFECALL;
        function Get_NumSections: Integer; SAFECALL;
        procedure SetActiveSection(SectIdx: Integer); SAFECALL;
        function Get_OCPDeviceType: Integer; SAFECALL;
        function Get_NumSectionCustomers: Integer; SAFECALL;
        function Get_NumSectionBranches: Integer; SAFECALL;
        function Get_AvgRepairTime: Double; SAFECALL;
        function Get_FaultRateXRepairHrs: Double; SAFECALL;
        function Get_SumBranchFltRates: Double; SAFECALL;
        function Get_SectSeqIdx: Integer; SAFECALL;
        function Get_SectTotalCust: Integer; SAFECALL;
        property AllNames: Olevariant READ Get_AllNames;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property RegisterNames: Olevariant READ Get_RegisterNames;
        property RegisterValues: Olevariant READ Get_RegisterValues;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property Totals: Olevariant READ Get_Totals;
        property Peakcurrent: Olevariant READ Get_Peakcurrent WRITE Set_Peakcurrent;
        property CalcCurrent: Olevariant READ Get_CalcCurrent WRITE Set_CalcCurrent;
        property AllocFactors: Olevariant READ Get_AllocFactors WRITE Set_AllocFactors;
        property MeteredElement: Widestring READ Get_MeteredElement WRITE Set_MeteredElement;
        property MeteredTerminal: Integer READ Get_MeteredTerminal WRITE Set_MeteredTerminal;
        property DIFilesAreOpen: Wordbool READ Get_DIFilesAreOpen;
        property CountEndElements: Integer READ Get_CountEndElements;
        property AllEndElements: Olevariant READ Get_AllEndElements;
        property Count: Integer READ Get_Count;
        property AllBranchesInZone: Olevariant READ Get_AllBranchesInZone;
        property CountBranches: Integer READ Get_CountBranches;
        property SAIFI: Double READ Get_SAIFI;
        property SequenceIndex: Integer READ Get_SequenceIndex WRITE Set_SequenceIndex;
        property SAIFIKW: Double READ Get_SAIFIKW;
        property SeqListSize: Integer READ Get_SeqListSize;
        property TotalCustomers: Integer READ Get_TotalCustomers;
        property SAIDI: Double READ Get_SAIDI;
        property CustInterrupts: Double READ Get_CustInterrupts;
        property NumSections: Integer READ Get_NumSections;
        property OCPDeviceType: Integer READ Get_OCPDeviceType;
        property NumSectionCustomers: Integer READ Get_NumSectionCustomers;
        property NumSectionBranches: Integer READ Get_NumSectionBranches;
        property AvgRepairTime: Double READ Get_AvgRepairTime;
        property FaultRateXRepairHrs: Double READ Get_FaultRateXRepairHrs;
        property SumBranchFltRates: Double READ Get_SumBranchFltRates;
        property SectSeqIdx: Integer READ Get_SectSeqIdx;
        property SectTotalCust: Integer READ Get_SectTotalCust;
    end;

// *********************************************************************//
// DispIntf:  IMetersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {86705B6C-352A-47F8-A24B-78B750EC3859}
// *********************************************************************//
    IMetersDisp = dispinterface
        ['{86705B6C-352A-47F8-A24B-78B750EC3859}']
        property AllNames: Olevariant READONLY DISPID 2;
        property First: Integer READONLY DISPID 3;
        property Next: Integer READONLY DISPID 4;
        property RegisterNames: Olevariant READONLY DISPID 5;
        property RegisterValues: Olevariant READONLY DISPID 6;
        procedure Reset; DISPID 7;
        procedure ResetAll; DISPID 8;
        procedure Sample; DISPID 9;
        procedure Save; DISPID 10;
        property Name: Widestring DISPID 12;
        property Totals: Olevariant READONLY DISPID 1;
        property Peakcurrent: Olevariant DISPID 201;
        property CalcCurrent: Olevariant DISPID 202;
        property AllocFactors: Olevariant DISPID 203;
        property MeteredElement: Widestring DISPID 204;
        property MeteredTerminal: Integer DISPID 205;
        property DIFilesAreOpen: Wordbool READONLY DISPID 206;
        procedure SampleAll; DISPID 207;
        procedure SaveAll; DISPID 208;
        procedure OpenAllDIFiles; DISPID 209;
        procedure CloseAllDIFiles; DISPID 210;
        property CountEndElements: Integer READONLY DISPID 211;
        property AllEndElements: Olevariant READONLY DISPID 212;
        property Count: Integer READONLY DISPID 213;
        property AllBranchesInZone: Olevariant READONLY DISPID 214;
        property CountBranches: Integer READONLY DISPID 215;
        property SAIFI: Double READONLY DISPID 216;
        property SequenceIndex: Integer DISPID 217;
        property SAIFIKW: Double READONLY DISPID 218;
        procedure DoReliabilityCalc(AssumeRestoration: Wordbool); DISPID 219;
        property SeqListSize: Integer READONLY DISPID 220;
        property TotalCustomers: Integer READONLY DISPID 221;
        property SAIDI: Double READONLY DISPID 222;
        property CustInterrupts: Double READONLY DISPID 223;
        property NumSections: Integer READONLY DISPID 224;
        procedure SetActiveSection(SectIdx: Integer); DISPID 225;
        property OCPDeviceType: Integer READONLY DISPID 226;
        property NumSectionCustomers: Integer READONLY DISPID 227;
        property NumSectionBranches: Integer READONLY DISPID 228;
        property AvgRepairTime: Double READONLY DISPID 229;
        property FaultRateXRepairHrs: Double READONLY DISPID 230;
        property SumBranchFltRates: Double READONLY DISPID 231;
        property SectSeqIdx: Integer READONLY DISPID 232;
        property SectTotalCust: Integer READONLY DISPID 233;
    end;

// *********************************************************************//
// Interface: IGenerators
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2D9B7548-D03E-478A-9FEA-9FC4033C793E}
// *********************************************************************//
    IGenerators = interface(IDispatch)
        ['{2D9B7548-D03E-478A-9FEA-9FC4033C793E}']
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_RegisterNames: Olevariant; SAFECALL;
        function Get_RegisterValues: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_ForcedON: Wordbool; SAFECALL;
        procedure Set_ForcedON(Value: Wordbool); SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_kV: Double; SAFECALL;
        procedure Set_kV(Value: Double); SAFECALL;
        function Get_kW: Double; SAFECALL;
        procedure Set_kW(Value: Double); SAFECALL;
        function Get_kvar: Double; SAFECALL;
        procedure Set_kvar(Value: Double); SAFECALL;
        function Get_PF: Double; SAFECALL;
        procedure Set_PF(Value: Double); SAFECALL;
        function Get_Phases: Integer; SAFECALL;
        procedure Set_Phases(Value: Integer); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_idx: Integer; SAFECALL;
        procedure Set_idx(Value: Integer); SAFECALL;
        function Get_Model: Integer; SAFECALL;
        procedure Set_Model(Value: Integer); SAFECALL;
        function Get_kVArated: Double; SAFECALL;
        procedure Set_kVArated(Value: Double); SAFECALL;
        function Get_Vmaxpu: Double; SAFECALL;
        procedure Set_Vmaxpu(Value: Double); SAFECALL;
        function Get_Vminpu: Double; SAFECALL;
        procedure Set_Vminpu(Value: Double); SAFECALL;
        property AllNames: Olevariant READ Get_AllNames;
        property RegisterNames: Olevariant READ Get_RegisterNames;
        property RegisterValues: Olevariant READ Get_RegisterValues;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property ForcedON: Wordbool READ Get_ForcedON WRITE Set_ForcedON;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property kV: Double READ Get_kV WRITE Set_kV;
        property kW: Double READ Get_kW WRITE Set_kW;
        property kvar: Double READ Get_kvar WRITE Set_kvar;
        property PF: Double READ Get_PF WRITE Set_PF;
        property Phases: Integer READ Get_Phases WRITE Set_Phases;
        property Count: Integer READ Get_Count;
        property idx: Integer READ Get_idx WRITE Set_idx;
        property Model: Integer READ Get_Model WRITE Set_Model;
        property kVArated: Double READ Get_kVArated WRITE Set_kVArated;
        property Vmaxpu: Double READ Get_Vmaxpu WRITE Set_Vmaxpu;
        property Vminpu: Double READ Get_Vminpu WRITE Set_Vminpu;
    end;

// *********************************************************************//
// DispIntf:  IGeneratorsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2D9B7548-D03E-478A-9FEA-9FC4033C793E}
// *********************************************************************//
    IGeneratorsDisp = dispinterface
        ['{2D9B7548-D03E-478A-9FEA-9FC4033C793E}']
        property AllNames: Olevariant READONLY DISPID 2;
        property RegisterNames: Olevariant READONLY DISPID 3;
        property RegisterValues: Olevariant READONLY DISPID 4;
        property First: Integer READONLY DISPID 5;
        property Next: Integer READONLY DISPID 6;
        property ForcedON: Wordbool DISPID 8;
        property Name: Widestring DISPID 9;
        property kV: Double DISPID 201;
        property kW: Double DISPID 202;
        property kvar: Double DISPID 203;
        property PF: Double DISPID 204;
        property Phases: Integer DISPID 205;
        property Count: Integer READONLY DISPID 206;
        property idx: Integer DISPID 207;
        property Model: Integer DISPID 208;
        property kVArated: Double DISPID 209;
        property Vmaxpu: Double DISPID 210;
        property Vminpu: Double DISPID 211;
    end;

// *********************************************************************//
// Interface: IDSSProgress
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {315C0C38-929C-4942-BDF8-6DA12D001B47}
// *********************************************************************//
    IDSSProgress = interface(IDispatch)
        ['{315C0C38-929C-4942-BDF8-6DA12D001B47}']
        procedure Set_PctProgress(Param1: Integer); SAFECALL;
        procedure Set_Caption(const Param1: Widestring); SAFECALL;
        procedure Show; SAFECALL;
        procedure Close; SAFECALL;
        property PctProgress: Integer WRITE Set_PctProgress;
        property Caption: Widestring WRITE Set_Caption;
    end;

// *********************************************************************//
// DispIntf:  IDSSProgressDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {315C0C38-929C-4942-BDF8-6DA12D001B47}
// *********************************************************************//
    IDSSProgressDisp = dispinterface
        ['{315C0C38-929C-4942-BDF8-6DA12D001B47}']
        property PctProgress: Integer WRITEONLY DISPID 1;
        property Caption: Widestring WRITEONLY DISPID 2;
        procedure Show; DISPID 3;
        procedure Close; DISPID 4;
    end;

// *********************************************************************//
// Interface: ISettings
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4E3928A0-8B75-4127-885F-F4AD6B3F4323}
// *********************************************************************//
    ISettings = interface(IDispatch)
        ['{4E3928A0-8B75-4127-885F-F4AD6B3F4323}']
        function Get_AllowDuplicates: Wordbool; SAFECALL;
        procedure Set_AllowDuplicates(Value: Wordbool); SAFECALL;
        function Get_ZoneLock: Wordbool; SAFECALL;
        procedure Set_ZoneLock(Value: Wordbool); SAFECALL;
        procedure Set_AllocationFactors(Param1: Double); SAFECALL;
        function Get_AutoBusList: Widestring; SAFECALL;
        procedure Set_AutoBusList(const Value: Widestring); SAFECALL;
        function Get_CktModel: Integer; SAFECALL;
        procedure Set_CktModel(Value: Integer); SAFECALL;
        function Get_NormVminpu: Double; SAFECALL;
        procedure Set_NormVminpu(Value: Double); SAFECALL;
        function Get_NormVmaxpu: Double; SAFECALL;
        procedure Set_NormVmaxpu(Value: Double); SAFECALL;
        function Get_EmergVminpu: Double; SAFECALL;
        procedure Set_EmergVminpu(Value: Double); SAFECALL;
        function Get_EmergVmaxpu: Double; SAFECALL;
        procedure Set_EmergVmaxpu(Value: Double); SAFECALL;
        function Get_UEweight: Double; SAFECALL;
        procedure Set_UEweight(Value: Double); SAFECALL;
        function Get_LossWeight: Double; SAFECALL;
        procedure Set_LossWeight(Value: Double); SAFECALL;
        function Get_UEregs: Olevariant; SAFECALL;
        procedure Set_UEregs(Value: Olevariant); SAFECALL;
        function Get_LossRegs: Olevariant; SAFECALL;
        procedure Set_LossRegs(Value: Olevariant); SAFECALL;
        function Get_Trapezoidal: Wordbool; SAFECALL;
        procedure Set_Trapezoidal(Value: Wordbool); SAFECALL;
        function Get_VoltageBases: Olevariant; SAFECALL;
        procedure Set_VoltageBases(Value: Olevariant); SAFECALL;
        function Get_ControlTrace: Wordbool; SAFECALL;
        procedure Set_ControlTrace(Value: Wordbool); SAFECALL;
        function Get_PriceSignal: Double; SAFECALL;
        procedure Set_PriceSignal(Value: Double); SAFECALL;
        function Get_PriceCurve: Widestring; SAFECALL;
        procedure Set_PriceCurve(const Value: Widestring); SAFECALL;
        property AllowDuplicates: Wordbool READ Get_AllowDuplicates WRITE Set_AllowDuplicates;
        property ZoneLock: Wordbool READ Get_ZoneLock WRITE Set_ZoneLock;
        property AllocationFactors: Double WRITE Set_AllocationFactors;
        property AutoBusList: Widestring READ Get_AutoBusList WRITE Set_AutoBusList;
        property CktModel: Integer READ Get_CktModel WRITE Set_CktModel;
        property NormVminpu: Double READ Get_NormVminpu WRITE Set_NormVminpu;
        property NormVmaxpu: Double READ Get_NormVmaxpu WRITE Set_NormVmaxpu;
        property EmergVminpu: Double READ Get_EmergVminpu WRITE Set_EmergVminpu;
        property EmergVmaxpu: Double READ Get_EmergVmaxpu WRITE Set_EmergVmaxpu;
        property UEweight: Double READ Get_UEweight WRITE Set_UEweight;
        property LossWeight: Double READ Get_LossWeight WRITE Set_LossWeight;
        property UEregs: Olevariant READ Get_UEregs WRITE Set_UEregs;
        property LossRegs: Olevariant READ Get_LossRegs WRITE Set_LossRegs;
        property Trapezoidal: Wordbool READ Get_Trapezoidal WRITE Set_Trapezoidal;
        property VoltageBases: Olevariant READ Get_VoltageBases WRITE Set_VoltageBases;
        property ControlTrace: Wordbool READ Get_ControlTrace WRITE Set_ControlTrace;
        property PriceSignal: Double READ Get_PriceSignal WRITE Set_PriceSignal;
        property PriceCurve: Widestring READ Get_PriceCurve WRITE Set_PriceCurve;
    end;

// *********************************************************************//
// DispIntf:  ISettingsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4E3928A0-8B75-4127-885F-F4AD6B3F4323}
// *********************************************************************//
    ISettingsDisp = dispinterface
        ['{4E3928A0-8B75-4127-885F-F4AD6B3F4323}']
        property AllowDuplicates: Wordbool DISPID 1;
        property ZoneLock: Wordbool DISPID 2;
        property AllocationFactors: Double WRITEONLY DISPID 3;
        property AutoBusList: Widestring DISPID 4;
        property CktModel: Integer DISPID 5;
        property NormVminpu: Double DISPID 6;
        property NormVmaxpu: Double DISPID 7;
        property EmergVminpu: Double DISPID 8;
        property EmergVmaxpu: Double DISPID 9;
        property UEweight: Double DISPID 10;
        property LossWeight: Double DISPID 11;
        property UEregs: Olevariant DISPID 12;
        property LossRegs: Olevariant DISPID 13;
        property Trapezoidal: Wordbool DISPID 14;
        property VoltageBases: Olevariant DISPID 15;
        property ControlTrace: Wordbool DISPID 16;
        property PriceSignal: Double DISPID 17;
        property PriceCurve: Widestring DISPID 18;
    end;

// *********************************************************************//
// Interface: ILines
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E1616BDB-589B-4E5D-A7CE-828ACD73E5D4}
// *********************************************************************//
    ILines = interface(IDispatch)
        ['{E1616BDB-589B-4E5D-A7CE-828ACD73E5D4}']
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function New(const Name: Widestring): Integer; SAFECALL;
        function Get_Bus1: Widestring; SAFECALL;
        procedure Set_Bus1(const Value: Widestring); SAFECALL;
        function Get_Bus2: Widestring; SAFECALL;
        procedure Set_Bus2(const Value: Widestring); SAFECALL;
        function Get_LineCode: Widestring; SAFECALL;
        procedure Set_LineCode(const Value: Widestring); SAFECALL;
        function Get_Length: Double; SAFECALL;
        procedure Set_Length(Value: Double); SAFECALL;
        function Get_Phases: Integer; SAFECALL;
        procedure Set_Phases(Value: Integer); SAFECALL;
        function Get_R1: Double; SAFECALL;
        procedure Set_R1(Value: Double); SAFECALL;
        function Get_X1: Double; SAFECALL;
        procedure Set_X1(Value: Double); SAFECALL;
        function Get_R0: Double; SAFECALL;
        procedure Set_R0(Value: Double); SAFECALL;
        function Get_X0: Double; SAFECALL;
        procedure Set_X0(Value: Double); SAFECALL;
        function Get_C1: Double; SAFECALL;
        procedure Set_C1(Value: Double); SAFECALL;
        function Get_C0: Double; SAFECALL;
        procedure Set_C0(Value: Double); SAFECALL;
        function Get_Rmatrix: Olevariant; SAFECALL;
        procedure Set_Rmatrix(Value: Olevariant); SAFECALL;
        function Get_Xmatrix: Olevariant; SAFECALL;
        procedure Set_Xmatrix(Value: Olevariant); SAFECALL;
        function Get_Cmatrix: Olevariant; SAFECALL;
        procedure Set_Cmatrix(Value: Olevariant); SAFECALL;
        function Get_NormAmps: Double; SAFECALL;
        procedure Set_NormAmps(Value: Double); SAFECALL;
        function Get_EmergAmps: Double; SAFECALL;
        procedure Set_EmergAmps(Value: Double); SAFECALL;
        function Get_Geometry: Widestring; SAFECALL;
        procedure Set_Geometry(const Value: Widestring); SAFECALL;
        function Get_Rg: Double; SAFECALL;
        procedure Set_Rg(Value: Double); SAFECALL;
        function Get_Xg: Double; SAFECALL;
        procedure Set_Xg(Value: Double); SAFECALL;
        function Get_Rho: Double; SAFECALL;
        procedure Set_Rho(Value: Double); SAFECALL;
        function Get_Yprim: Olevariant; SAFECALL;
        procedure Set_Yprim(Value: Olevariant); SAFECALL;
        function Get_NumCust: Integer; SAFECALL;
        function Get_TotalCust: Integer; SAFECALL;
        function Get_Parent: Integer; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_Spacing: Widestring; SAFECALL;
        procedure Set_Spacing(const Value: Widestring); SAFECALL;
        function Get_Units: Integer; SAFECALL;
        procedure Set_Units(Value: Integer); SAFECALL;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property AllNames: Olevariant READ Get_AllNames;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property Bus1: Widestring READ Get_Bus1 WRITE Set_Bus1;
        property Bus2: Widestring READ Get_Bus2 WRITE Set_Bus2;
        property LineCode: Widestring READ Get_LineCode WRITE Set_LineCode;
        property Length: Double READ Get_Length WRITE Set_Length;
        property Phases: Integer READ Get_Phases WRITE Set_Phases;
        property R1: Double READ Get_R1 WRITE Set_R1;
        property X1: Double READ Get_X1 WRITE Set_X1;
        property R0: Double READ Get_R0 WRITE Set_R0;
        property X0: Double READ Get_X0 WRITE Set_X0;
        property C1: Double READ Get_C1 WRITE Set_C1;
        property C0: Double READ Get_C0 WRITE Set_C0;
        property Rmatrix: Olevariant READ Get_Rmatrix WRITE Set_Rmatrix;
        property Xmatrix: Olevariant READ Get_Xmatrix WRITE Set_Xmatrix;
        property Cmatrix: Olevariant READ Get_Cmatrix WRITE Set_Cmatrix;
        property NormAmps: Double READ Get_NormAmps WRITE Set_NormAmps;
        property EmergAmps: Double READ Get_EmergAmps WRITE Set_EmergAmps;
        property Geometry: Widestring READ Get_Geometry WRITE Set_Geometry;
        property Rg: Double READ Get_Rg WRITE Set_Rg;
        property Xg: Double READ Get_Xg WRITE Set_Xg;
        property Rho: Double READ Get_Rho WRITE Set_Rho;
        property Yprim: Olevariant READ Get_Yprim WRITE Set_Yprim;
        property NumCust: Integer READ Get_NumCust;
        property TotalCust: Integer READ Get_TotalCust;
        property Parent: Integer READ Get_Parent;
        property Count: Integer READ Get_Count;
        property Spacing: Widestring READ Get_Spacing WRITE Set_Spacing;
        property Units: Integer READ Get_Units WRITE Set_Units;
    end;

// *********************************************************************//
// DispIntf:  ILinesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E1616BDB-589B-4E5D-A7CE-828ACD73E5D4}
// *********************************************************************//
    ILinesDisp = dispinterface
        ['{E1616BDB-589B-4E5D-A7CE-828ACD73E5D4}']
        property Name: Widestring DISPID 6;
        property AllNames: Olevariant READONLY DISPID 7;
        property First: Integer READONLY DISPID 8;
        property Next: Integer READONLY DISPID 9;
        function New(const Name: Widestring): Integer; DISPID 10;
        property Bus1: Widestring DISPID 11;
        property Bus2: Widestring DISPID 12;
        property LineCode: Widestring DISPID 13;
        property Length: Double DISPID 14;
        property Phases: Integer DISPID 15;
        property R1: Double DISPID 16;
        property X1: Double DISPID 17;
        property R0: Double DISPID 18;
        property X0: Double DISPID 19;
        property C1: Double DISPID 20;
        property C0: Double DISPID 21;
        property Rmatrix: Olevariant DISPID 22;
        property Xmatrix: Olevariant DISPID 23;
        property Cmatrix: Olevariant DISPID 24;
        property NormAmps: Double DISPID 25;
        property EmergAmps: Double DISPID 26;
        property Geometry: Widestring DISPID 1;
        property Rg: Double DISPID 2;
        property Xg: Double DISPID 3;
        property Rho: Double DISPID 4;
        property Yprim: Olevariant DISPID 5;
        property NumCust: Integer READONLY DISPID 201;
        property TotalCust: Integer READONLY DISPID 202;
        property Parent: Integer READONLY DISPID 203;
        property Count: Integer READONLY DISPID 204;
        property Spacing: Widestring DISPID 205;
        property Units: Integer DISPID 206;
    end;

// *********************************************************************//
// Interface: ICtrlQueue
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {55055001-5EEC-4667-9CCA-63F3A60F31F3}
// *********************************************************************//
    ICtrlQueue = interface(IDispatch)
        ['{55055001-5EEC-4667-9CCA-63F3A60F31F3}']
        procedure ClearQueue; SAFECALL;
        procedure Delete(ActionHandle: Integer); SAFECALL;
        function Get_NumActions: Integer; SAFECALL;
        procedure Set_Action(Param1: Integer); SAFECALL;
        function Get_ActionCode: Integer; SAFECALL;
        function Get_DeviceHandle: Integer; SAFECALL;
        function Push(Hour: Integer; Seconds: Double; ActionCode: Integer; DeviceHandle: Integer): Integer; SAFECALL;
        procedure Show; SAFECALL;
        procedure ClearActions; SAFECALL;
        function Get_PopAction: Integer; SAFECALL;
        function Get_QueueSize: Integer; SAFECALL;
        procedure DoAllQueue; SAFECALL;
        function Get_Queue: Olevariant; SAFECALL;
        property NumActions: Integer READ Get_NumActions;
        property Action: Integer WRITE Set_Action;
        property ActionCode: Integer READ Get_ActionCode;
        property DeviceHandle: Integer READ Get_DeviceHandle;
        property PopAction: Integer READ Get_PopAction;
        property QueueSize: Integer READ Get_QueueSize;
        property Queue: Olevariant READ Get_Queue;
    end;

// *********************************************************************//
// DispIntf:  ICtrlQueueDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {55055001-5EEC-4667-9CCA-63F3A60F31F3}
// *********************************************************************//
    ICtrlQueueDisp = dispinterface
        ['{55055001-5EEC-4667-9CCA-63F3A60F31F3}']
        procedure ClearQueue; DISPID 101;
        procedure Delete(ActionHandle: Integer); DISPID 103;
        property NumActions: Integer READONLY DISPID 104;
        property Action: Integer WRITEONLY DISPID 102;
        property ActionCode: Integer READONLY DISPID 105;
        property DeviceHandle: Integer READONLY DISPID 106;
        function Push(Hour: Integer; Seconds: Double; ActionCode: Integer; DeviceHandle: Integer): Integer; DISPID 107;
        procedure Show; DISPID 108;
        procedure ClearActions; DISPID 109;
        property PopAction: Integer READONLY DISPID 110;
        property QueueSize: Integer READONLY DISPID 201;
        procedure DoAllQueue; DISPID 202;
        property Queue: Olevariant READONLY DISPID 203;
    end;

// *********************************************************************//
// Interface: ILoads
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9A3FFA05-5B82-488C-B08D-FCA2FDB23101}
// *********************************************************************//
    ILoads = interface(IDispatch)
        ['{9A3FFA05-5B82-488C-B08D-FCA2FDB23101}']
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Idx: Integer; SAFECALL;
        procedure Set_Idx(Value: Integer); SAFECALL;
        function Get_kW: Double; SAFECALL;
        procedure Set_kW(Value: Double); SAFECALL;
        function Get_kV: Double; SAFECALL;
        procedure Set_kV(Value: Double); SAFECALL;
        function Get_kvar: Double; SAFECALL;
        procedure Set_kvar(Value: Double); SAFECALL;
        function Get_PF: Double; SAFECALL;
        procedure Set_PF(Value: Double); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_PctMean: Double; SAFECALL;
        procedure Set_PctMean(Value: Double); SAFECALL;
        function Get_PctStdDev: Double; SAFECALL;
        procedure Set_PctStdDev(Value: Double); SAFECALL;
        function Get_AllocationFactor: Double; SAFECALL;
        procedure Set_AllocationFactor(Value: Double); SAFECALL;
        function Get_Cfactor: Double; SAFECALL;
        procedure Set_Cfactor(Value: Double); SAFECALL;
        function Get_Class_: Integer; SAFECALL;
        procedure Set_Class_(Value: Integer); SAFECALL;
        function Get_IsDelta: Wordbool; SAFECALL;
        procedure Set_IsDelta(Value: Wordbool); SAFECALL;
        function Get_CVRcurve: Widestring; SAFECALL;
        procedure Set_CVRcurve(const Value: Widestring); SAFECALL;
        function Get_CVRwatts: Double; SAFECALL;
        procedure Set_CVRwatts(Value: Double); SAFECALL;
        function Get_CVRvars: Double; SAFECALL;
        procedure Set_CVRvars(Value: Double); SAFECALL;
        function Get_daily: Widestring; SAFECALL;
        procedure Set_daily(const Value: Widestring); SAFECALL;
        function Get_duty: Widestring; SAFECALL;
        procedure Set_duty(const Value: Widestring); SAFECALL;
        function Get_kva: Double; SAFECALL;
        procedure Set_kva(Value: Double); SAFECALL;
        function Get_kwh: Double; SAFECALL;
        procedure Set_kwh(Value: Double); SAFECALL;
        function Get_kwhdays: Double; SAFECALL;
        procedure Set_kwhdays(Value: Double); SAFECALL;
        function Get_Model: LoadModels; SAFECALL;
        procedure Set_Model(Value: LoadModels); SAFECALL;
        function Get_NumCust: Integer; SAFECALL;
        procedure Set_NumCust(Value: Integer); SAFECALL;
        function Get_Rneut: Double; SAFECALL;
        procedure Set_Rneut(Value: Double); SAFECALL;
        function Get_Spectrum: Widestring; SAFECALL;
        procedure Set_Spectrum(const Value: Widestring); SAFECALL;
        function Get_Vmaxpu: Double; SAFECALL;
        procedure Set_Vmaxpu(Value: Double); SAFECALL;
        function Get_Vminemerg: Double; SAFECALL;
        procedure Set_Vminemerg(Value: Double); SAFECALL;
        function Get_Vminnorm: Double; SAFECALL;
        procedure Set_Vminnorm(Value: Double); SAFECALL;
        function Get_Vminpu: Double; SAFECALL;
        procedure Set_Vminpu(Value: Double); SAFECALL;
        function Get_xfkVA: Double; SAFECALL;
        procedure Set_xfkVA(Value: Double); SAFECALL;
        function Get_Xneut: Double; SAFECALL;
        procedure Set_Xneut(Value: Double); SAFECALL;
        function Get_Yearly: Widestring; SAFECALL;
        procedure Set_Yearly(const Value: Widestring); SAFECALL;
        function Get_Status: LoadStatus; SAFECALL;
        procedure Set_Status(Value: LoadStatus); SAFECALL;
        function Get_Growth: Widestring; SAFECALL;
        procedure Set_Growth(const Value: Widestring); SAFECALL;
        function Get_ZIPV: Olevariant; SAFECALL;
        procedure Set_ZIPV(Value: Olevariant); SAFECALL;
        function Get_pctSeriesRL: Double; SAFECALL;
        procedure Set_pctSeriesRL(Value: Double); SAFECALL;
        function Get_RelWeight: Double; SAFECALL;
        procedure Set_RelWeight(Value: Double); STDCALL;
        property AllNames: Olevariant READ Get_AllNames;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property Idx: Integer READ Get_Idx WRITE Set_Idx;
        property kW: Double READ Get_kW WRITE Set_kW;
        property kV: Double READ Get_kV WRITE Set_kV;
        property kvar: Double READ Get_kvar WRITE Set_kvar;
        property PF: Double READ Get_PF WRITE Set_PF;
        property Count: Integer READ Get_Count;
        property PctMean: Double READ Get_PctMean WRITE Set_PctMean;
        property PctStdDev: Double READ Get_PctStdDev WRITE Set_PctStdDev;
        property AllocationFactor: Double READ Get_AllocationFactor WRITE Set_AllocationFactor;
        property Cfactor: Double READ Get_Cfactor WRITE Set_Cfactor;
        property Class_: Integer READ Get_Class_ WRITE Set_Class_;
        property IsDelta: Wordbool READ Get_IsDelta WRITE Set_IsDelta;
        property CVRcurve: Widestring READ Get_CVRcurve WRITE Set_CVRcurve;
        property CVRwatts: Double READ Get_CVRwatts WRITE Set_CVRwatts;
        property CVRvars: Double READ Get_CVRvars WRITE Set_CVRvars;
        property daily: Widestring READ Get_daily WRITE Set_daily;
        property duty: Widestring READ Get_duty WRITE Set_duty;
        property kva: Double READ Get_kva WRITE Set_kva;
        property kwh: Double READ Get_kwh WRITE Set_kwh;
        property kwhdays: Double READ Get_kwhdays WRITE Set_kwhdays;
        property Model: LoadModels READ Get_Model WRITE Set_Model;
        property NumCust: Integer READ Get_NumCust WRITE Set_NumCust;
        property Rneut: Double READ Get_Rneut WRITE Set_Rneut;
        property Spectrum: Widestring READ Get_Spectrum WRITE Set_Spectrum;
        property Vmaxpu: Double READ Get_Vmaxpu WRITE Set_Vmaxpu;
        property Vminemerg: Double READ Get_Vminemerg WRITE Set_Vminemerg;
        property Vminnorm: Double READ Get_Vminnorm WRITE Set_Vminnorm;
        property Vminpu: Double READ Get_Vminpu WRITE Set_Vminpu;
        property xfkVA: Double READ Get_xfkVA WRITE Set_xfkVA;
        property Xneut: Double READ Get_Xneut WRITE Set_Xneut;
        property Yearly: Widestring READ Get_Yearly WRITE Set_Yearly;
        property Status: LoadStatus READ Get_Status WRITE Set_Status;
        property Growth: Widestring READ Get_Growth WRITE Set_Growth;
        property ZIPV: Olevariant READ Get_ZIPV WRITE Set_ZIPV;
        property pctSeriesRL: Double READ Get_pctSeriesRL WRITE Set_pctSeriesRL;
    // Skipped Property "RelWeight"
    end;

// *********************************************************************//
// DispIntf:  ILoadsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9A3FFA05-5B82-488C-B08D-FCA2FDB23101}
// *********************************************************************//
    ILoadsDisp = dispinterface
        ['{9A3FFA05-5B82-488C-B08D-FCA2FDB23101}']
        property AllNames: Olevariant READONLY DISPID 201;
        property First: Integer READONLY DISPID 202;
        property Next: Integer READONLY DISPID 203;
        property Name: Widestring DISPID 204;
        property Idx: Integer DISPID 205;
        property kW: Double DISPID 206;
        property kV: Double DISPID 207;
        property kvar: Double DISPID 208;
        property PF: Double DISPID 209;
        property Count: Integer READONLY DISPID 210;
        property PctMean: Double DISPID 211;
        property PctStdDev: Double DISPID 212;
        property AllocationFactor: Double DISPID 213;
        property Cfactor: Double DISPID 214;
        property Class_: Integer DISPID 215;
        property IsDelta: Wordbool DISPID 216;
        property CVRcurve: Widestring DISPID 217;
        property CVRwatts: Double DISPID 218;
        property CVRvars: Double DISPID 219;
        property daily: Widestring DISPID 220;
        property duty: Widestring DISPID 221;
        property kva: Double DISPID 223;
        property kwh: Double DISPID 224;
        property kwhdays: Double DISPID 225;
        property Model: LoadModels DISPID 226;
        property NumCust: Integer DISPID 227;
        property Rneut: Double DISPID 228;
        property Spectrum: Widestring DISPID 229;
        property Vmaxpu: Double DISPID 230;
        property Vminemerg: Double DISPID 231;
        property Vminnorm: Double DISPID 232;
        property Vminpu: Double DISPID 233;
        property xfkVA: Double DISPID 234;
        property Xneut: Double DISPID 235;
        property Yearly: Widestring DISPID 236;
        property Status: LoadStatus DISPID 237;
        property Growth: Widestring DISPID 222;
        property ZIPV: Olevariant DISPID 238;
        property pctSeriesRL: Double DISPID 239;
        function RelWeight: Double; DISPID 240;
    end;

// *********************************************************************//
// Interface: IDSSElement
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C22D4922-6DC2-4283-93AB-4F2138C4B922}
// *********************************************************************//
    IDSSElement = interface(IDispatch)
        ['{C22D4922-6DC2-4283-93AB-4F2138C4B922}']
        function Get_Name: Widestring; SAFECALL;
        function Get_Properties(Indx: Olevariant): IDSSProperty; SAFECALL;
        function Get_NumProperties: Integer; SAFECALL;
        function Get_AllPropertyNames: Olevariant; SAFECALL;
        property Name: Widestring READ Get_Name;
        property Properties[Indx: Olevariant]: IDSSProperty READ Get_Properties;
        property NumProperties: Integer READ Get_NumProperties;
        property AllPropertyNames: Olevariant READ Get_AllPropertyNames;
    end;

// *********************************************************************//
// DispIntf:  IDSSElementDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C22D4922-6DC2-4283-93AB-4F2138C4B922}
// *********************************************************************//
    IDSSElementDisp = dispinterface
        ['{C22D4922-6DC2-4283-93AB-4F2138C4B922}']
        property Name: Widestring READONLY DISPID 201;
        property Properties[Indx: Olevariant]: IDSSProperty READONLY DISPID 202;
        property NumProperties: Integer READONLY DISPID 203;
        property AllPropertyNames: Olevariant READONLY DISPID 204;
    end;

// *********************************************************************//
// Interface: IActiveClass
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E73B64C-0D99-4D19-AB90-170DBBD06FA0}
// *********************************************************************//
    IActiveClass = interface(IDispatch)
        ['{8E73B64C-0D99-4D19-AB90-170DBBD06FA0}']
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_NumElements: Integer; SAFECALL;
        function Get_ActiveClassName: Widestring; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        property AllNames: Olevariant READ Get_AllNames;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property NumElements: Integer READ Get_NumElements;
        property ActiveClassName: Widestring READ Get_ActiveClassName;
        property Count: Integer READ Get_Count;
    end;

// *********************************************************************//
// DispIntf:  IActiveClassDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E73B64C-0D99-4D19-AB90-170DBBD06FA0}
// *********************************************************************//
    IActiveClassDisp = dispinterface
        ['{8E73B64C-0D99-4D19-AB90-170DBBD06FA0}']
        property AllNames: Olevariant READONLY DISPID 201;
        property First: Integer READONLY DISPID 202;
        property Next: Integer READONLY DISPID 203;
        property Name: Widestring DISPID 204;
        property NumElements: Integer READONLY DISPID 205;
        property ActiveClassName: Widestring READONLY DISPID 206;
        property Count: Integer READONLY DISPID 207;
    end;

// *********************************************************************//
// Interface: ICapacitors
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3C171A69-40AB-46AA-B037-9C4EBB9FBFCD}
// *********************************************************************//
    ICapacitors = interface(IDispatch)
        ['{3C171A69-40AB-46AA-B037-9C4EBB9FBFCD}']
        function Get_kV: Double; SAFECALL;
        procedure Set_kV(Value: Double); SAFECALL;
        function Get_kvar: Double; SAFECALL;
        procedure Set_kvar(Value: Double); SAFECALL;
        function Get_NumSteps: Integer; SAFECALL;
        procedure Set_NumSteps(Value: Integer); SAFECALL;
        function Get_IsDelta: Wordbool; SAFECALL;
        procedure Set_IsDelta(Value: Wordbool); SAFECALL;
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function AddStep: Wordbool; SAFECALL;
        function SubtractStep: Wordbool; SAFECALL;
        function Get_AvailableSteps: Integer; SAFECALL;
        function Get_States: Olevariant; SAFECALL;
        procedure Set_States(Value: Olevariant); SAFECALL;
        procedure Open; SAFECALL;
        procedure Close; SAFECALL;
        property kV: Double READ Get_kV WRITE Set_kV;
        property kvar: Double READ Get_kvar WRITE Set_kvar;
        property NumSteps: Integer READ Get_NumSteps WRITE Set_NumSteps;
        property IsDelta: Wordbool READ Get_IsDelta WRITE Set_IsDelta;
        property AllNames: Olevariant READ Get_AllNames;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property Count: Integer READ Get_Count;
        property AvailableSteps: Integer READ Get_AvailableSteps;
        property States: Olevariant READ Get_States WRITE Set_States;
    end;

// *********************************************************************//
// DispIntf:  ICapacitorsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3C171A69-40AB-46AA-B037-9C4EBB9FBFCD}
// *********************************************************************//
    ICapacitorsDisp = dispinterface
        ['{3C171A69-40AB-46AA-B037-9C4EBB9FBFCD}']
        property kV: Double DISPID 201;
        property kvar: Double DISPID 202;
        property NumSteps: Integer DISPID 203;
        property IsDelta: Wordbool DISPID 204;
        property AllNames: Olevariant READONLY DISPID 205;
        property First: Integer READONLY DISPID 206;
        property Next: Integer READONLY DISPID 207;
        property Name: Widestring DISPID 208;
        property Count: Integer READONLY DISPID 209;
        function AddStep: Wordbool; DISPID 210;
        function SubtractStep: Wordbool; DISPID 211;
        property AvailableSteps: Integer READONLY DISPID 212;
        property States: Olevariant DISPID 213;
        procedure Open; DISPID 214;
        procedure Close; DISPID 215;
    end;

// *********************************************************************//
// Interface: ITransformers
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {94E9CACF-A548-4DC2-B460-E2642B501387}
// *********************************************************************//
    ITransformers = interface(IDispatch)
        ['{94E9CACF-A548-4DC2-B460-E2642B501387}']
        function Get_NumWindings: Integer; SAFECALL;
        procedure Set_NumWindings(Value: Integer); SAFECALL;
        function Get_XfmrCode: Widestring; SAFECALL;
        procedure Set_XfmrCode(const Value: Widestring); SAFECALL;
        function Get_Wdg: Integer; SAFECALL;
        procedure Set_Wdg(Value: Integer); SAFECALL;
        function Get_R: Double; SAFECALL;
        procedure Set_R(Value: Double); SAFECALL;
        function Get_Tap: Double; SAFECALL;
        procedure Set_Tap(Value: Double); SAFECALL;
        function Get_MinTap: Double; SAFECALL;
        procedure Set_MinTap(Value: Double); SAFECALL;
        function Get_MaxTap: Double; SAFECALL;
        procedure Set_MaxTap(Value: Double); SAFECALL;
        function Get_NumTaps: Integer; SAFECALL;
        procedure Set_NumTaps(Value: Integer); SAFECALL;
        function Get_kV: Double; SAFECALL;
        procedure Set_kV(Value: Double); SAFECALL;
        function Get_kVA: Double; SAFECALL;
        procedure Set_kVA(Value: Double); SAFECALL;
        function Get_Xneut: Double; SAFECALL;
        procedure Set_Xneut(Value: Double); SAFECALL;
        function Get_Rneut: Double; SAFECALL;
        procedure Set_Rneut(Value: Double); SAFECALL;
        function Get_IsDelta: Wordbool; SAFECALL;
        procedure Set_IsDelta(Value: Wordbool); SAFECALL;
        function Get_Xhl: Double; SAFECALL;
        procedure Set_Xhl(Value: Double); SAFECALL;
        function Get_Xht: Double; SAFECALL;
        procedure Set_Xht(Value: Double); SAFECALL;
        function Get_Xlt: Double; SAFECALL;
        procedure Set_Xlt(Value: Double); SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_CoreType: Integer; SAFECALL;
        procedure Set_CoreType(Value: Integer); SAFECALL;
        function Get_WdgVoltages: Olevariant; SAFECALL;
        function Get_WdgCurrents: Olevariant; SAFECALL;
        function Get_StrWdgCurrents: Widestring; SAFECALL;
        function Get_RdcOhms: Double; SAFECALL;
        procedure Set_RdcOhms(Value: Double); SAFECALL;
        property NumWindings: Integer READ Get_NumWindings WRITE Set_NumWindings;
        property XfmrCode: Widestring READ Get_XfmrCode WRITE Set_XfmrCode;
        property Wdg: Integer READ Get_Wdg WRITE Set_Wdg;
        property R: Double READ Get_R WRITE Set_R;
        property Tap: Double READ Get_Tap WRITE Set_Tap;
        property MinTap: Double READ Get_MinTap WRITE Set_MinTap;
        property MaxTap: Double READ Get_MaxTap WRITE Set_MaxTap;
        property NumTaps: Integer READ Get_NumTaps WRITE Set_NumTaps;
        property kV: Double READ Get_kV WRITE Set_kV;
        property kVA: Double READ Get_kVA WRITE Set_kVA;
        property Xneut: Double READ Get_Xneut WRITE Set_Xneut;
        property Rneut: Double READ Get_Rneut WRITE Set_Rneut;
        property IsDelta: Wordbool READ Get_IsDelta WRITE Set_IsDelta;
        property Xhl: Double READ Get_Xhl WRITE Set_Xhl;
        property Xht: Double READ Get_Xht WRITE Set_Xht;
        property Xlt: Double READ Get_Xlt WRITE Set_Xlt;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property AllNames: Olevariant READ Get_AllNames;
        property Count: Integer READ Get_Count;
        property CoreType: Integer READ Get_CoreType WRITE Set_CoreType;
        property WdgVoltages: Olevariant READ Get_WdgVoltages;
        property WdgCurrents: Olevariant READ Get_WdgCurrents;
        property StrWdgCurrents: Widestring READ Get_StrWdgCurrents;
        property RdcOhms: Double READ Get_RdcOhms WRITE Set_RdcOhms;
    end;

// *********************************************************************//
// DispIntf:  ITransformersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {94E9CACF-A548-4DC2-B460-E2642B501387}
// *********************************************************************//
    ITransformersDisp = dispinterface
        ['{94E9CACF-A548-4DC2-B460-E2642B501387}']
        property NumWindings: Integer DISPID 201;
        property XfmrCode: Widestring DISPID 202;
        property Wdg: Integer DISPID 203;
        property R: Double DISPID 204;
        property Tap: Double DISPID 205;
        property MinTap: Double DISPID 206;
        property MaxTap: Double DISPID 207;
        property NumTaps: Integer DISPID 208;
        property kV: Double DISPID 209;
        property kVA: Double DISPID 210;
        property Xneut: Double DISPID 211;
        property Rneut: Double DISPID 212;
        property IsDelta: Wordbool DISPID 213;
        property Xhl: Double DISPID 214;
        property Xht: Double DISPID 215;
        property Xlt: Double DISPID 216;
        property Name: Widestring DISPID 217;
        property First: Integer READONLY DISPID 218;
        property Next: Integer READONLY DISPID 219;
        property AllNames: Olevariant READONLY DISPID 220;
        property Count: Integer READONLY DISPID 221;
        property CoreType: Integer DISPID 222;
        property WdgVoltages: Olevariant READONLY DISPID 223;
        property WdgCurrents: Olevariant READONLY DISPID 224;
        property StrWdgCurrents: Widestring READONLY DISPID 225;
        property RdcOhms: Double DISPID 226;
    end;

// *********************************************************************//
// Interface: ISwtControls
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {112AB9E6-C112-46BE-A8A3-F72C5FA3A657}
// *********************************************************************//
    ISwtControls = interface(IDispatch)
        ['{112AB9E6-C112-46BE-A8A3-F72C5FA3A657}']
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Action: ActionCodes; SAFECALL;
        procedure Set_Action(Value: ActionCodes); SAFECALL;
        function Get_IsLocked: Wordbool; SAFECALL;
        procedure Set_IsLocked(Value: Wordbool); SAFECALL;
        function Get_Delay: Double; SAFECALL;
        procedure Set_Delay(Value: Double); SAFECALL;
        function Get_SwitchedObj: Widestring; SAFECALL;
        procedure Set_SwitchedObj(const Value: Widestring); SAFECALL;
        function Get_SwitchedTerm: Integer; SAFECALL;
        procedure Set_SwitchedTerm(Value: Integer); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_NormalState: ActionCodes; SAFECALL;
        procedure Set_NormalState(Value: ActionCodes); SAFECALL;
        function Get_State: ActionCodes; SAFECALL;
        procedure Set_State(Value: ActionCodes); SAFECALL;
        procedure Reset; SAFECALL;
        property AllNames: Olevariant READ Get_AllNames;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property Action: ActionCodes READ Get_Action WRITE Set_Action;
        property IsLocked: Wordbool READ Get_IsLocked WRITE Set_IsLocked;
        property Delay: Double READ Get_Delay WRITE Set_Delay;
        property SwitchedObj: Widestring READ Get_SwitchedObj WRITE Set_SwitchedObj;
        property SwitchedTerm: Integer READ Get_SwitchedTerm WRITE Set_SwitchedTerm;
        property Count: Integer READ Get_Count;
        property NormalState: ActionCodes READ Get_NormalState WRITE Set_NormalState;
        property State: ActionCodes READ Get_State WRITE Set_State;
    end;

// *********************************************************************//
// DispIntf:  ISwtControlsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {112AB9E6-C112-46BE-A8A3-F72C5FA3A657}
// *********************************************************************//
    ISwtControlsDisp = dispinterface
        ['{112AB9E6-C112-46BE-A8A3-F72C5FA3A657}']
        property AllNames: Olevariant READONLY DISPID 201;
        property Name: Widestring DISPID 202;
        property First: Integer READONLY DISPID 203;
        property Next: Integer READONLY DISPID 204;
        property Action: ActionCodes DISPID 205;
        property IsLocked: Wordbool DISPID 206;
        property Delay: Double DISPID 207;
        property SwitchedObj: Widestring DISPID 208;
        property SwitchedTerm: Integer DISPID 209;
        property Count: Integer READONLY DISPID 210;
        property NormalState: ActionCodes DISPID 211;
        property State: ActionCodes DISPID 212;
        procedure Reset; DISPID 213;
    end;

// *********************************************************************//
// Interface: ICapControls
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4C132096-4161-4D9B-A701-E6CCCFF1D5AE}
// *********************************************************************//
    ICapControls = interface(IDispatch)
        ['{4C132096-4161-4D9B-A701-E6CCCFF1D5AE}']
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Mode: CapControlModes; SAFECALL;
        procedure Set_Mode(Value: CapControlModes); SAFECALL;
        function Get_Capacitor: Widestring; SAFECALL;
        procedure Set_Capacitor(const Value: Widestring); SAFECALL;
        function Get_MonitoredObj: Widestring; SAFECALL;
        procedure Set_MonitoredObj(const Value: Widestring); SAFECALL;
        function Get_MonitoredTerm: Integer; SAFECALL;
        procedure Set_MonitoredTerm(Value: Integer); SAFECALL;
        function Get_CTratio: Double; SAFECALL;
        procedure Set_CTratio(Value: Double); SAFECALL;
        function Get_PTratio: Double; SAFECALL;
        procedure Set_PTratio(Value: Double); SAFECALL;
        function Get_ONSetting: Double; SAFECALL;
        procedure Set_ONSetting(Value: Double); SAFECALL;
        function Get_OFFSetting: Double; SAFECALL;
        procedure Set_OFFSetting(Value: Double); SAFECALL;
        function Get_Vmax: Double; SAFECALL;
        procedure Set_Vmax(Value: Double); SAFECALL;
        function Get_Vmin: Double; SAFECALL;
        procedure Set_Vmin(Value: Double); SAFECALL;
        function Get_UseVoltOverride: Wordbool; SAFECALL;
        procedure Set_UseVoltOverride(Value: Wordbool); SAFECALL;
        function Get_Delay: Double; SAFECALL;
        procedure Set_Delay(Value: Double); SAFECALL;
        function Get_DelayOff: Double; SAFECALL;
        procedure Set_DelayOff(Value: Double); SAFECALL;
        function Get_DeadTime: Double; SAFECALL;
        procedure Set_DeadTime(Value: Double); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        property AllNames: Olevariant READ Get_AllNames;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property Mode: CapControlModes READ Get_Mode WRITE Set_Mode;
        property Capacitor: Widestring READ Get_Capacitor WRITE Set_Capacitor;
        property MonitoredObj: Widestring READ Get_MonitoredObj WRITE Set_MonitoredObj;
        property MonitoredTerm: Integer READ Get_MonitoredTerm WRITE Set_MonitoredTerm;
        property CTratio: Double READ Get_CTratio WRITE Set_CTratio;
        property PTratio: Double READ Get_PTratio WRITE Set_PTratio;
        property ONSetting: Double READ Get_ONSetting WRITE Set_ONSetting;
        property OFFSetting: Double READ Get_OFFSetting WRITE Set_OFFSetting;
        property Vmax: Double READ Get_Vmax WRITE Set_Vmax;
        property Vmin: Double READ Get_Vmin WRITE Set_Vmin;
        property UseVoltOverride: Wordbool READ Get_UseVoltOverride WRITE Set_UseVoltOverride;
        property Delay: Double READ Get_Delay WRITE Set_Delay;
        property DelayOff: Double READ Get_DelayOff WRITE Set_DelayOff;
        property DeadTime: Double READ Get_DeadTime WRITE Set_DeadTime;
        property Count: Integer READ Get_Count;
    end;

// *********************************************************************//
// DispIntf:  ICapControlsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4C132096-4161-4D9B-A701-E6CCCFF1D5AE}
// *********************************************************************//
    ICapControlsDisp = dispinterface
        ['{4C132096-4161-4D9B-A701-E6CCCFF1D5AE}']
        property AllNames: Olevariant READONLY DISPID 201;
        property Name: Widestring DISPID 202;
        property First: Integer READONLY DISPID 203;
        property Next: Integer READONLY DISPID 204;
        property Mode: CapControlModes DISPID 205;
        property Capacitor: Widestring DISPID 206;
        property MonitoredObj: Widestring DISPID 207;
        property MonitoredTerm: Integer DISPID 208;
        property CTratio: Double DISPID 209;
        property PTratio: Double DISPID 210;
        property ONSetting: Double DISPID 211;
        property OFFSetting: Double DISPID 212;
        property Vmax: Double DISPID 213;
        property Vmin: Double DISPID 214;
        property UseVoltOverride: Wordbool DISPID 215;
        property Delay: Double DISPID 216;
        property DelayOff: Double DISPID 217;
        property DeadTime: Double DISPID 218;
        property Count: Integer READONLY DISPID 219;
    end;

// *********************************************************************//
// Interface: IRegControls
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3F983AD2-B658-4CE8-B4C1-DE0A9EDD47FD}
// *********************************************************************//
    IRegControls = interface(IDispatch)
        ['{3F983AD2-B658-4CE8-B4C1-DE0A9EDD47FD}']
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_MonitoredBus: Widestring; SAFECALL;
        procedure Set_MonitoredBus(const Value: Widestring); SAFECALL;
        function Get_Transformer: Widestring; SAFECALL;
        procedure Set_Transformer(const Value: Widestring); SAFECALL;
        function Get_TapWinding: Integer; SAFECALL;
        procedure Set_TapWinding(Value: Integer); SAFECALL;
        function Get_Winding: Integer; SAFECALL;
        procedure Set_Winding(Value: Integer); SAFECALL;
        function Get_CTPrimary: Double; SAFECALL;
        procedure Set_CTPrimary(Value: Double); SAFECALL;
        function Get_PTratio: Double; SAFECALL;
        procedure Set_PTratio(Value: Double); SAFECALL;
        function Get_ForwardR: Double; SAFECALL;
        procedure Set_ForwardR(Value: Double); SAFECALL;
        function Get_ForwardX: Double; SAFECALL;
        procedure Set_ForwardX(Value: Double); SAFECALL;
        function Get_ReverseR: Double; SAFECALL;
        procedure Set_ReverseR(Value: Double); SAFECALL;
        function Get_ReverseX: Double; SAFECALL;
        procedure Set_ReverseX(Value: Double); SAFECALL;
        function Get_IsReversible: Wordbool; SAFECALL;
        procedure Set_IsReversible(Value: Wordbool); SAFECALL;
        function Get_IsInverseTime: Wordbool; SAFECALL;
        procedure Set_IsInverseTime(Value: Wordbool); SAFECALL;
        function Get_Delay: Double; SAFECALL;
        procedure Set_Delay(Value: Double); SAFECALL;
        function Get_TapDelay: Double; SAFECALL;
        procedure Set_TapDelay(Value: Double); SAFECALL;
        function Get_MaxTapChange: Integer; SAFECALL;
        procedure Set_MaxTapChange(Value: Integer); SAFECALL;
        function Get_VoltageLimit: Double; SAFECALL;
        procedure Set_VoltageLimit(Value: Double); SAFECALL;
        function Get_ForwardBand: Double; SAFECALL;
        procedure Set_ForwardBand(Value: Double); SAFECALL;
        function Get_ForwardVreg: Double; SAFECALL;
        procedure Set_ForwardVreg(Value: Double); SAFECALL;
        function Get_ReverseBand: Double; SAFECALL;
        procedure Set_ReverseBand(Value: Double); SAFECALL;
        function Get_ReverseVreg: Double; SAFECALL;
        procedure Set_ReverseVreg(Value: Double); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_TapNumber: Integer; SAFECALL;
        procedure Set_TapNumber(Value: Integer); SAFECALL;
        property AllNames: Olevariant READ Get_AllNames;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property MonitoredBus: Widestring READ Get_MonitoredBus WRITE Set_MonitoredBus;
        property Transformer: Widestring READ Get_Transformer WRITE Set_Transformer;
        property TapWinding: Integer READ Get_TapWinding WRITE Set_TapWinding;
        property Winding: Integer READ Get_Winding WRITE Set_Winding;
        property CTPrimary: Double READ Get_CTPrimary WRITE Set_CTPrimary;
        property PTratio: Double READ Get_PTratio WRITE Set_PTratio;
        property ForwardR: Double READ Get_ForwardR WRITE Set_ForwardR;
        property ForwardX: Double READ Get_ForwardX WRITE Set_ForwardX;
        property ReverseR: Double READ Get_ReverseR WRITE Set_ReverseR;
        property ReverseX: Double READ Get_ReverseX WRITE Set_ReverseX;
        property IsReversible: Wordbool READ Get_IsReversible WRITE Set_IsReversible;
        property IsInverseTime: Wordbool READ Get_IsInverseTime WRITE Set_IsInverseTime;
        property Delay: Double READ Get_Delay WRITE Set_Delay;
        property TapDelay: Double READ Get_TapDelay WRITE Set_TapDelay;
        property MaxTapChange: Integer READ Get_MaxTapChange WRITE Set_MaxTapChange;
        property VoltageLimit: Double READ Get_VoltageLimit WRITE Set_VoltageLimit;
        property ForwardBand: Double READ Get_ForwardBand WRITE Set_ForwardBand;
        property ForwardVreg: Double READ Get_ForwardVreg WRITE Set_ForwardVreg;
        property ReverseBand: Double READ Get_ReverseBand WRITE Set_ReverseBand;
        property ReverseVreg: Double READ Get_ReverseVreg WRITE Set_ReverseVreg;
        property Count: Integer READ Get_Count;
        property TapNumber: Integer READ Get_TapNumber WRITE Set_TapNumber;
    end;

// *********************************************************************//
// DispIntf:  IRegControlsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3F983AD2-B658-4CE8-B4C1-DE0A9EDD47FD}
// *********************************************************************//
    IRegControlsDisp = dispinterface
        ['{3F983AD2-B658-4CE8-B4C1-DE0A9EDD47FD}']
        property AllNames: Olevariant READONLY DISPID 201;
        property Name: Widestring DISPID 202;
        property First: Integer READONLY DISPID 203;
        property Next: Integer READONLY DISPID 204;
        property MonitoredBus: Widestring DISPID 205;
        property Transformer: Widestring DISPID 206;
        property TapWinding: Integer DISPID 207;
        property Winding: Integer DISPID 208;
        property CTPrimary: Double DISPID 209;
        property PTratio: Double DISPID 210;
        property ForwardR: Double DISPID 211;
        property ForwardX: Double DISPID 212;
        property ReverseR: Double DISPID 213;
        property ReverseX: Double DISPID 214;
        property IsReversible: Wordbool DISPID 215;
        property IsInverseTime: Wordbool DISPID 216;
        property Delay: Double DISPID 217;
        property TapDelay: Double DISPID 218;
        property MaxTapChange: Integer DISPID 219;
        property VoltageLimit: Double DISPID 220;
        property ForwardBand: Double DISPID 221;
        property ForwardVreg: Double DISPID 222;
        property ReverseBand: Double DISPID 223;
        property ReverseVreg: Double DISPID 224;
        property Count: Integer READONLY DISPID 225;
        property TapNumber: Integer DISPID 226;
    end;

// *********************************************************************//
// Interface: ITopology
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {03FADB98-4F30-416E-ACD2-9BD987A0CBC3}
// *********************************************************************//
    ITopology = interface(IDispatch)
        ['{03FADB98-4F30-416E-ACD2-9BD987A0CBC3}']
        function Get_NumLoops: Integer; SAFECALL;
        function Get_NumIsolatedBranches: Integer; SAFECALL;
        function Get_AllLoopedPairs: Olevariant; SAFECALL;
        function Get_AllIsolatedBranches: Olevariant; SAFECALL;
        function Get_NumIsolatedLoads: Integer; SAFECALL;
        function Get_AllIsolatedLoads: Olevariant; SAFECALL;
        function Get_BranchName: Widestring; SAFECALL;
        procedure Set_BranchName(const Value: Widestring); SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_ActiveBranch: Integer; SAFECALL;
        function Get_ForwardBranch: Integer; SAFECALL;
        function Get_BackwardBranch: Integer; SAFECALL;
        function Get_LoopedBranch: Integer; SAFECALL;
        function Get_ParallelBranch: Integer; SAFECALL;
        function Get_FirstLoad: Integer; SAFECALL;
        function Get_NextLoad: Integer; SAFECALL;
        function Get_ActiveLevel: Integer; SAFECALL;
        function Get_BusName: Widestring; SAFECALL;
        procedure Set_BusName(const Value: Widestring); SAFECALL;
        property NumLoops: Integer READ Get_NumLoops;
        property NumIsolatedBranches: Integer READ Get_NumIsolatedBranches;
        property AllLoopedPairs: Olevariant READ Get_AllLoopedPairs;
        property AllIsolatedBranches: Olevariant READ Get_AllIsolatedBranches;
        property NumIsolatedLoads: Integer READ Get_NumIsolatedLoads;
        property AllIsolatedLoads: Olevariant READ Get_AllIsolatedLoads;
        property BranchName: Widestring READ Get_BranchName WRITE Set_BranchName;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property ActiveBranch: Integer READ Get_ActiveBranch;
        property ForwardBranch: Integer READ Get_ForwardBranch;
        property BackwardBranch: Integer READ Get_BackwardBranch;
        property LoopedBranch: Integer READ Get_LoopedBranch;
        property ParallelBranch: Integer READ Get_ParallelBranch;
        property FirstLoad: Integer READ Get_FirstLoad;
        property NextLoad: Integer READ Get_NextLoad;
        property ActiveLevel: Integer READ Get_ActiveLevel;
        property BusName: Widestring READ Get_BusName WRITE Set_BusName;
    end;

// *********************************************************************//
// DispIntf:  ITopologyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {03FADB98-4F30-416E-ACD2-9BD987A0CBC3}
// *********************************************************************//
    ITopologyDisp = dispinterface
        ['{03FADB98-4F30-416E-ACD2-9BD987A0CBC3}']
        property NumLoops: Integer READONLY DISPID 201;
        property NumIsolatedBranches: Integer READONLY DISPID 202;
        property AllLoopedPairs: Olevariant READONLY DISPID 203;
        property AllIsolatedBranches: Olevariant READONLY DISPID 204;
        property NumIsolatedLoads: Integer READONLY DISPID 205;
        property AllIsolatedLoads: Olevariant READONLY DISPID 206;
        property BranchName: Widestring DISPID 207;
        property First: Integer READONLY DISPID 208;
        property Next: Integer READONLY DISPID 209;
        property ActiveBranch: Integer READONLY DISPID 210;
        property ForwardBranch: Integer READONLY DISPID 211;
        property BackwardBranch: Integer READONLY DISPID 212;
        property LoopedBranch: Integer READONLY DISPID 213;
        property ParallelBranch: Integer READONLY DISPID 214;
        property FirstLoad: Integer READONLY DISPID 215;
        property NextLoad: Integer READONLY DISPID 217;
        property ActiveLevel: Integer READONLY DISPID 216;
        property BusName: Widestring DISPID 218;
    end;

// *********************************************************************//
// Interface: IDSS_Executive
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DD7B80E9-5EFB-4E79-96CA-9C88F5A8A11C}
// *********************************************************************//
    IDSS_Executive = interface(IDispatch)
        ['{DD7B80E9-5EFB-4E79-96CA-9C88F5A8A11C}']
        function Get_NumCommands: Integer; SAFECALL;
        function Get_NumOptions: Integer; SAFECALL;
        function Get_Command(i: Integer): Widestring; SAFECALL;
        function Get_Option(i: Integer): Widestring; SAFECALL;
        function Get_CommandHelp(i: Integer): Widestring; SAFECALL;
        function Get_OptionHelp(i: Integer): Widestring; SAFECALL;
        function Get_OptionValue(i: Integer): Widestring; SAFECALL;
        property NumCommands: Integer READ Get_NumCommands;
        property NumOptions: Integer READ Get_NumOptions;
        property Command[i: Integer]: Widestring READ Get_Command;
        property Option[i: Integer]: Widestring READ Get_Option;
        property CommandHelp[i: Integer]: Widestring READ Get_CommandHelp;
        property OptionHelp[i: Integer]: Widestring READ Get_OptionHelp;
        property OptionValue[i: Integer]: Widestring READ Get_OptionValue;
    end;

// *********************************************************************//
// DispIntf:  IDSS_ExecutiveDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DD7B80E9-5EFB-4E79-96CA-9C88F5A8A11C}
// *********************************************************************//
    IDSS_ExecutiveDisp = dispinterface
        ['{DD7B80E9-5EFB-4E79-96CA-9C88F5A8A11C}']
        property NumCommands: Integer READONLY DISPID 201;
        property NumOptions: Integer READONLY DISPID 202;
        property Command[i: Integer]: Widestring READONLY DISPID 203;
        property Option[i: Integer]: Widestring READONLY DISPID 204;
        property CommandHelp[i: Integer]: Widestring READONLY DISPID 205;
        property OptionHelp[i: Integer]: Widestring READONLY DISPID 206;
        property OptionValue[i: Integer]: Widestring READONLY DISPID 207;
    end;

// *********************************************************************//
// Interface: IDSSEvents
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3F5A5530-4E67-44BF-AE6D-561584C6BF47}
// *********************************************************************//
    IDSSEvents = interface(IDispatch)
        ['{3F5A5530-4E67-44BF-AE6D-561584C6BF47}']
    end;

// *********************************************************************//
// DispIntf:  IDSSEventsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3F5A5530-4E67-44BF-AE6D-561584C6BF47}
// *********************************************************************//
    IDSSEventsDisp = dispinterface
        ['{3F5A5530-4E67-44BF-AE6D-561584C6BF47}']
    end;

// *********************************************************************//
// DispIntf:  IDSSEventsEvents
// Flags:     (0)
// GUID:      {AE501F77-F7F0-4201-A9AD-6AB385262203}
// *********************************************************************//
    IDSSEventsEvents = dispinterface
        ['{AE501F77-F7F0-4201-A9AD-6AB385262203}']
        function InitControls: HResult; DISPID 201;
        function StepControls: HResult; DISPID 202;
        function CheckControls: HResult; DISPID 203;
    end;

// *********************************************************************//
// Interface: ISensors
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E7444ECD-B491-4D8E-A1E3-E5804BD571E2}
// *********************************************************************//
    ISensors = interface(IDispatch)
        ['{E7444ECD-B491-4D8E-A1E3-E5804BD571E2}']
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_IsDelta: Wordbool; SAFECALL;
        procedure Set_IsDelta(Value: Wordbool); SAFECALL;
        function Get_ReverseDelta: Wordbool; SAFECALL;
        procedure Set_ReverseDelta(Value: Wordbool); SAFECALL;
        function Get_PctError: Double; SAFECALL;
        procedure Set_PctError(Value: Double); SAFECALL;
        function Get_Weight: Double; SAFECALL;
        procedure Set_Weight(Value: Double); SAFECALL;
        function Get_MeteredElement: Widestring; SAFECALL;
        procedure Set_MeteredElement(const Value: Widestring); SAFECALL;
        function Get_MeteredTerminal: Integer; SAFECALL;
        procedure Set_MeteredTerminal(Value: Integer); SAFECALL;
        procedure Reset; SAFECALL;
        procedure ResetAll; SAFECALL;
        function Get_kVbase: Double; SAFECALL;
        procedure Set_kVbase(Value: Double); SAFECALL;
        function Get_Currents: Olevariant; SAFECALL;
        procedure Set_Currents(Value: Olevariant); SAFECALL;
        function Get_kVS: Olevariant; SAFECALL;
        procedure Set_kVS(Value: Olevariant); SAFECALL;
        function Get_kVARS: Olevariant; SAFECALL;
        procedure Set_kVARS(Value: Olevariant); SAFECALL;
        function Get_kWS: Olevariant; SAFECALL;
        procedure Set_kWS(Value: Olevariant); SAFECALL;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property Count: Integer READ Get_Count;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property AllNames: Olevariant READ Get_AllNames;
        property IsDelta: Wordbool READ Get_IsDelta WRITE Set_IsDelta;
        property ReverseDelta: Wordbool READ Get_ReverseDelta WRITE Set_ReverseDelta;
        property PctError: Double READ Get_PctError WRITE Set_PctError;
        property Weight: Double READ Get_Weight WRITE Set_Weight;
        property MeteredElement: Widestring READ Get_MeteredElement WRITE Set_MeteredElement;
        property MeteredTerminal: Integer READ Get_MeteredTerminal WRITE Set_MeteredTerminal;
        property kVbase: Double READ Get_kVbase WRITE Set_kVbase;
        property Currents: Olevariant READ Get_Currents WRITE Set_Currents;
        property kVS: Olevariant READ Get_kVS WRITE Set_kVS;
        property kVARS: Olevariant READ Get_kVARS WRITE Set_kVARS;
        property kWS: Olevariant READ Get_kWS WRITE Set_kWS;
    end;

// *********************************************************************//
// DispIntf:  ISensorsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E7444ECD-B491-4D8E-A1E3-E5804BD571E2}
// *********************************************************************//
    ISensorsDisp = dispinterface
        ['{E7444ECD-B491-4D8E-A1E3-E5804BD571E2}']
        property Name: Widestring DISPID 201;
        property Count: Integer READONLY DISPID 202;
        property First: Integer READONLY DISPID 203;
        property Next: Integer READONLY DISPID 204;
        property AllNames: Olevariant READONLY DISPID 205;
        property IsDelta: Wordbool DISPID 206;
        property ReverseDelta: Wordbool DISPID 207;
        property PctError: Double DISPID 208;
        property Weight: Double DISPID 209;
        property MeteredElement: Widestring DISPID 210;
        property MeteredTerminal: Integer DISPID 211;
        procedure Reset; DISPID 212;
        procedure ResetAll; DISPID 213;
        property kVbase: Double DISPID 214;
        property Currents: Olevariant DISPID 215;
        property kVS: Olevariant DISPID 216;
        property kVARS: Olevariant DISPID 217;
        property kWS: Olevariant DISPID 218;
    end;

// *********************************************************************//
// Interface: IXYCurves
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {97AA7680-E994-4A0C-BAC3-9B67BA49825C}
// *********************************************************************//
    IXYCurves = interface(IDispatch)
        ['{97AA7680-E994-4A0C-BAC3-9B67BA49825C}']
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Npts: Integer; SAFECALL;
        procedure Set_Npts(Value: Integer); SAFECALL;
        function Get_Xarray: Olevariant; SAFECALL;
        procedure Set_Xarray(Value: Olevariant); SAFECALL;
        function Get_Yarray: Olevariant; SAFECALL;
        procedure Set_Yarray(Value: Olevariant); STDCALL;
        function Get_x: Double; SAFECALL;
        procedure Set_x(Value: Double); SAFECALL;
        function Get_y: Double; SAFECALL;
        procedure Set_y(Value: Double); SAFECALL;
        function Get_Xshift: Double; SAFECALL;
        procedure Set_Xshift(Value: Double); SAFECALL;
        function Get_Yshift: Double; SAFECALL;
        procedure Set_Yshift(Value: Double); SAFECALL;
        function Get_Xscale: Double; SAFECALL;
        procedure Set_Xscale(Value: Double); SAFECALL;
        function Get_Yscale: Double; SAFECALL;
        procedure Set_Yscale(Value: Double); SAFECALL;
        property Count: Integer READ Get_Count;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property Npts: Integer READ Get_Npts WRITE Set_Npts;
        property Xarray: Olevariant READ Get_Xarray WRITE Set_Xarray;
    // Skipped Property "Yarray"
        property x: Double READ Get_x WRITE Set_x;
        property y: Double READ Get_y WRITE Set_y;
        property Xshift: Double READ Get_Xshift WRITE Set_Xshift;
        property Yshift: Double READ Get_Yshift WRITE Set_Yshift;
        property Xscale: Double READ Get_Xscale WRITE Set_Xscale;
        property Yscale: Double READ Get_Yscale WRITE Set_Yscale;
    end;

// *********************************************************************//
// DispIntf:  IXYCurvesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {97AA7680-E994-4A0C-BAC3-9B67BA49825C}
// *********************************************************************//
    IXYCurvesDisp = dispinterface
        ['{97AA7680-E994-4A0C-BAC3-9B67BA49825C}']
        property Count: Integer READONLY DISPID 201;
        property First: Integer READONLY DISPID 202;
        property Next: Integer READONLY DISPID 203;
        property Name: Widestring DISPID 204;
        property Npts: Integer DISPID 205;
        property Xarray: Olevariant DISPID 206;
        function Yarray: Olevariant; DISPID 207;
        property x: Double DISPID 208;
        property y: Double DISPID 209;
        property Xshift: Double DISPID 210;
        property Yshift: Double DISPID 211;
        property Xscale: Double DISPID 212;
        property Yscale: Double DISPID 213;
    end;

// *********************************************************************//
// Interface: IPDElements
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {05D4E15E-1588-4ABB-8339-3527420C668B}
// *********************************************************************//
    IPDElements = interface(IDispatch)
        ['{05D4E15E-1588-4ABB-8339-3527420C668B}']
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_IsShunt: Wordbool; SAFECALL;
        function Get_FaultRate: Double; SAFECALL;
        procedure Set_FaultRate(Value: Double); SAFECALL;
        function Get_pctPermanent: Double; SAFECALL;
        procedure Set_pctPermanent(Value: Double); SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Lambda: Double; SAFECALL;
        function Get_AccumulatedL: Double; SAFECALL;
        function Get_Numcustomers: Integer; SAFECALL;
        function Get_Totalcustomers: Integer; SAFECALL;
        function Get_ParentPDElement: Integer; SAFECALL;
        function Get_FromTerminal: Integer; SAFECALL;
        function Get_TotalMiles: Double; SAFECALL;
        function Get_SectionID: Integer; SAFECALL;
        function Get_RepairTime: Double; SAFECALL;
        procedure Set_RepairTime(Value: Double); SAFECALL;
        property Count: Integer READ Get_Count;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property IsShunt: Wordbool READ Get_IsShunt;
        property FaultRate: Double READ Get_FaultRate WRITE Set_FaultRate;
        property pctPermanent: Double READ Get_pctPermanent WRITE Set_pctPermanent;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property Lambda: Double READ Get_Lambda;
        property AccumulatedL: Double READ Get_AccumulatedL;
        property Numcustomers: Integer READ Get_Numcustomers;
        property Totalcustomers: Integer READ Get_Totalcustomers;
        property ParentPDElement: Integer READ Get_ParentPDElement;
        property FromTerminal: Integer READ Get_FromTerminal;
        property TotalMiles: Double READ Get_TotalMiles;
        property SectionID: Integer READ Get_SectionID;
        property RepairTime: Double READ Get_RepairTime WRITE Set_RepairTime;
    end;

// *********************************************************************//
// DispIntf:  IPDElementsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {05D4E15E-1588-4ABB-8339-3527420C668B}
// *********************************************************************//
    IPDElementsDisp = dispinterface
        ['{05D4E15E-1588-4ABB-8339-3527420C668B}']
        property Count: Integer READONLY DISPID 201;
        property First: Integer READONLY DISPID 202;
        property Next: Integer READONLY DISPID 203;
        property IsShunt: Wordbool READONLY DISPID 204;
        property FaultRate: Double DISPID 205;
        property pctPermanent: Double DISPID 206;
        property Name: Widestring DISPID 207;
        property Lambda: Double READONLY DISPID 208;
        property AccumulatedL: Double READONLY DISPID 209;
        property Numcustomers: Integer READONLY DISPID 211;
        property Totalcustomers: Integer READONLY DISPID 212;
        property ParentPDElement: Integer READONLY DISPID 213;
        property FromTerminal: Integer READONLY DISPID 214;
        property TotalMiles: Double READONLY DISPID 215;
        property SectionID: Integer READONLY DISPID 216;
        property RepairTime: Double DISPID 210;
    end;

// *********************************************************************//
// Interface: IReclosers
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {21001789-9F46-4323-93B0-8B31395FD6E4}
// *********************************************************************//
    IReclosers = interface(IDispatch)
        ['{21001789-9F46-4323-93B0-8B31395FD6E4}']
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_MonitoredObj: Widestring; SAFECALL;
        procedure Set_MonitoredObj(const Value: Widestring); SAFECALL;
        function Get_MonitoredTerm: Integer; SAFECALL;
        procedure Set_MonitoredTerm(Value: Integer); SAFECALL;
        function Get_SwitchedObj: Widestring; SAFECALL;
        procedure Set_SwitchedObj(const Value: Widestring); SAFECALL;
        function Get_SwitchedTerm: Integer; SAFECALL;
        procedure Set_SwitchedTerm(Value: Integer); SAFECALL;
        function Get_NumFast: Integer; SAFECALL;
        procedure Set_NumFast(Value: Integer); SAFECALL;
        function Get_Shots: Integer; SAFECALL;
        procedure Set_Shots(Value: Integer); SAFECALL;
        function Get_RecloseIntervals: Olevariant; SAFECALL;
        function Get_PhaseTrip: Double; SAFECALL;
        procedure Set_PhaseTrip(Value: Double); SAFECALL;
        function Get_PhaseInst: Double; SAFECALL;
        procedure Set_PhaseInst(Value: Double); SAFECALL;
        function Get_GroundTrip: Double; SAFECALL;
        procedure Set_GroundTrip(Value: Double); SAFECALL;
        function Get_GroundInst: Double; SAFECALL;
        procedure Set_GroundInst(Value: Double); SAFECALL;
        procedure Open; SAFECALL;
        procedure Close; SAFECALL;
        function Get_idx: Integer; SAFECALL;
        procedure Set_idx(Value: Integer); SAFECALL;
        property AllNames: Olevariant READ Get_AllNames;
        property Count: Integer READ Get_Count;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property MonitoredObj: Widestring READ Get_MonitoredObj WRITE Set_MonitoredObj;
        property MonitoredTerm: Integer READ Get_MonitoredTerm WRITE Set_MonitoredTerm;
        property SwitchedObj: Widestring READ Get_SwitchedObj WRITE Set_SwitchedObj;
        property SwitchedTerm: Integer READ Get_SwitchedTerm WRITE Set_SwitchedTerm;
        property NumFast: Integer READ Get_NumFast WRITE Set_NumFast;
        property Shots: Integer READ Get_Shots WRITE Set_Shots;
        property RecloseIntervals: Olevariant READ Get_RecloseIntervals;
        property PhaseTrip: Double READ Get_PhaseTrip WRITE Set_PhaseTrip;
        property PhaseInst: Double READ Get_PhaseInst WRITE Set_PhaseInst;
        property GroundTrip: Double READ Get_GroundTrip WRITE Set_GroundTrip;
        property GroundInst: Double READ Get_GroundInst WRITE Set_GroundInst;
        property idx: Integer READ Get_idx WRITE Set_idx;
    end;

// *********************************************************************//
// DispIntf:  IReclosersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {21001789-9F46-4323-93B0-8B31395FD6E4}
// *********************************************************************//
    IReclosersDisp = dispinterface
        ['{21001789-9F46-4323-93B0-8B31395FD6E4}']
        property AllNames: Olevariant READONLY DISPID 201;
        property Count: Integer READONLY DISPID 202;
        property First: Integer READONLY DISPID 203;
        property Next: Integer READONLY DISPID 204;
        property Name: Widestring DISPID 205;
        property MonitoredObj: Widestring DISPID 206;
        property MonitoredTerm: Integer DISPID 207;
        property SwitchedObj: Widestring DISPID 208;
        property SwitchedTerm: Integer DISPID 209;
        property NumFast: Integer DISPID 210;
        property Shots: Integer DISPID 211;
        property RecloseIntervals: Olevariant READONLY DISPID 212;
        property PhaseTrip: Double DISPID 213;
        property PhaseInst: Double DISPID 214;
        property GroundTrip: Double DISPID 215;
        property GroundInst: Double DISPID 216;
        procedure Open; DISPID 217;
        procedure Close; DISPID 218;
        property idx: Integer DISPID 219;
    end;

// *********************************************************************//
// Interface: IRelays
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {76956697-6055-4E8E-B4D6-650805D3F90D}
// *********************************************************************//
    IRelays = interface(IDispatch)
        ['{76956697-6055-4E8E-B4D6-650805D3F90D}']
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_MonitoredObj: Widestring; SAFECALL;
        procedure Set_MonitoredObj(const Value: Widestring); SAFECALL;
        function Get_MonitoredTerm: Integer; SAFECALL;
        procedure Set_MonitoredTerm(Value: Integer); SAFECALL;
        function Get_SwitchedObj: Widestring; SAFECALL;
        procedure Set_SwitchedObj(const Value: Widestring); SAFECALL;
        function Get_SwitchedTerm: Integer; SAFECALL;
        procedure Set_SwitchedTerm(Value: Integer); SAFECALL;
        function Get_idx: Integer; SAFECALL;
        procedure Set_idx(Value: Integer); SAFECALL;
        property AllNames: Olevariant READ Get_AllNames;
        property Count: Integer READ Get_Count;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property MonitoredObj: Widestring READ Get_MonitoredObj WRITE Set_MonitoredObj;
        property MonitoredTerm: Integer READ Get_MonitoredTerm WRITE Set_MonitoredTerm;
        property SwitchedObj: Widestring READ Get_SwitchedObj WRITE Set_SwitchedObj;
        property SwitchedTerm: Integer READ Get_SwitchedTerm WRITE Set_SwitchedTerm;
        property idx: Integer READ Get_idx WRITE Set_idx;
    end;

// *********************************************************************//
// DispIntf:  IRelaysDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {76956697-6055-4E8E-B4D6-650805D3F90D}
// *********************************************************************//
    IRelaysDisp = dispinterface
        ['{76956697-6055-4E8E-B4D6-650805D3F90D}']
        property AllNames: Olevariant READONLY DISPID 201;
        property Count: Integer READONLY DISPID 202;
        property First: Integer READONLY DISPID 203;
        property Next: Integer READONLY DISPID 204;
        property Name: Widestring DISPID 205;
        property MonitoredObj: Widestring DISPID 206;
        property MonitoredTerm: Integer DISPID 207;
        property SwitchedObj: Widestring DISPID 208;
        property SwitchedTerm: Integer DISPID 209;
        property idx: Integer DISPID 210;
    end;

// *********************************************************************//
// Interface: ICmathLib
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2B649EC0-FA89-45ED-A937-E7CB47806A3A}
// *********************************************************************//
    ICmathLib = interface(IDispatch)
        ['{2B649EC0-FA89-45ED-A937-E7CB47806A3A}']
        function Get_cmplx(RealPart: Double; ImagPart: Double): Olevariant; SAFECALL;
        function Get_cabs(realpart: Double; imagpart: Double): Double; SAFECALL;
        function Get_cdang(RealPart: Double; ImagPart: Double): Double; SAFECALL;
        function Get_ctopolardeg(RealPart: Double; ImagPart: Double): Olevariant; SAFECALL;
        function Get_pdegtocomplex(magnitude: Double; angle: Double): Olevariant; SAFECALL;
        function Get_cmul(a1: Double; b1: Double; a2: Double; b2: Double): Olevariant; SAFECALL;
        function Get_cdiv(a1: Double; b1: Double; a2: Double; b2: Double): Olevariant; SAFECALL;
        property cmplx[RealPart: Double; ImagPart: Double]: Olevariant READ Get_cmplx;
        property cabs[realpart: Double; imagpart: Double]: Double READ Get_cabs;
        property cdang[RealPart: Double; ImagPart: Double]: Double READ Get_cdang;
        property ctopolardeg[RealPart: Double; ImagPart: Double]: Olevariant READ Get_ctopolardeg;
        property pdegtocomplex[magnitude: Double; angle: Double]: Olevariant READ Get_pdegtocomplex;
        property cmul[a1: Double; b1: Double; a2: Double; b2: Double]: Olevariant READ Get_cmul;
        property cdiv[a1: Double; b1: Double; a2: Double; b2: Double]: Olevariant READ Get_cdiv;
    end;

// *********************************************************************//
// DispIntf:  ICmathLibDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2B649EC0-FA89-45ED-A937-E7CB47806A3A}
// *********************************************************************//
    ICmathLibDisp = dispinterface
        ['{2B649EC0-FA89-45ED-A937-E7CB47806A3A}']
        property cmplx[RealPart: Double; ImagPart: Double]: Olevariant READONLY DISPID 201;
        property cabs[realpart: Double; imagpart: Double]: Double READONLY DISPID 202;
        property cdang[RealPart: Double; ImagPart: Double]: Double READONLY DISPID 203;
        property ctopolardeg[RealPart: Double; ImagPart: Double]: Olevariant READONLY DISPID 204;
        property pdegtocomplex[magnitude: Double; angle: Double]: Olevariant READONLY DISPID 205;
        property cmul[a1: Double; b1: Double; a2: Double; b2: Double]: Olevariant READONLY DISPID 206;
        property cdiv[a1: Double; b1: Double; a2: Double; b2: Double]: Olevariant READONLY DISPID 207;
    end;

// *********************************************************************//
// Interface: IParser
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9714FED4-9D39-4692-B76B-9A18F206A934}
// *********************************************************************//
    IParser = interface(IDispatch)
        ['{9714FED4-9D39-4692-B76B-9A18F206A934}']
        function Get_CmdString: Widestring; SAFECALL;
        procedure Set_CmdString(const Value: Widestring); SAFECALL;
        function Get_NextParam: Widestring; SAFECALL;
        function Get_AutoIncrement: Wordbool; SAFECALL;
        procedure Set_AutoIncrement(Value: Wordbool); SAFECALL;
        function Get_DblValue: Double; SAFECALL;
        function Get_IntValue: Integer; SAFECALL;
        function Get_StrValue: Widestring; SAFECALL;
        function Get_WhiteSpace: Widestring; SAFECALL;
        procedure Set_WhiteSpace(const Value: Widestring); SAFECALL;
        function Get_BeginQuote: Widestring; SAFECALL;
        procedure Set_BeginQuote(const Value: Widestring); SAFECALL;
        function Get_EndQuote: Widestring; SAFECALL;
        procedure Set_EndQuote(const Value: Widestring); SAFECALL;
        function Get_Delimiters: Widestring; SAFECALL;
        procedure Set_Delimiters(const Value: Widestring); SAFECALL;
        procedure ResetDelimiters; SAFECALL;
        function Get_Vector(ExpectedSize: Integer): Olevariant; SAFECALL;
        function Get_Matrix(ExpectedOrder: Integer): Olevariant; SAFECALL;
        function Get_SymMatrix(ExpectedOrder: Integer): Olevariant; SAFECALL;
        property CmdString: Widestring READ Get_CmdString WRITE Set_CmdString;
        property NextParam: Widestring READ Get_NextParam;
        property AutoIncrement: Wordbool READ Get_AutoIncrement WRITE Set_AutoIncrement;
        property DblValue: Double READ Get_DblValue;
        property IntValue: Integer READ Get_IntValue;
        property StrValue: Widestring READ Get_StrValue;
        property WhiteSpace: Widestring READ Get_WhiteSpace WRITE Set_WhiteSpace;
        property BeginQuote: Widestring READ Get_BeginQuote WRITE Set_BeginQuote;
        property EndQuote: Widestring READ Get_EndQuote WRITE Set_EndQuote;
        property Delimiters: Widestring READ Get_Delimiters WRITE Set_Delimiters;
        property Vector[ExpectedSize: Integer]: Olevariant READ Get_Vector;
        property Matrix[ExpectedOrder: Integer]: Olevariant READ Get_Matrix;
        property SymMatrix[ExpectedOrder: Integer]: Olevariant READ Get_SymMatrix;
    end;

// *********************************************************************//
// DispIntf:  IParserDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9714FED4-9D39-4692-B76B-9A18F206A934}
// *********************************************************************//
    IParserDisp = dispinterface
        ['{9714FED4-9D39-4692-B76B-9A18F206A934}']
        property CmdString: Widestring DISPID 201;
        property NextParam: Widestring READONLY DISPID 202;
        property AutoIncrement: Wordbool DISPID 203;
        property DblValue: Double READONLY DISPID 204;
        property IntValue: Integer READONLY DISPID 205;
        property StrValue: Widestring READONLY DISPID 206;
        property WhiteSpace: Widestring DISPID 207;
        property BeginQuote: Widestring DISPID 208;
        property EndQuote: Widestring DISPID 209;
        property Delimiters: Widestring DISPID 210;
        procedure ResetDelimiters; DISPID 211;
        property Vector[ExpectedSize: Integer]: Olevariant READONLY DISPID 212;
        property Matrix[ExpectedOrder: Integer]: Olevariant READONLY DISPID 213;
        property SymMatrix[ExpectedOrder: Integer]: Olevariant READONLY DISPID 214;
    end;

// *********************************************************************//
// Interface: ILoadShapes
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {196861FB-38C6-4FB4-B8A5-B2DDA3DDA663}
// *********************************************************************//
    ILoadShapes = interface(IDispatch)
        ['{196861FB-38C6-4FB4-B8A5-B2DDA3DDA663}']
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Npts: Integer; SAFECALL;
        procedure Set_Npts(Value: Integer); SAFECALL;
        function Get_Pmult: Olevariant; SAFECALL;
        procedure Set_Pmult(Value: Olevariant); SAFECALL;
        function Get_Qmult: Olevariant; SAFECALL;
        procedure Set_Qmult(Value: Olevariant); SAFECALL;
        procedure Normalize; SAFECALL;
        function Get_TimeArray: Olevariant; SAFECALL;
        procedure Set_TimeArray(Value: Olevariant); SAFECALL;
        function Get_HrInterval: Double; SAFECALL;
        procedure Set_HrInterval(Value: Double); SAFECALL;
        function Get_MinInterval: Double; SAFECALL;
        procedure Set_MinInterval(Value: Double); SAFECALL;
        function New(const Name: Widestring): Integer; STDCALL;
        function Get_Pbase: Double; SAFECALL;
        procedure Set_Pbase(Value: Double); SAFECALL;
        function Get_Qbase: Double; SAFECALL;
        procedure Set_Qbase(Value: Double); SAFECALL;
        function Get_UseActual: Wordbool; SAFECALL;
        procedure Set_UseActual(Value: Wordbool); SAFECALL;
        function Get_Sinterval: Double; SAFECALL;
        procedure Set_Sinterval(Value: Double); SAFECALL;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property Count: Integer READ Get_Count;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property AllNames: Olevariant READ Get_AllNames;
        property Npts: Integer READ Get_Npts WRITE Set_Npts;
        property Pmult: Olevariant READ Get_Pmult WRITE Set_Pmult;
        property Qmult: Olevariant READ Get_Qmult WRITE Set_Qmult;
        property TimeArray: Olevariant READ Get_TimeArray WRITE Set_TimeArray;
        property HrInterval: Double READ Get_HrInterval WRITE Set_HrInterval;
        property MinInterval: Double READ Get_MinInterval WRITE Set_MinInterval;
        property Pbase: Double READ Get_Pbase WRITE Set_Pbase;
        property Qbase: Double READ Get_Qbase WRITE Set_Qbase;
        property UseActual: Wordbool READ Get_UseActual WRITE Set_UseActual;
        property Sinterval: Double READ Get_Sinterval WRITE Set_Sinterval;
    end;

// *********************************************************************//
// DispIntf:  ILoadShapesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {196861FB-38C6-4FB4-B8A5-B2DDA3DDA663}
// *********************************************************************//
    ILoadShapesDisp = dispinterface
        ['{196861FB-38C6-4FB4-B8A5-B2DDA3DDA663}']
        property Name: Widestring DISPID 201;
        property Count: Integer READONLY DISPID 202;
        property First: Integer READONLY DISPID 203;
        property Next: Integer READONLY DISPID 204;
        property AllNames: Olevariant READONLY DISPID 205;
        property Npts: Integer DISPID 206;
        property Pmult: Olevariant DISPID 207;
        property Qmult: Olevariant DISPID 208;
        procedure Normalize; DISPID 209;
        property TimeArray: Olevariant DISPID 210;
        property HrInterval: Double DISPID 211;
        property MinInterval: Double DISPID 212;
        function New(const Name: Widestring): Integer; DISPID 214;
        property Pbase: Double DISPID 215;
        property Qbase: Double DISPID 216;
        property UseActual: Wordbool DISPID 217;
        property Sinterval: Double DISPID 213;
    end;

// *********************************************************************//
// Interface: IFuses
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AABE4DA8-3D5A-447F-AFFB-78946BA68DA5}
// *********************************************************************//
    IFuses = interface(IDispatch)
        ['{AABE4DA8-3D5A-447F-AFFB-78946BA68DA5}']
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_MonitoredObj: Widestring; SAFECALL;
        procedure Set_MonitoredObj(const Value: Widestring); SAFECALL;
        function Get_MonitoredTerm: Integer; SAFECALL;
        procedure Set_MonitoredTerm(Value: Integer); SAFECALL;
        function Get_SwitchedObj: Widestring; SAFECALL;
        procedure Set_SwitchedObj(const Value: Widestring); SAFECALL;
        function Get_SwitchedTerm: Integer; SAFECALL;
        procedure Set_SwitchedTerm(Value: Integer); SAFECALL;
        function Get_TCCcurve: Widestring; SAFECALL;
        procedure Set_TCCcurve(const Value: Widestring); SAFECALL;
        function Get_RatedCurrent: Double; SAFECALL;
        procedure Set_RatedCurrent(Value: Double); SAFECALL;
        function Get_Delay: Double; SAFECALL;
        procedure Set_Delay(Value: Double); SAFECALL;
        procedure Open; SAFECALL;
        procedure Close; SAFECALL;
        function IsBlown: Wordbool; STDCALL;
        function Get_idx: Integer; SAFECALL;
        procedure Set_idx(Value: Integer); SAFECALL;
        function Get_NumPhases: Integer; SAFECALL;
        property AllNames: Olevariant READ Get_AllNames;
        property Count: Integer READ Get_Count;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property MonitoredObj: Widestring READ Get_MonitoredObj WRITE Set_MonitoredObj;
        property MonitoredTerm: Integer READ Get_MonitoredTerm WRITE Set_MonitoredTerm;
        property SwitchedObj: Widestring READ Get_SwitchedObj WRITE Set_SwitchedObj;
        property SwitchedTerm: Integer READ Get_SwitchedTerm WRITE Set_SwitchedTerm;
        property TCCcurve: Widestring READ Get_TCCcurve WRITE Set_TCCcurve;
        property RatedCurrent: Double READ Get_RatedCurrent WRITE Set_RatedCurrent;
        property Delay: Double READ Get_Delay WRITE Set_Delay;
        property idx: Integer READ Get_idx WRITE Set_idx;
        property NumPhases: Integer READ Get_NumPhases;
    end;

// *********************************************************************//
// DispIntf:  IFusesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AABE4DA8-3D5A-447F-AFFB-78946BA68DA5}
// *********************************************************************//
    IFusesDisp = dispinterface
        ['{AABE4DA8-3D5A-447F-AFFB-78946BA68DA5}']
        property AllNames: Olevariant READONLY DISPID 201;
        property Count: Integer READONLY DISPID 202;
        property First: Integer READONLY DISPID 203;
        property Next: Integer READONLY DISPID 204;
        property Name: Widestring DISPID 205;
        property MonitoredObj: Widestring DISPID 206;
        property MonitoredTerm: Integer DISPID 207;
        property SwitchedObj: Widestring DISPID 208;
        property SwitchedTerm: Integer DISPID 209;
        property TCCcurve: Widestring DISPID 210;
        property RatedCurrent: Double DISPID 211;
        property Delay: Double DISPID 212;
        procedure Open; DISPID 213;
        procedure Close; DISPID 214;
        function IsBlown: Wordbool; DISPID 215;
        property idx: Integer DISPID 216;
        property NumPhases: Integer READONLY DISPID 217;
    end;

// *********************************************************************//
// Interface: IISources
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CB2C7310-1717-4C6E-A7B8-DA54CF1722CD}
// *********************************************************************//
    IISources = interface(IDispatch)
        ['{CB2C7310-1717-4C6E-A7B8-DA54CF1722CD}']
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Amps: Double; SAFECALL;
        procedure Set_Amps(Value: Double); SAFECALL;
        function Get_AngleDeg: Double; SAFECALL;
        procedure Set_AngleDeg(Value: Double); SAFECALL;
        function Get_Frequency: Double; SAFECALL;
        procedure Set_Frequency(Value: Double); SAFECALL;
        property AllNames: Olevariant READ Get_AllNames;
        property Count: Integer READ Get_Count;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property Amps: Double READ Get_Amps WRITE Set_Amps;
        property AngleDeg: Double READ Get_AngleDeg WRITE Set_AngleDeg;
        property Frequency: Double READ Get_Frequency WRITE Set_Frequency;
    end;

// *********************************************************************//
// DispIntf:  IISourcesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CB2C7310-1717-4C6E-A7B8-DA54CF1722CD}
// *********************************************************************//
    IISourcesDisp = dispinterface
        ['{CB2C7310-1717-4C6E-A7B8-DA54CF1722CD}']
        property AllNames: Olevariant READONLY DISPID 201;
        property Count: Integer READONLY DISPID 202;
        property First: Integer READONLY DISPID 203;
        property Next: Integer READONLY DISPID 204;
        property Name: Widestring DISPID 205;
        property Amps: Double DISPID 206;
        property AngleDeg: Double DISPID 207;
        property Frequency: Double DISPID 208;
    end;

// *********************************************************************//
// Interface: IDSSimComs
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {25C5373D-5888-4A0C-974B-77EBD57ED0D1}
// *********************************************************************//
    IDSSimComs = interface(IDispatch)
        ['{25C5373D-5888-4A0C-974B-77EBD57ED0D1}']
        function BusVoltagepu(Index: SYSUINT): Olevariant; SAFECALL;
        function BusVoltage(Index: SYSUINT): Olevariant; SAFECALL;
    end;

// *********************************************************************//
// DispIntf:  IDSSimComsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {25C5373D-5888-4A0C-974B-77EBD57ED0D1}
// *********************************************************************//
    IDSSimComsDisp = dispinterface
        ['{25C5373D-5888-4A0C-974B-77EBD57ED0D1}']
        function BusVoltagepu(Index: SYSUINT): Olevariant; DISPID 202;
        function BusVoltage(Index: SYSUINT): Olevariant; DISPID 203;
    end;

// *********************************************************************//
// Interface: IPVSystems
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FAF19717-5887-43F6-8DC3-D0337E1081AD}
// *********************************************************************//
    IPVSystems = interface(IDispatch)
        ['{FAF19717-5887-43F6-8DC3-D0337E1081AD}']
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_RegisterNames: Olevariant; SAFECALL;
        function Get_RegisterValues: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_idx: Integer; SAFECALL;
        procedure Set_idx(Value: Integer); SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Irradiance: Double; SAFECALL;
        procedure Set_Irradiance(Value: Double); SAFECALL;
        function Get_kW: Double; SAFECALL;
        function Get_kvar: Double; SAFECALL;
        procedure Set_kvar(Value: Double); STDCALL;
        function Get_PF: Double; SAFECALL;
        procedure Set_PF(Value: Double); STDCALL;
        function Get_kVArated: Double; SAFECALL;
        procedure Set_kVArated(Value: Double); STDCALL;
        property AllNames: Olevariant READ Get_AllNames;
        property RegisterNames: Olevariant READ Get_RegisterNames;
        property RegisterValues: Olevariant READ Get_RegisterValues;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property Count: Integer READ Get_Count;
        property idx: Integer READ Get_idx WRITE Set_idx;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property Irradiance: Double READ Get_Irradiance WRITE Set_Irradiance;
        property kW: Double READ Get_kW;
    // Skipped Property "kvar"
    // Skipped Property "PF"
    // Skipped Property "kVArated"
    end;

// *********************************************************************//
// DispIntf:  IPVSystemsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FAF19717-5887-43F6-8DC3-D0337E1081AD}
// *********************************************************************//
    IPVSystemsDisp = dispinterface
        ['{FAF19717-5887-43F6-8DC3-D0337E1081AD}']
        property AllNames: Olevariant READONLY DISPID 201;
        property RegisterNames: Olevariant READONLY DISPID 202;
        property RegisterValues: Olevariant READONLY DISPID 203;
        property First: Integer READONLY DISPID 204;
        property Next: Integer READONLY DISPID 205;
        property Count: Integer READONLY DISPID 206;
        property idx: Integer DISPID 207;
        property Name: Widestring DISPID 208;
        property Irradiance: Double DISPID 209;
        property kW: Double READONLY DISPID 210;
        function kvar: Double; DISPID 211;
        function PF: Double; DISPID 212;
        function kVArated: Double; DISPID 213;
    end;

// *********************************************************************//
// Interface: IVsources
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8DCD1962-268B-40E1-B49E-B7C01C3E07CD}
// *********************************************************************//
    IVsources = interface(IDispatch)
        ['{8DCD1962-268B-40E1-B49E-B7C01C3E07CD}']
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_BasekV: Double; SAFECALL;
        procedure Set_BasekV(Value: Double); SAFECALL;
        function Get_pu: Double; SAFECALL;
        procedure Set_pu(Value: Double); SAFECALL;
        function Get_AngleDeg: Double; SAFECALL;
        procedure Set_AngleDeg(Value: Double); SAFECALL;
        function Get_Frequency: Double; SAFECALL;
        procedure Set_Frequency(Value: Double); SAFECALL;
        function Get_Phases: Integer; SAFECALL;
        procedure Set_Phases(Value: Integer); SAFECALL;
        property AllNames: Olevariant READ Get_AllNames;
        property Count: Integer READ Get_Count;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property BasekV: Double READ Get_BasekV WRITE Set_BasekV;
        property pu: Double READ Get_pu WRITE Set_pu;
        property AngleDeg: Double READ Get_AngleDeg WRITE Set_AngleDeg;
        property Frequency: Double READ Get_Frequency WRITE Set_Frequency;
        property Phases: Integer READ Get_Phases WRITE Set_Phases;
    end;

// *********************************************************************//
// DispIntf:  IVsourcesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8DCD1962-268B-40E1-B49E-B7C01C3E07CD}
// *********************************************************************//
    IVsourcesDisp = dispinterface
        ['{8DCD1962-268B-40E1-B49E-B7C01C3E07CD}']
        property AllNames: Olevariant READONLY DISPID 201;
        property Count: Integer READONLY DISPID 202;
        property First: Integer READONLY DISPID 203;
        property Next: Integer READONLY DISPID 204;
        property Name: Widestring DISPID 205;
        property BasekV: Double DISPID 206;
        property pu: Double DISPID 207;
        property AngleDeg: Double DISPID 208;
        property Frequency: Double DISPID 209;
        property Phases: Integer DISPID 210;
    end;

// *********************************************************************//
// Interface: IParallel
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A0351633-A988-4A5B-B551-A7E2ADDD4984}
// *********************************************************************//
    IParallel = interface(IDispatch)
        ['{A0351633-A988-4A5B-B551-A7E2ADDD4984}']
        function Get_NumCPUs: Integer; SAFECALL;
        function Get_NumCores: Integer; SAFECALL;
        function Get_ActiveActor: Integer; SAFECALL;
        procedure Set_ActiveActor(Value: Integer); SAFECALL;
        procedure CreateActor; SAFECALL;
        function Get_ActorCPU: Integer; SAFECALL;
        procedure Set_ActorCPU(Value: Integer); SAFECALL;
        function Get_NumOfActors: Integer; SAFECALL;
        procedure Wait; SAFECALL;
        function Get_ActorProgress: Olevariant; SAFECALL;
        function Get_ActorStatus: Olevariant; SAFECALL;
        function Get_ActiveParallel: Integer; SAFECALL;
        procedure Set_ActiveParallel(Value: Integer); SAFECALL;
        function Get_ConcatenateReports: Integer; SAFECALL;
        procedure Set_ConcatenateReports(Value: Integer); SAFECALL;
        property NumCPUs: Integer READ Get_NumCPUs;
        property NumCores: Integer READ Get_NumCores;
        property ActiveActor: Integer READ Get_ActiveActor WRITE Set_ActiveActor;
        property ActorCPU: Integer READ Get_ActorCPU WRITE Set_ActorCPU;
        property NumOfActors: Integer READ Get_NumOfActors;
        property ActorProgress: Olevariant READ Get_ActorProgress;
        property ActorStatus: Olevariant READ Get_ActorStatus;
        property ActiveParallel: Integer READ Get_ActiveParallel WRITE Set_ActiveParallel;
        property ConcatenateReports: Integer READ Get_ConcatenateReports WRITE Set_ConcatenateReports;
    end;

// *********************************************************************//
// DispIntf:  IParallelDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A0351633-A988-4A5B-B551-A7E2ADDD4984}
// *********************************************************************//
    IParallelDisp = dispinterface
        ['{A0351633-A988-4A5B-B551-A7E2ADDD4984}']
        property NumCPUs: Integer READONLY DISPID 201;
        property NumCores: Integer READONLY DISPID 202;
        property ActiveActor: Integer DISPID 203;
        procedure CreateActor; DISPID 204;
        property ActorCPU: Integer DISPID 205;
        property NumOfActors: Integer READONLY DISPID 206;
        procedure Wait; DISPID 207;
        property ActorProgress: Olevariant READONLY DISPID 208;
        property ActorStatus: Olevariant READONLY DISPID 209;
        property ActiveParallel: Integer DISPID 210;
        property ConcatenateReports: Integer DISPID 211;
    end;

// *********************************************************************//
// Interface: ILineCodes
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {519DBEAC-F1D5-4770-A890-3C8A7BB5E54D}
// *********************************************************************//
    ILineCodes = interface(IDispatch)
        ['{519DBEAC-F1D5-4770-A890-3C8A7BB5E54D}']
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_IsZ1Z0: Wordbool; SAFECALL;
        function Get_Units: Integer; SAFECALL;
        procedure Set_Units(Value: Integer); SAFECALL;
        function Get_Phases: Integer; SAFECALL;
        procedure Set_Phases(Value: Integer); SAFECALL;
        function Get_R1: Double; SAFECALL;
        procedure Set_R1(Value: Double); SAFECALL;
        function Get_X1: Double; SAFECALL;
        procedure Set_X1(Value: Double); SAFECALL;
        function Get_R0: Double; SAFECALL;
        procedure Set_R0(Value: Double); SAFECALL;
        function Get_X0: Double; SAFECALL;
        procedure Set_X0(Value: Double); SAFECALL;
        function Get_C1: Double; SAFECALL;
        procedure Set_C1(Value: Double); SAFECALL;
        function Get_C0: Double; SAFECALL;
        procedure Set_C0(Value: Double); SAFECALL;
        function Get_Rmatrix: Olevariant; SAFECALL;
        procedure Set_Rmatrix(Value: Olevariant); SAFECALL;
        function Get_Xmatrix: Olevariant; SAFECALL;
        procedure Set_Xmatrix(Value: Olevariant); SAFECALL;
        function Get_Cmatrix: Olevariant; SAFECALL;
        procedure Set_Cmatrix(Value: Olevariant); SAFECALL;
        function Get_NormAmps: Double; SAFECALL;
        procedure Set_NormAmps(Value: Double); SAFECALL;
        function Get_EmergAmps: Double; SAFECALL;
        procedure Set_EmergAmps(Value: Double); SAFECALL;
        function Get_AllNames: Olevariant; SAFECALL;
        property Count: Integer READ Get_Count;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property IsZ1Z0: Wordbool READ Get_IsZ1Z0;
        property Units: Integer READ Get_Units WRITE Set_Units;
        property Phases: Integer READ Get_Phases WRITE Set_Phases;
        property R1: Double READ Get_R1 WRITE Set_R1;
        property X1: Double READ Get_X1 WRITE Set_X1;
        property R0: Double READ Get_R0 WRITE Set_R0;
        property X0: Double READ Get_X0 WRITE Set_X0;
        property C1: Double READ Get_C1 WRITE Set_C1;
        property C0: Double READ Get_C0 WRITE Set_C0;
        property Rmatrix: Olevariant READ Get_Rmatrix WRITE Set_Rmatrix;
        property Xmatrix: Olevariant READ Get_Xmatrix WRITE Set_Xmatrix;
        property Cmatrix: Olevariant READ Get_Cmatrix WRITE Set_Cmatrix;
        property NormAmps: Double READ Get_NormAmps WRITE Set_NormAmps;
        property EmergAmps: Double READ Get_EmergAmps WRITE Set_EmergAmps;
        property AllNames: Olevariant READ Get_AllNames;
    end;

// *********************************************************************//
// DispIntf:  ILineCodesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {519DBEAC-F1D5-4770-A890-3C8A7BB5E54D}
// *********************************************************************//
    ILineCodesDisp = dispinterface
        ['{519DBEAC-F1D5-4770-A890-3C8A7BB5E54D}']
        property Count: Integer READONLY DISPID 201;
        property First: Integer READONLY DISPID 202;
        property Next: Integer READONLY DISPID 203;
        property Name: Widestring DISPID 204;
        property IsZ1Z0: Wordbool READONLY DISPID 205;
        property Units: Integer DISPID 206;
        property Phases: Integer DISPID 207;
        property R1: Double DISPID 208;
        property X1: Double DISPID 209;
        property R0: Double DISPID 210;
        property X0: Double DISPID 211;
        property C1: Double DISPID 212;
        property C0: Double DISPID 213;
        property Rmatrix: Olevariant DISPID 214;
        property Xmatrix: Olevariant DISPID 215;
        property Cmatrix: Olevariant DISPID 216;
        property NormAmps: Double DISPID 217;
        property EmergAmps: Double DISPID 218;
        property AllNames: Olevariant READONLY DISPID 219;
    end;

// *********************************************************************//
// Interface: IGICSources
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9CD30253-86C0-4339-B86E-745C912E8B15}
// *********************************************************************//
    IGICSources = interface(IDispatch)
        ['{9CD30253-86C0-4339-B86E-745C912E8B15}']
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Bus1: Widestring; SAFECALL;
        function Get_Bus2: Widestring; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Phases: Integer; SAFECALL;
        procedure Set_Phases(Value: Integer); SAFECALL;
        function Get_EN: Double; SAFECALL;
        procedure Set_EN(Value: Double); SAFECALL;
        function Get_EE: Double; SAFECALL;
        procedure Set_EE(Value: Double); SAFECALL;
        function Get_Lat1: Double; SAFECALL;
        procedure Set_Lat1(Value: Double); SAFECALL;
        function Get_Lat2: Double; SAFECALL;
        procedure Set_Lat2(Value: Double); SAFECALL;
        function Get_Lon1: Double; SAFECALL;
        procedure Set_Lon1(Value: Double); SAFECALL;
        function Get_Lon2: Double; SAFECALL;
        procedure Set_Lon2(Value: Double); SAFECALL;
        function Get_Volts: Double; SAFECALL;
        procedure Set_Volts(Value: Double); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        property AllNames: Olevariant READ Get_AllNames;
        property Bus1: Widestring READ Get_Bus1;
        property Bus2: Widestring READ Get_Bus2;
        property Name: Widestring READ Get_Name WRITE Set_Name;
        property Phases: Integer READ Get_Phases WRITE Set_Phases;
        property EN: Double READ Get_EN WRITE Set_EN;
        property EE: Double READ Get_EE WRITE Set_EE;
        property Lat1: Double READ Get_Lat1 WRITE Set_Lat1;
        property Lat2: Double READ Get_Lat2 WRITE Set_Lat2;
        property Lon1: Double READ Get_Lon1 WRITE Set_Lon1;
        property Lon2: Double READ Get_Lon2 WRITE Set_Lon2;
        property Volts: Double READ Get_Volts WRITE Set_Volts;
        property Count: Integer READ Get_Count;
        property First: Integer READ Get_First;
        property Next: Integer READ Get_Next;
    end;

// *********************************************************************//
// DispIntf:  IGICSourcesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9CD30253-86C0-4339-B86E-745C912E8B15}
// *********************************************************************//
    IGICSourcesDisp = dispinterface
        ['{9CD30253-86C0-4339-B86E-745C912E8B15}']
        property AllNames: Olevariant READONLY DISPID 201;
        property Bus1: Widestring READONLY DISPID 202;
        property Bus2: Widestring READONLY DISPID 203;
        property Name: Widestring DISPID 204;
        property Phases: Integer DISPID 205;
        property EN: Double DISPID 206;
        property EE: Double DISPID 207;
        property Lat1: Double DISPID 208;
        property Lat2: Double DISPID 209;
        property Lon1: Double DISPID 210;
        property Lon2: Double DISPID 211;
        property Volts: Double DISPID 212;
        property Count: Integer READONLY DISPID 213;
        property First: Integer READONLY DISPID 214;
        property Next: Integer READONLY DISPID 215;
    end;

// *********************************************************************//
// The Class CoText provides a Create and CreateRemote method to
// create instances of the default interface IText exposed by
// the CoClass Text. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoText = class
        class function Create: IText;
        class function CreateRemote(const MachineName: String): IText;
    end;

// *********************************************************************//
// The Class CoDSSProperty provides a Create and CreateRemote method to
// create instances of the default interface IDSSProperty exposed by
// the CoClass DSSProperty. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoDSSProperty = class
        class function Create: IDSSProperty;
        class function CreateRemote(const MachineName: String): IDSSProperty;
    end;

// *********************************************************************//
// The Class CoCktElement provides a Create and CreateRemote method to
// create instances of the default interface ICktElement exposed by
// the CoClass CktElement. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoCktElement = class
        class function Create: ICktElement;
        class function CreateRemote(const MachineName: String): ICktElement;
    end;

// *********************************************************************//
// The Class CoError provides a Create and CreateRemote method to
// create instances of the default interface IError exposed by
// the CoClass Error. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoError = class
        class function Create: IError;
        class function CreateRemote(const MachineName: String): IError;
    end;

// *********************************************************************//
// The Class CoCircuit provides a Create and CreateRemote method to
// create instances of the default interface ICircuit exposed by
// the CoClass Circuit. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoCircuit = class
        class function Create: ICircuit;
        class function CreateRemote(const MachineName: String): ICircuit;
    end;

// *********************************************************************//
// The Class CoBus provides a Create and CreateRemote method to
// create instances of the default interface IBus exposed by
// the CoClass Bus. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoBus = class
        class function Create: IBus;
        class function CreateRemote(const MachineName: String): IBus;
    end;

// *********************************************************************//
// The Class CoDSS provides a Create and CreateRemote method to
// create instances of the default interface IDSS exposed by
// the CoClass DSS. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoDSS = class
        class function Create: IDSS;
        class function CreateRemote(const MachineName: String): IDSS;
    end;

// *********************************************************************//
// The Class CoSolution provides a Create and CreateRemote method to
// create instances of the default interface ISolution exposed by
// the CoClass Solution. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoSolution = class
        class function Create: ISolution;
        class function CreateRemote(const MachineName: String): ISolution;
    end;

// *********************************************************************//
// The Class CoMonitors provides a Create and CreateRemote method to
// create instances of the default interface IMonitors exposed by
// the CoClass Monitors. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoMonitors = class
        class function Create: IMonitors;
        class function CreateRemote(const MachineName: String): IMonitors;
    end;

// *********************************************************************//
// The Class CoMeters provides a Create and CreateRemote method to
// create instances of the default interface IMeters exposed by
// the CoClass Meters. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoMeters = class
        class function Create: IMeters;
        class function CreateRemote(const MachineName: String): IMeters;
    end;

// *********************************************************************//
// The Class CoGenerators provides a Create and CreateRemote method to
// create instances of the default interface IGenerators exposed by
// the CoClass Generators. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoGenerators = class
        class function Create: IGenerators;
        class function CreateRemote(const MachineName: String): IGenerators;
    end;

// *********************************************************************//
// The Class CoDSSProgress provides a Create and CreateRemote method to
// create instances of the default interface IDSSProgress exposed by
// the CoClass DSSProgress. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoDSSProgress = class
        class function Create: IDSSProgress;
        class function CreateRemote(const MachineName: String): IDSSProgress;
    end;

// *********************************************************************//
// The Class CoSettings provides a Create and CreateRemote method to
// create instances of the default interface ISettings exposed by
// the CoClass Settings. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoSettings = class
        class function Create: ISettings;
        class function CreateRemote(const MachineName: String): ISettings;
    end;

// *********************************************************************//
// The Class CoLines provides a Create and CreateRemote method to
// create instances of the default interface ILines exposed by
// the CoClass Lines. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoLines = class
        class function Create: ILines;
        class function CreateRemote(const MachineName: String): ILines;
    end;

// *********************************************************************//
// The Class CoCtrlQueue provides a Create and CreateRemote method to
// create instances of the default interface ICtrlQueue exposed by
// the CoClass CtrlQueue. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoCtrlQueue = class
        class function Create: ICtrlQueue;
        class function CreateRemote(const MachineName: String): ICtrlQueue;
    end;

// *********************************************************************//
// The Class CoLoads provides a Create and CreateRemote method to
// create instances of the default interface ILoads exposed by
// the CoClass Loads. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoLoads = class
        class function Create: ILoads;
        class function CreateRemote(const MachineName: String): ILoads;
    end;

// *********************************************************************//
// The Class CoDSSElement provides a Create and CreateRemote method to
// create instances of the default interface IDSSElement exposed by
// the CoClass DSSElement. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoDSSElement = class
        class function Create: IDSSElement;
        class function CreateRemote(const MachineName: String): IDSSElement;
    end;

// *********************************************************************//
// The Class CoActiveClass provides a Create and CreateRemote method to
// create instances of the default interface IActiveClass exposed by
// the CoClass ActiveClass. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoActiveClass = class
        class function Create: IActiveClass;
        class function CreateRemote(const MachineName: String): IActiveClass;
    end;

// *********************************************************************//
// The Class CoCapacitors provides a Create and CreateRemote method to
// create instances of the default interface ICapacitors exposed by
// the CoClass Capacitors. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoCapacitors = class
        class function Create: ICapacitors;
        class function CreateRemote(const MachineName: String): ICapacitors;
    end;

// *********************************************************************//
// The Class CoTransformers provides a Create and CreateRemote method to
// create instances of the default interface ITransformers exposed by
// the CoClass Transformers. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoTransformers = class
        class function Create: ITransformers;
        class function CreateRemote(const MachineName: String): ITransformers;
    end;

// *********************************************************************//
// The Class CoSwtControls provides a Create and CreateRemote method to
// create instances of the default interface ISwtControls exposed by
// the CoClass SwtControls. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoSwtControls = class
        class function Create: ISwtControls;
        class function CreateRemote(const MachineName: String): ISwtControls;
    end;

// *********************************************************************//
// The Class CoCapControls provides a Create and CreateRemote method to
// create instances of the default interface ICapControls exposed by
// the CoClass CapControls. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoCapControls = class
        class function Create: ICapControls;
        class function CreateRemote(const MachineName: String): ICapControls;
    end;

// *********************************************************************//
// The Class CoRegControls provides a Create and CreateRemote method to
// create instances of the default interface IRegControls exposed by
// the CoClass RegControls. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoRegControls = class
        class function Create: IRegControls;
        class function CreateRemote(const MachineName: String): IRegControls;
    end;

// *********************************************************************//
// The Class CoTopology provides a Create and CreateRemote method to
// create instances of the default interface ITopology exposed by
// the CoClass Topology. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoTopology = class
        class function Create: ITopology;
        class function CreateRemote(const MachineName: String): ITopology;
    end;

// *********************************************************************//
// The Class CoDSS_Executive provides a Create and CreateRemote method to
// create instances of the default interface IDSS_Executive exposed by
// the CoClass DSS_Executive. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoDSS_Executive = class
        class function Create: IDSS_Executive;
        class function CreateRemote(const MachineName: String): IDSS_Executive;
    end;

// *********************************************************************//
// The Class CoDSSEvents provides a Create and CreateRemote method to
// create instances of the default interface IDSSEvents exposed by
// the CoClass DSSEvents. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoDSSEvents = class
        class function Create: IDSSEvents;
        class function CreateRemote(const MachineName: String): IDSSEvents;
    end;

// *********************************************************************//
// The Class CoSensors provides a Create and CreateRemote method to
// create instances of the default interface ISensors exposed by
// the CoClass Sensors. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoSensors = class
        class function Create: ISensors;
        class function CreateRemote(const MachineName: String): ISensors;
    end;

// *********************************************************************//
// The Class CoXYCurves provides a Create and CreateRemote method to
// create instances of the default interface IXYCurves exposed by
// the CoClass XYCurves. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoXYCurves = class
        class function Create: IXYCurves;
        class function CreateRemote(const MachineName: String): IXYCurves;
    end;

// *********************************************************************//
// The Class CoPDElements provides a Create and CreateRemote method to
// create instances of the default interface IPDElements exposed by
// the CoClass PDElements. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoPDElements = class
        class function Create: IPDElements;
        class function CreateRemote(const MachineName: String): IPDElements;
    end;

// *********************************************************************//
// The Class CoReclosers provides a Create and CreateRemote method to
// create instances of the default interface IReclosers exposed by
// the CoClass Reclosers. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoReclosers = class
        class function Create: IReclosers;
        class function CreateRemote(const MachineName: String): IReclosers;
    end;

// *********************************************************************//
// The Class CoRelays provides a Create and CreateRemote method to
// create instances of the default interface IRelays exposed by
// the CoClass Relays. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoRelays = class
        class function Create: IRelays;
        class function CreateRemote(const MachineName: String): IRelays;
    end;

// *********************************************************************//
// The Class CoCmathLib provides a Create and CreateRemote method to
// create instances of the default interface ICmathLib exposed by
// the CoClass CmathLib. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoCmathLib = class
        class function Create: ICmathLib;
        class function CreateRemote(const MachineName: String): ICmathLib;
    end;

// *********************************************************************//
// The Class CoParser provides a Create and CreateRemote method to
// create instances of the default interface IParser exposed by
// the CoClass Parser. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoParser = class
        class function Create: IParser;
        class function CreateRemote(const MachineName: String): IParser;
    end;

// *********************************************************************//
// The Class CoLoadShapes provides a Create and CreateRemote method to
// create instances of the default interface ILoadShapes exposed by
// the CoClass LoadShapes. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoLoadShapes = class
        class function Create: ILoadShapes;
        class function CreateRemote(const MachineName: String): ILoadShapes;
    end;

// *********************************************************************//
// The Class CoFuses provides a Create and CreateRemote method to
// create instances of the default interface IFuses exposed by
// the CoClass Fuses. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoFuses = class
        class function Create: IFuses;
        class function CreateRemote(const MachineName: String): IFuses;
    end;

// *********************************************************************//
// The Class CoISources provides a Create and CreateRemote method to
// create instances of the default interface IISources exposed by
// the CoClass ISources. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoISources = class
        class function Create: IISources;
        class function CreateRemote(const MachineName: String): IISources;
    end;

// *********************************************************************//
// The Class CoDSSimComs provides a Create and CreateRemote method to
// create instances of the default interface IDSSimComs exposed by
// the CoClass DSSimComs. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoDSSimComs = class
        class function Create: IDSSimComs;
        class function CreateRemote(const MachineName: String): IDSSimComs;
    end;

// *********************************************************************//
// The Class CoPVSystems provides a Create and CreateRemote method to
// create instances of the default interface IPVSystems exposed by
// the CoClass PVSystems. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoPVSystems = class
        class function Create: IPVSystems;
        class function CreateRemote(const MachineName: String): IPVSystems;
    end;

// *********************************************************************//
// The Class CoVsources provides a Create and CreateRemote method to
// create instances of the default interface IVsources exposed by
// the CoClass Vsources. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoVsources = class
        class function Create: IVsources;
        class function CreateRemote(const MachineName: String): IVsources;
    end;

// *********************************************************************//
// The Class CoParallel provides a Create and CreateRemote method to
// create instances of the default interface IParallel exposed by
// the CoClass Parallel. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoParallel = class
        class function Create: IParallel;
        class function CreateRemote(const MachineName: String): IParallel;
    end;

// *********************************************************************//
// The Class CoLineCodes provides a Create and CreateRemote method to
// create instances of the default interface ILineCodes exposed by
// the CoClass LineCodes. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoLineCodes = class
        class function Create: ILineCodes;
        class function CreateRemote(const MachineName: String): ILineCodes;
    end;

// *********************************************************************//
// The Class CoGICSources provides a Create and CreateRemote method to
// create instances of the default interface IGICSources exposed by
// the CoClass GICSources. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
    CoGICSources = class
        class function Create: IGICSources;
        class function CreateRemote(const MachineName: String): IGICSources;
    end;

implementation

uses
    System.Win.ComObj;

class function CoText.Create: IText;
begin
    Result := CreateComObject(CLASS_Text) as IText;
end;

class function CoText.CreateRemote(const MachineName: String): IText;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Text) as IText;
end;

class function CoDSSProperty.Create: IDSSProperty;
begin
    Result := CreateComObject(CLASS_DSSProperty) as IDSSProperty;
end;

class function CoDSSProperty.CreateRemote(const MachineName: String): IDSSProperty;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_DSSProperty) as IDSSProperty;
end;

class function CoCktElement.Create: ICktElement;
begin
    Result := CreateComObject(CLASS_CktElement) as ICktElement;
end;

class function CoCktElement.CreateRemote(const MachineName: String): ICktElement;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_CktElement) as ICktElement;
end;

class function CoError.Create: IError;
begin
    Result := CreateComObject(CLASS_Error) as IError;
end;

class function CoError.CreateRemote(const MachineName: String): IError;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Error) as IError;
end;

class function CoCircuit.Create: ICircuit;
begin
    Result := CreateComObject(CLASS_Circuit) as ICircuit;
end;

class function CoCircuit.CreateRemote(const MachineName: String): ICircuit;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Circuit) as ICircuit;
end;

class function CoBus.Create: IBus;
begin
    Result := CreateComObject(CLASS_Bus) as IBus;
end;

class function CoBus.CreateRemote(const MachineName: String): IBus;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Bus) as IBus;
end;

class function CoDSS.Create: IDSS;
begin
    Result := CreateComObject(CLASS_DSS) as IDSS;
end;

class function CoDSS.CreateRemote(const MachineName: String): IDSS;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_DSS) as IDSS;
end;

class function CoSolution.Create: ISolution;
begin
    Result := CreateComObject(CLASS_Solution) as ISolution;
end;

class function CoSolution.CreateRemote(const MachineName: String): ISolution;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Solution) as ISolution;
end;

class function CoMonitors.Create: IMonitors;
begin
    Result := CreateComObject(CLASS_Monitors) as IMonitors;
end;

class function CoMonitors.CreateRemote(const MachineName: String): IMonitors;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Monitors) as IMonitors;
end;

class function CoMeters.Create: IMeters;
begin
    Result := CreateComObject(CLASS_Meters) as IMeters;
end;

class function CoMeters.CreateRemote(const MachineName: String): IMeters;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Meters) as IMeters;
end;

class function CoGenerators.Create: IGenerators;
begin
    Result := CreateComObject(CLASS_Generators) as IGenerators;
end;

class function CoGenerators.CreateRemote(const MachineName: String): IGenerators;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Generators) as IGenerators;
end;

class function CoDSSProgress.Create: IDSSProgress;
begin
    Result := CreateComObject(CLASS_DSSProgress) as IDSSProgress;
end;

class function CoDSSProgress.CreateRemote(const MachineName: String): IDSSProgress;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_DSSProgress) as IDSSProgress;
end;

class function CoSettings.Create: ISettings;
begin
    Result := CreateComObject(CLASS_Settings) as ISettings;
end;

class function CoSettings.CreateRemote(const MachineName: String): ISettings;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Settings) as ISettings;
end;

class function CoLines.Create: ILines;
begin
    Result := CreateComObject(CLASS_Lines) as ILines;
end;

class function CoLines.CreateRemote(const MachineName: String): ILines;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Lines) as ILines;
end;

class function CoCtrlQueue.Create: ICtrlQueue;
begin
    Result := CreateComObject(CLASS_CtrlQueue) as ICtrlQueue;
end;

class function CoCtrlQueue.CreateRemote(const MachineName: String): ICtrlQueue;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_CtrlQueue) as ICtrlQueue;
end;

class function CoLoads.Create: ILoads;
begin
    Result := CreateComObject(CLASS_Loads) as ILoads;
end;

class function CoLoads.CreateRemote(const MachineName: String): ILoads;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Loads) as ILoads;
end;

class function CoDSSElement.Create: IDSSElement;
begin
    Result := CreateComObject(CLASS_DSSElement) as IDSSElement;
end;

class function CoDSSElement.CreateRemote(const MachineName: String): IDSSElement;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_DSSElement) as IDSSElement;
end;

class function CoActiveClass.Create: IActiveClass;
begin
    Result := CreateComObject(CLASS_ActiveClass) as IActiveClass;
end;

class function CoActiveClass.CreateRemote(const MachineName: String): IActiveClass;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_ActiveClass) as IActiveClass;
end;

class function CoCapacitors.Create: ICapacitors;
begin
    Result := CreateComObject(CLASS_Capacitors) as ICapacitors;
end;

class function CoCapacitors.CreateRemote(const MachineName: String): ICapacitors;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Capacitors) as ICapacitors;
end;

class function CoTransformers.Create: ITransformers;
begin
    Result := CreateComObject(CLASS_Transformers) as ITransformers;
end;

class function CoTransformers.CreateRemote(const MachineName: String): ITransformers;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Transformers) as ITransformers;
end;

class function CoSwtControls.Create: ISwtControls;
begin
    Result := CreateComObject(CLASS_SwtControls) as ISwtControls;
end;

class function CoSwtControls.CreateRemote(const MachineName: String): ISwtControls;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_SwtControls) as ISwtControls;
end;

class function CoCapControls.Create: ICapControls;
begin
    Result := CreateComObject(CLASS_CapControls) as ICapControls;
end;

class function CoCapControls.CreateRemote(const MachineName: String): ICapControls;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_CapControls) as ICapControls;
end;

class function CoRegControls.Create: IRegControls;
begin
    Result := CreateComObject(CLASS_RegControls) as IRegControls;
end;

class function CoRegControls.CreateRemote(const MachineName: String): IRegControls;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_RegControls) as IRegControls;
end;

class function CoTopology.Create: ITopology;
begin
    Result := CreateComObject(CLASS_Topology) as ITopology;
end;

class function CoTopology.CreateRemote(const MachineName: String): ITopology;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Topology) as ITopology;
end;

class function CoDSS_Executive.Create: IDSS_Executive;
begin
    Result := CreateComObject(CLASS_DSS_Executive) as IDSS_Executive;
end;

class function CoDSS_Executive.CreateRemote(const MachineName: String): IDSS_Executive;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_DSS_Executive) as IDSS_Executive;
end;

class function CoDSSEvents.Create: IDSSEvents;
begin
    Result := CreateComObject(CLASS_DSSEvents) as IDSSEvents;
end;

class function CoDSSEvents.CreateRemote(const MachineName: String): IDSSEvents;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_DSSEvents) as IDSSEvents;
end;

class function CoSensors.Create: ISensors;
begin
    Result := CreateComObject(CLASS_Sensors) as ISensors;
end;

class function CoSensors.CreateRemote(const MachineName: String): ISensors;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Sensors) as ISensors;
end;

class function CoXYCurves.Create: IXYCurves;
begin
    Result := CreateComObject(CLASS_XYCurves) as IXYCurves;
end;

class function CoXYCurves.CreateRemote(const MachineName: String): IXYCurves;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_XYCurves) as IXYCurves;
end;

class function CoPDElements.Create: IPDElements;
begin
    Result := CreateComObject(CLASS_PDElements) as IPDElements;
end;

class function CoPDElements.CreateRemote(const MachineName: String): IPDElements;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_PDElements) as IPDElements;
end;

class function CoReclosers.Create: IReclosers;
begin
    Result := CreateComObject(CLASS_Reclosers) as IReclosers;
end;

class function CoReclosers.CreateRemote(const MachineName: String): IReclosers;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Reclosers) as IReclosers;
end;

class function CoRelays.Create: IRelays;
begin
    Result := CreateComObject(CLASS_Relays) as IRelays;
end;

class function CoRelays.CreateRemote(const MachineName: String): IRelays;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Relays) as IRelays;
end;

class function CoCmathLib.Create: ICmathLib;
begin
    Result := CreateComObject(CLASS_CmathLib) as ICmathLib;
end;

class function CoCmathLib.CreateRemote(const MachineName: String): ICmathLib;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_CmathLib) as ICmathLib;
end;

class function CoParser.Create: IParser;
begin
    Result := CreateComObject(CLASS_Parser) as IParser;
end;

class function CoParser.CreateRemote(const MachineName: String): IParser;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Parser) as IParser;
end;

class function CoLoadShapes.Create: ILoadShapes;
begin
    Result := CreateComObject(CLASS_LoadShapes) as ILoadShapes;
end;

class function CoLoadShapes.CreateRemote(const MachineName: String): ILoadShapes;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_LoadShapes) as ILoadShapes;
end;

class function CoFuses.Create: IFuses;
begin
    Result := CreateComObject(CLASS_Fuses) as IFuses;
end;

class function CoFuses.CreateRemote(const MachineName: String): IFuses;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Fuses) as IFuses;
end;

class function CoISources.Create: IISources;
begin
    Result := CreateComObject(CLASS_ISources) as IISources;
end;

class function CoISources.CreateRemote(const MachineName: String): IISources;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_ISources) as IISources;
end;

class function CoDSSimComs.Create: IDSSimComs;
begin
    Result := CreateComObject(CLASS_DSSimComs) as IDSSimComs;
end;

class function CoDSSimComs.CreateRemote(const MachineName: String): IDSSimComs;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_DSSimComs) as IDSSimComs;
end;

class function CoPVSystems.Create: IPVSystems;
begin
    Result := CreateComObject(CLASS_PVSystems) as IPVSystems;
end;

class function CoPVSystems.CreateRemote(const MachineName: String): IPVSystems;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_PVSystems) as IPVSystems;
end;

class function CoVsources.Create: IVsources;
begin
    Result := CreateComObject(CLASS_Vsources) as IVsources;
end;

class function CoVsources.CreateRemote(const MachineName: String): IVsources;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Vsources) as IVsources;
end;

class function CoParallel.Create: IParallel;
begin
    Result := CreateComObject(CLASS_Parallel) as IParallel;
end;

class function CoParallel.CreateRemote(const MachineName: String): IParallel;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_Parallel) as IParallel;
end;

class function CoLineCodes.Create: ILineCodes;
begin
    Result := CreateComObject(CLASS_LineCodes) as ILineCodes;
end;

class function CoLineCodes.CreateRemote(const MachineName: String): ILineCodes;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_LineCodes) as ILineCodes;
end;

class function CoGICSources.Create: IGICSources;
begin
    Result := CreateComObject(CLASS_GICSources) as IGICSources;
end;

class function CoGICSources.CreateRemote(const MachineName: String): IGICSources;
begin
    Result := CreateRemoteComObject(MachineName, CLASS_GICSources) as IGICSources;
end;

end.
