unit ExportCIMXML;

{
  ----------------------------------------------------------
  Copyright (c) 2009-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

// Write a CIM XML file using RDF Schema for the Common Distribution
// Power System Model, IEC 61968-13.

interface

uses
    Classes, NamedObject,  // for TUuid
    DSSClass,
    CktElement,
    PDElement,
    Transformer,
    AutoTrans,
    Storage2,
    InvControl2,
    ExpControl,
    PVSystem2,
    HashList;



type
    UuidChoice = (Bank, Wdg, XfCore, XfMesh, WdgInf, ScTest, OcTest,
        BaseV, LinePhase, LoadPhase, GenPhase, CapPhase, SolarPhase, BatteryPhase,
        XfLoc, LoadLoc, LineLoc, CapLoc, Topo, ReacLoc, SolarLoc, BatteryLoc,
        OpLimV, OpLimI, LoadResp, CIMVer, PosPt, CoordSys, TopoIsland, Station,
        GeoRgn, SubGeoRgn, ZData, OpLimT, XfInfo, FdrLoc, OpLimAHi, OpLimALo,
        OpLimBHi, OpLimBLo, MachLoc, PVPanels, Battery, SrcLoc, TankInfo,
        TapCtrl, PUZ, WirePos, NormAmps, EmergAmps,
        I1547NameplateData, I1547NameplateDataApplied, I1547Signal, I1547VoltVar,
        I1547WattVar, I1547ConstPF, I1547VoltWatt, I1547ConstQ);

    ProfileChoice = (FunPrf, EpPrf, GeoPrf, TopoPrf, CatPrf, SshPrf, DynPrf);

    TCIMExporter = class;

    TCIMBankObject = class(TNamedObject)
    PUBLIC
        vectorGroup: String;
        maxWindings: Integer;
        nWindings: Integer;
        connections: array of Integer;
        bAuto: Boolean;
        angles: array of Integer;
        phaseA: array of Integer;
        phaseB: array of Integer;
        phaseC: array of Integer;
        ground: array of Integer;
        pd_unit: TPDElement;  // save this for writing the bank coordinates

        constructor Create(MaxWdg: Integer);
        destructor Destroy; OVERRIDE;

        procedure AddTransformer(CE: TCIMExporter; pXf: TTransfObj);
        procedure AddAutoTransformer(CE: TCIMExporter; pAuto: TAutoTransObj);
        procedure BuildVectorGroup;
    end;

    TCIMOpLimitObject = class(TNamedObject)
    PUBLIC
        NormAmps: Double;
        EmergAmps: Double;
        constructor Create(norm, emerg: Double);
        destructor Destroy; OVERRIDE;
    end;

    TCIMExporter = class(TObject)
    public
        procedure StartUuidList(size: Integer);
        procedure FreeUuidList;
        procedure WriteHashedUUIDs(F: TFileStream);
        procedure AddHashedUUID(key: String; UuidVal: String);
        procedure DefaultCircuitUUIDs(var fdrID: TUuid; var subID: TUuid; var rgnID: TUuid; var subGeoID: TUuid);
        procedure ExportCDPSM(FileNm: String;
            Substation: String;
            SubGeographicRegion: String;
            GeographicRegion: String;
            FdrUUID: TUuid;
            SubUUID: TUuid;
            SubGeoUUID: TUuid;
            RgnUUID: TUuid;
            Combined: Boolean = TRUE);

        constructor Create(dssContext: TDSSContext);
        destructor Destroy; override;
    private
        DSS: TDSSContext;
        UuidHash: THashList;       // index is 1-based
        UuidList: array of TUuid;  // index is 0-based
        UuidKeyList: array of String;
        BankHash: THashList;
        BankList: array of TCIMBankObject;
        OpLimitHash: THashList;
        OpLimitList: array of TCIMOpLimitObject;
        // the Combined XML can be broken into six separate profiles
        F_FUN: TFileStream;
        F_EP: TFileStream;
        F_SSH: TFileStream;
        F_CAT: TFileStream;
        F_GEO: TFileStream;
        F_TOPO: TFileStream;
        F_DYN: TFileStream;
        roots: array[ProfileChoice] of String;
        ids: array[ProfileChoice] of TUuid;
    public
        Separate: Boolean;
        procedure WriteCimLn(prf: ProfileChoice; const s: String);
        procedure StartInstance(prf: ProfileChoice; Root: String; Obj: TNamedObject);
        procedure StartFreeInstance(prf: ProfileChoice; Root: String; uuid: TUUID);
        procedure EndInstance(prf: ProfileChoice; Root: String);
    end;

    TRemoteSignalObject = class(TNamedObject)
    public
        busName: String;
        pElem: TDSSCktElement;
        trm: Integer;
        phase: String; // want A, B, C, s1 or s2
        ex: TCIMExporter;
        constructor Create(exporter: TCIMExporter; aBusName:String; seq: Integer; invName: String);
        destructor Destroy; override;
    end;

    TIEEE1547Controller = class(TObject)
    private
        ND_acVmax, ND_acVmin, AD_pMax, AD_pMaxOverPF, AD_overPF, AD_pMaxUnderPF: Double;
        AD_underPF, AD_sMax, AD_qMaxInj, AD_qMaxAbs, AD_pMaxCharge: Double;
        AD_apparentPowerChargeMax, AD_acVnom: Double;
        VV_vRef, VV_vRefOlrt, VV_curveV1, VV_curveV2, VV_curveV3, VV_curveV4: Double;
        VV_olrt, VV_curveQ1, VV_curveQ2, VV_curveQ3, VV_curveQ4: Double;
        Q_reactivePower, PF_powerFactor, VW_olrt, VW_curveV1, VW_curveV2: Double;
        VW_curveP1, VW_curveP2gen, VW_curveP2load: Double;
        WV_curveP1gen, WV_curveP2gen, WV_curveP3gen: Double;
        WV_curveP1load, WV_curveP2load, WV_curveP3load: Double;
        WV_curveQ1gen, WV_curveQ2gen, WV_curveQ3gen: Double;
        WV_curveQ1load, WV_curveQ2load, WV_curveQ3load: Double;

        ND_normalOPcatKind, PF_constPFexcitationKind: String;
        VV_enabled, WV_enabled, PF_enabled, Q_enabled, VW_enabled: Boolean;
        VV_vRefAutoModeEnabled: Boolean;

        pInvName: TNamedObject;
        pPlateName: TNamedObject;
        pSetName: TNamedObject;
        pDERNames: TStringList;
        pMonBuses: TStringList;
        Signals: array of TRemoteSignalObject;

        bNameplateSet: Boolean;
        ex: TCIMExporter;

        procedure FinishNameplate;
        procedure SetStorageNameplate(pBat: TStorage2Obj);
        procedure SetPhotovoltaicNameplate(pPV: TPVSystem2Obj);
        procedure SetElementNameplate(pElem: TDSSCktElement);
        procedure SetDefaults(bCatB: Boolean);
        procedure FindSignalTerminals;
        function CheckSignalMatch(sig: TRemoteSignalObject; pElm:TDSSCktElement; seq: Integer) : Boolean;
    public
        constructor Create(exporter: TCIMExporter);
        destructor Destroy; override;

        procedure PullFromInvControl(pInv: TInvControl2Obj);
        procedure PullFromExpControl(pExp: TExpControlObj);
        procedure WriteCIM (prf: ProfileChoice);
    end;

implementation

uses
    BufStream,
    SysUtils,
    Utilities,
    Circuit,
    DSSClassDefs,
    DSSGlobals,
    PCElement,
    Generator,
    Load,
    RegControl,
    Vsource,
    Line,
    UComplex, DSSUcomplex,
    UcMatrix,
    LineCode,
    Fuse,
    Capacitor,
    CapControl,
    Reactor,
    ConductorData,
    LineUnits,
    LineGeometry,
    StrUtils,
    Math,
    WireData,
    XfmrCode,
    LineSpacing,
    CableData,
    CNData,
    TSData,
    Storage,
    PVSystem,
    Relay,
    Recloser,
    XYCurve,
    DSSObject,
    DSSHelper,
    CmdForms;

const
//  CIM_NS = 'http://iec.ch/TC57/2012/CIM-schema-cim17';
    CIM_NS = 'http://iec.ch/TC57/CIM100';
    CatBQmin = 0.43; // for IEEE 1547 Category B estimate

type
    TCIMExporterHelper = class helper for TCIMExporter
    private
        procedure StartCIMFile(F: TFileStream; FileNm: String; prf: ProfileChoice);
        procedure StartOpLimitList(size: Integer);
        procedure StartBankList(size: Integer);
        procedure FreeBankList;
        procedure FreeOpLimitList;
        procedure AddBank(pBank: TCIMBankObject);
        function GetBank(sBank: String): TCIMBankObject;
        procedure AddOpLimit(pLimit: TCIMOpLimitObject);
        function GetOpLimit(sLimit: String): TCIMOpLimitObject;
        function GetHashedUuid(key: String): TUuid;
        function GetDevUuid(which: UuidChoice; Name: String; Seq: Integer): TUuid;
        function GetTermUuid(pElem: TDSSCktElement; Seq: Integer): TUuid;
        function GetBaseVUuid(val: Double): TUuid;
        function GetOpLimVUuid(val: Double): TUuid;
        function GetOpLimIUuid(norm, emerg: Double): TUuid;
        function PhaseString(pElem: TDSSCktElement; bus: Integer; bAllowSec: Boolean = True): String; // if order doesn't matter
        function PhaseOrderString(pElem: TDSSCktElement; bus: Integer; bAllowSec: Boolean = True): String; // for transposition
        procedure ParseSwitchClass(pLine: TLineObj; var swtCls: String; var ratedAmps, breakingAmps: Double);
        procedure DoubleNode(prf: ProfileChoice; Node: String; val: Double);
        procedure IntegerNode(prf: ProfileChoice; Node: String; val: Integer);
        procedure BooleanNode(prf: ProfileChoice; Node: String; val: Boolean);
        procedure RefNode(prf: ProfileChoice; Node: String; Obj: TNamedObject);
        procedure UuidNode(prf: ProfileChoice; Node: String; ID: TUuid);
        procedure LineCodeRefNode(prf: ProfileChoice; List: TLineCode; Obj: TLineCodeObj);
        procedure LineSpacingRefNode(prf: ProfileChoice; Obj: TDSSObject);
        procedure PhaseWireRefNode(prf: ProfileChoice; Obj: TConductorDataObj);
        procedure CircuitNode(prf: ProfileChoice; Obj: TNamedObject);
        function FirstPhaseString(pElem: TDSSCktElement; bus: Integer): String;
        procedure GeneratorControlEnum(prf: ProfileChoice; val: String);
        procedure BatteryStateEnum(prf: ProfileChoice; val: Integer);
        procedure SynchMachTypeEnum(prf: ProfileChoice; val: String);
        procedure SynchMachModeEnum(prf: ProfileChoice; val: String);
        procedure RegulatingControlEnum(prf: ProfileChoice; val: String);
        procedure WindingConnectionEnum(prf: ProfileChoice; val: String);
        procedure ConductorInsulationEnum(prf: ProfileChoice; val: String);
        procedure ConductorUsageEnum(prf: ProfileChoice; val: String);
        procedure CableShieldMaterialEnum(prf: ProfileChoice; val: String);
        procedure ConductorMaterialEnum(prf: ProfileChoice; val: String);
        procedure CableOuterJacketEnum(prf: ProfileChoice; val: String);
        procedure CableConstructionEnum(prf: ProfileChoice; val: String);
        procedure TransformerControlEnum(prf: ProfileChoice; val: String);
        procedure MonitoredPhaseNode(prf: ProfileChoice; val: String);
        procedure OpLimitDirectionEnum(prf: ProfileChoice; val: String);
        procedure NormalOpCatEnum (prf: ProfileChoice; val: String);
        //procedure SupportedModesEnum (prf: ProfileChoice; val: String);
        procedure PowerFactorExcitationEnum (prf: ProfileChoice; val: String);
        procedure RemoteInputSignalEnum (prf: ProfileChoice; val: String);
        procedure StringNode(prf: ProfileChoice; Node: String; val: String);
        procedure StartInstance(prf: ProfileChoice; Root: String; Obj: TNamedObject);
        procedure StartFreeInstance(prf: ProfileChoice; Root: String; uuid: TUUID);
        procedure EndInstance(prf: ProfileChoice; Root: String);
        procedure XfmrTankPhasesAndGround(fprf: ProfileChoice; eprf: ProfileChoice; pXf:TTransfObj; bus: Integer);
        procedure PhaseNode(prf: ProfileChoice; Root: String; val: String);
        procedure PhaseKindNode(prf: ProfileChoice; Root: String; val: String);
        procedure PhaseSideNode(prf: ProfileChoice; Root: String; Side: Integer; val: String);
        procedure ShuntConnectionKindNode(prf: ProfileChoice; Root: String; val: String); // D, Y, Yn, I
        procedure WindingConnectionKindNode(prf: ProfileChoice; val: String); // D, Y, Z, Yn, Zn, A, I
        procedure AttachLinePhases(pLine: TLineObj);
        procedure AttachSwitchPhases(pLine: TLineObj);
        procedure AttachCapPhases(pCap: TCapacitorObj; geoUUID: TUuid; sections: double);
        procedure AttachSecondaryPhases(pLoad: TLoadObj; geoUUID: TUuid; pPhase: TNamedObject; p, q: Double; phs: String);
        procedure AttachLoadPhases(pLoad: TLoadObj; geoUUID: TUuid);
        procedure AttachSecondaryGenPhases(pGen: TGeneratorObj; geoUUID: TUuid; pPhase: TNamedObject; p, q: Double; phs: String);
        procedure AttachGeneratorPhases(pGen: TGeneratorObj; geoUUID: TUuid);
        procedure AttachSecondarySolarPhases(pPV: TPVSystem2Obj; geoUUID: TUuid; pPhase: TNamedObject; p, q: Double; phs: String);
        procedure AttachSolarPhases(pPV: TPVSystem2Obj; geoUUID: TUuid);
        procedure AttachSecondaryStoragePhases(pBat: TStorage2Obj; geoUUID: TUuid; pPhase: TNamedObject; p, q: Double; phs: String);
        procedure AttachStoragePhases(pBat: TStorage2Obj; geoUUID: TUuid);
        procedure WriteLoadModel(Name: String; ID: TUuid;
            zP: Double; iP: Double; pP: Double; zQ: Double; iQ: Double; pQ: Double;
            eP: Double; eQ: Double);
        procedure WritePositions(pElem: TDSSCktElement; geoUUID: TUuid; crsUUID: TUuid);
        procedure WriteReferenceTerminals(pElem: TDSSCktElement; RefUuid: TUuid; norm: Double = 0.0; emerg: Double = 0.0);
        procedure WriteTerminals(pElem: TDSSCktElement; geoUUID: TUuid; crsUUID: TUuid; norm: Double = 0.0; emerg: Double = 0.0);
        procedure VbaseNode(prf: ProfileChoice; pElem: TDSSCktElement);
        procedure WriteXfmrCode(pXfCd: TXfmrCodeObj);
        procedure WriteCableData(pCab: TCableDataObj);
        procedure WriteTapeData(pCab: TTSDataObj);
        procedure WriteConcData(pCab: TCNDataObj);
        procedure WriteWireData(pWire: TConductorDataObj);
        procedure ConverterControlEnum(prf: ProfileChoice; varMode: Integer; CIMdynamics: Boolean);

        procedure FD_Destroy;
        procedure FD_Create(Combined: Boolean; FileName: String);

        function ActiveCircuit: TDSSCircuit;inline;
        function FD: TCIMExporter;inline;
    end;

function TCIMExporterHelper.FD: TCIMExporter;
begin
    Result := self;
end;

function TCIMExporterHelper.ActiveCircuit: TDSSCircuit;
begin
    Result := DSS.ActiveCircuit;
end;

procedure TCIMExporter.WriteCimLn(prf: ProfileChoice; const s: String);
begin
    if Separate then
    begin
        if prf <> FunPrf then
        begin
            if length(roots[prf]) < 1 then
            begin
                StartFreeInstance(prf, roots[FunPrf], ids[FunPrf]);
            end;
        end;
        case prf of
            FunPrf:
                FSWriteLn(F_FUN, s);
            EpPrf:
                FSWriteLn(F_EP, s);
            GeoPrf:
                FSWriteLn(F_GEO, s);
            TopoPrf:
                FSWriteLn(F_TOPO, s);
            CatPrf:
                FSWriteLn(F_CAT, s);
            SshPrf:
                FSWriteLn(F_SSH, s);
            DynPrf:
                FSWriteLn(F_DYN, s);
        end;
    end
    else
    begin
        FSWriteLn(F_FUN, s)
    end;
end;

procedure TCIMExporter.StartInstance(prf: ProfileChoice; Root: String; Obj: TNamedObject);
begin
    if Separate then
    begin // must be first to avoid stack overflow in WriteCimLn
        roots[prf] := Root;
        ids[prf] := Obj.UUID;
    end;
    WriteCimLn(prf, Format('<cim:%s rdf:ID="%s">', [Root, Obj.CIM_ID]));
    WriteCimLn(prf, Format('  <cim:IdentifiedObject.mRID>%s</cim:IdentifiedObject.mRID>', [Obj.CIM_ID]));
    WriteCimLn(prf, Format('  <cim:IdentifiedObject.name>%s</cim:IdentifiedObject.name>', [Obj.localName]));
end;

procedure TCIMExporter.StartFreeInstance(prf: ProfileChoice; Root: String; uuid: TUUID);
begin
    if Separate then
    begin // must be first to avoid stack overflow in WriteCimLn
        roots[prf] := Root;
        ids[prf] := uuid;
    end;
    WriteCimLn(prf, Format('<cim:%s rdf:ID="%s">', [Root, UUIDToCIMString(uuid)]));
end;

procedure TCIMExporter.EndInstance(prf: ProfileChoice; Root: String);
var
    i: ProfileChoice;
begin
    if Separate then
    begin
        for i := Low(ProfileChoice) to High(ProfileChoice) do
        begin
            if length(roots[i]) > 0 then
            begin
                WriteCimLn(i, Format('</cim:%s>', [Root]));
                roots[i] := ''
            end;
        end
    end
    else
        WriteCimLn(prf, Format('</cim:%s>', [Root]));
end;

procedure TCIMExporterHelper.ParseSwitchClass(pLine: TLineObj; var swtCls: String; var ratedAmps, breakingAmps: Double);
var
    pFuse: TFuseObj;
    pRelay: TRelayObj;
    pRecloser: TRecloserObj;
begin
    swtCls := 'LoadBreakSwitch';
    ratedAmps := pLine.NormAmps;
    breakingAmps := ratedAmps;
    pFuse := ActiveCircuit.Fuses.First;
    while (pFuse <> NIL) do
    begin
        if pFuse.ControlledElement = pLine then
        begin
            swtCls := 'Fuse';
            ratedAmps := pFuse.RatedCurrent;
            breakingAmps := 0.0;
            exit;
        end;
        pFuse := ActiveCircuit.Fuses.Next;
    end;
    pRelay := ActiveCircuit.Relays.First;
    while (pRelay <> NIL) do
    begin
        if pRelay.ControlledElement = pLine then
        begin
            swtCls := 'Breaker';
            exit;
        end;
        pRelay := ActiveCircuit.Relays.Next;
    end;
    pRecloser := ActiveCircuit.Reclosers.First;
    while (pRecloser <> NIL) do
    begin
        if pRecloser.ControlledElement = pLine then
        begin
            swtCls := 'Recloser';
            exit;
        end;
        pRecloser := ActiveCircuit.Reclosers.Next;
    end;
end;

// this returns s1, s2, or a combination of ABCN
function TCIMExporterHelper.PhaseString(pElem: TDSSCktElement; bus: Integer; bAllowSec: Boolean = True): String; // if order doesn't matter
var
    val, phs: String;
    dot: Integer;
    bSec: Boolean;
begin
    phs := pElem.FirstBus;
    for dot := 2 to bus do
        phs := pElem.NextBus;
    bSec := FALSE;
    if bAllowSec then 
    begin
        if pElem.NPhases = 2 then
            if ActiveCircuit.Buses^[pElem.Terminals[bus - 1].BusRef].kVBase < 0.25 then
                bSec := TRUE;
        if pElem.NPhases = 1 then
            if ActiveCircuit.Buses^[pElem.Terminals[bus - 1].BusRef].kVBase < 0.13 then
                bSec := TRUE;
    end;
    dot := pos('.', phs);
    if dot < 1 then
    begin
        val := 'ABC';
    end
    else
    begin
        phs := Copy(phs, dot + 1, Length(phs));
        if Pos('3', phs) > 0 then
            bSec := FALSE; // i.e. it's a three-phase secondary, not split-phase
        if bSec then
        begin
            if Pos('1', phs) > 0 then
            begin
                val := 's1';
                if Pos('2', phs) > 0 then
                    val := val + '2';
            end
            else
            if Pos('2', phs) > 0 then
                val := 's2';
        end
        else
        begin
            val := '';
            if Pos('1', phs) > 0 then
                val := val + 'A';
            if Pos('2', phs) > 0 then
                val := val + 'B';
            if Pos('3', phs) > 0 then
                val := val + 'C';
            if Pos('4', phs) > 0 then
                val := val + 'N';
        end;
    end;
    Result := val;
end;

function TCIMExporterHelper.PhaseOrderString(pElem: TDSSCktElement; bus: Integer; bAllowSec: Boolean = True): String; // for transposition
var
    phs: String;
    dot: Integer;
    bSec: Boolean;
begin
    phs := pElem.FirstBus;
    for dot := 2 to bus do
        phs := pElem.NextBus;

    bSec := false;
    if bAllowSec then 
    begin
        if pElem.NPhases = 2 then
            if ActiveCircuit.Buses^[pElem.Terminals[bus - 1].BusRef].kVBase < 0.25 then 
                bSec := true;
        if pElem.NPhases = 1 then
            if ActiveCircuit.Buses^[pElem.Terminals[bus - 1].BusRef].kVBase < 0.13 then 
                bSec := true;
    end;

    dot := pos('.', phs);
    if dot < 1 then
    begin
        Result := 'ABC';
    end
    else
    begin
        phs := Copy(phs, dot + 1, Length(phs));
        if Pos ('3', phs) > 0 then 
            bSec := false; // i.e. it's a three-phase secondary, not split-phase
        if bSec then 
        begin
            if Pos ('1', phs) > 0 then 
            begin
                Result := 's1';
                if Pos ('2', phs) > 0 then 
                    Result := Result + '2';
            end 
            else 
            if Pos ('2', phs) > 0 then 
                Result := 's2';
        end 
        else
        if Pos('1.2.3', phs) > 0 then
            Result := 'ABC'
        else
        if Pos('1.3.2', phs) > 0 then
            Result := 'ACB'
        else
        if Pos('2.3.1', phs) > 0 then
            Result := 'BCA'
        else
        if Pos('2.1.3', phs) > 0 then
            Result := 'BAC'
        else
        if Pos('3.2.1', phs) > 0 then
            Result := 'CBA'
        else
        if Pos('3.1.2', phs) > 0 then
            Result := 'CAB'
        else
        if Pos('1.2', phs) > 0 then
            Result := 'AB'
        else
        if Pos('1.3', phs) > 0 then
            Result := 'AC'
        else
        if Pos('2.3', phs) > 0 then
            Result := 'BC'
        else
        if Pos('2.1', phs) > 0 then
            Result := 'BA'
        else
        if Pos('3.2', phs) > 0 then
            Result := 'CB'
        else
        if Pos('3.1', phs) > 0 then
            Result := 'CA'
        else
        if Pos('1', phs) > 0 then
            Result := 'A'
        else
        if Pos('2', phs) > 0 then
            Result := 'B'
        else
            Result := 'C';
    end;
end;

function DeltaPhaseString(pElem: TDSSCktElement): String;
var
    phs: String;
    dot: Integer;
begin
    phs := pElem.FirstBus;

    dot := pos('.', phs);
    if (dot < 1) or (pElem.NPhases = 3) then
    begin
        Result := 'ABC'; // if Nphases < 3 this would be a user input error
    end
    else
    begin
        phs := Copy(phs, dot + 1, Length(phs));
        if pElem.NPhases = 1 then
        begin
            if Pos('1.2', phs) > 0 then
                Result := 'A'
            else
            if Pos('2.1', phs) > 0 then
                Result := 'A'
            else
            if Pos('2.3', phs) > 0 then
                Result := 'B'
            else
            if Pos('3.2', phs) > 0 then
                Result := 'B'
            else
            if Pos('1.3', phs) > 0 then
                Result := 'C'
            else
            if Pos('3.1', phs) > 0 then
                Result := 'C'
        end
        else
        begin
            if Pos('1.2.3', phs) > 0 then
                Result := 'AB'
            else
            if Pos('1.3.2', phs) > 0 then
                Result := 'CB'
            else
            if Pos('2.1.3', phs) > 0 then
                Result := 'AC'
            else
            if Pos('2.3.1', phs) > 0 then
                Result := 'BC'
            else
            if Pos('3.1.2', phs) > 0 then
                Result := 'CA'
            else
            if Pos('3.2.1', phs) > 0 then
                Result := 'BA'
        end;
    end;
end;

{$R+}

constructor TCIMBankObject.Create(MaxWdg: Integer);
begin
    maxWindings := MaxWdg;
    nWindings := 0;
    bAuto := False;
    SetLength(connections, MaxWdg);
    SetLength(angles, MaxWdg);
    SetLength(phaseA, MaxWdg);
    SetLength(phaseB, MaxWdg);
    SetLength(phaseC, MaxWdg);
    SetLength(ground, MaxWdg);
    inherited Create('Bank');
end;

destructor TCIMBankObject.Destroy;
begin
    connections := NIL;
    angles := NIL;
    phaseA := NIL;
    phaseB := NIL;
    phaseC := NIL;
    ground := NIL;
    pd_unit := NIL;
    inherited Destroy;
end;

procedure TCIMBankObject.BuildVectorGroup;
var
    i: Integer;
begin
    if bAuto then 
    begin
        if nWindings < 3 then
            vectorGroup := 'YNa'
        else
            vectorGroup := 'YNad1';
        Exit;
    end;
    vectorGroup := '';
    i := 0; // dynamic arrays are zero-based
    while i < nWindings do
    begin
        if (phaseA[i] > 0) and (phaseB[i] > 0) and (phaseC[i] > 0) then
        begin
            if connections[i] > 0 then
                vectorGroup := vectorGroup + 'd'
            else
                vectorGroup := vectorGroup + 'y';
            if ground[i] > 0 then
                vectorGroup := vectorGroup + 'n';
            if angles[i] > 0 then
                vectorGroup := vectorGroup + IntToStr(angles[i])
        end
        else
            vectorGroup := vectorGroup + 'i';
        Inc(i)
    end;
    if Length(vectorGroup) > 0 then
        vectorGroup := AnsiUpperCase(LeftStr(vectorGroup, 1)) + RightStr(vectorGroup, Length(vectorGroup) - 1);
end;

procedure TCIMBankObject.AddTransformer(CE: TCIMExporter; pXf: TTransfObj);
var
    i: Integer;
    phs: String;
begin
    if pXf.NumWindings > nWindings then
        nWindings := pXf.NumWindings;

    pd_unit := pXf;
    for i := 1 to pXf.NumWindings do
    begin
        phs := CE.PhaseString(pXf, i);
        if Pos('A', phs) > 0 then
            phaseA[i - 1] := 1;
        if Pos('B', phs) > 0 then
            phaseB[i - 1] := 1;
        if Pos('C', phs) > 0 then
            phaseC[i - 1] := 1;
        connections[i - 1] := pXf.WdgConnection[i];
        if connections[i - 1] <> connections[0] then
            angles[i - 1] := 1;
        if (pXf.WdgRneutral[i] >= 0.0) or (pXf.WdgXneutral[i] > 0.0) then
            if connections[i - 1] < 1 then
                ground[i - 1] := 1;
    end;
end;

procedure TCIMBankObject.AddAutoTransformer(CE: TCIMExporter; pAuto: TAutoTransObj); // 3-phase, 2 or 3 windings
var
    i: integer;
begin
    if pAuto.NumWindings > nWindings then 
        nWindings := pAuto.NumWindings;
    
    bAuto := True;
    pd_unit := pAuto;
    for i:=1 to pAuto.NumWindings do 
    begin
        phaseA[i-1] := 1;
        phaseB[i-1] := 1;
        phaseC[i-1] := 1;
        connections[i-1] := pAuto.WdgConnection[i];
        if i = 2 then
            ground[i-1] := 1;
    end;
end;

constructor TCIMOpLimitObject.Create(norm, emerg: Double);
begin
    NormAmps := norm;
    EmergAmps := emerg;
    inherited Create('OpLimI');
end;

destructor TCIMOpLimitObject.Destroy;
begin
    inherited Destroy;
end;

// the CIM transformer model requires some identified objects that don't have
// a counterpart in the DSS named objects.  These include banks, windings, and
// winding info.  So we create temporary UUIDs on the fly, and use a hash list when we
// need the UUIDs for later reference
procedure TCIMExporter.StartUuidList(size: Integer);
begin
    if assigned(UuidList) then
        FreeUuidList;
    UuidHash := THashList.Create(size);
    SetLength(UuidList, size);
    SetLength(UuidKeyList, size);
end;

procedure TCIMExporterHelper.StartBankList(size: Integer);
begin
    BankHash := THashList.Create(size);
    SetLength(BankList, size);
end;

procedure TCIMExporterHelper.StartOpLimitList(size: Integer);
begin
    OpLimitHash := THashList.Create(size);
    SetLength(OpLimitList, size);
end;

procedure TCIMExporter.FreeUuidList;
begin
    UuidHash.Free;
    UuidList := NIL;
    UuidKeyList := NIL;
end;

procedure TCIMExporterHelper.FreeBankList;
begin
    BankHash.Free;
    BankList := NIL;
end;

procedure TCIMExporterHelper.FreeOpLimitList;
begin
    OpLimitHash.Free;
    OpLimitList := NIL;
end;

procedure TCIMExporterHelper.AddBank(pBank: TCIMBankObject);
var
    ref, size: Integer;
begin
    ref := BankHash.Add(pBank.localName);
    size := High(BankList) + 1;
    if ref > size then
        SetLength(BankList, 2 * size);
    BankList[ref - 1] := pBank;
end;

function TCIMExporterHelper.GetBank(sBank: String): TCIMBankObject;
var
    ref: Integer;
begin
    Result := NIL;
    ref := BankHash.Find(sBank);
    if ref > 0 then
        Result := BankList[ref - 1];
end;

procedure TCIMExporterHelper.AddOpLimit(pLimit: TCIMOpLimitObject);
var
    ref, size: Integer;
begin
    ref := OpLimitHash.Add(pLimit.localName);
    size := High(OpLimitList) + 1;
    if ref > size then
        SetLength(OpLimitList, 2 * size);
    OpLimitList[ref - 1] := pLimit;
end;

function TCIMExporterHelper.GetOpLimit(sLimit: String): TCIMOpLimitObject;
var
    ref: Integer;
begin
    Result := NIL;
    ref := OpLimitHash.Find(sLimit);
    if ref > 0 then
        Result := OpLimitList[ref - 1];
end;

function TCIMExporterHelper.GetHashedUuid(key: String): TUuid;
var
    ref: Integer;
    size: Integer;
begin
    ref := UuidHash.Find(key);
    if ref = 0 then
    begin
        ref := UuidHash.Add(key);
        CreateUUID4(Result);  // this should be the ONLY place to call CreateUUID4
        size := High(UuidList) + 1;
        if ref > size then
        begin
            SetLength(UuidList, 2 * (size + 1));
            SetLength(UuidKeyList, 2 * (size + 1));
        end;
        UuidList[ref - 1] := Result;
        UuidKeyList[ref - 1] := key
    end
    else
    begin
        Result := UuidList[ref - 1]
    end;
end;

procedure TCIMExporter.AddHashedUuid(key: String; UuidVal: String);
var
    ref: Integer;
    size: Integer;
begin
    ref := UuidHash.Find(key);
    if ref = 0 then
    begin
        ref := UuidHash.Add(key);
        size := High(UuidList) + 1;
        if ref > size then
        begin
            SetLength(UuidList, 2 * (size + 1));
            SetLength(UuidKeyList, 2 * (size + 1));
        end;
        UuidList[ref - 1] := StringToUuid(UuidVal);
        UuidKeyList[ref - 1] := key
    end
    else
    begin
        UuidList[ref - 1] := StringToUuid(UuidVal);
    end;
end;

// any temporary object (not managed by DSS) should have '=' prepended to the Name
function TCIMExporterHelper.GetDevUuid(which: UuidChoice; Name: String; Seq: Integer): TUuid;
var
    key: String;
begin
    case which of
        Bank:
            key := 'Bank=';
        Wdg:
            key := 'Wdg=';
        XfCore:
            key := 'XfCore=';
        XfMesh:
            key := 'XfMesh=';
        WdgInf:
            key := 'WdgInf=';
        ScTest:
            key := 'ScTest=';
        OcTest:
            key := 'OcTest=';
        BaseV:
            key := 'BaseV=';
        OpLimV:
            key := 'OpLimV=';
        OpLimI:
            key := 'OpLimI=';
        LinePhase:
            key := 'LinePhase=';
        LoadPhase:
            key := 'LoadPhase=';
        GenPhase:
            key := 'GenPhase=';
        SolarPhase:
            key := 'PVPhase=';
        BatteryPhase:
            key := 'BattPhase=';
        CapPhase:
            key := 'CapPhase=';
        XfLoc:
            key := 'XfLoc=';
        LoadLoc:
            key := 'LoadLoc=';
        LineLoc:
            key := 'LineLoc=';
        ReacLoc:
            key := 'ReacLoc=';
        CapLoc:
            key := 'CapLoc=';
        Topo:
            key := 'Topo=';
        SolarLoc:
            key := 'SolarLoc=';
        BatteryLoc:
            key := 'BatteryLoc=';
        LoadResp:
            key := 'LoadResp=';
        CIMVer:
            key := 'CIMVer=';
        ZData:
            key := 'ZData=';
        PosPt:
            key := 'PosPt=';
        CoordSys:
            key := 'CoordSys=';
        TopoIsland:
            key := 'TopoIsland=';
        OpLimT:
            key := 'OpLimT=';
        Station:
            key := 'Station=';
        GeoRgn:
            key := 'GeoRgn=';
        SubGeoRgn:
            key := 'SubGeoRgn=';
        FdrLoc:
            key := 'FdrLoc=';
        XfInfo:
            key := 'XfInfo=';
        OpLimAHi:
            key := 'OpLimAHi=';
        OpLimALo:
            key := 'OpLimALo=';
        OpLimBHi:
            key := 'OpLimBHi=';
        OpLimBLo:
            key := 'OpLimBLo=';
        MachLoc:
            key := 'MachLoc=';
        SrcLoc:
            key := 'SrcLoc=';
        PVPanels:
            key := 'PVPanels=';
        Battery:
            key := 'Battery=';
        TankInfo:
            key := 'TankInfo=';
        TapCtrl:
            key := 'TapCtrl=';
        PUZ:
            key := 'PUZ=';
        WirePos:
            key := 'WirePos=';
        NormAmps:
            key := 'NormAmps=';
        EmergAmps:
            key := 'EmergAmps=';
        I1547NameplateData:
            key := 'INameplate=';
        I1547NameplateDataApplied:
            key := 'IApplied=';
        I1547Signal:
            key := 'ISignal=';
        I1547VoltVar:
            key := 'IVVar=';
        I1547WattVar:
            key := 'IWVar=';
        I1547ConstPF:
            key := 'IPF=';
        I1547VoltWatt:
            key := 'IVWatt=';
        I1547ConstQ:
            key := 'IQ=';            
    end;
    key := key + Name + '=' + IntToStr(Seq);
    Result := GetHashedUuid(key);
end;

procedure TCIMExporter.DefaultCircuitUUIDs(var fdrID: TUuid; var subID: TUuid; var rgnID: TUuid; var subGeoID: TUuid);
begin
    if not assigned(uuidlist) then
        StartUuidList(ActiveCircuit.NumBuses + 2 * ActiveCircuit.NumDevices);
    fdrID := ActiveCircuit.UUID;
    subID := GetDevUuid(Station, 'Station', 1);
    rgnID := GetDevUuid(GeoRgn, 'GeoRgn', 1);
    subGeoID := GetDevUuid(SubGeoRgn, 'SubGeoRgn', 1);
end;

procedure TCIMExporter.WriteHashedUUIDs(F: TFileStream);
var
    i: Integer;
begin
    for i := 0 to High(UuidList) do
    begin
        if Length(UuidKeyList[i]) < 1 then
            break;
        FSWriteLn(F, Format('%s %s', [UuidKeyList[i], UUIDToString(UuidList[i])]));
    end;
end;

// terminals are uniquely identified by class (DSSObjType), plus name and sequence
function TCIMExporterHelper.GetTermUuid(pElem: TDSSCktElement; Seq: Integer): TUuid;
var
    key: String;
begin
    key := IntToStr(pElem.DSSObjType) + '=' + pElem.Name + '=' + IntToStr(Seq);
    Result := GetHashedUuid(key);
end;

{$R-}

function GetBaseVName(val: Double): String;
begin
//  Result := Format('BaseV_%.3f', [val]);
    Result := 'BaseV_' + FloatToStrF(val, ffFixed, 6, 4);
end;

function TCIMExporterHelper.GetBaseVUuid(val: Double): TUuid;
begin
    Result := GetDevUuid(BaseV, GetBaseVName(val), 1);
end;

function GetOpLimVName(val: Double): String;
begin
    Result := 'OpLimV_' + FloatToStrF(val, ffFixed, 6, 4);
end;

function TCIMExporterHelper.GetOpLimVUuid(val: Double): TUuid;
begin
    Result := GetDevUuid(OpLimV, GetOpLimVName(val), 1);
end;

function GetOpLimIName(norm, emerg: Double): String;
begin
    Result := 'OpLimI_' + FloatToStrF(norm, ffFixed, 6, 1) + '_' + FloatToStrF(emerg, ffFixed, 6, 1);
end;

function TCIMExporterHelper.GetOpLimIUuid(norm, emerg: Double): TUuid;
begin
    Result := GetDevUuid(OpLimI, GetOpLimIName(norm, emerg), 1);
end;

procedure TCIMExporterHelper.DoubleNode(prf: ProfileChoice; Node: String; val: Double);
begin
    FD.WriteCimLn(prf, Format('  <cim:%s>%.8g</cim:%s>', [Node, val, Node]));
end;

procedure TCIMExporterHelper.IntegerNode(prf: ProfileChoice; Node: String; val: Integer);
begin
    FD.WriteCimLn(prf, Format('  <cim:%s>%d</cim:%s>', [Node, val, Node]));
end;

procedure TCIMExporterHelper.BooleanNode(prf: ProfileChoice; Node: String; val: Boolean);
var
    i: String;
begin
    if val then
        i := 'true'
    else
        i := 'false';
    FD.WriteCimLn(prf, Format('  <cim:%s>%s</cim:%s>', [Node, i, Node]));
end;

procedure TCIMExporterHelper.RefNode(prf: ProfileChoice; Node: String; Obj: TNamedObject);
begin
    FD.WriteCimLn(prf, Format('  <cim:%s rdf:resource="#%s"/>', [Node, Obj.CIM_ID]));
end;

procedure TCIMExporterHelper.UuidNode(prf: ProfileChoice; Node: String; ID: TUuid);
begin
    FD.WriteCimLn(prf, Format('  <cim:%s rdf:resource="#%s"/>', [Node, UUIDToCIMString(ID)]));
end;

procedure TCIMExporterHelper.LineCodeRefNode(prf: ProfileChoice; List: TLineCode; Obj: TLineCodeObj);
begin
    FD.WriteCimLn(prf, Format('  <cim:ACLineSegment.PerLengthImpedance rdf:resource="#%s"/>', [Obj.CIM_ID]));
end;

procedure TCIMExporterHelper.LineSpacingRefNode(prf: ProfileChoice; Obj: TDSSObject);
begin
    FD.WriteCimLn(prf, Format('  <cim:ACLineSegment.WireSpacingInfo rdf:resource="#%s"/>', [Obj.CIM_ID]));
end;

procedure TCIMExporterHelper.PhaseWireRefNode(prf: ProfileChoice; Obj: TConductorDataObj);
begin
    FD.WriteCimLn(prf, Format('  <cim:ACLineSegmentPhase.WireInfo rdf:resource="#%s"/>', [Obj.CIM_ID]));
end;

procedure TCIMExporterHelper.CircuitNode(prf: ProfileChoice; Obj: TNamedObject);
begin
    FD.WriteCimLn(prf, Format('  <cim:Equipment.EquipmentContainer rdf:resource="#%s"/>', [Obj.CIM_ID]));
end;

function TCIMExporterHelper.FirstPhaseString(pElem: TDSSCktElement; bus: Integer): String;
var
    val: String;
begin
    val := PhaseString(pElem, bus);
    if val <> '' then
        Result := LeftStr(val, 1)
    else
        Result := 'A';
end;

procedure TCIMExporterHelper.GeneratorControlEnum(prf: ProfileChoice; val: String);
begin
    FD.WriteCimLn(prf, Format('  <cim:GeneratingUnit.genControlSource rdf:resource="%s#GeneratorControlSource.%s"/>',
        [CIM_NS, val]));
end;

procedure TCIMExporterHelper.BatteryStateEnum(prf: ProfileChoice; val: Integer);
var
    str: String;
begin
    str := 'waiting';
    if val = STORE_CHARGING then
        str := 'charging'
    else
    if val = STORE_DISCHARGING then
        str := 'discharging';
    FD.WriteCimLn(prf, Format('  <cim:BatteryUnit.batteryState rdf:resource="%s#BatteryStateKind.%s"/>',
        [CIM_NS, str]));
end;

procedure TCIMExporterHelper.SynchMachTypeEnum(prf: ProfileChoice; val: String);
begin
    FD.WriteCimLn(prf, Format('  <cim:SynchronousMachine.type rdf:resource="%s#SynchronousMachineType.%s"/>',
        [CIM_NS, val]));
end;

procedure TCIMExporterHelper.SynchMachModeEnum(prf: ProfileChoice; val: String);
begin
    FD.WriteCimLn(prf, Format('  <cim:SynchronousMachine.operatingMode rdf:resource="%s#SynchronousMachineOperatingMode.%s"/>',
        [CIM_NS, val]));
end;

procedure TCIMExporterHelper.RegulatingControlEnum(prf: ProfileChoice; val: String);
begin
    FD.WriteCimLn(prf, Format('  <cim:RegulatingControl.mode rdf:resource="%s#RegulatingControlModeKind.%s"/>',
        [CIM_NS, val]));
end;

procedure TCIMExporterHelper.WindingConnectionEnum(prf: ProfileChoice; val: String);
begin
    FD.WriteCimLn(prf, Format('  <cim:TransformerEndInfo.connectionKind rdf:resource="%s#WindingConnection.%s"/>',
        [CIM_NS, val]));
end;

procedure TCIMExporterHelper.ConductorInsulationEnum(prf: ProfileChoice; val: String);
begin
    FD.WriteCimLn(prf, Format('  <cim:WireInfo.insulationMaterial rdf:resource="%s#WireInsulationKind.%s"/>',
        [CIM_NS, val]));
end;

procedure TCIMExporterHelper.ConductorUsageEnum(prf: ProfileChoice; val: String);
begin
    FD.WriteCimLn(prf, Format('  <cim:WireSpacingInfo.usage rdf:resource="%s#WireUsageKind.%s"/>',
        [CIM_NS, val]));
end;

procedure TCIMExporterHelper.CableShieldMaterialEnum(prf: ProfileChoice; val: String);
begin
//  FD.WriteCimLn (prf, Format ('  <cim:CableInfo.shieldMaterial rdf:resource="%s#CableShieldMaterialKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure TCIMExporterHelper.ConductorMaterialEnum(prf: ProfileChoice; val: String);
begin
//  FD.WriteCimLn (prf, Format ('  <cim:WireInfo.material rdf:resource="%s#WireMaterialKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure TCIMExporterHelper.CableOuterJacketEnum(prf: ProfileChoice; val: String);
begin
//  FD.WriteCimLn (prf, Format ('  <cim:CableInfo.outerJacketKind rdf:resource="%s#CableOuterJacketKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure TCIMExporterHelper.CableConstructionEnum(prf: ProfileChoice; val: String);
begin
//  FD.WriteCimLn (prf, Format ('  <cim:CableInfo.constructionKind rdf:resource="%s#CableConstructionKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure TCIMExporterHelper.TransformerControlEnum(prf: ProfileChoice; val: String);
begin
    FD.WriteCimLn(prf, Format('  <cim:RatioTapChanger.tculControlMode rdf:resource="%s#TransformerControlMode.%s"/>',
        [CIM_NS, val]));
end;

procedure TCIMExporterHelper.MonitoredPhaseNode(prf: ProfileChoice; val: String);
begin
    FD.WriteCimLn(prf, Format('  <cim:RegulatingControl.monitoredPhase rdf:resource="%s#PhaseCode.%s"/>',
        [CIM_NS, val]));
end;

procedure TCIMExporterHelper.OpLimitDirectionEnum(prf: ProfileChoice; val: String);
begin
    FD.WriteCimLn(prf, Format('  <cim:OperationalLimitType.direction rdf:resource="%s#OperationalLimitDirectionKind.%s"/>',
        [CIM_NS, val]));
end;

// next several for DERIEEEType1 CIM dynamics
procedure TCIMExporterHelper.NormalOpCatEnum (prf: ProfileChoice; val: String);
begin
    FD.WriteCimLn (prf, Format ('  <cim:DERNameplateData.normalOPcatKind rdf:resource="%s#NormalOPcatKind.%s"/>',
        [CIM_NS, val]));
end;

// procedure TCIMExporterHelper.SupportedModesEnum (prf: ProfileChoice; val: String);
// begin
//     FD.WriteCimLn (prf, Format ('  <cim:DERNameplateData.supportedModesKind rdf:resource="%s#SupportedModesKind.%s"/>',
//         [CIM_NS, val]));
// end;

procedure TCIMExporterHelper.PowerFactorExcitationEnum (prf: ProfileChoice; val: String);
begin
    FD.WriteCimLn (prf, Format ('  <cim:ConstantPowerFactorSettings.constantPowerFactorExcitationKind rdf:resource="%s#ConstantPowerFactorSettingKind.%s"/>',
        [CIM_NS, val]));
end;

procedure TCIMExporterHelper.RemoteInputSignalEnum (prf: ProfileChoice; val: String);
begin
    FD.WriteCimLn (prf, Format ('  <cim:RemoteInputSignal.remoteSignalType rdf:resource="%s#RemoteSignalKind.%s"/>',
        [CIM_NS, val]));
end;
// end of Enums for DERIEEEType1 CIM dynamics

procedure TCIMExporterHelper.StringNode(prf: ProfileChoice; Node: String; val: String);
begin
    FD.WriteCimLn(prf, Format('  <cim:%s>%s</cim:%s>', [Node, val, Node]));
end;

procedure TCIMExporterHelper.StartInstance(prf: ProfileChoice; Root: String; Obj: TNamedObject);
begin
    FD.StartInstance(prf, Root, Obj);
end;

procedure TCIMExporterHelper.StartFreeInstance(prf: ProfileChoice; Root: String; uuid: TUUID);
begin
    FD.StartFreeInstance(prf, Root, uuid);
end;

procedure TCIMExporterHelper.EndInstance(prf: ProfileChoice; Root: String);
begin
    FD.EndInstance(prf, Root);
end;

procedure TCIMExporterHelper.XfmrTankPhasesAndGround(fprf: ProfileChoice; eprf: ProfileChoice; pXf:TTransfObj; bus: Integer);
var
    ordered_phs, phs: String;
    j1, j2: Integer;
    reverse_ground, wye_ground, delta, wye_unground: Boolean;
begin
    j1 := (bus-1) * pXf.NConds + 1;
    j2 := j1 + pXf.Nphases;
    reverse_ground := False;
    wye_ground := False;
    wye_unground := False;
    delta := False;    
    //  writeln(Format('  Testing %d and %d', [j1, j2]));
    if (pXf.Winding^[bus].Connection = 1) then 
    begin // delta
        BooleanNode (fprf, 'TransformerEnd.grounded', false);
        delta := True;
    end 
    else 
    if (pXf.NodeRef^[j2] = 0) then 
    begin // last conductor is grounded solidly
        BooleanNode (FunPrf, 'TransformerEnd.grounded', true);
        DoubleNode (EpPrf, 'TransformerEnd.rground', 0.0);
        DoubleNode (EpPrf, 'TransformerEnd.xground', 0.0);
        wye_ground := True;
    end 
    else 
    if (pXf.NodeRef^[j1] = 0) then 
    begin // first conductor is grounded solidly, but should be reversed
        BooleanNode (FunPrf, 'TransformerEnd.grounded', true);
        DoubleNode (EpPrf, 'TransformerEnd.rground', 0.0);
        DoubleNode (EpPrf, 'TransformerEnd.xground', 0.0);
        reverse_ground := True;
    end 
    else 
    if (pXf.Winding^[bus].Rneut < 0.0) then 
    begin // probably wye ungrounded
        BooleanNode (FunPrf, 'TransformerEnd.grounded', false);
        wye_unground := True;
    end 
    else 
    begin // not delta, not wye solidly grounded or ungrounded
        BooleanNode (FunPrf, 'TransformerEnd.grounded', true);
        DoubleNode (EpPrf, 'TransformerEnd.rground', pXf.Winding^[bus].Rneut);
        DoubleNode (EpPrf, 'TransformerEnd.xground', pXf.Winding^[bus].Xneut);
    end;
    
    ordered_phs := PhaseOrderString(pXf, bus);
    if (ordered_phs = 's1') then
        ordered_phs := 's1N'
    else if (ordered_phs = 's2') then
        ordered_phs := 'Ns2'
    else if reverse_ground then
        ordered_phs := 'N' + ordered_phs
    else if wye_ground then
        ordered_phs := ordered_phs + 'N'
    else if wye_unground then
        ordered_phs := ordered_phs + 'N';

    FD.WriteCimLn (fprf, Format(
        '  <cim:TransformerTankEnd.orderedPhases rdf:resource="%s#OrderedPhaseCodeKind.%s"/>',
        [CIM_NS, ordered_phs]
    ));
end;

procedure TCIMExporterHelper.PhaseNode(prf: ProfileChoice; Root: String; val: String);
begin
    FD.WriteCimLn(prf, Format('  <cim:%s.phase rdf:resource="%s#PhaseCode.%s"/>',
        [Root, CIM_NS, val]));
end;

procedure TCIMExporterHelper.PhaseKindNode(prf: ProfileChoice; Root: String; val: String);
begin
    FD.WriteCimLn(prf, Format('  <cim:%s.phase rdf:resource="%s#SinglePhaseKind.%s"/>',
        [Root, CIM_NS, val]));
end;

procedure TCIMExporterHelper.PhaseSideNode(prf: ProfileChoice; Root: String; Side: Integer; val: String);
begin
    FD.WriteCimLn(prf, Format('  <cim:%s.phaseSide%d rdf:resource="%s#SinglePhaseKind.%s"/>',
        [Root, Side, CIM_NS, val]));
end;

procedure TCIMExporterHelper.ShuntConnectionKindNode(prf: ProfileChoice; Root: String; val: String); // D, Y, Yn, I
begin
    FD.WriteCimLn(prf, Format('  <cim:%s.phaseConnection rdf:resource="%s#PhaseShuntConnectionKind.%s"/>',
        [Root, CIM_NS, val]));
end;

procedure TCIMExporterHelper.WindingConnectionKindNode(prf: ProfileChoice; val: String); // D, Y, Z, Yn, Zn, A, I
begin
    FD.WriteCimLn(prf, Format('  <cim:PowerTransformerEnd.connectionKind rdf:resource="%s#WindingConnection.%s"/>',
        [CIM_NS, val]));
end;

// we specify phases except for balanced three-phase
procedure TCIMExporterHelper.AttachLinePhases(pLine: TLineObj);
var
    s, phs: String;
    i: Integer;
    pPhase: TNamedObject;
begin
    pPhase := TNamedObject.Create('dummy');
    s := PhaseString(pLine, 1);
    if pLine.NumConductorsAvailable > length(s) then
        s := s + 'N'; // so we can specify the neutral conductor
    for i := 1 to length(s) do
    begin
        phs := s[i];
        if phs = 's' then
            continue;
        if phs = '1' then
            phs := 's1';
        if phs = '2' then
            phs := 's2';
        pPhase.LocalName := pLine.Name + '_' + phs;
        pPhase.UUID := GetDevUuid(LinePhase, pPhase.LocalName, 1);
        StartInstance(FunPrf, 'ACLineSegmentPhase', pPhase);
        PhaseKindNode(FunPrf, 'ACLineSegmentPhase', phs);
        IntegerNode(FunPrf, 'ACLineSegmentPhase.sequenceNumber', i);
        if i <= pLine.NumConductorsAvailable then
            PhaseWireRefNode(CatPrf, pLine.ConductorData[i]);
        RefNode(FunPrf, 'ACLineSegmentPhase.ACLineSegment', pLine);
        UuidNode(GeoPrf, 'PowerSystemResource.Location',
            GetDevUuid(LineLoc, pLine.Name, 1));
        EndInstance(FunPrf, 'ACLineSegmentPhase');
    end;
end;

procedure TCIMExporterHelper.AttachSwitchPhases(pLine: TLineObj);
var
    s1, s2, phs1, phs2: String;
    i: Integer;
    pPhase: TNamedObject;
begin
  // also write the switch phases if needed to support transpositions
    s1 := PhaseOrderString(pLine, 1);
    s2 := PhaseOrderString(pLine, 2);
    if (pLine.NPhases = 3) and (length(s1) = 3) and (s1 = s2) then
        exit;
    pPhase := TNamedObject.Create('dummy');
    for i := 1 to length(s1) do
    begin
        phs1 := s1[i];
        phs2 := s2[i];
        if phs1 = 's' then
            continue;
        if phs2 = 's' then
            continue;
        if phs1 = '1' then
            phs1 := 's1';
        if phs1 = '2' then
            phs1 := 's2';
        if phs2 = '1' then
            phs2 := 's1';
        if phs2 = '2' then 
            phs2 := 's2';
        pPhase.LocalName := pLine.Name + '_' + phs1;
        pPhase.UUID := GetDevUuid(LinePhase, pPhase.LocalName, 1);
        StartInstance(FunPrf, 'SwitchPhase', pPhase);
        BooleanNode(SshPrf, 'SwitchPhase.closed', pLine.Closed[0]);
        BooleanNode(FunPrf, 'SwitchPhase.normalOpen', not pLine.Closed[0]);
        PhaseSideNode(FunPrf, 'SwitchPhase', 1, phs1);
        PhaseSideNode(FunPrf, 'SwitchPhase', 2, phs2);
        RefNode(FunPrf, 'SwitchPhase.Switch', pLine);
        UuidNode(GeoPrf, 'PowerSystemResource.Location', GetDevUuid(LineLoc, pLine.Name, 1));
        EndInstance(FunPrf, 'SwitchPhase');
    end;
end;

procedure TCIMExporterHelper.AttachCapPhases(pCap: TCapacitorObj; geoUUID: TUuid; sections: double);
var
    s, phs: String;
    i: Integer;
    pPhase: TNamedObject;
    bph: Double;
begin
    if pCap.NPhases = 3 then
        exit;
    pPhase := TNamedObject.Create('dummy');
    s := PhaseString(pCap, 1);
    with pCap do
    begin
        bph := 0.001 * Totalkvar / NomKV / NomKV / NumSteps / NPhases;
        if (Connection = TCapacitorConnection.Delta) then
            s := DeltaPhaseString(pCap);
    end;
    for i := 1 to length(s) do
    begin
        phs := s[i];
        pPhase.LocalName := pCap.Name + '_' + phs;
        pPhase.UUID := GetDevUuid(CapPhase, pPhase.LocalName, 1);
        StartInstance(FunPrf, 'LinearShuntCompensatorPhase', pPhase);
        PhaseKindNode(FunPrf, 'ShuntCompensatorPhase', phs);
        DoubleNode(EpPrf, 'LinearShuntCompensatorPhase.bPerSection', bph);
        DoubleNode(EpPrf, 'LinearShuntCompensatorPhase.gPerSection', 0.0);
        IntegerNode(EpPrf, 'ShuntCompensatorPhase.normalSections', pCap.NumSteps);
        IntegerNode(EpPrf, 'ShuntCompensatorPhase.maximumSections', pCap.NumSteps);
        DoubleNode(SshPrf, 'ShuntCompensatorPhase.sections', sections);
        RefNode(FunPrf, 'ShuntCompensatorPhase.ShuntCompensator', pCap);
        UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
        EndInstance(FunPrf, 'LinearShuntCompensatorPhase');
    end;
end;

procedure TCIMExporterHelper.AttachSecondaryPhases(pLoad: TLoadObj; geoUUID: TUuid; pPhase: TNamedObject; p, q: Double; phs: String);
begin
    pPhase.LocalName := pLoad.Name + '_' + phs;
    pPhase.UUID := GetDevUuid(LoadPhase, pPhase.LocalName, 1);
    StartInstance(FunPrf, 'EnergyConsumerPhase', pPhase);
    PhaseKindNode(FunPrf, 'EnergyConsumerPhase', phs);
    DoubleNode(SshPrf, 'EnergyConsumerPhase.p', p);
    DoubleNode(SshPrf, 'EnergyConsumerPhase.q', q);
    RefNode(FunPrf, 'EnergyConsumerPhase.EnergyConsumer', pLoad);
    UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
    EndInstance(FunPrf, 'EnergyConsumerPhase');
end;

procedure TCIMExporterHelper.AttachLoadPhases(pLoad: TLoadObj; geoUUID: TUuid);
var
    s, phs: String;
    i: Integer;
    pPhase: TNamedObject;
    p, q: Double;
    bAllowSec: Boolean;
begin
    if pLoad.NPhases = 3 then
        exit;
    // TODO - use a more robust filter than pLoad.LoadClass, which is > 1 only for PNNL taxonomy imports
    bAllowSec := (pLoad.LoadClass <= 1);
    p := 1000.0 * pLoad.kWBase / pLoad.NPhases;
    q := 1000.0 * pLoad.kvarBase / pLoad.NPhases;
    if pLoad.Connection = TLoadConnection.Delta then
        s := DeltaPhaseString(pLoad)
    else
        s := PhaseString(pLoad, 1, bAllowSec);

    pPhase := TNamedObject.Create('dummy');
  // first, filter out what appear to be split secondary loads
  // these can be 2-phase loads (balanced) nominally 0.208 kV, or
  //  1-phase loads (possibly unbalanced) nominally 0.12 kV
  //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
    if (pLoad.kVLoadBase < 0.25) and bAllowSec then
    begin
        if pLoad.NPhases = 2 then
        begin
            AttachSecondaryPhases(pLoad, geoUUID, pPhase, p, q, 's1');
            AttachSecondaryPhases(pLoad, geoUUID, pPhase, p, q, 's2');
            exit;
        end
        else
        begin
            AttachSecondaryPhases(pLoad, geoUUID, pPhase, p, q, s);
            exit;
        end;
    end;

    for i := 1 to length(s) do
    begin
        phs := s[i];
        pPhase.LocalName := pLoad.Name + '_' + phs;
        pPhase.UUID := GetDevUuid(LoadPhase, pPhase.LocalName, 1);
        StartInstance(FunPrf, 'EnergyConsumerPhase', pPhase);
        PhaseKindNode(FunPrf, 'EnergyConsumerPhase', phs);
        DoubleNode(SshPrf, 'EnergyConsumerPhase.p', p);
        DoubleNode(SshPrf, 'EnergyConsumerPhase.q', q);
        RefNode(FunPrf, 'EnergyConsumerPhase.EnergyConsumer', pLoad);
        UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
        EndInstance(FunPrf, 'EnergyConsumerPhase');
    end;
end;

procedure TCIMExporterHelper.AttachSecondaryGenPhases(pGen: TGeneratorObj; geoUUID: TUuid; pPhase: TNamedObject; p, q: Double; phs: String);
begin
    pPhase.LocalName := pGen.Name + '_' + phs;
    pPhase.UUID := GetDevUuid(GenPhase, pPhase.LocalName, 1);
    StartInstance(FunPrf, 'SynchronousMachinePhase', pPhase);
    PhaseKindNode(FunPrf, 'SynchronousMachinePhase', phs);
    DoubleNode(SshPrf, 'SynchronousMachinePhase.p', p);
    DoubleNode(SshPrf, 'SynchronousMachinePhase.q', q);
    RefNode(FunPrf, 'SynchronousMachinePhase.SynchronousMachine', pGen);
    UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
    EndInstance(FunPrf, 'SynchronousMachinePhase');
end;

procedure TCIMExporterHelper.AttachGeneratorPhases(pGen: TGeneratorObj; geoUUID: TUuid);
var
    s, phs: String;
    i: Integer;
    pPhase: TNamedObject;
    p, q: Double;
begin
    if pGen.NPhases = 3 then
        exit;
    p := 1000.0 * pGen.Presentkw / pGen.NPhases;
    q := 1000.0 * pGen.Presentkvar / pGen.NPhases;
    if pGen.Connection = 1 then
        s := DeltaPhaseString(pGen)
    else
        s := PhaseString(pGen, 1);

    pPhase := TNamedObject.Create('dummy');
  //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
    if pGen.Presentkv < 0.25 then
    begin
        if pGen.NPhases = 2 then
        begin
            AttachSecondaryGenPhases(pGen, geoUUID, pPhase, p, q, 's1');
            AttachSecondaryGenPhases(pGen, geoUUID, pPhase, p, q, 's2');
            exit;
        end
        else
        begin
            AttachSecondaryGenPhases(pGen, geoUUID, pPhase, p, q, s);
            exit;
        end;
    end;

    for i := 1 to length(s) do
    begin
        phs := s[i];
        pPhase.LocalName := pGen.Name + '_' + phs;
        pPhase.UUID := GetDevUuid(GenPhase, pPhase.LocalName, 1);
        StartInstance(FunPrf, 'SynchronousMachinePhase', pPhase);
        PhaseKindNode(FunPrf, 'SynchronousMachinePhase', phs);
        DoubleNode(SshPrf, 'SynchronousMachinePhase.p', p);
        DoubleNode(SshPrf, 'SynchronousMachinePhase.q', q);
        RefNode(FunPrf, 'SynchronousMachinePhase.SynchronousMachine', pGen);
        UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
        EndInstance(FunPrf, 'SynchronousMachinePhase');
    end;
end;

procedure TCIMExporterHelper.AttachSecondarySolarPhases(pPV: TPVSystem2Obj; geoUUID: TUuid; pPhase: TNamedObject; p, q: Double; phs: String);
begin
    pPhase.LocalName := pPV.Name + '_' + phs;
    pPhase.UUID := GetDevUuid(SolarPhase, pPhase.LocalName, 1);
    StartInstance(FunPrf, 'PowerElectronicsConnectionPhase', pPhase);
    PhaseKindNode(FunPrf, 'PowerElectronicsConnectionPhase', phs);
    DoubleNode(SshPrf, 'PowerElectronicsConnectionPhase.p', p);
    DoubleNode(SshPrf, 'PowerElectronicsConnectionPhase.q', q);
    RefNode(FunPrf, 'PowerElectronicsConnectionPhase.PowerElectronicsConnection', pPV);
    UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
    EndInstance(FunPrf, 'PowerElectronicsConnectionPhase');
end;

procedure TCIMExporterHelper.AttachSolarPhases(pPV: TPVSystem2Obj; geoUUID: TUuid);
var
    s, phs: String;
    i: Integer;
    pPhase: TNamedObject;
    p, q: Double;
begin
    if pPV.NPhases = 3 then
        exit;
    p := 1000.0 * pPV.Presentkw / pPV.NPhases;
    q := 1000.0 * pPV.Presentkvar / pPV.NPhases;
    if pPV.Connection = 1 then
        s := DeltaPhaseString(pPV)
    else
        s := PhaseString(pPV, 1);

    pPhase := TNamedObject.Create('dummy');
  //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
    if pPV.Presentkv < 0.25 then
    begin
        if pPV.NPhases = 2 then
        begin
            AttachSecondarySolarPhases(pPV, geoUUID, pPhase, p, q, 's1');
            AttachSecondarySolarPhases(pPV, geoUUID, pPhase, p, q, 's2');
            exit;
        end
        else
        begin
            AttachSecondarySolarPhases(pPV, geoUUID, pPhase, p, q, s);
            exit;
        end;
    end;

    for i := 1 to length(s) do
    begin
        phs := s[i];
        pPhase.LocalName := pPV.Name + '_' + phs;
        pPhase.UUID := GetDevUuid(SolarPhase, pPhase.LocalName, 1);
        StartInstance(FunPrf, 'PowerElectronicsConnectionPhase', pPhase);
        PhaseKindNode(FunPrf, 'PowerElectronicsConnectionPhase', phs);
        DoubleNode(SshPrf, 'PowerElectronicsConnectionPhase.p', p);
        DoubleNode(SshPrf, 'PowerElectronicsConnectionPhase.q', q);
        RefNode(FunPrf, 'PowerElectronicsConnectionPhase.PowerElectronicsConnection', pPV);
        UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
        EndInstance(FunPrf, 'PowerElectronicsConnectionPhase');
    end;
end;

procedure TCIMExporterHelper.AttachSecondaryStoragePhases(pBat: TStorage2Obj; geoUUID: TUuid; pPhase: TNamedObject; p, q: Double; phs: String);
begin
    pPhase.LocalName := pBat.Name + '_' + phs;
    pPhase.UUID := GetDevUuid(BatteryPhase, pPhase.LocalName, 1);
    StartInstance(FunPrf, 'PowerElectronicsConnectionPhase', pPhase);
    PhaseKindNode(FunPrf, 'PowerElectronicsConnectionPhase', phs);
    DoubleNode(SshPrf, 'PowerElectronicsConnectionPhase.p', p);
    DoubleNode(SshPrf, 'PowerElectronicsConnectionPhase.q', q);
    RefNode(FunPrf, 'PowerElectronicsConnectionPhase.PowerElectronicsConnection', pBat);
    UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
    EndInstance(FunPrf, 'PowerElectronicsConnectionPhase');
end;

procedure TCIMExporterHelper.AttachStoragePhases(pBat: TStorage2Obj; geoUUID: TUuid);
var
    s, phs: String;
    i: Integer;
    pPhase: TNamedObject;
    p, q: Double;
begin
    if pBat.NPhases = 3 then
        exit;
    p := 1000.0 * pBat.Presentkw / pBat.NPhases;
    q := 1000.0 * pBat.Presentkvar / pBat.NPhases;
    if pBat.Connection = 1 then
        s := DeltaPhaseString(pBat)
    else
        s := PhaseString(pBat, 1);

    pPhase := TNamedObject.Create('dummy');
  //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
    if pBat.Presentkv < 0.25 then
    begin
        if pBat.NPhases = 2 then
        begin
            AttachSecondaryStoragePhases(pBat, geoUUID, pPhase, p, q, 's1');
            AttachSecondaryStoragePhases(pBat, geoUUID, pPhase, p, q, 's2');
            exit;
        end
        else
        begin
            AttachSecondaryStoragePhases(pBat, geoUUID, pPhase, p, q, s);
            exit;
        end;
    end;

    for i := 1 to length(s) do
    begin
        phs := s[i];
        pPhase.LocalName := pBat.Name + '_' + phs;
        pPhase.UUID := GetDevUuid(BatteryPhase, pPhase.LocalName, 1);
        StartInstance(FunPrf, 'PowerElectronicsConnectionPhase', pPhase);
        PhaseKindNode(FunPrf, 'PowerElectronicsConnectionPhase', phs);
        DoubleNode(SshPrf, 'PowerElectronicsConnectionPhase.p', p);
        DoubleNode(SshPrf, 'PowerElectronicsConnectionPhase.q', q);
        RefNode(FunPrf, 'PowerElectronicsConnectionPhase.PowerElectronicsConnection', pBat);
        UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
        EndInstance(FunPrf, 'PowerElectronicsConnectionPhase');
    end;
end;

procedure TCIMExporterHelper.WriteLoadModel(Name: String; ID: TUuid;
    zP: Double; iP: Double; pP: Double; zQ: Double; iQ: Double; pQ: Double;
    eP: Double; eQ: Double);
begin
    FD.WriteCimln(FunPrf, Format('<cim:LoadResponseCharacteristic rdf:ID="%s">', [UUIDToCIMString(ID)]));
    StringNode(FunPrf, 'IdentifiedObject.mRID', UUIDToCIMString(ID));
    StringNode(FunPrf, 'IdentifiedObject.name', Name);
    if (eP > 0.0) or (eQ > 0.0) then
        BooleanNode(FunPrf, 'LoadResponseCharacteristic.exponentModel', TRUE)
    else
        BooleanNode(FunPrf, 'LoadResponseCharacteristic.exponentModel', FALSE);

    DoubleNode(FunPrf, 'LoadResponseCharacteristic.pConstantImpedance', zP);
    DoubleNode(FunPrf, 'LoadResponseCharacteristic.pConstantCurrent', iP);
    DoubleNode(FunPrf, 'LoadResponseCharacteristic.pConstantPower', pP);

    DoubleNode(FunPrf, 'LoadResponseCharacteristic.qConstantImpedance', zQ);
    DoubleNode(FunPrf, 'LoadResponseCharacteristic.qConstantCurrent', iQ);
    DoubleNode(FunPrf, 'LoadResponseCharacteristic.qConstantPower', pQ);

    DoubleNode(FunPrf, 'LoadResponseCharacteristic.pVoltageExponent', eP);
    DoubleNode(FunPrf, 'LoadResponseCharacteristic.qVoltageExponent', eQ);
    DoubleNode(FunPrf, 'LoadResponseCharacteristic.pFrequencyExponent', 0.0);
    DoubleNode(FunPrf, 'LoadResponseCharacteristic.qFrequencyExponent', 0.0);
    FD.WriteCimLn(FunPrf, '</cim:LoadResponseCharacteristic>');
end;

function IsGroundBus(const S: String): Boolean;
var
    i: Integer;
begin
    Result := TRUE;
    i := pos('.1', S);
    if i > 0 then
        Result := FALSE;
    i := pos('.2', S);
    if i > 0 then
        Result := FALSE;
    i := pos('.3', S);
    if i > 0 then
        Result := FALSE;
    i := pos('.', S);
    if i = 0 then
        Result := FALSE;
end;

procedure TCIMExporterHelper.WritePositions(pElem: TDSSCktElement; geoUUID: TUuid; crsUUID: TUuid);
var
    Nterm, j, ref: Integer;
    BusName: String;
begin
    Nterm := pElem.Nterms;
    BusName := pElem.FirstBus;
    StartFreeInstance(GeoPrf, 'Location', geoUUID);
    StringNode(GeoPrf, 'IdentifiedObject.mRID', UUIDToCIMString(geoUUID));
    StringNode(GeoPrf, 'IdentifiedObject.name', pElem.LocalName + '_Loc');
    UuidNode(GeoPrf, 'Location.CoordinateSystem', crsUUID);
    EndInstance(GeoPrf, 'Location');

    for j := 1 to NTerm do
    begin
        if IsGroundBus(BusName) = FALSE then
        begin
            ref := pElem.Terminals[j - 1].BusRef;
            StartFreeInstance(GeoPrf, 'PositionPoint', GetDevUuid(PosPt, pElem.ParentClass.Name + '.' + pElem.LocalName, j));
            UuidNode(GeoPrf, 'PositionPoint.Location', geoUUID);
            IntegerNode(GeoPrf, 'PositionPoint.sequenceNumber', j);
            StringNode(GeoPrf, 'PositionPoint.xPosition', FloatToStr(ActiveCircuit.Buses^[ref].x));
            StringNode(GeoPrf, 'PositionPoint.yPosition', FloatToStr(ActiveCircuit.Buses^[ref].y));
            EndInstance(GeoPrf, 'PositionPoint');
        end;
        BusName := pElem.Nextbus;
    end;
end;

procedure TCIMExporterHelper.WriteReferenceTerminals(pElem: TDSSCktElement; RefUuid: TUuid; norm: Double; emerg: Double);
var
    Nterm, j, ref: Integer;
    BusName, TermName, LimitName: String;
    TermUuid, LimiTUuid: TUuid;
    pLimit: TCIMOpLimitObject;
begin
    Nterm := pElem.Nterms;
    BusName := pElem.FirstBus;
    for j := 1 to NTerm do
    begin
        if IsGroundBus(BusName) = FALSE then
        begin
            ref := pElem.Terminals[j - 1].BusRef;
            TermName := pElem.Name + '_T' + IntToStr(j);
            TermUuid := GetTermUuid(pElem, j);
            StartFreeInstance(FunPrf, 'Terminal', TermUuid);
            StringNode(FunPrf, 'IdentifiedObject.mRID', UUIDToCIMString(TermUuid));
            StringNode(FunPrf, 'IdentifiedObject.name', TermName);
            UuidNode(FunPrf, 'Terminal.ConductingEquipment', RefUuid);
            IntegerNode(FunPrf, 'ACDCTerminal.sequenceNumber', j);
            FD.WriteCimLn(TopoPrf, Format('  <cim:Terminal.ConnectivityNode rdf:resource="#%s"/>',
                [ActiveCircuit.Buses[ref].CIM_ID]));
            if (j = 1) and (norm > 0.0) then
            begin
                if emerg < norm then
                    emerg := norm;
                LimitName := GetOpLimIName(norm, emerg);
                pLimit := GetOpLimit(LimitName);
                if pLimit = NIL then
                begin
                    pLimit := TCIMOpLimitObject.Create(norm, emerg);
                    pLimit.localName := LimitName;
                    pLimit.UUID := GetDevUuid(OpLimI, LimitName, 0);
                    AddOpLimit(pLimit);
                end;
                LimiTUuid := GetDevUuid(OpLimI, LimitName, 0);
                UuidNode(FunPrf, 'ACDCTerminal.OperationalLimitSet', LimiTUuid);
            end;
            EndInstance(FunPrf, 'Terminal');
        end;
        BusName := pElem.Nextbus;
    end;
end;

procedure TCIMExporterHelper.WriteTerminals(pElem: TDSSCktElement; geoUUID: TUuid; crsUUID: TUuid;
    norm: Double; emerg: Double);
begin
    WriteReferenceTerminals(pElem, pElem.UUID, norm, emerg);
    WritePositions(pElem, geoUUID, crsUUID);
end;

procedure TCIMExporterHelper.VbaseNode(prf: ProfileChoice; pElem: TDSSCktElement);
var
    j: Integer;
begin
    j := pElem.Terminals[0].BusRef;
    UuidNode(prf, 'ConductingEquipment.BaseVoltage',
        GetBaseVUuid(sqrt(3.0) * ActiveCircuit.Buses^[j].kVBase));
end;

procedure TCIMExporterHelper.WriteXfmrCode(pXfCd: TXfmrCodeObj);
var
    pName: TNamedObject;
    ratShort, ratEmerg, val, Zbase, pctIexc, r, x, TestKVA: Double;
    i, j, seq: Integer;
begin
    pName := TNamedObject.Create('dummy');
    with pXfCd do
    begin
        StartInstance(CatPrf, 'TransformerTankInfo', pXfCd);
        EndInstance(CatPrf, 'TransformerTankInfo');
        ratShort := NormMaxHKVA / Winding^[1].kva;
        ratEmerg := EmergMaxHKVA / Winding^[1].kva;
        for i := 1 to NumWindings do
        begin
            Zbase := Winding^[i].kvll;
            Zbase := 1000.0 * Zbase * Zbase / Winding^[1].kva;
            pName.localName := pXfCd.Name + '_' + IntToStr(i);
            pName.UUID := GetDevUuid(WdgInf, pXfCd.Name, i);
            StartInstance(CatPrf, 'TransformerEndInfo', pName);
            RefNode(CatPrf, 'TransformerEndInfo.TransformerTankInfo', pXfCd);
            IntegerNode(CatPrf, 'TransformerEndInfo.endNumber', i);
            if pXfCd.FNPhases < 3 then
            begin
                WindingConnectionEnum(CatPrf, 'I');
                if (i = 3) and (Winding^[i].kvll < 0.3) then // for center-tap secondary
                    IntegerNode(CatPrf, 'TransformerEndInfo.phaseAngleClock', 6)
                else
                    IntegerNode(CatPrf, 'TransformerEndInfo.phaseAngleClock', 0)
            end
            else
            begin
                if Winding^[i].Connection = 1 then
                    WindingConnectionEnum(CatPrf, 'D')
                else
                if (Winding^[i].Rneut > 0.0) or (Winding^[i].Xneut > 0.0) then
                    WindingConnectionEnum(CatPrf, 'Yn')
                else
                    WindingConnectionEnum(CatPrf, 'Y');
                if Winding^[i].Connection <> Winding^[1].Connection then
                    IntegerNode(CatPrf, 'TransformerEndInfo.phaseAngleClock', 1)
                else
                    IntegerNode(CatPrf, 'TransformerEndInfo.phaseAngleClock', 0);
            end;
            DoubleNode(CatPrf, 'TransformerEndInfo.ratedU', 1000 * Winding^[i].kvll);
            DoubleNode(CatPrf, 'TransformerEndInfo.ratedS', 1000 * Winding^[i].kva);
            DoubleNode(CatPrf, 'TransformerEndInfo.shortTermS', 1000 * Winding^[i].kva * ratShort);
            DoubleNode(CatPrf, 'TransformerEndInfo.emergencyS', 1000 * Winding^[i].kva * ratEmerg);
            DoubleNode(CatPrf, 'TransformerEndInfo.r', Winding^[i].Rpu * Zbase);
            DoubleNode(CatPrf, 'TransformerEndInfo.insulationU', 0.0);
            EndInstance(CatPrf, 'TransformerEndInfo');
        end;
        pName.localName := pXfCd.Name + '_' + IntToStr(1);
        pName.UUID := GetDevUuid(OcTest, pXfCd.Name, 1);
        StartInstance(CatPrf, 'NoLoadTest', pName);
        UuidNode(CatPrf, 'NoLoadTest.EnergisedEnd', GetDevUuid(WdgInf, pXfCd.Name, 1));
        DoubleNode(CatPrf, 'NoLoadTest.energisedEndVoltage', 1000.0 * Winding^[1].kvll);
        pctIexc := sqrt(pctImag * pctImag + pctNoLoadLoss * pctNoLoadLoss);
        DoubleNode(CatPrf, 'NoLoadTest.excitingCurrent', pctIexc);
        DoubleNode(CatPrf, 'NoLoadTest.excitingCurrentZero', pctIexc);
        val := 0.01 * pctNoLoadLoss * Winding^[1].kva; // losses to be in kW
        DoubleNode(CatPrf, 'NoLoadTest.loss', val);
        DoubleNode(CatPrf, 'NoLoadTest.lossZero', val);
        DoubleNode(CatPrf, 'TransformerTest.basePower', 1000.0 * Winding^[1].kva);
        DoubleNode(CatPrf, 'TransformerTest.temperature', 50.0);
        EndInstance(CatPrf, 'NoLoadTest');
        seq := 0;
        for i := 1 to NumWindings do
            for j := (i + 1) to NumWindings do
            begin
                Inc(seq);
                pName.localName := pXfCd.Name + '_' + IntToStr(seq);
                pName.UUID := GetDevUuid(ScTest, pXfCd.Name, seq);
                StartInstance(CatPrf, 'ShortCircuitTest', pName);
                UuidNode(CatPrf, 'ShortCircuitTest.EnergisedEnd', GetDevUuid(WdgInf, pXfCd.Name, i));
                // NOTE: can insert more than one GroundedEnds for three-winding short-circuit tests
                UuidNode(CatPrf, 'ShortCircuitTest.GroundedEnds', GetDevUuid(WdgInf, pXfCd.Name, j));
                IntegerNode(CatPrf, 'ShortCircuitTest.energisedEndStep', Winding^[i].NumTaps div 2);
                IntegerNode(CatPrf, 'ShortCircuitTest.groundedEndStep', Winding^[j].NumTaps div 2);
                TestKVA := Winding^[1].kva;
                Zbase := 1000.0 * Zbase * Zbase / TestKVA;  // all DSS impedances are on winding 1 kva base
                // windings are not overloaded during short-circuit tests, but in OpenDSS Sbase is on Winding 1 always
                x := Xsc^[seq];
                r := Winding^[i].Rpu + Winding^[j].Rpu;
                val := sqrt(r*r + x*x) * Zbase;
                DoubleNode(CatPrf, 'ShortCircuitTest.leakageImpedance', val);
                DoubleNode(CatPrf, 'ShortCircuitTest.leakageImpedanceZero', val);
                val := r * TestKVA;
                DoubleNode(CatPrf, 'ShortCircuitTest.loss', val);
                DoubleNode(CatPrf, 'ShortCircuitTest.lossZero', val);
                DoubleNode(CatPrf, 'TransformerTest.basePower', 1000.0 * TestKVA);
                DoubleNode(CatPrf, 'TransformerTest.temperature', 50.0);
                EndInstance(CatPrf, 'ShortCircuitTest');
            end;
    end;
    pName.Free;
end;

procedure TCIMExporterHelper.WriteCableData(pCab: TCableDataObj);
var
    v1: Double;
begin
    with pCab do
    begin
        v1 := To_Meters(RadiusUnits);
        BooleanNode(CatPrf, 'WireInfo.insulated', TRUE);
        DoubleNode(CatPrf, 'WireInfo.insulationThickness', v1 * pCab.InsLayer);
        ConductorInsulationEnum(CatPrf, 'crosslinkedPolyethylene'); // TODO -  code EpsR
        CableOuterJacketEnum(CatPrf, 'none');
        CableConstructionEnum(CatPrf, 'stranded');
        BooleanNode(CatPrf, 'CableInfo.isStrandFill', FALSE); // we don't really know this
        DoubleNode(CatPrf, 'CableInfo.diameterOverCore',
            v1 * (pCab.DiaIns - 2.0 * pCab.InsLayer));
        DoubleNode(CatPrf, 'CableInfo.diameterOverInsulation', v1 * pCab.DiaIns);
        DoubleNode(CatPrf, 'CableInfo.diameterOverJacket', v1 * pCab.DiaCable);
        DoubleNode(CatPrf, 'CableInfo.nominalTemperature', 90.0);  // we don't really know this
    end;
end;

procedure TCIMExporterHelper.WriteTapeData(pCab: TTSDataObj);
var
    v1: Double;
begin
    with pCab do
    begin
        v1 := To_Meters(RadiusUnits);
        DoubleNode(CatPrf, 'CableInfo.diameterOverScreen',
            v1 * (pCab.DiaShield - 2.0 * pCab.TapeLayer));
        DoubleNode(CatPrf, 'TapeShieldCableInfo.tapeLap', pCab.TapeLap);
        DoubleNode(CatPrf, 'TapeShieldCableInfo.tapeThickness', v1 * pCab.TapeLayer);
        CableShieldMaterialEnum(CatPrf, 'copper');
        BooleanNode(CatPrf, 'CableInfo.sheathAsNeutral', TRUE);
    end;
end;

procedure TCIMExporterHelper.WriteConcData(pCab: TCNDataObj);
var
    v1: Double;
begin
    with pCab do
    begin
        v1 := To_Meters(RadiusUnits);
        DoubleNode(CatPrf, 'CableInfo.diameterOverScreen',
            v1 * (pCab.DiaCable - 2.0 * pCab.DiaStrand));
        DoubleNode(CatPrf, 'ConcentricNeutralCableInfo.diameterOverNeutral',
            v1 * pCab.DiaCable);
        DoubleNode(CatPrf, 'ConcentricNeutralCableInfo.neutralStrandRadius',
            v1 * 0.5 * pCab.DiaStrand);
        DoubleNode(CatPrf, 'ConcentricNeutralCableInfo.neutralStrandGmr',
            v1 * pCab.GmrStrand);
        v1 := To_per_Meter(ResUnits);
        DoubleNode(CatPrf, 'ConcentricNeutralCableInfo.neutralStrandRDC20',
            v1 * pCab.RStrand);
        IntegerNode(CatPrf, 'ConcentricNeutralCableInfo.neutralStrandCount', pCab.NStrand);
        BooleanNode(CatPrf, 'CableInfo.sheathAsNeutral', FALSE);
    end;
end;

procedure TCIMExporterHelper.WriteWireData(pWire: TConductorDataObj);
var
    v1: Double;
begin
    with pWire do
    begin
        if DisplayName <> '' then
            StringNode(CatPrf, 'WireInfo.sizeDescription', DisplayName)
        else
            StringNode(CatPrf, 'WireInfo.sizeDescription', DSSClassName + '_' + Name);

        if CompareText(LeftStr(name, 2), 'AA') = 0 then
            ConductorMaterialEnum(CatPrf, 'aluminum')
        else
        if CompareText(LeftStr(name, 4), 'ACSR') = 0 then
            ConductorMaterialEnum(CatPrf, 'acsr')
        else
        if CompareText(LeftStr(name, 2), 'CU') = 0 then
            ConductorMaterialEnum(CatPrf, 'copper')
        else
        if CompareText(LeftStr(name, 3), 'EHS') = 0 then
            ConductorMaterialEnum(CatPrf, 'steel')
        else
            ConductorMaterialEnum(CatPrf, 'other');
        v1 := To_Meters(GMRUnits);
        DoubleNode(CatPrf, 'WireInfo.gmr', GMR * v1);
        v1 := To_Meters(RadiusUnits);
        DoubleNode(CatPrf, 'WireInfo.radius', Radius * v1);
        v1 := To_per_Meter(ResUnits);
        DoubleNode(CatPrf, 'WireInfo.rDC20', Rdc * v1);
        DoubleNode(CatPrf, 'WireInfo.rAC25', Rac * v1);
        DoubleNode(CatPrf, 'WireInfo.rAC50', Rac * v1);
        DoubleNode(CatPrf, 'WireInfo.rAC75', Rac * v1);
        DoubleNode(CatPrf, 'WireInfo.ratedCurrent', Max(NormAmps, 0.0));
        IntegerNode(CatPrf, 'WireInfo.strandCount', 0);
        IntegerNode(CatPrf, 'WireInfo.coreStrandCount', 0);
        DoubleNode(CatPrf, 'WireInfo.coreRadius', 0.0);
    end;
end;

///////// begin helper class for exporting IEEE 1547 model parameters /////////////

constructor TRemoteSignalObject.Create(exporter: TCIMExporter; aBusName: String; seq: Integer; invName: String);
begin
    ex := exporter;
    busName := aBusName;
    pElem := NIL;
    trm := -1;
    phase := 'A';
    inherited Create('ISignal');
    LocalName := invName + '_' + IntToStr(seq);
    UUID := ex.GetDevUUID(I1547Signal, LocalName, seq);
end;

destructor TRemoteSignalObject.Destroy;
begin
    inherited Destroy;
end;

function TIEEE1547Controller.CheckSignalMatch(sig: TRemoteSignalObject; pElm: TDSSCktElement; seq: Integer): Boolean;
var
    elmPhases, trmBus: String;
    dotpos: Integer;
begin
    Result := FALSE;
    trmBus := pElm.GetBus(seq);
    dotpos := ansipos('.', trmBus);
    if dotpos > 0 then
    begin
        trmBus := trmBus.Substring(0, dotpos - 1);
    end;

    if CompareText(sig.busName, trmBus) = 0 then
    begin
        elmPhases := ex.PhaseString(pElm, seq, TRUE);
        if Pos(sig.phase, elmPhases) > 0 then
        begin
            sig.trm := seq;
            sig.pElem := pElm;
            Result := TRUE;
        end
        else
        if (Pos('1', elmPhases) > 0) and (sig.phase = 'A') then
        begin  // switch to secondary phasing
            sig.trm := seq;
            sig.pElem := pElm;
            sig.phase := 's1';
            Result := TRUE;
        end
        else
        if (Pos('2', elmPhases) > 0) and (sig.phase = 'B') then
        begin  // switch to secondary phasing
            sig.trm := seq;
            sig.pElem := pElm;
            sig.phase := 's2';
            Result := TRUE;
        end;
    end;
end;

procedure TIEEE1547Controller.FindSignalTerminals;
var
    i, j, k, dotpos: Integer;
    bus, phase: String;
    elements: ArrayOfString;
    found: Boolean;
    pElem: TDSSCktElement;
begin
    if pMonBuses.Count < 1 then
    begin
        SetLength(Signals, 0);
        Exit;
    end;
    // create just one remote signal for the main bus, based on the first MonBus
    //  IEEE 1547 doesn't allow different main buses
    //  IEEE 1547 also specifies that the average (pos seq) of all applicable voltages be used
    SetLength(Signals, 1);

    for i := Low(Signals) to High(Signals) do
    begin
        bus := pMonBuses.Strings[i];
        Signals[i] := TRemoteSignalObject.Create(ex, bus, i + 1, pInvName.LocalName);
        dotpos := ansipos('.', bus); // removes the dot
        if dotpos > 0 then
        begin
            phase := bus.Substring(dotpos);
            if Pos('3', phase) > 0 then
                Signals[i].phase := 'C'
            else
            if Pos('2', phase) > 0 then
                Signals[i].phase := 'B'
            else
                Signals[i].phase := 'A';
            Signals[i].busName := bus.Substring(0, dotpos - 1);
        end
        else
        begin // this is a three-phase bus, which must be ABC, not s1 and/or s2
            Signals[i].phase := 'A'; // if user wants B and/or C as well, the MonBus input should have specified
        end;

        found := FALSE;
        with ex.ActiveCircuit do
        begin
            elements := getPDEatBus(Signals[i].busName);
            for j := Low(elements) to High(elements) do
            begin
                if found then
                    break;
                if SetElementActive(elements[j]) > 0 then
                begin
                    pElem := ActiveCktElement;
                    for k := 1 to pElem.NTerms do
                    begin
                        if CheckSignalMatch(Signals[i], pElem, k) then
                        begin
                            found := TRUE;
                            break;
                        end;
                    end;
                end;
            end;
            if not found then
            begin
                elements := getPCEatBus(bus);
                for j := Low(elements) to High(elements) do
                begin
                    if found then
                        break;
                    if SetElementActive(elements[j]) > 0 then
                    begin
                        pElem := ActiveCktElement;
                        for k := 1 to pElem.NTerms do
                        begin
                            if CheckSignalMatch(Signals[i], pElem, k) then
                            begin
                                found := TRUE;
                                break;
                            end;
                        end;
                    end;
                end;
            end;
        end;
    end;
end;

constructor TIEEE1547Controller.Create(exporter: TCIMExporter);
begin
    inherited Create;
    ex := exporter;
    SetDefaults(FALSE);
    pInvName := TNamedObject.Create('Inv');
    pPlateName := TNamedObject.Create('Nameplate');
    pSetName := TNamedObject.Create('Settings');
    pDERNames := TStringList.Create;
    pMonBuses := TStringList.Create;
    Signals := NIL;
end;

destructor TIEEE1547Controller.Destroy;
begin
    pInvName.Free;
    pPlateName.Free;
    pSetName.Free;
    pDERNames.Free;
    pMonBuses.Free;
    Signals := NIL;
    inherited Destroy;
end;

procedure TIEEE1547Controller.PullFromInvControl(pInv: TInvControl2Obj);
var
    xy: TXYcurveObj;
    bCatB, bValid, bSet1, bSet2, bSet3, bSet4, bSet5, bSet6: Boolean;
    mode, combi, i: Integer;
    v, p, q, qvslope: Double;
begin
    pInvName.LocalName := pInv.Name;
    pInvName.UUID := pInv.UUID;
    pDERNames.Assign(pInv.DERNameList);
    if pInv.MonBusesNameList.Count > 0 then
        pMonBuses.Assign(pInv.MonBusesNameList)
    else
        pMonBuses.Clear;

    bCatB := FALSE;
    xy := pInv.Fvvc_curve;
    if (xy <> NIL) then
    begin
        for i := 1 to xy.NumPoints do
        begin
            if xy.YValue_pt[i] < -CatBQmin then
            begin
                bCatB := TRUE;
                break;
            end;
        end;
    end;
    SetDefaults(bCatB);

    VV_olrt := pInv.LPFTau * 2.3026;
    VW_olrt := VV_olrt;

    if (xy <> NIL) then
    begin
        i := 1;
        bValid := FALSE;
        bSet1 := FALSE;
        bSet2 := FALSE;
        bSet3 := FALSE;
        bSet4 := FALSE;
        while i <= xy.NumPoints do
        begin
            v := xy.XValue_pt[i];
            if (v >= 0.77) and (v <= 1.25) then
                bValid := TRUE;
            if bValid then
            begin
                if not bSet1 then
                begin
                    VV_curveV1 := v;
                    VV_curveQ1 := xy.YValue_pt[i];
                    bSet1 := TRUE;
                end
                else
                if not bSet2 then
                begin
                    if v > 1.05 then
                    begin
                        VV_curveV2 := 1.0;
                        VV_curveQ2 := 0.0;
                        if v > 1.08 then
                        begin
                            VV_curveV3 := 1.0;
                            VV_curveQ3 := 0.0;
                            bSet3 := TRUE;
                            VV_curveV4 := v;
                            VV_curveQ4 := xy.YValue_pt[i];
                            bSet4 := TRUE;
                        end;
                    end
                    else
                    begin
                        VV_curveV2 := v;
                        VV_curveQ2 := xy.YValue_pt[i];
                    end;
                    bSet2 := TRUE;
                end
                else
                if not bSet3 then
                begin
                    VV_curveV3 := v;
                    VV_curveQ3 := xy.YValue_pt[i];
                    bSet3 := TRUE;
                end
                else
                if not bSet4 then
                begin
                    VV_curveV4 := v;
                    VV_curveQ4 := xy.YValue_pt[i];
                    bSet4 := TRUE;
                end;
            end;
            inc(i);
        end;
    end;

    xy := pInv.Fvoltwatt_curve;
    if (xy <> NIL) then
    begin
        i := 1;
        bValid := FALSE;
        bSet1 := FALSE;
        bSet2 := FALSE;
        while i <= xy.NumPoints do
        begin
            v := xy.XValue_pt[i];
            p := xy.YValue_pt[i];
            if (v >= 1.00) and (v <= 1.10) then // TODO: per standard, v should be >= 1.05 but we loosen that criteria for testing
                bValid := TRUE;
            if bValid then
            begin
                if not bSet1 then
                begin
                    VW_curveV1 := v;
                    VW_curveP1 := p; // this is actually supposed to be 1.0 always
                    bSet1 := TRUE;
                end
                else
                if not bSet2 then
                begin
                    VW_curveV2 := v;
                    if p < 0.0 then
                    begin
                        VW_curveP2gen := 0.2; // TODO: should have a pMin
                        VW_curveP2load := p;
                    end
                    else
                    begin
                        VW_curveP2gen := p;
                        VW_curveP2load := 0.0;
                    end;
                    bSet2 := TRUE;
                end;
            end;
            inc(i);
        end;
    end;

    xy := pInv.FvoltwattCH_curve;
    if (xy <> NIL) then
    begin
        p := 0.0;
        i := 1;
        while i <= xy.NumPoints do
        begin
            if xy.YValue_pt[i] > p then
                p := xy.YValue_pt[i];
            inc(i);
        end;
        if (-p < VW_curveP2load) then
            VW_curveP2load := -p;
    end;

    xy := pInv.Fwattvar_curve;
    if (xy <> NIL) then
    begin
        i := 1;
        bValid := FALSE;
        bSet1 := FALSE;
        bSet2 := FALSE;
        bSet3 := FALSE;
        bSet4 := FALSE;
        bSet5 := FALSE;
        bSet6 := FALSE;
        while i <= xy.NumPoints do
        begin
            p := xy.XValue_pt[i];
            q := xy.YValue_pt[i];
            if (p >= -1.0) and (p <= 1.0) then
                bValid := TRUE;
            if bValid then
            begin
                if not bSet1 then
                begin
                    if p <= -0.5 then
                    begin
                        WV_curveP3load := p;
                        WV_curveQ3load := q;
                    end
                    else
                    begin
                        WV_curveP3load := -1.0;
                        WV_curveQ3load := 0.0;
                        dec(i); // re-scan
                    end;
                    bSet1 := TRUE;
                end
                else
                if not bSet2 then
                begin
                    if p <= -0.4 then
                    begin
                        WV_curveP2load := p;
                        WV_curveQ2load := q;
                    end
                    else
                    begin
                        WV_curveP2load := -0.5;
                        WV_curveQ2load := 0.0;
                        dec(i); // re-scan
                    end;
                    bSet2 := TRUE;
                end
                else
                if not bSet3 then
                begin
                    if p <= 0.0 then
                    begin
                        WV_curveP1load := p;
                        WV_curveQ1load := q;
                    end
                    else
                    begin
                        WV_curveP1load := -0.2;
                        WV_curveQ1load := 0.0;
                        dec(i); // re-scan
                    end;
                    bSet3 := TRUE;
                end
                else
                if not bSet4 then
                begin
                    if p <= 0.7 then
                    begin
                        WV_curveP1gen := p;
                        WV_curveQ1gen := q;
                    end
                    else
                    begin
                        WV_curveP1gen := 0.2;
                        WV_curveQ1gen := 0.0;
                        dec(i); // re-scan
                    end;
                    bSet4 := TRUE;
                end
                else
                if not bSet5 then
                begin
                    if p <= 0.8 then
                    begin
                        WV_curveP2gen := p;
                        WV_curveQ2gen := q;
                    end
                    else
                    begin
                        WV_curveP2gen := 0.5;
                        WV_curveQ2gen := 0.0;
                        dec(i); // re-scan
                    end;
                    bSet5 := TRUE;
                end
                else
                if not bSet6 then
                begin
                    if p <= 1.0 then
                    begin
                        WV_curveP3gen := p;
                        WV_curveQ3gen := q;
                    end
                    else
                    begin
                        WV_curveP3gen := 1.0;
                        WV_curveQ3gen := 0.0;
                        dec(i); // re-scan
                    end;
                    bSet6 := TRUE;
                end;
            end;
            inc(i);
        end;
    // handle the edge cases when default zero watt-var points were not input
        if WV_curveP1gen >= WV_curveP2gen then
            WV_curveP1gen := WV_curveP2gen - 0.1;
        if WV_curveP1load <= WV_curveP2load then
            WV_curveP1load := WV_curveP2load + 0.1;
    end;

{* copied from InvControl!!
    // Modes
    NONE_MODE = 0;
    VOLTVAR   = 1;
    VOLTWATT  = 2;
    DRC       = 3;
    WATTPF    = 4;
    WATTVAR   = 5;
    AVR       = 6;

    // Combi Modes
    NONE_COMBMODE = 0;
    VV_VW         = 1;
    VV_DRC        = 2;
*}
    mode := Ord(pInv.ControlMode); // TODO: use enum values
    combi := Ord(pInv.CombiMode); //TODO: use enum values
    if combi = 1 then
    begin
        PF_enabled := FALSE;
        VV_enabled := TRUE;
        VW_enabled := TRUE;
    end
    else
    if combi = 2 then
    begin
        PF_enabled := FALSE;
        VV_enabled := TRUE;
    end
    else
    if mode = 1 then
    begin
        PF_enabled := FALSE;
        VV_enabled := TRUE;
    end
    else
    if mode = 2 then
    begin
        PF_enabled := FALSE;
        VW_enabled := TRUE;
    end
    else
    if mode = 3 then // approximating AVR with DRC
    begin
        PF_enabled := False;
        VV_enabled := True;
        VV_vRefAutoModeEnabled := True;
        VV_vRefOlrt := pInv.FDRCRollAvgWindowLength;
        qvslope := 0.5 * (pInv.FArGraLowV + pInv.FArGraHiV);

        if qvslope > 12.5 then 
            bCatB := True;  // for catA, maximum slope would be 12.5    

        if bCatB then 
            q := 0.44 
        else 
            q := 0.25;

        VV_curveQ1:=q;
        VV_curveQ2:=VV_curveQ1;
        VV_curveQ3:=-VV_curveQ1;
        VV_curveQ4:=VV_curveQ3;
        VV_curveV1:=0.50;
        VV_curveV2:=1.0 - VV_curveQ2 / QVSlope;
        VV_curveV3:=1.0 - VV_curveQ3 / QVSlope;  // - because Q3 should be negative
        VV_curveV4:=1.50;
    end
    else
    if mode = 5 then
    begin
        PF_enabled := FALSE;
        WV_enabled := TRUE;
    end;
end;

procedure TIEEE1547Controller.PullFromExpControl(pExp: TExpControlObj);
var
    i: Integer;
begin
    pInvName.LocalName := pExp.Name;
    pInvName.UUID := pExp.UUID;

    i := 0;
    while i < pExp.DERNameList.Count do
    begin
        pDERNames.Add(pExp.DERNameList.Strings[i]);
        inc(i);
    end;
    pMonBuses.Clear;

    if pExp.QMaxLead > CatBQmin then // catB estimate
        SetDefaults(TRUE)
    else
        SetDefaults(FALSE);

    PF_enabled := False;
    VV_enabled := True;
    VV_vRefAutoModeEnabled := TRUE;
    VV_vRefOlrt := pExp.VregTau;
    VV_olrt := pExp.TResponse;
    VV_curveQ1 := pExp.QMaxLead;
    VV_curveQ2 := VV_curveQ1;
    VV_curveQ3 := -pExp.QMaxLag;
    VV_curveQ4 := VV_curveQ3;
    VV_curveV1 := 0.50;
    VV_curveV2 := 1.0 - VV_curveQ2 / pExp.QVSlope;
    VV_curveV3 := 1.0 - VV_curveQ3 / pExp.QVSlope;  // - because Q3 should be negative
    VV_curveV4 := 1.50;
end;

procedure TIEEE1547Controller.SetDefaults(bCatB: Boolean);
begin
    bNameplateSet := FALSE;
    ND_acVmax := 1.05;
    ND_acVmin := 0.95;
    AD_pMax := 0.0;
    AD_pMaxOverPF := 0.0;
    AD_overPF := 0.0;
    AD_pMaxUnderPF := 0.0;
    AD_underPF := 0.0;
    AD_sMax := 0.0;
    AD_pMaxCharge := 0.0;
    AD_apparentPowerChargeMax := 0.0;
    AD_acVnom := 0.0;
    AD_qMaxInj := 0.44;
    if bCatB then
    begin
        ND_normalOPcatKind := 'catB';
        AD_qMaxAbs := 0.44;
        VV_curveV1 := 0.92;
        VV_curveV2 := 0.98;
        VV_curveV3 := 1.02;
        VV_curveV4 := 1.08;
        VV_curveQ1 := 0.44;
        VV_curveQ2 := 0.0;
        VV_curveQ3 := 0.0;
        VV_curveQ4 := -0.44;
        VV_olrt := 5.0;
        WV_curveQ3load := 0.44;
    end
    else
    begin
        ND_normalOPcatKind := 'catA';
        AD_qMaxAbs := 0.25;
        VV_curveV1 := 0.90;
        VV_curveV2 := 1.00;
        VV_curveV3 := 1.00;
        VV_curveV4 := 1.10;
        VV_curveQ1 := 0.25;
        VV_curveQ2 := 0.0;
        VV_curveQ3 := 0.0;
        VV_curveQ4 := -0.25;
        VV_olrt := 10.0;
        WV_curveQ3load := 0.25;
    end;
    VV_vRef := 1.0;
    VV_vRefOlrt := 300.0;
    Q_reactivePower := 0.0;
    PF_powerFactor := 1.0;
    VW_olrt := 10.0;
    VW_curveV1 := 1.06;
    VW_curveV2 := 1.10;
    VW_curveP1 := 1.0;
    VW_curveP2gen := 0.2;
    VW_curveP2load := 0.0; // for storage, -1.0

    WV_curveP1gen := 0.2;
    WV_curveP2gen := 0.5;
    WV_curveP3gen := 1.0;
    WV_curveP1load := 0.0; // for storage, 0.2, 0.5, 1.0
    WV_curveP2load := 0.0;
    WV_curveP3load := 0.0;
    WV_curveQ1gen := 0.0;
    WV_curveQ2gen := 0.0;
    WV_curveQ3gen := 0.44;
    WV_curveQ1load := 0.0;
    WV_curveQ2load := 0.0;

    PF_constPFexcitationKind := 'inj';
    VV_enabled := FALSE;
    WV_enabled := FALSE;
    PF_enabled := TRUE;
    Q_enabled := FALSE;
    VW_enabled := FALSE;
    VV_vRefAutoModeEnabled := FALSE;
end;

procedure TIEEE1547Controller.FinishNameplate;
begin
    AD_overPF := AD_pMaxOverPF / AD_sMax;
    AD_underPF := AD_pMaxUnderPF / AD_sMax;
    bNameplateSet := TRUE;
end;

procedure TIEEE1547Controller.SetStorageNameplate(pBat: TStorage2Obj);
begin
    with pBat, StorageVars do 
    begin
        AD_acVnom := PresentKV * 1000.0;
        ND_acVmax := PresentKV * Vmaxpu * 1000.0;
        ND_acVmin := PresentKV * Vmaxpu * 1000.0;
        AD_sMax := kVARating * 1000.0;
        AD_pMax := (kwRating * pctKwOut / 100.0) * 1000.0;
        AD_pMaxOverPF := sqrt(FKvaRating * FKvaRating - FkvarLimit * FkvarLimit) * 1000.0;
        AD_pMaxUnderPF := sqrt(FKvaRating * FKvaRating - FkvarLimitNeg * FkvarLimitNeg) * 1000.0;
        AD_pMaxCharge := (kwRating * pctKwIn / 100.0) * 1000.0;
        AD_apparentPowerChargeMax := FkvaRating * 1000.0;
        AD_qMaxInj := Math.Min(Fkvarlimit, FkVARating) * 1000.0;
        AD_qMaxAbs := Math.Min(FkvarlimitNeg, FkVARating) * 1000.0;
    end;
    FinishNameplate;
end;

procedure TIEEE1547Controller.SetPhotovoltaicNameplate(pPV: TPVSystem2Obj);
var
    qmaxinj, qmaxabs: Double;
begin
    with pPV, PVSystemVars do
    begin
        qmaxinj := Fkvarlimit;
        if not kvarlimitset then 
            qmaxinj := 0.25 * kvarating; // unlike storage, defaults to category A

        qmaxabs := FkvarlimitNeg;
        if not kvarlimitnegset then 
            qmaxabs := 0.25 * kvarating; // unlike storage, defaults to category A

        AD_acVnom := pPV.PresentKV * 1000.0;
        ND_acVmax := pPV.PresentKV * pPV.Vmaxpu * 1000.0;
        ND_acVmin := pPV.PresentKV * pPV.Vminpu * 1000.0;
        AD_sMax := pPV.kVARating * 1000.0;
        AD_pMax := pPV.Pmpp * 1000.0;
        AD_pMaxOverPF := (sqrt(FKvaRating * FKvaRating - qmaxinj * qmaxinj)) * 1000.0;
        AD_pMaxUnderPF := (sqrt(FKvaRating * FKvaRating - qmaxabs * qmaxabs)) * 1000.0;
        AD_pMaxCharge := (0.0) * 1000.0;
        AD_apparentPowerChargeMax := (0.0) * 1000.0; 
        AD_qMaxInj := qmaxinj * 1000.0;
        AD_qMaxAbs := qmaxabs * 1000.0;
    end;
    FinishNameplate;
end;

procedure TIEEE1547Controller.SetElementNameplate(pElem: TDSSCktElement);
begin
    if bNameplateSet then
        exit;
    if pElem.DSSObjType = (PC_ELEMENT + PVSYSTEM_ELEMENT) then
        SetPhotovoltaicNameplate(TPVSystem2Obj(pElem));
    if pElem.DSSObjType = (PC_ELEMENT + STORAGE_ELEMENT) then
        SetStorageNameplate(TStorage2Obj(pElem));
    FinishNameplate;
end;

procedure TIEEE1547Controller.WriteCIM(prf: ProfileChoice);
var
    i: Integer;
    pPV: TPVSystem2Obj;
    pBat: TStorage2Obj;
begin
    with ex do
    begin
        FindSignalTerminals;
        StartInstance(prf, 'DERIEEEType1', pInvName);
        BooleanNode(prf, 'DynamicsFunctionBlock.enabled', TRUE);
        BooleanNode (prf, 'DERIEEEType1.phaseToGroundApplicable', True); // seems to be the only OpenDSS option
        BooleanNode (prf, 'DERIEEEType1.phaseToNeutralApplicable', False);
        BooleanNode (prf, 'DERIEEEType1.phaseToPhaseApplicable', False);
        with ex.ActiveCircuit do
        begin
            if pDERNames.Count < 1 then
            begin
                pBat := StorageElements.First;
                while pBat <> NIL do
                begin
                    if pBat.Enabled then
                    begin
                        RefNode(prf, 'DERDynamics.PowerElectronicsConnection', pBat);
                        SetStorageNameplate(pBat);
                    end;
                    pBat := StorageElements.Next;
                end;
                pPV := PVSystems.First;
                while pPV <> NIL do
                begin
                    if pPV.Enabled then
                    begin
                        RefNode(prf, 'DERDynamics.PowerElectronicsConnection', pPV);
                        SetPhotovoltaicNameplate(pPV);
                    end;
                    pPV := PVSystems.Next;
                end;
            end
            else
            begin
                for i := 1 to pDERNames.Count do
                begin
                    ActiveCircuit.SetElementActive(pDERNames.Strings[i - 1]);
                    RefNode(prf, 'DERDynamics.PowerElectronicsConnection', ActiveCktElement);
                    SetElementNameplate(ActiveCktElement);
                end;
            end;
        end;
        for i := Low(Signals) to High(Signals) do
            RefNode(prf, 'DERDynamics.RemoteInputSignal', Signals[i]);
        EndInstance(prf, 'DERIEEEType1');

        for i := Low(Signals) to High(Signals) do
        begin
            StartInstance(prf, 'RemoteInputSignal', Signals[i]);
            RemoteInputSignalEnum(prf, 'remoteBusVoltageAmplitude');
            UuidNode(prf, 'RemoteInputSignal.Terminal', GetTermUuid(Signals[i].pElem, Signals[i].trm));
            EndInstance(prf, 'RemoteInputSignal');
        end;

        pPlateName.LocalName := pInvName.LocalName;
        pPlateName.UUID := GetDevUuid(I1547NameplateData, pInvName.LocalName, 1);
        StartInstance(prf, 'DERNameplateData', pPlateName);
        RefNode(prf, 'DERNameplateData.DERIEEEType1', pInvName);
        NormalOpCatEnum(prf, ND_normalOPcatKind);
        BooleanNode(prf, 'DERNameplateData.supportsConstPFmode', True);
        BooleanNode(prf, 'DERNameplateData.supportsConstQmode', True);
        BooleanNode(prf, 'DERNameplateData.supportsQVmode', True);
        if ND_normalOPcatKind = 'catB' then
        begin
            BooleanNode(prf, 'DERNameplateData.supportsPVmode', True);
            BooleanNode(prf, 'DERNameplateData.supportsQPmode', True);
        end 
        else 
        begin
            BooleanNode(prf, 'DERNameplateData.supportsPVmode', False);
            BooleanNode(prf, 'DERNameplateData.supportsQPmode', False);
        end;
        BooleanNode(prf, 'DERNameplateData.supportsPFmode', False); // no frequency response in GridAPPS-D
        DoubleNode(prf, 'DERNameplateData.acVmax', ND_acVmax);
        DoubleNode(prf, 'DERNameplateData.acVmin', ND_acVmin);
        EndInstance(prf, 'DERNameplateData');

        pSetName.LocalName := pInvName.LocalName;
        pSetName.UUID := GetDevUuid(I1547NameplateDataApplied, pSetName.LocalName, 1);
        StartInstance(prf, 'DERNameplateDataApplied', pSetName);
        RefNode(prf, 'DERNameplateDataApplied.DERNameplateData', pPlateName);
        DoubleNode(prf, 'DERNameplateDataApplied.pMax', AD_pMax);
        DoubleNode(prf, 'DERNameplateDataApplied.pMaxOverPF', AD_pMaxOverPF);
        DoubleNode(prf, 'DERNameplateDataApplied.overPF', AD_overPF);
        DoubleNode(prf, 'DERNameplateDataApplied.pMaxUnderPF', AD_pMaxUnderPF);
        DoubleNode(prf, 'DERNameplateDataApplied.underPF', AD_underPF);
        DoubleNode(prf, 'DERNameplateDataApplied.sMax', AD_sMax);
        DoubleNode(prf, 'DERNameplateDataApplied.qMaxInj', AD_qMaxInj);
        DoubleNode(prf, 'DERNameplateDataApplied.qMaxAbs', AD_qMaxAbs);
        DoubleNode(prf, 'DERNameplateDataApplied.pMaxCharge', AD_pMaxCharge);
        DoubleNode(prf, 'DERNameplateDataApplied.apparentPowerChargeMax', AD_apparentPowerChargeMax);
        DoubleNode(prf, 'DERNameplateDataApplied.acVnom', AD_acVnom);
        EndInstance(prf, 'DERNameplateDataApplied');

        pSetName.UUID := GetDevUuid(I1547VoltVar, pSetName.LocalName, 1);
        StartInstance(prf, 'VoltVarSettings', pSetName);
        RefNode(prf, 'VoltVarSettings.DERIEEEType1', pInvName);
        BooleanNode(prf, 'VoltVarSettings.enabled', VV_enabled);
        BooleanNode(prf, 'VoltVarSettings.vRefAutoModeEnabled', VV_vRefAutoModeEnabled);
        DoubleNode(prf, 'VoltVarSettings.vRef', VV_vRef);
        DoubleNode(prf, 'VoltVarSettings.vRefOlrt', VV_vRefOlrt);
        DoubleNode(prf, 'VoltVarSettings.curveV1', VV_curveV1);
        DoubleNode(prf, 'VoltVarSettings.curveV2', VV_curveV2);
        DoubleNode(prf, 'VoltVarSettings.curveV3', VV_curveV3);
        DoubleNode(prf, 'VoltVarSettings.curveV4', VV_curveV4);
        DoubleNode(prf, 'VoltVarSettings.curveQ1', VV_curveQ1);
        DoubleNode(prf, 'VoltVarSettings.curveQ2', VV_curveQ2);
        DoubleNode(prf, 'VoltVarSettings.curveQ3', VV_curveQ3);
        DoubleNode(prf, 'VoltVarSettings.curveQ4', VV_curveQ4);
        DoubleNode(prf, 'VoltVarSettings.olrt', VV_olrt);
        EndInstance(prf, 'VoltVarSettings');

        pSetName.UUID := GetDevUuid(I1547WattVar, pSetName.LocalName, 1);
        StartInstance(prf, 'WattVarSettings', pSetName);
        RefNode(prf, 'WattVarSettings.DERIEEEType1', pInvName);
        BooleanNode(prf, 'WattVarSettings.enabled', WV_enabled);
        DoubleNode(prf, 'WattVarSettings.curveP1gen', WV_curveP1gen);
        DoubleNode(prf, 'WattVarSettings.curveP2gen', WV_curveP2gen);
        DoubleNode(prf, 'WattVarSettings.curveP3gen', WV_curveP3gen);
        DoubleNode(prf, 'WattVarSettings.curveQ1gen', WV_curveQ1gen);
        DoubleNode(prf, 'WattVarSettings.curveQ2gen', WV_curveQ2gen);
        DoubleNode(prf, 'WattVarSettings.curveQ3gen', WV_curveQ3gen);
        DoubleNode(prf, 'WattVarSettings.curveP1load', WV_curveP1load);
        DoubleNode(prf, 'WattVarSettings.curveP2load', WV_curveP2load);
        DoubleNode(prf, 'WattVarSettings.curveP3load', WV_curveP3load);
        DoubleNode(prf, 'WattVarSettings.curveQ1load', WV_curveQ1load);
        DoubleNode(prf, 'WattVarSettings.curveQ2load', WV_curveQ2load);
        DoubleNode(prf, 'WattVarSettings.curveQ3load', WV_curveQ3load);
        EndInstance(prf, 'WattVarSettings');

        pSetName.UUID := GetDevUuid(I1547ConstPF, pSetName.LocalName, 1);
        StartInstance(prf, 'ConstantPowerFactorSettings', pSetName);
        RefNode(prf, 'ConstantPowerFactorSettings.DERIEEEType1', pInvName);
        BooleanNode(prf, 'ConstantPowerFactorSettings.enabled', PF_enabled);
        PowerFactorExcitationEnum(prf, PF_constPFexcitationKind);
        DoubleNode(prf, 'ConstantPowerFactorSettings.powerFactor', PF_powerFactor);
        EndInstance(prf, 'ConstantPowerFactorSettings');

        pSetName.UUID := GetDevUuid(I1547ConstQ, pSetName.LocalName, 1);
        StartInstance(prf, 'ConstantReactivePowerSettings', pSetName);
        RefNode(prf, 'ConstantReactivePowerSettings.DERIEEEType1', pInvName);
        BooleanNode(prf, 'ConstantReactivePowerSettings.enabled', Q_enabled);
        DoubleNode(prf, 'ConstantReactivePowerSettings.reactivePower', Q_reactivePower);
        EndInstance(prf, 'ConstantReactivePowerSettings');

        pSetName.UUID := GetDevUuid(I1547VoltWatt, pSetName.LocalName, 1);
        StartInstance(prf, 'VoltWattSettings', pSetName);
        RefNode(prf, 'VoltWattSettings.DERIEEEType1', pInvName);
        BooleanNode(prf, 'VoltWattSettings.enabled', VW_enabled);
        DoubleNode(prf, 'VoltWattSettings.curveV1', VW_curveV1);
        DoubleNode(prf, 'VoltWattSettings.curveV2', VW_curveV2);
        DoubleNode(prf, 'VoltWattSettings.curveP1', VW_curveP1);
        DoubleNode(prf, 'VoltWattSettings.curveP2gen', VW_curveP2gen);
        DoubleNode(prf, 'VoltWattSettings.curveP2load', VW_curveP2load);
        DoubleNode(prf, 'VoltWattSettings.olrt', VW_olrt);
        EndInstance(prf, 'VoltWattSettings');
    end; // with ex
end;

///////// end helper class for exporting IEEE 1547 model parameters /////////////


procedure TCIMExporterHelper.StartCIMFile(F: TFileStream; FileNm: String; prf: ProfileChoice);
begin
    F := TBufferedFileStream.Create(FileNm, fmCreate);
    FSWriteln(F, '<?xml version="1.0" encoding="utf-8"?>');
    FSWriteln(F, '<!-- un-comment this line to enable validation');
    FSWriteln(F, '-->');
    FSWriteln(F, '<rdf:RDF xmlns:cim="' + CIM_NS + '#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">');
    FSWriteln(F, '<!--');
    FSWriteln(F, '-->');
    FSWriteLn(F, Format('<cim:IEC61970CIMVersion rdf:ID="%s">', [UUIDToCIMString(GetDevUuid(CIMVer, 'IEC', 1))]));
    FSWriteLn(F, Format('  <cim:IEC61970CIMVersion.version>%s</cim:IEC61970CIMVersion.version>', ['IEC61970CIM100']));
    FSWriteLn(F, Format('  <cim:IEC61970CIMVersion.date>%s</cim:IEC61970CIMVersion.date>', ['2019-04-01']));
    FSWriteLn(F, '</cim:IEC61970CIMVersion>');
end;

procedure TCIMExporter.ExportCDPSM(FileNm: String;
    Substation: String;
    SubGeographicRegion: String;
    GeographicRegion: String;
    FdrUUID: TUuid;
    SubUUID: TUuid;
    SubGeoUUID: TUuid;
    RgnUUID: TUuid;
    Combined: Boolean);
var
    i, j, k: Integer;
    seq: Integer;
    val: Double;
    bval: Boolean;
    v1, v2: Double;
    i1, i2: Integer;
    Zs, Zm: complex;
    Rs, Rm, Xs, Xm, R1, R0, X1, X0: Double;
    pName1, pName2: TNamedObject;
    pIsland, pSwing: TNamedObject;  // island and ref node
    pRegion, pSubRegion, pLocation, pSubstation, pCRS: TNamedObject;

    pILimit: TCIMOpLimitObject;
    pNormLimit, pEmergLimit, pRangeAHiLimit, pRangeALoLimit, pRangeBHiLimit, pRangeBLoLimit: TNamedObject; // OperationalLimitType
    LimitName: String;
    LimiTUuid: TUuid;

    zbase: Double;
    s: String;
    swtCls: String;  // based on controls, if any, attached to a line having switch=yes
    ratedAmps, breakingAmps: Double;

    pBank: TCIMBankObject;
    maxWdg: Integer;
    WdgList: array of TNamedObject;
    CoreList: array of TNamedObject;
    MeshList: array of TNamedObject;
    sBank: String;
    bTanks: Boolean;

    pLoad: TLoadObj;
    pVsrc: TVsourceObj;
    pGen: TGeneratorObj;
    pPV: TPVSystem2Obj;
    pBat: TStorage2Obj;

    pCap: TCapacitorObj;
    pCapC: TCapControlObj;
    pXf: TTransfObj;
    pAuto: TAutoTransObj;
    pReg: TRegControlObj;
    pLine: TLineObj;
    pReac: TReactorObj;
    pInv: TInvControl2Obj;
    pExp  : TExpControlObj;
    pI1547: TIEEE1547Controller;

    clsLnCd: TLineCode;
    clsGeom: TLineGeometry;
    clsWire: TWireData;
    clsXfCd: TXfmrCode;
    clsSpac: TLineSpacing;
    clsTape: TTSData;
    clsConc: TCNData;

    pLnCd: TLineCodeObj;
    pGeom: TLineGeometryObj;
    pWire: TWireDataObj;
    pXfCd: TXfmrCodeObj;
    pSpac: TLineSpacingObj;
    pTape: TTSDataObj;
    pConc: TCNDataObj;

  // DSS-like load models
    id1_ConstkVA: TUuid;
    id2_ConstZ: TUuid;
    id3_ConstPQuadQ: TUuid;
    id4_LinPQuadQ: TUuid;
    id5_ConstI: TUuid;
    id6_ConstPConstQ: TUuid;  // P can vary, Q not
    id7_ConstPConstX: TUuid;

  // for CIM Locations
    geoUUID: TUuid;
    crsUUID: TUuid;
begin
    try
        clsLnCd := DSS.DSSClassList.Get(DSS.ClassNames.Find('linecode'));
        clsWire := DSS.DSSClassList.Get(DSS.ClassNames.Find('wiredata'));
        clsGeom := DSS.DSSClassList.Get(DSS.ClassNames.Find('linegeometry'));
        clsXfCd := DSS.DSSClassList.Get(DSS.ClassNames.Find('xfmrcode'));
        clsSpac := DSS.DSSClassList.Get(DSS.ClassNames.Find('linespacing'));
        clsTape := DSS.DSSClassList.Get(DSS.ClassNames.Find('TSData'));
        clsConc := DSS.DSSClassList.Get(DSS.ClassNames.Find('CNData'));
        pName1 := TNamedObject.Create('Temp1');
        pName2 := TNamedObject.Create('Temp2');
        if not assigned(UuidList) then
        begin  // this may have been done already from the uuids command
            i1 := clsXfCd.ElementCount * 6; // 3 wdg info, 3 sctest
            i2 := ActiveCircuit.Transformers.Count * 11; // bank, info, 3 wdg, 3 wdg info, 3sctest
            StartUuidList(i1 + i2);
        end;
        StartBankList(ActiveCircuit.Transformers.Count + ActiveCircuit.AutoTransformers.Count);
        StartOpLimitList(ActiveCircuit.Lines.Count + ActiveCircuit.Transformers.Count + ActiveCircuit.AutoTransformers.Count);

        DSSInfoMessageDlg(FileNm + '<=' + ActiveCircuit.Name + '<-' + Substation + '<-' + SubGeographicRegion + '<-' + GeographicRegion);

        FD_Create(Combined, FileNm);

        pCRS := TNamedObject.Create('CoordinateSystem');
        crsUUID := GetDevUuid(CoordSys, 'Local', 1);
        pCRS.UUID := crsUUID;
        pCRS.localName := ActiveCircuit.Name + '_CrsUrn';
        StartInstance(GeoPrf, 'CoordinateSystem', pCRS);
        StringNode(GeoPrf, 'CoordinateSystem.crsUrn', 'OpenDSSLocalBusCoordinates');
        EndInstance(GeoPrf, 'CoordinateSystem');

        pRegion := TNamedObject.Create('GeographicalRegion');
        pRegion.UUID := RgnUUID;
        pRegion.LocalName := GeographicRegion;
        StartInstance(FunPrf, 'GeographicalRegion', pRegion);
        EndInstance(FunPrf, 'GeographicalRegion');

        pSubRegion := TNamedObject.Create('SubGeographicalRegion');
        pSubRegion.UUID := SubGeoUUID;
        pSubRegion.LocalName := SubGeographicRegion;
        StartInstance(FunPrf, 'SubGeographicalRegion', pSubRegion);
        RefNode(FunPrf, 'SubGeographicalRegion.Region', pRegion);
        EndInstance(FunPrf, 'SubGeographicalRegion');

        pSubstation := TNamedObject.Create('Substation');
        pSubstation.UUID := SubUUID;
        pSubstation.LocalName := Substation;
        StartInstance(FunPrf, 'Substation', pSubstation);
        RefNode(FunPrf, 'Substation.Region', pSubRegion);
        EndInstance(FunPrf, 'Substation');

        pLocation := TNamedObject.Create('Location');
        pLocation.UUID := GetDevUuid(FdrLoc, ActiveCircuit.Name, 1);
        pLocation.localName := ActiveCircuit.Name + '_Location';
        StartInstance(GeoPrf, 'Location', pLocation);
        UuidNode(GeoPrf, 'Location.CoordinateSystem', crsUUID);
        EndInstance(GeoPrf, 'Location');

        ActiveCircuit.UUID := FdrUUID;
        StartInstance(FunPrf, 'Feeder', ActiveCircuit);
        RefNode(FunPrf, 'Feeder.NormalEnergizingSubstation', pSubstation);
        RefNode(FunPrf, 'PowerSystemResource.Location', pLocation);
        EndInstance(FunPrf, 'Feeder');

        // the whole system will be a topo island
        pIsland := TNamedObject.Create('Island');
        pIsland.localName := ActiveCircuit.Name + '_Island';
        pIsland.UUID := GetDevUuid(TopoIsland, 'Island', 1);
        pSwing := TNamedObject.Create('SwingBus');
        pSwing.localName := ActiveCircuit.Name + '_SwingBus';

        pNormLimit := TNamedObject.Create('NormalAmpsType');
        pNormLimit.localName := ActiveCircuit.Name + '_NormAmpsType';
        pNormLimit.UUID := GetDevUuid(OpLimT, 'NormalAmps', 1);
        StartInstance(FunPrf, 'OperationalLimitType', pNormLimit);
        DoubleNode(FunPrf, 'OperationalLimitType.acceptableDuration', 5.0e9);  // more than 100 years
        OpLimitDirectionEnum(FunPrf, 'absoluteValue');
        EndInstance(FunPrf, 'OperationalLimitType');

        pEmergLimit := TNamedObject.Create('EmergencyAmpsType');
        pEmergLimit.localName := ActiveCircuit.Name + '_EmergencyAmpsType';
        pEmergLimit.UUID := GetDevUuid(OpLimT, 'EmergencyAmps', 1);
        StartInstance(FunPrf, 'OperationalLimitType', pEmergLimit);
        DoubleNode(FunPrf, 'OperationalLimitType.acceptableDuration', 2.0 * 3600.0); // 2 hours
        OpLimitDirectionEnum(FunPrf, 'absoluteValue');
        EndInstance(FunPrf, 'OperationalLimitType');

        pRangeAHiLimit := TNamedObject.Create('RangeAHiType');
        pRangeAHiLimit.localName := ActiveCircuit.Name + '_RangeAHiType';
        pRangeAHiLimit.UUID := GetDevUuid(OpLimT, 'AHi', 1);
        StartInstance(FunPrf, 'OperationalLimitType', pRangeAHiLimit);
        DoubleNode(FunPrf, 'OperationalLimitType.acceptableDuration', 5.0e9);
        OpLimitDirectionEnum(FunPrf, 'high');
        EndInstance(FunPrf, 'OperationalLimitType');

        pRangeALoLimit := TNamedObject.Create('RangeALoType');
        pRangeALoLimit.localName := ActiveCircuit.Name + '_RangeALoType';
        pRangeALoLimit.UUID := GetDevUuid(OpLimT, 'ALo', 1);
        StartInstance(FunPrf, 'OperationalLimitType', pRangeALoLimit);
        DoubleNode(FunPrf, 'OperationalLimitType.acceptableDuration', 5.0e9);
        OpLimitDirectionEnum(FunPrf, 'low');
        EndInstance(FunPrf, 'OperationalLimitType');

        pRangeBHiLimit := TNamedObject.Create('RangeBHiType');
        pRangeBHiLimit.localName := ActiveCircuit.Name + '_RangeBHiType';
        pRangeBHiLimit.UUID := GetDevUuid(OpLimT, 'BHi', 1);
        StartInstance(FunPrf, 'OperationalLimitType', pRangeBHiLimit);
        DoubleNode(FunPrf, 'OperationalLimitType.acceptableDuration', 24.0 * 3600.0);
        OpLimitDirectionEnum(FunPrf, 'high');
        EndInstance(FunPrf, 'OperationalLimitType');

        pRangeBLoLimit := TNamedObject.Create('RangeBLoType');
        pRangeBLoLimit.localName := ActiveCircuit.Name + '_RangeBLoType';
        pRangeBLoLimit.UUID := GetDevUuid(OpLimT, 'BLo', 1);
        StartInstance(FunPrf, 'OperationalLimitType', pRangeBLoLimit);
        DoubleNode(FunPrf, 'OperationalLimitType.acceptableDuration', 24.0 * 3600.0);
        OpLimitDirectionEnum(FunPrf, 'low');
        EndInstance(FunPrf, 'OperationalLimitType');

        with ActiveCircuit do
        begin
      // build the lists of base voltages and operational voltage limits
            i := 1;
            while LegalVoltageBases[i] > 0.0 do
            begin
                s := GetBaseVName(LegalVoltageBases[i]);
                pName1.LocalName := s;
                pName1.UUID := GetBaseVUuid(LegalVoltageBases[i]);
                StartInstance(FunPrf, 'BaseVoltage', pName1);
                DoubleNode(FunPrf, 'BaseVoltage.nominalVoltage', 1000.0 * LegalVoltageBases[i]);
                EndInstance(FunPrf, 'BaseVoltage');

                pName1.LocalName := GetOpLimVName(LegalVoltageBases[i]);
                pName1.UUID := GetOpLimVUuid(LegalVoltageBases[i]);
                StartInstance(FunPrf, 'OperationalLimitSet', pName1);
                EndInstance(FunPrf, 'OperationalLimitSet');

                pName2.LocalName := pName1.LocalName + '_RangeAHi';
                pName2.UUID := GetDevUuid(OpLimAHi, s, 1);
                StartInstance(FunPrf, 'VoltageLimit', pName2);
                RefNode(FunPrf, 'OperationalLimit.OperationalLimitSet', pName1);
                RefNode(FunPrf, 'OperationalLimit.OperationalLimitType', pRangeAHiLimit);
                DoubleNode(FunPrf, 'VoltageLimit.value', 1.05 * 1000.0 * LegalVoltageBases[i]);
                EndInstance(FunPrf, 'VoltageLimit');

                pName2.LocalName := pName1.LocalName + '_RangeALo';
                pName2.UUID := GetDevUuid(OpLimALo, s, 1);
                StartInstance(FunPrf, 'VoltageLimit', pName2);
                RefNode(FunPrf, 'OperationalLimit.OperationalLimitSet', pName1);
                RefNode(FunPrf, 'OperationalLimit.OperationalLimitType', pRangeALoLimit);
                DoubleNode(FunPrf, 'VoltageLimit.value', 0.95 * 1000.0 * LegalVoltageBases[i]);
                EndInstance(FunPrf, 'VoltageLimit');

                pName2.LocalName := pName1.LocalName + '_RangeBHi';
                pName2.UUID := GetDevUuid(OpLimBHi, s, 1);
                StartInstance(FunPrf, 'VoltageLimit', pName2);
                RefNode(FunPrf, 'OperationalLimit.OperationalLimitSet', pName1);
                RefNode(FunPrf, 'OperationalLimit.OperationalLimitType', pRangeBHiLimit);
                DoubleNode(FunPrf, 'VoltageLimit.value', 1.0583333 * 1000.0 * LegalVoltageBases[i]);
                EndInstance(FunPrf, 'VoltageLimit');

                pName2.LocalName := pName1.LocalName + '_RangeBLo';
                pName2.UUID := GetDevUuid(OpLimBLo, s, 1);
                StartInstance(FunPrf, 'VoltageLimit', pName2);
                RefNode(FunPrf, 'OperationalLimit.OperationalLimitSet', pName1);
                RefNode(FunPrf, 'OperationalLimit.OperationalLimitType', pRangeBLoLimit);
                DoubleNode(FunPrf, 'VoltageLimit.value', 0.9166667 * 1000.0 * LegalVoltageBases[i]);
                EndInstance(FunPrf, 'VoltageLimit');

                inc(i);
            end;

            for i := 1 to NumBuses do
            begin
                Buses^[i].localName := BusList.NameOfIndex(i);
            end;

            // each bus corresponds to a topo node (TODO, do we need topo nodes anymore?) and connectivity node
            for i := 1 to NumBuses do
            begin
                geoUUID := GetDevUuid(Topo, Buses^[i].localName, 1);
                StartFreeInstance(TopoPrf, 'TopologicalNode', geoUUID);
                StringNode(TopoPrf, 'IdentifiedObject.mRID', UUIDToCIMString(geoUUID));
                StringNode(TopoPrf, 'IdentifiedObject.name', Buses^[i].localName);
                UuidNode(TopoPrf, 'TopologicalNode.TopologicalIsland', pIsland.UUID);
                EndInstance(TopoPrf, 'TopologicalNode');

                StartFreeInstance(TopoPrf, 'ConnectivityNode', Buses^[i].UUID);
                StringNode(TopoPrf, 'IdentifiedObject.mRID', UUIDToCIMString(Buses^[i].UUID));
                StringNode(TopoPrf, 'IdentifiedObject.name', Buses^[i].localName);
                UuidNode(TopoPrf, 'ConnectivityNode.TopologicalNode', geoUUID);
                UuidNode(TopoPrf, 'ConnectivityNode.OperationalLimitSet', GetOpLimVUuid(sqrt(3.0) * ActiveCircuit.Buses^[i].kVBase));
                FD.WriteCimLn(TopoPrf, Format('  <cim:ConnectivityNode.ConnectivityNodeContainer rdf:resource="#%s"/>',
                    [ActiveCircuit.CIM_ID]));
                EndInstance(TopoPrf, 'ConnectivityNode');
            end;

            // find the swing bus ==> first voltage source
            pVsrc := ActiveCircuit.Sources.First; // pIsrc are in the same list
            while pVsrc <> NIL do
            begin
                if pVsrc.ClassNameIs('TVSourceObj') then
                begin
                    if pVsrc.Enabled then
                    begin
                        i := pVsrc.Terminals[0].BusRef;
                        geoUUID := GetDevUuid(Topo, Buses^[i].localName, 1);
                        pSwing.UUID := geoUUID;
                        StartInstance(TopoPrf, 'TopologicalIsland', pIsland);
                        RefNode(TopoPrf, 'TopologicalIsland.AngleRefTopologicalNode', pSwing);
                        EndInstance(TopoPrf, 'TopologicalIsland');
                        break;
                    end;
                end;
                pVsrc := ActiveCircuit.Sources.Next;
            end;
        end;

        pGen := ActiveCircuit.Generators.First;
        while pGen <> NIL do
        begin
            if pGen.Enabled then
            begin
                StartInstance(FunPrf, 'SynchronousMachine', pGen);
                CircuitNode(FunPrf, ActiveCircuit);
                DoubleNode(SshPrf, 'SynchronousMachine.p', pGen.Presentkw * 1000.0);
                DoubleNode(SshPrf, 'SynchronousMachine.q', pGen.Presentkvar * 1000.0);
                DoubleNode(EpPrf, 'SynchronousMachine.ratedS', pGen.GenVars.kvarating * 1000.0);
                DoubleNode(EpPrf, 'SynchronousMachine.ratedU', pGen.Presentkv * 1000.0);
//        SynchMachTypeEnum (F, 'generator');
//        SynchMachModeEnum (F, 'generator');
                geoUUID := GetDevUuid(MachLoc, pGen.LocalName, 1);
                UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
                EndInstance(FunPrf, 'SynchronousMachine');
                AttachGeneratorPhases(pGen, geoUUID);
                WriteTerminals(pGen, geoUUID, crsUUID);
            end;
            pGen := ActiveCircuit.Generators.Next;
        end;

        pPV := ActiveCircuit.PVSystems.First;
        while pPV <> NIL do
        begin
            if pPV.Enabled then
            begin
                pName1.LocalName := pPV.Name; // + '_PVPanels';
                pName1.UUID := GetDevUuid(PVPanels, pPV.LocalName, 1);
                StartInstance(FunPrf, 'PhotovoltaicUnit', pName1);
                geoUUID := GetDevUuid(SolarLoc, pPV.localName, 1);
                UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
                DoubleNode(EpPrf, 'PowerElectronicsUnit.maxP', pPV.Pmpp * 1000.0);
                with pPV do
                    DoubleNode(EpPrf, 'PowerElectronicsUnit.minP', (min (FPctCutIn, FPctCutOut) * kVARating / 100.0) * 1000.0);
                EndInstance(FunPrf, 'PhotovoltaicUnit');
                StartInstance(FunPrf, 'PowerElectronicsConnection', pPV);
                CircuitNode(FunPrf, ActiveCircuit);
                RefNode(FunPrf, 'PowerElectronicsConnection.PowerElectronicsUnit', pName1);
                DoubleNode(EpPrf, 'PowerElectronicsConnection.maxIFault', 1.0 / pPV.MinModelVoltagePU);
//        if FD.Separate then StartFreeInstance (SshPrf, 'PowerElectronicsConnection', pPV.UUID);
                DoubleNode(SshPrf, 'PowerElectronicsConnection.p', pPV.Presentkw * 1000.0);
                DoubleNode(SshPrf, 'PowerElectronicsConnection.q', pPV.Presentkvar * 1000.0);
                ConverterControlEnum(SshPrf, pPV.VarMode, pPV.UsingCIMDynamics);
//        if FD.Separate then EndInstance (SshPrf, 'PowerElectronicsConnection');
                DoubleNode(EpPrf, 'PowerElectronicsConnection.ratedS', pPV.PVSystemVars.fkvarating * 1000.0);
                if pPV.nphases = 1 then
                    DoubleNode(EpPrf, 'PowerElectronicsConnection.ratedU', pPV.Presentkv * 1000.0 * sqrt(3.0))
                else
                    DoubleNode(EpPrf, 'PowerElectronicsConnection.ratedU', pPV.Presentkv * 1000.0);

                DoubleNode(EpPrf, 'PowerElectronicsConnection.maxQ', pPV.PVSystemVars.Fkvarlimit * 1000.0);
                DoubleNode(EpPrf, 'PowerElectronicsConnection.minQ', -pPV.PVSystemVars.FkvarlimitNeg * 1000.0);
                UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
                EndInstance(FunPrf, 'PowerElectronicsConnection');
                AttachSolarPhases(pPV, geoUUID);
                // we want the location using PV unit name
                WriteReferenceTerminals(pPV, pPV.UUID);
                s := pPV.LocalName;
                pPV.LocalName := pName1.LocalName;
                WritePositions(pPV, geoUUID, crsUUID);
                pPV.LocalName := s;
            end;
            pPV := ActiveCircuit.PVSystems.Next;
        end;

        pBat := ActiveCircuit.StorageElements.First;
        while pBat <> NIL do
        begin
            if pBat.Enabled then
            begin
                pName1.LocalName := pBat.Name; // + '_Cells';
                pName1.UUID := GetDevUuid(Battery, pBat.LocalName, 1);
                StartInstance(FunPrf, 'BatteryUnit', pName1);
                DoubleNode(EpPrf, 'PowerElectronicsUnit.maxP', (pBat.StorageVars.kwRating * pBat.pctKwOut / 100.0) * 1000.0);
                DoubleNode(EpPrf, 'PowerElectronicsUnit.minP', -(pBat.StorageVars.kwRating * pBat.pctKwIn / 100.0) * 1000.0);
                DoubleNode(SshPrf, 'BatteryUnit.ratedE', pBat.StorageVars.kwhRating * 1000.0);
                DoubleNode(SshPrf, 'BatteryUnit.storedE', pBat.StorageVars.kwhStored * 1000.0);
                BatteryStateEnum(SshPrf, pBat.StorageState);
                geoUUID := GetDevUuid(BatteryLoc, pBat.localName, 1);
                UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
                EndInstance(FunPrf, 'BatteryUnit');
                StartInstance(FunPrf, 'PowerElectronicsConnection', pBat);
                CircuitNode(FunPrf, ActiveCircuit);
                RefNode(FunPrf, 'PowerElectronicsConnection.PowerElectronicsUnit', pName1);
                DoubleNode(EpPrf, 'PowerElectronicsConnection.maxIFault', 1.0 / pBat.MinModelVoltagePU);
                DoubleNode(SshPrf, 'PowerElectronicsConnection.p', pBat.Presentkw * 1000.0);
                DoubleNode(SshPrf, 'PowerElectronicsConnection.q', pBat.Presentkvar * 1000.0);
                ConverterControlEnum(SshPrf, pBat.VarMode, pBat.UsingCIMDynamics);
                DoubleNode(EpPrf, 'PowerElectronicsConnection.ratedS', pBat.kvarating * 1000.0);
                if pBat.nphases = 1 then
                    DoubleNode(EpPrf, 'PowerElectronicsConnection.ratedU', pBat.Presentkv * 1000.0 * sqrt(3.0))
                else
                    DoubleNode(EpPrf, 'PowerElectronicsConnection.ratedU', pBat.Presentkv * 1000.0);

                DoubleNode(EpPrf, 'PowerElectronicsConnection.maxQ', Math.Min(pBat.StorageVars.Fkvarlimit, pBat.StorageVars.FkVArating) * 1000.0);
                DoubleNode(EpPrf, 'PowerElectronicsConnection.minQ', -Math.Min(pBat.StorageVars.FkvarlimitNeg, pBat.StorageVars.FkVArating) * 1000.0);                
                UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
                EndInstance(FunPrf, 'PowerElectronicsConnection');
                AttachStoragePhases(pBat, geoUUID);
                // we want the location using battery unit name
                WriteReferenceTerminals(pBat, pBat.UUID);
                s := pBat.LocalName;
                pBat.LocalName := pName1.LocalName;
                WritePositions(pBat, geoUUID, crsUUID);
                pBat.LocalName := s;
            end;
            pBat := ActiveCircuit.StorageElements.Next;
        end;

        with ActiveCircuit do
        begin
            if (InvControls.Count > 0) or (ExpControls.Count > 0) then
            begin
                pI1547 := TIEEE1547Controller.Create(self);
                pInv := InvControls.First;
                while pInv <> NIL do
                begin
                    if pInv.Enabled then
                    begin
                        pI1547.PullFromInvControl(pInv);
                        pI1547.WriteCIM(DynPrf);
                    end;
                    pInv := InvControls.Next;
                end;
                pExp := ExpControls.First;
                while pExp <> NIL do
                begin
                    if pExp.Enabled then
                    begin
                        pI1547.PullFromExpControl(pExp);
                        pI1547.WriteCIM(DynPrf);
                    end;
                    pExp := ExpControls.Next;
                end;
                pI1547.Free;
            end;
        end;

        pVsrc := ActiveCircuit.Sources.First; // pIsrc are in the same list
        while pVsrc <> NIL do
        begin
            if pVsrc.ClassNameIs('TVSourceObj') then
                if pVsrc.Enabled then
                    with pVsrc do
                    begin
                        Zs := Z.AvgDiagonal;
                        Zm := Z.AvgOffDiagonal;
                        Rs := Zs.re;
                        Rm := Zm.re;
                        Xs := Zs.im;
                        Xm := Zm.im;
                        v1 := pVsrc.NPhases;
                        if v1 > 1.0 then
                        begin
                            R1 := Rs - Rm;
                            X1 := Xs - Xm;
                            R0 := Rs + (v1 - 1.0) * Rm;
                            X0 := Xs + (v1 - 1.0) * Xm;
                        end
                        else
                        begin
                            R1 := Rs;
                            X1 := Xs;
                            R0 := Rs;
                            X0 := Xs;
                        end;

                        StartInstance(FunPrf, 'EnergySource', pVsrc);
                        CircuitNode(FunPrf, ActiveCircuit);
                        VbaseNode(FunPrf, pVsrc);
                        DoubleNode(EpPrf, 'EnergySource.nominalVoltage', 1000 * kVbase);
                        DoubleNode(SshPrf, 'EnergySource.voltageMagnitude', 1000 * kVbase * PerUnit);
                        DoubleNode(SshPrf, 'EnergySource.voltageAngle', TwoPi * Angle / 360.0);
                        DoubleNode(EpPrf, 'EnergySource.r', R1);
                        DoubleNode(EpPrf, 'EnergySource.x', X1);
                        DoubleNode(EpPrf, 'EnergySource.r0', R0);
                        DoubleNode(EpPrf, 'EnergySource.x0', X0);
                        geoUUID := GetDevUuid(SrcLoc, pVsrc.LocalName, 1);
                        UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
                        EndInstance(FunPrf, 'EnergySource');
//          AttachPhases (F, pVsrc, 1, 'EnergySource');
                        WriteTerminals(pVsrc, geoUUID, crsUUID);
                    end;
            pVsrc := ActiveCircuit.Sources.Next;
        end;

        pCap := ActiveCircuit.ShuntCapacitors.First;
        while pCap <> NIL do
        begin
            if pCap.Enabled then
            begin
                StartInstance(FunPrf, 'LinearShuntCompensator', pCap);
                CircuitNode(FunPrf, ActiveCircuit);
                VbaseNode(FunPrf, pCap);
                with pCap do
                begin
                    val := 0.001 * Totalkvar / NomKV / NomKV / NumSteps;
                    DoubleNode(EpPrf, 'ShuntCompensator.nomU', 1000.0 * NomKV);
                    DoubleNode(EpPrf, 'LinearShuntCompensator.bPerSection', val);
                    DoubleNode(EpPrf, 'LinearShuntCompensator.gPerSection', 0.0);

                    val := 0.0;
                    pCapC := ActiveCircuit.CapControls.First;
                    while (pCapC <> NIL) do
                    begin
                        if pCapC.This_Capacitor = pCap then
                            val := pCapC.OnDelayVal;
                        pCapC := ActiveCircuit.CapControls.Next;
                    end;
                    DoubleNode(EpPrf, 'ShuntCompensator.aVRDelay', val);

                    if Connection = TCapacitorConnection.Delta then
                    begin
                        ShuntConnectionKindNode(FunPrf, 'ShuntCompensator', 'Y');
                        BooleanNode(FunPrf, 'ShuntCompensator.grounded', TRUE);  // TODO - check bus 2
                        DoubleNode(EpPrf, 'LinearShuntCompensator.b0PerSection', val);
                    end
                    else
                    begin
                        ShuntConnectionKindNode(FunPrf, 'ShuntCompensator', 'D');
                        BooleanNode(FunPrf, 'LinearShuntCompensator.grounded', FALSE);
                        DoubleNode(EpPrf, 'LinearShuntCompensator.b0PerSection', 0.0);
                    end;
                    DoubleNode(EpPrf, 'LinearShuntCompensator.g0PerSection', 0.0);
                    IntegerNode(EpPrf, 'ShuntCompensator.normalSections', NumSteps);
                    IntegerNode(EpPrf, 'ShuntCompensator.maximumSections', NumSteps);
                    val := 0;
                    for i := 1 to NumSteps do
                        if States[i] > 0 then
                            val := val + 1.0;
                    DoubleNode(SshPrf, 'ShuntCompensator.sections', val);
                    geoUUID := GetDevUuid(CapLoc, pCap.localName, 1);
                    UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
                    EndInstance(FunPrf, 'LinearShuntCompensator');
                    AttachCapPhases(pCap, geoUUID, val);
                    WriteTerminals(pCap, geoUUID, crsUUID, pCap.NormAmps, pCap.EmergAmps);
                end;
            end;
            pCap := ActiveCircuit.ShuntCapacitors.Next;
        end;

        pCapC := ActiveCircuit.CapControls.First;
        while (pCapC <> NIL) do
        begin
            with pCapC do
            begin
                StartInstance(FunPrf, 'RegulatingControl', pCapC);
                UuidNode(GeoPrf, 'PowerSystemResource.Location', GetDevUuid(CapLoc, This_Capacitor.Name, 1));
                RefNode(FunPrf, 'RegulatingControl.RegulatingCondEq', This_Capacitor);
                UuidNode(FunPrf, 'RegulatingControl.Terminal', GetTermUuid(MonitoredElement, ElementTerminal));
                s := FirstPhaseString(MonitoredElement, 1);
                if PTPhase > 0 then
                    MonitoredPhaseNode(FunPrf, Char(Ord(s[1]) + PTPhase - 1))
                else
                    MonitoredPhaseNode(FunPrf, Char(Ord(s[1]))); // TODO - average, min and max unsupported in CIM
                val := 1.0;
                if CapControlType = PFCONTROL then
                begin
                    v1 := PfOnValue;
                    v2 := PfOffValue
                end
                else
                begin
                    v1 := OnValue;
                    v2 := OffValue;
                    if CapControlType = KVARCONTROL then
                        val := 1000.0;
                    if CapControlType = CURRENTCONTROL then
                        val := CTRatioVal;
                    if CapControlType = VOLTAGECONTROL then
                        val := PTRatioVal
                end;
                case CapControlType of
                    CURRENTCONTROL:
                        RegulatingControlEnum(EpPrf, 'currentFlow');
                    VOLTAGECONTROL:
                        RegulatingControlEnum(EpPrf, 'voltage');
                    KVARCONTROL:
                        RegulatingControlEnum(EpPrf, 'reactivePower');
                    TIMECONTROL:
                        RegulatingControlEnum(EpPrf, 'timeScheduled');
                    PFCONTROL:
                        RegulatingControlEnum(EpPrf, 'powerFactor');
                    USERCONTROL:
                        RegulatingControlEnum(EpPrf, 'userDefined'); // i.e. unsupported in CIM
                end;
                BooleanNode(EpPrf, 'RegulatingControl.discrete', TRUE);
                BooleanNode(EpPrf, 'RegulatingControl.enabled', Enabled);
                DoubleNode(EpPrf, 'RegulatingControl.targetValue', val * 0.5 * (v1 + v2));
                DoubleNode(EpPrf, 'RegulatingControl.targetDeadband', val * (v2 - v1));
                EndInstance(FunPrf, 'RegulatingControl');
            end;
            pCapC := ActiveCircuit.CapControls.Next;
        end;

    // size the auxiliary winding, mesh, and core lists for transformer export
        maxWdg := 3; // start with the size of autos
        pXf := ActiveCircuit.Transformers.First;
        while pXf <> NIL do
        begin
            if pXf.Enabled then
                if pXf.NumWindings > maxWdg then
                    maxWdg := pXf.NumWindings;
            pXf := ActiveCircuit.Transformers.Next;
        end;

        if MaxWdg > 0 then
        begin
            SetLength(WdgList, maxWdg);
            SetLength(CoreList, maxWdg);
            SetLength(MeshList, (maxWdg - 1) * maxWdg div 2);
            for i := 1 to maxWdg do
                WdgList[i - 1] := TNamedObject.Create('dummy');
            CoreList[0] := TNamedObject.Create('dummy');
            for i := 1 to ((maxWdg - 1) * maxWdg div 2) do
                MeshList[i - 1] := TNamedObject.Create('dummy');
        end;

    // do the autotransformers as balanced, three-phase autos, PowerTransformerEnd(s), mesh impedances and core admittances
    // only considering 2 windings, vector group YNa, or 3 windings, vector group YNad1
        pAuto := ActiveCircuit.AutoTransformers.First;
        while pAuto <> NIL do
        begin
            if pAuto.Enabled then
                with pAuto do
                begin
                    if XfmrBank = '' then
                        sBank := '=' + pAuto.Name
                    else
                        sBank := XfmrBank;
                    pBank := GetBank(sBank);
                    if pBank = NIL then
                    begin
                        pBank := TCIMBankObject.Create(maxWdg);
                        pBank.localName := sBank;
                        pBank.UUID := GetDevUuid(Bank, sBank, 0);
                        AddBank(pBank);
                    end;
                    pBank.AddAutoTransformer(self, pAuto);
                    geoUUID := GetDevUuid(XfLoc, pAuto.Name, 1);
                    WritePositions(pAuto, geoUUID, crsUUID);
                    // pre-make the winding, mesh and core name objects for easy reference
                    for i := 1 to NumWindings do
                    begin
                        WdgList[i - 1].localName := pAuto.Name + '_End_' + IntToStr(i);
                        WdgList[i - 1].UUID := GetDevUuid(Wdg, pAuto.Name, i);
                    end;
                    CoreList[0].LocalName := pAuto.Name + '_Yc';
                    CoreList[0].UUID := GetDevUuid(XfCore, pAuto.Name, 1);
                    for i := 1 to ((maxWdg - 1) * maxWdg div 2) do
                    begin
                        MeshList[i - 1].localName := pAuto.Name + '_Zsc_' + IntToStr(i);
                        MeshList[i - 1].UUID := GetDevUuid(XfMesh, pAuto.Name, i);
                    end;
                    val := BaseKVLL[1]; // write core Y
                    zbase := 1000.0 * val * val / WdgKva[1];
                    StartInstance(EpPrf, 'TransformerCoreAdmittance', CoreList[0]);
                    val := pAuto.pctNoLoadLoss / 100.0 / zbase;
                    DoubleNode(EpPrf, 'TransformerCoreAdmittance.g', val);
                    DoubleNode(EpPrf, 'TransformerCoreAdmittance.g0', val);
                    val := pAuto.pctImag / 100.0 / zbase;
                    DoubleNode(EpPrf, 'TransformerCoreAdmittance.b', val);
                    DoubleNode(EpPrf, 'TransformerCoreAdmittance.b0', val);
                    RefNode(EpPrf, 'TransformerCoreAdmittance.TransformerEnd', WdgList[0]);
                    EndInstance(EpPrf, 'TransformerCoreAdmittance');
                    seq := 1; // write mesh Z
                    for i := 1 to NumWindings do
                    begin
                        for k := i + 1 to NumWindings do
                        begin
                            val := BaseKVLL[i];
                            zbase := 1000.0 * val * val / WdgKva[i];
                            StartInstance(EpPrf, 'TransformerMeshImpedance', MeshList[seq - 1]);
                            val := zbase * (WdgResistance[i] + WdgResistance[k]);
                            DoubleNode(EpPrf, 'TransformerMeshImpedance.r', val);
                            DoubleNode(EpPrf, 'TransformerMeshImpedance.r0', val);
                            val := zbase * XscVal[seq];
                            inc(seq);
                            DoubleNode(EpPrf, 'TransformerMeshImpedance.x', val);
                            DoubleNode(EpPrf, 'TransformerMeshImpedance.x0', val);
                            RefNode(EpPrf, 'TransformerMeshImpedance.FromTransformerEnd', WdgList[i - 1]);
                            RefNode(EpPrf, 'TransformerMeshImpedance.ToTransformerEnd', WdgList[k - 1]);
                            EndInstance(EpPrf, 'TransformerMeshImpedance');
                        end;
                    end;
        // write the Ends, and a Terminal with operational limit for each End
                    for i := 1 to NumWindings do
                    begin
                        StartInstance(FunPrf, 'PowerTransformerEnd', WdgList[i - 1]);
                        RefNode(FunPrf, 'PowerTransformerEnd.PowerTransformer', pBank);
                        DoubleNode(EpPrf, 'PowerTransformerEnd.ratedS', 1000 * WdgKva[i]);
                        DoubleNode(EpPrf, 'PowerTransformerEnd.ratedU', 1000 * Winding^[i].kvll);
                        zbase := 1000.0 * BaseKVLL[i] * BaseKVLL[i] / WdgKva[i];
                        DoubleNode(EpPrf, 'PowerTransformerEnd.r', zbase * WdgResistance[i]);
                        if i = 1 then
                        begin
                            WindingConnectionKindNode(FunPrf, 'Y');
                            IntegerNode(FunPrf, 'PowerTransformerEnd.phaseAngleClock', 0);
                            BooleanNode(FunPrf, 'TransformerEnd.grounded', FALSE);
                        end
                        else
                        if i = 2 then
                        begin
                            WindingConnectionKindNode(FunPrf, 'A');
                            IntegerNode(FunPrf, 'PowerTransformerEnd.phaseAngleClock', 0);
                            BooleanNode(FunPrf, 'TransformerEnd.grounded', TRUE);
                            DoubleNode(EpPrf, 'TransformerEnd.rground', 0.0); // no rneut or xneut for autotrans
                            DoubleNode(EpPrf, 'TransformerEnd.xground', 0.0);
                        end
                        else
                        begin
                            WindingConnectionKindNode(FunPrf, 'D');
                            IntegerNode(FunPrf, 'PowerTransformerEnd.phaseAngleClock', 1);
                            BooleanNode(FunPrf, 'TransformerEnd.grounded', FALSE);
                        end;
                        IntegerNode(FunPrf, 'TransformerEnd.endNumber', i);
                        j := pAuto.Terminals[i - 1].BusRef;
                        pName2.LocalName := pAuto.Name + '_T' + IntToStr(i);
                        pName2.UUID := GetTermUuid(pAuto, i);
                        RefNode(FunPrf, 'TransformerEnd.Terminal', pName2);
                        UuidNode(FunPrf, 'TransformerEnd.BaseVoltage', GetBaseVUuid(sqrt(3.0) * ActiveCircuit.Buses^[j].kVBase));
                        EndInstance(FunPrf, 'PowerTransformerEnd');
          // write the Terminal for this End
                        StartInstance(FunPrf, 'Terminal', pName2);
                        RefNode(FunPrf, 'Terminal.ConductingEquipment', pBank);
                        IntegerNode(FunPrf, 'ACDCTerminal.sequenceNumber', i);
                        FD.WriteCimLn(TopoPrf, Format('  <cim:Terminal.ConnectivityNode rdf:resource="#%s"/>',
                            [ActiveCircuit.Buses[j].CIM_ID]));
                        if i = 1 then
                        begin   // write the current limit on HV winding, assuming that's winding 1
                            LimitName := GetOpLimIName(pAuto.NormAmps, pAuto.EmergAmps);
                            pILimit := GetOpLimit(LimitName);
                            if pILimit = NIL then
                            begin
                                pILimit := TCIMOpLimitObject.Create(pAuto.NormAmps, pAuto.EmergAmps);
                                pILimit.localName := LimitName;
                                pILimit.UUID := GetDevUuid(OpLimI, LimitName, 0);
                                AddOpLimit(pILimit);
                            end;
                            LimiTUuid := GetDevUuid(OpLimI, LimitName, 0);
                            UuidNode(FunPrf, 'ACDCTerminal.OperationalLimitSet', LimiTUuid);
                        end;
                        EndInstance(FunPrf, 'Terminal');
                    end;
                end;
            pAuto := ActiveCircuit.AutoTransformers.Next;
        end;

        // begin the transformers; 
            //   1. if balanced three-phase and no XfmrCode, use PowerTransformerEnd(s), mesh impedances and core admittances with no tanks
        //   2. with XfmrCode, write TransformerTank, TransformerTankEnd(s) and references to TransformerTankInfoInfo
        //   3. otherwise, write TransformerTank, then create and reference TransformerTankInfo classes

        // for case 3, it's better to identify and create the info classes first
        //    TODO: side effect is that these transformers will reference XfmrCode until the text file is reloaded. Solution results should be the same.
        pXf := ActiveCircuit.Transformers.First;
        while pXf <> NIL do
        begin
            if pXf.Enabled then
            begin
                if (pXf.XfmrCodeObj = NIL) and (pXf.NPhases <> 3) then
                begin
                    sBank := 'CIMXfmrCode_' + pXf.Name;
                    clsXfCd.NewObject(sBank);
                    pXfCd := clsXfCd.Find(sBank);
                    pXfCd.UUID := GetDevUuid(TankInfo, pXfCd.Name, 1);
                    pXfCd.PullFromTransformer(pXf);
                    pXf.XfmrCodeObj := pXfCd;
                end;
            end;
            pXf := ActiveCircuit.Transformers.Next;
        end;

        // write all the XfmrCodes first (CIM TransformerTankInfo)
        pXfCd := clsXfCd.ElementList.First;
        while pXfCd <> NIL do
        begin
            WriteXfmrCode(pXfCd);
            pXfCd := clsXfCd.ElementList.Next;
        end;

        // create all the banks (CIM PowerTransformer) for regular transformers
        pXf := ActiveCircuit.Transformers.First;
        while pXf <> NIL do
        begin
            if pXf.Enabled then
            begin
                if pXf.XfmrBank = '' then
                    sBank := '=' + pXf.Name
                else
                    sBank := pXf.XfmrBank;
                pBank := GetBank(sBank);
                if pBank = NIL then
                begin
                    pBank := TCIMBankObject.Create(maxWdg);
                    pBank.localName := sBank;
                    pBank.UUID := GetDevUuid(Bank, sBank, 0);
                    AddBank(pBank);
                end;
            end;
            pXf := ActiveCircuit.Transformers.Next;
        end;

        pXf := ActiveCircuit.Transformers.First;
        while pXf <> NIL do
        begin
            if pXf.Enabled then
            begin
                if pXf.XfmrBank = '' then
                    sBank := '=' + pXf.Name
                else
                    sBank := pXf.XfmrBank;
                pBank := GetBank(sBank);
                if pBank = NIL then
                begin
                    pBank := TCIMBankObject.Create(maxWdg);
                    pBank.localName := sBank;
                    pBank.UUID := GetDevUuid(Bank, sBank, 0);
                    AddBank(pBank);
                end;
            end;
            pXf := ActiveCircuit.Transformers.Next;
        end;

        // write all the transformers, according to the three cases
        pXf := ActiveCircuit.Transformers.First;
        while pXf <> NIL do
        begin
            if pXf.Enabled then
                with pXf do
                begin
                    // collect this transformer into tanks and banks, and make a location
                    if pXf.XfmrBank = '' then
                        sBank := '=' + pXf.Name
                    else
                        sBank := pXf.XfmrBank;
                    bTanks := TRUE;  // defaults to case 2 or 3 if XfmrCode exists
                    if (pXf.XfmrCodeObj = NIL) and (pXf.NPhases = 3) then
                        bTanks := FALSE; // case 1, balanced three-phase

                    pBank := GetBank(sBank);
                    pBank.AddTransformer(self, pXf);
                    geoUUID := GetDevUuid(XfLoc, pXf.Name, 1);

                    if bTanks then
                    begin
                        StartInstance(FunPrf, 'TransformerTank', pXf);
                        CircuitNode(FunPrf, ActiveCircuit);
                        pXfCd := pXf.XfmrCodeObj as TXfmrCodeObj;
                        RefNode(FunPrf, 'TransformerTank.TransformerTankInfo', pXfCd);
                        RefNode(FunPrf, 'TransformerTank.PowerTransformer', pBank);
                        UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
                        EndInstance(FunPrf, 'TransformerTank');
                        WritePositions(pXf, geoUUID, crsUUID);
                    end
                    else
                    begin
                        WritePositions(pXf, geoUUID, crsUUID);
                    end;

                    // make the winding, mesh and core name objects for easy reference
                    for i := 1 to NumWindings do
                    begin
                        WdgList[i - 1].localName := pXf.Name + '_End_' + IntToStr(i);
                        WdgList[i - 1].UUID := GetDevUuid(Wdg, pXf.Name, i);
                    end;
                    CoreList[0].LocalName := pXf.Name + '_Yc';
                    CoreList[0].UUID := GetDevUuid(XfCore, pXf.Name, 1);
                    for i := 1 to ((maxWdg - 1) * maxWdg div 2) do
                    begin
                        MeshList[i - 1].localName := pXf.Name + '_Zsc_' + IntToStr(i);
                        MeshList[i - 1].UUID := GetDevUuid(XfMesh, pXf.Name, i);
                    end;

                    if not bTanks then
                    begin // write the mesh impedances and core admittances
                        val := BaseKVLL[1];
                        zbase := 1000.0 * val * val / WdgKva[1];
                        StartInstance(EpPrf, 'TransformerCoreAdmittance', CoreList[0]);
                        val := pXf.pctNoLoadLoss / 100.0 / zbase;
                        DoubleNode(EpPrf, 'TransformerCoreAdmittance.g', val);
                        DoubleNode(EpPrf, 'TransformerCoreAdmittance.g0', val);
                        val := pXf.pctImag / 100.0 / zbase;
                        DoubleNode(EpPrf, 'TransformerCoreAdmittance.b', val);
                        DoubleNode(EpPrf, 'TransformerCoreAdmittance.b0', val);
                        RefNode(EpPrf, 'TransformerCoreAdmittance.TransformerEnd', WdgList[0]);
                        EndInstance(EpPrf, 'TransformerCoreAdmittance');
                        seq := 1; // write mesh Z
                        for i := 1 to NumWindings do
                        begin
                            for k := i + 1 to NumWindings do
                            begin
                                val := BaseKVLL[i];
                                zbase := 1000.0 * val * val / WdgKva[i];
                                StartInstance(EpPrf, 'TransformerMeshImpedance', MeshList[seq - 1]);
                                val := zbase * (WdgResistance[i] + WdgResistance[k]);
                                DoubleNode(EpPrf, 'TransformerMeshImpedance.r', val);
                                DoubleNode(EpPrf, 'TransformerMeshImpedance.r0', val);
                                val := zbase * XscVal[seq];
                                inc(seq);
                                DoubleNode(EpPrf, 'TransformerMeshImpedance.x', val);
                                DoubleNode(EpPrf, 'TransformerMeshImpedance.x0', val);
                                RefNode(EpPrf, 'TransformerMeshImpedance.FromTransformerEnd', WdgList[i - 1]);
                                RefNode(EpPrf, 'TransformerMeshImpedance.ToTransformerEnd', WdgList[k - 1]);
                                EndInstance(EpPrf, 'TransformerMeshImpedance');
                            end;
                        end;
                    end;

                    // write the Ends, and a Terminal for each End
                    for i := 1 to NumWindings do
                    begin
                        if bTanks then
                        begin
                            StartInstance(FunPrf, 'TransformerTankEnd', WdgList[i - 1]);
                            XfmrTankPhasesAndGround(FunPrf, EpPrf, pXf, i);
                            RefNode(FunPrf, 'TransformerTankEnd.TransformerTank', pXf);
                        end
                        else
                        begin
                            StartInstance(FunPrf, 'PowerTransformerEnd', WdgList[i - 1]);
                            RefNode(FunPrf, 'PowerTransformerEnd.PowerTransformer', pBank);
                            DoubleNode(EpPrf, 'PowerTransformerEnd.ratedS', 1000 * WdgKva[i]);
                            DoubleNode(EpPrf, 'PowerTransformerEnd.ratedU', 1000 * Winding^[i].kvll);
                            zbase := 1000.0 * BaseKVLL[i] * BaseKVLL[i] / WdgKva[i];
                            DoubleNode(EpPrf, 'PowerTransformerEnd.r', zbase * WdgResistance[i]);
                            if Winding^[i].Connection = 1 then
                                WindingConnectionKindNode(FunPrf, 'D')
                            else
                            if (Winding^[i].Rneut > 0.0) or (Winding^[i].Xneut > 0.0) then
                                WindingConnectionKindNode(FunPrf, 'Yn')
                            else
                                WindingConnectionKindNode(FunPrf, 'Y');
                            if Winding^[i].Connection <> Winding^[1].Connection then  // TODO - this assumes HV winding first, and normal usages
                                IntegerNode(FunPrf, 'PowerTransformerEnd.phaseAngleClock', 1)
                            else
                                IntegerNode(FunPrf, 'PowerTransformerEnd.phaseAngleClock', 0);
                            j := (i - 1) * pXf.NConds + pXf.Nphases + 1;
                            if (Winding^[i].Connection = 1) then
                            begin // delta
                                BooleanNode(FunPrf, 'TransformerEnd.grounded', FALSE);
                            end
                            else
                            if (pXf.NodeRef^[j] = 0) then
                            begin // last conductor is grounded solidly
                                BooleanNode(FunPrf, 'TransformerEnd.grounded', TRUE);
                                DoubleNode(EpPrf, 'TransformerEnd.rground', 0.0);
                                DoubleNode(EpPrf, 'TransformerEnd.xground', 0.0);
                            end
                            else
                            if (Winding^[i].Rneut < 0.0) then
                            begin // probably wye ungrounded
                                BooleanNode(FunPrf, 'TransformerEnd.grounded', FALSE);
                            end
                            else
                            begin // not delta, not wye solidly grounded or ungrounded
                                BooleanNode(FunPrf, 'TransformerEnd.grounded', TRUE);
                                DoubleNode(EpPrf, 'TransformerEnd.rground', Winding^[i].Rneut);
                                DoubleNode(EpPrf, 'TransformerEnd.xground', Winding^[i].Xneut);
                            end;
                        end;
                        IntegerNode(FunPrf, 'TransformerEnd.endNumber', i);
                        j := pXf.Terminals[i - 1].BusRef;
                        pName2.LocalName := pXf.Name + '_T' + IntToStr(i);
                        pName2.UUID := GetTermUuid(pXf, i);
                        RefNode(FunPrf, 'TransformerEnd.Terminal', pName2);
                        UuidNode(FunPrf, 'TransformerEnd.BaseVoltage', GetBaseVUuid(sqrt(3.0) * ActiveCircuit.Buses^[j].kVBase));
                        if bTanks then
                            EndInstance(FunPrf, 'TransformerTankEnd')
                        else
                            EndInstance(FunPrf, 'PowerTransformerEnd');
                        // write the Terminal for this End
                        StartInstance(FunPrf, 'Terminal', pName2);
                        RefNode(FunPrf, 'Terminal.ConductingEquipment', pBank);
                        IntegerNode(FunPrf, 'ACDCTerminal.sequenceNumber', i);
                        FD.WriteCimLn(TopoPrf, Format('  <cim:Terminal.ConnectivityNode rdf:resource="#%s"/>',
                            [ActiveCircuit.Buses[j].CIM_ID]));
                        if i = 1 then
                        begin   // write the current limit on HV winding, assuming that's winding 1
                            LimitName := GetOpLimIName(pXf.NormAmps, pXf.EmergAmps);
                            pILimit := GetOpLimit(LimitName);
                            if pILimit = NIL then
                            begin
                                pILimit := TCIMOpLimitObject.Create(pXf.NormAmps, pXf.EmergAmps);
                                pILimit.localName := LimitName;
                                pILimit.UUID := GetDevUuid(OpLimI, LimitName, 0);
                                AddOpLimit(pILimit);
                            end;
                            LimiTUuid := GetDevUuid(OpLimI, LimitName, 0);
                            UuidNode(FunPrf, 'ACDCTerminal.OperationalLimitSet', LimiTUuid);
                        end;
                        EndInstance(FunPrf, 'Terminal');
                    end;
                end;
            pXf := ActiveCircuit.Transformers.Next;
        end;

        // finally, write all the transformer banks (CIM PowerTransformer)
        for i := Low(BankList) to High(BankList) do
        begin
            pBank := BankList[i];
            if pBank = NIL then
                break;
            pBank.BuildVectorGroup;
            // we don't want = sign in the name.  These should still be unique names
            if AnsiPos('=', pBank.localName) = 1 then
                pBank.localName := Copy(pBank.localName, 2, MaxInt);
            StartInstance(FunPrf, 'PowerTransformer', pBank);
            CircuitNode(FunPrf, ActiveCircuit);
            StringNode(FunPrf, 'PowerTransformer.vectorGroup', pBank.vectorGroup);
            UuidNode(GeoPrf, 'PowerSystemResource.Location',
                GetDevUuid(XfLoc, pBank.pd_unit.Name, 1));
            EndInstance(FunPrf, 'PowerTransformer');
        end;

        WdgList := NIL;
        CoreList := NIL;
        MeshList := NIL;

        // voltage regulators
        pReg := ActiveCircuit.RegControls.First;
        while (pReg <> NIL) do
        begin
            with pReg do
            begin
                v1 := Transformer.BaseVoltage[TrWinding] / PTRatio;
                pName2.LocalName := pReg.LocalName + '_Ctrl';
                pName2.UUID := GetDevUuid(TapCtrl, pReg.LocalName, 1);
                StartInstance(FunPrf, 'TapChangerControl', pName2);
                RegulatingControlEnum(FunPrf, 'voltage');
                UuidNode(FunPrf, 'RegulatingControl.Terminal', GetTermUuid(Transformer, TrWinding));
                MonitoredPhaseNode(FunPrf, FirstPhaseString(Transformer, TrWinding));
                BooleanNode(FunPrf, 'RegulatingControl.enabled', pReg.Enabled);
                BooleanNode(EpPrf, 'RegulatingControl.discrete', TRUE);
                DoubleNode(EpPrf, 'RegulatingControl.targetValue', Vreg);
                DoubleNode(EpPrf, 'RegulatingControl.targetDeadband', Bandwidth);
                BooleanNode(EpPrf, 'TapChangerControl.lineDropCompensation', LDCActive);
                DoubleNode(EpPrf, 'TapChangerControl.lineDropR', R);
                DoubleNode(EpPrf, 'TapChangerControl.lineDropX', X);
                if IsReversible then
                begin
                    DoubleNode(EpPrf, 'TapChangerControl.reverseLineDropR', revR);
                    DoubleNode(EpPrf, 'TapChangerControl.reverseLineDropX', revX)
                end
                else
                begin
                    DoubleNode(EpPrf, 'TapChangerControl.reverseLineDropR', 0.0);
                    DoubleNode(EpPrf, 'TapChangerControl.reverseLineDropX', 0.0)
                end;
                if VLimitActive then 
                    // maxLimitVoltage only in OpenDSS
                    DoubleNode (EpPrf, 'TapChangerControl.maxLimitVoltage', Vlimit)
                else 
                    DoubleNode (EpPrf, 'TapChangerControl.maxLimitVoltage', MaxTap * v1);
                DoubleNode (EpPrf, 'TapChangerControl.minLimitVoltage', MinTap * v1);                
                
                UuidNode(GeoPrf, 'PowerSystemResource.Location',
                    GetDevUuid(XfLoc, Transformer.Name, 1));
                EndInstance(FunPrf, 'TapChangerControl');

                StartInstance(FunPrf, 'RatioTapChanger', pReg);
                UuidNode(FunPrf, 'RatioTapChanger.TransformerEnd',
                    GetDevUuid(Wdg, Transformer.Name, TrWinding));
                UuidNode(FunPrf, 'TapChanger.TapChangerControl', pName2.UUID);
                DoubleNode(EpPrf, 'RatioTapChanger.stepVoltageIncrement', 100.0 * TapIncrement);
                TransformerControlEnum(FunPrf, 'volt');
                IntegerNode(EpPrf, 'TapChanger.highStep', NumTaps div 2);
                IntegerNode(EpPrf, 'TapChanger.lowStep', -NumTaps div 2);
                IntegerNode(EpPrf, 'TapChanger.neutralStep', 0);
                IntegerNode(EpPrf, 'TapChanger.normalStep', 0);
                DoubleNode(EpPrf, 'TapChanger.neutralU', v1 * PTRatio);
                DoubleNode(EpPrf, 'TapChanger.initialDelay', TimeDelay);
                DoubleNode(EpPrf, 'TapChanger.subsequentDelay', TapDelay);
                BooleanNode(EpPrf, 'TapChanger.ltcFlag', TRUE);
                BooleanNode(SshPrf, 'TapChanger.controlEnabled', pReg.Enabled);
                DoubleNode(SshPrf, 'TapChanger.step', TapNum);
                DoubleNode(EpPrf, 'TapChanger.ptRatio', PTRatio);
                DoubleNode(EpPrf, 'TapChanger.ctRatio', CTRating / 0.2);
                DoubleNode(EpPrf, 'TapChanger.ctRating', CTRating);
                UuidNode(GeoPrf, 'PowerSystemResource.Location',
                    GetDevUuid(XfLoc, Transformer.Name, 1));
                EndInstance(FunPrf, 'RatioTapChanger');
            end;
            pReg := ActiveCircuit.RegControls.Next;
        end;

    // done with the transformers

        // series reactors, exported as SeriesCompensators
        pReac := ActiveCircuit.Reactors.First;
        while pReac <> NIL do
        begin
            if pReac.Enabled then
            begin
                StartInstance(FunPrf, 'SeriesCompensator', pReac);
                CircuitNode(FunPrf, ActiveCircuit);
                VbaseNode(FunPrf, pReac);
                geoUUID := GetDevUuid(ReacLoc, pReac.Name, 1);
                UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
                DoubleNode(EpPrf, 'SeriesCompensator.r', pReac.SimpleR);
                DoubleNode(EpPrf, 'SeriesCompensator.x', pReac.SimpleX);
                DoubleNode(EpPrf, 'SeriesCompensator.r0', pReac.SimpleR);
                DoubleNode(EpPrf, 'SeriesCompensator.x0', pReac.SimpleX);
                EndInstance(FunPrf, 'SeriesCompensator');
                // AttachLinePhases (F_, pReac); // for the 8500-node circuit, we only need 3 phase series reactors
                WriteTerminals(pReac, geoUUID, crsUUID, pReac.NormAmps, pReac.EmergAmps);
            end;
            pReac := ActiveCircuit.Reactors.Next;
        end;

        pLine := ActiveCircuit.Lines.First;
        while pLine <> NIL do
        begin
            if pLine.Enabled then
                with pLine do
                begin
                    bval := FALSE; // flag to write a "line code" of PULengthPhaseZ
                    v1 := To_Meters(pLine.LengthUnits);
                    geoUUID := GetDevUuid(LineLoc, pLine.Name, 1);
                    if IsSwitch then
                    begin
                        ParseSwitchClass(pLine, swtCls, ratedAmps, breakingAmps);
                        StartInstance(FunPrf, swtCls, pLine);
                        CircuitNode(FunPrf, ActiveCircuit);
                        VbaseNode(FunPrf, pLine);
                        if breakingAmps > 0.0 then
                            DoubleNode(EpPrf, 'ProtectedSwitch.breakingCapacity', breakingAmps); // Fuse and Sectionaliser don't have this, others do
                        DoubleNode(EpPrf, 'Switch.ratedCurrent', ratedAmps);
                        // some OpenDSS models have enabled=false to signal open switches, but we can't actually
                        // export them because disabled elements don't have terminal references in memory
                        if Enabled then
                        begin
                            BooleanNode(FunPrf, 'Switch.normalOpen', not pLine.Closed[0]);
                            BooleanNode(SshPrf, 'Switch.open', not pLine.Closed[0]);
                        end
                        else
                        begin
                            BooleanNode(FunPrf, 'Switch.normalOpen', TRUE);
                            BooleanNode(SshPrf, 'Switch.open', TRUE);
                        end;
                        BooleanNode(FunPrf, 'Switch.retained', TRUE);
                        UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
                        EndInstance(FunPrf, swtCls);
                        AttachSwitchPhases(pLine);
                    end
                    else
                    begin
                        StartInstance(FunPrf, 'ACLineSegment', pLine);
                        CircuitNode(FunPrf, ActiveCircuit);
                        VbaseNode(FunPrf, pLine);
                        if LineCodeSpecified then
                        begin
                            DoubleNode(FunPrf, 'Conductor.length', Len * v1);
                            LineCodeRefNode(EpPrf, clsLnCd, pLine.LineCodeObj);
                        end
                        else
                        if GeometrySpecified then
                        begin
                            DoubleNode(FunPrf, 'Conductor.length', Len * v1);
                            LineSpacingRefNode(CatPrf, pLine.LineGeometryObj);
                        end
                        else
                        if SpacingSpecified then
                        begin
                            DoubleNode(FunPrf, 'Conductor.length', Len * v1);
                            LineSpacingRefNode(CatPrf, pLine.LineSpacingObj);
                        end
                        else
                        begin
                            if SymComponentsModel and (NPhases = 3) then
                            begin
                                val := 1.0e-9 * TwoPi * BaseFrequency; // convert nF to mhos
                                DoubleNode(FunPrf, 'Conductor.length', 1.0); // we don't know the physical length
                                DoubleNode(EpPrf, 'ACLineSegment.r', Len * R1); // total ohms
                                DoubleNode(EpPrf, 'ACLineSegment.x', Len * X1);
                                DoubleNode(EpPrf, 'ACLineSegment.bch', Len * C1 * val);
                                DoubleNode(EpPrf, 'ACLineSegment.gch', 0.0);
                                DoubleNode(EpPrf, 'ACLineSegment.r0', Len * R0);
                                DoubleNode(EpPrf, 'ACLineSegment.x0', Len * X0);
                                DoubleNode(EpPrf, 'ACLineSegment.b0ch', Len * C0 * val);
                                DoubleNode(EpPrf, 'ACLineSegment.b0ch', 0.0);
                            end
                            else
                            begin
                                bval := TRUE;
                                pName1.LocalName := pLine.Name + '_PUZ';
                                pName1.UUID := GetDevUuid(PUZ, pLine.Name, 1);
                                RefNode(EpPrf, 'ACLineSegment.PerLengthImpedance', pName1);
              // TODO - we no longer have proper length units if matrices were specified
                                DoubleNode(FunPrf, 'Conductor.length', Len * v1);
                            end;
                        end;
                        UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
                        EndInstance(FunPrf, 'ACLineSegment');
                        if not (SymComponentsModel and (NPhases = 3)) then
                            AttachLinePhases(pLine);
                        if bVal = TRUE then
                        begin  // writing PuZ on the fly
                            StartInstance(EpPrf, 'PerLengthPhaseImpedance', pName1);
                            IntegerNode(EpPrf, 'PerLengthPhaseImpedance.conductorCount', NPhases);
                            EndInstance(EpPrf, 'PerLengthPhaseImpedance');
                            seq := 1;
                            for i := 1 to NPhases do
                            begin
                                for j := 1 to i do
                                begin
                                    StartFreeInstance(EpPrf, 'PhaseImpedanceData', GetDevUuid(ZData, pName1.LocalName, seq));
                                    RefNode(EpPrf, 'PhaseImpedanceData.PhaseImpedance', pName1);
                                    IntegerNode(EpPrf, 'PhaseImpedanceData.row', i);
                                    IntegerNode(EpPrf, 'PhaseImpedanceData.column', j);
                                    DoubleNode(EpPrf, 'PhaseImpedanceData.r', Z.GetElement(i, j).re / 1609.34);
                                    DoubleNode(EpPrf, 'PhaseImpedanceData.x', Z.GetElement(i, j).im / 1609.34);
                                    DoubleNode(EpPrf, 'PhaseImpedanceData.b', YC.GetElement(i, j).im / 1609.34);
                                    EndInstance(EpPrf, 'PhaseImpedanceData');
                                    inc(seq)
                                end;
                            end;
                        end;
                    end;
                    WriteTerminals(pLine, geoUUID, crsUUID, pLine.NormAmps, pLine.EmergAmps);
                end;
            pLine := ActiveCircuit.Lines.Next;
        end;

    // create the DSS-like load models
        id1_ConstkVA := GetDevUuid(LoadResp, 'ConstkVA', 1);
        id2_ConstZ := GetDevUuid(LoadResp, 'ConstZ', 1);
        id3_ConstPQuadQ := GetDevUuid(LoadResp, 'ConstPQuadQ', 1);
        id4_LinPQuadQ := GetDevUuid(LoadResp, 'LinPQuadQ', 1);
        id5_ConstI := GetDevUuid(LoadResp, 'ConstI', 1);
        id6_ConstPConstQ := GetDevUuid(LoadResp, 'ConstQ', 1);  // P can vary, Q not
        id7_ConstPConstX := GetDevUuid(LoadResp, 'ConstX', 1);

        WriteLoadModel('Constant kVA', id1_ConstkVA,
            0, 0, 100,
            0, 0, 100,
            0, 0);
        WriteLoadModel('Constant Z', id2_ConstZ,
            100, 0, 0,
            100, 0, 0,
            0, 0);
        WriteLoadModel('Motor', id3_ConstPQuadQ,
            0, 0, 100,
            100, 0, 0,
            0, 0);
        WriteLoadModel('Mix Motor/Res', id4_LinPQuadQ,
            0, 0, 0,
            0, 0, 0,
            1, 2);
        WriteLoadModel('Constant I', id5_ConstI,
            0, 100, 0,
            0, 100, 0,
            0, 0);
        WriteLoadModel('Variable P, Fixed Q', id6_ConstPConstQ,
            0, 0, 100,
            0, 0, 100,
            0, 0);
        WriteLoadModel('Variable P, Fixed X', id7_ConstPConstX,
            0, 0, 100,
            100, 0, 0,
            0, 0);

        pLoad := ActiveCircuit.Loads.First;
        while pLoad <> NIL do
        begin
            if pLoad.Enabled then
                with pLoad do
                begin
                    StartInstance(FunPrf, 'EnergyConsumer', pLoad);
                    CircuitNode(FunPrf, ActiveCircuit);
                    VbaseNode(FunPrf, pLoad);
                    case FLoadModel of
                        TLoadModel.ConstPQ:
                            UuidNode(FunPrf, 'EnergyConsumer.LoadResponse', id1_ConstkVA);
                        TLoadModel.ConstZ:
                            UuidNode(FunPrf, 'EnergyConsumer.LoadResponse', id2_ConstZ);
                        TLoadModel.Motor:
                            UuidNode(FunPrf, 'EnergyConsumer.LoadResponse', id3_ConstPQuadQ);
                        TLoadModel.CVR:
                            UuidNode(FunPrf, 'EnergyConsumer.LoadResponse', id4_LinPQuadQ);
                        TLoadModel.ConstI:
                            UuidNode(FunPrf, 'EnergyConsumer.LoadResponse', id5_ConstI);
                        TLoadModel.ConstPFixedQ:
                            UuidNode(FunPrf, 'EnergyConsumer.LoadResponse', id6_ConstPConstQ);
                        TLoadModel.ConstPFixedX:
                            UuidNode(FunPrf, 'EnergyConsumer.LoadResponse', id7_ConstPConstX);
                    end;
                    DoubleNode(SshPrf, 'EnergyConsumer.p', 1000.0 * kWBase);
                    DoubleNode(SshPrf, 'EnergyConsumer.q', 1000.0 * kvarBase);
                    IntegerNode(FunPrf, 'EnergyConsumer.customerCount', NumCustomers);
                    if Connection = TLoadConnection.Wye then
                    begin
                        ShuntConnectionKindNode(FunPrf, 'EnergyConsumer', 'Y');
                        BooleanNode(FunPrf, 'EnergyConsumer.grounded', TRUE);  // TODO - check bus 2
                    end
                    else
                    begin
                        ShuntConnectionKindNode(FunPrf, 'EnergyConsumer', 'D');
                        BooleanNode(FunPrf, 'EnergyConsumer.grounded', FALSE);
                    end;
                    geoUUID := GetDevUuid(LoadLoc, pLoad.Name, 1);
                    UuidNode(GeoPrf, 'PowerSystemResource.Location', geoUUID);
                    EndInstance(FunPrf, 'EnergyConsumer');
                    AttachLoadPhases(pLoad, geoUUID);
                    WriteTerminals(pLoad, geoUUID, crsUUID);
                end;
            pLoad := ActiveCircuit.Loads.Next;
        end;

        pLnCd := clsLnCd.ElementList.First;
        while pLnCd <> NIL do
        begin
            with pLnCd do
            begin
                if pLnCd.Units = UNITS_NONE then
                begin // we need the real units for CIM
                    pLine := ActiveCircuit.Lines.First;
                    while pLine <> NIL do
                    begin
                        if pLine.Enabled then
                        begin
                            if (pLine.LineCodeObj <> NIL) and (pLine.LineCodeObj.Name = pLnCd.LocalName) then
                            begin
                                pLnCd.Units := pLine.LengthUnits;
                                // writeln ('Setting Units on ' + pLnCd.LocalName + ' to ' + LineUnitsStr(pLnCd.Units));
                                break;
                            end;
                        end;
                        pLine := ActiveCircuit.Lines.Next;
                    end;
                end;
                v1 := To_per_Meter(pLnCd.Units); // TODO: warn if still UNITS_NONE
                if SymComponentsModel and (NumPhases = 3) then
                begin
                    v2 := 1.0e-9 * TwoPi * BaseFrequency; // convert nF to mhos
                    StartInstance(EpPrf, 'PerLengthSequenceImpedance', pLnCd);
                    DoubleNode(EpPrf, 'PerLengthSequenceImpedance.r', R1 * v1);
                    DoubleNode(EpPrf, 'PerLengthSequenceImpedance.x', X1 * v1);
                    DoubleNode(EpPrf, 'PerLengthSequenceImpedance.bch', C1 * v1 * v2);
                    DoubleNode(EpPrf, 'PerLengthSequenceImpedance.gch', 0.0);
                    DoubleNode(EpPrf, 'PerLengthSequenceImpedance.r0', R0 * v1);
                    DoubleNode(EpPrf, 'PerLengthSequenceImpedance.x0', X0 * v1);
                    DoubleNode(EpPrf, 'PerLengthSequenceImpedance.b0ch', C0 * v1 * v2);
                    DoubleNode(EpPrf, 'PerLengthSequenceImpedance.g0ch', 0.0);
                    EndInstance(EpPrf, 'PerLengthSequenceImpedance')
                end
                else
                begin
                    StartInstance(EpPrf, 'PerLengthPhaseImpedance', pLnCd);
                    IntegerNode(EpPrf, 'PerLengthPhaseImpedance.conductorCount', FNPhases);
                    EndInstance(EpPrf, 'PerLengthPhaseImpedance');
                    seq := 1;
                    for i := 1 to FNPhases do
                    begin
                        for j := 1 to i do
                        begin
                            StartFreeInstance(EpPrf, 'PhaseImpedanceData', GetDevUuid(ZData, pLnCd.LocalName, seq));
                            RefNode(EpPrf, 'PhaseImpedanceData.PhaseImpedance', pLnCd);
                            IntegerNode(EpPrf, 'PhaseImpedanceData.row', i);
                            IntegerNode(EpPrf, 'PhaseImpedanceData.column', j);
                            DoubleNode(EpPrf, 'PhaseImpedanceData.r', Z.GetElement(i, j).re * v1);
                            DoubleNode(EpPrf, 'PhaseImpedanceData.x', Z.GetElement(i, j).im * v1);
                            DoubleNode(EpPrf, 'PhaseImpedanceData.b', YC.GetElement(i, j).im * v1);
                            EndInstance(EpPrf, 'PhaseImpedanceData');
                            inc(seq)
                        end;
                    end;
                end;
            end;
            pLnCd := clsLnCd.ElementList.Next;
        end;

        pWire := clsWire.ElementList.First;
        while (pWire <> NIL) do
        begin
            StartInstance(CatPrf, 'OverheadWireInfo', pWire);
            WriteWireData(pWire);
            BooleanNode(CatPrf, 'WireInfo.insulated', FALSE);
            EndInstance(CatPrf, 'OverheadWireInfo');
            pWire := clsWire.ElementList.Next;
        end;

        pTape := clsTape.ElementList.First;
        while (pTape <> NIL) do
        begin
            StartInstance(CatPrf, 'TapeShieldCableInfo', pTape);
            WriteWireData(pTape);
            WriteCableData(pTape);
            WriteTapeData(pTape);
            EndInstance(CatPrf, 'TapeShieldCableInfo');
            pTape := clsTape.ElementList.Next;
        end;

        pConc := clsConc.ElementList.First;
        while (pConc <> NIL) do
        begin
            StartInstance(CatPrf, 'ConcentricNeutralCableInfo', pConc);
            WriteWireData(pConc);
            WriteCableData(pConc);
            WriteConcData(pConc);
            EndInstance(CatPrf, 'ConcentricNeutralCableInfo');
            pConc := clsConc.ElementList.Next;
        end;

        pGeom := clsGeom.ElementList.First;
        while pGeom <> NIL do
        begin
            with pGeom do
            begin
                StartInstance(CatPrf, 'WireSpacingInfo', pGeom);
                ConductorUsageEnum(CatPrf, 'distribution');
                IntegerNode(CatPrf, 'WireSpacingInfo.phaseWireCount', 1);
                DoubleNode(CatPrf, 'WireSpacingInfo.phaseWireSpacing', 0.0);
                if PhaseChoice[1] = Overhead then   // decide this off the first conductor
                    BooleanNode(CatPrf, 'WireSpacingInfo.isCable', FALSE)
                else
                    BooleanNode(CatPrf, 'WireSpacingInfo.isCable', TRUE);
                EndInstance(CatPrf, 'WireSpacingInfo');

                for i := 1 to NWires do
                begin
                    pName1.LocalName := 'WP_' + pGeom.Name + '_' + IntToStr(i);
                    pName1.UUID := GetDevUuid(WirePos, pName1.LocalName, 1);  // 1 for pGeom
                    StartInstance(CatPrf, 'WirePosition', pName1);
                    RefNode(CatPrf, 'WirePosition.WireSpacingInfo', pGeom);
                    IntegerNode(CatPrf, 'WirePosition.sequenceNumber', i);
                    v1 := To_Meters(Units[i]);
                    DoubleNode(CatPrf, 'WirePosition.xCoord', Xcoord[i] * v1);
                    DoubleNode(CatPrf, 'WirePosition.yCoord', Ycoord[i] * v1);
                    EndInstance(CatPrf, 'WirePosition')
                end;
            end;
            pGeom := clsGeom.ElementList.Next;
        end;

        pSpac := clsSpac.ElementList.First;
        while pSpac <> NIL do
        begin
            with pSpac do
            begin
                v1 := To_Meters(Units);
                StartInstance(CatPrf, 'WireSpacingInfo', pSpac);
                ConductorUsageEnum(CatPrf, 'distribution');
                IntegerNode(CatPrf, 'WireSpacingInfo.phaseWireCount', 1);
                DoubleNode(CatPrf, 'WireSpacingInfo.phaseWireSpacing', 0.0);
                if pSpac.Ycoord[1] > 0.0 then
                    BooleanNode(CatPrf, 'WireSpacingInfo.isCable', FALSE)
                else
                    BooleanNode(CatPrf, 'WireSpacingInfo.isCable', TRUE);
                EndInstance(CatPrf, 'WireSpacingInfo');

                for i := 1 to NWires do
                begin
                    pName1.LocalName := 'WP_' + pSpac.Name + '_' + IntToStr(i);
                    pName1.UUID := GetDevUuid(WirePos, pName1.LocalName, 2); // 2 for pSpac
                    StartInstance(CatPrf, 'WirePosition', pName1);
                    RefNode(CatPrf, 'WirePosition.WireSpacingInfo', pSpac);
                    IntegerNode(CatPrf, 'WirePosition.sequenceNumber', i);
                    DoubleNode(CatPrf, 'WirePosition.xCoord', Xcoord[i] * v1);
                    DoubleNode(CatPrf, 'WirePosition.yCoord', Ycoord[i] * v1);
                    EndInstance(CatPrf, 'WirePosition')
                end;
            end;
            pSpac := clsSpac.ElementList.Next;
        end;

    // export the operational current limits that were created on-the-fly
        for i := Low(OpLimitList) to High(OpLimitList) do
        begin
            pILimit := OpLimitList[i];
            if pILimit = NIL then
                break;
            StartInstance(FunPrf, 'OperationalLimitSet', pILimit);
            EndInstance(FunPrf, 'OperationalLimitSet');
            pName1.LocalName := pILimit.LocalName + '_Norm';
            pName1.UUID := GetDevUuid(NormAmps, pILimit.LocalName, 1);
            StartInstance(FunPrf, 'CurrentLimit', pName1);
            RefNode(FunPrf, 'OperationalLimit.OperationalLimitSet', pILimit);
            RefNode(FunPrf, 'OperationalLimit.OperationalLimitType', pNormLimit);
            DoubleNode(FunPrf, 'CurrentLimit.value', pILimit.NormAmps);
            EndInstance(FunPrf, 'CurrentLimit');
            pName2.LocalName := pILimit.LocalName + '_Emerg';
            pName2.UUID := GetDevUuid(EmergAmps, pILimit.LocalName, 1);
            StartInstance(FunPrf, 'CurrentLimit', pName2);
            RefNode(FunPrf, 'OperationalLimit.OperationalLimitSet', pILimit);
            RefNode(FunPrf, 'OperationalLimit.OperationalLimitType', pEmergLimit);
            DoubleNode(FunPrf, 'CurrentLimit.value', pILimit.EmergAmps);
            EndInstance(FunPrf, 'CurrentLimit');
        end;

        pName1.Free;
        pName2.Free;

//    FreeUuidList;  // this is deferred for UUID export
        FreeBankList;
        FreeOpLimitList;

        DSS.GlobalResult := FileNm;
    finally
        FD.Free;
    end;
end;

constructor TCIMExporter.Create(dssContext: TDSSContext);
begin
    DSS := dssContext;
end;

destructor TCIMExporter.Destroy;
begin
    inherited Destroy;
end;

procedure TCIMEXporterHelper.FD_Create(Combined: Boolean; FileName: String);
var
    i: ProfileChoice;
begin
    Separate := not Combined;
    if Separate then
    begin
        for i := Low(ProfileChoice) to High(ProfileChoice) do
            roots[i] := '';
        StartCIMFile(F_FUN, FileName + '_FUN.XML', FunPrf);
        StartCIMFile(F_GEO, FileName + '_GEO.XML', GeoPrf);
        StartCIMFile(F_TOPO, FileName + '_TOPO.XML', TopoPrf);
        StartCIMFile(F_SSH, FileName + '_SSH.XML', SshPrf);
        StartCIMFile(F_CAT, FileName + '_CAT.XML', CatPrf);
        StartCIMFile(F_EP, FileName + '_EP.XML', EpPrf);
        StartCIMFile(F_DYN, FileName + '_DYN.XML', EpPrf)
    end
    else
    begin
        StartCIMFile(F_FUN, FileName, FunPrf)
    end;
end;

procedure TCIMEXporterHelper.FD_Destroy;
begin
    FSWriteLn(F_FUN, '</rdf:RDF>');
    FreeAndNil(F_FUN);
    if Separate then
    begin
        FSWriteLn(F_GEO, '</rdf:RDF>');
        FSWriteLn(F_CAT, '</rdf:RDF>');
        FSWriteLn(F_SSH, '</rdf:RDF>');
        FSWriteLn(F_TOPO, '</rdf:RDF>');
        FSWriteLn(F_EP, '</rdf:RDF>');
        FSWriteLn(F_DYN, '</rdf:RDF>');
        FreeAndNil(F_GEO);
        FreeAndNil(F_CAT);
        FreeAndNil(F_SSH);
        FreeAndNil(F_TOPO);
        FreeAndNil(F_EP);
        FreeAndNil(F_DYN);
    end;
    // inherited Destroy;
end;

procedure TCIMEXporterHelper.ConverterControlEnum(prf: ProfileChoice; varMode: Integer; CIMdynamics: Boolean);
var
    str: String;
begin
    str := 'constantPowerFactor'; // VARMODEPF
    if CIMDynamics then
        str := 'dynamic'
    else if varMode = VARMODEKVAR then
        str := 'constantReactivePower';

    FD.WriteCimLn (prf, Format(
        '  <cim:PowerElectronicsConnection.controlMode rdf:resource="%s#ConverterControlMode.%s"/>',
        [CIM_NS, str]
    ));
end;

end.
