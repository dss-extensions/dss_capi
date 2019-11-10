unit ExportCIMXML;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

{
  ----------------------------------------------------------
  Copyright (c) 2009-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Write a CIM XML file using RDF Schema for the Common Distribution
  Power System Model, IEC 61968-13.}

interface

uses
    NamedObject,  // for TUuid
    DSSClass;


type
    CIMProfileChoice = (Combined, Functional, ElectricalProperties,
        Asset, Geographical, Topology, StateVariables);

procedure ExportCDPSM(DSS: TDSSContext; FileNm: String;
    Substation: String;
    SubGeographicRegion: String;
    GeographicRegion: String;
    FdrUUID: TUuid;
    SubUUID: TUuid;
    SubGeoUUID: TUuid;
    RgnUUID: TUuid;
    prf: CIMProfileChoice = Combined);

implementation

uses
    SysUtils,
    Utilities,
    Circuit,
    DSSClassDefs,
    DSSGlobals,
    CktElement,
    PDElement,
    PCElement,
    Generator,
    Load,
    RegControl,
    Vsource,
    Line,
    Transformer,
    Ucomplex,
    UcMatrix,
    LineCode,
    Fuse,
    Capacitor,
    CapControl,
    CapControlvars,
    Reactor,
    Feeder,
    ConductorData,
    LineUnits,
    LineGeometry,
    StrUtils,
    Math,
    XfmrCode,
    HashList,
    WireData,
    LineSpacing,
    CableData,
    CNData,
    TSData,
    Storage,
    PVSystem,
    Relay,
    Recloser,
    DSSObject,
    DSSHelper;

type
    UuidChoice = (Bank, Wdg, XfCore, XfMesh, WdgInf, ScTest, OcTest,
        BaseV, LinePhase, LoadPhase, GenPhase, CapPhase, SolarPhase, BatteryPhase,
        XfLoc, LoadLoc, LineLoc, CapLoc, Topo, ReacLoc, SolarLoc, BatteryLoc,
        OpLimV, OpLimI);

    TBankObject = class(TNamedObject)
    PUBLIC
        vectorGroup: String;
        maxWindings: Integer;
        nWindings: Integer;
        connections: array of Integer;
        angles: array of Integer;
        phaseA: array of Integer;
        phaseB: array of Integer;
        phaseC: array of Integer;
        ground: array of Integer;
        a_unit: TTransfObj;  // save this for writing the bank coordinates

        constructor Create(MaxWdg: Integer);
        destructor Destroy; OVERRIDE;

        procedure AddTransformer(DSS: TDSSContext; pXf: TTransfObj);
        procedure BuildVectorGroup;
    end;

    TOpLimitObject = class(TNamedObject)
    PUBLIC
        NormAmps: Double;
        EmergAmps: Double;
        constructor Create(norm, emerg: Double);
        destructor Destroy; OVERRIDE;
    end;

var
    UuidHash: THashList;       // index is 1-based
    UuidList: array of TUuid;  // index is 0-based
    BankHash: THashList;
    BankList: array of TBankObject;
    OpLimitHash: THashList;
    OpLimitList: array of TOpLimitObject;

const
//  CIM_NS = 'http://iec.ch/TC57/2012/CIM-schema-cim17';
    CIM_NS = 'http://iec.ch/TC57/CIM100';

procedure ParseSwitchClass(DSS: TDSSContext; pLine: TLineObj; var swtCls: String; var ratedAmps, breakingAmps: Double);
var
    pFuse: TFuseObj;
    pRelay: TRelayObj;
    pRecloser: TRecloserObj;
begin
    swtCls := 'LoadBreakSwitch';
    ratedAmps := pLine.NormAmps;
    breakingAmps := ratedAmps;
    pFuse := DSS.ActiveCircuit.Fuses.First;
    while (pFuse <> NIL) do
    begin
        if pFuse.ControlledElement = pLine then
        begin
            swtCls := 'Fuse';
            ratedAmps := pFuse.RatedCurrent;
            breakingAmps := 0.0;
            exit;
        end;
        pFuse := DSS.ActiveCircuit.Fuses.Next;
    end;
    pRelay := DSS.ActiveCircuit.Relays.First;
    while (pRelay <> NIL) do
    begin
        if pRelay.ControlledElement = pLine then
        begin
            swtCls := 'Breaker';
            exit;
        end;
        pRelay := DSS.ActiveCircuit.Relays.Next;
    end;
    pRecloser := DSS.ActiveCircuit.Reclosers.First;
    while (pRecloser <> NIL) do
    begin
        if pRecloser.ControlledElement = pLine then
        begin
            swtCls := 'Recloser';
            exit;
        end;
        pRecloser := DSS.ActiveCircuit.Reclosers.Next;
    end;
end;

// this returns s1, s2, or a combination of ABCN
function PhaseString(DSS: TDSSContext; pElem: TDSSCktElement; bus: Integer): String; // if order doesn't matter
var
    val, phs: String;
    dot: Integer;
    bSec: Boolean;
begin
    phs := pElem.FirstBus;
    for dot := 2 to bus do
        phs := pElem.NextBus;
    bSec := FALSE;
    if pElem.NPhases = 2 then
        if DSS.ActiveCircuit.Buses^[pElem.Terminals^[bus].BusRef].kVBase < 0.25 then
            bSec := TRUE;
    if pElem.NPhases = 1 then
        if DSS.ActiveCircuit.Buses^[pElem.Terminals^[bus].BusRef].kVBase < 0.13 then
            bSec := TRUE;

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

function PhaseOrderString(pElem: TDSSCktElement; bus: Integer): String; // for transposition
var
    phs: String;
    dot: Integer;
begin
    phs := pElem.FirstBus;
    for dot := 2 to bus do
        phs := pElem.NextBus;

    dot := pos('.', phs);
    if dot < 1 then
    begin
        Result := 'ABC';
    end
    else
    begin
        phs := Copy(phs, dot + 1, Length(phs));
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

constructor TBankObject.Create(MaxWdg: Integer);
begin
    maxWindings := MaxWdg;
    nWindings := 0;
    SetLength(connections, MaxWdg);
    SetLength(angles, MaxWdg);
    SetLength(phaseA, MaxWdg);
    SetLength(phaseB, MaxWdg);
    SetLength(phaseC, MaxWdg);
    SetLength(ground, MaxWdg);
    inherited Create('Bank');
end;

destructor TBankObject.Destroy;
begin
    connections := NIL;
    angles := NIL;
    phaseA := NIL;
    phaseB := NIL;
    phaseC := NIL;
    ground := NIL;
    a_unit := NIL;
    inherited Destroy;
end;

procedure TBankObject.BuildVectorGroup;
var
    i: Integer;
begin
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
        vectorGroup := UpperCase(LeftStr(vectorGroup, 1)) + RightStr(vectorGroup, Length(vectorGroup) - 1);
end;

procedure TBankObject.AddTransformer(DSS: TDSSContext; pXf: TTransfObj);
var
    i: Integer;
    phs: String;
begin
    if pXf.NumberOfWindings > nWindings then
        nWindings := pXf.NumberOfWindings;

    a_unit := pXf;
    for i := 1 to pXf.NumberOfWindings do
    begin
        phs := PhaseString(DSS, pXf, i);
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

constructor TOpLimitObject.Create(norm, emerg: Double);
begin
    NormAmps := norm;
    EmergAmps := emerg;
    inherited Create('OpLimI');
end;

destructor TOpLimitObject.Destroy;
begin
    inherited Destroy;
end;

// the CIM transformer model requires some identified objects that don't have
// a counterpart in the DSS named objects.  These include banks, windings, and
// winding info.  So we create temporary UUIDs on the fly, and use a hash list when we
// need the UUIDs for later reference
procedure StartUuidList(size: Integer);
begin
    UuidHash := THashList.Create(size);
    SetLength(UuidList, size);
end;

procedure StartBankList(size: Integer);
begin
    BankHash := THashList.Create(size);
    SetLength(BankList, size);
end;

procedure StartOpLimitList(size: Integer);
begin
    OpLimitHash := THashList.Create(size);
    SetLength(OpLimitList, size);
end;

procedure AddBank(pBank: TBankObject);
var
    ref, size: Integer;
begin
    ref := BankHash.Add(pBank.localName);
    size := High(BankList) + 1;
    if ref > size then
        SetLength(BankList, 2 * size);
    BankList[ref - 1] := pBank;
end;

function GetBank(sBank: String): TBankObject;
var
    ref: Integer;
begin
    Result := NIL;
    ref := BankHash.Find(sBank);
    if ref > 0 then
        Result := BankList[ref - 1];
end;

procedure AddOpLimit(pLimit: TOpLimitObject);
var
    ref, size: Integer;
begin
    ref := OpLimitHash.Add(pLimit.localName);
    size := High(OpLimitList) + 1;
    if ref > size then
        SetLength(OpLimitList, 2 * size);
    OpLimitList[ref - 1] := pLimit;
end;

function GetOpLimit(sLimit: String): TOpLimitObject;
var
    ref: Integer;
begin
    Result := NIL;
    ref := OpLimitHash.Find(sLimit);
    if ref > 0 then
        Result := OpLimitList[ref - 1];
end;

function GetHashedUuid(key: String): TUuid;
var
    ref: Integer;
    size: Integer;
begin
    ref := UuidHash.Find(key);
    if ref = 0 then
    begin
        ref := UuidHash.Add(key);
        CreateUUID4(Result);
        size := High(UuidList) + 1;
        if ref > size then
            SetLength(UuidList, 2 * (size + 1));
        UuidList[ref - 1] := Result
    end
    else
    begin
        Result := UuidList[ref - 1]
    end;
end;

// any temporary object (not managed by DSS) should have '=' prepended to the Name
function GetDevUuid(which: UuidChoice; Name: String; Seq: Integer): TUuid;
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
    end;
    key := key + Name + '=' + IntToStr(Seq);
    Result := GetHashedUuid(key);
end;

// terminals are uniquely identified by class (DSSObjType), plus name and sequence
function GetTermUuid(pElem: TDSSCktElement; Seq: Integer): TUuid;
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

function GetBaseVUuid(val: Double): TUuid;
begin
    Result := GetDevUuid(BaseV, GetBaseVName(val), 1);
end;

function GetOpLimVName(val: Double): String;
begin
    Result := 'OpLimV_' + FloatToStrF(val, ffFixed, 6, 4);
end;

function GetOpLimVUuid(val: Double): TUuid;
begin
    Result := GetDevUuid(OpLimV, GetOpLimVName(val), 1);
end;

function GetOpLimIName(norm, emerg: Double): String;
begin
    Result := 'OpLimI_' + FloatToStrF(norm, ffFixed, 6, 1) + '_' + FloatToStrF(emerg, ffFixed, 6, 1);
end;

function GetOpLimIUuid(norm, emerg: Double): TUuid;
begin
    Result := GetDevUuid(OpLimI, GetOpLimIName(norm, emerg), 1);
end;

procedure FreeUuidList;
begin
    UuidHash.Free;
    UuidList := NIL;
end;

procedure FreeBankList;
begin
    BankHash.Free;
    BankList := NIL;
end;

procedure FreeOpLimitList;
begin
    OpLimitHash.Free;
    OpLimitList := NIL;
end;

procedure DoubleNode(var F: TextFile; Node: String; val: Double);
begin
    Writeln(F, Format('  <cim:%s>%.8g</cim:%s>', [Node, val, Node]));
end;

procedure IntegerNode(var F: TextFile; Node: String; val: Integer);
begin
    Writeln(F, Format('  <cim:%s>%d</cim:%s>', [Node, val, Node]));
end;

procedure BooleanNode(var F: TextFile; Node: String; val: Boolean);
var
    i: String;
begin
    if val then
        i := 'true'
    else
        i := 'false';
    Writeln(F, Format('  <cim:%s>%s</cim:%s>', [Node, i, Node]));
end;

procedure RefNode(var F: TextFile; Node: String; Obj: TNamedObject);
begin
    Writeln(F, Format('  <cim:%s rdf:resource="#%s"/>', [Node, Obj.CIM_ID]));
end;

procedure UuidNode(var F: TextFile; Node: String; ID: TUuid);
begin
    Writeln(F, Format('  <cim:%s rdf:resource="#%s"/>', [Node, UUIDToCIMString(ID)]));
end;

procedure LineCodeRefNode(var F: TextFile; List: TLineCode; Name: String);
var
    Obj: TLineCodeObj;
begin
    if List.SetActive(Name) then
    begin
        Obj := List.GetActiveObj;
        Writeln(F, Format('  <cim:ACLineSegment.PerLengthImpedance rdf:resource="#%s"/>', [Obj.CIM_ID]));
    end;
end;

procedure LineSpacingRefNode(var F: TextFile; List: TDSSClass; Name: String);
var
    Obj: TDSSObject; // should be a TLineGeometryObj or TLineSpacingObj
begin
    if List.SetActive(Name) then
    begin
        Obj := List.GetActiveObj;
        Writeln(F, Format('  <cim:ACLineSegment.WireSpacingInfo rdf:resource="#%s"/>', [Obj.CIM_ID]));
    end;
end;

procedure PhaseWireRefNode(var F: TextFile; Obj: TConductorDataObj);
begin
    Writeln(F, Format('  <cim:ACLineSegmentPhase.WireInfo rdf:resource="#%s"/>', [Obj.CIM_ID]));
end;

procedure CircuitNode(var F: TextFile; Obj: TNamedObject);
begin
    Writeln(F, Format('  <cim:Equipment.EquipmentContainer rdf:resource="#%s"/>', [Obj.CIM_ID]));
end;

function FirstPhaseString(DSS: TDSSContext; pElem: TDSSCktElement; bus: Integer): String;
var
    val: String;
begin
    val := PhaseString(DSS, pElem, bus);
    if val <> '' then
        Result := LeftStr(val, 1)
    else
        Result := 'A';
end;

procedure GeneratorControlEnum(var F: TextFile; val: String);
begin
    Writeln(F, Format('  <cim:GeneratingUnit.genControlSource rdf:resource="%s#GeneratorControlSource.%s"/>',
        [CIM_NS, val]));
end;

procedure BatteryStateEnum(var F: TextFile; val: Integer);
var
    str: String;
begin
    str := 'Waiting';
    if val = STORE_CHARGING then
        str := 'Charging'
    else
    if val = STORE_DISCHARGING then
        str := 'Discharging';
    Writeln(F, Format('  <cim:BatteryUnit.batteryState rdf:resource="%s#BatteryState.%s"/>',
        [CIM_NS, str]));
end;

procedure SynchMachTypeEnum(var F: TextFile; val: String);
begin
    Writeln(F, Format('  <cim:SynchronousMachine.type rdf:resource="%s#SynchronousMachineType.%s"/>',
        [CIM_NS, val]));
end;

procedure SynchMachModeEnum(var F: TextFile; val: String);
begin
    Writeln(F, Format('  <cim:SynchronousMachine.operatingMode rdf:resource="%s#SynchronousMachineOperatingMode.%s"/>',
        [CIM_NS, val]));
end;

procedure RegulatingControlEnum(var F: TextFile; val: String);
begin
    Writeln(F, Format('  <cim:RegulatingControl.mode rdf:resource="%s#RegulatingControlModeKind.%s"/>',
        [CIM_NS, val]));
end;

procedure WindingConnectionEnum(var F: TextFile; val: String);
begin
    Writeln(F, Format('  <cim:TransformerEndInfo.connectionKind rdf:resource="%s#WindingConnection.%s"/>',
        [CIM_NS, val]));
end;

procedure ConductorInsulationEnum(var F: TextFile; val: String);
begin
    Writeln(F, Format('  <cim:WireInfo.insulationMaterial rdf:resource="%s#WireInsulationKind.%s"/>',
        [CIM_NS, val]));
end;

procedure ConductorUsageEnum(var F: TextFile; val: String);
begin
    Writeln(F, Format('  <cim:WireSpacingInfo.usage rdf:resource="%s#WireUsageKind.%s"/>',
        [CIM_NS, val]));
end;

procedure CableShieldMaterialEnum(var F: TextFile; val: String);
begin
//  Writeln (F, Format ('  <cim:CableInfo.shieldMaterial rdf:resource="%s#CableShieldMaterialKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure ConductorMaterialEnum(var F: TextFile; val: String);
begin
//  Writeln (F, Format ('  <cim:WireInfo.material rdf:resource="%s#WireMaterialKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure CableOuterJacketEnum(var F: TextFile; val: String);
begin
//  Writeln (F, Format ('  <cim:CableInfo.outerJacketKind rdf:resource="%s#CableOuterJacketKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure CableConstructionEnum(var F: TextFile; val: String);
begin
//  Writeln (F, Format ('  <cim:CableInfo.constructionKind rdf:resource="%s#CableConstructionKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure TransformerControlEnum(var F: TextFile; val: String);
begin
    Writeln(F, Format('  <cim:RatioTapChanger.tculControlMode rdf:resource="%s#TransformerControlMode.%s"/>',
        [CIM_NS, val]));
end;

procedure MonitoredPhaseNode(var F: TextFile; val: String);
begin
    Writeln(F, Format('  <cim:RegulatingControl.monitoredPhase rdf:resource="%s#PhaseCode.%s"/>',
        [CIM_NS, val]));
end;

procedure OpLimitDirectionEnum(var F: TextFile; val: String);
begin
    Writeln(F, Format('  <cim:OperationalLimitType.direction rdf:resource="%s#OperationalLimitDirectionKind.%s"/>',
        [CIM_NS, val]));
end;

procedure StringNode(var F: TextFile; Node: String; val: String);
begin
    Writeln(F, Format('  <cim:%s>%s</cim:%s>', [Node, val, Node]));
end;

procedure StartInstance(var F: TextFile; Root: String; Obj: TNamedObject);
begin
    Writeln(F, Format('<cim:%s rdf:ID="%s">', [Root, Obj.CIM_ID]));
    StringNode(F, 'IdentifiedObject.mRID', Obj.CIM_ID);
    StringNode(F, 'IdentifiedObject.name', Obj.localName);
end;

procedure StartFreeInstance(var F: TextFile; Root: String);
var
    temp: TUuid;
begin
    CreateUUID4(temp);
    Writeln(F, Format('<cim:%s rdf:ID="%s">', [Root, UUIDToCIMString(temp)]));
end;

procedure EndInstance(var F: TextFile; Root: String);
begin
    Writeln(F, Format('</cim:%s>', [Root]));
end;

procedure XfmrPhasesEnum(DSS: TDSSContext; var F: TextFile; pElem: TDSSCktElement; bus: Integer);
begin
    Writeln(F, Format('  <cim:TransformerTankEnd.phases rdf:resource="%s#PhaseCode.%s"/>',
        [CIM_NS, PhaseString(DSS, pElem, bus)]));
end;

procedure PhaseNode(var F: TextFile; Root: String; val: String);
begin
    Writeln(F, Format('  <cim:%s.phase rdf:resource="%s#PhaseCode.%s"/>',
        [Root, CIM_NS, val]));
end;

procedure PhaseKindNode(var F: TextFile; Root: String; val: String);
begin
    Writeln(F, Format('  <cim:%s.phase rdf:resource="%s#SinglePhaseKind.%s"/>',
        [Root, CIM_NS, val]));
end;

procedure PhaseSideNode(var F: TextFile; Root: String; Side: Integer; val: String);
begin
    Writeln(F, Format('  <cim:%s.phaseSide%d rdf:resource="%s#SinglePhaseKind.%s"/>',
        [Root, Side, CIM_NS, val]));
end;

procedure ShuntConnectionKindNode(var F: TextFile; Root: String; val: String); // D, Y, Yn, I
begin
    Writeln(F, Format('  <cim:%s.phaseConnection rdf:resource="%s#PhaseShuntConnectionKind.%s"/>',
        [Root, CIM_NS, val]));
end;

procedure WindingConnectionKindNode(var F: TextFile; val: String); // D, Y, Z, Yn, Zn, A, I
begin
    Writeln(F, Format('  <cim:PowerTransformerEnd.connectionKind rdf:resource="%s#WindingConnection.%s"/>',
        [CIM_NS, val]));
end;

// we specify phases except for balanced three-phase
procedure AttachLinePhases(DSS: TDSSContext; var F: TextFile; pLine: TLineObj);
var
    s, phs: String;
    i: Integer;
    pPhase: TNamedObject;
begin
    pPhase := TNamedObject.Create('dummy');
    s := PhaseString(DSS, pLine, 1);
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
        StartInstance(F, 'ACLineSegmentPhase', pPhase);
        PhaseKindNode(F, 'ACLineSegmentPhase', phs);
        IntegerNode(F, 'ACLineSegmentPhase.sequenceNumber', i);
        if i <= pLine.NumConductorsAvailable then
            PhaseWireRefNode(F, pLine.ConductorData[i]);
        RefNode(F, 'ACLineSegmentPhase.ACLineSegment', pLine);
        UuidNode(F, 'PowerSystemResource.Location',
            GetDevUuid(LineLoc, pLine.Name, 1));
        EndInstance(F, 'ACLineSegmentPhase');
    end;
end;

procedure AttachSwitchPhases(var F: TextFile; pLine: TLineObj);
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
        pPhase.LocalName := pLine.Name + '_' + phs1;
        pPhase.UUID := GetDevUuid(LinePhase, pPhase.LocalName, 1);
        StartInstance(F, 'SwitchPhase', pPhase);
        BooleanNode(F, 'SwitchPhase.closed', pLine.Closed[0]);
        BooleanNode(F, 'SwitchPhase.normalOpen', not pLine.Closed[0]);
        PhaseSideNode(F, 'SwitchPhase', 1, phs1);
        PhaseSideNode(F, 'SwitchPhase', 2, phs2);
        RefNode(F, 'SwitchPhase.Switch', pLine);
        UuidNode(F, 'PowerSystemResource.Location', GetDevUuid(LineLoc, pLine.Name, 1));
        EndInstance(F, 'SwitchPhase');
    end;
end;

procedure AttachCapPhases(DSS: TDSSContext; var F: TextFile; pCap: TCapacitorObj; geoUUID: TUuid);
var
    s, phs: String;
    i: Integer;
    pPhase: TNamedObject;
    bph: Double;
begin
    if pCap.NPhases = 3 then
        exit;
    pPhase := TNamedObject.Create('dummy');
    s := PhaseString(DSS, pCap, 1);
    with pCap do
    begin
        bph := 0.001 * Totalkvar / NomKV / NomKV / NumSteps / NPhases;
        if (Connection = 1) then
            s := DeltaPhaseString(pCap);
    end;
    for i := 1 to length(s) do
    begin
        phs := s[i];
        pPhase.LocalName := pCap.Name + '_' + phs;
        pPhase.UUID := GetDevUuid(CapPhase, pPhase.LocalName, 1);
        StartInstance(F, 'LinearShuntCompensatorPhase', pPhase);
        PhaseKindNode(F, 'ShuntCompensatorPhase', phs);
        DoubleNode(F, 'LinearShuntCompensatorPhase.bPerSection', bph);
        DoubleNode(F, 'LinearShuntCompensatorPhase.gPerSection', 0.0);
        IntegerNode(F, 'ShuntCompensatorPhase.normalSections', pCap.NumSteps);
        IntegerNode(F, 'ShuntCompensatorPhase.maximumSections', pCap.NumSteps);
        RefNode(F, 'ShuntCompensatorPhase.ShuntCompensator', pCap);
        UuidNode(F, 'PowerSystemResource.Location', geoUUID);
        EndInstance(F, 'LinearShuntCompensatorPhase');
    end;
end;

procedure AttachSecondaryPhases(var F: TextFile; pLoad: TLoadObj; geoUUID: TUuid; pPhase: TNamedObject; p, q: Double; phs: String);
begin
    pPhase.LocalName := pLoad.Name + '_' + phs;
    pPhase.UUID := GetDevUuid(LoadPhase, pPhase.LocalName, 1);
    StartInstance(F, 'EnergyConsumerPhase', pPhase);
    PhaseKindNode(F, 'EnergyConsumerPhase', phs);
    DoubleNode(F, 'EnergyConsumerPhase.p', p);
    DoubleNode(F, 'EnergyConsumerPhase.q', q);
    RefNode(F, 'EnergyConsumerPhase.EnergyConsumer', pLoad);
    UuidNode(F, 'PowerSystemResource.Location', geoUUID);
    EndInstance(F, 'EnergyConsumerPhase');
end;

procedure AttachLoadPhases(DSS: TDSSContext; var F: TextFile; pLoad: TLoadObj; geoUUID: TUuid);
var
    s, phs: String;
    i: Integer;
    pPhase: TNamedObject;
    p, q: Double;
begin
    if pLoad.NPhases = 3 then
        exit;
    p := 1000.0 * pLoad.kWBase / pLoad.NPhases;
    q := 1000.0 * pLoad.kvarBase / pLoad.NPhases;
    if pLoad.Connection = 1 then
        s := DeltaPhaseString(pLoad)
    else
        s := PhaseString(DSS, pLoad, 1);

    pPhase := TNamedObject.Create('dummy');
  // first, filter out what appear to be split secondary loads
  // these can be 2-phase loads (balanced) nominally 0.208 kV, or
  //  1-phase loads (possibly unbalanced) nominally 0.12 kV
  //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
    if pLoad.kVLoadBase < 0.25 then
    begin
        if pLoad.NPhases = 2 then
        begin
            AttachSecondaryPhases(F, pLoad, geoUUID, pPhase, p, q, 's1');
            AttachSecondaryPhases(F, pLoad, geoUUID, pPhase, p, q, 's2');
            exit;
        end
        else
        begin
            AttachSecondaryPhases(F, pLoad, geoUUID, pPhase, p, q, s);
            exit;
        end;
    end;

    for i := 1 to length(s) do
    begin
        phs := s[i];
        pPhase.LocalName := pLoad.Name + '_' + phs;
        pPhase.UUID := GetDevUuid(LoadPhase, pPhase.LocalName, 1);
        StartInstance(F, 'EnergyConsumerPhase', pPhase);
        PhaseKindNode(F, 'EnergyConsumerPhase', phs);
        DoubleNode(F, 'EnergyConsumerPhase.p', p);
        DoubleNode(F, 'EnergyConsumerPhase.q', q);
        RefNode(F, 'EnergyConsumerPhase.EnergyConsumer', pLoad);
        UuidNode(F, 'PowerSystemResource.Location', geoUUID);
        EndInstance(F, 'EnergyConsumerPhase');
    end;
end;

procedure AttachSecondaryGenPhases(var F: TextFile; pGen: TGeneratorObj; geoUUID: TUuid; pPhase: TNamedObject; p, q: Double; phs: String);
begin
    pPhase.LocalName := pGen.Name + '_' + phs;
    pPhase.UUID := GetDevUuid(GenPhase, pPhase.LocalName, 1);
    StartInstance(F, 'SynchronousMachinePhase', pPhase);
    PhaseKindNode(F, 'SynchronousMachinePhase', phs);
    DoubleNode(F, 'SynchronousMachinePhase.p', p);
    DoubleNode(F, 'SynchronousMachinePhase.q', q);
    RefNode(F, 'SynchronousMachinePhase.SynchronousMachine', pGen);
    UuidNode(F, 'PowerSystemResource.Location', geoUUID);
    EndInstance(F, 'SynchronousMachinePhase');
end;

procedure AttachGeneratorPhases(DSS: TDSSContext; var F: TextFile; pGen: TGeneratorObj; geoUUID: TUuid);
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
        s := PhaseString(DSS, pGen, 1);

    pPhase := TNamedObject.Create('dummy');
  //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
    if pGen.Presentkv < 0.25 then
    begin
        if pGen.NPhases = 2 then
        begin
            AttachSecondaryGenPhases(F, pGen, geoUUID, pPhase, p, q, 's1');
            AttachSecondaryGenPhases(F, pGen, geoUUID, pPhase, p, q, 's2');
            exit;
        end
        else
        begin
            AttachSecondaryGenPhases(F, pGen, geoUUID, pPhase, p, q, s);
            exit;
        end;
    end;

    for i := 1 to length(s) do
    begin
        phs := s[i];
        pPhase.LocalName := pGen.Name + '_' + phs;
        pPhase.UUID := GetDevUuid(GenPhase, pPhase.LocalName, 1);
        StartInstance(F, 'SynchronousMachinePhase', pPhase);
        PhaseKindNode(F, 'SynchronousMachinePhase', phs);
        DoubleNode(F, 'SynchronousMachinePhase.p', p);
        DoubleNode(F, 'SynchronousMachinePhase.q', q);
        RefNode(F, 'SynchronousMachinePhase.SynchronousMachine', pGen);
        UuidNode(F, 'PowerSystemResource.Location', geoUUID);
        EndInstance(F, 'SynchronousMachinePhase');
    end;
end;

procedure AttachSecondarySolarPhases(var F: TextFile; pPV: TPVSystemObj; geoUUID: TUuid; pPhase: TNamedObject; p, q: Double; phs: String);
begin
    pPhase.LocalName := pPV.Name + '_' + phs;
    pPhase.UUID := GetDevUuid(SolarPhase, pPhase.LocalName, 1);
    StartInstance(F, 'PowerElectronicsConnectionPhase', pPhase);
    PhaseKindNode(F, 'PowerElectronicsConnectionPhase', phs);
    DoubleNode(F, 'PowerElectronicsConnectionPhase.p', p);
    DoubleNode(F, 'PowerElectronicsConnectionPhase.q', q);
    RefNode(F, 'PowerElectronicsConnectionPhase.PowerElectronicsConnection', pPV);
    UuidNode(F, 'PowerSystemResource.Location', geoUUID);
    EndInstance(F, 'PowerElectronicsConnectionPhase');
end;

procedure AttachSolarPhases(DSS: TDSSContext; var F: TextFile; pPV: TPVSystemObj; geoUUID: TUuid);
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
        s := PhaseString(DSS, pPV, 1);

    pPhase := TNamedObject.Create('dummy');
  //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
    if pPV.Presentkv < 0.25 then
    begin
        if pPV.NPhases = 2 then
        begin
            AttachSecondarySolarPhases(F, pPV, geoUUID, pPhase, p, q, 's1');
            AttachSecondarySolarPhases(F, pPV, geoUUID, pPhase, p, q, 's2');
            exit;
        end
        else
        begin
            AttachSecondarySolarPhases(F, pPV, geoUUID, pPhase, p, q, s);
            exit;
        end;
    end;

    for i := 1 to length(s) do
    begin
        phs := s[i];
        pPhase.LocalName := pPV.Name + '_' + phs;
        pPhase.UUID := GetDevUuid(SolarPhase, pPhase.LocalName, 1);
        StartInstance(F, 'PowerElectronicsConnectionPhase', pPhase);
        PhaseKindNode(F, 'PowerElectronicsConnectionPhase', phs);
        DoubleNode(F, 'PowerElectronicsConnectionPhase.p', p);
        DoubleNode(F, 'PowerElectronicsConnectionPhase.q', q);
        RefNode(F, 'PowerElectronicsConnectionPhase.PowerElectronicsConnection', pPV);
        UuidNode(F, 'PowerSystemResource.Location', geoUUID);
        EndInstance(F, 'PowerElectronicsConnectionPhase');
    end;
end;

procedure AttachSecondaryStoragePhases(var F: TextFile; pBat: TStorageObj; geoUUID: TUuid; pPhase: TNamedObject; p, q: Double; phs: String);
begin
    pPhase.LocalName := pBat.Name + '_' + phs;
    pPhase.UUID := GetDevUuid(BatteryPhase, pPhase.LocalName, 1);
    StartInstance(F, 'PowerElectronicsConnectionPhase', pPhase);
    PhaseKindNode(F, 'PowerElectronicsConnectionPhase', phs);
    DoubleNode(F, 'PowerElectronicsConnectionPhase.p', p);
    DoubleNode(F, 'PowerElectronicsConnectionPhase.q', q);
    RefNode(F, 'PowerElectronicsConnectionPhase.PowerElectronicsConnection', pBat);
    UuidNode(F, 'PowerSystemResource.Location', geoUUID);
    EndInstance(F, 'PowerElectronicsConnectionPhase');
end;

procedure AttachStoragePhases(DSS: TDSSContext; var F: TextFile; pBat: TStorageObj; geoUUID: TUuid);
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
        s := PhaseString(DSS, pBat, 1);

    pPhase := TNamedObject.Create('dummy');
  //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
    if pBat.Presentkv < 0.25 then
    begin
        if pBat.NPhases = 2 then
        begin
            AttachSecondaryStoragePhases(F, pBat, geoUUID, pPhase, p, q, 's1');
            AttachSecondaryStoragePhases(F, pBat, geoUUID, pPhase, p, q, 's2');
            exit;
        end
        else
        begin
            AttachSecondaryStoragePhases(F, pBat, geoUUID, pPhase, p, q, s);
            exit;
        end;
    end;

    for i := 1 to length(s) do
    begin
        phs := s[i];
        pPhase.LocalName := pBat.Name + '_' + phs;
        pPhase.UUID := GetDevUuid(BatteryPhase, pPhase.LocalName, 1);
        StartInstance(F, 'PowerElectronicsConnectionPhase', pPhase);
        PhaseKindNode(F, 'PowerElectronicsConnectionPhase', phs);
        DoubleNode(F, 'PowerElectronicsConnectionPhase.p', p);
        DoubleNode(F, 'PowerElectronicsConnectionPhase.q', q);
        RefNode(F, 'PowerElectronicsConnectionPhase.PowerElectronicsConnection', pBat);
        UuidNode(F, 'PowerSystemResource.Location', geoUUID);
        EndInstance(F, 'PowerElectronicsConnectionPhase');
    end;
end;

procedure VersionInstance(var F: TextFile);
begin
    StartFreeInstance(F, 'IEC61970CIMVersion');
    StringNode(F, 'IEC61970CIMVersion.version', 'IEC61970CIM100');
    StringNode(F, 'IEC61970CIMVersion.date', '2019-04-01');
    EndInstance(F, 'IEC61970CIMVersion');
end;

procedure WriteLoadModel(var F: TextFile; Name: String; ID: TUuid;
    zP: Double; iP: Double; pP: Double; zQ: Double; iQ: Double; pQ: Double;
    eP: Double; eQ: Double);
begin
    Writeln(F, Format('<cim:LoadResponseCharacteristic rdf:ID="%s">', [UUIDToCIMString(ID)]));
    StringNode(F, 'IdentifiedObject.mRID', UUIDToCIMString(ID));
    StringNode(F, 'IdentifiedObject.name', Name);
    if (eP > 0.0) or (eQ > 0.0) then
        BooleanNode(F, 'LoadResponseCharacteristic.exponentModel', TRUE)
    else
        BooleanNode(F, 'LoadResponseCharacteristic.exponentModel', FALSE);

    DoubleNode(F, 'LoadResponseCharacteristic.pConstantImpedance', zP);
    DoubleNode(F, 'LoadResponseCharacteristic.pConstantCurrent', iP);
    DoubleNode(F, 'LoadResponseCharacteristic.pConstantPower', pP);

    DoubleNode(F, 'LoadResponseCharacteristic.qConstantImpedance', zQ);
    DoubleNode(F, 'LoadResponseCharacteristic.qConstantCurrent', iQ);
    DoubleNode(F, 'LoadResponseCharacteristic.qConstantPower', pQ);

    DoubleNode(F, 'LoadResponseCharacteristic.pVoltageExponent', eP);
    DoubleNode(F, 'LoadResponseCharacteristic.qVoltageExponent', eQ);
    DoubleNode(F, 'LoadResponseCharacteristic.pFrequencyExponent', 0.0);
    DoubleNode(F, 'LoadResponseCharacteristic.qFrequencyExponent', 0.0);
    Writeln(F, '</cim:LoadResponseCharacteristic>');
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

procedure WritePositions(DSS: TDSSContext; var F: TextFile; pElem: TDSSCktElement; geoUUID: TUuid; crsUUID: TUuid);
var
    Nterm, j, ref: Integer;
    BusName: String;
begin
    Nterm := pElem.Nterms;
    BusName := pElem.FirstBus;
    Writeln(F, Format('<cim:Location rdf:ID="%s">', [UUIDToCIMString(geoUUID)]));
    StringNode(F, 'IdentifiedObject.mRID', UUIDToCIMString(geoUUID));
    StringNode(F, 'IdentifiedObject.name', pElem.LocalName + '_Loc');
    UuidNode(F, 'Location.CoordinateSystem', crsUUID);
    EndInstance(F, 'Location');

    for j := 1 to NTerm do
    begin
        if IsGroundBus(BusName) = FALSE then
        begin
            ref := pElem.Terminals^[j].BusRef;
            StartFreeInstance(F, 'PositionPoint');
            UuidNode(F, 'PositionPoint.Location', geoUUID);
            IntegerNode(F, 'PositionPoint.sequenceNumber', j);
            StringNode(F, 'PositionPoint.xPosition', FloatToStr(DSS.ActiveCircuit.Buses^[ref].x));
            StringNode(F, 'PositionPoint.yPosition', FloatToStr(DSS.ActiveCircuit.Buses^[ref].y));
            EndInstance(F, 'PositionPoint');
        end;
        BusName := pElem.Nextbus;
    end;
end;

procedure WriteReferenceTerminals(DSS: TDSSContext; var F: TextFile; pElem: TDSSCktElement;
    geoUUID: TUuid; crsUUID: TUuid; RefUuid: TUuid;
    norm: Double = 0.0; emerg: Double = 0.0);
var
    Nterm, j, ref: Integer;
    BusName, TermName, LimitName: String;
    TermUuid, LimiTUuid: TUuid;
    pLimit: TOpLimitObject;
begin
    Nterm := pElem.Nterms;
    BusName := pElem.FirstBus;
    for j := 1 to NTerm do
    begin
        if IsGroundBus(BusName) = FALSE then
        begin
            ref := pElem.Terminals^[j].BusRef;
            TermName := pElem.Name + '_T' + IntToStr(j);
            TermUuid := GetTermUuid(pElem, j);
            Writeln(F, Format('<cim:Terminal rdf:ID="%s">', [UUIDToCIMString(TermUuid)]));
            StringNode(F, 'IdentifiedObject.mRID', UUIDToCIMString(TermUuid));
            StringNode(F, 'IdentifiedObject.name', TermName);
            UuidNode(F, 'Terminal.ConductingEquipment', RefUuid);
            IntegerNode(F, 'ACDCTerminal.sequenceNumber', j);
            Writeln(F, Format('  <cim:Terminal.ConnectivityNode rdf:resource="#%s"/>',
                [DSS.ActiveCircuit.Buses[ref].CIM_ID]));
            if (j = 1) and (norm > 0.0) then
            begin
                if emerg < norm then
                    emerg := norm;
                LimitName := GetOpLimIName(norm, emerg);
                pLimit := GetOpLimit(LimitName);
                if pLimit = NIL then
                begin
                    pLimit := TOpLimitObject.Create(norm, emerg);
                    pLimit.localName := LimitName;
                    pLimit.UUID := GetDevUuid(OpLimI, LimitName, 0);
                    AddOpLimit(pLimit);
                end;
                LimiTUuid := GetDevUuid(OpLimI, LimitName, 0);
                UuidNode(F, 'ACDCTerminal.OperationalLimitSet', LimiTUuid);
            end;
            EndInstance(F, 'Terminal');
        end;
        BusName := pElem.Nextbus;
    end;
end;

procedure WriteTerminals(DSS: TDSSContext; var F: TextFile; pElem: TDSSCktElement; geoUUID: TUuid; crsUUID: TUuid;
    norm: Double = 0.0; emerg: Double = 0.0);
begin
    WriteReferenceTerminals(DSS, F, pElem, geoUUID, crsUUID, pElem.UUID, norm, emerg);
    WritePositions(DSS, F, pElem, geoUUID, crsUUID);
end;

procedure VbaseNode(DSS: TDSSContext; var F: TextFile; pElem: TDSSCktElement);
var
    j: Integer;
begin
    j := pElem.Terminals^[1].BusRef;
    UuidNode(F, 'ConductingEquipment.BaseVoltage',
        GetBaseVUuid(sqrt(3.0) * DSS.ActiveCircuit.Buses^[j].kVBase));
end;

procedure WriteXfmrCode(var F: TextFile; pXfmr: TXfmrCodeObj);
var
    pName, pBank: TNamedObject;
    ratShort, ratEmerg, val, Zbase: Double;
    i, j, seq: Integer;
    temp: TUuid;
begin
    pName := TNamedObject.Create('dummy');
    pBank := TNamedObject.Create('dummy');
    with pXfmr do
    begin
        pBank.LocalName := pXfmr.Name + '_PowerXfInfo';
        CreateUUID4(temp);
        pBank.UUID := temp;
        StartInstance(F, 'PowerTransformerInfo', pBank);
        EndInstance(F, 'PowerTransformerInfo');
        StartInstance(F, 'TransformerTankInfo', pXfmr);
        RefNode(F, 'TransformerTankInfo.PowerTransformerInfo', pBank);
        EndInstance(F, 'TransformerTankInfo');
        ratShort := NormMaxHKVA / Winding^[1].kva;
        ratEmerg := EmergMaxHKVA / Winding^[1].kva;
        for i := 1 to NumWindings do
        begin
            Zbase := Winding^[i].kvll;
            Zbase := 1000.0 * Zbase * Zbase / Winding^[1].kva;
            pName.localName := pXfmr.Name + '_' + IntToStr(i);
            pName.UUID := GetDevUuid(WdgInf, pXfmr.Name, i);
            StartInstance(F, 'TransformerEndInfo', pName);
            RefNode(F, 'TransformerEndInfo.TransformerTankInfo', pXfmr);
            IntegerNode(F, 'TransformerEndInfo.endNumber', i);
            if pXfmr.FNPhases < 3 then
            begin
                WindingConnectionEnum(F, 'I');
                if (i = 3) and (Winding^[i].kvll < 0.3) then // for center-tap secondary
                    IntegerNode(F, 'TransformerEndInfo.phaseAngleClock', 6)
                else
                    IntegerNode(F, 'TransformerEndInfo.phaseAngleClock', 0)
            end
            else
            begin
                if Winding^[i].Connection = 1 then
                    WindingConnectionEnum(F, 'D')
                else
                if (Winding^[i].Rneut > 0.0) or (Winding^[i].Xneut > 0.0) then
                    WindingConnectionEnum(F, 'Yn')
                else
                    WindingConnectionEnum(F, 'Y');
                if Winding^[i].Connection <> Winding^[1].Connection then
                    IntegerNode(F, 'TransformerEndInfo.phaseAngleClock', 1)
                else
                    IntegerNode(F, 'TransformerEndInfo.phaseAngleClock', 0);
            end;
            DoubleNode(F, 'TransformerEndInfo.ratedU', 1000 * Winding^[i].kvll);
            DoubleNode(F, 'TransformerEndInfo.ratedS', 1000 * Winding^[i].kva);
            DoubleNode(F, 'TransformerEndInfo.shortTermS', 1000 * Winding^[i].kva * ratShort);
            DoubleNode(F, 'TransformerEndInfo.emergencyS', 1000 * Winding^[i].kva * ratEmerg);
            DoubleNode(F, 'TransformerEndInfo.r', Winding^[i].Rpu * Zbase);
            DoubleNode(F, 'TransformerEndInfo.insulationU', 0.0);
            EndInstance(F, 'TransformerEndInfo');
        end;
        pName.localName := pXfmr.Name + '_' + IntToStr(1);
        pName.UUID := GetDevUuid(OcTest, pXfmr.Name, 1);
        StartInstance(F, 'NoLoadTest', pName);
        UuidNode(F, 'NoLoadTest.EnergisedEnd', GetDevUuid(WdgInf, pXfmr.Name, 1));
        DoubleNode(F, 'NoLoadTest.energisedEndVoltage', 1000.0 * Winding^[1].kvll);
        DoubleNode(F, 'NoLoadTest.excitingCurrent', pctImag);
        DoubleNode(F, 'NoLoadTest.excitingCurrentZero', pctImag);
        val := 0.01 * pctNoLoadLoss * Winding^[1].kva; // losses to be in kW
        DoubleNode(F, 'NoLoadTest.loss', val);
        DoubleNode(F, 'NoLoadTest.lossZero', val);
        DoubleNode(F, 'TransformerTest.basePower', 1000.0 * Winding^[1].kva);
        DoubleNode(F, 'TransformerTest.temperature', 50.0);
        EndInstance(F, 'NoLoadTest');
        seq := 0;
        for i := 1 to NumWindings do
            for j := (i + 1) to NumWindings do
            begin
                Inc(seq);
                pName.localName := pXfmr.Name + '_' + IntToStr(seq);
                pName.UUID := GetDevUuid(ScTest, pXfmr.Name, seq);
                StartInstance(F, 'ShortCircuitTest', pName);
                UuidNode(F, 'ShortCircuitTest.EnergisedEnd', GetDevUuid(WdgInf, pXfmr.Name, i));
         // NOTE: can insert more than one GroundedEnds for three-winding short-circuit tests
                UuidNode(F, 'ShortCircuitTest.GroundedEnds', GetDevUuid(WdgInf, pXfmr.Name, j));
                IntegerNode(F, 'ShortCircuitTest.energisedEndStep', Winding^[i].NumTaps div 2);
                IntegerNode(F, 'ShortCircuitTest.groundedEndStep', Winding^[j].NumTaps div 2);
                Zbase := Winding^[i].kvll;
                Zbase := 1000.0 * Zbase * Zbase / Winding^[1].kva;  // all DSS impedances are on winding 1 base
                val := Xsc^[seq] * Zbase;
                DoubleNode(F, 'ShortCircuitTest.leakageImpedance', val);
                DoubleNode(F, 'ShortCircuitTest.leakageImpedanceZero', val);
                if seq = 1 then
                begin // put all the load loss on test from wdg1 to wdg2
                    val := 0.01 * pctLoadLoss * Winding^[1].kva; // losses are to be in kW
                    DoubleNode(F, 'ShortCircuitTest.loss', val);
                    DoubleNode(F, 'ShortCircuitTest.lossZero', val);
                end
                else
                begin
                    DoubleNode(F, 'ShortCircuitTest.loss', 0.0);
                    DoubleNode(F, 'ShortCircuitTest.lossZero', 0.0);
                end;
                DoubleNode(F, 'TransformerTest.basePower', 1000.0 * Winding^[i].kva);
                DoubleNode(F, 'TransformerTest.temperature', 50.0);
                EndInstance(F, 'ShortCircuitTest');
            end;
    end;
    pName.Free;
end;

procedure WriteCableData(var F: TextFile; pCab: TCableDataObj);
var
    v1: Double;
begin
    with pCab do
    begin
        v1 := To_Meters(RadiusUnits);
        BooleanNode(F, 'WireInfo.insulated', TRUE);
        DoubleNode(F, 'WireInfo.insulationThickness', v1 * pCab.InsLayer);
        ConductorInsulationEnum(F, 'crosslinkedPolyethylene'); // TODO -  code EpsR
        CableOuterJacketEnum(F, 'none');
        CableConstructionEnum(F, 'stranded');
        BooleanNode(F, 'CableInfo.isStrandFill', FALSE); // we don't really know this
        DoubleNode(F, 'CableInfo.diameterOverCore',
            v1 * (pCab.DiaIns - 2.0 * pCab.InsLayer));
        DoubleNode(F, 'CableInfo.diameterOverInsulation', v1 * pCab.DiaIns);
        DoubleNode(F, 'CableInfo.diameterOverJacket', v1 * pCab.DiaCable);
        DoubleNode(F, 'CableInfo.nominalTemperature', 90.0);  // we don't really know this
    end;
end;

procedure WriteTapeData(var F: TextFile; pCab: TTSDataObj);
var
    v1: Double;
begin
    with pCab do
    begin
        v1 := To_Meters(RadiusUnits);
        DoubleNode(F, 'CableInfo.diameterOverScreen',
            v1 * (pCab.DiaShield - 2.0 * pCab.TapeLayer));
        DoubleNode(F, 'TapeShieldCableInfo.tapeLap', pCab.TapeLap);
        DoubleNode(F, 'TapeShieldCableInfo.tapeThickness', v1 * pCab.TapeLayer);
        CableShieldMaterialEnum(F, 'copper');
        BooleanNode(F, 'CableInfo.sheathAsNeutral', TRUE);
    end;
end;

procedure WriteConcData(var F: TextFile; pCab: TCNDataObj);
var
    v1: Double;
begin
    with pCab do
    begin
        v1 := To_Meters(RadiusUnits);
        DoubleNode(F, 'CableInfo.diameterOverScreen',
            v1 * (pCab.DiaCable - 2.0 * pCab.DiaStrand));
        DoubleNode(F, 'ConcentricNeutralCableInfo.diameterOverNeutral',
            v1 * pCab.DiaCable);
        DoubleNode(F, 'ConcentricNeutralCableInfo.neutralStrandRadius',
            v1 * 0.5 * pCab.DiaStrand);
        DoubleNode(F, 'ConcentricNeutralCableInfo.neutralStrandGmr',
            v1 * pCab.GmrStrand);
        v1 := To_per_Meter(ResUnits);
        DoubleNode(F, 'ConcentricNeutralCableInfo.neutralStrandRDC20',
            v1 * pCab.RStrand);
        IntegerNode(F, 'ConcentricNeutralCableInfo.neutralStrandCount', pCab.NStrand);
        BooleanNode(F, 'CableInfo.sheathAsNeutral', FALSE);
    end;
end;

procedure WriteWireData(var F: TextFile; pWire: TConductorDataObj);
var
    v1: Double;
begin
    with pWire do
    begin
        StringNode(F, 'WireInfo.sizeDescription', DisplayName);
        if CompareText(LeftStr(name, 2), 'AA') = 0 then
            ConductorMaterialEnum(F, 'aluminum')
        else
        if CompareText(LeftStr(name, 4), 'ACSR') = 0 then
            ConductorMaterialEnum(F, 'acsr')
        else
        if CompareText(LeftStr(name, 2), 'CU') = 0 then
            ConductorMaterialEnum(F, 'copper')
        else
        if CompareText(LeftStr(name, 3), 'EHS') = 0 then
            ConductorMaterialEnum(F, 'steel')
        else
            ConductorMaterialEnum(F, 'other');
        v1 := To_Meters(GMRUnits);
        DoubleNode(F, 'WireInfo.gmr', GMR * v1);
        v1 := To_Meters(RadiusUnits);
        DoubleNode(F, 'WireInfo.radius', Radius * v1);
        v1 := To_per_Meter(ResUnits);
        DoubleNode(F, 'WireInfo.rDC20', Rdc * v1);
        DoubleNode(F, 'WireInfo.rAC25', Rac * v1);
        DoubleNode(F, 'WireInfo.rAC50', Rac * v1);
        DoubleNode(F, 'WireInfo.rAC75', Rac * v1);
        DoubleNode(F, 'WireInfo.ratedCurrent', MaxValue([NormAmps, 0.0]));
        IntegerNode(F, 'WireInfo.strandCount', 0);
        IntegerNode(F, 'WireInfo.coreStrandCount', 0);
        DoubleNode(F, 'WireInfo.coreRadius', 0.0);
    end;
end;

procedure ExportCDPSM(DSS: TDSSContext; FileNm: String;
    Substation: String;
    SubGeographicRegion: String;
    GeographicRegion: String;
    FdrUUID: TUuid;
    SubUUID: TUuid;
    SubGeoUUID: TUuid;
    RgnUUID: TUuid;
    prf: CIMProfileChoice);
var
    F: TextFile;
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

    pILimit: TOpLimitObject;
    pNormLimit, pEmergLimit, pRangeAHiLimit, pRangeALoLimit, pRangeBHiLimit, pRangeBLoLimit: TNamedObject; // OperationalLimitType
    LimitName: String;
    LimiTUuid: TUuid;

    zbase: Double;
    s: String;
    swtCls: String;  // based on controls, if any, attached to a line having switch=yes
    ratedAmps, breakingAmps: Double;

    pBank: TBankObject;
    maxWdg: Integer;
    WdgList: array of TNamedObject;
    CoreList: array of TNamedObject;
    MeshList: array of TNamedObject;
    sBank: String;
    bTanks: Boolean;

    pLoad: TLoadObj;
    pVsrc: TVsourceObj;
    pGen: TGeneratorObj;
    pPV: TPVSystemObj;
    pBat: TStorageObj;

    pCap: TCapacitorObj;
    pCapC: TCapControlObj;
    pXf: TTransfObj;
    pReg: TRegControlObj;
    pLine: TLineObj;
    pReac: TReactorObj;

    clsCode: TLineCode;
    clsGeom: TLineGeometry;
    clsWire: TWireData;
    clsXfmr: TXfmrCode;
    clsSpac: TLineSpacing;
    clsTape: TTSData;
    clsConc: TCNData;

    pCode: TLineCodeObj;
    pGeom: TLineGeometryObj;
    pWire: TWireDataObj;
    pXfmr: TXfmrCodeObj;
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
    tmpUUID: TUuid;
begin
    try
        clsCode := DSS.DSSClassList.Get(DSS.ClassNames.Find('linecode'));
        clsWire := DSS.DSSClassList.Get(DSS.ClassNames.Find('wiredata'));
        clsGeom := DSS.DSSClassList.Get(DSS.ClassNames.Find('linegeometry'));
        clsXfmr := DSS.DSSClassList.Get(DSS.ClassNames.Find('xfmrcode'));
        clsSpac := DSS.DSSClassList.Get(DSS.ClassNames.Find('linespacing'));
        clsTape := DSS.DSSClassList.Get(DSS.ClassNames.Find('TSData'));
        clsConc := DSS.DSSClassList.Get(DSS.ClassNames.Find('CNData'));
        pName1 := TNamedObject.Create('Temp1');
        pName2 := TNamedObject.Create('Temp2');
        i1 := clsXfmr.ElementCount * 6; // 3 wdg info, 3 sctest
        i2 := DSS.ActiveCircuit.Transformers.ListSize * 11; // bank, info, 3 wdg, 3 wdg info, 3sctest
        StartUuidList(i1 + i2);
        StartBankList(DSS.ActiveCircuit.Transformers.ListSize);
        StartOpLimitList(ActiveCircuit.Lines.ListSize);

    {$IFDEF FPC}
         // this only works in the command line version
        Writeln(FileNm + '<=' + DSS.ActiveCircuit.Name + '<-' + Substation + '<-' + SubGeographicRegion + '<-' + GeographicRegion);
    {$ENDIF}
        Assignfile(F, FileNm);
        ReWrite(F);

        Writeln(F, '<?xml version="1.0" encoding="utf-8"?>');
        Writeln(F, '<!-- un-comment this line to enable validation');
        Writeln(F, '-->');
        Writeln(F, '<rdf:RDF xmlns:cim="' + CIM_NS + '#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">');
        Writeln(F, '<!--');
        Writeln(F, '-->');

        VersionInstance(F);

        pCRS := TNamedObject.Create('CoordinateSystem');
        CreateUUID4(crsUUID);
        pCRS.UUID := crsUUID;
        pCRS.localName := DSS.ActiveCircuit.Name + '_CrsUrn';
        StartInstance(F, 'CoordinateSystem', pCRS);
        StringNode(F, 'CoordinateSystem.crsUrn', 'OpenDSSLocalBusCoordinates');
        EndInstance(F, 'CoordinateSystem');

        pRegion := TNamedObject.Create('GeographicalRegion');
        pRegion.UUID := RgnUUID;
        pRegion.LocalName := GeographicRegion;
        StartInstance(F, 'GeographicalRegion', pRegion);
        EndInstance(F, 'GeographicalRegion');

        pSubRegion := TNamedObject.Create('SubGeographicalRegion');
        pSubRegion.UUID := SubGeoUUID;
        pSubRegion.LocalName := SubGeographicRegion;
        StartInstance(F, 'SubGeographicalRegion', pSubRegion);
        RefNode(F, 'SubGeographicalRegion.Region', pRegion);
        EndInstance(F, 'SubGeographicalRegion');

        pSubstation := TNamedObject.Create('Substation');
        pSubstation.UUID := SubUUID;
        pSubstation.LocalName := Substation;
        StartInstance(F, 'Substation', pSubstation);
        RefNode(F, 'Substation.Region', pSubRegion);
        EndInstance(F, 'Substation');

        pLocation := TNamedObject.Create('Location');
        CreateUUID4(geoUUID);
        pLocation.UUID := geoUUID;
        pLocation.localName := DSS.ActiveCircuit.Name + '_Location';
        StartInstance(F, 'Location', pLocation);
        UuidNode(F, 'Location.CoordinateSystem', crsUUID);
        EndInstance(F, 'Location');

        DSS.ActiveCircuit.UUID := FdrUUID;
        StartInstance(F, 'Feeder', DSS.ActiveCircuit);
        RefNode(F, 'Feeder.NormalEnergizingSubstation', pSubstation);
        RefNode(F, 'PowerSystemResource.Location', pLocation);
        EndInstance(F, 'Feeder');

        // the whole system will be a topo island
        pIsland := TNamedObject.Create('Island');
        pIsland.localName := DSS.ActiveCircuit.Name + '_Island';
        CreateUUID4(geoUUID);
        pIsland.UUID := geoUUID;
        pSwing := TNamedObject.Create('SwingBus');
        pSwing.localName := DSS.ActiveCircuit.Name + '_SwingBus';

        pNormLimit := TNamedObject.Create('NormalAmpsType');
        pNormLimit.localName := ActiveCircuit.Name + '_NormAmpsType';
        CreateUUID4(tmpUUID);
        pNormLimit.UUID := tmpUUID;
        StartInstance(F, 'OperationalLimitType', pNormLimit);
        DoubleNode(F, 'OperationalLimitType.acceptableDuration', 5.0e9);  // more than 100 years
        OpLimitDirectionEnum(F, 'absoluteValue');
        EndInstance(F, 'OperationalLimitType');

        pEmergLimit := TNamedObject.Create('EmergencyAmpsType');
        pEmergLimit.localName := ActiveCircuit.Name + '_EmergencyAmpsType';
        CreateUUID4(tmpUUID);
        pEmergLimit.UUID := tmpUUID;
        StartInstance(F, 'OperationalLimitType', pEmergLimit);
        DoubleNode(F, 'OperationalLimitType.acceptableDuration', 2.0 * 3600.0); // 2 hours
        OpLimitDirectionEnum(F, 'absoluteValue');
        EndInstance(F, 'OperationalLimitType');

        pRangeAHiLimit := TNamedObject.Create('RangeAHiType');
        pRangeAHiLimit.localName := ActiveCircuit.Name + '_RangeAHiType';
        CreateUUID4(tmpUUID);
        pRangeAHiLimit.UUID := tmpUUID;
        StartInstance(F, 'OperationalLimitType', pRangeAHiLimit);
        DoubleNode(F, 'OperationalLimitType.acceptableDuration', 5.0e9);
        OpLimitDirectionEnum(F, 'high');
        EndInstance(F, 'OperationalLimitType');

        pRangeALoLimit := TNamedObject.Create('RangeALoType');
        pRangeALoLimit.localName := ActiveCircuit.Name + '_RangeALoType';
        CreateUUID4(tmpUUID);
        pRangeALoLimit.UUID := tmpUUID;
        StartInstance(F, 'OperationalLimitType', pRangeALoLimit);
        DoubleNode(F, 'OperationalLimitType.acceptableDuration', 5.0e9);
        OpLimitDirectionEnum(F, 'low');
        EndInstance(F, 'OperationalLimitType');

        pRangeBHiLimit := TNamedObject.Create('RangeBHiType');
        pRangeBHiLimit.localName := ActiveCircuit.Name + '_RangeBHiType';
        CreateUUID4(tmpUUID);
        pRangeBHiLimit.UUID := tmpUUID;
        StartInstance(F, 'OperationalLimitType', pRangeBHiLimit);
        DoubleNode(F, 'OperationalLimitType.acceptableDuration', 24.0 * 3600.0);
        OpLimitDirectionEnum(F, 'high');
        EndInstance(F, 'OperationalLimitType');

        pRangeBLoLimit := TNamedObject.Create('RangeBLoType');
        pRangeBLoLimit.localName := ActiveCircuit.Name + '_RangeBLoType';
        CreateUUID4(tmpUUID);
        pRangeBLoLimit.UUID := tmpUUID;
        StartInstance(F, 'OperationalLimitType', pRangeBLoLimit);
        DoubleNode(F, 'OperationalLimitType.acceptableDuration', 24.0 * 3600.0);
        OpLimitDirectionEnum(F, 'low');
        EndInstance(F, 'OperationalLimitType');

        begin
      // build the lists of base voltages and operational voltage limits
            i := 1;
            while LegalVoltageBases[i] > 0.0 do
            begin
                pName1.LocalName := GetBaseVName(LegalVoltageBases[i]);
                pName1.UUID := GetBaseVUuid(LegalVoltageBases[i]);
                StartInstance(F, 'BaseVoltage', pName1);
                DoubleNode(F, 'BaseVoltage.nominalVoltage', 1000.0 * LegalVoltageBases[i]);
                EndInstance(F, 'BaseVoltage');

                pName1.LocalName := GetOpLimVName(LegalVoltageBases[i]);
                pName1.UUID := GetOpLimVUuid(LegalVoltageBases[i]);
                StartInstance(F, 'OperationalLimitSet', pName1);
                EndInstance(F, 'OperationalLimitSet');

                pName2.LocalName := pName1.LocalName + '_RangeAHi';
                CreateUUID4(tmpUUID);
                pName2.UUID := tmpUUID;
                StartInstance(F, 'VoltageLimit', pName2);
                RefNode(F, 'OperationalLimit.OperationalLimitSet', pName1);
                RefNode(F, 'OperationalLimit.OperationalLimitType', pRangeAHiLimit);
                DoubleNode(F, 'VoltageLimit.value', 1.05 * 1000.0 * LegalVoltageBases[i]);
                EndInstance(F, 'VoltageLimit');

                pName2.LocalName := pName1.LocalName + '_RangeALo';
                CreateUUID4(tmpUUID);
                pName2.UUID := tmpUUID;
                StartInstance(F, 'VoltageLimit', pName2);
                RefNode(F, 'OperationalLimit.OperationalLimitSet', pName1);
                RefNode(F, 'OperationalLimit.OperationalLimitType', pRangeALoLimit);
                DoubleNode(F, 'VoltageLimit.value', 0.95 * 1000.0 * LegalVoltageBases[i]);
                EndInstance(F, 'VoltageLimit');

                pName2.LocalName := pName1.LocalName + '_RangeBHi';
                CreateUUID4(tmpUUID);
                pName2.UUID := tmpUUID;
                StartInstance(F, 'VoltageLimit', pName2);
                RefNode(F, 'OperationalLimit.OperationalLimitSet', pName1);
                RefNode(F, 'OperationalLimit.OperationalLimitType', pRangeBHiLimit);
                DoubleNode(F, 'VoltageLimit.value', 1.0583333 * 1000.0 * LegalVoltageBases[i]);
                EndInstance(F, 'VoltageLimit');

                pName2.LocalName := pName1.LocalName + '_RangeBLo';
                CreateUUID4(tmpUUID);
                pName2.UUID := tmpUUID;
                StartInstance(F, 'VoltageLimit', pName2);
                RefNode(F, 'OperationalLimit.OperationalLimitSet', pName1);
                RefNode(F, 'OperationalLimit.OperationalLimitType', pRangeBLoLimit);
                DoubleNode(F, 'VoltageLimit.value', 0.9166667 * 1000.0 * LegalVoltageBases[i]);
                EndInstance(F, 'VoltageLimit');

                inc(i);
            end;

            for i := 1 to NumBuses do
            begin
                Buses^[i].localName := BusList.Get(i);
            end;

            // each bus corresponds to a topo node (TODO, do we need topo nodes anymore?) and connectivity node
            for i := 1 to NumBuses do
            begin
                geoUUID := GetDevUuid(Topo, Buses^[i].localName, 1);
                Writeln(F, Format('<cim:TopologicalNode rdf:ID="%s">', [UUIDToCIMString(geoUUID)]));
                StringNode(F, 'IdentifiedObject.mRID', UUIDToCIMString(geoUUID));
                StringNode(F, 'IdentifiedObject.name', Buses^[i].localName);
                UuidNode(F, 'TopologicalNode.TopologicalIsland', pIsland.UUID);
                Writeln(F, '</cim:TopologicalNode>');

                Writeln(F, Format('<cim:ConnectivityNode rdf:ID="%s">',
                    [UUIDToCIMString(Buses^[i].UUID)]));
                StringNode(F, 'IdentifiedObject.mRID', UUIDToCIMString(Buses^[i].UUID));
                StringNode(F, 'IdentifiedObject.name', Buses^[i].localName);
                UuidNode(F, 'ConnectivityNode.TopologicalNode', geoUUID);
                UuidNode(F, 'ConnectivityNode.OperationalLimitSet', GetOpLimVUuid(sqrt(3.0) * ActiveCircuit.Buses^[i].kVBase));
                Writeln(F, Format('  <cim:ConnectivityNode.ConnectivityNodeContainer rdf:resource="#%s"/>',
                    [DSS.ActiveCircuit.CIM_ID]));
                Writeln(F, '</cim:ConnectivityNode>');
            end;

            // find the swing bus ==> first voltage source
            pVsrc := DSS.ActiveCircuit.Sources.First; // pIsrc are in the same list
            while pVsrc <> NIL do
            begin
                if pVsrc.ClassNameIs('TVSourceObj') then
                begin
                    if pVsrc.Enabled then
                    begin
                        i := pVsrc.Terminals^[1].BusRef;
                        geoUUID := GetDevUuid(Topo, Buses^[i].localName, 1);
                        pSwing.UUID := geoUUID;
                        StartInstance(F, 'TopologicalIsland', pIsland);
                        RefNode(F, 'TopologicalIsland.AngleRefTopologicalNode', pSwing);
                        EndInstance(F, 'TopologicalIsland');
                        break;
                    end;
                end;
                pVsrc := DSS.ActiveCircuit.Sources.Next;
            end;
        end;

        pGen := DSS.ActiveCircuit.Generators.First;
        while pGen <> NIL do
        begin
            if pGen.Enabled then
            begin
                StartInstance(F, 'SynchronousMachine', pGen);
                CircuitNode(F, DSS.ActiveCircuit);
                DoubleNode(F, 'SynchronousMachine.p', pGen.Presentkw * 1000.0);
                DoubleNode(F, 'SynchronousMachine.q', pGen.Presentkvar * 1000.0);
                DoubleNode(F, 'SynchronousMachine.ratedS', pGen.GenVars.kvarating * 1000.0);
                DoubleNode(F, 'SynchronousMachine.ratedU', pGen.Presentkv * 1000.0);
//        SynchMachTypeEnum (F, 'generator');
//        SynchMachModeEnum (F, 'generator');
                CreateUUID4(geoUUID);
                UuidNode(F, 'PowerSystemResource.Location', geoUUID);
                EndInstance(F, 'SynchronousMachine');
                AttachGeneratorPhases(DSS, F, pGen, geoUUID);
                WriteTerminals(DSS, F, pGen, geoUUID, crsUUID);
            end;
            pGen := DSS.ActiveCircuit.Generators.Next;
        end;

        pPV := DSS.ActiveCircuit.PVSystems.First;
        while pPV <> NIL do
        begin
            if pPV.Enabled then
            begin
                pName1.LocalName := pPV.Name; // + '_PVPanels';
                CreateUUID4(geoUUID);
                pName1.UUID := geoUUID;
                StartInstance(F, 'PhotovoltaicUnit', pName1);
                geoUUID := GetDevUuid(SolarLoc, pPV.localName, 1);
                UuidNode(F, 'PowerSystemResource.Location', geoUUID);
                EndInstance(F, 'PhotovoltaicUnit');
                StartInstance(F, 'PowerElectronicsConnection', pPV);
                CircuitNode(F, DSS.ActiveCircuit);
                RefNode(F, 'PowerElectronicsConnection.PowerElectronicsUnit', pName1);
                DoubleNode(F, 'PowerElectronicsConnection.maxIFault', 1.0 / pPV.MinModelVoltagePU);
                DoubleNode(F, 'PowerElectronicsConnection.p', pPV.Presentkw * 1000.0);
                DoubleNode(F, 'PowerElectronicsConnection.q', pPV.Presentkvar * 1000.0);
                DoubleNode(F, 'PowerElectronicsConnection.ratedS', pPV.PVSystemVars.fkvarating * 1000.0);
                DoubleNode(F, 'PowerElectronicsConnection.ratedU', pPV.Presentkv * 1000.0);
                UuidNode(F, 'PowerSystemResource.Location', geoUUID);
                EndInstance(F, 'PowerElectronicsConnection');
                AttachSolarPhases(DSS, F, pPV, geoUUID);
        // we want the location using PV unit name
                WriteReferenceTerminals(DSS, F, pPV, geoUUID, crsUUID, pPV.UUID);
                s := pPV.LocalName;
                pPV.LocalName := pName1.LocalName;
                WritePositions(DSS, F, pPV, geoUUID, crsUUID);
                pPV.LocalName := s;
            end;
            pPV := DSS.ActiveCircuit.PVSystems.Next;
        end;

        pBat := DSS.ActiveCircuit.StorageElements.First;
        while pBat <> NIL do
        begin
            if pBat.Enabled then
            begin
                pName1.LocalName := pBat.Name; // + '_Cells';
                CreateUUID4(geoUUID);
                pName1.UUID := geoUUID;
                StartInstance(F, 'BatteryUnit', pName1);
                DoubleNode(F, 'BatteryUnit.ratedE', pBat.StorageVars.kwhRating * 1000.0);
                DoubleNode(F, 'BatteryUnit.storedE', pBat.StorageVars.kwhStored * 1000.0);
                BatteryStateEnum(F, pBat.StorageState);
                geoUUID := GetDevUuid(BatteryLoc, pBat.localName, 1);
                UuidNode(F, 'PowerSystemResource.Location', geoUUID);
                EndInstance(F, 'BatteryUnit');
                StartInstance(F, 'PowerElectronicsConnection', pBat);
                CircuitNode(F, DSS.ActiveCircuit);
                RefNode(F, 'PowerElectronicsConnection.PowerElectronicsUnit', pName1);
                DoubleNode(F, 'PowerElectronicsConnection.maxIFault', 1.0 / pBat.MinModelVoltagePU);
                DoubleNode(F, 'PowerElectronicsConnection.p', pBat.Presentkw * 1000.0);
                DoubleNode(F, 'PowerElectronicsConnection.q', pBat.Presentkvar * 1000.0);
                DoubleNode(F, 'PowerElectronicsConnection.ratedS', pBat.StorageVars.kvarating * 1000.0);
                DoubleNode(F, 'PowerElectronicsConnection.ratedU', pBat.Presentkv * 1000.0);
                UuidNode(F, 'PowerSystemResource.Location', geoUUID);
                EndInstance(F, 'PowerElectronicsConnection');
                AttachStoragePhases(DSS, F, pBat, geoUUID);
        // we want the location using battery unit name
                WriteReferenceTerminals(DSS, F, pBat, geoUUID, crsUUID, pBat.UUID);
                s := pBat.LocalName;
                pBat.LocalName := pName1.LocalName;
                WritePositions(DSS, F, pBat, geoUUID, crsUUID);
                pBat.LocalName := s;
            end;
            pBat := DSS.ActiveCircuit.StorageElements.Next;
        end;

        pVsrc := DSS.ActiveCircuit.Sources.First; // pIsrc are in the same list
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

                        StartInstance(F, 'EnergySource', pVsrc);
                        CircuitNode(F, DSS.ActiveCircuit);
                        VbaseNode(DSS, F, pVsrc);
                        DoubleNode(F, 'EnergySource.nominalVoltage', 1000 * kVbase);
                        DoubleNode(F, 'EnergySource.voltageMagnitude', 1000 * kVbase * PerUnit);
                        DoubleNode(F, 'EnergySource.voltageAngle', TwoPi * Angle / 360.0);
                        DoubleNode(F, 'EnergySource.r', R1);
                        DoubleNode(F, 'EnergySource.x', X1);
                        DoubleNode(F, 'EnergySource.r0', R0);
                        DoubleNode(F, 'EnergySource.x0', X0);
                        CreateUUID4(geoUUID);
                        UuidNode(F, 'PowerSystemResource.Location', geoUUID);
                        EndInstance(F, 'EnergySource');
//          AttachPhases (F, pVsrc, 1, 'EnergySource');
                        WriteTerminals(DSS, F, pVsrc, geoUUID, crsUUID);
                    end;
            pVsrc := DSS.ActiveCircuit.Sources.Next;
        end;

        pCap := DSS.ActiveCircuit.ShuntCapacitors.First;
        while pCap <> NIL do
        begin
            if pCap.Enabled then
            begin
                StartInstance(F, 'LinearShuntCompensator', pCap);
                CircuitNode(F, DSS.ActiveCircuit);
                VbaseNode(DSS, F, pCap);
                with pCap do
                begin
                    val := 0.001 * Totalkvar / NomKV / NomKV / NumSteps;
                    DoubleNode(F, 'ShuntCompensator.nomU', 1000.0 * NomKV);
                    DoubleNode(F, 'LinearShuntCompensator.bPerSection', val);
                    DoubleNode(F, 'LinearShuntCompensator.gPerSection', 0.0);

                    val := 0.0;
                    pCapC := DSS.ActiveCircuit.CapControls.First;
                    while (pCapC <> NIL) do
                    begin
                        if pCapC.This_Capacitor = pCap then
                            val := pCapC.OnDelayVal;
                        pCapC := DSS.ActiveCircuit.CapControls.Next;
                    end;
                    DoubleNode(F, 'ShuntCompensator.aVRDelay', val);

                    if Connection = 0 then
                    begin
                        ShuntConnectionKindNode(F, 'ShuntCompensator', 'Y');
                        BooleanNode(F, 'ShuntCompensator.grounded', TRUE);  // TODO - check bus 2
                        DoubleNode(F, 'LinearShuntCompensator.b0PerSection', val);
                    end
                    else
                    begin
                        ShuntConnectionKindNode(F, 'ShuntCompensator', 'D');
                        BooleanNode(F, 'LinearShuntCompensator.grounded', FALSE);
                        DoubleNode(F, 'LinearShuntCompensator.b0PerSection', 0.0);
                    end;
                    DoubleNode(F, 'LinearShuntCompensator.g0PerSection', 0.0);
                    IntegerNode(F, 'ShuntCompensator.normalSections', NumSteps);
                    IntegerNode(F, 'ShuntCompensator.maximumSections', NumSteps);
                    geoUUID := GetDevUuid(CapLoc, pCap.localName, 1);
                    UuidNode(F, 'PowerSystemResource.Location', geoUUID);
                    EndInstance(F, 'LinearShuntCompensator');
                    AttachCapPhases(DSS, F, pCap, geoUUID);
                    WriteTerminals(DSS, F, pCap, geoUUID, crsUUID, pCap.NormAmps, pCap.EmergAmps);
                end;
            end;
            pCap := DSS.ActiveCircuit.ShuntCapacitors.Next;
        end;

        pCapC := DSS.ActiveCircuit.CapControls.First;
        while (pCapC <> NIL) do
        begin
            with pCapC do
            begin
                StartInstance(F, 'RegulatingControl', pCapC);
                UuidNode(F, 'PowerSystemResource.Location', GetDevUuid(CapLoc, This_Capacitor.Name, 1));
                RefNode(F, 'RegulatingControl.RegulatingCondEq', This_Capacitor);
                i1 := GetCktElementIndex(DSS, ElementName); // Global function
                UuidNode(F, 'RegulatingControl.Terminal',
                    GetTermUuid(DSS.ActiveCircuit.CktElements.Get(i1), ElementTerminal));
                s := FirstPhaseString(DSS, DSS.ActiveCircuit.CktElements.Get(i1), 1);
                if PTPhase > 0 then
                    MonitoredPhaseNode(F, Char(Ord(s[1]) + PTPhase - 1))
                else
                    MonitoredPhaseNode(F, Char(Ord(s[1]))); // TODO - average, min and max unsupported in CIM
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
                        RegulatingControlEnum(F, 'currentFlow');
                    VOLTAGECONTROL:
                        RegulatingControlEnum(F, 'voltage');
                    KVARCONTROL:
                        RegulatingControlEnum(F, 'reactivePower');
                    TIMECONTROL:
                        RegulatingControlEnum(F, 'timeScheduled');
                    PFCONTROL:
                        RegulatingControlEnum(F, 'powerFactor');
                    USERCONTROL:
                        RegulatingControlEnum(F, 'userDefined'); // i.e. unsupported in CIM
                end;
                BooleanNode(F, 'RegulatingControl.discrete', TRUE);
                BooleanNode(F, 'RegulatingControl.enabled', Enabled);
                DoubleNode(F, 'RegulatingControl.targetValue', val * 0.5 * (v1 + v2));
                DoubleNode(F, 'RegulatingControl.targetDeadband', val * (v2 - v1));
                EndInstance(F, 'RegulatingControl');
            end;
            pCapC := DSS.ActiveCircuit.CapControls.Next;
        end;

    // begin the transformers; 
        //   1. if balanced three-phase and no XfmrCode, use PowerTransformerEnd(s), mesh impedances and core admittances with no tanks
    //   2. with XfmrCode, write TransformerTank, TransformerTankEnd(s) and references to TransformerTankInfoInfo
    //   3. otherwise, write TransformerTank, then create and reference TransformerTankInfo classes

    // for case 3, it's better to identify and create the info classes first
    //    TODO: side effect is that these transformers will reference XfmrCode until the text file is reloaded. Solution results should be the same.
        pXf := DSS.ActiveCircuit.Transformers.First;
        while pXf <> NIL do
        begin
            if pXf.Enabled then
            begin
                if (length(pXf.XfmrCode) < 1) and (pXf.NPhases <> 3) then
                begin
                    sBank := 'CIMXfmrCode_' + pXf.Name;
                    clsXfmr.NewObject(sBank);
                    clsXfmr.Code := sBank;
                    pXfmr := DSS.ActiveXfmrCodeObj;
                    CreateUUID4(tmpUUID);
                    pXfmr.UUID := tmpUUID;
                    pXfmr.PullFromTransformer(pXf);
                    pXf.XfmrCode := pXfmr.Name;
                end;
            end;
            pXf := DSS.ActiveCircuit.Transformers.Next;
        end;

        // write all the XfmrCodes first (CIM TransformerTankInfo)
        pXfmr := clsXfmr.ElementList.First;
        while pXfmr <> NIL do
        begin
            WriteXfmrCode(F, pXfmr);
      // link to the transformers using this XfmrCode
            pName1.LocalName := 'TankAsset_' + pXfmr.Name;
            CreateUUID4(tmpUUID);
            pName1.UUID := tmpUUID;
            StartInstance(F, 'Asset', pName1);
            RefNode(F, 'Asset.AssetInfo', pXfmr);
            pXf := DSS.ActiveCircuit.Transformers.First;
            while pXf <> NIL do
            begin
                if pXf.XfmrCode = pXfmr.Name then
                    RefNode(F, 'Asset.PowerSystemResources', pXf);
                pXf := DSS.ActiveCircuit.Transformers.Next;
            end;
            EndInstance(F, 'Asset');
            pXfmr := clsXfmr.ElementList.Next;
        end;

    // create all the banks (CIM PowerTransformer)
        maxWdg := 0;
        pXf := DSS.ActiveCircuit.Transformers.First;
        while pXf <> NIL do
        begin
            if pXf.Enabled then
                if pXf.NumberOfWindings > maxWdg then
                    maxWdg := pXf.NumberofWindings;
            pXf := DSS.ActiveCircuit.Transformers.Next;
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

        pXf := DSS.ActiveCircuit.Transformers.First;
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
                    pBank := TBankObject.Create(maxWdg);
                    pBank.localName := sBank;
                    pBank.UUID := GetDevUuid(Bank, sBank, 0);
                    AddBank(pBank);
                end;
            end;
            pXf := DSS.ActiveCircuit.Transformers.Next;
        end;

    // write all the transformers, according to the three cases
        pXf := DSS.ActiveCircuit.Transformers.First;
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
                    if (length(pXf.XfmrCode) < 1) and (pXf.NPhases = 3) then
                        bTanks := FALSE; // case 1, balanced three-phase

                    pBank := GetBank(sBank);
                    pBank.AddTransformer(DSS, pXf);
                    geoUUID := GetDevUuid(XfLoc, pXf.Name, 1);

                    if bTanks then
                    begin
                        StartInstance(F, 'TransformerTank', pXf);
                        CircuitNode(F, DSS.ActiveCircuit);
                        RefNode(F, 'TransformerTank.PowerTransformer', pBank);
                        UuidNode(F, 'PowerSystemResource.Location', geoUUID);
                        EndInstance(F, 'TransformerTank');
                        WritePositions(DSS, F, pXf, geoUUID, crsUUID);
                    end
                    else
                    begin
                        WritePositions(DSS, F, pXf, geoUUID, crsUUID);
                    end;

        // make the winding, mesh and core name objects for easy reference
                    for i := 1 to NumberOfWindings do
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
                        StartInstance(F, 'TransformerCoreAdmittance', CoreList[0]);
                        val := pXf.noLoadLossPct / 100.0 / zbase;
                        DoubleNode(F, 'TransformerCoreAdmittance.g', val);
                        DoubleNode(F, 'TransformerCoreAdmittance.g0', val);
                        val := pXf.imagPct / 100.0 / zbase;
                        DoubleNode(F, 'TransformerCoreAdmittance.b', val);
                        DoubleNode(F, 'TransformerCoreAdmittance.b0', val);
                        RefNode(F, 'TransformerCoreAdmittance.TransformerEnd', WdgList[0]);
                        EndInstance(F, 'TransformerCoreAdmittance');
                        seq := 1; // write mesh Z
                        for i := 1 to NumberOfWindings do
                        begin
                            for k := i + 1 to NumberOfWindings do
                            begin
                                val := BaseKVLL[i];
                                zbase := 1000.0 * val * val / WdgKva[i];
                                StartInstance(F, 'TransformerMeshImpedance', MeshList[seq - 1]);
                                val := zbase * (WdgResistance[i] + WdgResistance[k]);
                                DoubleNode(F, 'TransformerMeshImpedance.r', val);
                                DoubleNode(F, 'TransformerMeshImpedance.r0', val);
                                val := zbase * XscVal[seq];
                                inc(seq);
                                DoubleNode(F, 'TransformerMeshImpedance.x', val);
                                DoubleNode(F, 'TransformerMeshImpedance.x0', val);
                                RefNode(F, 'TransformerMeshImpedance.FromTransformerEnd', WdgList[i - 1]);
                                RefNode(F, 'TransformerMeshImpedance.ToTransformerEnd', WdgList[k - 1]);
                                EndInstance(F, 'TransformerMeshImpedance');
                            end;
                        end;
                    end;

        // write the Ends, and a Terminal for each End
                    for i := 1 to NumberOfWindings do
                    begin
                        if bTanks then
                        begin
                            StartInstance(F, 'TransformerTankEnd', WdgList[i - 1]);
                            XfmrPhasesEnum(DSS, F, pXf, i);
                            RefNode(F, 'TransformerTankEnd.TransformerTank', pXf);
                        end
                        else
                        begin
                            StartInstance(F, 'PowerTransformerEnd', WdgList[i - 1]);
                            RefNode(F, 'PowerTransformerEnd.PowerTransformer', pBank);
                            DoubleNode(F, 'PowerTransformerEnd.ratedS', 1000 * WdgKva[i]);
                            DoubleNode(F, 'PowerTransformerEnd.ratedU', 1000 * Winding^[i].kvll);
                            zbase := 1000.0 * BaseKVLL[i] * BaseKVLL[i] / WdgKva[i];
                            DoubleNode(F, 'PowerTransformerEnd.r', zbase * WdgResistance[i]);
                            if Winding^[i].Connection = 1 then
                                WindingConnectionKindNode(F, 'D')
                            else
                            if (Winding^[i].Rneut > 0.0) or (Winding^[i].Xneut > 0.0) then
                                WindingConnectionKindNode(F, 'Yn')
                            else
                                WindingConnectionKindNode(F, 'Y');
                            if Winding^[i].Connection <> Winding^[1].Connection then  // TODO - this assumes HV winding first, and normal usages
                                IntegerNode(F, 'PowerTransformerEnd.phaseAngleClock', 1)
                            else
                                IntegerNode(F, 'PowerTransformerEnd.phaseAngleClock', 0);
                        end;
                        IntegerNode(F, 'TransformerEnd.endNumber', i);
                        j := (i - 1) * pXf.NConds + pXf.Nphases + 1;
//          Writeln (Format ('# %s wdg=%d conn=%d nterm=%d nref=%d',
//            [pXf.Name, i, Winding^[i].Connection, j, pXf.NodeRef^[j]]));
                        if (Winding^[i].Connection = 1) then
                        begin // delta
                            BooleanNode(F, 'TransformerEnd.grounded', FALSE);
                        end
                        else
                        if (pXf.NodeRef^[j] = 0) then
                        begin // last conductor is grounded solidly
                            BooleanNode(F, 'TransformerEnd.grounded', TRUE);
                            DoubleNode(F, 'TransformerEnd.rground', 0.0);
                            DoubleNode(F, 'TransformerEnd.xground', 0.0);
                        end
                        else
                        if (Winding^[i].Rneut < 0.0) then
                        begin // probably wye ungrounded
                            BooleanNode(F, 'TransformerEnd.grounded', FALSE);
                        end
                        else
                        begin // not delta, not wye solidly grounded or ungrounded
                            BooleanNode(F, 'TransformerEnd.grounded', TRUE);
                            DoubleNode(F, 'TransformerEnd.rground', Winding^[i].Rneut);
                            DoubleNode(F, 'TransformerEnd.xground', Winding^[i].Xneut);
                        end;
                        j := pXf.Terminals^[i].BusRef;
                        pName2.LocalName := pXf.Name + '_T' + IntToStr(i);
                        pName2.UUID := GetTermUuid(pXf, i);
                        RefNode(F, 'TransformerEnd.Terminal', pName2);
                        UuidNode(F, 'TransformerEnd.BaseVoltage', GetBaseVUuid(sqrt(3.0) * DSS.ActiveCircuit.Buses^[j].kVBase));
                        if bTanks then
                            EndInstance(F, 'TransformerTankEnd')
                        else
                            EndInstance(F, 'PowerTransformerEnd');
          // write the Terminal for this End
                        StartInstance(F, 'Terminal', pName2);
                        RefNode(F, 'Terminal.ConductingEquipment', pBank);
                        IntegerNode(F, 'ACDCTerminal.sequenceNumber', i);
                        Writeln(F, Format('<cim:Terminal.ConnectivityNode rdf:resource="#%s"/>',
                            [DSS.ActiveCircuit.Buses[j].CIM_ID]));
                        if i = 1 then
                        begin   // write the current limit on HV winding, assuming that's winding 1
                            LimitName := GetOpLimIName(pXf.NormAmps, pXf.EmergAmps);
                            pILimit := GetOpLimit(LimitName);
                            if pILimit = NIL then
                            begin
                                pILimit := TOpLimitObject.Create(pXf.NormAmps, pXf.EmergAmps);
                                pILimit.localName := LimitName;
                                pILimit.UUID := GetDevUuid(OpLimI, LimitName, 0);
                                AddOpLimit(pILimit);
                            end;
                            LimiTUuid := GetDevUuid(OpLimI, LimitName, 0);
                            UuidNode(F, 'ACDCTerminal.OperationalLimitSet', LimiTUuid);
                        end;
                        EndInstance(F, 'Terminal');
                    end;
                end;
            pXf := DSS.ActiveCircuit.Transformers.Next;
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
            StartInstance(F, 'PowerTransformer', pBank);
            CircuitNode(F, DSS.ActiveCircuit);
            StringNode(F, 'PowerTransformer.vectorGroup', pBank.vectorGroup);
            UuidNode(F, 'PowerSystemResource.Location',
                GetDevUuid(XfLoc, pBank.a_unit.Name, 1));
            EndInstance(F, 'PowerTransformer');
        end;

        WdgList := NIL;
        CoreList := NIL;
        MeshList := NIL;

    // voltage regulators
        pReg := DSS.ActiveCircuit.RegControls.First;
        while (pReg <> NIL) do
        begin
            with pReg do
            begin
                pName1.LocalName := pReg.LocalName + '_Info';
                CreateUUID4(geoUUID);
                pName1.UUID := geoUUID;
                StartInstance(F, 'TapChangerInfo', pName1);
                DoubleNode(F, 'TapChangerInfo.ptRatio', PT);
                DoubleNode(F, 'TapChangerInfo.ctRatio', CT / 0.2);
                DoubleNode(F, 'TapChangerInfo.ctRating', CT);
                EndInstance(F, 'TapChangerInfo');

                pName2.LocalName := pReg.LocalName + '_Ctrl';
                CreateUUID4(geoUUID);
                pName2.UUID := geoUUID;
                StartInstance(F, 'TapChangerControl', pName2);
                RegulatingControlEnum(F, 'voltage');
                UuidNode(F, 'RegulatingControl.Terminal', GetTermUuid(Transformer, TrWinding));
                MonitoredPhaseNode(F, FirstPhaseString(DSS, Transformer, TrWinding));
                BooleanNode(F, 'RegulatingControl.enabled', pReg.Enabled);
                BooleanNode(F, 'RegulatingControl.discrete', TRUE);
                DoubleNode(F, 'RegulatingControl.targetValue', TargetVoltage);
                DoubleNode(F, 'RegulatingControl.targetDeadband', BandVoltage);
                BooleanNode(F, 'TapChangerControl.lineDropCompensation', UseLineDrop);
                DoubleNode(F, 'TapChangerControl.lineDropR', LineDropR);
                DoubleNode(F, 'TapChangerControl.lineDropX', LineDropX);
                if UseReverseDrop then
                begin
                    DoubleNode(F, 'TapChangerControl.reverseLineDropR', RevLineDropR);
                    DoubleNode(F, 'TapChangerControl.reverseLineDropX', RevLineDropX)
                end
                else
                begin
                    DoubleNode(F, 'TapChangerControl.reverseLineDropR', 0.0);
                    DoubleNode(F, 'TapChangerControl.reverseLineDropX', 0.0)
                end;
                if UseLimit then
                    DoubleNode(F, 'TapChangerControl.limitVoltage', VoltageLimit)
                else
                    DoubleNode(F, 'TapChangerControl.limitVoltage', 0.0);
                UuidNode(F, 'PowerSystemResource.Location',
                    GetDevUuid(XfLoc, Transformer.Name, 1));
                EndInstance(F, 'TapChangerControl');

                StartInstance(F, 'RatioTapChanger', pReg);
                UuidNode(F, 'RatioTapChanger.TransformerEnd',
                    GetDevUuid(Wdg, Transformer.Name, TrWinding));
                UuidNode(F, 'TapChanger.TapChangerControl', pName2.UUID);
                DoubleNode(F, 'RatioTapChanger.stepVoltageIncrement', 100.0 * TapIncrement);
                TransformerControlEnum(F, 'volt');
                IntegerNode(F, 'TapChanger.highStep', NumTaps div 2);
                IntegerNode(F, 'TapChanger.lowStep', -NumTaps div 2);
                IntegerNode(F, 'TapChanger.neutralStep', 0);
                IntegerNode(F, 'TapChanger.normalStep', 0);
                DoubleNode(F, 'TapChanger.neutralU', 120.0 * PT);
                DoubleNode(F, 'TapChanger.initialDelay', InitialDelay);
                DoubleNode(F, 'TapChanger.subsequentDelay', SubsequentDelay);
                BooleanNode(F, 'TapChanger.ltcFlag', TRUE);
                BooleanNode(F, 'TapChanger.controlEnabled', pReg.Enabled);
                DoubleNode(F, 'TapChanger.step', TapNum);
                UuidNode(F, 'PowerSystemResource.Location',
                    GetDevUuid(XfLoc, Transformer.Name, 1));
                EndInstance(F, 'RatioTapChanger');

                pName2.LocalName := 'TapChangerAsset_' + pReg.LocalName;
                CreateUUID4(tmpUUID);
                pName2.UUID := tmpUUID;
                StartInstance(F, 'Asset', pName2);
                RefNode(F, 'Asset.AssetInfo', pName1);
                RefNode(F, 'Asset.PowerSystemResources', pReg);
                EndInstance(F, 'Asset');
            end;
            pReg := DSS.ActiveCircuit.RegControls.Next;
        end;

    // done with the transformers

        // series reactors, exported as lines
        pReac := DSS.ActiveCircuit.Reactors.First;
        while pReac <> NIL do
        begin
            if pReac.Enabled then
            begin
                StartInstance(F, 'ACLineSegment', pReac);
                CircuitNode(F, DSS.ActiveCircuit);
                VbaseNode(DSS, F, pReac);
                geoUUID := GetDevUuid(ReacLoc, pReac.Name, 1);
                UuidNode(F, 'PowerSystemResource.Location', geoUUID);
                DoubleNode(F, 'Conductor.length', 1.0);
                DoubleNode(F, 'ACLineSegment.r', pReac.SimpleR);
                DoubleNode(F, 'ACLineSegment.x', pReac.SimpleX);
                DoubleNode(F, 'ACLineSegment.bch', 0.0);
                DoubleNode(F, 'ACLineSegment.gch', 0.0);
                DoubleNode(F, 'ACLineSegment.r0', pReac.SimpleR);
                DoubleNode(F, 'ACLineSegment.x0', pReac.SimpleX);
                DoubleNode(F, 'ACLineSegment.b0ch', 0.0);
                DoubleNode(F, 'ACLineSegment.b0ch', 0.0);
                EndInstance(F, 'ACLineSegment');
                // AttachLinePhases (F, pReac); // for the 8500-node circuit, we only need 3 phase series reactors
                WriteTerminals(DSS, F, pReac, geoUUID, crsUUID, pReac.NormAmps, pReac.EmergAmps);
            end;
            pReac := DSS.ActiveCircuit.Reactors.Next;
        end;

        pLine := DSS.ActiveCircuit.Lines.First;
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
                        ParseSwitchClass(DSS, pLine, swtCls, ratedAmps, breakingAmps);
                        StartInstance(F, swtCls, pLine);
                        CircuitNode(F, DSS.ActiveCircuit);
                        VbaseNode(DSS, F, pLine);
                        if breakingAmps > 0.0 then
                            DoubleNode(F, 'ProtectedSwitch.breakingCapacity', breakingAmps); // Fuse and Sectionaliser don't have this, others do
                        DoubleNode(F, 'Switch.ratedCurrent', ratedAmps);
          // some OpenDSS models have enabled=false to signal open switches, but we can't actually
          // export them because disabled elements don't have terminal references in memory
                        if Enabled then
                        begin
                            BooleanNode(F, 'Switch.normalOpen', not pLine.Closed[0]);
                            BooleanNode(F, 'Switch.open', not pLine.Closed[0]);
                        end
                        else
                        begin
                            BooleanNode(F, 'Switch.normalOpen', TRUE);
                            BooleanNode(F, 'Switch.open', TRUE);
                        end;
                        BooleanNode(F, 'Switch.retained', TRUE);
                        UuidNode(F, 'PowerSystemResource.Location', geoUUID);
                        EndInstance(F, swtCls);
                        AttachSwitchPhases(F, pLine);
                    end
                    else
                    begin
                        StartInstance(F, 'ACLineSegment', pLine);
                        CircuitNode(F, DSS.ActiveCircuit);
                        VbaseNode(DSS, F, pLine);
                        if LineCodeSpecified then
                        begin
                            DoubleNode(F, 'Conductor.length', Len * v1);
                            LineCodeRefNode(F, clsCode, pLine.CondCode);
                        end
                        else
                        if GeometrySpecified then
                        begin
                            DoubleNode(F, 'Conductor.length', Len * v1);
                            LineSpacingRefNode(F, clsGeom, pLine.GeometryCode);
                        end
                        else
                        if SpacingSpecified then
                        begin
                            DoubleNode(F, 'Conductor.length', Len * v1);
                            LineSpacingRefNode(F, clsSpac, pLine.SpacingCode);
                        end
                        else
                        begin
                            if SymComponentsModel and (NPhases = 3) then
                            begin
                                val := 1.0e-9 * TwoPi * BaseFrequency; // convert nF to mhos
                                DoubleNode(F, 'Conductor.length', 1.0); // we don't know the physical length
                                DoubleNode(F, 'ACLineSegment.r', Len * R1); // total ohms
                                DoubleNode(F, 'ACLineSegment.x', Len * X1);
                                DoubleNode(F, 'ACLineSegment.bch', Len * C1 * val);
                                DoubleNode(F, 'ACLineSegment.gch', 0.0);
                                DoubleNode(F, 'ACLineSegment.r0', Len * R0);
                                DoubleNode(F, 'ACLineSegment.x0', Len * X0);
                                DoubleNode(F, 'ACLineSegment.b0ch', Len * C0 * val);
                                DoubleNode(F, 'ACLineSegment.b0ch', 0.0);
                            end
                            else
                            begin
                                bval := TRUE;
                                pName1.LocalName := pLine.Name + '_PUZ';
                                CreateUUID4(tmpUUID);
                                pName1.UUID := tmpUUID;
                                RefNode(F, 'ACLineSegment.PerLengthImpedance', pName1);
              // TODO - we no longer have proper length units if matrices were specified
                                DoubleNode(F, 'Conductor.length', Len * v1);
                            end;
                        end;
                        UuidNode(F, 'PowerSystemResource.Location', geoUUID);
                        EndInstance(F, 'ACLineSegment');
                        if not (SymComponentsModel and (NPhases = 3)) then
                            AttachLinePhases(DSS, F, pLine);
                        if bVal = TRUE then
                        begin  // writing PuZ on the fly
                            StartInstance(F, 'PerLengthPhaseImpedance', pName1);
                            IntegerNode(F, 'PerLengthPhaseImpedance.conductorCount', NPhases);
                            EndInstance(F, 'PerLengthPhaseImpedance');
                            for i := 1 to NPhases do
                            begin
                                for j := 1 to i do
                                begin
                                    StartFreeInstance(F, 'PhaseImpedanceData');
                                    RefNode(F, 'PhaseImpedanceData.PhaseImpedance', pName1);
                                    IntegerNode(F, 'PhaseImpedanceData.row', i);
                                    IntegerNode(F, 'PhaseImpedanceData.column', j);
                                    DoubleNode(F, 'PhaseImpedanceData.r', Z.GetElement(i, j).re / 1609.34);
                                    DoubleNode(F, 'PhaseImpedanceData.x', Z.GetElement(i, j).im / 1609.34);
                                    DoubleNode(F, 'PhaseImpedanceData.b', YC.GetElement(i, j).im / 1609.34);
                                    EndInstance(F, 'PhaseImpedanceData')
                                end;
                            end;
                        end;
                    end;
                    WriteTerminals(DSS, F, pLine, geoUUID, crsUUID, pLine.NormAmps, pLine.EmergAmps);
                end;
            pLine := DSS.ActiveCircuit.Lines.Next;
        end;

    // create the DSS-like load models
        CreateUUID4(id1_ConstkVA);
        CreateUUID4(id2_ConstZ);
        CreateUUID4(id3_ConstPQuadQ);
        CreateUUID4(id4_LinPQuadQ);
        CreateUUID4(id5_ConstI);
        CreateUUID4(id6_ConstPConstQ);  // P can vary, Q not
        CreateUUID4(id7_ConstPConstX);

        WriteLoadModel(F, 'Constant kVA', id1_ConstkVA,
            0, 0, 100,
            0, 0, 100,
            0, 0);
        WriteLoadModel(F, 'Constant Z', id2_ConstZ,
            100, 0, 0,
            100, 0, 0,
            0, 0);
        WriteLoadModel(F, 'Motor', id3_ConstPQuadQ,
            0, 0, 100,
            100, 0, 0,
            0, 0);
        WriteLoadModel(F, 'Mix Motor/Res', id4_LinPQuadQ,
            0, 0, 0,
            0, 0, 0,
            1, 2);
        WriteLoadModel(F, 'Constant I', id5_ConstI,
            0, 100, 0,
            0, 100, 0,
            0, 0);
        WriteLoadModel(F, 'Variable P, Fixed Q', id6_ConstPConstQ,
            0, 0, 100,
            0, 0, 100,
            0, 0);
        WriteLoadModel(F, 'Variable P, Fixed X', id7_ConstPConstX,
            0, 0, 100,
            100, 0, 0,
            0, 0);

        pLoad := DSS.ActiveCircuit.Loads.First;
        while pLoad <> NIL do
        begin
            if pLoad.Enabled then
                with pLoad do
                begin
                    StartInstance(F, 'EnergyConsumer', pLoad);
                    CircuitNode(F, DSS.ActiveCircuit);
                    VbaseNode(DSS, F, pLoad);
                    case FLoadModel of
                        1:
                            UuidNode(F, 'EnergyConsumer.LoadResponse', id1_ConstkVA);
                        2:
                            UuidNode(F, 'EnergyConsumer.LoadResponse', id2_ConstZ);
                        3:
                            UuidNode(F, 'EnergyConsumer.LoadResponse', id3_ConstPQuadQ);
                        4:
                            UuidNode(F, 'EnergyConsumer.LoadResponse', id4_LinPQuadQ);
                        5:
                            UuidNode(F, 'EnergyConsumer.LoadResponse', id5_ConstI);
                        6:
                            UuidNode(F, 'EnergyConsumer.LoadResponse', id6_ConstPConstQ);
                        7:
                            UuidNode(F, 'EnergyConsumer.LoadResponse', id7_ConstPConstX);
                    end;
                    DoubleNode(F, 'EnergyConsumer.p', 1000.0 * kWBase);
                    DoubleNode(F, 'EnergyConsumer.q', 1000.0 * kvarBase);
                    IntegerNode(F, 'EnergyConsumer.customerCount', NumCustomers);
                    if Connection = 0 then
                    begin
                        ShuntConnectionKindNode(F, 'EnergyConsumer', 'Y');
                        BooleanNode(F, 'EnergyConsumer.grounded', TRUE);  // TODO - check bus 2
                    end
                    else
                    begin
                        ShuntConnectionKindNode(F, 'EnergyConsumer', 'D');
                        BooleanNode(F, 'EnergyConsumer.grounded', FALSE);
                    end;
                    CreateUUID4(geoUUID);
                    UuidNode(F, 'PowerSystemResource.Location', geoUUID);
                    EndInstance(F, 'EnergyConsumer');
                    AttachLoadPhases(DSS, F, pLoad, geoUUID);
                    WriteTerminals(DSS, F, pLoad, geoUUID, crsUUID);
                end;
            pLoad := DSS.ActiveCircuit.Loads.Next;
        end;

        pCode := clsCode.ElementList.First;
        while pCode <> NIL do
        begin
            with pCode do
            begin
                if pCode.Units = UNITS_NONE then
                begin // we need the real units for CIM
                    pLine := DSS.ActiveCircuit.Lines.First;
                    while pLine <> NIL do
                    begin
                        if pLine.Enabled then
                        begin
                            if pLine.CondCode = pCode.LocalName then
                            begin
                                pCode.Units := pLine.LengthUnits;
//                writeln ('Setting Units on ' + pCode.LocalName + ' to ' + LineUnitsStr(pCode.Units));
                                break;
                            end;
                        end;
                        pLine := DSS.ActiveCircuit.Lines.Next;
                    end;
                end;
                v1 := To_per_Meter(pCode.Units); // TODO: warn if still UNITS_NONE
                if SymComponentsModel and (NumPhases = 3) then
                begin
                    v2 := 1.0e-9 * TwoPi * BaseFrequency; // convert nF to mhos
                    StartInstance(F, 'PerLengthSequenceImpedance', pCode);
                    DoubleNode(F, 'PerLengthSequenceImpedance.r', R1 * v1);
                    DoubleNode(F, 'PerLengthSequenceImpedance.x', X1 * v1);
                    DoubleNode(F, 'PerLengthSequenceImpedance.bch', C1 * v1 * v2);
                    DoubleNode(F, 'PerLengthSequenceImpedance.gch', 0.0);
                    DoubleNode(F, 'PerLengthSequenceImpedance.r0', R0 * v1);
                    DoubleNode(F, 'PerLengthSequenceImpedance.x0', X0 * v1);
                    DoubleNode(F, 'PerLengthSequenceImpedance.b0ch', C0 * v1 * v2);
                    DoubleNode(F, 'PerLengthSequenceImpedance.g0ch', 0.0);
                    EndInstance(F, 'PerLengthSequenceImpedance')
                end
                else
                begin
                    StartInstance(F, 'PerLengthPhaseImpedance', pCode);
                    IntegerNode(F, 'PerLengthPhaseImpedance.conductorCount', FNPhases);
                    EndInstance(F, 'PerLengthPhaseImpedance');
                    for i := 1 to FNPhases do
                    begin
                        for j := 1 to i do
                        begin
                            StartFreeInstance(F, 'PhaseImpedanceData');
                            RefNode(F, 'PhaseImpedanceData.PhaseImpedance', pCode);
                            IntegerNode(F, 'PhaseImpedanceData.row', i);
                            IntegerNode(F, 'PhaseImpedanceData.column', j);
                            DoubleNode(F, 'PhaseImpedanceData.r', Z.GetElement(i, j).re * v1);
                            DoubleNode(F, 'PhaseImpedanceData.x', Z.GetElement(i, j).im * v1);
                            DoubleNode(F, 'PhaseImpedanceData.b', YC.GetElement(i, j).im * v1);
                            EndInstance(F, 'PhaseImpedanceData')
                        end;
                    end;
                end;
            end;
            pCode := clsCode.ElementList.Next;
        end;

        pWire := clsWire.ElementList.First;
        while (pWire <> NIL) do
        begin
            StartInstance(F, 'OverheadWireInfo', pWire);
            WriteWireData(F, pWire);
            BooleanNode(F, 'WireInfo.insulated', FALSE);
            EndInstance(F, 'OverheadWireInfo');
            pWire := clsWire.ElementList.Next;
        end;

        pTape := clsTape.ElementList.First;
        while (pTape <> NIL) do
        begin
            StartInstance(F, 'TapeShieldCableInfo', pTape);
            WriteWireData(F, pTape);
            WriteCableData(F, pTape);
            WriteTapeData(F, pTape);
            EndInstance(F, 'TapeShieldCableInfo');
            pTape := clsTape.ElementList.Next;
        end;

        pConc := clsConc.ElementList.First;
        while (pConc <> NIL) do
        begin
            StartInstance(F, 'ConcentricNeutralCableInfo', pConc);
            WriteWireData(F, pConc);
            WriteCableData(F, pConc);
            WriteConcData(F, pConc);
            EndInstance(F, 'ConcentricNeutralCableInfo');
            pConc := clsConc.ElementList.Next;
        end;

        pGeom := clsGeom.ElementList.First;
        while pGeom <> NIL do
        begin
            with pGeom do
            begin
                StartInstance(F, 'WireSpacingInfo', pGeom);
                ConductorUsageEnum(F, 'distribution');
                IntegerNode(F, 'WireSpacingInfo.phaseWireCount', 1);
                DoubleNode(F, 'WireSpacingInfo.phaseWireSpacing', 0.0);
                if PhaseChoice[1] = Overhead then   // decide this off the first conductor
                    BooleanNode(F, 'WireSpacingInfo.isCable', FALSE)
                else
                    BooleanNode(F, 'WireSpacingInfo.isCable', TRUE);
                EndInstance(F, 'WireSpacingInfo');

                for i := 1 to NWires do
                begin
                    pName1.LocalName := 'WP_' + pGeom.Name + '_' + IntToStr(i);
                    CreateUUID4(tmpUUID);
                    pName1.UUID := tmpUUID;
                    StartInstance(F, 'WirePosition', pName1);
                    RefNode(F, 'WirePosition.WireSpacingInfo', pGeom);
                    IntegerNode(F, 'WirePosition.sequenceNumber', i);
                    v1 := To_Meters(Units[i]);
                    DoubleNode(F, 'WirePosition.xCoord', Xcoord[i] * v1);
                    DoubleNode(F, 'WirePosition.yCoord', Ycoord[i] * v1);
                    EndInstance(F, 'WirePosition')
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
                StartInstance(F, 'WireSpacingInfo', pSpac);
                ConductorUsageEnum(F, 'distribution');
                IntegerNode(F, 'WireSpacingInfo.phaseWireCount', 1);
                DoubleNode(F, 'WireSpacingInfo.phaseWireSpacing', 0.0);
                if pSpac.Ycoord[1] > 0.0 then
                    BooleanNode(F, 'WireSpacingInfo.isCable', FALSE)
                else
                    BooleanNode(F, 'WireSpacingInfo.isCable', TRUE);
                EndInstance(F, 'WireSpacingInfo');

                for i := 1 to NWires do
                begin
                    pName1.LocalName := 'WP_' + pSpac.Name + '_' + IntToStr(i);
                    CreateUUID4(tmpUUID);
                    pName1.UUID := tmpUUID;
                    StartInstance(F, 'WirePosition', pName1);
                    RefNode(F, 'WirePosition.WireSpacingInfo', pSpac);
                    IntegerNode(F, 'WirePosition.sequenceNumber', i);
                    DoubleNode(F, 'WirePosition.xCoord', Xcoord[i] * v1);
                    DoubleNode(F, 'WirePosition.yCoord', Ycoord[i] * v1);
                    EndInstance(F, 'WirePosition')
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
            StartInstance(F, 'OperationalLimitSet', pILimit);
            EndInstance(F, 'OperationalLimitSet');
            pName1.LocalName := pILimit.LocalName + '_Norm';
            CreateUUID4(tmpUUID);
            pName1.UUID := tmpUUID;
            StartInstance(F, 'CurrentLimit', pName1);
            RefNode(F, 'OperationalLimit.OperationalLimitSet', pILimit);
            RefNode(F, 'OperationalLimit.OperationalLimitType', pNormLimit);
            DoubleNode(F, 'CurrentLimit.value', pILimit.NormAmps);
            EndInstance(F, 'CurrentLimit');
            pName2.LocalName := pILimit.LocalName + '_Emerg';
            CreateUUID4(tmpUUID);
            pName2.UUID := tmpUUID;
            StartInstance(F, 'CurrentLimit', pName2);
            RefNode(F, 'OperationalLimit.OperationalLimitSet', pILimit);
            RefNode(F, 'OperationalLimit.OperationalLimitType', pEmergLimit);
            DoubleNode(F, 'CurrentLimit.value', pILimit.EmergAmps);
            EndInstance(F, 'CurrentLimit');
        end;

        pName1.Free;
        pName2.Free;

        FreeUuidList;
        FreeBankList;
        FreeOpLimitList;

        Writeln(F, '</rdf:RDF>');

        DSS.GlobalResult := FileNm;
    finally
        CloseFile(F);
    end;
end;

end.
