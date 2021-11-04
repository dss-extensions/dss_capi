unit ExportCIMXML;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

{
  ---------------------------------------------------------V-
  Copyright (c) 2009-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Write a CIM XML file using RDF Schema for the Common Distribution
  Power System Model, IEC 61968-13.}

interface

uses NamedObject;  // for TUuid

Procedure ExportCDPSM (FileNm:String;
  Substation: String;
  SubGeographicRegion: String;
  GeographicRegion: String;
  FdrUUID: TUuid;
  SubUUID: TUuid;
  SubGeoUUID: TUuid;
  RgnUUID: TUuid;
  Combined:Boolean = True);

procedure StartUuidList (size:Integer);
procedure FreeUuidList;
procedure WriteHashedUUIDs (var F: TextFile);
procedure AddHashedUUID (key: String; UuidVal: String);
procedure DefaultCircuitUUIDs (var fdrID: TUuid; var subID: TUuid; var rgnID: TUuid; var subGeoID: TUuid);

implementation

Uses SysUtils, Utilities, Circuit, DSSClassDefs, DSSGlobals, CktElement,
     PDElement, PCElement, Generator, Load, RegControl,
     Vsource, Line, Transformer, Ucomplex, UcMatrix, LineCode,
     Fuse, Capacitor, CapControl, CapControlvars,  Reactor, Feeder, ConductorData, LineUnits,
     LineGeometry, StrUtils, Math, HashList, WireData, XfmrCode,
     LineSpacing, CableData, CNData, TSData, Storage, PVSystem, Relay, Recloser,
     DSSObject, DSSClass;

Type
  UuidChoice = (Bank, Wdg, XfCore, XfMesh, WdgInf, ScTest, OcTest,
    BaseV, LinePhase, LoadPhase, GenPhase, CapPhase, SolarPhase, BatteryPhase,
    XfLoc, LoadLoc, LineLoc, CapLoc, Topo, ReacLoc, SolarLoc, BatteryLoc,
    OpLimV, OpLimI, LoadResp, CIMVer, PosPt, CoordSys, TopoIsland, Station,
    GeoRgn, SubGeoRgn, ZData, OpLimT, XfInfo, FdrLoc, OpLimAHi, OpLimALo,
    OpLimBHi, OpLimBLo, MachLoc, PVPanels, Battery, SrcLoc, TankInfo, TankAsset,
    TapInfo, TapCtrl, TapAsset, PUZ, WirePos, NormAmps, EmergAmps);

  ProfileChoice = (FunPrf, EpPrf, GeoPrf, TopoPrf, CatPrf, SshPrf);

  TBankObject = class(TNamedObject)
  public
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
    destructor Destroy; override;

    procedure AddTransformer (pXf: TTransfObj);
    procedure BuildVectorGroup;
  end;

  TOpLimitObject = class(TNamedObject)
  public
    NormAmps: double;
    EmergAmps: double;
    constructor Create(norm, emerg: double);
    destructor Destroy; override;
  end;

  TFileDealer = class(TObject)
  private
    // the Combined XML can be broken into six separate profiles
    F_FUN: TextFile;
    F_EP: TextFile;
    F_SSH: TextFile;
    F_CAT: TextFile;
    F_GEO: TextFile;
    F_TOPO: TextFile;
    roots: array[ProfileChoice] of String;
    ids: array[ProfileChoice] of TUuid;
  public
    Separate: Boolean;
    constructor Create(Combined: Boolean; FileName: String);
    destructor Destroy; override;
    procedure WriteCimLn (prf:ProfileChoice; const s:String);
    procedure StartInstance (prf: ProfileChoice; Root: String; Obj: TNamedObject);
    procedure StartFreeInstance (prf: ProfileChoice; Root: String; uuid: TUUID);
    procedure EndInstance (prf: ProfileChoice; Root: String);
  end;

Var
  UuidHash: THashList;       // index is 1-based
  UuidList: array of TUuid;  // index is 0-based
  UuidKeyList: array of String;
  BankHash: THashList;
  BankList: array of TBankObject;
  OpLimitHash: THashList;
  OpLimitList: array of TOpLimitObject;
  FD: TFileDealer;

Const
//  CIM_NS = 'http://iec.ch/TC57/2012/CIM-schema-cim17';
  CIM_NS = 'http://iec.ch/TC57/CIM100';

procedure StartCIMFile (var F: TextFile; FileNm:String; prf: ProfileChoice);forward;

procedure TFileDealer.WriteCimLn(prf: ProfileChoice; const s: string);
begin
  if Separate then begin
    if prf <> FunPrf then begin
      if length(roots[prf]) < 1 then begin
        StartFreeInstance (prf, roots[FunPrf], ids[FunPrf]);
      end;
    end;
    case prf of
      FunPrf: WriteLn (F_FUN, s);
      EpPrf: WriteLn (F_EP, s);
      GeoPrf: WriteLn (F_GEO, s);
      TopoPrf: WriteLn (F_TOPO, s);
      CatPrf: WriteLn (F_CAT, s);
      SshPrf: WriteLn (F_SSH, s);
    end;
  end else begin
    WriteLn (F_FUN, s)
  end;
end;

constructor TFileDealer.Create(Combined: Boolean; FileName: String);
var
  i: ProfileChoice;
begin
  Inherited Create;
  Separate := not Combined;
  if Separate then begin
    for i := Low(ProfileChoice) to High(ProfileChoice) do roots[i] := '';
    StartCIMFile (F_FUN, FileName + '_FUN.XML', FunPrf);
    StartCIMFile (F_GEO, FileName + '_GEO.XML', GeoPrf);
    StartCIMFile (F_TOPO, FileName + '_TOPO.XML', TopoPrf);
    StartCIMFile (F_SSH, FileName + '_SSH.XML', SshPrf);
    StartCIMFile (F_CAT, FileName + '_CAT.XML', CatPrf);
    StartCIMFile (F_EP, FileName + '_EP.XML', EpPrf)
  end else begin
    StartCIMFile (F_FUN, FileName, FunPrf)
  end;
end;

procedure TFileDealer.StartInstance (prf: ProfileChoice; Root: String; Obj: TNamedObject);
begin
  if Separate then begin // must be first to avoid stack overflow in WriteCimLn
    roots[prf] := Root;
    ids[prf] := Obj.UUID;
  end;
  WriteCimLn (prf, Format('<cim:%s rdf:ID="%s">', [Root, Obj.CIM_ID]));
  WriteCimLn (prf, Format ('  <cim:IdentifiedObject.mRID>%s</cim:IdentifiedObject.mRID>', [Obj.CIM_ID]));
  WriteCimLn (prf, Format ('  <cim:IdentifiedObject.name>%s</cim:IdentifiedObject.name>', [Obj.localName]));
end;

procedure TFileDealer.StartFreeInstance (prf: ProfileChoice; Root: String; uuid: TUUID);
begin
  if Separate then begin // must be first to avoid stack overflow in WriteCimLn
    roots[prf] := Root;
    ids[prf] := uuid;
  end;
  WriteCimLn (prf, Format('<cim:%s rdf:ID="%s">', [Root, UUIDToCIMString (uuid)]));
end;

procedure TFileDealer.EndInstance (prf: ProfileChoice; Root: String);
var
  i: ProfileChoice;
begin
  if Separate then begin
    for i := Low(ProfileChoice) to High(ProfileChoice) do begin
      if length(roots[i]) > 0 then begin
        WriteCimLn(i, Format ('</cim:%s>', [Root]));
        roots[i] := ''
      end;
    end
  end else
    WriteCimLn (prf, Format ('</cim:%s>', [Root]));
end;

destructor TFileDealer.Destroy;
begin
  WriteLn (F_FUN, '</rdf:RDF>');
  CloseFile (F_FUN);
  if Separate then begin
    WriteLn (F_GEO, '</rdf:RDF>');
    WriteLn (F_CAT, '</rdf:RDF>');
    WriteLn (F_SSH, '</rdf:RDF>');
    WriteLn (F_TOPO, '</rdf:RDF>');
    WriteLn (F_EP, '</rdf:RDF>');
    CloseFile (F_GEO);
    CloseFile (F_CAT);
    CloseFile (F_SSH);
    CloseFile (F_TOPO);
    CloseFile (F_EP);
  end;
  Inherited Destroy;
end;

procedure ParseSwitchClass (pLine:TLineObj; var swtCls:String; var ratedAmps, breakingAmps: double);
var
  pFuse: TFuseObj;
  pRelay: TRelayObj;
  pRecloser: TRecloserObj;
begin
  swtCls := 'LoadBreakSwitch';
  ratedAmps := pLine.NormAmps;
  breakingAmps := ratedAmps;
  pFuse := ActiveCircuit.Fuses.First;
  while (pFuse <> nil) do begin
    if pFuse.ControlledElement = pLine then begin
      swtCls := 'Fuse';
      ratedAmps := pFuse.RatedCurrent;
      breakingAmps := 0.0;
      exit;
    end;
    pFuse := ActiveCircuit.Fuses.Next;
  end;
  pRelay := ActiveCircuit.Relays.First;
  while (pRelay <> nil) do begin
    if pRelay.ControlledElement = pLine then begin
      swtCls := 'Breaker';
      exit;
    end;
    pRelay := ActiveCircuit.Relays.Next;
  end;
  pRecloser := ActiveCircuit.Reclosers.First;
  while (pRecloser <> nil) do begin
    if pRecloser.ControlledElement = pLine then begin
      swtCls := 'Recloser';
      exit;
    end;
    pRecloser := ActiveCircuit.Reclosers.Next;
  end;
end;

// this returns s1, s2, or a combination of ABCN
function PhaseString (pElem:TDSSCktElement; bus: Integer; bAllowSec: Boolean = True):String; // if order doesn't matter
var
  val, phs: String;
  dot: Integer;
  bSec: boolean;
begin
  phs := pElem.FirstBus;
  for dot:= 2 to bus do phs := pElem.NextBus;
  bSec := false;
  if bAllowSec then begin
    if pElem.NPhases = 2 then
      if ActiveCircuit.Buses^[pElem.Terminals^[bus].BusRef].kVBase < 0.25 then bSec := true;
    if pElem.NPhases = 1 then
      if ActiveCircuit.Buses^[pElem.Terminals^[bus].BusRef].kVBase < 0.13 then bSec := true;
  end;

  dot := pos('.',phs);
  if dot < 1 then begin
    val := 'ABC';
  end else begin
    phs := Copy (phs, dot+1, Length(phs));
    if Pos ('3', phs) > 0 then bSec := false; // i.e. it's a three-phase secondary, not split-phase
    if bSec then begin
      if Pos ('1', phs) > 0 then begin
        val := 's1';
        if Pos ('2', phs) > 0 then val := val + '2';
      end else if Pos ('2', phs) > 0 then val := 's2';
    end else begin
      val := '';
      if Pos ('1', phs) > 0 then val := val + 'A';
      if Pos ('2', phs) > 0 then val := val + 'B';
      if Pos ('3', phs) > 0 then val := val + 'C';
      if Pos ('4', phs) > 0 then val := val + 'N';
    end;
  end;
  Result := val;
end;

// returns s1, s12, s2, or an ordered combination of ABC
function PhaseOrderString (pElem:TDSSCktElement; bus: Integer; bAllowSec: Boolean = True):String; // for transposition
var
  phs: String;
  dot: Integer;
  bSec: boolean;
begin
  phs := pElem.FirstBus;
  for dot:= 2 to bus do phs := pElem.NextBus;

  bSec := false;
  if bAllowSec then begin
    if pElem.NPhases = 2 then
      if ActiveCircuit.Buses^[pElem.Terminals^[bus].BusRef].kVBase < 0.25 then bSec := true;
    if pElem.NPhases = 1 then
      if ActiveCircuit.Buses^[pElem.Terminals^[bus].BusRef].kVBase < 0.13 then bSec := true;
  end;

  dot := pos('.',phs);
  if dot < 1 then begin
    Result := 'ABC';
  end else begin
    phs := Copy (phs, dot+1, Length(phs));
    if Pos ('3', phs) > 0 then bSec := false; // i.e. it's a three-phase secondary, not split-phase
    if bSec then begin
      if Pos ('1', phs) > 0 then begin
        Result := 's1';
        if Pos ('2', phs) > 0 then Result := Result + '2';
      end else if Pos ('2', phs) > 0 then Result := 's2';
    end else
      if Pos ('1.2.3', phs) > 0 then
        Result := 'ABC'
      else if Pos('1.3.2', phs) > 0 then
        Result := 'ACB'
      else if Pos('2.3.1', phs) > 0 then
        Result := 'BCA'
      else if Pos('2.1.3', phs) > 0 then
        Result := 'BAC'
      else if Pos('3.2.1', phs) > 0 then
        Result := 'CBA'
      else if Pos('3.1.2', phs) > 0 then
        Result := 'CAB'
      else if Pos('1.2', phs) > 0 then
        Result := 'AB'
      else if Pos('1.3', phs) > 0 then
        Result := 'AC'
      else if Pos('2.3', phs) > 0 then
        Result := 'BC'
      else if Pos('2.1', phs) > 0 then
        Result := 'BA'
      else if Pos('3.2', phs) > 0 then
        Result := 'CB'
      else if Pos('3.1', phs) > 0 then
        Result := 'CA'
      else if Pos('1', phs) > 0 then
        Result := 'A'
      else if Pos('2', phs) > 0 then
        Result := 'B'
      else
        Result := 'C';
  end;
end;

function DeltaPhaseString (pElem:TDSSCktElement):String;
var
  phs: String;
  dot: Integer;
begin
  phs := pElem.FirstBus;

  dot := pos('.',phs);
  if (dot < 1) or (pElem.NPhases = 3) then begin
    Result := 'ABC'; // if Nphases < 3 this would be a user input error
  end else begin
    phs := Copy (phs, dot+1, Length(phs));
    if pElem.NPhases = 1 then begin
      if Pos ('1.2', phs) > 0 then
        Result := 'A'
      else if Pos ('2.1', phs) > 0 then
        Result := 'A'
      else if Pos ('2.3', phs) > 0 then
        Result := 'B'
      else if Pos ('3.2', phs) > 0 then
        Result := 'B'
      else if Pos ('1.3', phs) > 0 then
        Result := 'C'
      else if Pos ('3.1', phs) > 0 then
        Result := 'C'
    end else begin
      if Pos ('1.2.3', phs) > 0 then
        Result := 'AB'
      else if Pos ('1.3.2', phs) > 0 then
        Result := 'CB'
      else if Pos ('2.1.3', phs) > 0 then
        Result := 'AC'
      else if Pos ('2.3.1', phs) > 0 then
        Result := 'BC'
      else if Pos ('3.1.2', phs) > 0 then
        Result := 'CA'
      else if Pos ('3.2.1', phs) > 0 then
        Result := 'BA'
    end;
  end;
end;

{$R+}

constructor TBankObject.Create(MaxWdg: Integer);
begin
  maxWindings:=MaxWdg;
  nWindings:=0;
  SetLength (connections, MaxWdg);
  SetLength (angles, MaxWdg);
  SetLength (phaseA, MaxWdg);
  SetLength (phaseB, MaxWdg);
  SetLength (phaseC, MaxWdg);
  SetLength (ground, MaxWdg);
  Inherited Create('Bank');
end;

destructor TBankObject.Destroy;
begin
  connections := nil;
  angles := nil;
  phaseA := nil;
  phaseB := nil;
  phaseC := nil;
  ground := nil;
  a_unit := nil;
  Inherited Destroy;
end;

procedure TBankObject.BuildVectorGroup;
var
  i: Integer;
begin
  vectorGroup := '';
  i := 0; // dynamic arrays are zero-based
  while i < nWindings do begin
    if (phaseA[i] > 0) and (phaseB[i] > 0 ) and (phaseC[i] > 0) then begin
      if connections[i] > 0 then
        vectorGroup := vectorGroup + 'd'
      else
        vectorGroup := vectorGroup + 'y';
      if ground[i] > 0 then vectorGroup := vectorGroup + 'n';
      if angles[i] > 0 then vectorGroup := vectorGroup + IntToStr(angles[i])
    end else
      vectorGroup := vectorGroup + 'i';
    Inc (i)
  end;
  if Length(vectorGroup) > 0 then
    vectorGroup := UpperCase(LeftStr(vectorGroup, 1)) + RightStr (vectorGroup, Length(vectorGroup) - 1);
end;

procedure TBankObject.AddTransformer(pXf: TTransfObj);
var
  i: Integer;
  phs: String;
begin
  if pXf.NumberOfWindings > nWindings then nWindings := pXf.NumberOfWindings;

  a_unit := pXf;
  for i:=1 to pXf.NumberOfWindings do begin
    phs := PhaseString (pXf, i);
    if Pos('A', phs) > 0 then phaseA[i-1] := 1;
    if Pos('B', phs) > 0 then phaseB[i-1] := 1;
    if Pos('C', phs) > 0 then phaseC[i-1] := 1;
    connections[i-1] := pXf.WdgConnection[i];
    if connections[i-1] <> connections[0] then angles [i-1] := 1;
    if (pXf.WdgRneutral[i] >= 0.0) or (pXf.WdgXneutral[i] > 0.0) then
      if connections[i-1] < 1 then
        ground[i-1] := 1;
  end;
end;

constructor TOpLimitObject.Create(norm, emerg:double);
begin
  NormAmps := norm;
  EmergAmps := emerg;
  Inherited Create('OpLimI');
end;

destructor TOpLimitObject.Destroy;
begin
  Inherited Destroy;
end;

// the CIM transformer model requires some identified objects that don't have
// a counterpart in the DSS named objects.  These include banks, windings, and
// winding info.  So we create temporary UUIDs on the fly, and use a hash list when we
// need the UUIDs for later reference
procedure StartUuidList (size:Integer);
begin
  if assigned(UuidList) then FreeUuidList;
  UuidHash := THashList.Create(size);
  SetLength (UuidList, size);
  SetLength (UuidKeyList, size);
end;

procedure StartBankList (size: Integer);
begin
  BankHash := THashList.Create(size);
  SetLength (BankList, size);
end;

procedure StartOpLimitList (size: Integer);
begin
  OpLimitHash := THashList.Create(size);
  SetLength (OpLimitList, size);
end;

procedure FreeUuidList;
begin
  UuidHash.Free;
  UuidList := nil;
  UuidKeyList := nil;
end;

procedure FreeBankList;
begin
  BankHash.Free;
  BankList := nil;
end;

procedure FreeOpLimitList;
begin
  OpLimitHash.Free;
  OpLimitList := nil;
end;

procedure AddBank (pBank: TBankObject);
var
  ref, size: Integer;
begin
  ref := BankHash.Add(pBank.localName);
  size := High(BankList) + 1;
  if ref > size then SetLength (BankList, 2 * size);
  BankList[ref-1] := pBank;
end;

function GetBank (sBank: String): TBankObject;
var
  ref : Integer;
begin
  Result := nil;
  ref := BankHash.Find (sBank);
  if ref > 0 then Result:=BankList[ref-1];
end;

procedure AddOpLimit (pLimit: TOpLimitObject);
var
  ref, size: Integer;
begin
  ref := OpLimitHash.Add(pLimit.localName);
  size := High(OpLimitList) + 1;
  if ref > size then SetLength (OpLimitList, 2 * size);
  OpLimitList[ref-1] := pLimit;
end;

function GetOpLimit (sLimit: String): TOpLimitObject;
var
  ref : Integer;
begin
  Result := nil;
  ref := OpLimitHash.Find (sLimit);
  if ref > 0 then Result:=OpLimitList[ref-1];
end;

function GetHashedUuid (key: String): TUuid;
var
  ref: integer;
  size: integer;
begin
  ref:=UuidHash.Find(key);
  if ref = 0 then begin
    ref := UuidHash.Add(key);
    CreateUUID4 (Result);  // this should be the ONLY place to call CreateUUID4
    size := High(UuidList) + 1;
    if ref > size then begin
      SetLength (UuidList, 2 * (size+1));
      SetLength (UuidKeyList, 2 * (size+1));
    end;
    UuidList[ref-1] := Result;
    UuidKeyList[ref-1] := key
  end else begin
    Result := UuidList[ref-1]
  end;
end;

procedure AddHashedUuid (key: String; UuidVal: String);
var
  ref: integer;
  size: integer;
begin
  ref:=UuidHash.Find(key);
  if ref = 0 then begin
    ref := UuidHash.Add(key);
    size := High(UuidList) + 1;
    if ref > size then begin
      SetLength (UuidList, 2 * (size+1));
      SetLength (UuidKeyList, 2 * (size+1));
    end;
    UuidList[ref-1] := StringToUuid (UuidVal);
    UuidKeyList[ref-1] := key
  end else begin
    UuidList[ref-1] := StringToUuid (UuidVal);
  end;
end;

// any temporary object (not managed by DSS) should have '=' prepended to the Name
function GetDevUuid (which: UuidChoice; Name: String; Seq: Integer): TUuid;
var
  key: String;
begin
  case which of
    Bank: key := 'Bank=';
    Wdg: key := 'Wdg=';
    XfCore: key := 'XfCore=';
    XfMesh: key := 'XfMesh=';
    WdgInf: key := 'WdgInf=';
    ScTest: key := 'ScTest=';
    OcTest: key := 'OcTest=';
    BaseV: key := 'BaseV=';
    OpLimV: key := 'OpLimV=';
    OpLimI: key := 'OpLimI=';
    LinePhase: key := 'LinePhase=';
    LoadPhase: key := 'LoadPhase=';
    GenPhase: key := 'GenPhase=';
    SolarPhase: key := 'PVPhase=';
    BatteryPhase: key := 'BattPhase=';
    CapPhase: key := 'CapPhase=';
    XfLoc: key := 'XfLoc=';
    LoadLoc: key := 'LoadLoc=';
    LineLoc: key := 'LineLoc=';
    ReacLoc: key := 'ReacLoc=';
    CapLoc: key := 'CapLoc=';
    Topo: key := 'Topo=';
    SolarLoc: key := 'SolarLoc=';
    BatteryLoc: key := 'BatteryLoc=';
    LoadResp: key := 'LoadResp=';
    CIMVer: key := 'CIMVer=';
    ZData: key := 'ZData=';
    PosPt: key := 'PosPt=';
    CoordSys: key := 'CoordSys=';
    TopoIsland: key := 'TopoIsland=';
    OpLimT: key := 'OpLimT=';
    Station: key := 'Station=';
    GeoRgn: key := 'GeoRgn=';
    SubGeoRgn: key := 'SubGeoRgn=';
    FdrLoc: key := 'FdrLoc=';
    XfInfo: key := 'XfInfo=';
    OpLimAHi: key := 'OpLimAHi=';
    OpLimALo: key := 'OpLimALo=';
    OpLimBHi: key := 'OpLimBHi=';
    OpLimBLo: key := 'OpLimBLo=';
    MachLoc: key := 'MachLoc=';
    SrcLoc: key := 'SrcLoc=';
    PVPanels: key := 'PVPanels=';
    Battery: key := 'Battery=';
    TankInfo: key := 'TankInfo=';
    TankAsset: key := 'TankAsset=';
    TapInfo: key := 'TapInfo=';
    TapCtrl: key := 'TapCtrl=';
    TapAsset: key := 'TapAsset=';
    PUZ: key := 'PUZ=';
    WirePos: key := 'WirePos=';
    NormAmps: key := 'NormAmps=';
    EmergAmps: key := 'EmergAmps=';
  end;
  key:=key + Name + '=' + IntToStr (Seq);
  Result := GetHashedUuid (key);
end;

procedure DefaultCircuitUUIDs (var fdrID: TUuid; var subID: TUuid; var rgnID: TUuid; var subGeoID: TUuid);
begin
  if not assigned (uuidlist) then
    StartUuidList (ActiveCircuit.NumBuses + 2 * ActiveCircuit.NumDevices);
  fdrID := ActiveCircuit.UUID;
  subID := GetDevUuid (Station, 'Station', 1);
  rgnID := GetDevUuid (GeoRgn, 'GeoRgn', 1);
  subGeoID := GetDevUuid (SubGeoRgn, 'SubGeoRgn', 1);
end;

procedure WriteHashedUUIDs (var F: TextFile);
var
  i: Integer;
begin
  for i := 0 to High(UuidList) do begin
    if Length(UuidKeyList[i]) < 1 then break;
    WriteLn (F, Format ('%s %s', [UuidKeyList[i], UUIDToString (UuidList[i])]));
  end;
end;

// terminals are uniquely identified by class (DSSObjType), plus name and sequence
function GetTermUuid (pElem: TDSSCktElement; Seq: Integer): TUuid;
var
  key: String;
begin
  key:=IntToStr (pElem.DSSObjType) + '=' + pElem.Name + '=' + IntToStr (Seq);
  Result := GetHashedUuid (key);
end;

{$R-}

function GetBaseVName (val: double): String;
begin
//  Result := Format('BaseV_%.3f', [val]);
  Result := 'BaseV_' + FloatToStrF (val, ffFixed, 6, 4);
end;

function GetBaseVUuid (val: double): TUuid;
begin
  Result := GetDevUuid (BaseV, GetBaseVName (val), 1);
end;

function GetOpLimVName (val: double): String;
begin
  Result := 'OpLimV_' + FloatToStrF (val, ffFixed, 6, 4);
end;

function GetOpLimVUuid (val: double): TUuid;
begin
  Result := GetDevUuid (OpLimV, GetOpLimVName (val), 1);
end;

function GetOpLimIName (norm, emerg: double): String;
begin
  Result := 'OpLimI_' + FloatToStrF (norm, ffFixed, 6, 1) + '_' + FloatToStrF (emerg, ffFixed, 6, 1);
end;

function GetOpLimIUuid (norm, emerg: double): TUuid;
begin
  Result := GetDevUuid (OpLimI, GetOpLimIName (norm, emerg), 1);
end;

procedure DoubleNode (prf: ProfileChoice; Node: String; val: Double);
begin
  FD.WriteCimLn (prf, Format ('  <cim:%s>%.8g</cim:%s>', [Node, val, Node]));
end;

procedure IntegerNode (prf: ProfileChoice; Node: String; val: Integer);
begin
  FD.WriteCimLn (prf, Format ('  <cim:%s>%d</cim:%s>', [Node, val, Node]));
end;

procedure BooleanNode (prf: ProfileChoice; Node: String; val: Boolean);
var
  i: String;
begin
  if val then i := 'true' else i := 'false';
  FD.WriteCimLn (prf, Format ('  <cim:%s>%s</cim:%s>', [Node, i, Node]));
end;

procedure RefNode (prf: ProfileChoice; Node: String; Obj: TNamedObject);
begin
  FD.WriteCimLn (prf, Format ('  <cim:%s rdf:resource="#%s"/>', [Node, Obj.CIM_ID]));
end;

procedure UuidNode (prf: ProfileChoice; Node: String; ID: TUuid);
begin
  FD.WriteCimLn (prf, Format ('  <cim:%s rdf:resource="#%s"/>', [Node, UUIDToCIMString (ID)]));
end;

procedure LineCodeRefNode (prf: ProfileChoice; List: TLineCode; Name: String);
var
  Obj : TLineCodeObj;
begin
  if List.SetActive (Name) then begin
    Obj := List.GetActiveObj;
    FD.WriteCimLn (prf, Format ('  <cim:ACLineSegment.PerLengthImpedance rdf:resource="#%s"/>', [Obj.CIM_ID]));
  end;
end;

procedure LineSpacingRefNode (prf: ProfileChoice; List: TDSSClass; Name: String);
var
  Obj : TDSSObject; // should be a TLineGeometryObj or TLineSpacingObj
begin
  if List.SetActive (Name) then begin
    Obj := List.GetActiveObj;
    FD.WriteCimLn (prf, Format ('  <cim:ACLineSegment.WireSpacingInfo rdf:resource="#%s"/>', [Obj.CIM_ID]));
  end;
end;

procedure PhaseWireRefNode (prf: ProfileChoice; Obj: TConductorDataObj);
begin
  FD.WriteCimLn (prf, Format ('  <cim:ACLineSegmentPhase.WireInfo rdf:resource="#%s"/>', [Obj.CIM_ID]));
end;

procedure CircuitNode (prf: ProfileChoice; Obj: TNamedObject);
begin
  FD.WriteCimLn (prf, Format('  <cim:Equipment.EquipmentContainer rdf:resource="#%s"/>', [Obj.CIM_ID]));
end;

function FirstPhaseString (pElem:TDSSCktElement; bus: Integer): String;
var
  val: String;
begin
  val := PhaseString (pElem, bus);
  if val <> '' then
    Result := LeftStr (val, 1)
  else
    Result := 'A';
end;

procedure GeneratorControlEnum (prf: ProfileChoice; val: String);
begin
  FD.WriteCimLn (prf, Format ('  <cim:GeneratingUnit.genControlSource rdf:resource="%s#GeneratorControlSource.%s"/>',
    [CIM_NS, val]));
end;

procedure BatteryStateEnum (prf: ProfileChoice; val: Integer);
var
  str: String;
begin
  str := 'Waiting';
  if val = STORE_CHARGING then
    str := 'Charging'
  else if val = STORE_DISCHARGING then
    str := 'Discharging';
  FD.WriteCimLn (prf, Format ('  <cim:BatteryUnit.batteryState rdf:resource="%s#BatteryState.%s"/>',
    [CIM_NS, str]));
end;

procedure SynchMachTypeEnum (prf: ProfileChoice; val: String);
begin
  FD.WriteCimLn (prf, Format ('  <cim:SynchronousMachine.type rdf:resource="%s#SynchronousMachineType.%s"/>',
    [CIM_NS, val]));
end;

procedure SynchMachModeEnum (prf: ProfileChoice; val: String);
begin
  FD.WriteCimLn (prf, Format ('  <cim:SynchronousMachine.operatingMode rdf:resource="%s#SynchronousMachineOperatingMode.%s"/>',
    [CIM_NS, val]));
end;

procedure RegulatingControlEnum (prf: ProfileChoice; val: String);
begin
  FD.WriteCimLn (prf, Format ('  <cim:RegulatingControl.mode rdf:resource="%s#RegulatingControlModeKind.%s"/>',
    [CIM_NS, val]));
end;

procedure WindingConnectionEnum (prf: ProfileChoice; val: String);
begin
  FD.WriteCimLn (prf, Format ('  <cim:TransformerEndInfo.connectionKind rdf:resource="%s#WindingConnection.%s"/>',
    [CIM_NS, val]));
end;

procedure ConductorInsulationEnum (prf: ProfileChoice; val: String);
begin
  FD.WriteCimLn (prf, Format ('  <cim:WireInfo.insulationMaterial rdf:resource="%s#WireInsulationKind.%s"/>',
    [CIM_NS, val]));
end;

procedure ConductorUsageEnum (prf: ProfileChoice; val: String);
begin
  FD.WriteCimLn (prf, Format ('  <cim:WireSpacingInfo.usage rdf:resource="%s#WireUsageKind.%s"/>',
    [CIM_NS, val]));
end;

procedure CableShieldMaterialEnum (prf: ProfileChoice; val: String);
begin
//  FD.WriteCimLn (prf, Format ('  <cim:CableInfo.shieldMaterial rdf:resource="%s#CableShieldMaterialKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure ConductorMaterialEnum (prf: ProfileChoice; val: String);
begin
//  FD.WriteCimLn (prf, Format ('  <cim:WireInfo.material rdf:resource="%s#WireMaterialKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure CableOuterJacketEnum (prf: ProfileChoice; val: String);
begin
//  FD.WriteCimLn (prf, Format ('  <cim:CableInfo.outerJacketKind rdf:resource="%s#CableOuterJacketKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure CableConstructionEnum (prf: ProfileChoice; val: String);
begin
//  FD.WriteCimLn (prf, Format ('  <cim:CableInfo.constructionKind rdf:resource="%s#CableConstructionKind.%s"/>',
//    [CIM_NS, val]));
end;

procedure TransformerControlEnum (prf: ProfileChoice; val: String);
begin
  FD.WriteCimLn (prf, Format ('  <cim:RatioTapChanger.tculControlMode rdf:resource="%s#TransformerControlMode.%s"/>',
    [CIM_NS, val]));
end;

procedure MonitoredPhaseNode (prf: ProfileChoice; val: String);
begin
  FD.WriteCimLn (prf, Format ('  <cim:RegulatingControl.monitoredPhase rdf:resource="%s#PhaseCode.%s"/>',
    [CIM_NS, val]));
end;

procedure OpLimitDirectionEnum (prf: ProfileChoice; val: String);
begin
  FD.WriteCimLn (prf, Format ('  <cim:OperationalLimitType.direction rdf:resource="%s#OperationalLimitDirectionKind.%s"/>',
    [CIM_NS, val]));
end;

procedure StringNode (prf: ProfileChoice; Node: String; val: String);
begin
  FD.WriteCimLn (prf, Format ('  <cim:%s>%s</cim:%s>', [Node, val, Node]));
end;

procedure StartInstance (prf: ProfileChoice; Root: String; Obj: TNamedObject);
begin
  FD.StartInstance (prf, Root, Obj);
end;

procedure StartFreeInstance (prf: ProfileChoice; Root: String; uuid: TUUID);
begin
  FD.StartFreeInstance (prf, Root, uuid);
end;

procedure EndInstance (prf: ProfileChoice; Root: String);
begin
  FD.EndInstance (prf, Root);
end;

procedure XfmrTankPhasesAndGround (fprf: ProfileChoice; eprf: ProfileChoice; pXf:TTransfObj; bus: Integer);
var 
  ordered_phs, phs: String;
  reversed: Boolean;
  j1, j2: Integer;
//  j, jmax: Integer;
begin
//  writeln(Format ('Xfmr Tank: %s end: %d Nconds: %d Nterms: %d Nphases: %d', [pXf.LocalName, bus, pXf.Nconds, pXf.Nterms, pXf.Nphases]));
  reversed := False;
  ordered_phs := PhaseOrderString(pXf, bus);
  if (ordered_phs = 'BCA') or (ordered_phs = 'CAB') then begin  // 'ABC' already fine
    phs := 'ABC'
  end else if (ordered_phs = 'ACB') or (ordered_phs = 'BAC') or (ordered_phs = 'CBA') then begin
    phs := 'ABC';
    reversed := True
  end else if (ordered_phs = 'BA') then begin
    phs := 'AB';
    reversed := True
  end else if (ordered_phs = 'CA') then begin
    phs := 'AC';
    reversed := True
  end else if (ordered_phs = 'CB') then begin
    phs := 'BC';
    reversed := True
  end else if (ordered_phs = 's2') then begin
    phs := 's2';
    reversed := True
  end else begin
    phs := ordered_phs;
  end;
  FD.WriteCimLn (fprf, Format ('  <cim:TransformerTankEnd.phases rdf:resource="%s#PhaseCode.%s"/>', [CIM_NS, phs]));
  // interpret the grounding and reversal connections
//  jmax := pXf.NConds * pXf.NTerms;
//  for j := 1 to jmax do begin
//    writeln(Format ('  j: %d, noderef^[j]: %d', [j, pXf.NodeRef^[j]]));
//  end;
  j1 := (bus-1) * pXf.NConds + 1;
  j2 := j1 + pXf.Nphases;
//  writeln(Format('  Testing %d and %d', [j1, j2]));
  if (pXf.Winding^[bus].Connection = 1) then begin // delta
    BooleanNode (fprf, 'TransformerEnd.grounded', false);
  end else if (pXf.NodeRef^[j2] = 0) then begin // last conductor is grounded solidly
    BooleanNode (FunPrf, 'TransformerEnd.grounded', true);
    DoubleNode (EpPrf, 'TransformerEnd.rground', 0.0);
    DoubleNode (EpPrf, 'TransformerEnd.xground', 0.0);
  end else if (pXf.NodeRef^[j1] = 0) then begin // first conductor is grounded solidly, but should be reversed
    BooleanNode (FunPrf, 'TransformerEnd.grounded', true);
    DoubleNode (EpPrf, 'TransformerEnd.rground', 0.0);
    DoubleNode (EpPrf, 'TransformerEnd.xground', 0.0);
    reversed := True;
  end else if (pXf.Winding^[bus].Rneut < 0.0) then begin // probably wye ungrounded
    BooleanNode (FunPrf, 'TransformerEnd.grounded', false);
  end else begin // not delta, not wye solidly grounded or ungrounded
    BooleanNode (FunPrf, 'TransformerEnd.grounded', true);
    DoubleNode (EpPrf, 'TransformerEnd.rground', pXf.Winding^[bus].Rneut);
    DoubleNode (EpPrf, 'TransformerEnd.xground', pXf.Winding^[bus].Xneut);
  end;
  BooleanNode (fprf, 'TransformerTankEnd.reversed', reversed);
end;

procedure PhaseNode (prf: ProfileChoice; Root: String; val: String);
begin
  FD.WriteCimLn (prf, Format ('  <cim:%s.phase rdf:resource="%s#PhaseCode.%s"/>',
    [Root, CIM_NS, val]));
end;

procedure PhaseKindNode (prf: ProfileChoice; Root: String; val: String);
begin
  FD.WriteCimLn (prf, Format ('  <cim:%s.phase rdf:resource="%s#SinglePhaseKind.%s"/>',
    [Root, CIM_NS, val]));
end;

procedure PhaseSideNode (prf: ProfileChoice; Root: String; Side: integer; val: String);
begin
  FD.WriteCimLn (prf, Format ('  <cim:%s.phaseSide%d rdf:resource="%s#SinglePhaseKind.%s"/>',
    [Root, Side, CIM_NS, val]));
end;

procedure ShuntConnectionKindNode (prf: ProfileChoice; Root: String; val: String); // D, Y, Yn, I
begin
  FD.WriteCimLn (prf, Format ('  <cim:%s.phaseConnection rdf:resource="%s#PhaseShuntConnectionKind.%s"/>',
    [Root, CIM_NS, val]));
end;

procedure WindingConnectionKindNode (prf: ProfileChoice; val: String); // D, Y, Z, Yn, Zn, A, I
begin
  FD.WriteCimLn (prf, Format ('  <cim:PowerTransformerEnd.connectionKind rdf:resource="%s#WindingConnection.%s"/>',
    [CIM_NS, val]));
end;

// we specify phases except for balanced three-phase
procedure AttachLinePhases (pLine:TLineObj);
var
  s, phs: String;
  i: Integer;
  pPhase: TNamedObject;
begin
  pPhase := TNamedObject.Create('dummy');
  s := PhaseOrderString(pLine, 1);
  if pLine.NumConductorsAvailable > length(s) then s := s + 'N'; // so we can specify the neutral conductor
  for i := 1 to length(s) do begin
    phs := s[i];
    if phs = 's' then continue;
    if phs = '1' then phs := 's1';
    if phs = '2' then phs := 's2';
    pPhase.LocalName := pLine.Name + '_' + phs;
    pPhase.UUID := GetDevUuid (LinePhase, pPhase.LocalName, 1);
    StartInstance (FunPrf, 'ACLineSegmentPhase', pPhase);
    PhaseKindNode (FunPrf, 'ACLineSegmentPhase', phs);
    IntegerNode (FunPrf, 'ACLineSegmentPhase.sequenceNumber', i);
    if i <= pLine.NumConductorsAvailable then
      PhaseWireRefNode (CatPrf, pLine.ConductorData[i]);
    RefNode (FunPrf, 'ACLineSegmentPhase.ACLineSegment', pLine);
    UuidNode (GeoPrf, 'PowerSystemResource.Location',
      GetDevUuid (LineLoc, pLine.Name, 1));
    EndInstance (FunPrf, 'ACLineSegmentPhase');
  end;
end;

procedure AttachSwitchPhases (pLine:TLineObj);
var
  s1, s2, phs1, phs2: String;
  i: Integer;
  pPhase: TNamedObject;
begin
  // also write the switch phases if needed to support transpositions
  s1 := PhaseOrderString(pLine, 1);
  s2 := PhaseOrderString(pLine, 2);
  if (pLine.NPhases = 3) and (length(s1) = 3) and (s1 = s2) then exit;
  pPhase := TNamedObject.Create('dummy');
  for i := 1 to length(s1) do begin
    phs1 := s1[i];
    phs2 := s2[i];
    if phs1 = 's' then continue;
    if phs2 = 's' then continue;
    if phs1 = '1' then phs1 := 's1';
    if phs1 = '2' then phs1 := 's2';
    if phs2 = '1' then phs2 := 's1';
    if phs2 = '2' then phs2 := 's2';
    pPhase.LocalName := pLine.Name + '_' + phs1;
    pPhase.UUID := GetDevUuid (LinePhase, pPhase.LocalName, 1);
    StartInstance (FunPrf, 'SwitchPhase', pPhase);
    BooleanNode (SshPrf, 'SwitchPhase.closed', pLine.Closed[0]);
    BooleanNode (FunPrf, 'SwitchPhase.normalOpen', not pLine.Closed[0]);
    PhaseSideNode (FunPrf, 'SwitchPhase', 1, phs1);
    PhaseSideNode (FunPrf, 'SwitchPhase', 2, phs2);
    RefNode (FunPrf, 'SwitchPhase.Switch', pLine);
    UuidNode (GeoPrf, 'PowerSystemResource.Location', GetDevUuid (LineLoc, pLine.Name, 1));
    EndInstance (FunPrf, 'SwitchPhase');
  end;
end;

procedure AttachCapPhases (pCap:TCapacitorObj; geoUUID: TUuid; sections: double);
var
  s, phs: String;
  i: Integer;
  pPhase: TNamedObject;
  bph: double;
begin
  if pCap.NPhases = 3 then exit;
  pPhase := TNamedObject.Create('dummy');
  s := PhaseString(pCap, 1);
  with pCap do begin
    bph := 0.001 * Totalkvar / NomKV / NomKV / NumSteps / NPhases;
    if (Connection = 1) then s := DeltaPhaseString(pCap);
  end;
  for i := 1 to length(s) do begin
    phs := s[i];
    pPhase.LocalName := pCap.Name + '_' + phs;
    pPhase.UUID := GetDevUuid (CapPhase, pPhase.LocalName, 1);
    StartInstance (FunPrf, 'LinearShuntCompensatorPhase', pPhase);
    PhaseKindNode (FunPrf, 'ShuntCompensatorPhase', phs);
    DoubleNode (EpPrf, 'LinearShuntCompensatorPhase.bPerSection', bph);
    DoubleNode (EpPrf, 'LinearShuntCompensatorPhase.gPerSection', 0.0);
    IntegerNode (EpPrf, 'ShuntCompensatorPhase.normalSections', pCap.NumSteps);
    IntegerNode (EpPrf, 'ShuntCompensatorPhase.maximumSections', pCap.NumSteps);
    DoubleNode (SshPrf, 'ShuntCompensatorPhase.sections', sections);
    RefNode (FunPrf, 'ShuntCompensatorPhase.ShuntCompensator', pCap);
    UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
    EndInstance (FunPrf, 'LinearShuntCompensatorPhase');
  end;
end;

procedure AttachSecondaryPhases (pLoad:TLoadObj; geoUUID: TUuid; pPhase: TNamedObject; p, q: double; phs:String);
begin
  pPhase.LocalName := pLoad.Name + '_' + phs;
  pPhase.UUID := GetDevUuid (LoadPhase, pPhase.LocalName, 1);
  StartInstance (FunPrf, 'EnergyConsumerPhase', pPhase);
  PhaseKindNode (FunPrf, 'EnergyConsumerPhase', phs);
  DoubleNode (SshPrf, 'EnergyConsumerPhase.p', p);
  DoubleNode (SshPrf, 'EnergyConsumerPhase.q', q);
  RefNode (FunPrf, 'EnergyConsumerPhase.EnergyConsumer', pLoad);
  UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
  EndInstance (FunPrf, 'EnergyConsumerPhase');
end;

procedure AttachLoadPhases (pLoad:TLoadObj; geoUUID: TUuid);
var
  s, phs: String;
  i: Integer;
  pPhase: TNamedObject;
  p, q: double;
  bAllowSec: Boolean;
begin
  if pLoad.NPhases = 3 then exit;
  // TODO - use a more robust filter than pLoad.LoadClass, which is > 1 only for PNNL taxonomy imports
  if pLoad.LoadClass <= 1 then
    bAllowSec := True
  else
    bAllowSec := False;
  p := 1000.0 * pLoad.kWBase / pLoad.NPhases;
  q := 1000.0 * pLoad.kvarBase / pLoad.NPhases;
  if pLoad.Connection = 1 then
    s := DeltaPhaseString(pLoad)
  else
    s := PhaseString(pLoad, 1, bAllowSec);

  pPhase := TNamedObject.Create('dummy');
  // first, filter out what appear to be split secondary loads
  // these can be 2-phase loads (balanced) nominally 0.208 kV, or
  //  1-phase loads (possibly unbalanced) nominally 0.12 kV
  //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
  if (pLoad.kVLoadBase < 0.25) and bAllowSec then begin
    if pLoad.NPhases=2 then begin
      AttachSecondaryPhases (pLoad, geoUUID, pPhase, p, q, 's1');
      AttachSecondaryPhases (pLoad, geoUUID, pPhase, p, q, 's2');
      exit;
    end else begin
      AttachSecondaryPhases (pLoad, geoUUID, pPhase, p, q, s);
      exit;
    end;
  end;

  for i := 1 to length(s) do begin
    phs := s[i];
    pPhase.LocalName := pLoad.Name + '_' + phs;
    pPhase.UUID := GetDevUuid (LoadPhase, pPhase.LocalName, 1);
    StartInstance (FunPrf, 'EnergyConsumerPhase', pPhase);
    PhaseKindNode (FunPrf, 'EnergyConsumerPhase', phs);
    DoubleNode (SshPrf, 'EnergyConsumerPhase.p', p);
    DoubleNode (SshPrf, 'EnergyConsumerPhase.q', q);
    RefNode (FunPrf, 'EnergyConsumerPhase.EnergyConsumer', pLoad);
    UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
    EndInstance (FunPrf, 'EnergyConsumerPhase');
  end;
end;

procedure AttachSecondaryGenPhases (pGen:TGeneratorObj; geoUUID: TUuid; pPhase: TNamedObject; p, q: double; phs:String);
begin
  pPhase.LocalName := pGen.Name + '_' + phs;
  pPhase.UUID := GetDevUuid (GenPhase, pPhase.LocalName, 1);
  StartInstance (FunPrf, 'SynchronousMachinePhase', pPhase);
  PhaseKindNode (FunPrf, 'SynchronousMachinePhase', phs);
  DoubleNode (SshPrf, 'SynchronousMachinePhase.p', p);
  DoubleNode (SshPrf, 'SynchronousMachinePhase.q', q);
  RefNode (FunPrf, 'SynchronousMachinePhase.SynchronousMachine', pGen);
  UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
  EndInstance (FunPrf, 'SynchronousMachinePhase');
end;

procedure AttachGeneratorPhases (pGen:TGeneratorObj; geoUUID: TUuid);
var
  s, phs: String;
  i: Integer;
  pPhase: TNamedObject;
  p, q: double;
begin
  if pGen.NPhases = 3 then exit;
  p := 1000.0 * pGen.Presentkw / pGen.NPhases;
  q := 1000.0 * pGen.Presentkvar / pGen.NPhases;
  if pGen.Connection = 1 then
    s := DeltaPhaseString(pGen)
  else
    s := PhaseString(pGen, 1);

  pPhase := TNamedObject.Create('dummy');
  //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
  if pGen.Presentkv < 0.25 then begin
    if pGen.NPhases=2 then begin
      AttachSecondaryGenPhases (pGen, geoUUID, pPhase, p, q, 's1');
      AttachSecondaryGenPhases (pGen, geoUUID, pPhase, p, q, 's2');
      exit;
    end else begin
      AttachSecondaryGenPhases (pGen, geoUUID, pPhase, p, q, s);
      exit;
    end;
  end;

  for i := 1 to length(s) do begin
    phs := s[i];
    pPhase.LocalName := pGen.Name + '_' + phs;
    pPhase.UUID := GetDevUuid (GenPhase, pPhase.LocalName, 1);
    StartInstance (FunPrf, 'SynchronousMachinePhase', pPhase);
    PhaseKindNode (FunPrf, 'SynchronousMachinePhase', phs);
    DoubleNode (SshPrf, 'SynchronousMachinePhase.p', p);
    DoubleNode (SshPrf, 'SynchronousMachinePhase.q', q);
    RefNode (FunPrf, 'SynchronousMachinePhase.SynchronousMachine', pGen);
    UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
    EndInstance (FunPrf, 'SynchronousMachinePhase');
  end;
end;

procedure AttachSecondarySolarPhases (pPV:TPVSystemObj; geoUUID: TUuid; pPhase: TNamedObject; p, q: double; phs:String);
begin
  pPhase.LocalName := pPV.Name + '_' + phs;
  pPhase.UUID := GetDevUuid (SolarPhase, pPhase.LocalName, 1);
  StartInstance (FunPrf, 'PowerElectronicsConnectionPhase', pPhase);
  PhaseKindNode (FunPrf, 'PowerElectronicsConnectionPhase', phs);
  DoubleNode (SshPrf, 'PowerElectronicsConnectionPhase.p', p);
  DoubleNode (SshPrf, 'PowerElectronicsConnectionPhase.q', q);
  RefNode (FunPrf, 'PowerElectronicsConnectionPhase.PowerElectronicsConnection', pPV);
  UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
  EndInstance (FunPrf, 'PowerElectronicsConnectionPhase');
end;

procedure AttachSolarPhases (pPV:TPVSystemObj; geoUUID: TUuid);
var
  s, phs: String;
  i: Integer;
  pPhase: TNamedObject;
  p, q: double;
begin
  if pPV.NPhases = 3 then exit;
  p := 1000.0 * pPV.Presentkw / pPV.NPhases;
  q := 1000.0 * pPV.Presentkvar / pPV.NPhases;
  if pPV.Connection = 1 then
    s := DeltaPhaseString(pPV)
  else
    s := PhaseString(pPV, 1);

  pPhase := TNamedObject.Create('dummy');
  //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
  if pPV.Presentkv < 0.25 then begin
    if pPV.NPhases=2 then begin
      AttachSecondarySolarPhases (pPV, geoUUID, pPhase, p, q, 's1');
      AttachSecondarySolarPhases (pPV, geoUUID, pPhase, p, q, 's2');
      exit;
    end else begin
      AttachSecondarySolarPhases (pPV, geoUUID, pPhase, p, q, s);
      exit;
    end;
  end;

  for i := 1 to length(s) do begin
    phs := s[i];
    pPhase.LocalName := pPV.Name + '_' + phs;
    pPhase.UUID := GetDevUuid (SolarPhase, pPhase.LocalName, 1);
    StartInstance (FunPrf, 'PowerElectronicsConnectionPhase', pPhase);
    PhaseKindNode (FunPrf, 'PowerElectronicsConnectionPhase', phs);
    DoubleNode (SshPrf, 'PowerElectronicsConnectionPhase.p', p);
    DoubleNode (SshPrf, 'PowerElectronicsConnectionPhase.q', q);
    RefNode (FunPrf, 'PowerElectronicsConnectionPhase.PowerElectronicsConnection', pPV);
    UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
    EndInstance (FunPrf, 'PowerElectronicsConnectionPhase');
  end;
end;

procedure AttachSecondaryStoragePhases (pBat:TStorageObj; geoUUID: TUuid; pPhase: TNamedObject; p, q: double; phs:String);
begin
  pPhase.LocalName := pBat.Name + '_' + phs;
  pPhase.UUID := GetDevUuid (BatteryPhase, pPhase.LocalName, 1);
  StartInstance (FunPrf, 'PowerElectronicsConnectionPhase', pPhase);
  PhaseKindNode (FunPrf, 'PowerElectronicsConnectionPhase', phs);
  DoubleNode (SshPrf, 'PowerElectronicsConnectionPhase.p', p);
  DoubleNode (SshPrf, 'PowerElectronicsConnectionPhase.q', q);
  RefNode (FunPrf, 'PowerElectronicsConnectionPhase.PowerElectronicsConnection', pBat);
  UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
  EndInstance (FunPrf, 'PowerElectronicsConnectionPhase');
end;

procedure AttachStoragePhases (pBat:TStorageObj; geoUUID: TUuid);
var
  s, phs: String;
  i: Integer;
  pPhase: TNamedObject;
  p, q: double;
begin
  if pBat.NPhases = 3 then exit;
  p := 1000.0 * pBat.Presentkw / pBat.NPhases;
  q := 1000.0 * pBat.Presentkvar / pBat.NPhases;
  if pBat.Connection = 1 then
    s := DeltaPhaseString(pBat)
  else
    s := PhaseString(pBat, 1);

  pPhase := TNamedObject.Create('dummy');
  //  TODO - handle s1 to s2 240-volt loads; these would be s12, which is not a valid SinglePhaseKind
  if pBat.Presentkv < 0.25 then begin
    if pBat.NPhases=2 then begin
      AttachSecondaryStoragePhases (pBat, geoUUID, pPhase, p, q, 's1');
      AttachSecondaryStoragePhases (pBat, geoUUID, pPhase, p, q, 's2');
      exit;
    end else begin
      AttachSecondaryStoragePhases (pBat, geoUUID, pPhase, p, q, s);
      exit;
    end;
  end;

  for i := 1 to length(s) do begin
    phs := s[i];
    pPhase.LocalName := pBat.Name + '_' + phs;
    pPhase.UUID := GetDevUuid (BatteryPhase, pPhase.LocalName, 1);
    StartInstance (FunPrf, 'PowerElectronicsConnectionPhase', pPhase);
    PhaseKindNode (FunPrf, 'PowerElectronicsConnectionPhase', phs);
    DoubleNode (SshPrf, 'PowerElectronicsConnectionPhase.p', p);
    DoubleNode (SshPrf, 'PowerElectronicsConnectionPhase.q', q);
    RefNode (FunPrf, 'PowerElectronicsConnectionPhase.PowerElectronicsConnection', pBat);
    UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
    EndInstance (FunPrf, 'PowerElectronicsConnectionPhase');
  end;
end;

procedure WriteLoadModel (Name: String; ID: TUuid;
  zP: Double; iP: Double; pP: Double; zQ: Double; iQ: Double; pQ: Double;
  eP: Double; eQ: Double);
begin
  FD.WriteCimln(FunPrf, Format('<cim:LoadResponseCharacteristic rdf:ID="%s">', [UUIDToCIMString(ID)]));
  StringNode (FunPrf, 'IdentifiedObject.mRID', UUIDToCIMString(ID));
  StringNode (FunPrf, 'IdentifiedObject.name', Name);
  if (eP > 0.0) or (eQ > 0.0) then
    BooleanNode (FunPrf, 'LoadResponseCharacteristic.exponentModel', true)
  else
    BooleanNode (FunPrf, 'LoadResponseCharacteristic.exponentModel', false);

  DoubleNode (FunPrf, 'LoadResponseCharacteristic.pConstantImpedance', zP);
  DoubleNode (FunPrf, 'LoadResponseCharacteristic.pConstantCurrent', iP);
  DoubleNode (FunPrf, 'LoadResponseCharacteristic.pConstantPower', pP);

  DoubleNode (FunPrf, 'LoadResponseCharacteristic.qConstantImpedance', zQ);
  DoubleNode (FunPrf, 'LoadResponseCharacteristic.qConstantCurrent', iQ);
  DoubleNode (FunPrf, 'LoadResponseCharacteristic.qConstantPower', pQ);

  DoubleNode (FunPrf, 'LoadResponseCharacteristic.pVoltageExponent', eP);
  DoubleNode (FunPrf, 'LoadResponseCharacteristic.qVoltageExponent', eQ);
  DoubleNode (FunPrf, 'LoadResponseCharacteristic.pFrequencyExponent', 0.0);
  DoubleNode (FunPrf, 'LoadResponseCharacteristic.qFrequencyExponent', 0.0);
  FD.WriteCimLn (FunPrf, '</cim:LoadResponseCharacteristic>');
end;

function IsGroundBus (const S: String) : Boolean;
var
  i : Integer;
begin
  Result := True;
  i := pos ('.1', S);
  if i > 0 then Result := False;
  i := pos ('.2', S);
  if i > 0 then Result := False;
  i := pos ('.3', S);
  if i > 0 then Result := False;
  i := pos ('.', S);
  if i = 0 then Result := False;
end;

procedure WritePositions(pElem:TDSSCktElement; geoUUID: TUuid; crsUUID: TUuid);
var
  Nterm, j, ref : Integer;
  BusName : String;
begin
  Nterm := pElem.Nterms;
  BusName := pElem.FirstBus;
  StartFreeInstance (GeoPrf, 'Location', geoUUID);
  StringNode(GeoPrf, 'IdentifiedObject.mRID', UUIDToCIMString(geoUUID));
  StringNode(GeoPrf, 'IdentifiedObject.name', pElem.LocalName + '_Loc');
  UuidNode (GeoPrf, 'Location.CoordinateSystem', crsUUID);
  EndInstance (GeoPrf, 'Location');

  for j := 1 to NTerm do begin
    if IsGroundBus (BusName) = False then begin
      ref := pElem.Terminals^[j].BusRef;
      StartFreeInstance (GeoPrf, 'PositionPoint', GetDevUuid (PosPt, pElem.ParentClass.Name + '.' + pElem.LocalName, j));
      UuidNode (GeoPrf, 'PositionPoint.Location', geoUUID);
      IntegerNode (GeoPrf, 'PositionPoint.sequenceNumber', j);
      StringNode (GeoPrf, 'PositionPoint.xPosition', FloatToStr (ActiveCircuit.Buses^[ref].x));
      StringNode (GeoPrf, 'PositionPoint.yPosition', FloatToStr (ActiveCircuit.Buses^[ref].y));
      EndInstance (GeoPrf, 'PositionPoint');
    end;
    BusName := pElem.Nextbus;
  end;
end;

procedure WriteReferenceTerminals(pElem:TDSSCktElement; RefUuid: TUuid; norm: double=0.0; emerg: double=0.0);
var
  Nterm, j, ref : Integer;
  BusName, TermName, LimitName : String;
  TermUuid, LimiTUuid: TUuid;
  pLimit: TOpLimitObject;
begin
  Nterm := pElem.Nterms;
  BusName := pElem.FirstBus;
  for j := 1 to NTerm do begin
    if IsGroundBus (BusName) = False then begin
      ref := pElem.Terminals^[j].BusRef;
      TermName := pElem.Name + '_T' + IntToStr(j);
      TermUuid := GetTermUuid (pElem, j);
      StartFreeInstance (FunPrf, 'Terminal', TermUuid);
      StringNode (FunPrf, 'IdentifiedObject.mRID', UUIDToCIMString(TermUuid));
      StringNode (FunPrf, 'IdentifiedObject.name', TermName);
      UuidNode (FunPrf, 'Terminal.ConductingEquipment', RefUuid);
      IntegerNode (FunPrf, 'ACDCTerminal.sequenceNumber', j);
      FD.WriteCimLn (TopoPrf, Format('  <cim:Terminal.ConnectivityNode rdf:resource="#%s"/>',
        [ActiveCircuit.Buses[ref].CIM_ID]));
      if (j = 1) and (norm > 0.0) then begin
        if emerg < norm then emerg := norm;
        LimitName := GetOpLimIName (norm, emerg);
        pLimit := GetOpLimit (LimitName);
        if pLimit = nil then begin
          pLimit := TOpLimitObject.Create(norm, emerg);
          pLimit.localName := LimitName;
          pLimit.UUID := GetDevUuid (OpLimI, LimitName, 0);
          AddOpLimit (pLimit);
        end;
        LimiTUuid := GetDevUuid (OpLimI, LimitName, 0);
        UuidNode (FunPrf, 'ACDCTerminal.OperationalLimitSet', LimiTUuid);
      end;
      EndInstance (FunPrf, 'Terminal');
    end;
    BusName := pElem.Nextbus;
  end;
end;

procedure WriteTerminals(pElem:TDSSCktElement; geoUUID: TUuid; crsUUID: TUuid;
  norm: double=0.0; emerg: double=0.0);
begin
  WriteReferenceTerminals (pElem, pElem.UUID, norm, emerg);
  WritePositions (pElem, geoUUID, crsUUID);
end;

procedure VbaseNode (prf:ProfileChoice; pElem:TDSSCktElement);
var
  j: integer;
begin
  j := pElem.Terminals^[1].BusRef;
  UuidNode (prf, 'ConductingEquipment.BaseVoltage',
    GetBaseVUuid (sqrt(3.0) * ActiveCircuit.Buses^[j].kVBase));
end;

Procedure WriteXfmrCode (pXfCd: TXfmrCodeObj);
var
  pName, pBank: TNamedObject;
  ratShort, ratEmerg, val, Zbase: double;
  i, j, seq: Integer;
begin
  pName := TNamedObject.Create('dummy');
  pBank := TNamedObject.Create('dummy');
  with pXfCd do begin
    pBank.LocalName := pXfCd.Name + '_PowerXfInfo';
    pBank.UUID := GetDevUuid (XfInfo, pXfCd.Name, 1);
    StartInstance (CatPrf, 'PowerTransformerInfo', pBank);
    EndInstance (CatPrf, 'PowerTransformerInfo');
    StartInstance (CatPrf, 'TransformerTankInfo', pXfCd);
    RefNode (CatPrf, 'TransformerTankInfo.PowerTransformerInfo', pBank);
    EndInstance (CatPrf, 'TransformerTankInfo');
    ratShort := NormMaxHKVA / Winding^[1].kva;
    ratEmerg := EmergMaxHKVA / Winding^[1].kva;
    for i := 1 to NumWindings do begin
      Zbase := Winding^[i].kvll;
      Zbase := 1000.0 * Zbase * Zbase / Winding^[1].kva;
      pName.localName := pXfCd.Name + '_' + IntToStr (i);
      pName.UUID := GetDevUuid (WdgInf, pXfCd.Name, i);
      StartInstance (CatPrf, 'TransformerEndInfo', pName);
      RefNode (CatPrf, 'TransformerEndInfo.TransformerTankInfo', pXfCd);
      IntegerNode (CatPrf, 'TransformerEndInfo.endNumber', i);
      if pXfCd.FNPhases < 3 then begin
        WindingConnectionEnum (CatPrf, 'I');
        if (i = 3) and (Winding^[i].kvll < 0.3) then // for center-tap secondary
          IntegerNode (CatPrf, 'TransformerEndInfo.phaseAngleClock', 6)
        else
          IntegerNode (CatPrf, 'TransformerEndInfo.phaseAngleClock', 0)
      end else begin
        if Winding^[i].Connection = 1 then
          WindingConnectionEnum (CatPrf, 'D')
        else
          if (Winding^[i].Rneut > 0.0) or (Winding^[i].Xneut > 0.0) then
            WindingConnectionEnum (CatPrf, 'Yn')
          else
            WindingConnectionEnum (CatPrf, 'Y');
        if Winding^[i].Connection <> Winding^[1].Connection then
          IntegerNode (CatPrf, 'TransformerEndInfo.phaseAngleClock', 1)
        else
          IntegerNode (CatPrf, 'TransformerEndInfo.phaseAngleClock', 0);
      end;
      DoubleNode (CatPrf, 'TransformerEndInfo.ratedU', 1000 * Winding^[i].kvll);
      DoubleNode (CatPrf, 'TransformerEndInfo.ratedS', 1000 * Winding^[i].kva);
      DoubleNode (CatPrf, 'TransformerEndInfo.shortTermS', 1000 * Winding^[i].kva * ratShort);
      DoubleNode (CatPrf, 'TransformerEndInfo.emergencyS', 1000 * Winding^[i].kva * ratEmerg);
      DoubleNode (CatPrf, 'TransformerEndInfo.r', Winding^[i].Rpu * Zbase);
      DoubleNode (CatPrf, 'TransformerEndInfo.insulationU', 0.0);
      EndInstance (CatPrf, 'TransformerEndInfo');
    end;
    pName.localName:= pXfCd.Name + '_' + IntToStr(1);
    pName.UUID := GetDevUuid (OcTest, pXfCd.Name, 1);
    StartInstance (CatPrf, 'NoLoadTest', pName);
    UuidNode (CatPrf, 'NoLoadTest.EnergisedEnd', GetDevUuid (WdgInf, pXfCd.Name, 1));
    DoubleNode (CatPrf, 'NoLoadTest.energisedEndVoltage', 1000.0 * Winding^[1].kvll);
    DoubleNode (CatPrf, 'NoLoadTest.excitingCurrent', pctImag);
    DoubleNode (CatPrf, 'NoLoadTest.excitingCurrentZero', pctImag);
    val := 0.01 * pctNoLoadLoss * Winding^[1].kva; // losses to be in kW
    DoubleNode (CatPrf, 'NoLoadTest.loss', val);
    DoubleNode (CatPrf, 'NoLoadTest.lossZero', val);
    DoubleNode (CatPrf, 'TransformerTest.basePower', 1000.0 * Winding^[1].kva);
    DoubleNode (CatPrf, 'TransformerTest.temperature', 50.0);
    EndInstance (CatPrf, 'NoLoadTest');
    seq := 0;
    for i:= 1 to NumWindings do
      for j:= (i+1) to NumWindings do begin
        Inc (seq);
        pName.localName:= pXfCd.Name + '_' + IntToStr(seq);
        pName.UUID := GetDevUuid (ScTest, pXfCd.Name, seq);
        StartInstance (CatPrf, 'ShortCircuitTest', pName);
        UuidNode (CatPrf, 'ShortCircuitTest.EnergisedEnd', GetDevUuid (WdgInf, pXfCd.Name, i));
         // NOTE: can insert more than one GroundedEnds for three-winding short-circuit tests
        UuidNode (CatPrf, 'ShortCircuitTest.GroundedEnds', GetDevUuid (WdgInf, pXfCd.Name, j));
        IntegerNode (CatPrf, 'ShortCircuitTest.energisedEndStep', Winding^[i].NumTaps div 2);
        IntegerNode (CatPrf, 'ShortCircuitTest.groundedEndStep', Winding^[j].NumTaps div 2);
        Zbase := Winding^[i].kvll;
        Zbase := 1000.0 * Zbase * Zbase / Winding^[1].kva;  // all DSS impedances are on winding 1 base
        val := Xsc^[seq] * Zbase;
        DoubleNode (CatPrf, 'ShortCircuitTest.leakageImpedance', val);
        DoubleNode (CatPrf, 'ShortCircuitTest.leakageImpedanceZero', val);
        if seq = 1 then begin // put all the load loss on test from wdg1 to wdg2
          val := 0.01 * pctLoadLoss * Winding^[1].kva; // losses are to be in kW
          DoubleNode (CatPrf, 'ShortCircuitTest.loss', val);
          DoubleNode (CatPrf, 'ShortCircuitTest.lossZero', val);
        end else begin
          DoubleNode (CatPrf, 'ShortCircuitTest.loss', 0.0);
          DoubleNode (CatPrf, 'ShortCircuitTest.lossZero', 0.0);
        end;
        DoubleNode (CatPrf, 'TransformerTest.basePower', 1000.0 * Winding^[i].kva);
        DoubleNode (CatPrf, 'TransformerTest.temperature', 50.0);
        EndInstance (CatPrf, 'ShortCircuitTest');
      end;
  end;
  pName.Free;
end;

Procedure WriteCableData (pCab: TCableDataObj);
var
  v1: double;
begin
  with pCab do begin
    v1 := To_Meters (RadiusUnits);
    BooleanNode (CatPrf, 'WireInfo.insulated', True);
    DoubleNode (CatPrf, 'WireInfo.insulationThickness', v1 * pCab.InsLayer);
    ConductorInsulationEnum (CatPrf, 'crosslinkedPolyethylene'); // TODO -  code EpsR
    CableOuterJacketEnum (CatPrf, 'none');
    CableConstructionEnum (CatPrf, 'stranded');
    BooleanNode (CatPrf, 'CableInfo.isStrandFill', False); // we don't really know this
    DoubleNode (CatPrf, 'CableInfo.diameterOverCore',
      v1 * (pCab.DiaIns - 2.0 * pCab.InsLayer));
    DoubleNode (CatPrf, 'CableInfo.diameterOverInsulation', v1 * pCab.DiaIns);
    DoubleNode (CatPrf, 'CableInfo.diameterOverJacket', v1 * pCab.DiaCable);
    DoubleNode (CatPrf, 'CableInfo.nominalTemperature', 90.0);  // we don't really know this
  end;
end;

Procedure WriteTapeData (pCab: TTSDataObj);
var
  v1: double;
begin
  with pCab do begin
    v1 := To_Meters (RadiusUnits);
    DoubleNode (CatPrf, 'CableInfo.diameterOverScreen',
      v1 * (pCab.DiaShield - 2.0 * pCab.TapeLayer));
    DoubleNode (CatPrf, 'TapeShieldCableInfo.tapeLap', pCab.TapeLap);
    DoubleNode (CatPrf, 'TapeShieldCableInfo.tapeThickness', v1 * pCab.TapeLayer);
    CableShieldMaterialEnum (CatPrf, 'copper');
    BooleanNode (CatPrf, 'CableInfo.sheathAsNeutral', True);
  end;
end;

Procedure WriteConcData (pCab: TCNDataObj);
var
  v1: double;
begin
  with pCab do begin
    v1 := To_Meters (RadiusUnits);
    DoubleNode (CatPrf, 'CableInfo.diameterOverScreen',
      v1 * (pCab.DiaCable - 2.0 * pCab.DiaStrand));
    DoubleNode (CatPrf, 'ConcentricNeutralCableInfo.diameterOverNeutral',
      v1 * pCab.DiaCable);
    DoubleNode (CatPrf, 'ConcentricNeutralCableInfo.neutralStrandRadius',
      v1 * 0.5 * pCab.DiaStrand);
    DoubleNode (CatPrf, 'ConcentricNeutralCableInfo.neutralStrandGmr',
      v1 * pCab.GmrStrand);
    v1 := To_per_Meter (ResUnits);
    DoubleNode (CatPrf, 'ConcentricNeutralCableInfo.neutralStrandRDC20',
      v1 * pCab.RStrand);
    IntegerNode (CatPrf, 'ConcentricNeutralCableInfo.neutralStrandCount', pCab.NStrand);
    BooleanNode (CatPrf, 'CableInfo.sheathAsNeutral', False);
  end;
end;

Procedure WriteWireData (pWire: TConductorDataObj);
var
  v1: double;
begin
  with pWire do begin
    StringNode (CatPrf, 'WireInfo.sizeDescription', DisplayName);
    if CompareText (LeftStr (name, 2), 'AA') = 0 then
      ConductorMaterialEnum (CatPrf, 'aluminum')
    else if CompareText (LeftStr (name, 4), 'ACSR') = 0 then
      ConductorMaterialEnum (CatPrf, 'acsr')
    else if CompareText (LeftStr (name, 2), 'CU') = 0 then
      ConductorMaterialEnum (CatPrf, 'copper')
    else if CompareText (LeftStr (name, 3), 'EHS') = 0 then
      ConductorMaterialEnum (CatPrf, 'steel')
    else
      ConductorMaterialEnum (CatPrf, 'other');
    v1 := To_Meters (GMRUnits);
    DoubleNode (CatPrf, 'WireInfo.gmr', GMR * v1);
    v1 := To_Meters (RadiusUnits);
    DoubleNode (CatPrf, 'WireInfo.radius', Radius * v1);
    v1 := To_per_Meter (ResUnits);
    DoubleNode (CatPrf, 'WireInfo.rDC20', Rdc * v1);
    DoubleNode (CatPrf, 'WireInfo.rAC25', Rac * v1);
    DoubleNode (CatPrf, 'WireInfo.rAC50', Rac * v1);
    DoubleNode (CatPrf, 'WireInfo.rAC75', Rac * v1);
    DoubleNode (CatPrf, 'WireInfo.ratedCurrent', MaxValue ([NormAmps, 0.0]));
    IntegerNode (CatPrf, 'WireInfo.strandCount', 0);
    IntegerNode (CatPrf, 'WireInfo.coreStrandCount', 0);
    DoubleNode (CatPrf, 'WireInfo.coreRadius', 0.0);
  end;
end;

procedure StartCIMFile (var F: TextFile; FileNm:String; prf: ProfileChoice);
begin
  Assignfile (F, FileNm);
  ReWrite (F);
  Writeln (F,'<?xml version="1.0" encoding="utf-8"?>');
  Writeln (F,'<!-- un-comment this line to enable validation');
  Writeln (F,'-->');
  Writeln (F,'<rdf:RDF xmlns:cim="' + CIM_NS + '#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">');
  Writeln (F,'<!--');
  Writeln (F,'-->');
  WriteLn (F, Format('<cim:IEC61970CIMVersion rdf:ID="%s">', [UUIDToCIMString (GetDevUuid (CIMVer, 'IEC', 1))]));
  WriteLn (F, Format ('  <cim:IEC61970CIMVersion.version>%s</cim:IEC61970CIMVersion.version>', ['IEC61970CIM100']));
  WriteLn (F, Format ('  <cim:IEC61970CIMVersion.date>%s</cim:IEC61970CIMVersion.date>', ['2019-04-01']));
  WriteLn (F, '</cim:IEC61970CIMVersion>');
end;

procedure ListXfmrCodes (clsXfCd : TXfmrCode; lbl:String); // for debugging
var
  pXfCd : TXfmrCodeObj;
begin
  writeln('xfmrcodes at ' + lbl);
  pXfCd := clsXfCd.ElementList.First;
  while pXfCd <> nil do begin
    writeln('  ' + pXfCd.LocalName + ' ' + pXfCd.Name + ' ' + UUIDtoString (pXfCd.UUID));
    pXfCd := clsXfCd.ElementList.Next;
  end;
end;

Procedure ExportCDPSM(FileNm:String;
  Substation:String;
  SubGeographicRegion:String;
  GeographicRegion: String;
  FdrUUID: TUuid; 
  SubUUID: TUuid;
  SubGeoUUID: TUuid; 
  RgnUUID: TUuid;
  Combined:Boolean);
Var
  i, j, k: Integer;
  seq    : Integer;
  val    : double;
  bval   : Boolean;
  v1, v2 : double;
  i1, i2 : Integer;
  Zs, Zm : complex;
  Rs, Rm, Xs, Xm, R1, R0, X1, X0: double;
  pName1, pName2 : TNamedObject;
  pIsland, pSwing : TNamedObject;  // island and ref node
  pRegion, pSubRegion, pLocation, pSubstation, pCRS : TNamedObject;

  pILimit : TOpLimitObject;
  pNormLimit, pEmergLimit, pRangeAHiLimit, pRangeALoLimit, pRangeBHiLimit, pRangeBLoLimit : TNamedObject; // OperationalLimitType
  LimitName : String;
  LimiTUuid: TUuid;

  zbase  : double;
  s      : String;
  swtCls : String;  // based on controls, if any, attached to a line having switch=yes
  ratedAmps, breakingAmps: double;

  pBank  : TBankObject;
  maxWdg : Integer;
  WdgList  : array of TNamedObject;
  CoreList : array of TNamedObject;
  MeshList : array of TNamedObject;
  sBank  : String;
  bTanks : boolean;

  pLoad  : TLoadObj;
  pVsrc  : TVsourceObj;
  pGen   : TGeneratorObj;
  pPV    : TPVSystemObj;
  pBat   : TStorageObj;

  pCap  : TCapacitorObj;
  pCapC : TCapControlObj;
  pXf   : TTransfObj;
  pReg  : TRegControlObj;
  pLine : TLineObj;
  pReac : TReactorObj;

  clsLnCd : TLineCode;
  clsGeom : TLineGeometry;
  clsWire : TWireData;
  clsXfCd : TXfmrCode;
  clsSpac : TLineSpacing;
  clsTape : TTSData;
  clsConc : TCNData;

  pLnCd : TLineCodeObj;
  pGeom : TLineGeometryObj;
  pWire : TWireDataObj;
  pXfCd : TXfmrCodeObj;
  pSpac : TLineSpacingObj;
  pTape : TTSDataObj;
  pConc : TCNDataObj;

  // DSS-like load models
  id1_ConstkVA:     TUuid;
  id2_ConstZ:       TUuid;
  id3_ConstPQuadQ:  TUuid;
  id4_LinPQuadQ:    TUuid;
  id5_ConstI:       TUuid;
  id6_ConstPConstQ: TUuid;  // P can vary, Q not
  id7_ConstPConstX: TUuid;

  // for CIM Locations
  geoUUID: TUuid;
  crsUUID: TUuid;
Begin
  Try
    clsLnCd := DSSClassList.Get(ClassNames.Find('linecode'));
    clsWire := DSSClassList.Get(ClassNames.Find('wiredata'));
    clsGeom := DSSClassList.Get(ClassNames.Find('linegeometry'));
    clsXfCd := DSSClassList.Get(ClassNames.Find('xfmrcode'));
    clsSpac := DSSClassList.Get(ClassNames.Find('linespacing'));
    clsTape := DSSClassList.Get(ClassNames.Find('TSData'));
    clsConc := DSSClassList.Get(ClassNames.Find('CNData'));
    pName1 := TNamedObject.Create('Temp1');
    pName2 := TNamedObject.Create('Temp2');
    if not assigned(UuidList) then begin  // this may have been done already from the uuids command
        i1 := clsXfCd.ElementCount * 6; // 3 wdg info, 3 sctest
        i2 := ActiveCircuit.Transformers.ListSize * 11; // bank, info, 3 wdg, 3 wdg info, 3sctest
        StartUuidList (i1 + i2);
    end;
    StartBankList (ActiveCircuit.Transformers.ListSize);
    StartOpLimitList (ActiveCircuit.Lines.ListSize);

    {$IFDEF FPC}
    // this only works in the command line version
    Writeln(FileNm + '<=' + ActiveCircuit.Name + '<-' + Substation + '<-' + SubGeographicRegion + '<-' + GeographicRegion);
    {$ENDIF}

    FD := TFileDealer.Create(Combined, FileNm);

    pCRS := TNamedObject.Create ('CoordinateSystem');
    crsUUID := GetDevUuid (CoordSys, 'Local', 1);
    pCRS.UUID := crsUUID;
    pCRS.localName := ActiveCircuit.Name + '_CrsUrn';
    StartInstance (GeoPrf, 'CoordinateSystem', pCRS);
    StringNode (GeoPrf, 'CoordinateSystem.crsUrn', 'OpenDSSLocalBusCoordinates');
    EndInstance (GeoPrf, 'CoordinateSystem');

    pRegion := TNamedObject.Create ('GeographicalRegion');
    pRegion.UUID := RgnUUID;
    pRegion.LocalName := GeographicRegion;
    StartInstance (FunPrf, 'GeographicalRegion', pRegion);
    EndInstance (FunPrf, 'GeographicalRegion');

    pSubRegion := TNamedObject.Create ('SubGeographicalRegion');
    pSubRegion.UUID := SubGeoUUID;
    pSubRegion.LocalName := SubGeographicRegion;
    StartInstance (FunPrf, 'SubGeographicalRegion', pSubRegion);
    RefNode (FunPrf, 'SubGeographicalRegion.Region', pRegion);
    EndInstance (FunPrf, 'SubGeographicalRegion');

    pSubstation := TNamedObject.Create ('Substation');
    pSubstation.UUID := SubUUID;
    pSubstation.LocalName := Substation;
    StartInstance (FunPrf, 'Substation', pSubstation);
    RefNode (FunPrf, 'Substation.Region', pSubRegion);
    EndInstance (FunPrf, 'Substation');

    pLocation := TNamedObject.Create ('Location');
    pLocation.UUID := GetDevUuid (FdrLoc, ActiveCircuit.Name, 1);
    pLocation.localName := ActiveCircuit.Name + '_Location';
    StartInstance (GeoPrf, 'Location', pLocation);
    UuidNode (GeoPrf, 'Location.CoordinateSystem', crsUUID);
    EndInstance (GeoPrf, 'Location');

    ActiveCircuit.UUID := FdrUUID;
    StartInstance (FunPrf, 'Feeder', ActiveCircuit);
    RefNode (FunPrf, 'Feeder.NormalEnergizingSubstation', pSubstation);
    RefNode (FunPrf, 'PowerSystemResource.Location', pLocation);
    EndInstance (FunPrf, 'Feeder');

    // the whole system will be a topo island
    pIsland := TNamedObject.Create('Island');
    pIsland.localName := ActiveCircuit.Name + '_Island';
    pIsland.UUID := GetDevUuid (TopoIsland, 'Island', 1);
    pSwing := TNamedObject.Create('SwingBus');
    pSwing.localName := ActiveCircuit.Name + '_SwingBus';

    pNormLimit := TNamedObject.Create('NormalAmpsType');
    pNormLimit.localName := ActiveCircuit.Name + '_NormAmpsType';
    pNormLimit.UUID := GetDevUuid (OpLimT, 'NormalAmps', 1);
    StartInstance (FunPrf, 'OperationalLimitType', pNormLimit);
    DoubleNode (FunPrf, 'OperationalLimitType.acceptableDuration', 5.0e9);  // more than 100 years
    OpLimitDirectionEnum (FunPrf, 'absoluteValue');
    EndInstance (FunPrf, 'OperationalLimitType');

    pEmergLimit := TNamedObject.Create('EmergencyAmpsType');
    pEmergLimit.localName := ActiveCircuit.Name + '_EmergencyAmpsType';
    pEmergLimit.UUID := GetDevUuid (OpLimT, 'EmergencyAmps', 1);
    StartInstance (FunPrf, 'OperationalLimitType', pEmergLimit);
    DoubleNode (FunPrf, 'OperationalLimitType.acceptableDuration', 2.0 * 3600.0); // 2 hours
    OpLimitDirectionEnum (FunPrf, 'absoluteValue');
    EndInstance (FunPrf, 'OperationalLimitType');

    pRangeAHiLimit := TNamedObject.Create('RangeAHiType');
    pRangeAHiLimit.localName := ActiveCircuit.Name + '_RangeAHiType';
    pRangeAHiLimit.UUID := GetDevUuid (OpLimT, 'AHi', 1);
    StartInstance (FunPrf, 'OperationalLimitType', pRangeAHiLimit);
    DoubleNode (FunPrf, 'OperationalLimitType.acceptableDuration', 5.0e9);
    OpLimitDirectionEnum (FunPrf, 'high');
    EndInstance (FunPrf, 'OperationalLimitType');

    pRangeALoLimit := TNamedObject.Create('RangeALoType');
    pRangeALoLimit.localName := ActiveCircuit.Name + '_RangeALoType';
    pRangeALoLimit.UUID := GetDevUuid (OpLimT, 'ALo', 1);
    StartInstance (FunPrf, 'OperationalLimitType', pRangeALoLimit);
    DoubleNode (FunPrf, 'OperationalLimitType.acceptableDuration', 5.0e9);
    OpLimitDirectionEnum (FunPrf, 'low');
    EndInstance (FunPrf, 'OperationalLimitType');

    pRangeBHiLimit := TNamedObject.Create('RangeBHiType');
    pRangeBHiLimit.localName := ActiveCircuit.Name + '_RangeBHiType';
    pRangeBHiLimit.UUID := GetDevUuid (OpLimT, 'BHi', 1);
    StartInstance (FunPrf, 'OperationalLimitType', pRangeBHiLimit);
    DoubleNode (FunPrf, 'OperationalLimitType.acceptableDuration', 24.0 * 3600.0);
    OpLimitDirectionEnum (FunPrf, 'high');
    EndInstance (FunPrf, 'OperationalLimitType');

    pRangeBLoLimit := TNamedObject.Create('RangeBLoType');
    pRangeBLoLimit.localName := ActiveCircuit.Name + '_RangeBLoType';
    pRangeBLoLimit.UUID := GetDevUuid (OpLimT, 'BLo', 1);
    StartInstance (FunPrf, 'OperationalLimitType', pRangeBLoLimit);
    DoubleNode (FunPrf, 'OperationalLimitType.acceptableDuration', 24.0 * 3600.0);
    OpLimitDirectionEnum (FunPrf, 'low');
    EndInstance (FunPrf, 'OperationalLimitType');

    with ActiveCircuit do begin
      // build the lists of base voltages and operational voltage limits
      i := 1;
      while LegalVoltageBases[i] > 0.0 do begin
        s := GetBaseVName (LegalVoltageBases[i]);
        pName1.LocalName := s;
        pName1.UUID := GetBaseVUuid (LegalVoltageBases[i]);
        StartInstance (FunPrf, 'BaseVoltage', pName1);
        DoubleNode (FunPrf, 'BaseVoltage.nominalVoltage', 1000.0 * LegalVoltageBases[i]);
        EndInstance (FunPrf, 'BaseVoltage');

        pName1.LocalName := GetOpLimVName (LegalVoltageBases[i]);
        pName1.UUID := GetOpLimVUuid (LegalVoltageBases[i]);
        StartInstance (FunPrf, 'OperationalLimitSet', pName1);
        EndInstance (FunPrf, 'OperationalLimitSet');

        pName2.LocalName := pName1.LocalName + '_RangeAHi';
        pName2.UUID := GetDevUuid (OpLimAHi, s, 1);
        StartInstance (FunPrf, 'VoltageLimit', pName2);
        RefNode (FunPrf, 'OperationalLimit.OperationalLimitSet', pName1);
        RefNode (FunPrf, 'OperationalLimit.OperationalLimitType', pRangeAHiLimit);
        DoubleNode (FunPrf, 'VoltageLimit.value', 1.05 * 1000.0 * LegalVoltageBases[i]);
        EndInstance (FunPrf, 'VoltageLimit');

        pName2.LocalName := pName1.LocalName + '_RangeALo';
        pName2.UUID := GetDevUuid (OpLimALo, s, 1);
        StartInstance (FunPrf, 'VoltageLimit', pName2);
        RefNode (FunPrf, 'OperationalLimit.OperationalLimitSet', pName1);
        RefNode (FunPrf, 'OperationalLimit.OperationalLimitType', pRangeALoLimit);
        DoubleNode (FunPrf, 'VoltageLimit.value', 0.95 * 1000.0 * LegalVoltageBases[i]);
        EndInstance (FunPrf, 'VoltageLimit');

        pName2.LocalName := pName1.LocalName + '_RangeBHi';
        pName2.UUID := GetDevUuid (OpLimBHi, s, 1);
        StartInstance (FunPrf, 'VoltageLimit', pName2);
        RefNode (FunPrf, 'OperationalLimit.OperationalLimitSet', pName1);
        RefNode (FunPrf, 'OperationalLimit.OperationalLimitType', pRangeBHiLimit);
        DoubleNode (FunPrf, 'VoltageLimit.value', 1.0583333 * 1000.0 * LegalVoltageBases[i]);
        EndInstance (FunPrf, 'VoltageLimit');

        pName2.LocalName := pName1.LocalName + '_RangeBLo';
        pName2.UUID := GetDevUuid (OpLimBLo, s, 1);
        StartInstance (FunPrf, 'VoltageLimit', pName2);
        RefNode (FunPrf, 'OperationalLimit.OperationalLimitSet', pName1);
        RefNode (FunPrf, 'OperationalLimit.OperationalLimitType', pRangeBLoLimit);
        DoubleNode (FunPrf, 'VoltageLimit.value', 0.9166667 * 1000.0 * LegalVoltageBases[i]);
        EndInstance (FunPrf, 'VoltageLimit');

        inc(i);
      end;

      for i := 1 to NumBuses do begin
        Buses^[i].localName:= BusList.Get(i);
      end;

      // each bus corresponds to a topo node (TODO, do we need topo nodes anymore?) and connectivity node
      for i := 1 to NumBuses do begin
        geoUUID := GetDevUuid (Topo, Buses^[i].localName, 1);
        StartFreeInstance (TopoPrf, 'TopologicalNode', geoUUID);
        StringNode (TopoPrf, 'IdentifiedObject.mRID', UUIDToCIMString(geoUUID));
        StringNode (TopoPrf, 'IdentifiedObject.name', Buses^[i].localName);
        UuidNode (TopoPrf, 'TopologicalNode.TopologicalIsland', pIsland.UUID);
        EndInstance (TopoPrf,'TopologicalNode');

        StartFreeInstance (TopoPrf, 'ConnectivityNode', Buses^[i].UUID);
        StringNode (TopoPrf, 'IdentifiedObject.mRID', UUIDToCIMString(Buses^[i].UUID));
        StringNode (TopoPrf, 'IdentifiedObject.name', Buses^[i].localName);
        UuidNode (TopoPrf, 'ConnectivityNode.TopologicalNode', geoUUID);
        UuidNode (TopoPrf, 'ConnectivityNode.OperationalLimitSet', GetOpLimVUuid (sqrt(3.0) * ActiveCircuit.Buses^[i].kVBase));
        FD.WriteCimLn (TopoPrf, Format('  <cim:ConnectivityNode.ConnectivityNodeContainer rdf:resource="#%s"/>',
          [ActiveCircuit.CIM_ID]));
        EndInstance (TopoPrf,'ConnectivityNode');
      end;

      // find the swing bus ==> first voltage source
      pVsrc := ActiveCircuit.Sources.First; // pIsrc are in the same list
      while pVsrc <> nil do begin
        if pVsrc.ClassNameIs('TVSourceObj') then begin
          if pVsrc.Enabled then begin
            i := pVsrc.Terminals^[1].BusRef;
            geoUUID := GetDevUuid (Topo, Buses^[i].localName, 1);
            pSwing.UUID := geoUUID;
            StartInstance (TopoPrf, 'TopologicalIsland', pIsland);
            RefNode (TopoPrf, 'TopologicalIsland.AngleRefTopologicalNode', pSwing);
            EndInstance (TopoPrf, 'TopologicalIsland');
            break;
          end;
        end;
        pVsrc := ActiveCircuit.Sources.Next;
      end;
    end;

    pGen := ActiveCircuit.Generators.First;
    while pGen <> nil do begin
     if pGen.Enabled  then   begin
        StartInstance (FunPrf, 'SynchronousMachine', pGen);
        CircuitNode (FunPrf, ActiveCircuit);
        DoubleNode (SshPrf, 'SynchronousMachine.p', pGen.Presentkw * 1000.0);
        DoubleNode (SshPrf, 'SynchronousMachine.q', pGen.Presentkvar * 1000.0);
        DoubleNode (EpPrf, 'SynchronousMachine.ratedS', pGen.GenVars.kvarating * 1000.0);
        DoubleNode (EpPrf, 'SynchronousMachine.ratedU', pGen.Presentkv * 1000.0);
//        SynchMachTypeEnum (F, 'generator');
//        SynchMachModeEnum (F, 'generator');
        geoUUID := GetDevUuid (MachLoc, pGen.LocalName, 1);
        UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
        EndInstance (FunPrf, 'SynchronousMachine');
        AttachGeneratorPhases (pGen, geoUUID);
        WriteTerminals (pGen, geoUUID, crsUUID);
     end;
     pGen := ActiveCircuit.Generators.Next;
    end;

    pPV := ActiveCircuit.PVSystems.First;
    while pPV <> nil do begin
      if pPV.Enabled then begin
        pName1.LocalName := pPV.Name; // + '_PVPanels';
        pName1.UUID := GetDevUuid (PVPanels, pPV.LocalName, 1);
        StartInstance (FunPrf, 'PhotovoltaicUnit', pName1);
        geoUUID := GetDevUuid (SolarLoc, pPV.localName, 1);
        UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
        DoubleNode (EpPrf, 'PowerElectronicsUnit.maxP', pPV.Pmpp * 1000.0);
        DoubleNode (EpPrf, 'PowerElectronicsUnit.minP', min(pPV.PctCutIn, pPV.PctCutOut) * pPV.kVARating * 1000.0 / 100.0);
        EndInstance (FunPrf, 'PhotovoltaicUnit');
        StartInstance (FunPrf, 'PowerElectronicsConnection', pPV);
        CircuitNode (FunPrf, ActiveCircuit);
        RefNode (FunPrf, 'PowerElectronicsConnection.PowerElectronicsUnit', pName1);
        DoubleNode (EpPrf, 'PowerElectronicsConnection.maxIFault', 1.0 / pPV.MinModelVoltagePU);
//        if FD.Separate then StartFreeInstance (SshPrf, 'PowerElectronicsConnection', pPV.UUID);
        DoubleNode (SshPrf, 'PowerElectronicsConnection.p', pPV.Presentkw * 1000.0);
        DoubleNode (SshPrf, 'PowerElectronicsConnection.q', pPV.Presentkvar * 1000.0);
//        if FD.Separate then EndInstance (SshPrf, 'PowerElectronicsConnection');
        DoubleNode (EpPrf, 'PowerElectronicsConnection.ratedS', pPV.PVSystemVars.fkvarating * 1000.0);
        DoubleNode (EpPrf, 'PowerElectronicsConnection.ratedU', pPV.Presentkv * 1000.0);
        UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
        EndInstance (FunPrf, 'PowerElectronicsConnection');
        AttachSolarPhases (pPV, geoUUID);
        // we want the location using PV unit name
        WriteReferenceTerminals (pPV, pPV.UUID);
        s := pPV.LocalName;
        pPV.LocalName := pName1.LocalName;
        WritePositions (pPV, geoUUID, crsUUID);
        pPV.LocalName := s;
      end;
      pPV := ActiveCircuit.PVSystems.Next;
    end;

    pBat := ActiveCircuit.StorageElements.First;
    while pBat <> nil do begin
      if pBat.Enabled then begin
        pName1.LocalName := pBat.Name; // + '_Cells';
        pName1.UUID := GetDevUuid (Battery, pBat.LocalName, 1);
        StartInstance (FunPrf, 'BatteryUnit', pName1);
        DoubleNode (EpPrf, 'PowerElectronicsUnit.maxP', pBat.StorageVars.kwRating * pBat.pctKwOut * 1000.0 / 100.0);
        DoubleNode (EpPrf, 'PowerElectronicsUnit.minP', -pBat.StorageVars.kwRating * pBat.pctKwIn * 1000.0 / 100.0);
        DoubleNode (SshPrf, 'BatteryUnit.ratedE', pBat.StorageVars.kwhRating * 1000.0);
        DoubleNode (SshPrf, 'BatteryUnit.storedE', pBat.StorageVars.kwhStored * 1000.0);
        BatteryStateEnum (SshPrf, pBat.StorageState);
        geoUUID := GetDevUuid (BatteryLoc, pBat.localName, 1);
        UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
        EndInstance (FunPrf, 'BatteryUnit');
        StartInstance (FunPrf, 'PowerElectronicsConnection', pBat);
        CircuitNode (FunPrf, ActiveCircuit);
        RefNode (FunPrf, 'PowerElectronicsConnection.PowerElectronicsUnit', pName1);
        DoubleNode (EpPrf, 'PowerElectronicsConnection.maxIFault', 1.0 / pBat.MinModelVoltagePU);
        DoubleNode (SshPrf, 'PowerElectronicsConnection.p', pBat.Presentkw * 1000.0);
        DoubleNode (SshPrf, 'PowerElectronicsConnection.q', pBat.Presentkvar * 1000.0);
        DoubleNode (EpPrf, 'PowerElectronicsConnection.ratedS', pBat.StorageVars.kvarating * 1000.0);
        DoubleNode (EpPrf, 'PowerElectronicsConnection.ratedU', pBat.Presentkv * 1000.0);
        UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
        EndInstance (FunPrf, 'PowerElectronicsConnection');
        AttachStoragePhases (pBat, geoUUID);
        // we want the location using battery unit name
        WriteReferenceTerminals (pBat, pBat.UUID);
        s := pBat.LocalName;
        pBat.LocalName := pName1.LocalName;
        WritePositions (pBat, geoUUID, crsUUID);
        pBat.LocalName := s;
      end;
      pBat := ActiveCircuit.StorageElements.Next;
    end;

    pVsrc := ActiveCircuit.Sources.First; // pIsrc are in the same list
    while pVsrc <> nil do begin
      if pVsrc.ClassNameIs('TVSourceObj') then
        if pVsrc.Enabled  then
        with pVsrc do begin
          Zs := Z.AvgDiagonal;
          Zm := Z.AvgOffDiagonal;
          Rs := Zs.re;
          Rm := Zm.re;
          Xs := Zs.im;
          Xm := Zm.im;
          v1 := pVsrc.NPhases;
          if v1 > 1.0 then begin
            R1 := Rs - Rm;
            X1 := Xs - Xm;
            R0 := Rs + (v1 - 1.0) * Rm;
            X0 := Xs + (v1 - 1.0) * Xm;
          end else begin
            R1 := Rs;
            X1 := Xs;
            R0 := Rs;
            X0 := Xs;
          end;

          StartInstance (FunPrf, 'EnergySource', pVsrc);
          CircuitNode (FunPrf, ActiveCircuit);
          VbaseNode (FunPrf, pVsrc);
          DoubleNode (EpPrf, 'EnergySource.nominalVoltage', 1000 * kVbase);
          DoubleNode (SshPrf, 'EnergySource.voltageMagnitude', 1000 * kVbase * PerUnit);
          DoubleNode (SshPrf, 'EnergySource.voltageAngle', TwoPi * Angle / 360.0);
          DoubleNode (EpPrf, 'EnergySource.r', R1);
          DoubleNode (EpPrf, 'EnergySource.x', X1);
          DoubleNode (EpPrf, 'EnergySource.r0', R0);
          DoubleNode (EpPrf, 'EnergySource.x0', X0);
          geoUUID := GetDevUuid (SrcLoc, pVsrc.LocalName, 1);
          UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
          EndInstance (FunPrf, 'EnergySource');
//          AttachPhases (F, pVsrc, 1, 'EnergySource');
          WriteTerminals (pVsrc, geoUUID, crsUUID);
        end;
      pVsrc := ActiveCircuit.Sources.Next;
    end;

    pCap := ActiveCircuit.ShuntCapacitors.First;
    while pCap <> nil do begin
      if pCap.Enabled then begin
        StartInstance (FunPrf, 'LinearShuntCompensator', pCap);
        CircuitNode (FunPrf, ActiveCircuit);
        VbaseNode (FunPrf, pCap);
        with pCap do begin
          val := 0.001 * Totalkvar / NomKV / NomKV / NumSteps;
          DoubleNode (EpPrf, 'ShuntCompensator.nomU', 1000.0 * NomKV);
          DoubleNode (EpPrf, 'LinearShuntCompensator.bPerSection', val);
          DoubleNode (EpPrf, 'LinearShuntCompensator.gPerSection', 0.0);

          val := 0.0;
          pCapC := ActiveCircuit.CapControls.First;
          while (pCapC <> nil) do begin
            if pCapC.This_Capacitor = pCap then val := pCapC.OnDelayVal;
            pCapC := ActiveCircuit.CapControls.Next;
          end;
          DoubleNode (EpPrf, 'ShuntCompensator.aVRDelay', val);

          if Connection = 0 then begin
            ShuntConnectionKindNode (FunPrf, 'ShuntCompensator', 'Y');
            BooleanNode (FunPrf, 'ShuntCompensator.grounded', True);  // TODO - check bus 2
            DoubleNode (EpPrf, 'LinearShuntCompensator.b0PerSection', val);
          end else begin
            ShuntConnectionKindNode (FunPrf, 'ShuntCompensator', 'D');
            BooleanNode (FunPrf, 'LinearShuntCompensator.grounded', False);
            DoubleNode (EpPrf, 'LinearShuntCompensator.b0PerSection', 0.0);
          end;
          DoubleNode (EpPrf, 'LinearShuntCompensator.g0PerSection', 0.0);
          IntegerNode (EpPrf, 'ShuntCompensator.normalSections', NumSteps);
          IntegerNode (EpPrf, 'ShuntCompensator.maximumSections', NumSteps);
          val := 0;
          for i := 1 to NumSteps do if States[i] > 0 then val := val + 1.0;
          DoubleNode (SshPrf, 'ShuntCompensator.sections', val);
          geoUUID := GetDevUuid (CapLoc, pCap.localName, 1);
          UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
          EndInstance (FunPrf, 'LinearShuntCompensator');
          AttachCapPhases (pCap, geoUUID, val);
          WriteTerminals (pCap, geoUUID, crsUUID, pCap.NormAmps, pCap.EmergAmps);
        end;
      end;
      pCap := ActiveCircuit.ShuntCapacitors.Next;
    end;

    pCapC := ActiveCircuit.CapControls.First;
    while (pCapC <> nil) do begin
      with pCapC do begin
        StartInstance (FunPrf, 'RegulatingControl', pCapC);
        UuidNode (GeoPrf, 'PowerSystemResource.Location', GetDevUuid (CapLoc, This_Capacitor.Name, 1));
        RefNode (FunPrf, 'RegulatingControl.RegulatingCondEq', This_Capacitor);
        i1 := GetCktElementIndex(ElementName); // Global function
        UuidNode (FunPrf, 'RegulatingControl.Terminal',
          GetTermUuid (ActiveCircuit.CktElements.Get(i1), ElementTerminal));
        s := FirstPhaseString (ActiveCircuit.CktElements.Get(i1), 1);
        if PTPhase > 0 then
          MonitoredPhaseNode (FunPrf, Char(Ord(s[1]) + PTPhase - 1))
        else
          MonitoredPhaseNode (FunPrf, Char(Ord(s[1]))); // TODO - average, min and max unsupported in CIM
        val := 1.0;
        if CapControlType = PFCONTROL then begin
          v1 := PfOnValue;
          v2 := PfOffValue
        end else begin
          v1 := OnValue;
          v2 := OffValue;
          if CapControlType = KVARCONTROL then val:= 1000.0;
          if CapControlType = CURRENTCONTROL then val:= CTRatioVal;
          if CapControlType = VOLTAGECONTROL then val:= PTRatioVal
        end;
        case CapControlType of
          CURRENTCONTROL: RegulatingControlEnum (EpPrf, 'currentFlow');
          VOLTAGECONTROL: RegulatingControlEnum (EpPrf, 'voltage');
          KVARCONTROL:    RegulatingControlEnum (EpPrf, 'reactivePower');
          TIMECONTROL:    RegulatingControlEnum (EpPrf, 'timeScheduled');
          PFCONTROL :     RegulatingControlEnum (EpPrf, 'powerFactor');
          USERCONTROL :   RegulatingControlEnum (EpPrf, 'userDefined'); // i.e. unsupported in CIM
        end;
        BooleanNode (EpPrf, 'RegulatingControl.discrete', true);
        BooleanNode (EpPrf, 'RegulatingControl.enabled', Enabled);
        DoubleNode (EpPrf, 'RegulatingControl.targetValue', val * 0.5 * (v1 + v2));
        DoubleNode (EpPrf, 'RegulatingControl.targetDeadband', val * (v2 - v1));
        EndInstance (FunPrf, 'RegulatingControl');
      end;
      pCapC := ActiveCircuit.CapControls.Next;
    end;

    // begin the transformers;
    //   1. if balanced three-phase and no XfmrCode, use PowerTransformerEnd(s), mesh impedances and core admittances with no tanks
    //   2. with XfmrCode, write TransformerTank, TransformerTankEnd(s) and references to TransformerTankInfoInfo
    //   3. otherwise, write TransformerTank, then create and reference TransformerTankInfo classes

    // for case 3, it's better to identify and create the info classes first
    //    TODO: side effect is that these transformers will reference XfmrCode until the text file is reloaded. Solution results should be the same.
    pXf := ActiveCircuit.Transformers.First;
    while pXf <> nil do begin
      if pXf.Enabled then begin
        if (length(pXf.XfmrCode) < 1) and (pXf.NPhases <> 3) then begin
          sBank := 'CIMXfmrCode_' + pXf.Name;
          clsXfCd.NewObject (sBank);
          clsXfCd.Code := sBank;
          pXfCd := ActiveXfmrCodeObj;
          pXfCd.UUID := GetDevUuid (TankInfo, pXfCd.Name, 1);
          pXfCd.PullFromTransformer (pXf);
          pXf.XfmrCode := pXfCd.Name;
        end;
      end;
      pXf := ActiveCircuit.Transformers.Next;
    end;

    // write all the XfmrCodes first (CIM TransformerTankInfo)
    pXfCd := clsXfCd.ElementList.First;
    while pXfCd <> nil do begin
      WriteXfmrCode (pXfCd);
      // link to the transformers using this XfmrCode
      pName1.LocalName := 'TankAsset_' + pXfCd.Name;
      pName1.UUID := GetDevUuid (TankAsset, pXfCd.Name, 1);
      StartInstance (CatPrf, 'Asset', pName1);
      RefNode (CatPrf, 'Asset.AssetInfo', pXfCd);
      pXf := ActiveCircuit.Transformers.First;
      while pXf <> nil do begin
        if pXf.XfmrCode = pXfCd.Name then
          RefNode (CatPrf, 'Asset.PowerSystemResources', pXf);
        pXf := ActiveCircuit.Transformers.Next;
      end;
      EndInstance (CatPrf, 'Asset');
      pXfCd := clsXfCd.ElementList.Next;
    end;

    // create all the banks (CIM PowerTransformer)
    maxWdg := 0;
    pXf := ActiveCircuit.Transformers.First;
    while pXf <> nil do begin
      if pXf.Enabled then
        if pXf.NumberOfWindings > maxWdg then maxWdg := pXf.NumberofWindings;
      pXf := ActiveCircuit.Transformers.Next;
    end;

    if MaxWdg>0 then  Begin
      SetLength (WdgList, maxWdg);
      SetLength (CoreList, maxWdg);
      SetLength (MeshList, (maxWdg-1)*maxWdg div 2);
      for i:=1 to maxWdg do WdgList[i-1]:=TNamedObject.Create('dummy');
      CoreList[0]:=TNamedObject.Create('dummy');
      for i:=1 to ((maxWdg-1)*maxWdg div 2) do MeshList[i-1]:=TNamedObject.Create('dummy');
    End;

    pXf := ActiveCircuit.Transformers.First;
    while pXf <> nil do begin
      if pXf.Enabled  then  Begin
        if pXf.XfmrBank = '' then
          sBank := '=' + pXf.Name
        else
          sBank := pXf.XfmrBank;
        pBank := GetBank (sBank);
        if pBank = nil then begin
          pBank := TBankObject.Create(maxWdg);
          pBank.localName := sBank;
          pBank.UUID := GetDevUuid (Bank, sBank, 0);
          AddBank (pBank);
        end;
      End;
      pXf := ActiveCircuit.Transformers.Next;
    end;

    // write all the transformers, according to the three cases
    pXf := ActiveCircuit.Transformers.First;
    while pXf <> nil do begin
      if pXf.Enabled then with pXf do begin
        // collect this transformer into tanks and banks, and make a location
        if pXf.XfmrBank = '' then
          sBank := '=' + pXf.Name
        else
          sBank := pXf.XfmrBank;
        bTanks := true;  // defaults to case 2 or 3 if XfmrCode exists
        if (length(pXf.XfmrCode) < 1) and (pXf.NPhases = 3) then
          bTanks := false; // case 1, balanced three-phase

        pBank := GetBank (sBank);
        pBank.AddTransformer (pXf);
        geoUUID := GetDevUuid (XfLoc, pXf.Name, 1);

        if bTanks then begin
          StartInstance (FunPrf, 'TransformerTank', pXf);
          CircuitNode (FunPrf, ActiveCircuit);
          RefNode (FunPrf, 'TransformerTank.PowerTransformer', pBank);
          UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
          EndInstance (FunPrf, 'TransformerTank');
          WritePositions (pXf, geoUUID, crsUUID);
        end else begin
          WritePositions (pXf, geoUUID, crsUUID);
        end;

        // make the winding, mesh and core name objects for easy reference
        for i:=1 to NumberOfWindings do begin
          WdgList[i-1].localName := pXf.Name + '_End_' + IntToStr(i);
          WdgList[i-1].UUID := GetDevUuid (Wdg, pXf.Name, i);
        end;
        CoreList[0].LocalName := pXf.Name + '_Yc';
        CoreList[0].UUID := GetDevUuid (XfCore, pXf.Name, 1);
        for i:=1 to ((maxWdg-1)*maxWdg div 2) do begin
          MeshList[i-1].localName := pXf.Name + '_Zsc_' + IntToStr(i);
          MeshList[i-1].UUID := GetDevUuid (XfMesh, pXf.Name, i);
        end;

        if not bTanks then begin // write the mesh impedances and core admittances
          val := BaseKVLL[1];
          zbase := 1000.0 * val * val / WdgKva[1];
          StartInstance (EpPrf, 'TransformerCoreAdmittance', CoreList[0]);
          val := pXf.noLoadLossPct / 100.0 / zbase;
          DoubleNode (EpPrf, 'TransformerCoreAdmittance.g', val);
          DoubleNode (EpPrf, 'TransformerCoreAdmittance.g0', val);
          val := pXf.imagPct / 100.0 / zbase;
          DoubleNode (EpPrf, 'TransformerCoreAdmittance.b', val);
          DoubleNode (EpPrf, 'TransformerCoreAdmittance.b0', val);
          RefNode (EpPrf, 'TransformerCoreAdmittance.TransformerEnd', WdgList[0]);
          EndInstance (EpPrf, 'TransformerCoreAdmittance');
          seq := 1; // write mesh Z
          for i:=1 to NumberOfWindings do begin
            for k := i+1 to NumberOfWindings do begin
              val := BaseKVLL[i];
              zbase := 1000.0 * val * val / WdgKva[i];
              StartInstance (EpPrf, 'TransformerMeshImpedance', MeshList[seq-1]);
              val := zbase * (WdgResistance[i] + WdgResistance[k]);
              DoubleNode (EpPrf, 'TransformerMeshImpedance.r', val);
              DoubleNode (EpPrf, 'TransformerMeshImpedance.r0', val);
              val := zbase * XscVal[seq];
              inc (seq);
              DoubleNode (EpPrf, 'TransformerMeshImpedance.x', val);
              DoubleNode (EpPrf, 'TransformerMeshImpedance.x0', val);
              RefNode (EpPrf, 'TransformerMeshImpedance.FromTransformerEnd', WdgList[i-1]);
              RefNode (EpPrf, 'TransformerMeshImpedance.ToTransformerEnd', WdgList[k-1]);
              EndInstance (EpPrf, 'TransformerMeshImpedance');
            end;
          end;
        end;

        // write the Ends, and a Terminal for each End
        for i:=1 to NumberOfWindings do begin
          if bTanks then begin
            StartInstance (FunPrf, 'TransformerTankEnd', WdgList[i-1]);
            XfmrTankPhasesAndGround (FunPrf, EpPrf, pXf, i);
            RefNode (FunPrf, 'TransformerTankEnd.TransformerTank', pXf);
          end else begin
            StartInstance (FunPrf, 'PowerTransformerEnd', WdgList[i-1]);
            RefNode (FunPrf, 'PowerTransformerEnd.PowerTransformer', pBank);
            DoubleNode (EpPrf, 'PowerTransformerEnd.ratedS', 1000 * WdgKva[i]);
            DoubleNode (EpPrf, 'PowerTransformerEnd.ratedU', 1000 * Winding^[i].kvll);
            zbase := 1000.0 * BaseKVLL[i] * BaseKVLL[i] / WdgKva[i];
            DoubleNode (EpPrf, 'PowerTransformerEnd.r', zbase * WdgResistance[i]);
            if Winding^[i].Connection = 1 then
              WindingConnectionKindNode (FunPrf, 'D')
            else
              if (Winding^[i].Rneut > 0.0) or (Winding^[i].Xneut > 0.0) then
                WindingConnectionKindNode (FunPrf, 'Yn')
              else
                WindingConnectionKindNode (FunPrf, 'Y');
            if Winding^[i].Connection <> Winding^[1].Connection then  // TODO - this assumes HV winding first, and normal usages
              IntegerNode (FunPrf, 'PowerTransformerEnd.phaseAngleClock', 1)
            else
              IntegerNode (FunPrf, 'PowerTransformerEnd.phaseAngleClock', 0);
            j := (i-1) * pXf.NConds + pXf.Nphases + 1;
            if (Winding^[i].Connection = 1) then begin // delta
              BooleanNode (FunPrf, 'TransformerEnd.grounded', false);
            end else if (pXf.NodeRef^[j] = 0) then begin // last conductor is grounded solidly
              BooleanNode (FunPrf, 'TransformerEnd.grounded', true);
              DoubleNode (EpPrf, 'TransformerEnd.rground', 0.0);
              DoubleNode (EpPrf, 'TransformerEnd.xground', 0.0);
            end else if (Winding^[i].Rneut < 0.0) then begin // probably wye ungrounded
              BooleanNode (FunPrf, 'TransformerEnd.grounded', false);
            end else begin // not delta, not wye solidly grounded or ungrounded
              BooleanNode (FunPrf, 'TransformerEnd.grounded', true);
              DoubleNode (EpPrf, 'TransformerEnd.rground', Winding^[i].Rneut);
              DoubleNode (EpPrf, 'TransformerEnd.xground', Winding^[i].Xneut);
            end;
          end;
          IntegerNode (FunPrf, 'TransformerEnd.endNumber', i);
          j := pXf.Terminals^[i].BusRef;
          pName2.LocalName := pXf.Name + '_T' + IntToStr (i);
          pName2.UUID := GetTermUuid (pXf, i);
          RefNode (FunPrf, 'TransformerEnd.Terminal', pName2);
          UuidNode (FunPrf, 'TransformerEnd.BaseVoltage', GetBaseVUuid (sqrt(3.0) * ActiveCircuit.Buses^[j].kVBase));
          if bTanks then
            EndInstance (FunPrf, 'TransformerTankEnd')
          else
            EndInstance (FunPrf, 'PowerTransformerEnd');
          // write the Terminal for this End
          StartInstance (FunPrf, 'Terminal', pName2);
          RefNode (FunPrf, 'Terminal.ConductingEquipment', pBank);
          IntegerNode (FunPrf, 'ACDCTerminal.sequenceNumber', i);
          FD.WriteCimLn (TopoPrf, Format('  <cim:Terminal.ConnectivityNode rdf:resource="#%s"/>',
            [ActiveCircuit.Buses[j].CIM_ID]));
          if i = 1 then begin   // write the current limit on HV winding, assuming that's winding 1
            LimitName := GetOpLimIName (pXf.NormAmps, pXf.EmergAmps);
            pILimit := GetOpLimit (LimitName);
            if pILimit = nil then begin
              pILimit := TOpLimitObject.Create(pXf.NormAmps, pXf.EmergAmps);
              pILimit.localName := LimitName;
              pILimit.UUID := GetDevUuid (OpLimI, LimitName, 0);
              AddOpLimit (pILimit);
            end;
            LimiTUuid := GetDevUuid (OpLimI, LimitName, 0);
            UuidNode (FunPrf, 'ACDCTerminal.OperationalLimitSet', LimiTUuid);
          end;
          EndInstance (FunPrf, 'Terminal');
        end;
      end;
      pXf := ActiveCircuit.Transformers.Next;
    end;

    // finally, write all the transformer banks (CIM PowerTransformer)
    for i:=Low(BankList) to High(BankList) do begin
      pBank := BankList[i];
      if pBank = nil then break;
      pBank.BuildVectorGroup;
      // we don't want = sign in the name.  These should still be unique names
      if AnsiPos ('=', pBank.localName) = 1 then
        pBank.localName := Copy(pBank.localName, 2, MaxInt);
      StartInstance (FunPrf, 'PowerTransformer', pBank);
      CircuitNode (FunPrf, ActiveCircuit);
      StringNode (FunPrf, 'PowerTransformer.vectorGroup', pBank.vectorGroup);
      UuidNode (GeoPrf, 'PowerSystemResource.Location',
        GetDevUuid (XfLoc, pBank.a_unit.Name, 1));
      EndInstance (FunPrf, 'PowerTransformer');
    end;

    WdgList:=nil;
    CoreList:=nil;
    MeshList:=nil;

    // voltage regulators
    pReg := ActiveCircuit.RegControls.First;
    while (pReg <> nil) do begin
      with pReg do begin
        pName1.LocalName := pReg.LocalName + '_Info';
        pName1.UUID := GetDevUuid (TapInfo, pReg.LocalName, 1);
        StartInstance (CatPrf, 'TapChangerInfo', pName1);
        DoubleNode (CatPrf, 'TapChangerInfo.ptRatio', PT);
        DoubleNode (CatPrf, 'TapChangerInfo.ctRatio', CT / 0.2);
        DoubleNode (CatPrf, 'TapChangerInfo.ctRating', CT);
        EndInstance (CatPrf, 'TapChangerInfo');

        pName2.LocalName := pReg.LocalName + '_Ctrl';
        pName2.UUID := GetDevUuid (TapCtrl, pReg.LocalName, 1);
        StartInstance (FunPrf, 'TapChangerControl', pName2);
        RegulatingControlEnum (FunPrf, 'voltage');
        UuidNode (FunPrf, 'RegulatingControl.Terminal', GetTermUuid (Transformer, TrWinding));
        MonitoredPhaseNode (FunPrf, FirstPhaseString (Transformer, TrWinding));
        BooleanNode (FunPrf, 'RegulatingControl.enabled', pReg.Enabled);
        BooleanNode (EpPrf, 'RegulatingControl.discrete', True);
        DoubleNode (EpPrf, 'RegulatingControl.targetValue', TargetVoltage);
        DoubleNode (EpPrf, 'RegulatingControl.targetDeadband', BandVoltage);
        BooleanNode (EpPrf, 'TapChangerControl.lineDropCompensation', UseLineDrop);
        DoubleNode (EpPrf, 'TapChangerControl.lineDropR', LineDropR);
        DoubleNode (EpPrf, 'TapChangerControl.lineDropX', LineDropX);
        if UseReverseDrop then begin
          DoubleNode (EpPrf, 'TapChangerControl.reverseLineDropR', RevLineDropR);
          DoubleNode (EpPrf, 'TapChangerControl.reverseLineDropX', RevLineDropX)
        end else begin
          DoubleNode (EpPrf, 'TapChangerControl.reverseLineDropR', 0.0);
          DoubleNode (EpPrf, 'TapChangerControl.reverseLineDropX', 0.0)
        end;
        if UseLimit then
          DoubleNode (EpPrf, 'TapChangerControl.limitVoltage', VoltageLimit)
        else
          DoubleNode (EpPrf, 'TapChangerControl.limitVoltage', 0.0);
        UuidNode (GeoPrf, 'PowerSystemResource.Location',
          GetDevUuid (XfLoc, Transformer.Name, 1));
        EndInstance (FunPrf, 'TapChangerControl');

        StartInstance (FunPrf, 'RatioTapChanger', pReg);
        UuidNode (FunPrf, 'RatioTapChanger.TransformerEnd',
          GetDevUuid (Wdg, Transformer.Name, TrWinding));
        UuidNode (FunPrf, 'TapChanger.TapChangerControl', pName2.UUID);
        DoubleNode (EpPrf, 'RatioTapChanger.stepVoltageIncrement', 100.0 * TapIncrement);
        TransformerControlEnum (FunPrf, 'volt');
        IntegerNode (EpPrf, 'TapChanger.highStep', NumTaps div 2);
        IntegerNode (EpPrf, 'TapChanger.lowStep', -NumTaps div 2);
        IntegerNode (EpPrf, 'TapChanger.neutralStep', 0);
        IntegerNode (EpPrf, 'TapChanger.normalStep', 0);
        DoubleNode (EpPrf, 'TapChanger.neutralU', 120.0 * PT);
        DoubleNode (EpPrf, 'TapChanger.initialDelay', InitialDelay);
        DoubleNode (EpPrf, 'TapChanger.subsequentDelay', SubsequentDelay);
        BooleanNode (EpPrf, 'TapChanger.ltcFlag', True);
        BooleanNode (SshPrf, 'TapChanger.controlEnabled', pReg.Enabled);
        DoubleNode (SshPrf, 'TapChanger.step', TapNum);
        UuidNode (GeoPrf, 'PowerSystemResource.Location',
          GetDevUuid (XfLoc, Transformer.Name, 1));
        EndInstance (FunPrf, 'RatioTapChanger');

        pName2.LocalName := 'TapChangerAsset_' + pReg.LocalName;
        pName2.UUID := GetDevUuid (TapAsset, pReg.LocalName, 1);
        StartInstance (CatPrf, 'Asset', pName2);
        RefNode (CatPrf, 'Asset.AssetInfo', pName1);
        RefNode (CatPrf, 'Asset.PowerSystemResources', pReg);
        EndInstance (CatPrf, 'Asset');
      end;
      pReg := ActiveCircuit.RegControls.Next;
    end;

    // done with the transformers

    // series reactors, exported as SeriesCompensators
    pReac := ActiveCircuit.Reactors.First;
    while pReac <> nil do begin
      if pReac.Enabled then begin
        StartInstance (FunPrf, 'SeriesCompensator', pReac);
        CircuitNode (FunPrf, ActiveCircuit);
        VbaseNode (FunPrf, pReac);
        geoUUID := GetDevUuid (ReacLoc, pReac.Name, 1);
        UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
        DoubleNode (EpPrf, 'SeriesCompensator.r', pReac.SimpleR);
        DoubleNode (EpPrf, 'SeriesCompensator.x', pReac.SimpleX);
        DoubleNode (EpPrf, 'SeriesCompensator.r0', pReac.SimpleR);
        DoubleNode (EpPrf, 'SeriesCompensator.x0', pReac.SimpleX);
        EndInstance (FunPrf, 'SeriesCompensator');
        // AttachLinePhases (F_, pReac); // for the 8500-node circuit, we only need 3 phase series reactors
        WriteTerminals (pReac, geoUUID, crsUUID, pReac.NormAmps, pReac.EmergAmps);
      end;
      pReac := ActiveCircuit.Reactors.Next;
    end;

    pLine := ActiveCircuit.Lines.First;
    while pLine <> nil do begin
      If pLine.Enabled Then
      With pLine do begin
        bval := False; // flag to write a "line code" of PULengthPhaseZ
        v1 := To_Meters (pLine.UserLengthUnits);
        geoUUID := GetDevUuid (LineLoc, pLine.Name, 1);
        if IsSwitch then begin
          ParseSwitchClass (pLine, swtCls, ratedAmps, breakingAmps);
          StartInstance (FunPrf, swtCls, pLine);
          CircuitNode (FunPrf, ActiveCircuit);
          VbaseNode (FunPrf, pLine);
          if breakingAmps > 0.0 then
            DoubleNode (EpPrf, 'ProtectedSwitch.breakingCapacity', breakingAmps); // Fuse and Sectionaliser don't have this, others do
          DoubleNode (EpPrf, 'Switch.ratedCurrent', ratedAmps);
          // some OpenDSS models have enabled=false to signal open switches, but we can't actually
          // export them because disabled elements don't have terminal references in memory
          if Enabled then begin
            BooleanNode (FunPrf, 'Switch.normalOpen', not pLine.Closed[0]);
            BooleanNode (SshPrf, 'Switch.open', not pLine.Closed[0]);
          end else begin
            BooleanNode (FunPrf, 'Switch.normalOpen', true);
            BooleanNode (SshPrf, 'Switch.open', true);
          end;
          BooleanNode (FunPrf, 'Switch.retained', True);
          UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
          EndInstance (FunPrf, swtCls);
          AttachSwitchPhases (pLine);
        end else begin
          StartInstance (FunPrf, 'ACLineSegment', pLine);
          CircuitNode (FunPrf, ActiveCircuit);
          VbaseNode (FunPrf, pLine);
          if LineCodeSpecified then begin
            DoubleNode (FunPrf, 'Conductor.length', Len * v1);
            LineCodeRefNode (EpPrf, clsLnCd, pLine.CondCode);
          end else if GeometrySpecified then begin
            DoubleNode (FunPrf, 'Conductor.length', Len * v1);
            LineSpacingRefNode (CatPrf, clsGeom, pLine.GeometryCode);
          end else if SpacingSpecified then begin
            DoubleNode (FunPrf, 'Conductor.length', Len * v1);
            LineSpacingRefNode (CatPrf, clsSpac, pLine.SpacingCode);
          end else begin
            if SymComponentsModel and (NPhases=3) then begin
              val := 1.0e-9 * TwoPi * BaseFrequency; // convert nF to mhos
              DoubleNode (FunPrf, 'Conductor.length', 1.0); // we don't know the physical length
              DoubleNode (EpPrf, 'ACLineSegment.r', Len * R1); // total ohms
              DoubleNode (EpPrf, 'ACLineSegment.x', Len * X1);
              DoubleNode (EpPrf, 'ACLineSegment.bch', Len * C1 * val);
              DoubleNode (EpPrf, 'ACLineSegment.gch', 0.0);
              DoubleNode (EpPrf, 'ACLineSegment.r0', Len * R0);
              DoubleNode (EpPrf, 'ACLineSegment.x0', Len * X0);
              DoubleNode (EpPrf, 'ACLineSegment.b0ch', Len * C0 * val);
              DoubleNode (EpPrf, 'ACLineSegment.b0ch', 0.0);
            end else begin
              bval := True;
              pName1.LocalName := pLine.Name + '_PUZ';
              pName1.UUID := GetDevUuid (PUZ, pLine.Name, 1);
              RefNode (EpPrf, 'ACLineSegment.PerLengthImpedance', pName1);
              // TODO - we no longer have proper length units if matrices were specified
              DoubleNode (FunPrf, 'Conductor.length', Len * v1);
            end;
          end;
          UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
          EndInstance (FunPrf, 'ACLineSegment');
          if not (SymComponentsModel and (NPhases=3)) then
            AttachLinePhases (pLine);
          if bVal = True then begin  // writing PuZ on the fly
            StartInstance (EpPrf, 'PerLengthPhaseImpedance', pName1);
            IntegerNode (EpPrf, 'PerLengthPhaseImpedance.conductorCount', NPhases);
            EndInstance (EpPrf, 'PerLengthPhaseImpedance');
            seq := 1;
            for i:= 1 to NPhases do begin
              for j:= 1 to i do begin
                StartFreeInstance (EpPrf, 'PhaseImpedanceData', GetDevUuid (ZData, pName1.LocalName, seq));
                RefNode (EpPrf, 'PhaseImpedanceData.PhaseImpedance', pName1);
                IntegerNode (EpPrf, 'PhaseImpedanceData.row', i);
                IntegerNode (EpPrf, 'PhaseImpedanceData.column', j);
                DoubleNode (EpPrf, 'PhaseImpedanceData.r', Z.GetElement(i,j).re / 1609.34);
                DoubleNode (EpPrf, 'PhaseImpedanceData.x', Z.GetElement(i,j).im / 1609.34);
                DoubleNode (EpPrf, 'PhaseImpedanceData.b', YC.GetElement(i,j).im / 1609.34);
                EndInstance (EpPrf, 'PhaseImpedanceData');
                inc (seq)
              end;
            end;
          end;
        end;
        WriteTerminals (pLine, geoUUID, crsUUID, pLine.NormAmps, pLine.EmergAmps);
      end;
      pLine := ActiveCircuit.Lines.Next;
    end;

    // create the DSS-like load models
    id1_ConstkVA := GetDevUuid (LoadResp, 'ConstkVA', 1);
    id2_ConstZ := GetDevUuid (LoadResp, 'ConstZ', 1);
    id3_ConstPQuadQ := GetDevUuid (LoadResp, 'ConstPQuadQ', 1);
    id4_LinPQuadQ := GetDevUuid (LoadResp, 'LinPQuadQ', 1);
    id5_ConstI := GetDevUuid (LoadResp, 'ConstI', 1);
    id6_ConstPConstQ := GetDevUuid (LoadResp, 'ConstQ', 1);  // P can vary, Q not
    id7_ConstPConstX := GetDevUuid (LoadResp, 'ConstX', 1);

    WriteLoadModel ('Constant kVA', id1_ConstkVA,
        0, 0, 100,
        0, 0, 100,
        0, 0);
    WriteLoadModel ('Constant Z', id2_ConstZ,
        100, 0, 0,
        100, 0, 0,
        0, 0);
    WriteLoadModel ('Motor', id3_ConstPQuadQ,
        0, 0, 100,
        100, 0, 0,
        0, 0);
    WriteLoadModel ('Mix Motor/Res', id4_LinPQuadQ,
        0, 0, 0,
        0, 0, 0,
        1, 2);
    WriteLoadModel ('Constant I', id5_ConstI,
        0, 100, 0,
        0, 100, 0,
        0, 0);
    WriteLoadModel ('Variable P, Fixed Q', id6_ConstPConstQ,
        0, 0, 100,
        0, 0, 100,
        0, 0);
    WriteLoadModel ('Variable P, Fixed X', id7_ConstPConstX,
        0, 0, 100,
        100, 0, 0,
        0, 0);

    pLoad := ActiveCircuit.Loads.First;
    while pLoad <> nil do begin
      if pLoad.Enabled then
        with pLoad do begin
          StartInstance (FunPrf, 'EnergyConsumer', pLoad);
          CircuitNode (FunPrf, ActiveCircuit);
          VbaseNode (FunPrf, pLoad);
          case FLoadModel of
            1: UuidNode (FunPrf, 'EnergyConsumer.LoadResponse', id1_ConstkVA);
            2: UuidNode (FunPrf, 'EnergyConsumer.LoadResponse', id2_ConstZ);
            3: UuidNode (FunPrf, 'EnergyConsumer.LoadResponse', id3_ConstPQuadQ);
            4: UuidNode (FunPrf, 'EnergyConsumer.LoadResponse', id4_LinPQuadQ);
            5: UuidNode (FunPrf, 'EnergyConsumer.LoadResponse', id5_ConstI);
            6: UuidNode (FunPrf, 'EnergyConsumer.LoadResponse', id6_ConstPConstQ);
            7: UuidNode (FunPrf, 'EnergyConsumer.LoadResponse', id7_ConstPConstX);
          end;
          DoubleNode (SshPrf, 'EnergyConsumer.p', 1000.0 * kWBase);
          DoubleNode (SshPrf, 'EnergyConsumer.q', 1000.0 * kvarBase);
          IntegerNode (FunPrf, 'EnergyConsumer.customerCount', NumCustomers);
          if Connection = 0 then begin
            ShuntConnectionKindNode (FunPrf, 'EnergyConsumer', 'Y');
            BooleanNode (FunPrf, 'EnergyConsumer.grounded', True);  // TODO - check bus 2
          end else begin
            ShuntConnectionKindNode (FunPrf, 'EnergyConsumer', 'D');
            BooleanNode (FunPrf, 'EnergyConsumer.grounded', False);
          end;
          geoUUID := GetDevUuid (LoadLoc, pLoad.Name, 1);
          UuidNode (GeoPrf, 'PowerSystemResource.Location', geoUUID);
          EndInstance (FunPrf, 'EnergyConsumer');
          AttachLoadPhases (pLoad, geoUUID);
          WriteTerminals (pLoad, geoUUID, crsUUID);
        end;
        pLoad := ActiveCircuit.Loads.Next;
    end;

    pLnCd := clsLnCd.ElementList.First;
    while pLnCd <> nil do begin
      with pLnCd do begin
        if pLnCd.Units = UNITS_NONE then begin // we need the real units for CIM
          pLine := ActiveCircuit.Lines.First;
          while pLine <> nil do begin
            If pLine.Enabled Then Begin
              if pLine.CondCode = pLnCd.LocalName then begin
                pLnCd.Units := pLine.UserLengthUnits;
//                writeln ('Setting Units on ' + pLnCd.LocalName + ' to ' + LineUnitsStr(pLnCd.Units));
                break;
              end;
            end;
            pLine := ActiveCircuit.Lines.Next;
          end;
        end;
        v1 := To_per_Meter (pLnCd.Units); // TODO: warn if still UNITS_NONE
        if SymComponentsModel and (NumPhases=3) then begin
          v2 := 1.0e-9 * TwoPi * BaseFrequency; // convert nF to mhos
          StartInstance (EpPrf, 'PerLengthSequenceImpedance', pLnCd);
          DoubleNode (EpPrf, 'PerLengthSequenceImpedance.r', R1 * v1);
          DoubleNode (EpPrf, 'PerLengthSequenceImpedance.x', X1 * v1);
          DoubleNode (EpPrf, 'PerLengthSequenceImpedance.bch', C1 * v1 * v2);
          DoubleNode (EpPrf, 'PerLengthSequenceImpedance.gch', 0.0);
          DoubleNode (EpPrf, 'PerLengthSequenceImpedance.r0', R0 * v1);
          DoubleNode (EpPrf, 'PerLengthSequenceImpedance.x0', X0 * v1);
          DoubleNode (EpPrf, 'PerLengthSequenceImpedance.b0ch', C0 * v1 * v2);
          DoubleNode (EpPrf, 'PerLengthSequenceImpedance.g0ch', 0.0);
          EndInstance (EpPrf, 'PerLengthSequenceImpedance')
        end else begin
          StartInstance (EpPrf, 'PerLengthPhaseImpedance', pLnCd);
          IntegerNode (EpPrf, 'PerLengthPhaseImpedance.conductorCount', FNPhases);
          EndInstance (EpPrf, 'PerLengthPhaseImpedance');
          seq := 1;
          for i:= 1 to FNPhases do begin
            for j:= 1 to i do begin
              StartFreeInstance (EpPrf, 'PhaseImpedanceData', GetDevUuid (ZData, pLnCd.LocalName, seq));
              RefNode (EpPrf, 'PhaseImpedanceData.PhaseImpedance', pLnCd);
              IntegerNode (EpPrf, 'PhaseImpedanceData.row', i);
              IntegerNode (EpPrf, 'PhaseImpedanceData.column', j);
              DoubleNode (EpPrf, 'PhaseImpedanceData.r', Z.GetElement(i,j).re * v1);
              DoubleNode (EpPrf, 'PhaseImpedanceData.x', Z.GetElement(i,j).im * v1);
              DoubleNode (EpPrf, 'PhaseImpedanceData.b', YC.GetElement(i,j).im * v1);
              EndInstance (EpPrf, 'PhaseImpedanceData');
              inc (seq)
            end;
          end;
        end;
      end;
      pLnCd := clsLnCd.ElementList.Next;
    end;

    pWire := clsWire.ElementList.First;
    while (pWire <> nil) do begin
      StartInstance (CatPrf, 'OverheadWireInfo', pWire);
      WriteWireData (pWire);
      BooleanNode (CatPrf, 'WireInfo.insulated', false);
      EndInstance (CatPrf, 'OverheadWireInfo');
      pWire := clsWire.ElementList.Next;
    end;

    pTape := clsTape.ElementList.First;
    while (pTape <> nil) do begin
      StartInstance (CatPrf, 'TapeShieldCableInfo', pTape);
      WriteWireData (pTape);
      WriteCableData (pTape);
      WriteTapeData (pTape);
      EndInstance (CatPrf, 'TapeShieldCableInfo');
      pTape := clsTape.ElementList.Next;
    end;

    pConc := clsConc.ElementList.First;
    while (pConc <> nil) do begin
      StartInstance (CatPrf, 'ConcentricNeutralCableInfo', pConc);
      WriteWireData (pConc);
      WriteCableData (pConc);
      WriteConcData (pConc);
      EndInstance (CatPrf, 'ConcentricNeutralCableInfo');
      pConc := clsConc.ElementList.Next;
    end;

    pGeom := clsGeom.ElementList.First;
    while pGeom <> nil do begin
      with pGeom do begin
        StartInstance (CatPrf, 'WireSpacingInfo', pGeom);
        ConductorUsageEnum (CatPrf, 'distribution');
        IntegerNode (CatPrf, 'WireSpacingInfo.phaseWireCount', 1);
        DoubleNode (CatPrf, 'WireSpacingInfo.phaseWireSpacing', 0.0);
        if PhaseChoice = Overhead then
          BooleanNode (CatPrf, 'WireSpacingInfo.isCable', False)
        else
          BooleanNode (CatPrf, 'WireSpacingInfo.isCable', True);
        EndInstance (CatPrf, 'WireSpacingInfo');

        for i := 1 to NWires do begin
          pName1.LocalName := 'WP_' + pGeom.Name + '_' + IntToStr(i);
          pName1.UUID := GetDevUuid (WirePos, pName1.LocalName, 1);  // 1 for pGeom
          StartInstance (CatPrf, 'WirePosition', pName1);
          RefNode (CatPrf, 'WirePosition.WireSpacingInfo', pGeom);
          IntegerNode (CatPrf, 'WirePosition.sequenceNumber', i);
          v1 := To_Meters (Units[i]);
          DoubleNode (CatPrf, 'WirePosition.xCoord', Xcoord[i] * v1);
          DoubleNode (CatPrf, 'WirePosition.yCoord', Ycoord[i] * v1);
          EndInstance (CatPrf, 'WirePosition')
        end;
      end;
      pGeom := clsGeom.ElementList.Next;
    end;

    pSpac := clsSpac.ElementList.First;
    while pSpac <> nil do begin
      with pSpac do begin
        v1 := To_Meters (Units);
        StartInstance (CatPrf, 'WireSpacingInfo', pSpac);
        ConductorUsageEnum (CatPrf, 'distribution');
        IntegerNode (CatPrf, 'WireSpacingInfo.phaseWireCount', 1);
        DoubleNode (CatPrf, 'WireSpacingInfo.phaseWireSpacing', 0.0);
        if pSpac.Ycoord[1] > 0.0 then
          BooleanNode (CatPrf, 'WireSpacingInfo.isCable', False)
        else
          BooleanNode (CatPrf, 'WireSpacingInfo.isCable', True);
        EndInstance (CatPrf, 'WireSpacingInfo');

        for i := 1 to NWires do begin
          pName1.LocalName := 'WP_' + pSpac.Name + '_' + IntToStr(i);
          pName1.UUID := GetDevUuid (WirePos, pName1.LocalName, 2); // 2 for pSpac
          StartInstance (CatPrf, 'WirePosition', pName1);
          RefNode (CatPrf, 'WirePosition.WireSpacingInfo', pSpac);
          IntegerNode (CatPrf, 'WirePosition.sequenceNumber', i);
          DoubleNode (CatPrf, 'WirePosition.xCoord', Xcoord[i] * v1);
          DoubleNode (CatPrf, 'WirePosition.yCoord', Ycoord[i] * v1);
          EndInstance (CatPrf, 'WirePosition')
        end;
      end;
      pSpac := clsSpac.ElementList.Next;
    end;

    // export the operational current limits that were created on-the-fly
    for i:=Low(OpLimitList) to High(OpLimitList) do begin
      pILimit := OpLimitList[i];
      if pILimit = nil then break;
      StartInstance (FunPrf, 'OperationalLimitSet', pILimit);
      EndInstance (FunPrf, 'OperationalLimitSet');
      pName1.LocalName := pILimit.LocalName + '_Norm';
      pName1.UUID := GetDevUuid (NormAmps, pILimit.LocalName, 1);
      StartInstance (FunPrf, 'CurrentLimit', pName1);
      RefNode (FunPrf, 'OperationalLimit.OperationalLimitSet', pILimit);
      RefNode (FunPrf, 'OperationalLimit.OperationalLimitType', pNormLimit);
      DoubleNode (FunPrf, 'CurrentLimit.value', pILimit.NormAmps);
      EndInstance (FunPrf, 'CurrentLimit');
      pName2.LocalName := pILimit.LocalName + '_Emerg';
      pName2.UUID := GetDevUuid (EmergAmps, pILimit.LocalName, 1);
      StartInstance (FunPrf, 'CurrentLimit', pName2);
      RefNode (FunPrf, 'OperationalLimit.OperationalLimitSet', pILimit);
      RefNode (FunPrf, 'OperationalLimit.OperationalLimitType', pEmergLimit);
      DoubleNode (FunPrf, 'CurrentLimit.value', pILimit.EmergAmps);
      EndInstance (FunPrf, 'CurrentLimit');
    end;

    pName1.Free;
    pName2.Free;

//    FreeUuidList;  // this is deferred for UUID export
    FreeBankList;
    FreeOpLimitList;

    GlobalResult := FileNm;
  Finally
    FD.Free;
  End;
End;

end.

