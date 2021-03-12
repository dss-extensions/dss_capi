{
 ----------------------------------------------------------
  Copyright (c) 2017-2020 Battelle Memorial Institute
 ----------------------------------------------------------
}
unit HELICS;

{$mode delphi}
{$MACRO ON}
{$IFDEF Windows}
{$DEFINE HELICS_CALL:=stdcall}
//{$DEFINE HELICS_CALL:=cdecl}
{$ELSE} // Darwin and Unix
{$DEFINE HELICS_CALL:=cdecl}
{$ENDIF}
interface

uses
  Classes, SysUtils, Executive, {$IFDEF Unix} unix, {$ENDIF} dynlibs, DSSGlobals,
  UComplex, CktElement, Utilities, math, Dos;

type
  helics_time = Double;
  helics_federate = Pointer;
  helics_input = Pointer;
  helics_publication = Pointer;
  helics_endpoint = Pointer;
  helics_bool = Boolean;
  //helics_version = Pointer;

  // lists for HELICS classic publications
  THELICSClass = (helicsBus, helicsLine, helicsSwitch, helicsCapacitor, helicsPVSystem,
                helicsVSource, helicsTransformer, helicsFault, helicsStorage, helicsNoClass);
  THELICSAttribute = (helicsVoltage, helicsCurrent, helicsPower, helicsSwitchState,
                helicsTapPosition, helicsEnergy, helicsNoAttribute);
  THELICSLogLevel = (helicsLogWarning, helicsLogInfo, helicsLogDebug1, helicsLogDebug2,
                helicsLogDebug3, helicsLogDebug4);
  THELICSPublishMode = (helicsPublishJSON, helicsPublishText);
  THELICSSubTopic = class (TObject)
  public
    tag: String;            // what HELICS calls it
    text_key: String;       // in Text-mode export we need to include the higher-level topic tag, and cache it
    att: THELICSAttribute;
    trm: Integer;           // terminal number
    ref: Integer;           // index into the phases
    constructor Create(attKey, trmKey, phsKey: String; idxRoot: Integer);
  end;
  THELICSTopic = class (TObject)
  public
    tag: String;            // what HELICS calls it
    dss: String;            // what OpenDSS calls it
    cls: THELICSClass;
    idx: Integer;           // index into BusList or DeviceList
    sub: TList;             // list of attribute subtopics
    constructor Create (clsKey, objKey: String);
  end;
  THELICSERROR = record
    error_code: Integer;
    message: Pchar;
  end;
  TPHELICSERROR = ^THELICSERROR;
  THELICSMESSAGE = record
    time: helics_time;
    data: PChar;
    length: qword;
    messageID: Integer;
    flags: word;
    original_source: Pchar;
    source: Pchar;
    dest: Pchar;
    original_dest: Pchar;
  end;

  THELICS = class(TObject)
  private
    FLibHandle: TLibHandle;
    FuncError: Boolean;
    helics_create_value_fed: procedure;HELICS_CALL;
    // Create federate, Connect to broker and parse config file.
    helics_create_value_fed_config: function (configFile:Pchar; error:Pointer):helics_federate;HELICS_CALL;
    helics_create_message_fed_config: function (configFile:Pchar; error:Pointer):helics_federate;HELICS_CALL;
    helics_create_combination_fed_config: function (configFile:Pchar; error:Pointer):helics_federate;HELICS_CALL;
    // federate enter intialization mode
    helics_enter_initialize_mode: procedure (helicsFed:helics_federate; error:Pointer);HELICS_CALL;
    // federate enter execution mode
    helics_enter_executing_mode: procedure (helicsFed:helics_federate; error:Pointer);HELICS_CALL;

    helics_get_publication_count: function (helicsFed:helics_federate):Integer;HELICS_CALL;
    helics_get_subscription_count: function (helicsFed:helics_federate):Integer;HELICS_CALL;
    helics_get_endpoint_count: function (helicsFed:helics_federate):Integer;HELICS_CALL;
    helics_fed_get_pub_by_index: function (helicsFed:helics_federate; index:Integer; error:Pointer):helics_publication;HELICS_CALL;
    helics_fed_get_input_by_index: function (helicsFed:helics_federate; index:Integer; error:Pointer):helics_input;HELICS_CALL;
    helics_fed_get_endpoints_by_index: function (helicsFed:helics_federate; index:Integer; error:Pointer):helics_endpoint;HELICS_CALL;

    helics_input_is_valid: function(input:helics_input):helics_bool;HELICS_CALL;
    helics_input_is_updated: function(input:helics_input):helics_bool;HELICS_CALL;
    helics_endpoint_is_valid: function(input:helics_endpoint):helics_bool;HELICS_CALL; 
    helics_endpoint_has_message: function(input:helics_endpoint):helics_bool;HELICS_CALL;
    // Request the next time step to process.
    helics_time_request: function (helicsFed:helics_federate; next:helics_time; error:Pointer):helics_time;HELICS_CALL;
    helics_fed_get_current_time: function (helicsFed:helics_federate; error:Pointer):helics_time;HELICS_CALL;
    helics_time_request_iter: function (helicsFed:helics_federate; time:helics_time; iterate:Integer; result:Pointer; error:Pointer):helics_time;HELICS_CALL;
    helics_time_request_iter_complete: function (helicsFed:helics_federate; result:Pointer; error:Pointer):helics_time;HELICS_CALL;

    //// Publish value using the given key.
    //helics_publish: procedure (key:Pchar; value:Pchar);HELICS_CALL;
    //// Publish value anonymously using the given key.
    //helics_publish_anon: procedure (key:Pchar; value:Pchar);HELICS_CALL;
    //// Publish function for transactive agents.
    //helics_agentPublish: procedure (value:Pchar);HELICS_CALL;
    //// Publish value using the given key, adding from:to into the key.
    //helics_route: procedure (source:Pchar; target:Pchar; key:Pchar; value:Pchar);HELICS_CALL;
    //// Tell broker of a fatal client error.
    //helics_die: procedure;HELICS_CALL;

    // Close the connection to the broker.
    helics_finalize: procedure (helicsFed:helics_federate; error:Pointer);HELICS_CALL;

    //// Update minimum time delta after connection to broker is made. Assumes time unit is not changing.
    //helics_update_time_delta: procedure (delta:helics_time);HELICS_CALL;
    //// Get the number of keys for all values that were updated during the last time_request.
    //helics_get_events_size: function:size_t;HELICS_CALL;
    //// Get the keys for all values that were updated during the last time_request.
    //helics_get_events: function:ppchar;HELICS_CALL;
    //// Get one key for the given event index that as updated during the last time_request.
    //helics_get_event_at: function (index:size_t):pchar;HELICS_CALL;
    //// Get the agent events for all values that were updated during the last time_request.
    //helics_agentGetEvents: function:pchar;HELICS_CALL;


    //// Get the number of values from the cache with the given key.
    //helics_get_values_size: function (key:Pchar):size_t;HELICS_CALL;
    //// Get an array of values from the cache with the given key. Will return an array of size 1 if only a single value exists.
    //helics_get_values: function (key:Pchar):ppchar;HELICS_CALL;
    //// Get a single value from the array of values for the given key.
    //helics_get_value_at: function (key:Pchar; index:size_t):pchar;HELICS_CALL;
    //// Get the number of subscribed keys.
    //helics_get_keys_size: function:size_t;HELICS_CALL;
    //// Get the subscribed keys. Will return NULL if helics_get_keys_size() returns 0.
    //helics_get_keys: function:ppchar;HELICS_CALL;
    //// Get the subscribed key at the given index. Will return NULL if helics_get_keys_size() returns 0.
    //helics_get_key_at: function (index:size_t):pchar;HELICS_CALL;
    //// Return the name of the simulator.
    //helics_get_name: function:pchar;HELICS_CALL;
    //// Return a unique numeric ID for the simulator.
    //helics_get_id: function:longint;HELICS_CALL;
    //// Return the number of simulators connected to the broker.
    //helics_get_simulator_count: function:longint;HELICS_CALL;
    //// Run-time API version detection.
    helics_get_version: function ():PChar;HELICS_CALL;

    helics_close_library: procedure ();HELICS_CALL;

    //helics_count_events: function:size_t;HELICS_CALL;
    //helics_count_values: function (key:Pchar):size_t;HELICS_CALL;
    helics_publication_get_key: function(pub:helics_publication):pchar;HELICS_CALL;
    helics_publication_get_info: function(pub:helics_publication):pchar;HELICS_CALL;                            
    helics_publication_get_type: function(pub:helics_publication):pchar;HELICS_CALL;
    helics_publication_publish_string: procedure(pub:helics_publication; str:Pchar; error:Pointer);HELICS_CALL;
    helics_publication_publish_int: procedure(pub:helics_publication; val:Integer; error:Pointer);HELICS_CALL;
    helics_publication_publish_boolean: procedure(pub:helics_publication; val:helics_bool; error:Pointer);HELICS_CALL;
    helics_publication_publish_double: procedure(pub:helics_publication; val:double; error:Pointer);HELICS_CALL;
    helics_publication_publish_complex: procedure(pub:helics_publication; real:double; imag:double; error:Pointer);HELICS_CALL;



    helics_input_get_key: function(input:helics_input):pchar;HELICS_CALL;
    helics_input_get_type: function(input:helics_input):pchar;HELICS_CALL;
    helics_input_get_integer: function(input:helics_input; error:Pointer):integer;HELICS_CALL;
    helics_input_get_boolean: function(input:helics_input; error:Pointer):helics_bool;HELICS_CALL;
    helics_input_get_double: function(input:helics_input; error:Pointer):Double;HELICS_CALL;
    helics_input_get_string: function(input:helics_input; output:Pchar; maxLength:Integer; actualLength:PInteger ;error:Pointer):pchar;HELICS_CALL;
    helics_input_get_string_size: function(input:helics_input):Integer;HELICS_CALL;
    helics_input_get_complex: function(input:helics_input; real:Pointer; imag:Pointer; error:Pointer):Double;HELICS_CALL;
    helics_endpoint_get_message: function(endpoint:helics_endpoint):THELICSMESSAGE;HELICS_CALL;
    helics_fed_get_message: function(helicsFed:helics_federate):THELICSMESSAGE;HELICS_CALL;


  private
    in_helics_loop:Boolean;
    next_helics_publish: helics_time;
    existing_helics_grant: helics_time;
    ETHelicsReadPub: Extended;
    ETHelicsPublish: Extended;
    ETHelicsLoad: Extended;
    ETHelicsTimeRequest: Extended;
    ETHelicsGetEvents: Extended;
    log_level: THELICSLogLevel;
    topicList: TList;
    helicsOutputStream: TStringStream;
    helicsFed: helics_federate;
    pub_list, sub_list, endpoint_list: TFPList;
    pub_count, sub_count, endpoint_count: integer;
    opendssSub: helics_input;
    opendssPub: helics_publication;
    pubJsonStr: pchar;
    procedure ReadHELICSJsonConfig (fname: string);
    procedure ReadHELICSTextConfig (fname: string);
    procedure ReadHelicsJsonString (jsonStr: pchar);
    procedure SetPublishInterval (val: Integer);
    procedure SetPublishMode (val: string);
    function find_helics_function (name: String): Pointer;
    function helics_get_input_string (input:helics_input; error:Pointer):pchar;
    function parse_key(key:Pchar):string;

  public
    PublishInterval:Integer;
    PublishMode:THELICSPublishMode;
    FedName:string;

    function IsReady:Boolean;
    function IsRunning:Boolean;
    procedure RunHELICSLoop (const s:string);
    constructor Create();
    destructor Destroy; override;
    function HelicsTimeRequest (next_helics:helics_time):Boolean; 
    procedure ReadHELICSPubConfigFile (fname: string);
    procedure ReadHELICSPubConfig;
    procedure TopicsListToPublication;
    procedure DumpHELICSLists;
  end;


var
  ActiveHELICS:THELICS;

implementation

uses
  fpjson, jsonparser, jsonscanner, strutils, Transformer, Load, Storage, EpikTimer; // RegControl,ControlElem;

var
  ET: TEpikTimer; // for profiling
  sep: string;    // for delimiting tokens in a FNCS topic key; this is always '.' for DSS

constructor THELICSTopic.Create (clsKey, objKey: String);
begin
  dss := clsKey + '.' + objKey;
  tag := clsKey + sep + objKey;
  if clsKey = 'bus' then
    cls := helicsBus
  else if clsKey = 'line' then
    cls := helicsLine
  else if clsKey = 'switch' then
    cls := helicsSwitch
  else if clsKey = 'capacitor' then
    cls := helicsCapacitor
  else if clsKey = 'pvsystem' then
    cls := helicsPVSystem
  else if clsKey = 'storage' then
    cls := helicsStorage
  else if clsKey = 'vsource' then
    cls := helicsVSource
  else if clsKey = 'transformer' then
    cls := helicsTransformer
  else if clsKey = 'fault' then
    cls := helicsFault
  else
    cls := helicsNoClass;
  if cls = helicsBus then
    idx := ActiveCircuit.BusList.Find (objKey)
  else
    idx := ActiveCircuit.SetElementActive (dss);
  if idx = 0 then writeln ('*** can not find HELICS output for ' + dss);
  sub := TList.Create();
end;

constructor THELICSSubTopic.Create (attKey, trmKey, phsKey: String; idxRoot: Integer);
var
  idxPhs, idxLoc: Integer;
  pElem :TDSSCktElement;
  Ncond, Nterm, kmax, k: Integer;
  PhaseTable: array[1..2, 0..3] of Integer; // index into cBuffer by terminal, then phase
begin
  trm := StrToInt (trmKey);
  if attKey = 'voltage' then
    att := helicsVoltage
  else if attKey = 'current' then
    att := helicsCurrent
  else if attKey = 'power' then
    att := helicsPower
  else if attKey = 'switchstate' then
    att := helicsSwitchState
  else if attKey = 'tapposition' then
    att := helicsTapPosition
  else if attKey = 'kwhstored' then
    att := helicsEnergy
  else
    att := helicsNoAttribute;
  if trm > 0 then
    if att = helicsVoltage then
      tag := attKey + sep + phsKey
    else
      tag := attKey + sep + trmKey + sep + phsKey
  else
    tag := attKey;
  ref := 0;
  if idxRoot <= 0 then begin
    att := helicsNoAttribute;
    exit;
  end;
  if att = helicsVoltage then begin
    idxPhs := 1 + Ord(phsKey[1]) - Ord('A');  // TODO: s1 and s2; can't ask for ground or neutral voltage
    idxLoc := ActiveCircuit.Buses^[idxRoot].FindIdx(idxPhs);
    ref := ActiveCircuit.Buses^[idxRoot].GetRef(idxLoc);
  end else begin
    if (trm > 0) then begin
      idxPhs := 1 + Ord(phsKey[1]) - Ord('A');  // TODO: s1 and s2; can't ask for ground or neutral voltage
      pElem := ActiveCircuit.CktElements.Get(idxRoot);
      NCond := pElem.NConds;
      Nterm := pElem.Nterms;
      kmax := Ncond * Nterm;
      for k :=  0 to 3 do begin
        PhaseTable[1, k] := 0;
        PhaseTable[2, k] := 0;
      end;
      for k := 1 to kmax do begin
        idxLoc := GetNodeNum (pElem.NodeRef^[k]);
        if k > Ncond then
          PhaseTable[2, idxLoc] := k
        else
          PhaseTable[1, idxLoc] := k;
      end;
      if (idxPhs < 1) or (idxPhs > 3) then idxPhs := 0;
      ref := PhaseTable[trm, idxPhs];
    end else
      ref := 0;
  end;
end;

FUNCTION  InterpretStopTimeForHELICS(const s:string):helics_time;
Var
  Code :Integer;
  ch :char;
  s2 :String;
Begin
  {Adapted from InterpretTimeStepSize}
  val(s,Result, Code);
  If Code = 0 then Exit; // Only a number was specified, so must be seconds

  {Error occurred so must have a units specifier}
  ch := s[Length(s)];  // get last character
  s2 := copy(s, 1, Length(s)-1);
  Val(S2, Result, Code);
  If Code>0 then Begin
    writeln('Error in FNCS Stop Time: ' + s);
    Exit;
  End;
  case ch of
    'd': Result := Result * 86400;
    'h': Result := Result * 3600;
    'm': Result := Result * 60;
    's':; // still seconds
  Else
    writeln('Error in FNCS Stop Time: "' + s +'" Units can only be d, h, m, or s (single char only)');
  end;
End;

procedure THELICS.SetPublishInterval (val: Integer);
begin
  if val > 0 then PublishInterval := val;
end;

procedure THELICS.SetPublishMode (val: string);
var
  tok: string;
begin
  tok := LowerCase(val);
  if tok = 'json' then
    PublishMode := helicsPublishJSON
  else if tok = 'text' then
    PublishMode := helicsPublishText;
end;

// for performance reasons, we avoid concatenating strings or calling the Format function here
procedure THELICS.TopicsListToPublication;
var
  top: THELICSTopic;
  sub: THELICSSubTopic;
  Flow, Volts: Complex;
  sign, pubtype: String;
  pElem :TDSSCktElement;
  pXf: TTransfObj;
  pStore: TStorageObj;
  cBuffer :pComplexArray;
  k, kmax, idxWdg, tap, ival: integer;
  key, val, pubkey: PChar;
  firstObjectFlag:Boolean=true;
  writeKeyComma:Boolean=false;
  pos1,pos2,i:Int64;
  helics_error: THELICSERROR = (error_code:0; message:nil);
  apub: helics_publication;
  dval, real, imag: double;
begin
  ET.Clear;
  ET.Start;
  if log_level >= helicsLogDebug3 then writeln ('Entering TopicsListToPublication');
  if PublishMode = helicsPublishJSON then begin
    pos1 := helicsOutputStream.Position;
    helicsOutputStream.Seek (0, soFromBeginning);
//    helicsOutputStream.WriteString ('{"'+FedName+'":{');
    helicsOutputStream.WriteString ('{"');
    helicsOutputStream.WriteString (FedName);
    helicsOutputStream.WriteString ('":{');
  end;
  kmax := GetMaxCktElementSize;
  Getmem(cBuffer, sizeof(cBuffer^[1])*kmax);
  for k := 1 to kmax do begin
    cBuffer^[k].re := 0.0;
    cBuffer^[k].im := 0.0;
  end;
  for top in topicList do begin
    if PublishMode = helicsPublishJSON then begin
      if not firstObjectFlag then helicsOutputStream.WriteString (',');
//      helicsOutputStream.WriteString (Format('"%s":{', [top.tag]));
      helicsOutputStream.WriteString ('"');
      helicsOutputStream.WriteString (top.tag);
      helicsOutputStream.WriteString ('":{');
      firstObjectFlag := False;
    end;
    for sub in top.sub do begin
      if PublishMode = helicsPublishJSON then
        key := PChar (sub.tag)
      else
        key := PChar (sub.text_key);
      val := nil;
      if sub.att = helicsVoltage then begin
        Volts := ActiveCircuit.Solution.NodeV^[sub.ref];
        if Volts.im < 0 then
          sign:='-'
        else
          sign:='+';
        real := Volts.re;
        imag := Volts.im;
        val := PChar (FloatToStrF(real, ffFixed, 0, 3) + sign + FloatToStrF(abs(imag), ffFixed, 0, 3) + 'j');
      end else if (sub.att = helicsCurrent) or (sub.att = helicsPower) then begin
        pElem := ActiveCircuit.CktElements.Get(top.idx);
        pElem.GetCurrents(cBuffer);
        if (sub.att = helicsCurrent) then begin
          Flow := cBuffer^[sub.ref];
        end else begin
          Volts := ActiveCircuit.Solution.NodeV^[pElem.NodeRef^[sub.ref]];
          Flow:=Cmul(Volts, conjg(cBuffer^[sub.ref]));
          if ActiveCircuit.PositiveSequence then Flow:=CmulReal(Flow, 3.0);
        end;
        if Flow.im < 0 then
          sign:='-'
        else
          sign:='+';
        real := Flow.re;
        imag := Flow.im;
        val := PChar (FloatToStrF(real, ffFixed, 0, 3) + sign + FloatToStrF(abs(imag), ffFixed, 0, 3) + 'j');
      end else if (sub.att = helicsSwitchState) then begin
        pElem := ActiveCircuit.CktElements.Get(top.idx);
        if AllTerminalsClosed (pElem) then
          ival := 1
        else
          ival := 0;
        val := PChar (IntToStr (ival));
      end else if (sub.att = helicsTapPosition) then begin
        pXf := TTransfObj (ActiveCircuit.CktElements.Get(top.idx));
        idxWdg := 2; // TODO: identify and map this using pReg.Transformer and pReg.TrWinding
        tap := Round((pXf.PresentTap[idxWdg]-(pXf.Maxtap[idxWdg]+pXf.Mintap[idxWdg])/2.0)/pXf.TapIncrement[idxWdg]);
        ival := tap;
        val := PChar (IntToStr (tap));
      end else if (sub.att = helicsEnergy) then begin
        pStore := TStorageObj (ActiveCircuit.CktElements.Get(top.idx));
        dval := pStore.StorageVars.kwhStored;
        val := PChar (FloatToStrF(dval, ffFixed, 0, 3));
      end;
      if assigned(val) then begin
        if PublishMode = helicsPublishJSON then begin
          if writeKeyComma then helicsOutputStream.WriteString (',');
          writeKeyComma := True;
//          fncsOutputStream.WriteString (Format ('"%s":"%s"', [key, val]));
          helicsOutputStream.WriteString ('"');
          helicsOutputStream.WriteString (key);
          helicsOutputStream.WriteString ('":"');
          helicsOutputStream.WriteString (val);
          helicsOutputStream.WriteString ('"');
        end else begin
          for i := 0 to pub_count - 1 do begin
              apub := pub_list.Items[i];
              pubkey := Pchar(parse_key(helics_publication_get_key(apub)));
              if CompareText (pubkey, key) = 0 then begin
                 pubtype := StrPas(helics_publication_get_type(apub));
                 if CompareText(pubtype, 'double') = 0 then helics_publication_publish_double(apub, dval, @helics_error); 
                 if CompareText(pubtype, 'complex') = 0 then helics_publication_publish_complex(apub, real, imag, @helics_error);
                 if CompareText(pubtype, 'string') = 0 then helics_publication_publish_string(apub, val, @helics_error);
                 if pos('int', pubtype) > 0 then helics_publication_publish_int(apub, ival, @helics_error);
              end;
          end;
          //helics_publish (key, val);
          if log_level >= helicsLogDebug3 then writeln(Format ('Publish %s = %s', [key, val]));
        end;
      end;
    end;
    if PublishMode = helicsPublishJSON then begin
      helicsOutputStream.WriteString ('}');
      writeKeyComma:=False;
    end;
  end;
  if PublishMode = helicsPublishJSON then begin
    helicsOutputStream.WriteString ('}}');
    pos2 := helicsOutputStream.Position;
    if pos2 < pos1 then
      for i := 1 to (pos1 - pos2) do
        helicsOutputStream.WriteString (' ');
    for i := 0 to pub_count - 1 do begin
        apub := pub_list.Items[i];
        pubkey := Pchar(parse_key(helics_publication_get_key(apub)));
        helics_publication_publish_string (apub, PChar(helicsOutputStream.DataString), @helics_error);
        //if CompareText (pubkey, key) = 0 then begin
        //   pubtype := StrPas(helics_publication_get_type(apub));
        //   if CompareText(pubtype, 'double') = 0 then helics_publication_publish_double(apub, dval, @helics_error);
        //   if CompareText(pubtype, 'complex') = 0 then helics_publication_publish_complex(apub, real, imag, @helics_error);
        //   if CompareText(pubtype, 'string') = 0 then helics_publication_publish_string(apub, val, @helics_error);
        //   if pos('int', pubtype) > 0 then helics_publication_publish_int(apub, ival, @helics_error);
        //end;
    end;
    //helics_publication_publish_string (opendssPub, PChar(helicsOutputStream.DataString), @helics_error);
  end;
  ETHelicsPublish := ETHelicsPublish + ET.Elapsed;
  ET.Clear;
end;

procedure THELICS.DumpHELICSLists;
var
  top: THELICSTopic;
  sub: THELICSSubTopic;
begin
  writeln('***DumpHELICSLists');
  for top in topicList do begin
    writeln(Format('  %2d %5d %s',[top.cls, top.idx, top.tag]));
    for sub in top.sub do begin
      writeln(Format('    %2d %2d %5d %s',[sub.att, sub.trm, sub.ref, sub.tag]));
    end;
  end;
end;

procedure THELICS.ReadHelicsPubConfigFile (fname: string);
var
  buf: String;
begin
  ET.Clear;
  ET.Start;
  buf := '   ';
  helicsOutputStream:=TStringStream.Create(buf);
  next_helics_publish := 0;

  if Pos ('.json', ExtractFileExt (LowerCase (fname))) > 0 then
    ReadHelicsJsonConfig (fname)
  else
    ReadHelicsTextConfig (fname);
  if log_level >= helicsLogInfo then begin
    Writeln('  Exiting ReadHelicsPubConfig');
    system.flush (stdout);
  end;
  ETHelicsReadPub := ETHelicsReadPub + ET.Elapsed;
  ET.Clear;
end;


procedure THELICS.ReadHelicsPubConfig ();
var
  buf: String;
begin
  ET.Clear;
  ET.Start;
  buf := '   ';
  helicsOutputStream:=TStringStream.Create(buf);
  next_helics_publish := 0;
  
  ReadHelicsJsonString(pubJsonStr);
  //if Pos ('.json', ExtractFileExt (LowerCase (fname))) > 0 then
  //  ReadHelicsJsonConfig (fname)
  //else
  //  ReadHelicsTextConfig (fname);
  if log_level >= helicsLogInfo then begin
    Writeln('  Exiting ReadHelicsPubConfig');
    system.flush (stdout);
  end;
  ETHelicsReadPub := ETHelicsReadPub + ET.Elapsed;
  ET.Clear;
end;

procedure THELICS.ReadHelicsTextConfig (fname: string);
var
  lines: TStringList;
  i: Integer;
begin
  try
    lines := TStringList.Create;
    lines.LoadFromFile (fname);
    for i := 1 to lines.Count do begin
      writeln(lines[i-1]);
    end;
    system.flush (stdout);
  finally
    if log_level >= helicsLogInfo then begin
      Writeln(Format('  ReadHelicsTextConfig processed %u lines', [lines.count]));
      system.flush (stdout);
    end;
    lines.free;
  end;
  if log_level >= helicsLogInfo then begin
    Writeln('  Exiting ReadHelicsTextConfig');
    system.flush (stdout);
  end;
end;

procedure THELICS.ReadHelicsJsonConfig (fname: string);
var
  inputfile:TFileStream;
  parser:TJSONParser;
  config:TJSONData;
  el,attri,cls,obj,terminal,conductor:TJSONEnum;
  attriKey, clsKey, objKey, terminalKey, condKey:string;
  top: THELICSTopic;
  sub: THELICSSubTopic;
begin
  inputfile:=TFileStream.Create(fname, fmOpenRead);
  try
    parser:=TJSONParser.Create(inputfile, [joUTF8]);
    try
      config:=parser.Parse;
      // build the lists
      for el in config do begin
        if el.Key = 'name' then
          FedName:=el.Value.AsString
        else if el.Key = 'publishInterval' then
          SetPublishInterval (el.Value.AsInteger)
        else if el.Key = 'publishMode' then
          SetPublishMode (el.Value.AsString)
        else if el.Key = 'topics' then begin
          for cls in el.Value do begin
            clsKey:=LowerCase(cls.Key);
            for obj in cls.Value do begin
              objKey:=LowerCase(obj.Key);
              top := THELICSTopic.Create (clsKey, objKey);
              topicList.Add(top);
              for attri in obj.Value do begin
                attriKey := LowerCase(attri.Key);
                if attri.Value is Tjsonarray then begin
                  if attri.Value.Count=0 then begin
                    sub := THELICSSubTopic.Create (attriKey, '-1', '', top.idx); // switchstate, tapposition, etc.
                    top.sub.Add(sub);
                  end else
                    for conductor in attri.Value do begin
                      condKey := conductor.Value.asstring;
                      sub := THELICSSubTopic.Create (attriKey, '1', condKey, top.idx);
                      top.sub.Add(sub);
                    end;
                end else begin  // attri.Value is not a TJSONArray
                  for terminal in attri.Value do begin
                    terminalKey:=LowerCase(terminal.Key);
                    for conductor in terminal.Value do begin
                      condKey := conductor.Value.asstring;
                      sub := THELICSSubTopic.Create (attriKey, terminalKey, condKey, top.idx);
                      top.sub.Add(sub);
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    finally
      parser.Free;
    end;
  finally
    inputfile.Free;
  end;

  for top in topicList do
    for sub in top.sub do
      sub.text_key := Format ('%s%s%s',[top.tag, sep, sub.tag]);

  if log_level >= helicsLogInfo then
    writeln('Done reading HELICS publication requests from: ' + fname);
  if log_level >= helicsLogDebug1 then begin
    DumpHELICSLists;
  end;
end;

procedure THELICS.ReadHelicsJsonString (jsonStr: pchar);
var
  parser:TJSONParser;
  config:TJSONData;
  el,attri,cls,obj,terminal,conductor:TJSONEnum;
  attriKey, clsKey, objKey, terminalKey, condKey:string;
  top: THELICSTopic;
  sub: THELICSSubTopic;
begin
    parser:=TJSONParser.Create(StrPas(jsonStr), [joUTF8]);
    try
      config:=parser.Parse;
      // build the lists
      for el in config do begin
        if el.Key = 'name' then
          FedName:=el.Value.AsString
        else if el.Key = 'publishInterval' then
          SetPublishInterval (el.Value.AsInteger)
        else if el.Key = 'publishMode' then
          SetPublishMode (el.Value.AsString)
        else if el.Key = 'topics' then begin
          for cls in el.Value do begin
            clsKey:=LowerCase(cls.Key);
            for obj in cls.Value do begin
              objKey:=LowerCase(obj.Key);
              top := THELICSTopic.Create (clsKey, objKey);
              topicList.Add(top);
              for attri in obj.Value do begin
                attriKey := LowerCase(attri.Key);
                if attri.Value is Tjsonarray then begin
                  if attri.Value.Count=0 then begin
                    sub := THELICSSubTopic.Create (attriKey, '-1', '', top.idx); // switchstate, tapposition, etc.
                    top.sub.Add(sub);
                  end else
                    for conductor in attri.Value do begin
                      condKey := conductor.Value.asstring;
                      sub := THELICSSubTopic.Create (attriKey, '1', condKey, top.idx);
                      top.sub.Add(sub);
                    end;
                end else begin  // attri.Value is not a TJSONArray
                  for terminal in attri.Value do begin
                    terminalKey:=LowerCase(terminal.Key);
                    for conductor in terminal.Value do begin
                      condKey := conductor.Value.asstring;
                      sub := THELICSSubTopic.Create (attriKey, terminalKey, condKey, top.idx);
                      top.sub.Add(sub);
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    finally
      parser.Free;
    end;

  for top in topicList do
    for sub in top.sub do
      sub.text_key := Format ('%s%s%s',[top.tag, sep, sub.tag]);

  if log_level >= helicsLogInfo then
    writeln('Done parsing HELICS publication requests from pub info ');
  if log_level >= helicsLogDebug1 then begin
    DumpHELICSLists;
  end;
end;


// called from ActiveSolution.Increment_time
function THELICS.HelicsTimeRequest (next_helics:helics_time): Boolean;
var
  time_granted: helics_time;
  helics_error: THELICSERROR = (error_code:0; message:nil);
  apub: helics_publication;
  asub: helics_input;
  key, input_value: pchar;
  i: integer;
  ilast: size_t;
  nvalues, ival: size_t;
  re, im: double;
  ld: TLoadObj;
  Hour: integer;
  Sec, real, imag: double;
  cmd, input_type, sign: string;
begin
  // execution blocks here, until HELICS permits the time step loop to continue
  time_granted := 0;
  while time_granted < next_helics do begin
    ET.Clear;
    ET.Start;
    time_granted := helics_time_request(helicsFed, next_helics, @helics_error);
    ETHelicsTimeRequest := ETHelicsTimeRequest + ET.Elapsed;
    ET.Clear;
    if log_level >= helicsLogDebug2 then begin
      Writeln(Format('  Already granted %f by HELICS and requested %f, granted %f', [existing_helics_grant, next_helics, time_granted]));
      system.flush (stdout);
    end;
    Hour := ActiveCircuit.Solution.DynaVars.intHour;
    Sec :=  ActiveCircuit.Solution.Dynavars.t;
    if time_granted >= next_helics_publish then begin
      if topicList.Count > 0 then begin
        if log_level >= helicsLogDebug2 then begin
          Writeln(Format('  Stream size %u at %f, next at %f, interval %u, %d:%.3f',
            [helicsOutputStream.size, time_granted, next_helics_publish, PublishInterval, Hour, Sec]));
          system.flush (stdout);
        end;
        TopicsListToPublication;
      end;
      while next_helics_publish <= time_granted do
        next_helics_publish := next_helics_publish + PublishInterval;
    end;
    ET.Clear;
    ET.Start;
    //ilast := helics_count_events();
    // TODO: executing OpenDSS commands here may cause unwanted interactions
    //for i := 1 to ilast do begin
    //  //key := helics_next_event();
    //  //nvalues := helics_count_values (key);
    //  if CompareText (key, 'command') = 0 then begin
    //    for ival := 1 to nvalues do begin
    //      //value := helics_next_value();
    //      if log_level >= helicsLogDebug2 then begin
    //        writeln(Format('  FNCSTimeRequest command %s at %u, %d:%.3f',
    //          [value, time_granted, Hour, Sec]));
    //        system.flush (stdout);
    //      end;
    //      DSSExecutive.Command := value;
    //    end;
    //  end else if Pos ('#load', key) > 0 then begin
    //    //value := helics_next_value();
    //    re := StrToFloat (ExtractWord (1, value, ['+', 'j', ' ']));
    //    im := StrToFloat (ExtractWord (2, value, ['+', 'j', ' ']));
    //    ActiveCircuit.SetElementActive ('load.F1_house_B0');
    //    ld := TLoadObj (ActiveCircuit.ActiveCktElement);
    //    ld.LoadSpecType := 1;
    //    ld.kwBase := re;
    //    ld.kvarBase := im;
    //    ld.RecalcElementData;
    //    if log_level >= helicsLogDebug2 then begin
    //      writeln(Format ('HELICS Request %s to %g + j %g at %u, %d:%.3f',
    //        [ld.Name, re, im, time_granted, Hour, Sec]));
    //      system.flush (stdout);
    //    end;
    //  end;
    //end;
    
    writeln(Format('number of subscription: %d, count in subscription list: %d', [sub_count, sub_list.Count]));
    for i := 0 to sub_count - 1 do begin
      asub := sub_list.Items[i];
      if helics_input_is_valid(asub) and helics_input_is_updated(asub) then begin
        writeln('asub is valid.');
        key := helics_input_get_key(asub);
        cmd := parse_key(key);
        input_type := StrPas(helics_input_get_type(asub));
        if CompareText(input_type, 'double') = 0 then input_value := Pchar(FloatToStrF(helics_input_get_double(asub, @helics_error), ffFixed, 0, 3));
        if CompareText(input_type, 'complex') = 0 then begin
          helics_input_get_complex(asub, @real, @imag, @helics_error);
          if imag < 0 then
            sign:='-'
          else
            sign:='+';
          input_value := PChar (FloatToStrF(real, ffFixed, 0, 3) + sign + FloatToStrF(abs(imag), ffFixed, 0, 3) + 'j');
        end;
        if CompareText(input_type, 'string') = 0 then input_value := helics_get_input_string(asub, @helics_error);
        if pos('int', input_type) > 0 then input_value := Pchar(IntToStr (helics_input_get_integer(asub, @helics_error)));
        writeln(Format('subscription key: %s, cmd: %s, input_value: %s at %f',
                               [key, cmd, input_value, time_granted]));
        if CompareText (cmd, 'command') = 0 then begin
           if log_level >= helicsLogDebug1 then begin
              writeln(Format('HELICS command %s at %f, evt %u of %d',
                                     [input_value, time_granted, i + 1, sub_count]));
              system.flush (stdout);
           end;
           DSSExecutive.Command := input_value;
           if log_level >= helicsLogDebug1 then begin
              writeln(Format('Finished with %s at %f, evt %u of %d',
                                       [input_value, time_granted, i + 1, sub_count]));
              system.flush (stdout);
           end;
        end else if Pos ('load', cmd) > 0 then begin
          //input_value := helics_get_input_string(asub, @helics_error);
          re := StrToFloat (ExtractWord (1, input_value, ['+', 'j', ' ']));
          im := StrToFloat (ExtractWord (2, input_value, ['+', 'j', ' ']));
          ActiveCircuit.SetElementActive ('load.F1_house_B0');
          ld := TLoadObj (ActiveCircuit.ActiveCktElement);
          ld.LoadSpecType := 1;
          ld.kwBase := re;
          ld.kvarBase := im;
          ld.RecalcElementData;
          if log_level >= helicsLogDebug2 then begin
            writeln(Format ('HELICS input received: %s, at %f, %d:%.3f',
              [cmd, time_granted, Hour, Sec]));
            system.flush (stdout);
          end;
        end else if CompareText ('solar_flux', cmd) = 0 then begin
          if log_level >= helicsLogDebug2 then begin
            writeln(Format ('HELICS Request %s to %g + j %g at %f, %d:%.3f',
              [ld.Name, re, im, time_granted, Hour, Sec]));
            system.flush (stdout);
          end;
        end;
      end;
    end;
    ETHelicsGetEvents := ETHelicsGetEvents + ET.Elapsed;
    ET.Clear;
  end;
  Result := True;
end;

function THELICS.parse_key(key:Pchar):string;
var
  cmd: string;
  slash_found: Boolean;
  i: integer;
begin
  slash_found := false;
  cmd := '';
  for i := 0 to StrLen(key) - 1 do begin
    if (slash_found = False) and (key[i] = '/') then begin
      slash_found := true; //writeln(key[i])
      cmd := '';
    end
    else begin
        if slash_found then cmd := cmd + key[i]
    end;
  end;
  Result := cmd;
end;

function THELICS.helics_get_input_string(input: helics_input; error: Pointer): Pchar;
var
  input_type, input_value: pchar;
  stype: string;
  maxLength, actualLength: Integer;
  double_value: Double;
  startQuoteIndex, endQuoteIndex, startIdx: Integer;
begin
  input_type := helics_input_get_type(input);
  if CompareText (input_type, 'string') = 0 then begin
    maxLength := helics_input_get_string_size(input);
    input_value := StrAlloc(maxLength);
    helics_input_get_string(input, input_value, maxLength, @actualLength, error);
    startIdx := 1;
    startQuoteIndex := 0;
    endQuoteIndex := 0;
    startQuoteIndex := Pos(input_value[0], '"''');
    endQuoteIndex := Pos(input_value[actualLength - 2], '"''');
    if (startQuoteIndex > 0) and (endQuoteIndex > 0) then begin
      actualLength := actualLength - 3;
      startIdx := 2;
    end;
    Result := StrAlloc(actualLength + 1);
    strpcopy(Result, Copy(input_value, startIdx, actualLength));
  end else if CompareText (input_type, 'double') = 0 then begin
    double_value := helics_input_get_double(input, error);
    Result := Pchar(double_value)
  end;
  //stype := StrPas(input_type);
  //case (stype) of
  //  'string' : writeln('string!' );
  //  'double' : writeln('double!' );
  //end;
end;

procedure THELICS.RunHELICSLoop (const s:string);
var
  time_granted, time_stop: helics_time;
  key, input_value: pchar;
  i, aaa: integer;
  configFile, cmd: string;
  //configFile2: pchar;
  //helicsFed: helics_federate;
  //helics_version: PChar;
  helics_error: THELICSERROR = (error_code:0; message:nil);
  //adr_helics_error: ^word;
  //pub_count, sub_count: integer;
  //pub_list, sub_list: TFPList;
  apub: helics_publication;
  asub: helics_input;
  aep: helics_endpoint;
  helics_message: THELICSMESSAGE;
  parser:TJSONParser;
begin
  //pub_list := TFPList.Create;
  //sub_list := TFPList.Create;
  //endpoint_list := TFPList.Create;
  time_granted := 0;
  time_stop := InterpretStopTimeForHELICS(s);
  if log_level >= helicsLogInfo then
    writeln(Format('Starting HELICS loop to run %s or %f seconds', [s, time_stop]));
  ET.Clear;
  ET.Start;
  configFile := GetEnvironmentVariable('HELICS_CONFIG_FILE');
  configFile := GetCurrentDir() + '/' + configFile;
  //configFile2 := '/home/xcosmos/src/OpenDSS/Version7/Source/CMD/test/opendss.json';
  //helics_version := helics_get_version();
  //writeln('HELICS version is: ', helics_version);
  helicsFed := helics_create_value_fed_config(Pchar(configFile), @helics_error);
  //helicsFed := helics_create_message_fed_config(Pchar(configFile), @helics_error);
  //helicsFed := helics_create_combination_fed_config(Pchar(configFile), @helics_error);
  //writeln(Format ('helicsFed address: %p', [helicsFed]));
  if helicsFed <> nil then begin
    //endpoint_count := helics_get_endpoint_count(helicsFed);
    //for i := 0 to endpoint_count - 1 do begin
    //  endpoint_list.Add(helics_fed_get_endpoints_by_index(helicsFed, i, @helics_error))
    //end;
    pub_count := helics_get_publication_count(helicsFed);
    //if pub_count <> 1 then begin
    //  writeln ('Exact 1 publication is needed for opendss federate.');
    //  Exit
    //end;
    //opendssPub := helics_fed_get_pub_by_index(helicsFed, 0, @helics_error);
    //pubJsonStr := helics_publication_get_info(opendssPub);
    for i := 0 to pub_count - 1 do begin
      pub_list.Add(helics_fed_get_pub_by_index(helicsFed, i, @helics_error))
    end;
    if pub_count = 1 then pubJsonStr := helics_publication_get_info(pub_list.Items[0]);
    sub_count := helics_get_subscription_count(helicsFed);
    //if sub_count <> 1 then begin
    //  writeln ('Exact 1 subscription is needed for opendss federate.');
    //  Exit
    //end;
//    opendssSub := helics_fed_get_input_by_index(helicsFed, 0, @helics_error);
    for i := 0 to sub_count - 1 do begin
      sub_list.Add(helics_fed_get_input_by_index(helicsFed, i, @helics_error))
    end
  end
  else begin
    writeln('create helics value federate from config file error: ', helics_error.message);
    //writeln('create helics combination federate from config file error: ', helics_error.message);
    exit
  end;

  if helicsFed <> nil then
    //adr_helics_error := addr(helics_error);
    //writeln(Format ('@helics_error address: %p', [adr_helics_error]));
    writeln('HELICS enter intializing mode.');
    helics_enter_initialize_mode(helicsFed, @helics_error);
    //time_granted := helics_time_request(helicsFed, time_stop, @helics_error);
    //for i := 0 to sub_count - 1 do begin
    //  asub := sub_list.Items[i];
    //  if helics_input_is_valid(asub) then begin
    //    writeln('asub is valid.');
    //    key := helics_input_get_key(asub);
    //    cmd := parse_key(key);
    //    input_value := helics_get_input_string(asub, @helics_error);
    //    aaa := StrLen(input_value);
    //    if StrLen(input_value) > 0 then begin
    //      if CompareText (cmd, 'command') = 0 then begin
    //         if log_level >= helicsLogDebug1 then begin
    //            writeln(Format('HELICS command %s at %f, evt %u of %d',
    //                                   [input_value, time_granted, i + 1, sub_count]));
    //            system.flush (stdout);
    //         end;
    //         DSSExecutive.Command := input_value;
    //         if log_level >= helicsLogDebug1 then begin
    //            writeln(Format('Finished with %s at %f, evt %u of %d',
    //                                     [input_value, time_granted, i + 1, sub_count]));
    //            system.flush (stdout);
    //         end;
    //      end else if Pos ('#load', cmd) > 0 then begin
    //        input_value := helics_get_input_string(asub, @helics_error);
    //        if log_level >= helicsLogDebug1 then begin
    //         writeln(Format ('HELICS Loop %s to %s', [cmd, input_value]));
    //         system.flush (stdout);
    //        end;
    //      end;
    //    end;
    //  end;
    //end;

    writeln('HELICS enter executing mode.');
    helics_enter_executing_mode(helicsFed, @helics_error);
    //helics_message := helics_fed_get_message(helicsFed);
    time_granted := helics_fed_get_current_time(helicsFed, @helics_error);
    //time_granted := helics_time_request_iter(helicsFed, 0, 1, nil, @helics_error);
                                                                                    
    for i := 0 to sub_count - 1 do begin
      asub := sub_list.Items[i];
      if helics_input_is_valid(asub) and helics_input_is_updated(asub) then begin
        writeln('asub is valid.');
        key := helics_input_get_key(asub);
        cmd := parse_key(key);
        input_value := helics_get_input_string(asub, @helics_error);
        //aaa := StrLen(input_value);
        if StrLen(input_value) > 0 then begin
          if CompareText (cmd, 'command') = 0 then begin
             if log_level >= helicsLogDebug1 then begin
                writeln(Format('HELICS command %s at %f, evt %u of %d',
                                       [input_value, time_granted, i + 1, sub_count]));
                system.flush (stdout);
             end;
             DSSExecutive.Command := input_value;
             if log_level >= helicsLogDebug1 then begin
                writeln(Format('Finished with %s at %f, evt %u of %d',
                                         [input_value, time_granted, i + 1, sub_count]));
                system.flush (stdout);
             end;
          end else if Pos ('#load', cmd) > 0 then begin
//            input_value := helics_get_input_string(opendssSub, @helics_error);
            if log_level >= helicsLogDebug1 then begin
             writeln(Format ('HELICS Loop %s to %s', [cmd, input_value]));
             system.flush (stdout);
            end;
          end;
        end;
      end;
    end;

    //for i := 0 to endpoint_count - 1 do begin
    //  aep := endpoint_list.Items[i];
    //  if helics_endpoint_is_valid(aep) then begin
    //    writeln('an endpoint is valid.');
    //    helics_message := helics_endpoint_get_message(aep);
    //    cmd := parse_key(key);
    //    input_value := helics_get_input_string(asub, @helics_error);
    //    aaa := StrLen(input_value);
    //    if StrLen(input_value) > 0 then begin
    //      if CompareText (cmd, 'command') = 0 then begin
    //         if log_level >= helicsLogDebug1 then begin
    //            writeln(Format('HELICS command %s at %f, evt %u of %d',
    //                                   [input_value, time_granted, i + 1, sub_count]));
    //            system.flush (stdout);
    //         end;
    //         DSSExecutive.Command := input_value;
    //         if log_level >= helicsLogDebug1 then begin
    //            writeln(Format('Finished with %s at %f, evt %u of %d',
    //                                     [input_value, time_granted, i + 1, sub_count]));
    //            system.flush (stdout);
    //         end;
    //      end else if Pos ('#load', cmd) > 0 then begin
    //        input_value := helics_get_input_string(asub, @helics_error);
    //        if log_level >= helicsLogDebug1 then begin
    //         writeln(Format ('HELICS Loop %s to %s', [cmd, input_value]));
    //         system.flush (stdout);
    //        end;
    //      end;
    //    end;
    //  end;
    //end;

  ETHelicsLoad := ETHelicsLoad + ET.Elapsed;
  ET.Clear;
  in_helics_loop := True;

  Try
    while time_granted < time_stop do begin
      ET.Clear;
      ET.Start;
      //adr_helics_error := addr(helics_error);
      //writeln(Format ('@helics_error address: %p before time request', [adr_helics_error]));
      //writeln(Format ('helicsFed address: %p before time request', [helicsFed]));
      //writeln(Format ('time_stop is: %f before time request', [time_stop]));\
      time_granted := helics_time_request(helicsFed, time_stop, @helics_error);
      ETHelicsTimeRequest := ETHelicsTimeRequest + ET.Elapsed;
      ET.Clear;
      existing_helics_grant := time_granted;
      ET.Start;

      //for i := 0 to pub_count - 1 do begin
      //  apub := pub_list.Items[i];
      //  key := helics_publication_get_key(apub);
      //end;
      for i := 0 to sub_count - 1 do begin
        asub := sub_list.Items[i];
        if helics_input_is_valid(asub) and helics_input_is_updated(asub) then begin
          writeln('asub is valid.');
          key := helics_input_get_key(asub);
          cmd := parse_key(key);
          input_value := helics_get_input_string(asub, @helics_error);
          if CompareText (cmd, 'command') = 0 then begin
             if log_level >= helicsLogDebug1 then begin
                writeln(Format('HELICS command %s at %f, evt %u of %d',
                                       [input_value, time_granted, i + 1, sub_count]));
                system.flush (stdout);
             end;
             DSSExecutive.Command := input_value;
             if log_level >= helicsLogDebug1 then begin
                writeln(Format('Finished with %s at %f, evt %u of %d',
                                         [input_value, time_granted, i + 1, sub_count]));
                system.flush (stdout);
             end;
          end else if Pos ('#load', cmd) > 0 then begin
            //input_value := helics_get_input_string(asub, @helics_error);
            if log_level >= helicsLogDebug1 then begin
             writeln(Format ('HELICS Loop %s to %s', [cmd, input_value]));
             system.flush (stdout);
            end;
          end;
        end;
      end;

    //for i := 0 to endpoint_count - 1 do begin
    //  aep := endpoint_list.Items[i];
    //  if helics_endpoint_is_valid(aep) and helics_endpoint_has_message(aep) then begin
    //    writeln('an endpoint is valid.');
    //    helics_message := helics_endpoint_get_message(aep);
    //      if CompareText (cmd, 'command') = 0 then begin
    //         if log_level >= helicsLogDebug1 then begin
    //            writeln(Format('HELICS command %s at %f, evt %u of %d',
    //                                   [input_value, time_granted, i + 1, sub_count]));
    //            system.flush (stdout);
    //         end;
    //         DSSExecutive.Command := input_value;
    //         if log_level >= helicsLogDebug1 then begin
    //            writeln(Format('Finished with %s at %f, evt %u of %d',
    //                                     [input_value, time_granted, i + 1, sub_count]));
    //            system.flush (stdout);
    //         end;
    //      end else if Pos ('#load', cmd) > 0 then begin
    //        input_value := helics_get_input_string(asub, @helics_error);
    //        if log_level >= helicsLogDebug1 then begin
    //         writeln(Format ('HELICS Loop %s to %s', [cmd, input_value]));
    //         system.flush (stdout);
    //        end;
    //      end;
    //  end;
    //end;


      ETHelicsGetEvents := ETHelicsGetEvents + ET.Elapsed;
      ET.Clear;
      existing_helics_grant := 0;
    end;
  finally
    in_helics_loop := False;
    writeln(Format('HELICS Timing: LoadLib=%.6f, ReadConfig=%.6f, TimeRequests=%.6f, GetEvents=%.6f, Publish=%.6f',
      [ETHelicsLoad, ETHelicsReadPub, ETHelicsTimeRequest, ETHelicsGetEvents, ETHelicsPublish]));
    helics_finalize(helicsFed, @helics_error);
    //helics_close_library;
  end;
end;

function THELICS.IsReady:Boolean;
begin
  Result := True;
  if FLibHandle = DynLibs.NilHandle then Result := False;
end;

function THELICS.IsRunning:Boolean;
begin
  Result := in_helics_loop;
end;

function THELICS.find_helics_function (name: String): Pointer;
begin
  Result := GetProcedureAddress (FLibHandle, name);
  if Result = nil then begin
    writeln ('HELICS library found, but missing function ', name);
    FuncError := True;
  end;
end;

constructor THELICS.Create;
var
  s: String = 'INFO';
begin
  sep := '/';
  PublishInterval := 1;
  PublishMode := helicsPublishJSON;
  ET := TEpikTimer.Create(nil);
  ETHelicsReadPub := 0.0;
  ETHelicsPublish := 0.0;
  ETHelicsLoad := 0.0;
  ETHelicsGetEvents := 0.0;
  ETHelicsTimeRequest := 0.0;
  existing_helics_grant := 0;
  in_helics_loop := False;
  log_level := helicsLogWarning;
  s := GetEnvironmentVariable ('HELICS_LOG_LEVEL');
  if s = 'INFO' then
    log_level := helicsLogInfo
  else if s = 'DEBUG' then
    log_level := helicsLogDebug1
  else if s = 'DEBUG1' then
    log_level := helicsLogDebug1
  else if s = 'DEBUG2' then
    log_level := helicsLogDebug2
  else if s = 'DEBUG3' then
    log_level := helicsLogDebug3
  else if s = 'DEBUG4' then
    log_level := helicsLogDebug4;

  ET.Clear;
  ET.Start;
{$IFDEF Windows}
  FLibHandle := SafeLoadLibrary ('helicsSharedLib.' + SharedSuffix);
{$ELSE} // Darwin and Unix
  if log_level < helicsLogDebug1 then
     FLibHandle := SafeLoadLibrary ('libhelicsSharedLib.' + SharedSuffix)
  else
     FLibHandle := SafeLoadLibrary ('libhelicsSharedLibd.' + SharedSuffix);
{$ENDIF}

  topicList:=TList.Create();
  pub_list := TFPList.Create();
  sub_list := TFPList.Create();
  endpoint_list := TFPList.Create();
  if FLibHandle <> DynLibs.NilHandle then begin
    FuncError := False;
    @helics_create_value_fed := find_helics_function ('helicsCreateValueFederate');
    if not FuncError then @helics_create_value_fed_config := find_helics_function ('helicsCreateValueFederateFromConfig'); 
    if not FuncError then @helics_create_message_fed_config := find_helics_function ('helicsCreateMessageFederateFromConfig');
    if not FuncError then @helics_create_combination_fed_config := find_helics_function ('helicsCreateCombinationFederateFromConfig');
    if not FuncError then @helics_get_publication_count := find_helics_function ('helicsFederateGetPublicationCount'); 
    if not FuncError then @helics_get_subscription_count := find_helics_function ('helicsFederateGetInputCount');
    if not FuncError then @helics_get_endpoint_count := find_helics_function ('helicsFederateGetEndpointCount');
    if not FuncError then @helics_fed_get_pub_by_index := find_helics_function ('helicsFederateGetPublicationByIndex');
    if not FuncError then @helics_fed_get_input_by_index := find_helics_function ('helicsFederateGetInputByIndex'); 
    if not FuncError then @helics_fed_get_endpoints_by_index := find_helics_function ('helicsFederateGetEndpointByIndex');
                                                                                                                  
    if not FuncError then @helics_enter_initialize_mode := find_helics_function ('helicsFederateEnterInitializingMode');
    if not FuncError then @helics_enter_executing_mode := find_helics_function ('helicsFederateEnterExecutingMode');
    //if not FuncError then @fncs_is_initialized := find_fncs_function ('fncs_is_initialized');
    if not FuncError then @helics_time_request := find_helics_function ('helicsFederateRequestTime');
    if not FuncError then @helics_fed_get_current_time := find_helics_function ('helicsFederateGetCurrentTime');
    if not FuncError then @helics_time_request_iter := find_helics_function ('helicsFederateRequestTimeIterative');
    if not FuncError then @helics_time_request_iter_complete := find_helics_function ('helicsFederateRequestTimeIterativeComplete');
    if not FuncError then @helics_input_is_updated := find_helics_function ('helicsInputIsUpdated');
    if not FuncError then @helics_input_is_valid := find_helics_function ('helicsInputIsValid');
    if not FuncError then @helics_endpoint_is_valid := find_helics_function ('helicsEndpointIsValid');
    if not FuncError then @helics_endpoint_has_message := find_helics_function ('helicsEndpointHasMessage');
    //if not FuncError then @fncs_die := find_fncs_function ('fncs_die');
    if not FuncError then @helics_finalize := find_helics_function ('helicsFederateFinalize');
    //if not FuncError then @fncs_get_events_size := find_fncs_function ('fncs_get_events_size');
    //if not FuncError then @fncs_get_events := find_fncs_function ('fncs_get_events');
    //if not FuncError then @fncs_get_event_at := find_fncs_function ('fncs_get_event_at');
    //if not FuncError then @fncs_agentGetEvents := find_fncs_function ('fncs_agentGetEvents');
    //if not FuncError then @fncs_get_value := find_fncs_function ('fncs_get_value');
    //if not FuncError then @fncs_get_values_size := find_fncs_function ('fncs_get_values_size');
    //if not FuncError then @fncs_get_values := find_fncs_function ('fncs_get_values');
    //if not FuncError then @fncs_get_value_at := find_fncs_function ('fncs_get_value_at');
    //if not FuncError then @fncs_get_keys_size := find_fncs_function ('fncs_get_keys_size');
    //if not FuncError then @fncs_get_keys := find_fncs_function ('fncs_get_keys');
    //if not FuncError then @fncs_get_key_at := find_fncs_function ('fncs_get_key_at');
    //if not FuncError then @fncs_get_name := find_fncs_function ('fncs_get_name');
    //if not FuncError then @fncs_get_id := find_fncs_function ('fncs_get_id');
    //if not FuncError then @fncs_get_simulator_count := find_fncs_function ('fncs_get_simulator_count');
    if not FuncError then @helics_get_version := find_helics_function ('helicsGetVersion');
    if not FuncError then @helics_close_library := find_helics_function ('helicsCloseLibrary');
    //if not FuncError then @fncs_count_events := find_fncs_function ('fncs_count_events');
    //if not FuncError then @fncs_count_values := find_fncs_function ('fncs_count_values');
    //if not FuncError then @helics_input_get_key := find_helics_function ('helicsInputGetKey');
    if not FuncError then @helics_input_get_type := find_helics_function ('helicsInputGetType');
    if not FuncError then @helics_input_get_key := find_helics_function ('helicsSubscriptionGetKey');
    if not FuncError then @helics_publication_get_key := find_helics_function ('helicsPublicationGetKey');
    if not FuncError then @helics_publication_get_info := find_helics_function ('helicsPublicationGetInfo');  
    if not FuncError then @helics_publication_get_type := find_helics_function ('helicsPublicationGetType');
    if not FuncError then @helics_publication_publish_string := find_helics_function ('helicsPublicationPublishString');  
    if not FuncError then @helics_publication_publish_int := find_helics_function ('helicsPublicationPublishInteger');
    if not FuncError then @helics_publication_publish_double := find_helics_function ('helicsPublicationPublishDouble');
    if not FuncError then @helics_publication_publish_complex := find_helics_function ('helicsPublicationPublishComplex');
    if not FuncError then @helics_publication_publish_boolean := find_helics_function ('helicsPublicationPublishBoolean');

    if not FuncError then @helics_input_get_integer := find_helics_function ('helicsInputGetInteger');
    if not FuncError then @helics_input_get_boolean := find_helics_function ('helicsInputGetBoolean');
    if not FuncError then @helics_input_get_double := find_helics_function ('helicsInputGetDouble');
    if not FuncError then @helics_input_get_string := find_helics_function ('helicsInputGetString');
    if not FuncError then @helics_input_get_string_size := find_helics_function ('helicsInputGetStringSize');
    if not FuncError then @helics_input_get_complex := find_helics_function ('helicsInputGetComplex');
    if not FuncError then @helics_endpoint_get_message := find_helics_function ('helicsEndpointGetMessage'); 
    if not FuncError then @helics_fed_get_message := find_helics_function ('helicsFederateGetMessage');
    if FuncError then begin
      UnloadLibrary(FlibHandle);
      FLibHandle := DynLibs.NilHandle;
    end;
  end;
  ETHelicsLoad := ETHelicsLoad + ET.Elapsed;
  ET.Clear;
end;

destructor THELICS.Destroy;
begin
  in_helics_loop := False;
  topicList.Free;
  helics_close_library;
  If FLibHandle <> DynLibs.NilHandle Then Begin
    UnloadLibrary(FLibHandle);
  End;
  inherited;
end;

end.

