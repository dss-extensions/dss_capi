{
 ----------------------------------------------------------
  Copyright (c) 2017-2020 Battelle Memorial Institute
 ----------------------------------------------------------
}
unit FNCS;

{$mode delphi}
//{$mode objfpc}{$H+}
//{$MODESWITCH ADVANCEDRECORDS}
{$MACRO ON}
{$IFDEF Windows}
{$DEFINE FNCS_CALL:=stdcall}
//{$DEFINE FNCS_CALL:=cdecl}
{$ELSE} // Darwin and Unix
{$DEFINE FNCS_CALL:=cdecl}
{$ENDIF}

interface

uses
  Classes, SysUtils, Executive, {$IFDEF Unix} unix, {$ENDIF} dynlibs, DSSGlobals,
  UComplex, CktElement, Utilities, math;

type
  fncs_time = qword;

  // lists for FNCS classic publications
  TFNCSClass = (fncsBus, fncsLine, fncsSwitch, fncsCapacitor, fncsPVSystem, 
                fncsVSource, fncsTransformer, fncsFault, fncsStorage, fncsNoClass);
  TFNCSAttribute = (fncsVoltage, fncsCurrent, fncsPower, fncsSwitchState, 
                fncsTapPosition, fncsEnergy, fncsNoAttribute);
  TFNCSLogLevel = (fncsLogWarning, fncsLogInfo, fncsLogDebug1, fncsLogDebug2,
                fncsLogDebug3, fncsLogDebug4);
  TFNCSPublishMode = (fncsPublishJSON, fncsPublishText);
  TFNCSSubTopic = class (TObject)
  public
    tag: String;            // what FNCS calls it
    text_key: String;       // in Text-mode export we need to include the higher-level topic tag, and cache it
    att: TFNCSAttribute;
    trm: Integer;           // terminal number
    ref: Integer;           // index into the phases
    constructor Create(attKey, trmKey, phsKey: String; idxRoot: Integer);
  end;
  TFNCSTopic = class (TObject)
  public
    tag: String;            // what FNCS calls it
    dss: String;            // what OpenDSS calls it
    cls: TFNCSClass;
    idx: Integer;           // index into BusList or DeviceList
    sub: TList;             // list of attribute subtopics
    constructor Create (clsKey, objKey: String);
  end;

  TFNCS = class(TObject)
  private
    FLibHandle: TLibHandle;
    FuncError: Boolean;
    // Connect to broker and parse config file.
    fncs_initialize: procedure;FNCS_CALL;
    // Connect to broker and parse inline configuration.
    fncs_initialize_config: procedure (configuration:Pchar);FNCS_CALL;
    // Connect to broker and parse config file for Transactive agents.
    fncs_agentRegister: procedure;FNCS_CALL;
    // Connect to broker and parse inline configuration for transactive agents.
    fncs_agentRegisterConfig: procedure (configuration:Pchar);FNCS_CALL;
    // Check whether simulator is configured and connected to broker.
    fncs_is_initialized: function:longint;FNCS_CALL;
    // Request the next time step to process.
    fncs_time_request: function (next:fncs_time):fncs_time;FNCS_CALL;
    // Publish value using the given key.
    fncs_publish: procedure (key:Pchar; value:Pchar);FNCS_CALL;
    // Publish value anonymously using the given key.
    fncs_publish_anon: procedure (key:Pchar; value:Pchar);FNCS_CALL;
    // Publish function for transactive agents.
    fncs_agentPublish: procedure (value:Pchar);FNCS_CALL;
    // Publish value using the given key, adding from:to into the key.
    fncs_route: procedure (source:Pchar; target:Pchar; key:Pchar; value:Pchar);FNCS_CALL;
    // Tell broker of a fatal client error.
    fncs_die: procedure;FNCS_CALL;
    // Close the connection to the broker.
    fncs_finalize: procedure;FNCS_CALL;
    // Update minimum time delta after connection to broker is made. Assumes time unit is not changing.
    fncs_update_time_delta: procedure (delta:fncs_time);FNCS_CALL;
    // Get the number of keys for all values that were updated during the last time_request.
    fncs_get_events_size: function:size_t;FNCS_CALL;
    // Get the keys for all values that were updated during the last time_request.
    fncs_get_events: function:ppchar;FNCS_CALL;
    // Get one key for the given event index that as updated during the last time_request.
    fncs_get_event_at: function (index:size_t):pchar;FNCS_CALL;
    // Get the agent events for all values that were updated during the last time_request.
    fncs_agentGetEvents: function:pchar;FNCS_CALL;
    // Get a value from the cache with the given key. Will hard fault if key is not found.
    fncs_get_value: function (key:Pchar):pchar;FNCS_CALL;
    // Get the number of values from the cache with the given key.
    fncs_get_values_size: function (key:Pchar):size_t;FNCS_CALL;
    // Get an array of values from the cache with the given key. Will return an array of size 1 if only a single value exists.
    fncs_get_values: function (key:Pchar):ppchar;FNCS_CALL;
    // Get a single value from the array of values for the given key.
    fncs_get_value_at: function (key:Pchar; index:size_t):pchar;FNCS_CALL;
    // Get the number of subscribed keys.
    fncs_get_keys_size: function:size_t;FNCS_CALL;
    // Get the subscribed keys. Will return NULL if fncs_get_keys_size() returns 0.
    fncs_get_keys: function:ppchar;FNCS_CALL;
    // Get the subscribed key at the given index. Will return NULL if fncs_get_keys_size() returns 0.
    fncs_get_key_at: function (index:size_t):pchar;FNCS_CALL;
    // Return the name of the simulator.
    fncs_get_name: function:pchar;FNCS_CALL;
    // Return a unique numeric ID for the simulator.
    fncs_get_id: function:longint;FNCS_CALL;
    // Return the number of simulators connected to the broker.
    fncs_get_simulator_count: function:longint;FNCS_CALL;
    // Run-time API version detection.
    fncs_get_version: procedure (major:Plongint; minor:Plongint; patch:Plongint);FNCS_CALL;
    // Convenience wrapper around libc free.
    fncs_free: procedure (ptr:pointer);FNCS_CALL;
    // faster versions for Pascal interface
    fncs_count_events: function:size_t;FNCS_CALL;
    fncs_count_values: function (key:Pchar):size_t;FNCS_CALL;
    fncs_next_event: function:pchar;FNCS_CALL;
    fncs_next_value: function:pchar;FNCS_CALL;

    function find_fncs_function (name: String): Pointer;

  private
    in_fncs_loop:Boolean;
    next_fncs_publish: fncs_time;
    existing_fncs_grant: fncs_time;
    ETFncsReadPub: Extended;
    ETFncsPublish: Extended;
    ETFncsLoad: Extended;
    ETFncsTimeRequest: Extended;
    ETFncsGetEvents: Extended;
    log_level: TFNCSLogLevel;
    topicList: TList;
    fncsOutputStream: TStringStream;
    procedure ReadFNCSJsonConfig (fname: string);
    procedure ReadFNCSTextConfig (fname: string);
    procedure SetPublishInterval (val: Integer);
    procedure SetPublishMode (val: string);

  public
    PublishInterval:Integer;
    PublishMode:TFNCSPublishMode;
    FedName:string;

    function IsReady:Boolean;
    function IsRunning:Boolean;
    procedure RunFNCSLoop (const s:string);
    constructor Create();
    destructor Destroy; override;
    function FncsTimeRequest (next_fncs:fncs_time):Boolean;
    procedure ReadFNCSPubConfig (fname: string);
    procedure TopicsListToPublication;
    procedure DumpFNCSLists;
  end;

var
  ActiveFNCS:TFNCS;

implementation

uses
  fpjson, jsonparser, jsonscanner, strutils, Transformer, Load, Storage, EpikTimer; // RegControl,ControlElem;

var
  ET: TEpikTimer; // for profiling
  sep: string;    // for delimiting tokens in a FNCS topic key; this is always '.' for DSS

constructor TFNCSTopic.Create (clsKey, objKey: String);
begin
  dss := clsKey + '.' + objKey;
  tag := clsKey + sep + objKey;
  if clsKey = 'bus' then
    cls := fncsBus
  else if clsKey = 'line' then
    cls := fncsLine
  else if clsKey = 'switch' then
    cls := fncsSwitch
  else if clsKey = 'capacitor' then
    cls := fncsCapacitor
  else if clsKey = 'pvsystem' then
    cls := fncsPVSystem
  else if clsKey = 'storage' then
    cls := fncsStorage
  else if clsKey = 'vsource' then
    cls := fncsVSource
  else if clsKey = 'transformer' then
    cls := fncsTransformer
  else if clsKey = 'fault' then
    cls := fncsFault
  else
    cls := fncsNoClass;
  if cls = fncsBus then
    idx := ActiveCircuit.BusList.Find (objKey)
  else
    idx := ActiveCircuit.SetElementActive (dss);
  if idx = 0 then writeln ('*** can not find FNCS output for ' + dss);
  sub := TList.Create();
end;

constructor TFNCSSubTopic.Create (attKey, trmKey, phsKey: String; idxRoot: Integer);
var
  idxPhs, idxLoc: Integer;
  pElem :TDSSCktElement;
  Ncond, Nterm, kmax, k: Integer;
  PhaseTable: array[1..2, 0..3] of Integer; // index into cBuffer by terminal, then phase
begin
  trm := StrToInt (trmKey);
  if attKey = 'voltage' then
    att := fncsVoltage
  else if attKey = 'current' then
    att := fncsCurrent
  else if attKey = 'power' then
    att := fncsPower
  else if attKey = 'switchstate' then
    att := fncsSwitchState
  else if attKey = 'tapposition' then
    att := fncsTapPosition
  else if attKey = 'kwhstored' then
    att := fncsEnergy
  else
    att := fncsNoAttribute;
  if trm > 0 then
    if att = fncsVoltage then
      tag := attKey + sep + phsKey
    else
      tag := attKey + sep + trmKey + sep + phsKey
  else
    tag := attKey;
  ref := 0;
  if idxRoot <= 0 then begin
    att := fncsNoAttribute;
    exit;
  end;
  if att = fncsVoltage then begin
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

FUNCTION  InterpretStopTimeForFNCS(const s:string):fncs_time;
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
         
procedure TFNCS.SetPublishInterval (val: Integer);
begin
  if val > 0 then PublishInterval := val;
end;

procedure TFNCS.SetPublishMode (val: string);
var
  tok: string;
begin
  tok := LowerCase(val);
  if tok = 'json' then
    PublishMode := fncsPublishJSON
  else if tok = 'text' then
    PublishMode := fncsPublishText;
end;

// for performance reasons, we avoid concatenating strings or calling the Format function here         
procedure TFNCS.TopicsListToPublication;
var
  top: TFNCSTopic;
  sub: TFNCSSubTopic;
  Flow, Volts: Complex;
  sign: String;
  pElem :TDSSCktElement;
  pXf: TTransfObj;
  pStore: TStorageObj;
  cBuffer :pComplexArray;
  k, kmax, idxWdg, tap: integer;
  key, val: PChar;
  firstObjectFlag:Boolean=true;
  writeKeyComma:Boolean=false;
  pos1,pos2,i:Int64;
begin
  ET.Clear;
  ET.Start;
  if log_level >= fncsLogDebug3 then writeln ('Entering TopicsListToPublication');
  if PublishMode = fncsPublishJSON then begin
    pos1 := fncsOutputStream.Position;
    fncsOutputStream.Seek (0, soFromBeginning);
//    fncsOutputStream.WriteString ('{"'+FedName+'":{');
    fncsOutputStream.WriteString ('{"');
    fncsOutputStream.WriteString (FedName);
    fncsOutputStream.WriteString ('":{');
  end;
  kmax := GetMaxCktElementSize;
  Getmem(cBuffer, sizeof(cBuffer^[1])*kmax);
  for k := 1 to kmax do begin
    cBuffer^[k].re := 0.0;
    cBuffer^[k].im := 0.0;
  end;
  for top in topicList do begin
    if PublishMode = fncsPublishJSON then begin
      if not firstObjectFlag then fncsOutputStream.WriteString (',');
//      fncsOutputStream.WriteString (Format('"%s":{', [top.tag]));
      fncsOutputStream.WriteString ('"');
      fncsOutputStream.WriteString (top.tag);
      fncsOutputStream.WriteString ('":{');
      firstObjectFlag := False;
    end;
    for sub in top.sub do begin
      if PublishMode = fncsPublishJSON then
        key := PChar (sub.tag)
      else
        key := PChar (sub.text_key);
      val := nil;
      if sub.att = fncsVoltage then begin
        Volts := ActiveCircuit.Solution.NodeV^[sub.ref];
        if Volts.im < 0 then
          sign:=''
        else
          sign:='+';
//        val := PChar (Format('%.3f%s%.3f%s', [Volts.re, sign, Volts.im, 'j']));
        val := PChar (FloatToStrF(Volts.re, ffFixed, 0, 3) + sign + FloatToStrF(Volts.im, ffFixed, 0, 3) + 'j');
      end else if (sub.att = fncsCurrent) or (sub.att = fncsPower) then begin
        pElem := ActiveCircuit.CktElements.Get(top.idx);
        pElem.GetCurrents(cBuffer);
        if (sub.att = fncsCurrent) then begin
          Flow := cBuffer^[sub.ref];
        end else begin
          Volts := ActiveCircuit.Solution.NodeV^[pElem.NodeRef^[sub.ref]];
          Flow:=Cmul(Volts, conjg(cBuffer^[sub.ref]));
          if ActiveCircuit.PositiveSequence then Flow:=CmulReal(Flow, 3.0);
        end;
        if Flow.im < 0 then
          sign:=''
        else
          sign:='+';
//        val := PChar (Format('%.3f%s%.3f%s', [Flow.re, sign, Flow.im, 'i']));
        val := PChar (FloatToStrF(Flow.re, ffFixed, 0, 3) + sign + FloatToStrF(Flow.im, ffFixed, 0, 3) + 'j');
      end else if (sub.att = fncsSwitchState) then begin
        pElem := ActiveCircuit.CktElements.Get(top.idx);
        if AllTerminalsClosed (pElem) then 
          val := '1'
        else
          val := '0'
      end else if (sub.att = fncsTapPosition) then begin
        pXf := TTransfObj (ActiveCircuit.CktElements.Get(top.idx));
        idxWdg := 2; // TODO: identify and map this using pReg.Transformer and pReg.TrWinding
        tap := Round((pXf.PresentTap[idxWdg]-(pXf.Maxtap[idxWdg]+pXf.Mintap[idxWdg])/2.0)/pXf.TapIncrement[idxWdg]);
        val := PChar (IntToStr (tap));
      end else if (sub.att = fncsEnergy) then begin
        pStore := TStorageObj (ActiveCircuit.CktElements.Get(top.idx));
        val := PChar (FloatToStrF(pStore.StorageVars.kwhStored, ffFixed, 0, 3));
      end;
      if assigned(val) then begin
        if PublishMode = fncsPublishJSON then begin
          if writeKeyComma then fncsOutputStream.WriteString (',');
          writeKeyComma := True;
//          fncsOutputStream.WriteString (Format ('"%s":"%s"', [key, val]));
          fncsOutputStream.WriteString ('"');
          fncsOutputStream.WriteString (key);
          fncsOutputStream.WriteString ('":"');
          fncsOutputStream.WriteString (val);
          fncsOutputStream.WriteString ('"');
        end else begin
          fncs_publish (key, val);
          if log_level >= fncsLogDebug3 then writeln(Format ('Publish %s = %s', [key, val]));
        end;
      end;
    end;
    if PublishMode = fncsPublishJSON then begin
      fncsOutputStream.WriteString ('}');
      writeKeyComma:=False;
    end;
  end;
  if PublishMode = fncsPublishJSON then begin
    fncsOutputStream.WriteString ('}}');
    pos2 := fncsOutputStream.Position;
    if pos2 < pos1 then
      for i := 1 to (pos1 - pos2) do
        fncsOutputStream.WriteString (' ');
    fncs_publish ('fncs_output', PChar(fncsOutputStream.DataString));
  end;
  ETFncsPublish := ETFncsPublish + ET.Elapsed;
  ET.Clear;
end;

procedure TFNCS.DumpFNCSLists;
var
  top: TFNCSTopic;
  sub: TFNCSSubTopic;
begin
  writeln('***DumpFNCSLists');
  for top in topicList do begin
    writeln(Format('  %2d %5d %s',[top.cls, top.idx, top.tag]));
    for sub in top.sub do begin
      writeln(Format('    %2d %2d %5d %s',[sub.att, sub.trm, sub.ref, sub.tag]));
    end;
  end;
end;

procedure TFNCS.ReadFncsPubConfig (fname: string);
var
  buf: String;
begin
  ET.Clear;
  ET.Start;
  buf := '   ';
  fncsOutputStream:=TStringStream.Create(buf);
  next_fncs_publish := 0;

  if Pos ('.json', ExtractFileExt (LowerCase (fname))) > 0 then
    ReadFncsJsonConfig (fname)
  else
    ReadFncsTextConfig (fname);
  if log_level >= fncsLogInfo then begin
    Writeln('  Exiting ReadFncsPubConfig');
    system.flush (stdout);
  end;
  ETFncsReadPub := ETFncsReadPub + ET.Elapsed;
  ET.Clear;
end;

procedure TFNCS.ReadFncsTextConfig (fname: string);
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
    if log_level >= fncsLogInfo then begin
      Writeln(Format('  ReadFncsTextConfig processed %u lines', [lines.count]));
      system.flush (stdout);
    end;
    lines.free;
  end;
  if log_level >= fncsLogInfo then begin
    Writeln('  Exiting ReadFncsTextConfig');
    system.flush (stdout);
  end;
end;

procedure TFNCS.ReadFncsJsonConfig (fname: string);
var
  inputfile:TFileStream;
  parser:TJSONParser;
  config:TJSONData;
  el,attri,cls,obj,terminal,conductor:TJSONEnum;
  attriKey, clsKey, objKey, terminalKey, condKey:string;
  top: TFNCSTopic;
  sub: TFNCSSubTopic;
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
              top := TFNCSTopic.Create (clsKey, objKey);
              topicList.Add(top);
              for attri in obj.Value do begin
                attriKey := LowerCase(attri.Key);
                if attri.Value is Tjsonarray then begin
                  if attri.Value.Count=0 then begin
                    sub := TFNCSSubTopic.Create (attriKey, '-1', '', top.idx); // switchstate, tapposition, etc.
                    top.sub.Add(sub);
                  end else
                    for conductor in attri.Value do begin
                      condKey := conductor.Value.asstring;
                      sub := TFNCSSubTopic.Create (attriKey, '1', condKey, top.idx);
                      top.sub.Add(sub);
                    end;
                end else begin  // attri.Value is not a TJSONArray
                  for terminal in attri.Value do begin
                    terminalKey:=LowerCase(terminal.Key);
                    for conductor in terminal.Value do begin
                      condKey := conductor.Value.asstring;
                      sub := TFNCSSubTopic.Create (attriKey, terminalKey, condKey, top.idx);
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

  if log_level >= fncsLogInfo then
    writeln('Done reading FNCS publication requests from: ' + fname);
  if log_level >= fncsLogDebug1 then begin
    DumpFNCSLists;
  end;
end;

// called from ActiveSolution.Increment_time
function TFNCS.FncsTimeRequest (next_fncs:fncs_time): Boolean;
var
  time_granted: fncs_time;
  key, value: pchar;
  i: integer;
  ilast: size_t;
  nvalues, ival: size_t;
  re, im: double;
  ld: TLoadObj;
  Hour: integer;
  Sec: double;
begin
  // execution blocks here, until FNCS permits the time step loop to continue
  time_granted := 0;
  while time_granted < next_fncs do begin
    ET.Clear;
    ET.Start;
    time_granted := fncs_time_request (next_fncs);
    ETFncsTimeRequest := ETFncsTimeRequest + ET.Elapsed;
    ET.Clear;
    if log_level >= fncsLogDebug2 then begin
      Writeln(Format('  Already granted %u by FNCS and requested %u, granted %u', [existing_fncs_grant, next_fncs, time_granted]));
      system.flush (stdout);
    end;
    Hour := ActiveCircuit.Solution.DynaVars.intHour;
    Sec :=  ActiveCircuit.Solution.Dynavars.t;
    if time_granted >= next_fncs_publish then begin
      if topicList.Count > 0 then begin
        if log_level >= fncsLogDebug2 then begin
          Writeln(Format('  Stream size %u at %u, next at %u, interval %u, %d:%.3f', 
            [fncsOutputStream.size, time_granted, next_fncs_publish, PublishInterval, Hour, Sec]));
          system.flush (stdout);
        end;
        TopicsListToPublication;
      end;
      while next_fncs_publish <= time_granted do
        next_fncs_publish := next_fncs_publish + PublishInterval;
    end;
    ET.Clear;
    ET.Start;
    ilast := fncs_count_events();
    // TODO: executing OpenDSS commands here may cause unwanted interactions
    for i := 1 to ilast do begin
      key := fncs_next_event();
      nvalues := fncs_count_values (key);
      if CompareText (key, 'command') = 0 then begin
        for ival := 1 to nvalues do begin
          value := fncs_next_value();
          if log_level >= fncsLogDebug2 then begin
            writeln(Format('  FNCSTimeRequest command %s at %u, %d:%.3f', 
              [value, time_granted, Hour, Sec]));
            system.flush (stdout);
          end;
          DSSExecutive.Command := value;
        end;
      end else if Pos ('#load', key) > 0 then begin
        value := fncs_next_value();
        re := StrToFloat (ExtractWord (1, value, ['+', 'j', ' ']));
        im := StrToFloat (ExtractWord (2, value, ['+', 'j', ' ']));
        ActiveCircuit.SetElementActive ('load.F1_house_B0');
        ld := TLoadObj (ActiveCircuit.ActiveCktElement);
        ld.LoadSpecType := 1;
        ld.kwBase := re;
        ld.kvarBase := im;
        ld.RecalcElementData;
        if log_level >= fncsLogDebug2 then begin
          writeln(Format ('FNCS Request %s to %g + j %g at %u, %d:%.3f', 
            [ld.Name, re, im, time_granted, Hour, Sec]));
          system.flush (stdout);
        end;
      end;
    end;
    ETFncsGetEvents := ETFncsGetEvents + ET.Elapsed;
    ET.Clear;
  end;
  Result := True;
end;

procedure TFNCS.RunFNCSLoop (const s:string);
var
  time_granted, time_stop: fncs_time;
  key, value: pchar;
  i: integer;
  ilast: size_t;
  nvalues, ival: size_t;
begin
  time_granted := 0;
  time_stop := InterpretStopTimeForFNCS(s);
  if log_level >= fncsLogInfo then
    writeln(Format('Starting FNCS loop to run %s or %u seconds', [s, time_stop]));
  ET.Clear;
  ET.Start;
  fncs_initialize;
  ETFncsLoad := ETFncsLoad + ET.Elapsed;
  ET.Clear;
  in_fncs_loop := True;

  Try
    while time_granted < time_stop do begin
      ET.Clear;
      ET.Start;
      time_granted := fncs_time_request (time_stop);
      ETFncsTimeRequest := ETFncsTimeRequest + ET.Elapsed;
      ET.Clear;
      existing_fncs_grant := time_granted;
      ET.Start;
      ilast := fncs_count_events();
      for i := 1 to ilast do begin
        key := fncs_next_event();
        nvalues := fncs_count_values (key);
        if CompareText (key, 'command') = 0 then begin
          for ival := 1 to nvalues do begin
            value := fncs_next_value();
            if log_level >= fncsLogDebug1 then begin
              writeln(Format('FNCS command %s at %u, val %u of %u, evt %u of %u', 
                [value, time_granted, ival, nvalues, i, ilast]));
              system.flush (stdout);
            end;
            DSSExecutive.Command := value;
            if log_level >= fncsLogDebug1 then begin
              writeln(Format('Finished with %s at %u, val %u of %u, evt %u of %u', 
                [value, time_granted, ival, nvalues, i, ilast]));
              system.flush (stdout);
            end;
          end;
        end else if Pos ('#load', key) > 0 then begin
          value := fncs_next_value();
          if log_level >= fncsLogDebug1 then begin
            writeln(Format ('FNCS Loop %s to %s', [key, value]));
            system.flush (stdout);
          end;
        end;
      end;
      ETFncsGetEvents := ETFncsGetEvents + ET.Elapsed;
      ET.Clear;
      existing_fncs_grant := 0;
    end;
  finally
    in_fncs_loop := False;
    writeln(Format('FNCS Timing: LoadLib=%.6f, ReadConfig=%.6f, TimeRequests=%.6f, GetEvents=%.6f, Publish=%.6f', 
      [ETFncsLoad, ETFncsReadPub, ETFncsTimeRequest, ETFncsGetEvents, ETFncsPublish]));
    fncs_finalize;
  end;
end;

function TFNCS.IsReady:Boolean;
begin
  Result := True;
  if FLibHandle = DynLibs.NilHandle then Result := False;
end;

function TFNCS.IsRunning:Boolean;
begin
  Result := in_fncs_loop;
end;

function TFNCS.find_fncs_function (name: String): Pointer;
begin
  Result := GetProcedureAddress (FLibHandle, name);
  if Result = nil then begin
    writeln ('FNCS library found, but missing function ', name);
    FuncError := True;
  end;
end;

constructor TFNCS.Create;
var 
  s: String;
begin
  sep := '/';
  PublishInterval := 1;
  PublishMode := fncsPublishJSON;
  ET := TEpikTimer.Create(nil);
  ETFncsReadPub := 0.0;
  ETFncsPublish := 0.0;
  ETFncsLoad := 0.0;
  ETFncsGetEvents := 0.0;
  ETFncsTimeRequest := 0.0;
  existing_fncs_grant := 0;
  in_fncs_loop := False;
  log_level := fncsLogWarning;
  s := GetEnvironmentVariable ('FNCS_LOG_LEVEL');
  if s = 'INFO' then 
    log_level := fncsLogInfo
  else if s = 'DEBUG' then
    log_level := fncsLogDebug1
  else if s = 'DEBUG1' then
    log_level := fncsLogDebug1
  else if s = 'DEBUG2' then
    log_level := fncsLogDebug2
  else if s = 'DEBUG3' then
    log_level := fncsLogDebug3
  else if s = 'DEBUG4' then
    log_level := fncsLogDebug4;

  ET.Clear;
  ET.Start;
  FLibHandle := SafeLoadLibrary ('libfncs.' + SharedSuffix);
  topicList:=TList.Create();
  if FLibHandle <> DynLibs.NilHandle then begin
    FuncError := False;
    @fncs_initialize := find_fncs_function ('fncs_initialize');
    if not FuncError then @fncs_initialize_config := find_fncs_function ('fncs_initialize_config');
    if not FuncError then @fncs_agentRegister := find_fncs_function ('fncs_agentRegister');
    if not FuncError then @fncs_agentRegisterConfig := find_fncs_function ('fncs_agentRegisterConfig');
    if not FuncError then @fncs_is_initialized := find_fncs_function ('fncs_is_initialized');
    if not FuncError then @fncs_time_request := find_fncs_function ('fncs_time_request');
    if not FuncError then @fncs_publish := find_fncs_function ('fncs_publish');
    if not FuncError then @fncs_publish_anon := find_fncs_function ('fncs_publish_anon');
    if not FuncError then @fncs_agentPublish := find_fncs_function ('fncs_agentPublish');
    if not FuncError then @fncs_route := find_fncs_function ('fncs_route');
    if not FuncError then @fncs_die := find_fncs_function ('fncs_die');
    if not FuncError then @fncs_finalize := find_fncs_function ('fncs_finalize');
    if not FuncError then @fncs_update_time_delta := find_fncs_function ('fncs_update_time_delta');
    if not FuncError then @fncs_get_events_size := find_fncs_function ('fncs_get_events_size');
    if not FuncError then @fncs_get_events := find_fncs_function ('fncs_get_events');
    if not FuncError then @fncs_get_event_at := find_fncs_function ('fncs_get_event_at');
    if not FuncError then @fncs_agentGetEvents := find_fncs_function ('fncs_agentGetEvents');
    if not FuncError then @fncs_get_value := find_fncs_function ('fncs_get_value');
    if not FuncError then @fncs_get_values_size := find_fncs_function ('fncs_get_values_size');
    if not FuncError then @fncs_get_values := find_fncs_function ('fncs_get_values');
    if not FuncError then @fncs_get_value_at := find_fncs_function ('fncs_get_value_at');
    if not FuncError then @fncs_get_keys_size := find_fncs_function ('fncs_get_keys_size');
    if not FuncError then @fncs_get_keys := find_fncs_function ('fncs_get_keys');
    if not FuncError then @fncs_get_key_at := find_fncs_function ('fncs_get_key_at');
    if not FuncError then @fncs_get_name := find_fncs_function ('fncs_get_name');
    if not FuncError then @fncs_get_id := find_fncs_function ('fncs_get_id');
    if not FuncError then @fncs_get_simulator_count := find_fncs_function ('fncs_get_simulator_count');
    if not FuncError then @fncs_get_version := find_fncs_function ('fncs_get_version');
    if not FuncError then @fncs_free := find_fncs_function ('_fncs_free');
    if not FuncError then @fncs_count_events := find_fncs_function ('fncs_count_events');
    if not FuncError then @fncs_count_values := find_fncs_function ('fncs_count_values');
    if not FuncError then @fncs_next_event := find_fncs_function ('fncs_next_event');
    if not FuncError then @fncs_next_value := find_fncs_function ('fncs_next_value');
    if FuncError then begin
      UnloadLibrary(FlibHandle);
      FLibHandle := DynLibs.NilHandle;
    end;
  end;
  ETFncsLoad := ETFncsLoad + ET.Elapsed;
  ET.Clear;
end;

destructor TFNCS.Destroy;
begin
  in_fncs_loop := False;
  topicList.Free;
  If FLibHandle <> DynLibs.NilHandle Then Begin
    UnloadLibrary(FLibHandle);
  End;
  inherited;
end;

end.

