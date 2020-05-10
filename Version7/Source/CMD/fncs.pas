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
  UComplex, generics.collections, CktElement, Utilities, math, fgl;

type
  fncs_time = qword;

  // dictionaries of FNCS output topics, with indices into the corresponding OpenDSS lists
  ConductorValueDict = TDictionary<string, string>;
  TerminalConductorDict = TObjectDictionary<string, ConductorValueDict>;
  AttributeTerminalDict = TObjectDictionary<string, TerminalConductorDict>;
  TFNCSMap = class(TObject)
  public
    atd: AttributeTerminalDict; // hierarchy of attributes, terminals and values to publish
    idx: Integer;             // cached index of oad.key into ActiveCircuit.BusList or DeviceList
    constructor Create();
  end;
  ObjectAttributeDict = TObjectDictionary<string, TFNCSMap>;
  ClassObjectDict = TObjectDictionary<string, ObjectAttributeDict>;

	// lists for FNCS classic publications
	TFNCSClass = (fncsBus, fncsLine, fncsSwitch, fncsCapacitor, fncsPVSystem, 
	              fncsVSource, fncsTransformer, fncsFault);
	TFNCSAttribute = (fncsVoltage, fncsCurrent, 
	                    fncsPower, fncsSwitchState, fncsTapPosition);
	TFNCSLogLevel = (fncsLogWarning, fncsLogInfo, fncsLogDebug1, fncsLogDebug2,
			fncsLogDebug3, fncsLogDebug4);
	TFNCSTopic = class (TObject)
  public
		tag: String;            // what FNCS calls it
		cls: TFNCSClass;
		att: TFNCSAttribute;
		idx: Integer;           // index into BusList or DeviceList
		ref: Integer;           // index into the phases
		constructor Create();
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

    function find_fncs_function (name: String): Pointer;

	private
		in_fncs_loop:Boolean;
    next_fncs_publish: fncs_time;
		existing_fncs_grant: fncs_time;
		log_level: TFNCSLogLevel;
    topics: ClassObjectDict;
//		topicList: TList;
    fncsOutputStream: TStringStream;
		procedure ReadFNCSJsonConfig (fname: string);
		procedure ReadFNCSTextConfig (fname: string);

  public
    PublishInterval:Integer;
    PublishMode:string;
    FedName:string;
    FNCSTopicsMapped:Boolean;

    function IsReady:Boolean;
		function IsRunning:Boolean;
    procedure RunFNCSLoop (const s:string);
    constructor Create();
    destructor Destroy; override;
    function FncsTimeRequest (next_fncs:fncs_time):Boolean;
    procedure ReadFNCSPubConfig (fname: string);
    procedure MapFNCSTopics;
    procedure DumpFNCSTopics;
    procedure GetVoltagesForTopics (oad:ObjectAttributeDict);
		procedure GetBranchesForTopics (oad:ObjectAttributeDict);
    procedure GetValuesForTopics;
    procedure TopicsToJsonStream;
//    function RoundToSignificantFigure(value:double;digit:Integer):double;
  end;

var
  ActiveFNCS:TFNCS;

implementation

uses
  fpjson, jsonparser, jsonscanner, strutils, Transformer, Load, Storage; // RegControl,ControlElem;

constructor TFNCSTopic.Create;
begin
	tag := '';
	cls := fncsBus;
	att := fncsVoltage;
	idx := 0;
	ref := 0;
end;

constructor TFNCSMap.Create;
begin
  idx := 0;
  atd := AttributeTerminalDict.create([doOwnsValues]);
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
         
{*         
function TFNCS.RoundToSignificantFigure(value:double;digit:Integer):double;
var
  factor:double=1.0;
begin
  RoundToSignificantFigure:=value;
  if value = 0.0 then
    exit;
  factor:=power(10.0,digit-ceil(log10(abs(value))));
  if factor = 0.0 then
    exit;
  RoundToSignificantFigure:=round(value*factor)/factor;
end;
*}
procedure TFNCS.GetValuesForTopics;
begin
	if topics.containsKey('bus') then GetVoltagesForTopics (topics.Items['bus']);
	if topics.containsKey('line') then GetBranchesForTopics (topics.Items['line']);
	if topics.containsKey('vsource') then GetBranchesForTopics (topics.Items['vsource']);
	if topics.containsKey('fault') then GetBranchesForTopics (topics.Items['fault']);
	if topics.containsKey('capacitor') then GetBranchesForTopics (topics.Items['capacitor']);
	if topics.containsKey('load') then GetBranchesForTopics (topics.Items['load']);
	if topics.containsKey('pvsystem') then GetBranchesForTopics (topics.Items['pvsystem']);
	if topics.containsKey('transformer') then GetBranchesForTopics (topics.Items['transformer']);
	if topics.containsKey('storage') then GetBranchesForTopics (topics.Items['storage']);
end;
         
procedure TFNCS.GetBranchesForTopics (oad:ObjectAttributeDict);
var
  objKey, attKey, trmKey, valKey, dssName: String;
  map: TFNCSMap;
  atd: AttributeTerminalDict;
  tcd: TerminalConductorDict;
  cvd: ConductorValueDict;
	idxDev, idxTerm, idxPhs: Integer;
	Flow, Volts: Complex;
	sign: String;
  pElem :TDSSCktElement;
	cBuffer :pComplexArray;
	Ncond, Nterm, kmax, k, Nref: Integer;
	PhaseTable: array[1..2, 0..3] of Integer; // index into cBuffer by terminal, then phase
	pXf: TTransfObj;
	pStore: TStorageObj;
	idxWdg: Integer;
	tap: Integer;
begin
	kmax := GetMaxCktElementSize;
	Getmem(cBuffer, sizeof(cBuffer^[1])*kmax);
	for k := 1 to kmax do begin
		cBuffer^[k].re := 0.0;
		cBuffer^[k].im := 0.0;
	end;
	for objKey in oad.Keys do begin
		map := oad.Items[objKey];
		atd := map.atd;
		idxDev := map.idx;
		if idxDev <= 0 then continue;
		pElem := ActiveCircuit.CktElements.Get(idxDev);
		dssName := pElem.DSSClassName + '.' + ActiveCircuit.DeviceList.get(idxDev);
		NCond := pElem.NConds;
		Nterm := pElem.Nterms;
		kmax := Ncond * Nterm;
		for k :=  0 to 3 do begin
			PhaseTable[1, k] := 0;
			PhaseTable[2, k] := 0;
		end;
		for k := 1 to kmax do begin
			idxPhs := GetNodeNum (pElem.NodeRef^[k]);
			if k > Ncond then
				PhaseTable[2, idxPhs] := k
			else
				PhaseTable[1, idxPhs] := k;
		end;
		pElem.GetCurrents(cBuffer);
		for attKey in atd.Keys do begin
			tcd := atd.Items[attKey];
			for trmKey in tcd.Keys do begin
				cvd := tcd.Items[trmKey];
				idxTerm := StrToInt (trmKey);
				for valKey in cvd.Keys do begin
					idxPhs := 1 + Ord(valKey[1]) - Ord('A');
					if (idxPhs > 3) or (idxPhs < 1) then idxPhs := 0;
					Flow.re := 0.0;
					Flow.im := 0.0;
					if attKey = 'current' then begin
						k := PhaseTable [idxTerm, idxPhs];
					  Flow := cBuffer^[k];
						if Flow.im < 0 then
							sign:=''
						else
							sign:='+';
						cvd[valKey] := Format('%.3f%s%.3f%s', [Flow.re, sign, Flow.im, 'i']);
					end else if attKey = 'power' then begin
						k := PhaseTable [idxTerm, idxPhs];
						Volts := ActiveCircuit.Solution.NodeV^[pElem.NodeRef^[k]];
						Flow:=Cmul(Volts, conjg(cBuffer^[k]));
						if ActiveCircuit.PositiveSequence then Flow:=CmulReal(Flow, 3.0);
						if Flow.im < 0 then
							sign:=''
						else
							sign:='+';
						cvd[valKey] := Format('%.3f%s%.3f%s', [Flow.re, sign, Flow.im, 'i']);
					end else if attKey = 'tapposition' then begin
						pXf := TTransfObj (pElem);
						idxWdg := 2; // TODO: identify and map this using pReg.Transformer and pReg.TrWinding
						tap := Round((pXf.PresentTap[idxWdg]-(pXf.Maxtap[idxWdg]+pXf.Mintap[idxWdg])/2.0)/pXf.TapIncrement[idxWdg]);
 						cvd[valKey] := Format ('%d', [tap]);
					end else if attKey = 'switchstate' then begin
						if AllTerminalsClosed (pElem) then 
							cvd[valKey] := '1'
						else
							cvd[valKey] := '0'
					end else if attKey = 'kwhstored' then begin
						pStore := TStorageObj (pElem);
						cvd[valKey] := Format ('%.3f', [pStore.StorageVars.kwhStored]);
					end;
				end;
			end;
		end;
	end;
	if Assigned (cBuffer) then FreeMem (cBuffer);
end;

procedure TFNCS.GetVoltagesForTopics (oad:ObjectAttributeDict);
var
  objKey, attKey, trmKey, valKey, dssName: String;
  map: TFNCSMap;
  atd: AttributeTerminalDict;
  tcd: TerminalConductorDict;
  cvd: ConductorValueDict;
	idxBus, idxPhs, idxLoc, idxNode: Integer;
	Volts: Complex;
	sign: String;
begin
	for objKey in oad.Keys do begin
		map := oad.Items[objKey];
		atd := map.atd;
		idxBus := map.idx;
		if idxBus <= 0 then continue;
		dssName := ActiveCircuit.BusList.get(idxBus);
		for attKey in atd.Keys do begin
			tcd := atd.Items[attKey];
			for trmKey in tcd.Keys do begin
				cvd := tcd.Items[trmKey];
				for valKey in cvd.Keys do begin
					idxPhs := 1 + Ord(valKey[1]) - Ord('A');
					idxLoc := ActiveCircuit.Buses^[idxBus].FindIdx(idxPhs);
          idxNode := ActiveCircuit.Buses^[idxBus].GetRef(idxLoc);
					Volts := ActiveCircuit.Solution.NodeV^[idxNode];
					if Volts.im < 0 then
						sign:=''
					else
						sign:='+';
//					writeln(Format('Bus %s %s %s %s %d %d %d %g %g', 
//						 [dssName, attKey, trmKey, valKey, idxPhs, idxLoc, idxNode, Volts.re, Volts.im]));
//					cvd[valKey] := RoundToSignificantFigure(Volts.re,6).ToString
//					  + sign + RoundToSignificantFigure(Volts.im,6).ToString+'i';
					cvd[valKey] := Format('%.3f%s%.3f%s', [Volts.re, sign, Volts.im, 'i']);
				end;
			end;
		end;
	end;
end;

procedure TFNCS.DumpFNCSTopics;
var
  clsKey, objKey, attKey, trmKey, valKey: String;
  map:TFNCSMap;
  oad:ObjectAttributeDict;
  atd:AttributeTerminalDict;
  tcd:TerminalConductorDict;
  cvd:ConductorValueDict;
begin
  for clsKey in topics.Keys do begin
    writeln('  ' + clsKey);
    oad := topics.Items[clsKey];
    for objKey in oad.Keys do begin
      writeln('    ' + objKey);
      map := oad.Items[objKey];
      atd := map.atd;
      for attKey in atd.Keys do begin
        writeln('      ' + attKey);
        tcd := atd.Items[attKey];
        for trmKey in tcd.Keys do begin
          writeln('        ' + trmKey);
          cvd := tcd.Items[trmKey];
          for valKey in cvd.Keys do begin
            writeln('          ' + valKey);
          end;
        end;
      end;
    end;
  end;
end;

procedure TFNCS.MapFNCSTopics;
var
  clsKey, objKey, attKey, trmKey, valKey: String;
  map:TFNCSMap;
  oad:ObjectAttributeDict;
  atd:AttributeTerminalDict;
  tcd:TerminalConductorDict;
  cvd:ConductorValueDict;
begin
  FNCSTopicsMapped := True;
  for clsKey in topics.Keys do begin
    oad := topics.Items[clsKey];
    for objKey in oad.Keys do begin
      map := oad.Items[objKey];
      if clsKey = 'bus' then
        map.idx := ActiveCircuit.BusList.Find (objKey)
      else
        map.idx := ActiveCircuit.SetElementActive (clsKey + '.' + objKey);
      if map.idx = 0 then writeln ('*** can not find FNCS output for ' + clsKey + ':' + objKey);
      atd := map.atd;
      for attKey in atd.Keys do begin
        tcd := atd.Items[attKey];
        for trmKey in tcd.Keys do begin
          cvd := tcd.Items[trmKey];
          for valKey in cvd.Keys do begin
          end;
        end;
      end;
    end;
  end;
end;

procedure TFNCS.ReadFncsPubConfig (fname: string);
var
	buf: String;
begin
	buf := '   ';
	fncsOutputStream:=TStringStream.Create(buf);
	next_fncs_publish := 0;
	FNCSTopicsMapped := False;

	if Pos ('.json', ExtractFileExt (LowerCase (fname))) > 0 then
		ReadFncsJsonConfig (fname)
	else
		ReadFncsTextConfig (fname);
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
	finally
		lines.free;
	end;
end;

procedure TFNCS.ReadFncsJsonConfig (fname: string);
var
  inputfile:TFileStream;
  parser:TJSONParser;
  config:TJSONData;
  el,attri,cls,obj,terminal,conductor:TJSONEnum;
  attriKey, clsKey, objKey, terminalKey, condKey:string;
  map:TFNCSMap;
  oad:ObjectAttributeDict;
  atd:AttributeTerminalDict;
  tcd:TerminalConductorDict;
  cvd:ConductorValueDict;
begin
  inputfile:=TFileStream.Create(fname, fmOpenRead);
  try
    parser:=TJSONParser.Create(inputfile, [joUTF8]);
    try
      config:=parser.Parse;
      for el in config do begin
        if el.Key = 'name' then
          FedName:=el.Value.AsString
        else if el.Key = 'publishInterval' then
          PublishInterval:=el.Value.AsInteger
        else if el.Key = 'publishMode' then
          PublishMode:=el.Value.AsString
        else if el.Key = 'topics' then begin
          for cls in el.Value do begin
            clsKey:=LowerCase(cls.Key);
            if topics.ContainsKey(clsKey) then begin
              oad := topics[clsKey];
            end else begin
              oad := ObjectAttributeDict.create([doOwnsValues]);
              topics.AddOrSetValue(clsKey,oad);
            end;
            for obj in cls.Value do begin
              objKey:=LowerCase(obj.Key);
              if oad.ContainsKey(objKey) then begin
                map := oad[objKey];
                atd := map.atd;
              end else begin
                map := TFNCSMap.create();
                atd := map.atd;
                oad.AddOrSetvalue(objKey,map);
              end;
              for attri in obj.Value do begin
                attriKey:=LowerCase(attri.Key);
                if atd.ContainsKey(attriKey) then begin
                  tcd := atd[attriKey];
                end else begin
                  tcd := TerminalConductorDict.Create([doOwnsValues]);
                  atd.AddOrSetvalue(attriKey,tcd);
                end;
                if attri.Value is Tjsonarray then begin
                  terminalKey:='1';
                  if tcd.ContainsKey(terminalKey) then begin
                    cvd := tcd[terminalKey];
                  end else begin
                    cvd := ConductorValueDict.Create;
                    tcd.AddOrSetvalue(terminalKey, cvd);
                  end;
                  if attri.Value.count=0 then cvd.Add('-1', '');
                  for conductor in attri.Value do begin
                    condKey := conductor.Value.asstring;
                    if not cvd.ContainsKey(condKey) then begin
                      cvd.Add(condKey, '');
                    end;
									end;
                end else begin  // attri.Value is not a TJSONArray
                  for terminal in attri.Value do begin
                    terminalKey:=LowerCase(terminal.Key);
                    if tcd.ContainsKey(terminalKey) then begin
                      cvd := tcd[terminalKey];
                    end else begin
                      cvd := ConductorValueDict.Create;
                      tcd.AddOrSetvalue(terminalKey, cvd);
                    end;
                    for conductor in terminal.Value do begin
                      condKey := conductor.Value.asstring;
                      if not cvd.ContainsKey(condKey) then begin
                        cvd.Add(condKey, '');
                      end;
                    end; // terminal.Value
                  end;
                end;  // attri.Value
              end; // attri
            end; // obj
          end; // cls
        end // el.key is topics
        else
          Writeln('*** unknown key "' + el.Key + '" found in FNCS config file.');
      end; // el
    finally
      parser.Free;
    end;
  finally
    inputfile.Free;
  end;
	if log_level >= fncsLogInfo then
		writeln('Done reading FNCS publication requests from: ' + fname);
  if log_level >= fncsLogDebug3 then
		DumpFNCSTopics;
end;

procedure TFNCS.TopicsToJsonStream;
var
  attri:TPair<string,TerminalConductorDict>;
  cls:TPair<string,ObjectAttributeDict>;
  map:TPair<string,TFNCSMap>;
  atd:AttributeTerminalDict;
  terminal:TPair<string,ConductorValueDict>;
  conductor:TPair<string,string>;
  firstObjectFlag:Boolean=true;
  writeKeyComma:Boolean=false;
	pos1,pos2,i:Int64;
begin
  if topics.Count > 0 then begin
		pos1 := fncsOutputStream.Position;
		fncsOutputStream.Seek (0, soFromBeginning);
		fncsOutputStream.WriteString ('{"'+FedName+'":{');
    for cls in topics do begin
      for map in cls.Value do begin
        atd := map.value.atd;
        if not firstObjectFlag then
          fncsOutputStream.WriteString (',');
        fncsOutputStream.WriteString ('"' + cls.Key+'.'+map.Key + '":{');
        for attri in atd do begin
          for terminal in attri.Value do begin
            for conductor in terminal.Value do begin
              if writeKeyComma then fncsOutputStream.WriteString (',');
              writeKeyComma := True;
              if attri.Value.count > 1 Then
                fncsOutputStream.WriteString ('"'+attri.Key+'.'+terminal.Key+'.'+conductor.Key+'"')
              else if conductor.Key='-1' Then
                fncsOutputStream.WriteString ('"'+attri.Key+'"')
              else
                fncsOutputStream.WriteString ('"'+attri.Key+'.'+conductor.Key+'"');
              fncsOutputStream.WriteString (':"'+conductor.Value+'"');
            end;
          end;
        end;
        fncsOutputStream.WriteString ('}');
        writeKeyComma:=False;
        firstObjectFlag:=False;
      end;
    end;
		fncsOutputStream.WriteString ('}}');
		pos2 := fncsOutputStream.Position;
		if pos2 < pos1 then
			for i := 1 to (pos1 - pos2) do
				fncsOutputStream.WriteString (' ');
	end;
end;

// called from ActiveSolution.Increment_time
function TFNCS.FncsTimeRequest (next_fncs:fncs_time): Boolean;
var
  time_granted: fncs_time;
  events: ppchar;
  key, value: pchar;
  i: integer;
  ilast: size_t;
  nvalues, ival: size_t;
  values: ppchar;
	re, im: double;
	ld: TLoadObj;
	Hour: integer;
	Sec: double;
begin
  // execution blocks here, until FNCS permits the time step loop to continue
	time_granted := 0;
	while time_granted < next_fncs do begin
		time_granted := fncs_time_request (next_fncs);
		if log_level >= fncsLogDebug2 then begin
			Writeln(Format('  Already granted %u by FNCS and requested %u, granted %u', [existing_fncs_grant, next_fncs, time_granted]));
			system.flush (stdout);
		end;
		Hour := ActiveCircuit.Solution.DynaVars.intHour;
		Sec :=  ActiveCircuit.Solution.Dynavars.t;
		if time_granted >= next_fncs_publish then begin
			if topics.Count > 0 then begin
				if log_level >= fncsLogDebug2 then begin
					Writeln(Format('  Stream size %u at %u, next at %u, interval %u, %d:%.3f', 
						[fncsOutputStream.size, time_granted, next_fncs_publish, PublishInterval, Hour, Sec]));
					system.flush (stdout);
				end;
				if Not FNCSTopicsMapped then MapFNCSTopics;
				GetValuesForTopics;
				TopicsToJsonStream;
				fncs_publish ('fncs_output', PChar(fncsOutputStream.DataString));
			end;
			while next_fncs_publish <= time_granted do
				next_fncs_publish := next_fncs_publish + PublishInterval;
		end;
		ilast := fncs_get_events_size();
		// TODO: executing OpenDSS commands here may cause unwanted interactions
		if ilast > 0 then begin
			events := fncs_get_events();
			for i := 0 to ilast-1 do begin
				key := events[i];
				nvalues := fncs_get_values_size (key);
				values := fncs_get_values (key);
				if CompareText (key, 'command') = 0 then begin
					for ival := 0 to nvalues-1 do begin
						value := values[ival];
						if log_level >= fncsLogDebug2 then begin
							writeln(Format('  FNCSTimeRequest command %s at %u, %d:%.3f', 
								[value, time_granted, Hour, Sec]));
							system.flush (stdout);
						end;
						DSSExecutive.Command := value;
//						fncs_publish('fncs_command', value);
					end;
				end else if Pos ('#load', key) > 0 then begin
					value := values[0];
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
		end;
	end;
	Result := True;
end;

procedure TFNCS.RunFNCSLoop (const s:string);
var
  time_granted, time_stop: fncs_time;
  events: ppchar;
  key, value: pchar;
  i: integer;
  ilast: size_t;
  nvalues, ival: size_t;
  values: ppchar;
begin
  time_granted := 0;
  time_stop := InterpretStopTimeForFNCS(s);
  if log_level >= fncsLogInfo then
		writeln(Format('Starting FNCS loop to run %s or %u seconds', [s, time_stop]));
  fncs_initialize;
	in_fncs_loop := True;

  Try
    while time_granted < time_stop do begin
      time_granted := fncs_time_request (time_stop);
			existing_fncs_grant := time_granted;
      ilast := fncs_get_events_size();
      if ilast > 0 then begin
        events := fncs_get_events();
        for i := 0 to ilast-1 do begin
          key := events[i];
          nvalues := fncs_get_values_size (key);
          values := fncs_get_values (key);
					if CompareText (key, 'command') = 0 then begin
						for ival := 0 to nvalues-1 do begin
							value := values[ival];
							if log_level >= fncsLogDebug1 then begin
								writeln(Format('FNCS command %s at %u', [value, time_granted]));
								system.flush (stdout);
							end;
							DSSExecutive.Command := value;
//							fncs_publish ('fncs_command', value);
							if log_level >= fncsLogDebug1 then begin
								writeln(Format('Finished with %s at %u', [value, time_granted]));
								system.flush (stdout);
							end;
						end;
					end else if Pos ('#load', key) > 0 then begin
						value := values[0];
						if log_level >= fncsLogDebug1 then begin
							writeln(Format ('FNCS Loop %s to %s', [key, value]));
							system.flush (stdout);
						end;
					end;
        end;
      end;
			existing_fncs_grant := 0;
    end;
  finally
		in_fncs_loop := False;
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

  FLibHandle := SafeLoadLibrary ('libfncs.' + SharedSuffix);
  topics:=ClassObjectDict.create([doOwnsValues]);
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
    if FuncError then begin
      UnloadLibrary(FlibHandle);
      FLibHandle := DynLibs.NilHandle;
    end;
  end;
end;

destructor TFNCS.Destroy;
begin
	in_fncs_loop := False;
  topics.free;
  If FLibHandle <> DynLibs.NilHandle Then Begin
    UnloadLibrary(FLibHandle);
  End;
  inherited;
end;

end.

