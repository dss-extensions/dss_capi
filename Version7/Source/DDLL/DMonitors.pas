unit DMonitors;

interface

function MonitorsI(mode: Longint; arg: Longint): Longint; CDECL;
function MonitorsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure MonitorsV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    Monitor,
    DSSGlobals,
    SysUtils,
    Classes,
    Variants,
    Math;

type
    THeaderRec = record
        Signature: Integer;
        Version: Integer;
        RecordSize: Integer;
        Mode: Integer;
        StrBuffer: TMonitorStrBuffer;
    end;

    SingleArray = array[1..100] of Single;
    pSingleArray = ^SingleArray;

procedure ReadMonitorHeader(var HeaderRec: THeaderRec; Opt: Boolean);
var
    pMon: TMonitorObj;

begin
    pMon := ActiveCircuit.Monitors.Active;
    try
        with pmon.MonitorStream, HeaderRec do
        begin
            Seek(0, classes.soFromBeginning);
            Read(signature, Sizeof(signature));    // Signature   (32 bit Integer )
            Read(version, Sizeof(version));        // Version     (32 bit Integer )
            Read(RecordSize, Sizeof(RecordSize));    // RecordSize  (32 bit Integer )
            Read(Mode, Sizeof(Mode));                // Mode        (32 bit Integer )
            Read(StrBuffer, Sizeof(TMonitorStrBuffer)); // String      (255 char string)
        end;

    finally
          // If opt is false leave monitorstream at end of header record
        if Opt then
            pmon.MonitorStream.Seek(0, soFromEnd);    // put monitor stream pointer back where it was
    end;


end;

function MonitorsI(mode: Longint; arg: Longint): Longint; CDECL;

var
    pMon: TMonitorObj;
    Header: THeaderRec;

begin
    Result := 0;  // Default return value
    case mode of
        0:
        begin  // Monitors.First
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.First;
                if pMon <> NIL then
                begin
                    repeat
                        if pMon.enabled then
                        begin
                            ActiveCircuit.ActiveCktElement := pMon;
                            Result := 1;
                        end
                        else
                            pMon := ActiveCircuit.Monitors.Next;
                    until (Result = 1) or (pMon = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        1:
        begin  // Monitors.Next
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Next;
                if pMon <> NIL then
                begin
                    repeat
                        if pMon.Enabled then
                        begin
                            ActiveCircuit.ActiveCktElement := pMon;
                            Result := 1;
                        end
                        else
                            pMon := ActiveCircuit.Monitors.Next;
                    until (Result > 0) or (pMon = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        2:
        begin  // Monitors.Reset
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if PMon <> NIL then
                    PMon.ResetIt;
            end;
            Result := 0;
        end;
        3:
        begin  // Monitors.ResetAll
            if ActiveCircuit <> NIL then
            begin
                MonitorClass.ResetAll;
            end;
            Result := 0;
        end;
        4:
        begin  // Monitors.Sample
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if PMon <> NIL then
                    PMon.TakeSample;
            end;
            Result := 0;
        end;
        5:
        begin  // Monitors.Save
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if PMon <> NIL then
                    PMon.Save;  // TranslateToCSV(False);
            end;
            Result := 0;
        end;
        6:
        begin  // Monitors.Show
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if PMon <> NIL then
                    PMon.TranslateToCSV(TRUE);
            end;
            Result := 0;
        end;
        7:
        begin  // Monitors.Mode read
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if PMon <> NIL then
                    Result := PMon.Mode
                else
                    Result := 0;
            end;
        end;
        8:
        begin  // Monitors.Mode Write
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if PMon <> NIL then
                begin
                    PMon.Mode := arg;
                    PMon.ResetIt;  // Always reset the monitor after a Mode change
                end;
            end;
            Result := 0;
        end;
        9:
        begin  // Monitors.SampleCount
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                Result := pMon.SampleCount;
            end;
        end;
        10:
        begin  // Monitors.SampleAll
            if ActiveCircuit <> NIL then
            begin
                MonitorClass.SampleAll;
            end;
            Result := 0;
        end;
        11:
        begin  // Monitor.SaveAll
            if ActiveCircuit <> NIL then
            begin
                MonitorClass.SaveAll;
            end;
        end;
        12:
        begin  // Monitor.Count
            if ActiveCircuit <> NIL then
            begin
                Result := ActiveCircuit.Monitors.ListSize;
            end;
        end;
        13:
        begin  // Monitor.Process
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if PMon <> NIL then
                    pMon.PostProcess;
            end;
            Result := 0;
        end;
        14:
        begin  // Monitor.ProcessAll
            if ActiveCircuit <> NIL then
            begin
                MonitorClass.PostProcessAll;
            end;
            Result := 0;
        end;
        15:
        begin  // Monitor.FileVersion
            if ActiveCircuit <> NIL then
            begin
                ReadMonitorHeader(Header, TRUE);
                Result := Header.Version;
            end;
        end;
        16:
        begin // Monitor.RecordSize
            if ActiveCircuit <> NIL then
            begin
                ReadMonitorHeader(Header, TRUE);
                Result := Header.RecordSize;
            end;
        end;
        17:
        begin  // Monitor.NumChannels
            if ActiveCircuit <> NIL then
            begin
                ReadMonitorHeader(Header, TRUE);
                Result := Header.RecordSize;
            end;
        end;
        18:
        begin  // Monitor.Terminal read
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if PMon <> NIL then
                    Result := pMon.MeteredTerminal;
            end;
        end;
        19:
        begin  // Monitor.Terminal Write
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if PMon <> NIL then
                begin
                    pMon.MeteredTerminal := arg;
                    pMon.RecalcElementData;
                end;
                Result := 0;
            end;
        end
    else
        Result := -1; // The parameter is not valid
    end;
end;

//***********************String Type properties*********************************
function MonitorsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    pMon: TMonitorObj;
    Header: THeaderRec;
    activesave: Integer;
    S: String;
    Found: Boolean;

begin
    Result := pAnsiChar(Ansistring('0')); // Default return value
    case mode of
        0:
        begin  // Monitors.FIleName
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if PMon <> NIL then
                    Result := pAnsiChar(Ansistring(PMon.CSVFileName))
                else
                    Result := pAnsiChar(Ansistring(''));
            end;
        end;
        1:
        begin // Monitors.Name read
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if PMon <> NIL then
                    Result := pAnsiChar(Ansistring(PMon.Name))
                else
                    Result := String(Ansistring(''));
            end;
        end;
        2:
        begin  // Monitors.Nme Write
            if ActiveCircuit <> NIL then
            begin      // Search list of monitors in active circuit for name
                with ActiveCircuit.Monitors do
                begin
                    S := String(arg);  // Convert to Pascal String
                    Found := FALSE;
                    ActiveSave := ActiveIndex;
                    pMon := First;
                    while pMon <> NIL do
                    begin
                        if (CompareText(pMon.Name, S) = 0) then
                        begin
                            ActiveCircuit.ActiveCktElement := pMon;
                            Found := TRUE;
                            Break;
                        end;
                        pMon := Next;
                    end;
                    if not Found then
                    begin
                        DoSimpleMsg('Monitor "' + S + '" Not Found in Active Circuit.', 5004);
                        pMon := Get(ActiveSave);    // Restore active Monerator
                        ActiveCircuit.ActiveCktElement := pMon;
                    end;
                end;
            end;
        end;
        3:
        begin  // Monitors.Element read
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if PMon <> NIL then
                    Result := pAnsiChar(Ansistring(pMon.ElementName));
            end;
        end;
        4:
        begin  // Monitors.Element Write
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if PMon <> NIL then
                begin
                    pMon.ElementName := String(arg);
                    pMon.PropertyValue[1] := String(arg);
                    pMon.RecalcElementData;
                end;
            end;
        end
    else
        Result := String(Ansistring('Error, parameter not valid'));
    end;
end;

//****************************Variant type properties***************************
procedure MonitorsV(mode: Longint; out arg: Variant); CDECL;

var
    MonitorElem: TMonitorObj;
    i, k, index: Integer;
    pMon: TMonitorObj;
    p: Pointer;
    Header: THeaderRec;
    ListSize: Integer;
    SaveDelims: String;
    SaveWhiteSpace: String;
    hr: Single;
    s: Single;
    freq: Single;
    FirstCol: String;
    SngBuffer: pSingleArray;
    AllocSize: Integer;


begin
    case mode of
        0:
        begin  // Monitors.AllNames
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                    if Monitors.ListSize > 0 then
                    begin
                        VarArrayRedim(arg, Monitors.ListSize - 1);
                        k := 0;
                        MonitorElem := Monitors.First;
                        while MonitorElem <> NIL do
                        begin
                            arg[k] := MonitorElem.Name;
                            Inc(k);
                            MonitorElem := Monitors.Next;
                        end;
                    end;
        end;
        1:
        begin  // Monitor.ByteStream
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if PMon <> NIL then
                begin
                    arg := VarArrayCreate([0, pmon.MonitorStream.Size - 1], varByte);
                    pmon.MonitorStream.Seek(0, soFromBeginning);
                    p := VarArrayLock(arg);
                    pmon.MonitorStream.Read(p^, pmon.MonitorStream.Size);   // Move it all over
              // leaves stream at the end
                    VarArrayUnlock(arg);
                end
                else
                    arg := VarArrayCreate([0, 0], varByte);
            end;
        end;
        2:
        begin  // Monitors.dblHour
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                begin
                    ReadMonitorHeader(Header, TRUE);
                    if Header.RecordSize > 0 then
                    begin
                        ListSize := Header.RecordSize;
                        VarArrayRedim(arg, ListSize - 1);
                        k := 0;
                        SaveDelims := AuxParser.Delimiters;
                        AuxParser.Delimiters := ',';
                        SaveWhiteSpace := AuxParser.Whitespace;
                        AuxParser.Whitespace := '';
                        AuxParser.CmdString := String(Header.StrBuffer);
                        AuxParser.AutoIncrement := TRUE;
                        AuxParser.StrValue;  // Get rid of first two columns
                        AuxParser.StrValue;
                        while k < ListSize do
                        begin
                            arg[k] := AuxParser.StrValue;
                            Inc(k);
                        end;
                        AuxParser.AutoIncrement := FALSE; // be a good citizen
                        AuxParser.Delimiters := SaveDelims;
                        AuxParser.Whitespace := SaveWhiteSpace;
                    end;
                end;
        end;
        3:
        begin  // Monitors.dblHour
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if pMon.SampleCount > 0 then
                begin
                    arg := VarArrayCreate([0, pMon.SampleCount - 1], varDouble);
                    ReadMonitorHeader(Header, FALSE);   // leave at beginning of data
                    AuxParser.CmdString := String(Header.StrBuffer);
                    AuxParser.AutoIncrement := TRUE;
                    FirstCol := AuxParser.StrValue;  // Get rid of first two columns
                    AuxParser.AutoIncrement := FALSE;
               // check first col to see if it is "Hour"
                    if Sysutils.CompareText(FirstCol, 'hour') = 0 then
                    begin
                        AllocSize := Sizeof(Single) * Header.RecordSize;
                        SngBuffer := Allocmem(AllocSize);
                        k := 0;
                        for i := 1 to pMon.SampleCount do
                        begin
                            with pMon.MonitorStream do
                            begin
                                Read(hr, SizeOf(hr));  // Hour
                                Read(s, SizeOf(s));   // Seconds past the hour
                                Read(sngBuffer^[1], AllocSize);  // read rest of record
                            end;
                            arg[k] := hr + s / 3600.0;
                            inc(k);
                        end;
                        Reallocmem(SngBuffer, 0);  // Dispose of buffer
                    end
                    else
                    begin   // Not time solution, so return nil array
                        arg := VarArrayCreate([0, 0], varDouble);
                        pMon.MonitorStream.Seek(0, soFromEnd); // leave stream at end
                    end;
                end
                else
                    arg := VarArrayCreate([0, 0], varDouble);
            end;
        end;
        4:
        begin  // Monitors,dblFreq
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if pMon.SampleCount > 0 then
                begin
                    arg := VarArrayCreate([0, pMon.SampleCount - 1], varDouble);
                    ReadMonitorHeader(Header, FALSE);   // leave at beginning of data
                    AuxParser.CmdString := String(Header.StrBuffer);
                    AuxParser.AutoIncrement := TRUE;
                    FirstCol := AuxParser.StrValue;  // Get rid of first two columns
                    AuxParser.AutoIncrement := FALSE;
               // check first col to see if it is "Freq" for harmonics solution
                    if Sysutils.CompareText(FirstCol, 'freq') = 0 then
                    begin
                        AllocSize := Sizeof(SngBuffer^[1]) * Header.RecordSize;
                        SngBuffer := Allocmem(AllocSize);
                        k := 0;
                        for i := 1 to pMon.SampleCount do
                        begin
                            with pMon.MonitorStream do
                            begin
                                Read(freq, SizeOf(freq));  // frequency
                                Read(s, SizeOf(s));   // harmonic
                                Read(sngBuffer^[1], AllocSize);  // read rest of record
                            end;
                            arg[k] := freq;
                            inc(k);
                        end;
                        Reallocmem(SngBuffer, 0);  // Dispose of buffer
                    end
                    else
                    begin   // Not harmonic solution, so return nil array
                        arg := VarArrayCreate([0, 0], varDouble);
                        pMon.MonitorStream.Seek(0, soFromEnd); // leave stream at end
                    end;
                end
                else
                    arg := VarArrayCreate([0, 0], varDouble);
            end;
        end;
        5:
        begin
            if ActiveCircuit <> NIL then
            begin
                pMon := ActiveCircuit.Monitors.Active;
                if pMon.SampleCount > 0 then
                begin
                    index := Integer(arg);
                    arg := VarArrayCreate([0, pMon.SampleCount - 1], varDouble);
                    ReadMonitorHeader(Header, FALSE);   // FALSE = leave at beginning of data
                    AuxParser.CmdString := String(Header.StrBuffer);
                    AuxParser.AutoIncrement := TRUE;
                    FirstCol := AuxParser.StrValue;  // Get rid of first two columns
                    AuxParser.AutoIncrement := FALSE;
                    AllocSize := Sizeof(SngBuffer^[1]) * Header.RecordSize;
                    SngBuffer := Allocmem(AllocSize);
                    k := 0;
                    for i := 1 to pMon.SampleCount do
                    begin
                        with pMon.MonitorStream do
                        begin
                            Read(hr, SizeOf(hr));
                            Read(s, SizeOf(s));
                            Read(sngBuffer^[1], AllocSize);  // read rest of record
                        end;
                        arg[k] := sngBuffer^[index];
                        inc(k);
                    end;
                    Reallocmem(SngBuffer, 0);  // Dispose of buffer
                end
                else
                    arg := VarArrayCreate([0, 0], varDouble);
            end;
        end
    else
        arg[0] := 'Error, parameter not recognized';
    end;
end;

end.
