unit DMonitors;

interface

function MonitorsI(mode: Longint; arg: Longint): Longint; CDECL;
function MonitorsS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure MonitorsV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

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
        dummyRec: TMonitorStrBuffer;
    end;

    SingleArray = array[1..100] of Single;
    pSingleArray = ^SingleArray;

procedure ReadMonitorHeader(var HeaderRec: THeaderRec; Opt: Boolean);
var
    mypMon: TMonitorObj;

begin
    mypMon := ActiveCircuit[ActiveActor].Monitors.Active;
    try
        with mypmon.MonitorStream, HeaderRec do
        begin
            Seek(0, classes.soFromBeginning);
            Read(signature, Sizeof(signature));    // Signature   (32 bit Integer )
            Read(version, Sizeof(version));        // Version     (32 bit Integer )
            Read(RecordSize, Sizeof(RecordSize));    // RecordSize  (32 bit Integer )
            Read(Mode, Sizeof(Mode));                // Mode        (32 bit Integer )
            Read(dummyRec, Sizeof(TMonitorStrBuffer)); // String      (255 char string)
        end;

    finally
          // If opt is false leave monitorstream at end of header record
        if Opt then
            mypmon.MonitorStream.Seek(0, soFromEnd);    // put monitor stream pointer back where it was
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
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.First;
                if pMon <> NIL then
                begin
                    repeat
                        if pMon.enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pMon;
                            Result := 1;
                        end
                        else
                            pMon := ActiveCircuit[ActiveActor].Monitors.Next;
                    until (Result = 1) or (pMon = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        1:
        begin  // Monitors.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Next;
                if pMon <> NIL then
                begin
                    repeat
                        if pMon.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pMon;
                            Result := 1;
                        end
                        else
                            pMon := ActiveCircuit[ActiveActor].Monitors.Next;
                    until (Result > 0) or (pMon = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        2:
        begin  // Monitors.Reset
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if PMon <> NIL then
                    PMon.ResetIt(ActiveActor);
            end;
            Result := 0;
        end;
        3:
        begin  // Monitors.ResetAll
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                MonitorClass[ActiveActor].ResetAll(ActiveActor);
            end;
            Result := 0;
        end;
        4:
        begin  // Monitors.Sample
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if PMon <> NIL then
                    PMon.TakeSample(ActiveActor);
            end;
            Result := 0;
        end;
        5:
        begin  // Monitors.Save
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if PMon <> NIL then
                    PMon.Save;  // TranslateToCSV(False);
            end;
            Result := 0;
        end;
        6:
        begin  // Monitors.Show
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if PMon <> NIL then
                    PMon.TranslateToCSV(TRUE, ActiveActor);
            end;
            Result := 0;
        end;
        7:
        begin  // Monitors.Mode read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if PMon <> NIL then
                    Result := PMon.Mode
                else
                    Result := 0;
            end;
        end;
        8:
        begin  // Monitors.Mode Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if PMon <> NIL then
                begin
                    PMon.Mode := arg;
                    PMon.ResetIt(ActiveActor);  // Always reset the monitor after a Mode change
                end;
            end;
            Result := 0;
        end;
        9:
        begin  // Monitors.SampleCount
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                Result := pMon.SampleCount;
            end;
        end;
        10:
        begin  // Monitors.SampleAll
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                MonitorClass[ActiveActor].SampleAll(ActiveActor);
            end;
            Result := 0;
        end;
        11:
        begin  // Monitor.SaveAll
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                MonitorClass[ActiveActor].SaveAll(ActiveActor);
            end;
        end;
        12:
        begin  // Monitor.Count
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := ActiveCircuit[ActiveActor].Monitors.ListSize;
            end;
        end;
        13:
        begin  // Monitor.Process
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if PMon <> NIL then
                    pMon.PostProcess(ActiveActor);
            end;
            Result := 0;
        end;
        14:
        begin  // Monitor.ProcessAll
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                MonitorClass[ActiveActor].PostProcessAll(ActiveActor);
            end;
            Result := 0;
        end;
        15:
        begin  // Monitor.FileVersion
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ReadMonitorHeader(Header, TRUE);
                Result := Header.Version;
            end;
        end;
        16:
        begin // Monitor.RecordSize
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ReadMonitorHeader(Header, TRUE);
                Result := Header.RecordSize;
            end;
        end;
        17:
        begin  // Monitor.NumChannels
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ReadMonitorHeader(Header, TRUE);
                Result := Header.RecordSize;
            end;
        end;
        18:
        begin  // Monitor.Terminal read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if PMon <> NIL then
                    Result := pMon.MeteredTerminal;
            end;
        end;
        19:
        begin  // Monitor.Terminal Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if PMon <> NIL then
                begin
                    pMon.MeteredTerminal := arg;
                    pMon.RecalcElementData(ActiveActor);
                end;
                Result := 0;
            end;
        end
    else
        Result := -1; // The parameter is not valid
    end;
end;

//***********************String Type properties*********************************
function MonitorsS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    pMon: TMonitorObj;
    activesave: Integer;
    S: String;
    Found: Boolean;

begin
    Result := Pansichar(Ansistring('0')); // Default return value
    case mode of
        0:
        begin  // Monitors.FIleName
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if PMon <> NIL then
                    Result := Pansichar(Ansistring(PMon.Get_FileName(ActiveActor)))
                else
                    Result := Pansichar(Ansistring(''));
            end;
        end;
        1:
        begin // Monitors.Name read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if PMon <> NIL then
                    Result := Pansichar(Ansistring(PMon.Name))
                else
                    Result := Pansichar(Ansistring(''));
            end;
        end;
        2:
        begin  // Monitors.Nme Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin      // Search list of monitors in active circuit for name
                with ActiveCircuit[ActiveActor].Monitors do
                begin
                    S := String(arg);  // Convert to Pascal String
                    Found := FALSE;
                    ActiveSave := ActiveIndex;
                    pMon := First;
                    while pMon <> NIL do
                    begin
                        if (CompareText(pMon.Name, S) = 0) then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pMon;
                            Found := TRUE;
                            Break;
                        end;
                        pMon := Next;
                    end;
                    if not Found then
                    begin
                        DoSimpleMsg('Monitor "' + S + '" Not Found in Active Circuit.', 5004);
                        pMon := Get(ActiveSave);    // Restore active Monerator
                        ActiveCircuit[ActiveActor].ActiveCktElement := pMon;
                    end;
                end;
            end;
        end;
        3:
        begin  // Monitors.Element read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if PMon <> NIL then
                    Result := Pansichar(Ansistring(pMon.ElementName));
            end;
        end;
        4:
        begin  // Monitors.Element Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if PMon <> NIL then
                begin
                    s := String(arg);
                    pMon.ElementName := s;
                    pMon.PropertyValue[1] := s;
                    pMon.RecalcElementData(ActiveActor);
                end;
            end;
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//****************************Variant type properties***************************
procedure MonitorsV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    MonitorElem: TMonitorObj;
    AllocSize,
    i, k, index,
    ListSize: Integer;
    pMon: TMonitorObj;
    p: Pointer;
    Header: THeaderRec;
    TempStr,
    SaveDelims,
    SaveWhiteSpace,
    FirstCol: String;
    hr,
    s,
    freq: Single;
    SngBuffer: pSingleArray;
    PInt: ^Integer;

begin
  {$IFDEF FPC_DLL}
    initialize(SngBuffer);
{$ENDIF}
    case mode of
        0:
        begin  // Monitors.AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    if Monitors.ListSize > 0 then
                    begin
                        MonitorElem := Monitors.First;
                        while MonitorElem <> NIL do
                        begin
                            WriteStr2Array(MonitorElem.Name);
                            WriteStr2Array(Char(0));
                            MonitorElem := Monitors.Next;
                        end;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        1:
        begin  // Monitor.ByteStream
            myType := 5;        // NOT a String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if PMon <> NIL then
                begin
                    setlength(myStrArray, pmon.MonitorStream.Size);
                    pmon.MonitorStream.Seek(0, soFromBeginning);
                    p := @(myStrArray[0]);
                    pmon.MonitorStream.Read(p^, pmon.MonitorStream.Size);   // Move it all over
                end
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        2:
        begin  // Monitors.Header
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                    ReadMonitorHeader(Header, TRUE);
                    if length(pMon.StrBuffer) > 0 then
                    begin
                        ListSize := Header.RecordSize;
                        SaveDelims := AuxParser[ActiveActor].Delimiters;
                        AuxParser[ActiveActor].Delimiters := ',';
                        SaveWhiteSpace := AuxParser[ActiveActor].Whitespace;
                        AuxParser[ActiveActor].Whitespace := '';
                        TempStr := '';
                        for i := 0 to High(pMon.StrBuffer) do       // Moves the content to a string var
                            TempStr := TempStr + String(pMon.StrBuffer[i]); // For some reason needed for DLL

                        AuxParser[ActiveActor].CmdString := TempStr;
                        AuxParser[ActiveActor].AutoIncrement := TRUE;
                        AuxParser[ActiveActor].StrValue;  // Get rid of first two columns
                        AuxParser[ActiveActor].StrValue;
                        k := 0;
                        while k < ListSize do
                        begin
                            WriteStr2Array(AuxParser[ActiveActor].StrValue);
                            WriteStr2Array(Char(0));
                            inc(k);
                        end;
                        AuxParser[ActiveActor].AutoIncrement := FALSE; // be a good citizen
                        AuxParser[ActiveActor].Delimiters := SaveDelims;
                        AuxParser[ActiveActor].Whitespace := SaveWhiteSpace;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        3:
        begin  // Monitors.dblHour
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if pMon.SampleCount > 0 then
                begin
                    setlength(myDBLArray, pMon.SampleCount);
                    ReadMonitorHeader(Header, FALSE);   // leave at beginning of data

                    TempStr := '';
                    for i := 0 to High(pMon.StrBuffer) do       // Moves the content to a string var
                        TempStr := TempStr + String(pMon.StrBuffer[i]); // For some reason needed for DLL

                    AuxParser[ActiveActor].CmdString := String(TempStr);
                    AuxParser[ActiveActor].AutoIncrement := TRUE;
                    FirstCol := AuxParser[ActiveActor].StrValue;  // Get rid of first two columns
                    AuxParser[ActiveActor].AutoIncrement := FALSE;
          // check first col to see if it is "Hour"
                    if CompareText(FirstCol, 'hour') = 0 then
                    begin
                        AllocSize := Sizeof(SngBuffer^[1]) * Header.RecordSize;
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
                            myDBLArray[k] := hr + s / 3600.0;
                            inc(k);
                        end;
                        Reallocmem(SngBuffer, 0);  // Dispose of buffer
                    end
                    else
                    begin   // Not time solution, so return nil array
                        pMon.MonitorStream.Seek(0, soFromEnd); // leave stream at end
                    end;
                end
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        4:
        begin  // Monitors.dblFreq
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if pMon.SampleCount > 0 then
                begin
                    setlength(myDBLArray, pMon.SampleCount);
                    ReadMonitorHeader(Header, FALSE);   // leave at beginning of data
                    TempStr := '';
                    for i := 0 to High(pMon.StrBuffer) do       // Moves the content to a string var
                        TempStr := TempStr + String(pMon.StrBuffer[i]); // For some reason needed for DLL

                    AuxParser[ActiveActor].CmdString := String(TempStr);
                    AuxParser[ActiveActor].AutoIncrement := TRUE;
                    FirstCol := AuxParser[ActiveActor].StrValue;  // Get rid of first two columns
                    AuxParser[ActiveActor].AutoIncrement := FALSE;
          // check first col to see if it is "Freq" for harmonics solution
                    if CompareText(FirstCol, 'freq') = 0 then
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
                            myDBLArray[k] := freq;
                            inc(k);
                        end;
                        Reallocmem(SngBuffer, 0);  // Dispose of buffer
                    end
                    else
                    begin   // Not harmonic solution, so return nil array
                        pMon.MonitorStream.Seek(0, soFromEnd); // leave stream at end
                    end;
                end
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        5:
        begin    // Monitors.Channel
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pMon := ActiveCircuit[ActiveActor].Monitors.Active;
                if pMon.SampleCount > 0 then
                begin
                    PInt := myPointer;
                    index := Integer(PInt^);
                    setlength(myDBLArray, pMon.SampleCount);
                    ReadMonitorHeader(Header, FALSE);   // FALSE = leave at beginning of data
                    TempStr := '';
                    for i := 0 to High(pMon.StrBuffer) do       // Moves the content to a string var
                        TempStr := TempStr + String(pMon.StrBuffer[i]); // For some reason needed for DLL

                    AuxParser[ActiveActor].CmdString := String(TempStr);
                    AuxParser[ActiveActor].AutoIncrement := TRUE;
                    FirstCol := AuxParser[ActiveActor].StrValue;  // Get rid of first two columns
                    AuxParser[ActiveActor].AutoIncrement := FALSE;
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
                        myDBLArray[k] := sngBuffer^[index];
                        inc(k);
                    end;
                    Reallocmem(SngBuffer, 0);  // Dispose of buffer
                end
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end
    else
    begin
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
    end;
end;

end.
