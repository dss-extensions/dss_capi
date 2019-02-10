unit ImplMonitors;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   1-12-00  Modified first..next to return only enabled monitors

   8-1-2013 Added properties to make accessing the bytestream easier
}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSEngine_TLB,
    StdVcl;

type
    TMonitors = class(TAutoObject, IMonitors)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_FileName: Widestring; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Mode: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        procedure Reset; SAFECALL;
        procedure ResetAll; SAFECALL;
        procedure Sample; SAFECALL;
        procedure Save; SAFECALL;
        procedure Set_Mode(Value: Integer); SAFECALL;
        procedure Show; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_ByteStream: Olevariant; SAFECALL;
        function Get_SampleCount: Integer; SAFECALL;
        procedure SampleAll; SAFECALL;
        procedure SaveAll; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        procedure Process; SAFECALL;
        procedure ProcessAll; SAFECALL;
        function Get_Channel(Index: Integer): Olevariant; SAFECALL;
        function Get_dblFreq: Olevariant; SAFECALL;
        function Get_dblHour: Olevariant; SAFECALL;
        function Get_FileVersion: Integer; SAFECALL;
        function Get_Header: Olevariant; SAFECALL;
        function Get_NumChannels: Integer; SAFECALL;
        function Get_RecordSize: Integer; SAFECALL;
        function Get_Element: Widestring; SAFECALL;
        procedure Set_Element(const Value: Widestring); SAFECALL;
        function Get_Terminal: Integer; SAFECALL;
        procedure Set_Terminal(Value: Integer); SAFECALL;
    { Protected declarations }
    end;

implementation

uses
    ComServ,
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
    pMon := ActiveCircuit[ActiveActor].Monitors.Active;
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

function TMonitors.Get_AllNames: Olevariant;
var
    MonitorElem: TMonitorObj;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if Monitors.ListSize > 0 then
            begin
                VarArrayRedim(Result, Monitors.ListSize - 1);
                k := 0;
                MonitorElem := Monitors.First;
                while MonitorElem <> NIL do
                begin
                    Result[k] := MonitorElem.Name;
                    Inc(k);
                    MonitorElem := Monitors.Next;
                end;
            end;
end;

function TMonitors.Get_FileName: Widestring;

var
    pMon: TMonitorObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> NIL then
            Result := pMon.Get_FileName(ActiveActor)
        else
            Result := '';
    end;

end;

function TMonitors.Get_First: Integer;
var
    pMon: TMonitorObj;

begin

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

function TMonitors.Get_Mode: Integer;
var
    pMon: TMonitorObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> NIL then
            Result := PMon.Mode
        else
            Result := 0;
    end;

end;

function TMonitors.Get_Name: Widestring;
var
    pMon: TMonitorObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> NIL then
            Result := PMon.Name
        else
            Result := '';
    end;

end;

function TMonitors.Get_Next: Integer;
var
    pMon: TMonitorObj;

begin

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

procedure TMonitors.Reset;
var
    pMon: TMonitorObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> NIL then
            PMon.ResetIt(ActiveActor);
    end;

end;

procedure TMonitors.ResetAll;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        MonitorClass[ActiveActor].ResetAll(ActiveActor);
    end;
end;

procedure TMonitors.Sample;

var
    pMon: TMonitorObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> NIL then
            PMon.TakeSample(ActiveActor);
    end;

end;

procedure TMonitors.Save;
var
    pMon: TMonitorObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> NIL then
            PMon.Save;  // TranslateToCSV(False);
    end;

end;

procedure TMonitors.Set_Mode(Value: Integer);

var
    pMon: TMonitorObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> NIL then
        begin
            PMon.Mode := Value;
            PMon.ResetIt(ActiveActor);  // Always reset the monitor after a Mode change
        end;
    end;

end;

procedure TMonitors.Show;
var
    pMon: TMonitorObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> NIL then
            PMon.TranslateToCSV(TRUE, ActiveActor);
    end;

end;

procedure TMonitors.Set_Name(const Value: Widestring);
var
    activesave: Integer;
    Mon: TMonitorObj;
    S: String;
    Found: Boolean;
begin


    if ActiveCircuit[ActiveActor] <> NIL then
    begin      // Search list of monitors in active circuit for name
        with ActiveCircuit[ActiveActor].Monitors do
        begin
            S := Value;  // Convert to Pascal String
            Found := FALSE;
            ActiveSave := ActiveIndex;
            Mon := First;
            while Mon <> NIL do
            begin
                if (CompareText(Mon.Name, S) = 0) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := Mon;
                    Found := TRUE;
                    Break;
                end;
                Mon := Next;
            end;
            if not Found then
            begin
                DoSimpleMsg('Monitor "' + S + '" Not Found in Active Circuit.', 5004);
                Mon := Get(ActiveSave);    // Restore active Monerator
                ActiveCircuit[ActiveActor].ActiveCktElement := Mon;
            end;
        end;
    end;


end;

function TMonitors.Get_ByteStream: Olevariant;
var
    pMon: TMonitorObj;
    p: Pointer;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> NIL then
        begin
            Result := VarArrayCreate([0, pmon.MonitorStream.Size - 1], varByte);
            pmon.MonitorStream.Seek(0, soFromBeginning);
            p := VarArrayLock(Result);
            pmon.MonitorStream.Read(p^, pmon.MonitorStream.Size);   // Move it all over
          // leaves stream at the end
            VarArrayUnlock(Result);
        end
        else
            Result := VarArrayCreate([0, 0], varByte);
    end;

end;

function TMonitors.Get_SampleCount: Integer;
var
    pMon: TMonitorObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        Result := pMon.SampleCount;
    end;
end;

procedure TMonitors.SampleAll;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        MonitorClass[ActiveActor].SampleAll(ActiveActor);
    end;
end;

procedure TMonitors.SaveAll;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        MonitorClass[ActiveActor].SaveAll(ActiveActor);
    end;
end;

function TMonitors.Get_Count: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].Monitors.ListSize;
    end;
end;

procedure TMonitors.Process;
var
    pMon: TMonitorObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> NIL then
            pMon.PostProcess(ActiveActor);
    end;
end;

procedure TMonitors.ProcessAll;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        MonitorClass[ActiveActor].PostProcessAll(ActiveActor);
    end;
end;

function TMonitors.Get_Channel(Index: Integer): Olevariant;

// Return an array of doubles for selected channel

var
    Header: THeaderRec;
    k, i: Integer;
    FirstCol: String;
    pMon: TMonitorObj;
    SngBuffer: pSingleArray;
    hr: Single;
    s: Single;
    AllocSize: Integer;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin

        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if pMon.SampleCount > 0 then
        begin

            Result := VarArrayCreate([0, pMon.SampleCount - 1], varDouble);
            ReadMonitorHeader(Header, FALSE);   // FALSE = leave at beginning of data
            AuxParser[ActiveActor].CmdString := String(Header.StrBuffer);
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
                Result[k] := sngBuffer^[index];
                inc(k);
            end;

            Reallocmem(SngBuffer, 0);  // Dispose of buffer

        end
        else
            Result := VarArrayCreate([0, 0], varDouble);

    end;
end;

function TMonitors.Get_dblFreq: Olevariant;

// Return an array of doubles for frequence for Harmonic solutions
var
    Header: THeaderRec;
    k, i: Integer;
    FirstCol: String;
    pMon: TMonitorObj;
    SngBuffer: pSingleArray;
    freq: Single;
    s: Single;
    AllocSize: Integer;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin

        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if pMon.SampleCount > 0 then
        begin
            Result := VarArrayCreate([0, pMon.SampleCount - 1], varDouble);
            ReadMonitorHeader(Header, FALSE);   // leave at beginning of data
            AuxParser[ActiveActor].CmdString := String(Header.StrBuffer);
            AuxParser[ActiveActor].AutoIncrement := TRUE;
            FirstCol := AuxParser[ActiveActor].StrValue;  // Get rid of first two columns
            AuxParser[ActiveActor].AutoIncrement := FALSE;
             // check first col to see if it is "Freq" for harmonics solution
            if System.Sysutils.CompareText(FirstCol, 'freq') = 0 then
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
                    Result[k] := freq;
                    inc(k);
                end;

                Reallocmem(SngBuffer, 0);  // Dispose of buffer

            end
            else
            begin   // Not harmonic solution, so return nil array
                Result := VarArrayCreate([0, 0], varDouble);
                pMon.MonitorStream.Seek(0, soFromEnd); // leave stream at end
            end;
        end
        else
            Result := VarArrayCreate([0, 0], varDouble);

    end;

end;

function TMonitors.Get_dblHour: Olevariant;

// Return an array of doubles for time in hours
var
    Header: THeaderRec;
    k, i: Integer;
    FirstCol: String;
    pMon: TMonitorObj;
    SngBuffer: pSingleArray;
    hr: Single;
    s: Single;
    AllocSize: Integer;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin

        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if pMon.SampleCount > 0 then
        begin
            Result := VarArrayCreate([0, pMon.SampleCount - 1], varDouble);
            ReadMonitorHeader(Header, FALSE);   // leave at beginning of data
            AuxParser[ActiveActor].CmdString := String(Header.StrBuffer);
            AuxParser[ActiveActor].AutoIncrement := TRUE;
            FirstCol := AuxParser[ActiveActor].StrValue;  // Get rid of first two columns
            AuxParser[ActiveActor].AutoIncrement := FALSE;
             // check first col to see if it is "Hour"
            if System.Sysutils.CompareText(FirstCol, 'hour') = 0 then
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
                    Result[k] := hr + s / 3600.0;
                    inc(k);
                end;

                Reallocmem(SngBuffer, 0);  // Dispose of buffer

            end
            else
            begin   // Not time solution, so return nil array
                Result := VarArrayCreate([0, 0], varDouble);
                pMon.MonitorStream.Seek(0, soFromEnd); // leave stream at end
            end;
        end
        else
            Result := VarArrayCreate([0, 0], varDouble);

    end;

end;

function TMonitors.Get_FileVersion: Integer;
var
    Header: THeaderRec;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin

        Result := Header.Version;
    end;

end;

function TMonitors.Get_Header: Olevariant;

// Variant list of strings with names of all channels

var
    Header: THeaderRec;
    k: Integer;
    ListSize: Integer;
    SaveDelims: String;
    SaveWhiteSpace: String;
begin

    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            ReadMonitorHeader(Header, TRUE);
            if Header.RecordSize > 0 then
            begin
                ListSize := Header.RecordSize;
                VarArrayRedim(Result, ListSize - 1);
                k := 0;
                SaveDelims := AuxParser[ActiveActor].Delimiters;
                AuxParser[ActiveActor].Delimiters := ',';
                SaveWhiteSpace := AuxParser[ActiveActor].Whitespace;
                AuxParser[ActiveActor].Whitespace := '';
                AuxParser[ActiveActor].CmdString := String(Header.StrBuffer);
                AuxParser[ActiveActor].AutoIncrement := TRUE;
                AuxParser[ActiveActor].StrValue;  // Get rid of first two columns
                AuxParser[ActiveActor].StrValue;
                while k < ListSize do
                begin
                    Result[k] := AuxParser[ActiveActor].StrValue;
                    Inc(k);
                end;
                AuxParser[ActiveActor].AutoIncrement := FALSE; // be a good citizen
                AuxParser[ActiveActor].Delimiters := SaveDelims;
                AuxParser[ActiveActor].Whitespace := SaveWhiteSpace;
            end;
        end;

end;

function TMonitors.Get_NumChannels: Integer;
var
    Header: THeaderRec;
begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ReadMonitorHeader(Header, TRUE);
        Result := Header.RecordSize;
    end;
end;

function TMonitors.Get_RecordSize: Integer;
var
    Header: THeaderRec;
begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ReadMonitorHeader(Header, TRUE);
        Result := Header.RecordSize;
    end;
end;

function TMonitors.Get_Element: Widestring;
var
    pMon: TMonitorObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> NIL then
            Result := pMon.ElementName;
    end;
end;

procedure TMonitors.Set_Element(const Value: Widestring);
var
    pMon: TMonitorObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> NIL then
        begin
            pMon.ElementName := Value;
            pMon.PropertyValue[1] := Value;
            pMon.RecalcElementData(ActiveActor);
        end;
    end;

end;

function TMonitors.Get_Terminal: Integer;
var
    pMon: TMonitorObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> NIL then
            Result := pMon.MeteredTerminal;
    end;
end;

procedure TMonitors.Set_Terminal(Value: Integer);
var
    pMon: TMonitorObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> NIL then
        begin
            pMon.MeteredTerminal := Value;
            pMon.RecalcElementData(ActiveActor);
        end;
    end;

end;

initialization
    TAutoObjectFactory.Create(ComServer, TMonitors, Class_Monitors,
        ciInternal, tmApartment);
end.
