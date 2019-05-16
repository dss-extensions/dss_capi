unit CAPI_Monitors;

{$inline on}

interface

uses
    CAPI_Utils;

procedure Monitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Monitors_Get_AllNames_GR(); CDECL;
function Monitors_Get_FileName(): PAnsiChar; CDECL;
function Monitors_Get_First(): Integer; CDECL;
function Monitors_Get_Mode(): Integer; CDECL;
function Monitors_Get_Name(): PAnsiChar; CDECL;
function Monitors_Get_Next(): Integer; CDECL;
procedure Monitors_Reset(); CDECL;
procedure Monitors_ResetAll(); CDECL;
procedure Monitors_Sample(); CDECL;
procedure Monitors_Save(); CDECL;
procedure Monitors_Set_Mode(Value: Integer); CDECL;
procedure Monitors_Show(); CDECL;
procedure Monitors_Set_Name(const Value: PAnsiChar); CDECL;
procedure Monitors_Get_ByteStream(var ResultPtr: PByte; ResultCount: PInteger); CDECL;
procedure Monitors_Get_ByteStream_GR(); CDECL;
function Monitors_Get_SampleCount(): Integer; CDECL;
procedure Monitors_SampleAll(); CDECL;
procedure Monitors_SaveAll(); CDECL;
function Monitors_Get_Count(): Integer; CDECL;
procedure Monitors_Process(); CDECL;
procedure Monitors_ProcessAll(); CDECL;
procedure Monitors_Get_Channel(var ResultPtr: PDouble; ResultCount: PInteger; Index: Integer); CDECL;
procedure Monitors_Get_Channel_GR(Index: Integer); CDECL;
procedure Monitors_Get_dblFreq(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Monitors_Get_dblFreq_GR(); CDECL;
procedure Monitors_Get_dblHour(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Monitors_Get_dblHour_GR(); CDECL;
function Monitors_Get_FileVersion(): Integer; CDECL;
procedure Monitors_Get_Header(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Monitors_Get_Header_GR(); CDECL;
function Monitors_Get_NumChannels(): Integer; CDECL;
function Monitors_Get_RecordSize(): Integer; CDECL;
function Monitors_Get_Element(): PAnsiChar; CDECL;
procedure Monitors_Set_Element(const Value: PAnsiChar); CDECL;
function Monitors_Get_Terminal(): Integer; CDECL;
procedure Monitors_Set_Terminal(Value: Integer); CDECL;

// API extensions
function Monitors_Get_idx(): Integer; CDECL;
procedure Monitors_Set_idx(Value: Integer); CDECL;


implementation

uses
    CAPI_Constants,
    Monitor,
    DSSGlobals,
    SysUtils,
    Classes,
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
//------------------------------------------------------------------------------
procedure Monitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit = NIL then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.Monitors, False);
end;

procedure Monitors_Get_AllNames_GR(); CDECL;
// Same as Monitors_Get_AllNames but uses global result (GR) pointers
begin
    Monitors_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Monitors_Get_FileName_AnsiString(): Ansistring; inline;
var
    pMon: TMonitorObj;

begin
    Result := '';
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    Result := PMon.CSVFileName
end;

function Monitors_Get_FileName(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Monitors_Get_FileName_AnsiString());
end;
//------------------------------------------------------------------------------
function Monitors_Get_First(): Integer; CDECL;
var
    pMon: TMonitorObj;

begin
    Result := 0;  // signify no more
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.First;
    if pMon = NIL then
        Exit;
    repeat
        if pMon.enabled then
        begin
            ActiveCircuit.ActiveCktElement := pMon;
            Result := 1;
        end
        else
            pMon := ActiveCircuit.Monitors.Next;
    until (Result = 1) or (pMon = NIL);
end;
//------------------------------------------------------------------------------
function Monitors_Get_Mode(): Integer; CDECL;
var
    pMon: TMonitorObj;
begin
    Result := 0;
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    Result := PMon.Mode
end;
//------------------------------------------------------------------------------
function Monitors_Get_Name_AnsiString(): Ansistring; inline;
var
    pMon: TMonitorObj;

begin
    Result := '';
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    Result := PMon.Name
end;

function Monitors_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Monitors_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Monitors_Get_Next(): Integer; CDECL;
var
    pMon: TMonitorObj;
begin
    Result := 0;  // signify no more
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Next;
    if pMon = NIL then
        Exit;
    repeat
        if pMon.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := pMon;
            Result := 1;
        end
        else
            pMon := ActiveCircuit.Monitors.Next;
    until (Result > 0) or (pMon = NIL);
end;
//------------------------------------------------------------------------------
procedure Monitors_Reset(); CDECL;
var
    pMon: TMonitorObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    PMon.ResetIt;
end;
//------------------------------------------------------------------------------
procedure Monitors_ResetAll(); CDECL;
begin
    if ActiveCircuit = NIL then
        Exit;
    MonitorClass.ResetAll;
end;
//------------------------------------------------------------------------------
procedure Monitors_Sample(); CDECL;
var
    pMon: TMonitorObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    PMon.TakeSample;
end;
//------------------------------------------------------------------------------
procedure Monitors_Save(); CDECL;
var
    pMon: TMonitorObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    PMon.Save;  // TranslateToCSV(False);
end;
//------------------------------------------------------------------------------
procedure Monitors_Set_Mode(Value: Integer); CDECL;
var
    pMon: TMonitorObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    PMon.Mode := Value;
    PMon.ResetIt;  // Always reset the monitor after a Mode change
end;
//------------------------------------------------------------------------------
procedure Monitors_Show(); CDECL;
var
    pMon: TMonitorObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    PMon.TranslateToCSV(TRUE);
end;
//------------------------------------------------------------------------------
procedure Monitors_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit = NIL then
        Exit;
    if MonitorClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := MonitorClass.ElementList.Active;
        ActiveCircuit.Monitors.Get(MonitorClass.Active);
    end
    else
    begin
        DoSimpleMsg('Monitor "' + Value + '" Not Found in Active Circuit.', 5004);
    end;
end;
//------------------------------------------------------------------------------
procedure Monitors_Get_ByteStream(var ResultPtr: PByte; ResultCount: PInteger); CDECL;
var
    Result: PByteArray;
    pMon: TMonitorObj;
    p: Pointer;
begin
    if ActiveCircuit <> NIL then
    begin
        pMon := ActiveCircuit.Monitors.Active;
        if PMon <> NIL then
        begin
            Result := DSS_RecreateArray_PByte(ResultPtr, ResultCount, pmon.MonitorStream.Size);
            pmon.MonitorStream.Seek(0, soFromBeginning);
            p := ResultPtr;
            pmon.MonitorStream.Read(p^, pmon.MonitorStream.Size);   // Move it all over
          // leaves stream at the end
            Exit;
        end
    end;
    Result := DSS_RecreateArray_PByte(ResultPtr, ResultCount, 1);
    Result[0] := 0;
end;

procedure Monitors_Get_ByteStream_GR(); CDECL;
// Same as Monitors_Get_ByteStream but uses global result (GR) pointers
begin
    Monitors_Get_ByteStream(GR_DataPtr_PByte, GR_CountPtr_PByte)
end;

//------------------------------------------------------------------------------
function Monitors_Get_SampleCount(): Integer; CDECL;
var
    pMon: TMonitorObj;
begin
    Result := 0;
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    Result := pMon.SampleCount;
end;
//------------------------------------------------------------------------------
procedure Monitors_SampleAll(); CDECL;
begin
    if ActiveCircuit = NIL then
        Exit;
    MonitorClass.SampleAll;
end;
//------------------------------------------------------------------------------
procedure Monitors_SaveAll(); CDECL;
begin
    if ActiveCircuit = NIL then
        Exit;
    MonitorClass.SaveAll;
end;
//------------------------------------------------------------------------------
function Monitors_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit = NIL then
        Exit;
    Result := ActiveCircuit.Monitors.ListSize;
end;
//------------------------------------------------------------------------------
procedure Monitors_Process(); CDECL;
var
    pMon: TMonitorObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    pMon.PostProcess;
end;
//------------------------------------------------------------------------------
procedure Monitors_ProcessAll(); CDECL;
begin
    if ActiveCircuit = NIL then
        Exit;
    MonitorClass.PostProcessAll;
end;
//------------------------------------------------------------------------------
procedure Monitors_Get_Channel(var ResultPtr: PDouble; ResultCount: PInteger; Index: Integer); CDECL;
// Return an array of doubles for selected channel
var
    Result: PDoubleArray;
    Header: THeaderRec;
    i: Integer;
    pMon: TMonitorObj;
    SngBuffer: pSingleArray;
    AllocSize: Integer;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    Result[0] := 0;
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    if pMon.SampleCount <= 0 then
        Exit;
    ReadMonitorHeader(Header, FALSE);   // FALSE = leave at beginning of data

    if (Index < 1) or (Index > Header.RecordSize {NumChannels}) then
    begin
        DoSimpleMsg(Format(
            'Monitors.Channel: invalid channel index (%d), monitor "%s" has %d channels.',
            [Index, pMon.Name, Header.RecordSize]
            ), 5888);
        Exit;
    end;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, pMon.SampleCount);

    AllocSize := Sizeof(Single) * (Header.RecordSize + 2); // Include Hour and Second fields
    Index := Index + 2; // Skip Hour and Second fields
    SngBuffer := Allocmem(AllocSize); // Need a buffer to convert from float32 to float64
    for i := 1 to pMon.SampleCount do
    begin
        pMon.MonitorStream.Read(sngBuffer^[1], AllocSize);  // read rest of record
        Result[i - 1] := sngBuffer^[Index];
    end;
    Reallocmem(SngBuffer, 0);  // Dispose of buffer
end;

procedure Monitors_Get_Channel_GR(Index: Integer); CDECL;
// Same as Monitors_Get_Channel but uses global result (GR) pointers
begin
    Monitors_Get_Channel(GR_DataPtr_PDouble, GR_CountPtr_PDouble, Index)
end;

//------------------------------------------------------------------------------
procedure Monitors_Get_dblFreq(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
// Return an array of doubles for frequence for Harmonic solutions
var
    Result: PDoubleArray;
    Header: THeaderRec;
    k, i: Integer;
    FirstCol: String;
    pMon: TMonitorObj;
    SngBuffer: pSingleArray;
    freq: Single;
    s: Single;
    AllocSize: Integer;

begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    Result[0] := 0;
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    if pMon.SampleCount <= 0 then
        Exit;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (pMon.SampleCount - 1) + 1);
    ReadMonitorHeader(Header, FALSE);   // leave at beginning of data
    AuxParser.CmdString := String(Header.StrBuffer);
    AuxParser.AutoIncrement := TRUE;
    FirstCol := AuxParser.StrValue;  // Get rid of first two columns
    AuxParser.AutoIncrement := FALSE;
    // check first col to see if it is "Freq" for harmonics solution
    if SysUtils.CompareText(FirstCol, 'freq') = 0 then
    begin
        AllocSize := Sizeof(Single) * Header.RecordSize;
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
        pMon.MonitorStream.Seek(0, soFromEnd); // leave stream at end
    end;
end;

procedure Monitors_Get_dblFreq_GR(); CDECL;
// Same as Monitors_Get_dblFreq but uses global result (GR) pointers
begin
    Monitors_Get_dblFreq(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Monitors_Get_dblHour(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
// Return an array of doubles for time in hours
var
    Result: PDoubleArray;
    Header: THeaderRec;
    k, i: Integer;
    FirstCol: String;
    pMon: TMonitorObj;
    SngBuffer: pSingleArray;
    hr: Single;
    s: Single;
    AllocSize: Integer;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    Result[0] := 0;
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    if pMon.SampleCount <= 0 then
        Exit;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (pMon.SampleCount - 1) + 1);
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
            Result[k] := hr + s / 3600.0;
            inc(k);
        end;
        Reallocmem(SngBuffer, 0);  // Dispose of buffer
    end
    else
    begin   // Not time solution, so return nil array
        pMon.MonitorStream.Seek(0, soFromEnd); // leave stream at end
    end;
end;

procedure Monitors_Get_dblHour_GR(); CDECL;
// Same as Monitors_Get_dblHour but uses global result (GR) pointers
begin
    Monitors_Get_dblHour(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Monitors_Get_FileVersion(): Integer; CDECL;
var
    Header: THeaderRec;
begin
    if ActiveCircuit <> NIL then
    begin
        ReadMonitorHeader(Header, TRUE);
        Result := Header.Version;
        Exit;
    end;
    Result := 0;
end;
//------------------------------------------------------------------------------
procedure Monitors_Get_Header(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
// Variant list of strings with names of all channels
var
    Result: PPAnsiCharArray;
    Header: THeaderRec;
    k: Integer;
    ListSize: Integer;
    SaveDelims: String;
    SaveWhiteSpace: String;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit = NIL then
        Exit;
    if ActiveCircuit.Monitors.Active = NIL then
        Exit;
    ReadMonitorHeader(Header, TRUE);
    if Header.RecordSize <= 0 then
        Exit;
    ListSize := Header.RecordSize;
    DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, ListSize);

    with ActiveCircuit do
    begin
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
            Result[k] := DSS_CopyStringAsPChar(AuxParser.StrValue);
            Inc(k);
        end;
        AuxParser.AutoIncrement := FALSE; // be a good citizen
        AuxParser.Delimiters := SaveDelims;
        AuxParser.Whitespace := SaveWhiteSpace;
    end;
end;

procedure Monitors_Get_Header_GR(); CDECL;
// Same as Monitors_Get_Header but uses global result (GR) pointers
begin
    Monitors_Get_Header(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Monitors_Get_NumChannels(): Integer; CDECL;
var
    Header: THeaderRec;
begin
    Result := 0;
    if ActiveCircuit = NIL then
        Exit;
    ReadMonitorHeader(Header, TRUE);
    Result := Header.RecordSize;
end;
//------------------------------------------------------------------------------
function Monitors_Get_RecordSize(): Integer; CDECL;
var
    Header: THeaderRec;
begin
    Result := 0;
    if ActiveCircuit = NIL then
        Exit;
    ReadMonitorHeader(Header, TRUE);
    Result := Header.RecordSize;
end;
//------------------------------------------------------------------------------
function Monitors_Get_Element_AnsiString(): Ansistring; inline;
var
    pMon: TMonitorObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    Result := pMon.ElementName;
end;

function Monitors_Get_Element(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Monitors_Get_Element_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Monitors_Set_Element(const Value: PAnsiChar); CDECL;
var
    pMon: TMonitorObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    pMon.ElementName := Value;
    pMon.PropertyValue[1] := Value;
    pMon.RecalcElementData;
end;
//------------------------------------------------------------------------------
function Monitors_Get_Terminal(): Integer; CDECL;
var
    pMon: TMonitorObj;
begin
    Result := 0;
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    Result := pMon.MeteredTerminal;
end;
//------------------------------------------------------------------------------
procedure Monitors_Set_Terminal(Value: Integer); CDECL;
var
    pMon: TMonitorObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pMon := ActiveCircuit.Monitors.Active;
    if pMon = NIL then
        Exit;
    pMon.MeteredTerminal := Value;
    pMon.RecalcElementData;
end;
//------------------------------------------------------------------------------
function Monitors_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Monitors.ActiveIndex
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Monitors_Set_idx(Value: Integer); CDECL;
var
    pMonitor: TMonitorObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pMonitor := ActiveCircuit.Monitors.Get(Value);
    if pMonitor = NIL then
    begin
        DoSimpleMsg('Invalid Monitor index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := pMonitor;
end;
//------------------------------------------------------------------------------
end.
