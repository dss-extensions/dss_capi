unit CAPI_Monitors;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure Monitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
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
procedure Monitors_Get_ByteStream(var ResultPtr: PByte; ResultCount: PAPISize); CDECL;
procedure Monitors_Get_ByteStream_GR(); CDECL;
function Monitors_Get_SampleCount(): Integer; CDECL;
procedure Monitors_SampleAll(); CDECL;
procedure Monitors_SaveAll(); CDECL;
function Monitors_Get_Count(): Integer; CDECL;
procedure Monitors_Process(); CDECL;
procedure Monitors_ProcessAll(); CDECL;
procedure Monitors_Get_Channel(var ResultPtr: PDouble; ResultCount: PAPISize; Index: Integer); CDECL;
procedure Monitors_Get_Channel_GR(Index: Integer); CDECL;
procedure Monitors_Get_dblFreq(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Monitors_Get_dblFreq_GR(); CDECL;
procedure Monitors_Get_dblHour(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Monitors_Get_dblHour_GR(); CDECL;
function Monitors_Get_FileVersion(): Integer; CDECL;
procedure Monitors_Get_Header(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
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
    Math,
    DSSClass,
    DSSHelper,
    DSSObjectHelper;

type
    SingleArray = array[1..100] of Single;
    pSingleArray = ^SingleArray;

//------------------------------------------------------------------------------
function _activeObj(DSSPrime: TDSSContext; out obj: TMonitorObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
    
    obj := DSSPrime.ActiveCircuit.Monitors.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['Monitor'], 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Monitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.Monitors, False);
end;

procedure Monitors_Get_AllNames_GR(); CDECL;
// Same as Monitors_Get_AllNames but uses global result (GR) pointers
begin
    Monitors_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Monitors_Get_FileName(): PAnsiChar; CDECL;
var
    pMon: TMonitorObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, PMon.CSVFileName);
end;
//------------------------------------------------------------------------------
function Monitors_Get_First(): Integer; CDECL;
begin
    Result := 0;  // signify no more
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.Monitors);
end;
//------------------------------------------------------------------------------
function Monitors_Get_Next(): Integer; CDECL;
begin
    Result := 0;  // signify no more
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.Monitors);
    if Result <> 0 then Result := 1; //TODO: inconsistent with the rest
end;
//------------------------------------------------------------------------------
function Monitors_Get_Mode(): Integer; CDECL;
var
    pMon: TMonitorObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    Result := PMon.Mode
end;
//------------------------------------------------------------------------------
function Monitors_Get_Name(): PAnsiChar; CDECL;
var
    pMon: TMonitorObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, PMon.Name)
end;
//------------------------------------------------------------------------------
procedure Monitors_Reset(); CDECL;
var
    pMon: TMonitorObj;
begin
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    PMon.ResetIt();
end;
//------------------------------------------------------------------------------
procedure Monitors_ResetAll(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.MonitorClass.ResetAll;
end;
//------------------------------------------------------------------------------
procedure Monitors_Sample(); CDECL;
var
    pMon: TMonitorObj;
begin
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    PMon.TakeSample();
end;
//------------------------------------------------------------------------------
procedure Monitors_Save(); CDECL;
var
    pMon: TMonitorObj;
begin
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    PMon.Save();  // TranslateToCSV(False);
end;
//------------------------------------------------------------------------------
procedure Monitors_Set_Mode(Value: Integer); CDECL;
var
    pMon: TMonitorObj;
begin
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    PMon.Mode := Value;
    PMon.ResetIt();  // Always reset the monitor after a Mode change
end;
//------------------------------------------------------------------------------
procedure Monitors_Show(); CDECL;
var
    pMon: TMonitorObj;
begin
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    PMon.TranslateToCSV(TRUE);
end;
//------------------------------------------------------------------------------
procedure Monitors_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.MonitorClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.MonitorClass.ElementList.Active;
        DSSPrime.ActiveCircuit.Monitors.Get(DSSPrime.MonitorClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Monitor "%s" not found in Active Circuit.', [Value], 5004);
    end;
end;
//------------------------------------------------------------------------------
procedure Monitors_Get_ByteStream(var ResultPtr: PByte; ResultCount: PAPISize); CDECL;
var
    pMon: TMonitorObj;
begin
    if not _activeObj(DSSPrime, pMon) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    DSS_RecreateArray_PByte(ResultPtr, ResultCount, pmon.MonitorStream.Size);
    pmon.MonitorStream.Seek(0, soFromBeginning);
    pmon.MonitorStream.Read(ResultPtr^, pmon.MonitorStream.Size);   // Move it all over
    // leaves stream at the end
end;

procedure Monitors_Get_ByteStream_GR(); CDECL;
// Same as Monitors_Get_ByteStream but uses global result (GR) pointers
begin
    Monitors_Get_ByteStream(DSSPrime.GR_DataPtr_PByte, @DSSPrime.GR_Counts_PByte[0])
end;

//------------------------------------------------------------------------------
function Monitors_Get_SampleCount(): Integer; CDECL;
var
    pMon: TMonitorObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMon) then
        Exit;

    Result := pMon.SampleCount;
end;
//------------------------------------------------------------------------------
procedure Monitors_SampleAll(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.MonitorClass.SampleAll;
end;
//------------------------------------------------------------------------------
procedure Monitors_SaveAll(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.MonitorClass.SaveAll;
end;
//------------------------------------------------------------------------------
function Monitors_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Monitors.Count;
end;
//------------------------------------------------------------------------------
procedure Monitors_Process(); CDECL;
var
    pMon: TMonitorObj;
begin
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    pMon.PostProcess();
end;
//------------------------------------------------------------------------------
procedure Monitors_ProcessAll(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.MonitorClass.PostProcessAll;
end;
//------------------------------------------------------------------------------
procedure Monitors_Get_Channel(var ResultPtr: PDouble; ResultCount: PAPISize; Index: Integer); CDECL;
// Return an array of doubles for selected channel
var
    Result: PDoubleArray0;
    i: Integer;
    pMon: TMonitorObj;
    SngBuffer: pSingleArray;
    AllocSize: Integer;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    
    if pMon.SampleCount <= 0 then
        Exit;

    pMon.MonitorStream.Seek(256 + 4 * 4, soFromBeginning); // Skip header

    if (Index < 1) or (Index > pMon.RecordSize {NumChannels}) then
    begin
        DoSimpleMsg(DSSPrime,
            'Monitors.Channel: invalid channel index (%d), monitor "%s" has %d channels.',
            [Index, pMon.Name, pMon.RecordSize],
            5888);
        Exit;
    end;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, pMon.SampleCount);

    AllocSize := Sizeof(Single) * (pMon.RecordSize + 2); // Include Hour and Second fields
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
    Monitors_Get_Channel(DSSPrime.GR_DataPtr_PDouble, DSSPrime.GR_Counts_PDouble, Index)
end;

//------------------------------------------------------------------------------
procedure Monitors_Get_dblFreq(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Return an array of doubles for frequence for Harmonic solutions
var
    Result: PDoubleArray0;
    k, i: Integer;
    FirstCol: String;
    pMon: TMonitorObj;
    SngBuffer: pSingleArray;
    freq: Single;
    s: Single;
    AllocSize: Integer;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    if pMon.SampleCount <= 0 then
        Exit;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (pMon.SampleCount - 1) + 1);
    pMon.MonitorStream.Seek(256 + 4 * 4, soFromBeginning); // Skip header
    FirstCol := pMon.Header.Strings[0];
    // check first col to see if it is "Freq" for harmonics solution
    if SysUtils.CompareText(FirstCol, 'freq') = 0 then
    begin
        AllocSize := Sizeof(Single) * pMon.RecordSize;
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
    Monitors_Get_dblFreq(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Monitors_Get_dblHour(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Return an array of doubles for time in hours
var
    Result: PDoubleArray0;
    k, i: Integer;
    FirstCol: String;
    pMon: TMonitorObj;
    SngBuffer: pSingleArray;
    hr: Single;
    s: Single;
    AllocSize: Integer;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    if pMon.SampleCount <= 0 then
        Exit;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, pMon.SampleCount);
    pMon.MonitorStream.Seek(256 + 4 * 4, soFromBeginning); // Skip header
    FirstCol := pMon.Header.Strings[0];

    // check first col to see if it is "Hour"
    if Sysutils.CompareText(FirstCol, 'hour') = 0 then
    begin
        AllocSize := Sizeof(Single) * pMon.RecordSize;
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
    Monitors_Get_dblHour(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function Monitors_Get_FileVersion(): Integer; CDECL;
var
    pMon: TMonitorObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMon) then
        Exit;

    Result := pMon.FileVersion;
end;
//------------------------------------------------------------------------------
procedure Monitors_Get_Header(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
// Variant list of strings with names of all channels
var
    Result: PPAnsiCharArray0;
    k: Integer;
    ListSize: Integer;
    pMon: TMonitorObj;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, pMon) then
        Exit;

    if pMon.RecordSize <= 0 then
        Exit;

    ListSize := pMon.RecordSize;
    DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, ListSize);

    with DSSPrime.ActiveCircuit do
    begin
        k := 0;
        while k < ListSize do
        begin
            Result[k] := DSS_CopyStringAsPChar(pMon.Header.Strings[k + 2]);
            Inc(k);
        end;
    end;
end;

procedure Monitors_Get_Header_GR(); CDECL;
// Same as Monitors_Get_Header but uses global result (GR) pointers
begin
    Monitors_Get_Header(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Monitors_Get_NumChannels(): Integer; CDECL;
var
    pMon: TMonitorObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    Result := pMon.RecordSize;
end;
//------------------------------------------------------------------------------
function Monitors_Get_RecordSize(): Integer; CDECL;
var
    pMon: TMonitorObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    Result := pMon.RecordSize;
end;
//------------------------------------------------------------------------------
function Monitors_Get_Element(): PAnsiChar; CDECL;
var
    pMon: TMonitorObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, pMon) then
        Exit;

    if pMon.MeteredElement <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, AnsiLowerCase(pMon.MeteredElement.FullName));
end;
//------------------------------------------------------------------------------
procedure Monitors_Set_Element(const Value: PAnsiChar); CDECL;
var
    pMon: TMonitorObj;
begin
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    pMon.ParsePropertyValue(ord(TMonitorProp.element), Value);
    pMon.SetAsNextSeq(ord(TMonitorProp.Element));
    pMon.RecalcElementData;
end;
//------------------------------------------------------------------------------
function Monitors_Get_Terminal(): Integer; CDECL;
var
    pMon: TMonitorObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    Result := pMon.MeteredTerminal;
end;
//------------------------------------------------------------------------------
procedure Monitors_Set_Terminal(Value: Integer); CDECL;
var
    pMon: TMonitorObj;
begin
    if not _activeObj(DSSPrime, pMon) then
        Exit;
    pMon.MeteredTerminal := Value;
    pMon.RecalcElementData();
end;
//------------------------------------------------------------------------------
function Monitors_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Monitors.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Monitors_Set_idx(Value: Integer); CDECL;
var
    pMonitor: TMonitorObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    pMonitor := DSSPrime.ActiveCircuit.Monitors.Get(Value);
    if pMonitor = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['Monitor', Value], 656565);
        Exit;
    end;

    DSSPrime.ActiveCircuit.ActiveCktElement := pMonitor;
end;
//------------------------------------------------------------------------------
end.
