UNIT CAPI_Monitors;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE Monitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Monitors_Get_AllNames_GR();cdecl;
function Monitors_Get_FileName():PAnsiChar;cdecl;
function Monitors_Get_First():Integer;cdecl;
function Monitors_Get_Mode():Integer;cdecl;
function Monitors_Get_Name():PAnsiChar;cdecl;
function Monitors_Get_Next():Integer;cdecl;
procedure Monitors_Reset();cdecl;
procedure Monitors_ResetAll();cdecl;
procedure Monitors_Sample();cdecl;
procedure Monitors_Save();cdecl;
procedure Monitors_Set_Mode(Value: Integer);cdecl;
procedure Monitors_Show();cdecl;
procedure Monitors_Set_Name(const Value: PAnsiChar);cdecl;
PROCEDURE Monitors_Get_ByteStream(var ResultPtr: PByte; ResultCount: PInteger);cdecl;
PROCEDURE Monitors_Get_ByteStream_GR();cdecl;
function Monitors_Get_SampleCount():Integer;cdecl;
procedure Monitors_SampleAll();cdecl;
procedure Monitors_SaveAll();cdecl;
function Monitors_Get_Count():Integer;cdecl;
procedure Monitors_Process();cdecl;
procedure Monitors_ProcessAll();cdecl;
PROCEDURE Monitors_Get_Channel(var ResultPtr: PDouble; ResultCount: PInteger; Index: Integer);cdecl;
PROCEDURE Monitors_Get_Channel_GR(Index: Integer);cdecl;
PROCEDURE Monitors_Get_dblFreq(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Monitors_Get_dblFreq_GR();cdecl;
PROCEDURE Monitors_Get_dblHour(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Monitors_Get_dblHour_GR();cdecl;
function Monitors_Get_FileVersion():Integer;cdecl;
PROCEDURE Monitors_Get_Header(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Monitors_Get_Header_GR();cdecl;
function Monitors_Get_NumChannels():Integer;cdecl;
function Monitors_Get_RecordSize():Integer;cdecl;
function Monitors_Get_Element():PAnsiChar;cdecl;
procedure Monitors_Set_Element(const Value: PAnsiChar);cdecl;
function Monitors_Get_Terminal():Integer;cdecl;
procedure Monitors_Set_Terminal(Value: Integer);cdecl;

IMPLEMENTATION

USES CAPI_Constants, Monitor, DSSGlobals, SysUtils, Classes, Math;


Type THeaderRec = Record
        Signature  : Integer;
        Version    : Integer;
        RecordSize : Integer;
        Mode       : Integer;
        StrBuffer  : TMonitorStrBuffer;
      End;

     SingleArray  = Array[1..100] of Single;
     pSingleArray = ^SingleArray;
        Procedure ReadMonitorHeader(Var HeaderRec:THeaderRec; Opt:Boolean);
VAR
    pMon : TMonitorObj;

Begin
   pMon := ActiveCircuit[ActiveActor].Monitors.Active;
   TRY
       With pmon.MonitorStream, HeaderRec Do
         Begin
           Seek(0,           classes.soFromBeginning  );
           Read( signature,  Sizeof(signature));    // Signature   (32 bit Integer )
           Read( version,    Sizeof(version));        // Version     (32 bit Integer )
           Read( RecordSize, Sizeof(RecordSize));    // RecordSize  (32 bit Integer )
           Read( Mode,       Sizeof(Mode));                // Mode        (32 bit Integer )
           Read( StrBuffer,  Sizeof(TMonitorStrBuffer)); // String      (255 char string)
         End;

   FINALLY
          // If opt is false leave monitorstream at end of header record
          If Opt Then pmon.MonitorStream.Seek(0, soFromEnd);    // put monitor stream pointer back where it was
   END;


End;
//------------------------------------------------------------------------------
PROCEDURE Monitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  MonitorElem:TMonitorObj;
  k:Integer;

Begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     If Monitors.ListSize>0 Then
     Begin
       DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (Monitors.ListSize-1) + 1);
       k:=0;
       MonitorElem := Monitors.First;
       WHILE MonitorElem<>Nil DO Begin
          Result[k] := DSS_CopyStringAsPChar(MonitorElem.Name);
          Inc(k);
          MonitorElem := Monitors.Next;
       End;
     End;
end;
PROCEDURE Monitors_Get_AllNames_GR();cdecl;
// Same as Monitors_Get_AllNames but uses global result (GR) pointers
begin
   Monitors_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Monitors_Get_FileName_AnsiString():AnsiString;inline;
Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        If PMon <> Nil Then Result := PMon.Get_FileName(ActiveActor)
        Else Result := '';
   End;

end;

function Monitors_Get_FileName():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Monitors_Get_FileName_AnsiString());
end;
//------------------------------------------------------------------------------
function Monitors_Get_First():Integer;cdecl;
Var
   pMon:TMonitorObj;

Begin

     Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMon := ActiveCircuit[ActiveActor].Monitors.First;
        If pMon <> Nil Then
        Begin
          Repeat
            If pMon.enabled
            then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pMon;
              Result := 1;
            End
            Else  pMon := ActiveCircuit[ActiveActor].Monitors.Next;
          Until (Result = 1) or (pMon = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;
//------------------------------------------------------------------------------
function Monitors_Get_Mode():Integer;cdecl;
Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        If PMon <> Nil Then Result := PMon.Mode
        Else Result := 0;
   End;

end;
//------------------------------------------------------------------------------
function Monitors_Get_Name_AnsiString():AnsiString;inline;
Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        If PMon <> Nil Then Result := PMon.Name
        Else Result := '';
   End;

end;

function Monitors_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Monitors_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Monitors_Get_Next():Integer;cdecl;
Var
   pMon:TMonitorObj;

Begin

   Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Next;
        If pMon <> Nil Then
        Begin
          Repeat
            If pMon.Enabled
            Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pMon;
              Result := 1;
            End
            Else  pMon := ActiveCircuit[ActiveActor].Monitors.Next;
          Until (Result > 0) or (pMon = nil);
        End
        Else
            Result := 0;  // signify no more
   End;


end;
//------------------------------------------------------------------------------
procedure Monitors_Reset();cdecl;
Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        If PMon <> Nil Then PMon.ResetIt(ActiveActor);
   End;

end;
//------------------------------------------------------------------------------
procedure Monitors_ResetAll();cdecl;
Begin
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
        MonitorClass[ActiveActor].ResetAll(ActiveActor);
     End;
end;
//------------------------------------------------------------------------------
procedure Monitors_Sample();cdecl;
Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        If PMon <> Nil Then PMon.TakeSample(ActiveActor);
   End;

end;
//------------------------------------------------------------------------------
procedure Monitors_Save();cdecl;
Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        If PMon <> Nil Then PMon.Save;  // TranslateToCSV(False);
   End;

end;
//------------------------------------------------------------------------------
procedure Monitors_Set_Mode(Value: Integer);cdecl;
Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        If PMon <> Nil Then
        Begin
          PMon.Mode := Value;
          PMon.ResetIt(ActiveActor);  // Always reset the monitor after a Mode change
        End;
   End;

end;
//------------------------------------------------------------------------------
procedure Monitors_Show();cdecl;
Var
   pMon:TMonitorObj;

Begin

   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        If PMon <> Nil Then PMon.TranslateToCSV(True, ActiveActor);
   End;

end;
//------------------------------------------------------------------------------
procedure Monitors_Set_Name(const Value: PAnsiChar);cdecl;
VAR
    activesave :integer;
    Mon:TMonitorObj;
    S: String;
    Found :Boolean;
Begin


  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin      // Search list of monitors in active circuit for name
       WITH ActiveCircuit[ActiveActor].Monitors DO
       Begin
         S := Value;  // Convert to Pascal String
         Found := FALSE;
         ActiveSave := ActiveIndex;
         Mon := First;
         While Mon <> NIL Do Begin
            IF (CompareText(Mon.Name, S) = 0) THEN Begin
                ActiveCircuit[ActiveActor].ActiveCktElement := Mon;
                Found := TRUE;
                Break;
            End;
            Mon := Next;
         End;
         IF NOT Found THEN Begin
             DoSimpleMsg('Monitor "'+S+'" Not Found in Active Circuit.', 5004);
             Mon := Get(ActiveSave);    // Restore active Monerator
             ActiveCircuit[ActiveActor].ActiveCktElement := Mon;
         End;
       End;
  End;


end;
//------------------------------------------------------------------------------
PROCEDURE Monitors_Get_ByteStream(var ResultPtr: PByte; ResultCount: PInteger);cdecl;
VAR
  Result: PByteArray;
   pMon:TMonitorObj;
   p:Pointer;

Begin

   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        If PMon <> Nil Then Begin
          Result := DSS_RecreateArray_PByte(ResultPtr, ResultCount, (pmon.MonitorStream.Size -1) + 1);
          pmon.MonitorStream.Seek(0, soFromBeginning);
          p := ResultPtr;
          pmon.MonitorStream.Read(p^, pmon.MonitorStream.Size);   // Move it all over
          // leaves stream at the end
          // VarArrayUnlock(Result);
        End
        Else
             Result := DSS_RecreateArray_PByte(ResultPtr, ResultCount, (0) + 1);
   End;

end;
PROCEDURE Monitors_Get_ByteStream_GR();cdecl;
// Same as Monitors_Get_ByteStream but uses global result (GR) pointers
begin
   Monitors_Get_ByteStream(GR_DataPtr_PByte, GR_CountPtr_PByte)
end;

//------------------------------------------------------------------------------
function Monitors_Get_SampleCount():Integer;cdecl;
Var
   pMon:TMonitorObj;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
         pMon := ActiveCircuit[ActiveActor].Monitors.Active;
         Result := pMon.SampleCount;
     End;
end;
//------------------------------------------------------------------------------
procedure Monitors_SampleAll();cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
         MonitorClass[ActiveActor].SampleAll(ActiveActor);
     End;
end;
//------------------------------------------------------------------------------
procedure Monitors_SaveAll();cdecl;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then Begin
         MonitorClass[ActiveActor].SaveAll(ActiveActor);
     End;
end;
//------------------------------------------------------------------------------
function Monitors_Get_Count():Integer;cdecl;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
         Result := ActiveCircuit[ActiveActor].Monitors.ListSize;
     End;
end;
//------------------------------------------------------------------------------
procedure Monitors_Process();cdecl;
var
  pMon:TMonitorObj;
begin
  if ActiveCircuit[ActiveActor] <> Nil then begin
    pMon := ActiveCircuit[ActiveActor].Monitors.Active;
    if PMon <> Nil then pMon.PostProcess(ActiveActor);
  end;
end;
//------------------------------------------------------------------------------
procedure Monitors_ProcessAll();cdecl;
begin
  If ActiveCircuit[ActiveActor] <> Nil Then Begin
    MonitorClass[ActiveActor].PostProcessAll(ActiveActor);
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE Monitors_Get_Channel(var ResultPtr: PDouble; ResultCount: PInteger; Index: Integer);cdecl;
// Return an array of doubles for selected channel
VAR
  Result: PDoubleArray;  Header : THeaderRec;
     k , i : Integer;
     FirstCol : String;
     pMon : TMonitorObj;
     SngBuffer : pSingleArray;
     hr : Single;
     s  : Single;
     AllocSize : Integer;

begin

    If ActiveCircuit[ActiveActor] <> Nil Then Begin

      pMon := ActiveCircuit[ActiveActor].Monitors.Active;
      If pMon.SampleCount >0 Then Begin

             Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (pMon.SampleCount-1) + 1);
             ReadMonitorHeader(Header, FALSE);   // FALSE = leave at beginning of data
             AuxParser.CmdString := string(Header.StrBuffer);
             AuxParser.AutoIncrement := TRUE;
             FirstCol := AuxParser.StrValue;  // Get rid of first two columns
             AuxParser.AutoIncrement := FALSE;

              AllocSize :=  Sizeof(Single) * Header.RecordSize;
              SngBuffer := Allocmem(AllocSize);
              k := 0;
              for i := 1 to pMon.SampleCount  do Begin
                   With pMon.MonitorStream Do
                    Begin
                        Read( hr, SizeOf(hr) );
                        Read( s,  SizeOf(s) );
                        Read( sngBuffer^[1], AllocSize);  // read rest of record
                    End;
                    Result[k] := sngBuffer^[index];
                    inc(k);
              End;

              Reallocmem(SngBuffer, 0);  // Dispose of buffer

      End
      Else   Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

    End;
end;
PROCEDURE Monitors_Get_Channel_GR(Index: Integer);cdecl;
// Same as Monitors_Get_Channel but uses global result (GR) pointers
begin
   Monitors_Get_Channel(GR_DataPtr_PDouble, GR_CountPtr_PDouble, Index)
end;

//------------------------------------------------------------------------------
PROCEDURE Monitors_Get_dblFreq(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
// Return an array of doubles for frequence for Harmonic solutions
VAR
  Result: PDoubleArray;  Header : THeaderRec;
     k , i : Integer;
     FirstCol : String;
     pMon : TMonitorObj;
     SngBuffer : pSingleArray;
     freq : Single;
     s  : Single;
     AllocSize : Integer;

begin

    If ActiveCircuit[ActiveActor] <> Nil Then Begin

      pMon := ActiveCircuit[ActiveActor].Monitors.Active;
      If pMon.SampleCount >0 Then Begin
             Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (pMon.SampleCount-1) + 1);
             ReadMonitorHeader(Header, FALSE);   // leave at beginning of data
             AuxParser.CmdString := string(Header.StrBuffer);
             AuxParser.AutoIncrement := TRUE;
             FirstCol := AuxParser.StrValue;  // Get rid of first two columns
             AuxParser.AutoIncrement := FALSE;
             // check first col to see if it is "Freq" for harmonics solution
             If Sysutils.CompareText(FirstCol, 'freq') = 0  Then Begin
                  AllocSize :=  Sizeof(Single) * Header.RecordSize;
                  SngBuffer := Allocmem(AllocSize);
                  k := 0;
                  for i := 1 to pMon.SampleCount  do Begin
                       With pMon.MonitorStream Do
                        Begin
                            Read( freq, SizeOf(freq) );  // frequency
                            Read( s,  SizeOf(s) );   // harmonic
                            Read( sngBuffer^[1], AllocSize);  // read rest of record
                        End;
                        Result[k] := freq;
                        inc(k);
                  End;

                  Reallocmem(SngBuffer, 0);  // Dispose of buffer

             End Else Begin   // Not harmonic solution, so return nil array
                Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
                pMon.MonitorStream.Seek(0, soFromEnd) ; // leave stream at end
             End;
      End
      Else   Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

    End;

end;
PROCEDURE Monitors_Get_dblFreq_GR();cdecl;
// Same as Monitors_Get_dblFreq but uses global result (GR) pointers
begin
   Monitors_Get_dblFreq(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Monitors_Get_dblHour(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
// Return an array of doubles for time in hours
VAR
  Result: PDoubleArray;  Header : THeaderRec;
     k , i : Integer;
     FirstCol : String;
     pMon : TMonitorObj;
     SngBuffer : pSingleArray;
     hr : Single;
     s  : Single;
     AllocSize : Integer;

begin

    If ActiveCircuit[ActiveActor] <> Nil Then Begin

      pMon := ActiveCircuit[ActiveActor].Monitors.Active;
      If pMon.SampleCount >0 Then Begin
             Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (pMon.SampleCount-1) + 1);
             ReadMonitorHeader(Header, FALSE);   // leave at beginning of data
             AuxParser.CmdString := string(Header.StrBuffer);
             AuxParser.AutoIncrement := TRUE;
             FirstCol := AuxParser.StrValue;  // Get rid of first two columns
             AuxParser.AutoIncrement := FALSE;
             // check first col to see if it is "Hour"
             If Sysutils.CompareText(FirstCol, 'hour') = 0  Then Begin
                  AllocSize :=  Sizeof(Single) * Header.RecordSize;
                  SngBuffer := Allocmem(AllocSize);
                  k := 0;
                  for i := 1 to pMon.SampleCount  do Begin
                       With pMon.MonitorStream Do
                        Begin
                            Read( hr, SizeOf(hr) );  // Hour
                            Read( s,  SizeOf(s) );   // Seconds past the hour
                            Read( sngBuffer^[1], AllocSize);  // read rest of record
                        End;
                        Result[k] := hr + s / 3600.0;
                        inc(k);
                  End;

                  Reallocmem(SngBuffer, 0);  // Dispose of buffer

             End Else Begin   // Not time solution, so return nil array
                Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
                pMon.MonitorStream.Seek(0, soFromEnd) ; // leave stream at end
             End;
      End
      Else   Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);

    End;

end;
PROCEDURE Monitors_Get_dblHour_GR();cdecl;
// Same as Monitors_Get_dblHour but uses global result (GR) pointers
begin
   Monitors_Get_dblHour(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Monitors_Get_FileVersion():Integer;cdecl;
Var  Header : THeaderRec;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then Begin

       Result := Header.Version;
    End;

end;
//------------------------------------------------------------------------------
PROCEDURE Monitors_Get_Header(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
// Variant list of strings with names of all channels
VAR
  Result: PPAnsiCharArray;  Header : THeaderRec;
     k : Integer;
     ListSize : Integer;
     SaveDelims : String;
     SaveWhiteSpace : String;
begin

    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
         ReadMonitorHeader(Header, TRUE);
         If Header.RecordSize > 0 Then
         Begin
             ListSize := Header.RecordSize;
             DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (ListSize-1) + 1);
             k:=0;
             SaveDelims := AuxParser.Delimiters;
             AuxParser.Delimiters := ',';
             SaveWhiteSpace := AuxParser.Whitespace;
             AuxParser.Whitespace := '';
             AuxParser.CmdString := String(Header.StrBuffer);
             AuxParser.AutoIncrement := TRUE;
             AuxParser.StrValue;  // Get rid of first two columns
             AuxParser.StrValue;
             WHILE k < ListSize DO Begin
                Result[k] := DSS_CopyStringAsPChar(AuxParser.StrValue);
                Inc(k);
             End;
             AuxParser.AutoIncrement := FALSE; // be a good citizen
             AuxParser.Delimiters := SaveDelims;
             AuxParser.Whitespace := SaveWhiteSpace;
         End;
     End;

end;
PROCEDURE Monitors_Get_Header_GR();cdecl;
// Same as Monitors_Get_Header but uses global result (GR) pointers
begin
   Monitors_Get_Header(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Monitors_Get_NumChannels():Integer;cdecl;
Var  Header:THeaderRec;
begin

    If ActiveCircuit[ActiveActor] <> Nil Then Begin
        ReadMonitorHeader(Header, TRUE);
        Result := Header.RecordSize;
    End;
end;
//------------------------------------------------------------------------------
function Monitors_Get_RecordSize():Integer;cdecl;
Var  Header:THeaderRec;
begin

    If ActiveCircuit[ActiveActor] <> Nil Then Begin
        ReadMonitorHeader(Header, TRUE);
        Result := Header.RecordSize;
    End;
end;
//------------------------------------------------------------------------------
function Monitors_Get_Element_AnsiString():AnsiString;inline;
var
  pMon:TMonitorObj;
begin
  if ActiveCircuit[ActiveActor] <> Nil then begin
    pMon := ActiveCircuit[ActiveActor].Monitors.Active;
    if PMon <> Nil then Result := pMon.ElementName ;
  end;
end;

function Monitors_Get_Element():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Monitors_Get_Element_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Monitors_Set_Element(const Value: PAnsiChar);cdecl;
var
  pMon:TMonitorObj;
begin
  if ActiveCircuit[ActiveActor] <> Nil then begin
    pMon := ActiveCircuit[ActiveActor].Monitors.Active;
    if PMon <> Nil then Begin
       pMon.ElementName := Value ;
       pMon.PropertyValue [1] := Value;
       pMon.RecalcElementData(ActiveActor);
    End;
  end;

end;
//------------------------------------------------------------------------------
function Monitors_Get_Terminal():Integer;cdecl;
var
  pMon:TMonitorObj;
begin
  if ActiveCircuit[ActiveActor] <> Nil then begin
    pMon := ActiveCircuit[ActiveActor].Monitors.Active;
    if PMon <> Nil then Result := pMon.MeteredTerminal ;
  end;
end;
//------------------------------------------------------------------------------
procedure Monitors_Set_Terminal(Value: Integer);cdecl;
var
  pMon:TMonitorObj;
begin
  if ActiveCircuit[ActiveActor] <> Nil then begin
    pMon := ActiveCircuit[ActiveActor].Monitors.Active;
    if PMon <> Nil then Begin
       pMon.MeteredTerminal  := Value ;
       pMon.RecalcElementData(ActiveActor);
    End;
  end;

end;
//------------------------------------------------------------------------------
END.
