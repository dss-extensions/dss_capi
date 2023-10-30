unit DMonitors;

interface

function MonitorsI(mode:longint; arg: longint):longint; cdecl;
function MonitorsS(mode:longint; arg: pAnsiChar):pAnsiChar; cdecl;
procedure MonitorsV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses Monitor,
     DSSGlobals,
     SysUtils,
     Classes,
     Variants,
     Math;

Type THeaderRec = Record
        Signature  : Integer;
        Version    : Integer;
        RecordSize : Integer;
        Mode       : Integer;
        dummyRec   : TMonitorStrBuffer;
      End;

     SingleArray  = Array[1..100] of Single;
     pSingleArray = ^SingleArray;

Procedure ReadMonitorHeader(Var HeaderRec:THeaderRec; Opt:Boolean);
VAR
    mypMon : TMonitorObj;

Begin
   mypMon := ActiveCircuit[ActiveActor].Monitors.Active;
   TRY
       With mypmon.MonitorStream, HeaderRec Do
         Begin
           Seek(0,           classes.soFromBeginning  );
           Read( signature,  Sizeof(signature));    // Signature   (32 bit Integer )
           Read( version,    Sizeof(version));        // Version     (32 bit Integer )
           Read( RecordSize, Sizeof(RecordSize));    // RecordSize  (32 bit Integer )
           Read( Mode,       Sizeof(Mode));                // Mode        (32 bit Integer )
           Read( dummyRec,   Sizeof(TMonitorStrBuffer)); // String      (255 char string)
         End;

   FINALLY
          // If opt is false leave monitorstream at end of header record
          If Opt Then mypmon.MonitorStream.Seek(0, soFromEnd);    // put monitor stream pointer back where it was
   END;


End;

function MonitorsI(mode:longint; arg: longint):longint; cdecl;

Var
   pMon       : TMonitorObj;
   Header     : THeaderRec;

begin
  Result:=0;  // Default return value
  case mode of
  0: begin  // Monitors.First
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
  1: begin  // Monitors.Next
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
  2: begin  // Monitors.Reset
       If ActiveCircuit[ActiveActor] <> Nil Then
       Begin
            pMon := ActiveCircuit[ActiveActor].Monitors.Active;
            If PMon <> Nil Then PMon.ResetIt(ActiveActor);
       End;
       Result := 0;
  end;
  3: begin  // Monitors.ResetAll
         If ActiveCircuit[ActiveActor] <> Nil Then Begin
            MonitorClass[ActiveActor].ResetAll(ActiveActor);
         End;
         Result := 0;
  end;
  4: begin  // Monitors.Sample
       If ActiveCircuit[ActiveActor] <> Nil Then
       Begin
            pMon := ActiveCircuit[ActiveActor].Monitors.Active;
            If PMon <> Nil Then PMon.TakeSample(ActiveActor);
       End;
       Result := 0;
  end;
  5: begin  // Monitors.Save
       If ActiveCircuit[ActiveActor] <> Nil Then
       Begin
            pMon := ActiveCircuit[ActiveActor].Monitors.Active;
            If PMon <> Nil Then PMon.Save;  // TranslateToCSV(False);
       End;
       Result := 0;
  end;
  6: begin  // Monitors.Show
       If ActiveCircuit[ActiveActor] <> Nil Then
       Begin
            pMon := ActiveCircuit[ActiveActor].Monitors.Active;
            If PMon <> Nil Then PMon.TranslateToCSV(True,ActiveActor);
       End;
       Result := 0;
  end;
  7: begin  // Monitors.Mode read
       If ActiveCircuit[ActiveActor] <> Nil Then
       Begin
            pMon := ActiveCircuit[ActiveActor].Monitors.Active;
            If PMon <> Nil Then Result := PMon.Mode
            Else Result := 0;
       End;
  end;
  8: begin  // Monitors.Mode Write
      If ActiveCircuit[ActiveActor] <> Nil Then
       Begin
            pMon := ActiveCircuit[ActiveActor].Monitors.Active;
            If PMon <> Nil Then
            Begin
              PMon.Mode := arg;
              PMon.ResetIt(ActiveActor);  // Always reset the monitor after a Mode change
            End;
       End;
       Result := 0;
  end;
  9: begin  // Monitors.SampleCount
       If ActiveCircuit[ActiveActor] <> Nil Then Begin
           pMon := ActiveCircuit[ActiveActor].Monitors.Active;
           Result := pMon.SampleCount;
       End;
  end;
  10: begin  // Monitors.SampleAll
       If ActiveCircuit[ActiveActor] <> Nil Then Begin
           MonitorClass[ActiveActor].SampleAll(ActiveActor);
       End;
       Result:=0;
  end;
  11: begin  // Monitor.SaveAll
       If ActiveCircuit[ActiveActor] <> Nil Then Begin
           MonitorClass[ActiveActor].SaveAll(ActiveActor);
       End;
  end;
  12: begin  // Monitor.Count
      If ActiveCircuit[ActiveActor] <> Nil Then Begin
           Result := ActiveCircuit[ActiveActor].Monitors.ListSize;
       End;
  end;
  13: begin  // Monitor.Process
      if ActiveCircuit[ActiveActor] <> Nil then begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> Nil then pMon.PostProcess(ActiveActor);
      end;
      Result:=0;
  end;
  14: begin  // Monitor.ProcessAll
      If ActiveCircuit[ActiveActor] <> Nil Then Begin
        MonitorClass[ActiveActor].PostProcessAll(ActiveActor);
      End;
      Result:=0;
  end;
  15: begin  // Monitor.FileVersion
      If ActiveCircuit[ActiveActor] <> Nil Then Begin
         ReadMonitorHeader(Header, TRUE);
         Result := Header.Version;
      End;
  end;
  16: begin // Monitor.RecordSize
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
        ReadMonitorHeader(Header, TRUE);
        Result := Header.RecordSize;
    End;
  end;
  17: begin  // Monitor.NumChannels
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
        ReadMonitorHeader(Header, TRUE);
        Result := Header.RecordSize;
    End;
  end;
  18: begin  // Monitor.Terminal read
      if ActiveCircuit[ActiveActor] <> Nil then begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> Nil then Result := pMon.MeteredTerminal ;
      end;
  end;
  19: begin  // Monitor.Terminal Write
      if ActiveCircuit[ActiveActor] <> Nil then begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> Nil then Begin
           pMon.MeteredTerminal  := arg ;
           pMon.RecalcElementData(ActiveActor) ;
        End;
        Result:=0;
  end;
  end
  else
      Result:=-1; // The parameter is not valid
  end;
end;

//***********************String Type properties*********************************
function MonitorsS(mode:longint; arg: pAnsiChar):pAnsiChar; cdecl;

Var
   pMon       : TMonitorObj;
   activesave : integer;
   S          : String;
   Found      : Boolean;

begin
  Result := pAnsiChar(AnsiString('0')); // Default return value
  case mode of
  0: begin  // Monitors.FIleName
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          pMon := ActiveCircuit[ActiveActor].Monitors.Active;
          If PMon <> Nil Then Result := pAnsiChar(AnsiString(PMon.Get_FileName(ActiveActor)))
          Else Result := pAnsiChar(AnsiString(''));
     End;
  end;
  1: begin // Monitors.Name read
       If ActiveCircuit[ActiveActor] <> Nil Then
       Begin
            pMon := ActiveCircuit[ActiveActor].Monitors.Active;
            If PMon <> Nil Then Result := pAnsiChar(AnsiString(PMon.Name))
            Else Result := pAnsiChar(AnsiString(''));
   End;
  end;
  2: begin  // Monitors.Nme Write
      IF ActiveCircuit[ActiveActor] <> NIL
      THEN Begin      // Search list of monitors in active circuit for name
           WITH ActiveCircuit[ActiveActor].Monitors DO
           Begin
             S := string(arg);  // Convert to Pascal String
             Found := FALSE;
             ActiveSave := ActiveIndex;
             pMon := First;
             While pMon <> NIL Do Begin
                IF (CompareText(pMon.Name, S) = 0) THEN Begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pMon;
                    Found := TRUE;
                    Break;
                End;
                pMon := Next;
             End;
             IF NOT Found THEN Begin
                 DoSimpleMsg('Monitor "'+S+'" Not Found in Active Circuit.', 5004);
                 pMon := Get(ActiveSave);    // Restore active Monerator
                 ActiveCircuit[ActiveActor].ActiveCktElement := pMon;
             End;
           End;
      End;
  end;
  3: begin  // Monitors.Element read
      if ActiveCircuit[ActiveActor] <> Nil then begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> Nil then Result := pAnsiChar(AnsiString(pMon.ElementName)) ;
      end;
  end;
  4: begin  // Monitors.Element Write
     if ActiveCircuit[ActiveActor] <> Nil then begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        if PMon <> Nil then Begin
           s := string(arg);
           pMon.ElementName := s;
           pMon.PropertyValue [1] := s;
           pMon.RecalcElementData(ActiveActor) ;
        End;
     end;
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//****************************Variant type properties***************************
procedure MonitorsV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

Var
  MonitorElem     : TMonitorObj;
  AllocSize,
  i , k , index,
  ListSize        : Integer;
  pMon            : TMonitorObj;
  p               : Pointer;
  Header          : THeaderRec;
  TempStr,
  SaveDelims,
  SaveWhiteSpace,
  FirstCol        : String;
  hr,
  s,
  freq            : Single;
  SngBuffer       : pSingleArray;
  PInt            : ^Integer;

begin
  {$IFDEF FPC_DLL}initialize(SngBuffer);{$ENDIF}
  case mode of
  0:begin  // Monitors.AllNames
      myType  :=  4;        // String
      setlength(myStrArray,0);
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        WITH ActiveCircuit[ActiveActor] DO
        Begin
          If Monitors.ListSize>0 Then
          Begin
            MonitorElem := Monitors.First;
            WHILE MonitorElem<>Nil DO Begin
              WriteStr2Array(MonitorElem.Name);
              WriteStr2Array(Char(0));
              MonitorElem := Monitors.Next;
            End;
          End;
        End;
      End
      Else  WriteStr2Array('');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    end;
  1:begin  // Monitor.ByteStream
      myType  :=  5;        // NOT a String
      setlength(myStrArray,0);
      If ActiveCircuit[ActiveActor] <> Nil Then
      Begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        If PMon <> Nil Then Begin
          setlength(myStrArray, pmon.MonitorStream.Size);
          pmon.MonitorStream.Seek(0, soFromBeginning);
          p := @(myStrArray[0]);
          pmon.MonitorStream.Read(p^, pmon.MonitorStream.Size);   // Move it all over
        End
      End
      Else  WriteStr2Array('');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    end;
  2:begin  // Monitors.Header
      myType  :=  4;        // String
      setlength(myStrArray,0);
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        WITH ActiveCircuit[ActiveActor] DO
        Begin
          pMon := ActiveCircuit[ActiveActor].Monitors.Active;
          ReadMonitorHeader(Header, TRUE);
          If length(pMon.StrBuffer) > 0 Then
          Begin
            ListSize := Header.RecordSize;
            SaveDelims := AuxParser[ActiveActor].Delimiters;
            AuxParser[ActiveActor].Delimiters  :=  ',';
            SaveWhiteSpace := AuxParser[ActiveActor].Whitespace;
            AuxParser[ActiveActor].Whitespace  :=  '';
            TempStr                            :=  '';
            for i := 0 to High(pMon.StrBuffer) do       // Moves the content to a string var
            TempStr   :=  TempStr + string(pMon.StrBuffer[i]); // For some reason needed for DLL

            AuxParser[ActiveActor].CmdString := TempStr;
            AuxParser[ActiveActor].AutoIncrement := TRUE;
            AuxParser[ActiveActor].StrValue;  // Get rid of first two columns
            AuxParser[ActiveActor].StrValue;
            k :=  0;
            WHILE k < ListSize DO
            Begin
              WriteStr2Array(AuxParser[ActiveActor].StrValue);
              WriteStr2Array(Char(0));
              inc(k);
            End;
            AuxParser[ActiveActor].AutoIncrement := FALSE; // be a good citizen
            AuxParser[ActiveActor].Delimiters := SaveDelims;
            AuxParser[ActiveActor].Whitespace := SaveWhiteSpace;
          End;
        End;
      End
      Else  WriteStr2Array('');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    end;
  3:begin  // Monitors.dblHour
      myType  :=  2;        // Double
      setlength(myDBLArray, 1);
      myDBLArray[0] := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then Begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        If pMon.SampleCount >0 Then Begin
          setlength(myDBLArray, pMon.SampleCount);
          ReadMonitorHeader(Header, FALSE);   // leave at beginning of data

          TempStr                            :=  '';
          for i := 0 to High(pMon.StrBuffer) do       // Moves the content to a string var
          TempStr   :=  TempStr + string(pMon.StrBuffer[i]); // For some reason needed for DLL

          AuxParser[ActiveActor].CmdString := string(TempStr);
          AuxParser[ActiveActor].AutoIncrement := TRUE;
          FirstCol := AuxParser[ActiveActor].StrValue;  // Get rid of first two columns
          AuxParser[ActiveActor].AutoIncrement := FALSE;
          // check first col to see if it is "Hour"
          If CompareText(FirstCol, 'hour') = 0  Then
          Begin
            AllocSize :=  Sizeof(SngBuffer^[1]) * Header.RecordSize;
            SngBuffer := Allocmem(AllocSize);
            k := 0;
            for i := 1 to pMon.SampleCount  do
            Begin
              With pMon.MonitorStream Do
              Begin
                Read( hr, SizeOf(hr) );  // Hour
                Read( s,  SizeOf(s) );   // Seconds past the hour
                Read( sngBuffer^[1], AllocSize);  // read rest of record
              End;
              myDBLArray[k] := hr + s / 3600.0;
              inc(k);
            End;
            Reallocmem(SngBuffer, 0);  // Dispose of buffer
          End
          Else
          Begin   // Not time solution, so return nil array
            pMon.MonitorStream.Seek(0, soFromEnd) ; // leave stream at end
          End;
        End
      End;
      myPointer :=  @(myDBLArray[0]);
      mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    end;
  4:begin  // Monitors.dblFreq
      myType  :=  2;        // Double
      setlength(myDBLArray, 1);
      myDBLArray[0] := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
      Begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        If pMon.SampleCount >0 Then
        Begin
          setlength(myDBLArray, pMon.SampleCount);
          ReadMonitorHeader(Header, FALSE);   // leave at beginning of data
          TempStr                            :=  '';
          for i := 0 to High(pMon.StrBuffer) do       // Moves the content to a string var
          TempStr   :=  TempStr + string(pMon.StrBuffer[i]); // For some reason needed for DLL

          AuxParser[ActiveActor].CmdString := string(TempStr);
          AuxParser[ActiveActor].AutoIncrement := TRUE;
          FirstCol := AuxParser[ActiveActor].StrValue;  // Get rid of first two columns
          AuxParser[ActiveActor].AutoIncrement := FALSE;
          // check first col to see if it is "Freq" for harmonics solution
          If CompareText(FirstCol, 'freq') = 0  Then
          Begin
            AllocSize :=  Sizeof(SngBuffer^[1]) * Header.RecordSize;
            SngBuffer := Allocmem(AllocSize);
            k := 0;
            for i := 1 to pMon.SampleCount  do Begin
              With pMon.MonitorStream Do
              Begin
                  Read( freq, SizeOf(freq) );  // frequency
                  Read( s,  SizeOf(s) );   // harmonic
                  Read( sngBuffer^[1], AllocSize);  // read rest of record
              End;
              myDBLArray[k] := freq;
              inc(k);
            End;
            Reallocmem(SngBuffer, 0);  // Dispose of buffer
          End
          Else
          Begin   // Not harmonic solution, so return nil array
            pMon.MonitorStream.Seek(0, soFromEnd) ; // leave stream at end
          End;
        End
      End;
      myPointer :=  @(myDBLArray[0]);
      mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    end;
  5:begin    // Monitors.Channel
      myType  :=  2;        // Double
      setlength(myDBLArray, 1);
      myDBLArray[0] := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
      Begin
        pMon := ActiveCircuit[ActiveActor].Monitors.Active;
        If pMon.SampleCount >0 Then
        Begin
          PInt :=  myPointer;
          index:=integer(PInt^);
          setlength(myDBLArray, pMon.SampleCount);
          ReadMonitorHeader(Header, FALSE);   // FALSE = leave at beginning of data
          TempStr                            :=  '';
          for i := 0 to High(pMon.StrBuffer) do       // Moves the content to a string var
          TempStr   :=  TempStr + string(pMon.StrBuffer[i]); // For some reason needed for DLL

          AuxParser[ActiveActor].CmdString := string(TempStr);
          AuxParser[ActiveActor].AutoIncrement := TRUE;
          FirstCol := AuxParser[ActiveActor].StrValue;  // Get rid of first two columns
          AuxParser[ActiveActor].AutoIncrement := FALSE;
          AllocSize :=  Sizeof(SngBuffer^[1]) * Header.RecordSize;
          SngBuffer := Allocmem(AllocSize);
          k := 0;
          for i := 1 to pMon.SampleCount  do Begin
               With pMon.MonitorStream Do
                Begin
                    Read( hr, SizeOf(hr) );
                    Read( s,  SizeOf(s) );
                    Read( sngBuffer^[1], AllocSize);  // read rest of record
                End;
                myDBLArray[k] := sngBuffer^[index];
                inc(k);
          End;
          Reallocmem(SngBuffer, 0);  // Dispose of buffer
        End
      End;
      myPointer :=  @(myDBLArray[0]);
      mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    end
  else
    Begin
      myType  :=  4;        // String
      setlength(myStrArray, 0);
      WriteStr2Array('Error, parameter not recognized');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    End;
  end;
end;

end.
