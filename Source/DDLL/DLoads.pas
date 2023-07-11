unit DLoads;

interface

function DSSLoads(mode:longint; arg:longint):longint; cdecl;
function DSSLoadsF(mode:longint; arg:double):double; cdecl;
function DSSLoadsS(mode:longint; arg:pAnsiChar):pAnsiChar; cdecl;
procedure DSSLoadsV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses DSSGlobals, DSSClassDefs, Executive, Load, Variants, SysUtils, math, CktElement;

Function IsLoad(Const CktElem:TDSSCktElement): Boolean;
  Begin
    Result := ((CktElem.DssObjtype AND CLASSMASK) = LOAD_ELEMENT);
    If Not Result THEN
      DoSimpleMsg('Load Type Expected, but another found. Dss Class=' + CktElem.DSSClassName + CRLF +
      'Element name='+ CktElem.Name, 5007) ;
      // TODO what is the correct error number?
  END;

// Mode defines the property of the Loads Class
// Arg defines the argument and complementary data in case the property will be edited

function ActiveLoad: TLoadObj;
begin
  Result := nil;
  IF ActiveCircuit[ActiveActor] <> NIL THEN
    If IsLoad(ActiveCircuit[ActiveActor].ActiveCktElement) THEN
      begin
        Result := TLoadObj(ActiveCircuit[ActiveActor].ActiveCktElement)
      end;
End;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
  pLoad:TLoadObj;
begin
  if Assigned (ActiveCircuit[ActiveActor]) then
    begin
      pload := ActiveLoad;
      if pload <> nil then
        begin
          SolutionAbort := FALSE;  // Reset for commands entered from outside
          cmd := Format ('load.%s.%s=%s', [pload.Name, parm, val]);
          DSSExecutive[ActiveActor].Command := cmd;
        end;
    end;
end;
//*********************Properties int Type***********************************
function DSSLoads(mode:longint; arg: longint):longint; cdecl;
Var
   pLoad:TLoadObj;
begin
    Result := 0; // Default return value
    case mode of
    0: begin                                   // Loads.First   Read
       Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
       Begin
           pLoad := ActiveCircuit[ActiveActor].Loads.First;
           If pLoad <> Nil Then
           Begin
             Repeat
               If pLoad.Enabled
                Then Begin
                 ActiveCircuit[ActiveActor].ActiveCktElement := pLoad;
                 Result := 1;
               End
               Else pLoad := ActiveCircuit[ActiveActor].Loads.Next;
             Until (Result = 1) or (pLoad = nil);
           End
           Else
               Result := 0;  // signify no more
        End;
      end;
    1:begin                                    //Loads.Next  Read
         Result := 0;
         If ActiveCircuit[ActiveActor] <> Nil Then
           Begin
              pLoad := ActiveCircuit[ActiveActor].Loads.Next;
              If pLoad <> Nil Then
              Begin
                Repeat
                  If pLoad.Enabled
                  Then Begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pLoad;
                    Result := ActiveCircuit[ActiveActor].Loads.ActiveIndex;
                  End
                  Else pLoad := ActiveCircuit[ActiveActor].Loads.Next;
                Until (Result > 0) or (pLoad = nil);
              End
              Else
                  Result := 0;  // signify no more
           End;
      end;
    2: begin                                   //Loads.Idx  Read
         if ActiveCircuit[ActiveActor] <> Nil then
            Result := ActiveCircuit[ActiveActor].Loads.ActiveIndex
         else Result := 0;
       end;
    3: begin                                   //Loads.Idx  Write
         if ActiveCircuit[ActiveActor] <> Nil then   Begin
            pLoad := ActiveCircuit[ActiveActor].Loads.Get(arg);
            If pLoad <> Nil Then ActiveCircuit[ActiveActor].ActiveCktElement := pLoad;
          End;
          Result:=0;
       end ;
    4:begin                                    //Loads.Count
         If Assigned(ActiveCircuit[ActiveActor]) Then
            Result := ActiveCircuit[ActiveActor].Loads.ListSize ;
      end;
    5: begin                                   // Loads.Class  Read
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.LoadClass;
       end;
    6: begin                                   // Loads.Class  Write
         Set_Parameter ('Class', IntToStr (arg));
       end;
    7: begin                                   // Loads.Model  Read
        pload := ActiveLoad;
        if pload <> nil then
          Result:= pload.FLoadModel;
       end;
    8: begin                                   // Loads.Model  Write
        pload := ActiveLoad;
        if pload <> nil then
           pload.FLoadModel := arg; // enums match the integer codes
       end;
    9: begin                                   // Loads.NumCust  Read
        pload := ActiveLoad;
        if pload <> nil then
           Result := pload.NumCustomers;
       end;
   10: begin                                   // Loads.NumCust  Write
           Set_Parameter ('NumCust', IntToStr (arg));
       end;
   11: begin                                   // Loads.Status  Read
           Result := 0;
           pLoad := ActiveLoad;
           if pLoad.ExemptLoad then
             Result := 2
           else if pLoad.FixedLoad then
             Result := 1;
       end;
   12: begin                                   // Loads.Status  Write
          case arg of
              0: Set_Parameter('status', 'v');
              1: Set_Parameter('status', 'f');
              2: Set_Parameter('status', 'e');
          end;
       end;
   13: begin                                   // Loads.IsDelta  read
        pload := ActiveLoad;
        if pload <> nil then
           if pload.Connection > 0 then Result := 1;
       end;
   14: begin                                   // Loads.IsDelta  Write
        pload := ActiveLoad;
        if pload <> nil then
           pload.Connection := Integer (arg);
       end
   else
      Result:=-1;               //The case is not identified or do not exists
   end;
end;


//*********************Properties Float Type***********************************
function DSSLoadsF(mode:longint; arg:double):double; cdecl;
Var
   pLoad: TLoadObj;
begin
  Result:=0.0; // Default return value
  case mode of
    0: begin                                   // Loads.kW  read
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.kWBase;
       end;
    1: begin                                     // Loads.kW  Write
        pload := ActiveLoad;
        if pload <> nil then
          Begin
            pload.kWBase := arg;
            pload.LoadSpecType := 0;
            pload.RecalcElementData(ActiveActor); // sets kvar based on kW and pF
          end;
       end;
    2: begin                                   // Loads.kV  read
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.kVLoadBase;
       end;
    3: begin
        pload := ActiveLoad;
        if pload <> nil then
          Begin                                // Loads.kV  Write
            pload.kVLoadBase := arg;
            pload.UpdateVoltageBases;  // side effects
          end;
       end;
     4: begin                                   // Loads.kvar  read
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.kvarBase;
       end;
    5: begin                                   // Loads.kvar  Write
        pload := ActiveLoad;
        if pload <> nil then
          Begin
            pload.kvarBase := arg;
            pload.LoadSpecType := 1;
            pload.RecalcElementData(ActiveActor) ;  // set power factor based on kW, kvar
          end;
       end;
    6: begin                                   // Loads.PF  read
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.PFNominal;
       end;
    7: begin                                   // Loads.PF  Write
        pload := ActiveLoad;
        if pload <> nil then
          Begin
            pload.PFNominal := arg;
            pload.LoadSpecType := 0;
            pload.RecalcElementData(ActiveActor) ;  //  sets kvar based on kW and pF
          end;
       end;
    8: begin                                   // Loads.PctMean  read
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.puMean * 100.0;
       end;
    9: begin                                   // Loads.PctMean  Write
          Set_Parameter ('%mean', FloatToStr (arg));
       end;
   10: begin                                   // Loads.PctStdDev  read
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.puStdDev * 100.0;
       end;
   11: begin
          Set_Parameter ('%stddev', FloatToStr (arg));
       end;
   12: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.AllocationFactor;
       end;
   13: begin
          Set_Parameter ('AllocationFactor', FloatToStr (arg));
       end;
   14: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.CFactor;
       end;
   15: begin
          Set_Parameter ('Cfactor', FloatToStr (arg));
       end;
   16: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.CVRwatts;
       end;
   17: begin
          Set_Parameter ('CVRwatts', FloatToStr (arg));
       end;
   18: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.CVRvars;
       end;
   19: begin
          Set_Parameter ('CVRvars', FloatToStr (arg));
       end;
   20: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.kVABase;
       end;
   21: begin
          Set_Parameter ('kva', FloatToStr (arg));
       end;
   22: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.kWh;
       end;
   23: begin
          Set_Parameter ('kwh', FloatToStr (arg));
       end;
   24: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.kWhDays;
       end;
   25: begin
          Set_Parameter ('kwhdays', FloatToStr (arg));
       end;
   26: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.Rneut;
       end;
   27: begin
          Set_Parameter ('Rneut', FloatToStr (arg));
       end;
   28: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.MaxPU;
       end;
   29: begin
          Set_Parameter ('VmaxPu', FloatToStr (arg));
       end;
   30: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.MinEmerg;
       end;
   31: begin
          Set_Parameter ('VminEmerg', FloatToStr (arg));
       end;
   32: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.MinNormal;
       end;
   33: begin
          Set_Parameter ('VminNorm', FloatToStr (arg));
       end;
   34: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.MinPU;
       end;
   35: begin
          Set_Parameter ('VminPu', FloatToStr (arg));
       end;
   36: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.ConnectedkVA;
       end;
   37: begin
          Set_Parameter ('XfKVA', FloatToStr (arg));
       end;
   38: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.Xneut;
       end;
   39: begin
          Set_Parameter ('Xneut', FloatToStr (arg));
       end;
   40: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.puSeriesRL * 100.0;
       end;
   41: begin
        pload := ActiveLoad;
        if pload <> nil then
           pload.puSeriesRL  := arg / 100.0;
       end;
   42: begin
        pload := ActiveLoad;
        if pload <> nil then
          Result := pload.RelWeighting;
       end;
   43: begin
        pload := ActiveLoad;
        if pload <> nil then
          pload.RelWeighting := arg;
       end
  else
      Result:=-1;               //The case is not identified or do not exists
  end;
end;

//*********************Properties String Type***********************************
function DSSLoadsS(mode:longint; arg:pAnsiChar):pAnsiChar; cdecl;
Var
   pLoad:TDSSCktElement;
   Load:TLoadObj;
   ActiveSave :integer;
   S: String;
   Found :Boolean;
begin
  Result := pAnsiChar(AnsiString('')); // Default return value
  case mode of
  0: begin                                     // Loads.Name - Read
       Result := pAnsiChar(AnsiString(''));
       load := ActiveLoad;
       if load <> nil then
          Result := pAnsiChar(AnsiString(Load.Name))
  end;
  1: begin                                     // Loads.Name - Write
      IF ActiveCircuit[ActiveActor] <> NIL
      THEN Begin      // Search list of Loads in active circuit for name
         WITH ActiveCircuit[ActiveActor].Loads DO
         Begin
               S := WideString(arg);  // Convert to Pascal String
               Found := FALSE;
               ActiveSave := ActiveIndex;
               pLoad := First;
               While pLoad <> NIL Do
               Begin
                  IF (CompareText(pLoad.Name, S) = 0)
                  THEN Begin
                      ActiveCircuit[ActiveActor].ActiveCktElement := pLoad;
                      Found := TRUE;
                      Break;
                  End;
                  pLoad := Next;
               End;
               IF NOT Found
               THEN Begin
                   DoSimpleMsg('Load "' + S + '" Not Found in Active Circuit.', 5003);
                   pLoad := Get(ActiveSave);    // Restore active Load
                   ActiveCircuit[ActiveActor].ActiveCktElement := pLoad;
               End;
          End;
      End;
      Result:=pAnsiChar(AnsiString(''));
  end;
  2: begin                                     // Loads.CVRCurve - Read
      Result := pAnsiChar(AnsiString(''));
      Result := pAnsiChar(AnsiString(ActiveLoad.CVRshape));
  end;
  3: begin                                     // Loads.CVRCurve - Write
      Set_Parameter ('CVRcurve', widestring(arg));
      Result:=pAnsiChar(AnsiString(''));
  end;
  4: begin                                     // Loads.Daily - Read
      Result := pAnsiChar(AnsiString(''));
      Result := pAnsiChar(AnsiString(ActiveLoad.DailyShape));
  end;
  5: begin                                     // Loads.Daily - Write
      Set_Parameter ('Daily', widestring(arg));
      Result:=pAnsiChar(AnsiString(''));
  end;
  6: begin                                     // Loads.Duty - read
      Result := pAnsiChar(AnsiString(''));
      Result := pAnsiChar(AnsiString(ActiveLoad.DailyShape));
  end;
  7: begin                                     // Loads.Duty - Write
      Set_Parameter ('Duty', widestring(arg));
      Result:=pAnsiChar(AnsiString(''));
  end;
  8: begin                                     // Loads.Spectrum - Read
      Result := pAnsiChar(AnsiString(''));
      Result := pAnsiChar(AnsiString(ActiveLoad.Spectrum));
  end;
  9: begin                                     // Loads.Spectrum - Write
      Set_Parameter ('Spectrum', widestring(arg));
      Result:=pAnsiChar(AnsiString(''));
  end;
  10: begin                                    // Loads.Yearly - Read
      Result := pAnsiChar(AnsiString(''));
      Result := pAnsiChar(AnsiString(ActiveLoad.YearlyShape));
  end;
  11: begin                                    // Loads.Yearly - Write
      Set_Parameter ('Yearly', widestring(arg));
      Result:=pAnsiChar(AnsiString(''));
  end;
  12: begin                                    // Loads.Growth - read
      Result := pAnsiChar(AnsiString(''));
      Result := pAnsiChar(AnsiString(ActiveLoad.GrowthShape));
  end;
  13: begin                                    // Loads.Growth - Write
      Set_Parameter ('Growth', Widestring(arg));
      Result:=pAnsiChar(AnsiString(''));
  end;
  14: begin                                    // Loads.Sensor - read
      Result := pAnsiChar(AnsiString(''));
      if ActiveLoad.SensorObj <> nil then
          Result := pAnsiChar(AnsiString(ActiveLoad.SensorObj.ElementName));
  end
  else
      Result:=pAnsiChar(AnsiString('Error'));
  end
end;

//*********************Properties Variant Type***********************************
procedure DSSLoadsV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;
Var
   pLoad      :TLoadObj;
   k,
   i,
   Looplimit  :Integer;
   PDouble    : ^Double;
begin
  case mode of
  0:  begin                                   // Loads.Allnames
        myType  :=  4;        // String
        setlength(myStrArray,0);
        IF ActiveCircuit[ActiveActor] <> Nil THEN
        Begin
          WITH ActiveCircuit[ActiveActor] DO
          If Loads.ListSize > 0 Then
          Begin
            pLoad := Loads.First;
            WHILE pLoad<>Nil DO
            Begin
              WriteStr2Array(pLoad.Name);
              WriteStr2Array(Char(0));
              pLoad := Loads.Next;
            End;
          End;
        End
        Else  WriteStr2Array('');
        myPointer :=  @(myStrArray[0]);
        mySize    :=  Length(myStrArray);
      end;
  1:  begin                                   // Loads.ZIPV - read
        myType  :=  2;        // Double
        setlength(myDBLArray, 1);
        pLoad := ActiveLoad;
        IF pLoad <> Nil THEN
        Begin
          setlength(myDBLArray, pLoad.nZIPV);
          For k:=0 to ( pLoad.nZIPV - 1 ) Do
              myDBLArray[k] := pLoad.ZipV^[ k + 1 ];
        End
        else myDBLArray[0] := 0;
        myPointer :=  @(myDBLArray[0]);
        mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
      end;
  2:  begin
        pLoad := ActiveLoad;
        k := 1;
        If pLoad <> nil Then
        Begin
          // allocate space for 7
          pLoad.nZIPV := 7;
          // only put as many elements as proviced up to nZIPV
          If (mySize) > 7 Then
            LoopLimit :=  6
          else
            LoopLimit := mySize - 1;
          for i := 0 to LoopLimit do
          Begin
            PDouble :=  myPointer;
            pLoad.ZIPV^[k] := PDouble^;
            inc(k);
            inc(PByte(myPointer),8);
          End;
        End;
        mySize  :=  k - 1;
      end
  else
    begin
      myType  :=  4;        // String
      setlength(myStrArray, 0);
      WriteStr2Array('Error, parameter not recognized');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    end;
  end;
end;

end.
