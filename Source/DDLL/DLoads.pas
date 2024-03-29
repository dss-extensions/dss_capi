unit DLoads;

interface

function DSSLoads(mode: Longint; arg: Longint): Longint; CDECL;
function DSSLoadsF(mode: Longint; arg: Double): Double; CDECL;
function DSSLoadsS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure DSSLoadsV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    DSSGlobals,
    DSSClassDefs,
    Executive,
    Load,
    Variants,
    SysUtils,
    math,
    CktElement;

function IsLoad(const CktElem: TDSSCktElement): Boolean;
begin
    Result := ((CktElem.DssObjtype and CLASSMASK) = LOAD_ELEMENT);
    if not Result then
        DoSimpleMsg('Load Type Expected, but another found. Dss Class=' + CktElem.DSSClassName + CRLF +
            'Element name=' + CktElem.Name, 5007);
      // TODO what is the correct error number?
end;

// Mode defines the property of the Loads Class
// Arg defines the argument and complementary data in case the property will be edited

function ActiveLoad: TLoadObj;
begin
    Result := NIL;
    if ActiveCircuit[ActiveActor] <> NIL then
        if IsLoad(ActiveCircuit[ActiveActor].ActiveCktElement) then
        begin
            Result := TLoadObj(ActiveCircuit[ActiveActor].ActiveCktElement)
        end;
end;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
    pLoad: TLoadObj;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
    begin
        pload := ActiveLoad;
        if pload <> NIL then
        begin
            SolutionAbort := FALSE;  // Reset for commands entered from outside
            cmd := Format('load.%s.%s=%s', [pload.Name, parm, val]);
            DSSExecutive[ActiveActor].Command := cmd;
        end;
    end;
end;
//*********************Properties int Type***********************************
function DSSLoads(mode: Longint; arg: Longint): Longint; CDECL;
var
    pLoad: TLoadObj;
begin
    Result := 0; // Default return value
    case mode of
        0:
        begin                                   // Loads.First   Read
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLoad := ActiveCircuit[ActiveActor].Loads.First;
                if pLoad <> NIL then
                begin
                    repeat
                        if pLoad.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pLoad;
                            Result := 1;
                        end
                        else
                            pLoad := ActiveCircuit[ActiveActor].Loads.Next;
                    until (Result = 1) or (pLoad = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        1:
        begin                                    //Loads.Next  Read
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLoad := ActiveCircuit[ActiveActor].Loads.Next;
                if pLoad <> NIL then
                begin
                    repeat
                        if pLoad.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pLoad;
                            Result := ActiveCircuit[ActiveActor].Loads.ActiveIndex;
                        end
                        else
                            pLoad := ActiveCircuit[ActiveActor].Loads.Next;
                    until (Result > 0) or (pLoad = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        2:
        begin                                   //Loads.Idx  Read
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := ActiveCircuit[ActiveActor].Loads.ActiveIndex
            else
                Result := 0;
        end;
        3:
        begin                                   //Loads.Idx  Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLoad := ActiveCircuit[ActiveActor].Loads.Get(arg);
                if pLoad <> NIL then
                    ActiveCircuit[ActiveActor].ActiveCktElement := pLoad;
            end;
            Result := 0;
        end;
        4:
        begin                                    //Loads.Count
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := ActiveCircuit[ActiveActor].Loads.ListSize;
        end;
        5:
        begin                                   // Loads.Class  Read
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.LoadClass;
        end;
        6:
        begin                                   // Loads.Class  Write
            Set_Parameter('Class', IntToStr(arg));
        end;
        7:
        begin                                   // Loads.Model  Read
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.FLoadModel;
        end;
        8:
        begin                                   // Loads.Model  Write
            pload := ActiveLoad;
            if pload <> NIL then
                pload.FLoadModel := arg; // enums match the integer codes
        end;
        9:
        begin                                   // Loads.NumCust  Read
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.NumCustomers;
        end;
        10:
        begin                                   // Loads.NumCust  Write
            Set_Parameter('NumCust', IntToStr(arg));
        end;
        11:
        begin                                   // Loads.Status  Read
            Result := 0;
            pLoad := ActiveLoad;
            if pLoad.ExemptLoad then
                Result := 2
            else
            if pLoad.FixedLoad then
                Result := 1;
        end;
        12:
        begin                                   // Loads.Status  Write
            case arg of
                0:
                    Set_Parameter('status', 'v');
                1:
                    Set_Parameter('status', 'f');
                2:
                    Set_Parameter('status', 'e');
            end;
        end;
        13:
        begin                                   // Loads.IsDelta  read
            pload := ActiveLoad;
            if pload <> NIL then
                if pload.Connection > 0 then
                    Result := 1;
        end;
        14:
        begin                                   // Loads.IsDelta  Write
            pload := ActiveLoad;
            if pload <> NIL then
                pload.Connection := Integer(arg);
        end
    else
        Result := -1;               //The case is not identified or do not exists
    end;
end;


//*********************Properties Float Type***********************************
function DSSLoadsF(mode: Longint; arg: Double): Double; CDECL;
var
    pLoad: TLoadObj;
begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin                                   // Loads.kW  read
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.kWBase;
        end;
        1:
        begin                                     // Loads.kW  Write
            pload := ActiveLoad;
            if pload <> NIL then
            begin
                pload.kWBase := arg;
                pload.LoadSpecType := 0;
                pload.RecalcElementData(ActiveActor); // sets kvar based on kW and pF
            end;
        end;
        2:
        begin                                   // Loads.kV  read
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.kVLoadBase;
        end;
        3:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
            begin                                // Loads.kV  Write
                pload.kVLoadBase := arg;
                pload.UpdateVoltageBases;  // side effects
            end;
        end;
        4:
        begin                                   // Loads.kvar  read
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.kvarBase;
        end;
        5:
        begin                                   // Loads.kvar  Write
            pload := ActiveLoad;
            if pload <> NIL then
            begin
                pload.kvarBase := arg;
                pload.LoadSpecType := 1;
                pload.RecalcElementData(ActiveActor);  // set power factor based on kW, kvar
            end;
        end;
        6:
        begin                                   // Loads.PF  read
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.PFNominal;
        end;
        7:
        begin                                   // Loads.PF  Write
            pload := ActiveLoad;
            if pload <> NIL then
            begin
                pload.PFNominal := arg;
                pload.LoadSpecType := 0;
                pload.RecalcElementData(ActiveActor);  //  sets kvar based on kW and pF
            end;
        end;
        8:
        begin                                   // Loads.PctMean  read
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.puMean * 100.0;
        end;
        9:
        begin                                   // Loads.PctMean  Write
            Set_Parameter('%mean', FloatToStr(arg));
        end;
        10:
        begin                                   // Loads.PctStdDev  read
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.puStdDev * 100.0;
        end;
        11:
        begin
            Set_Parameter('%stddev', FloatToStr(arg));
        end;
        12:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.AllocationFactor;
        end;
        13:
        begin
            Set_Parameter('AllocationFactor', FloatToStr(arg));
        end;
        14:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.CFactor;
        end;
        15:
        begin
            Set_Parameter('Cfactor', FloatToStr(arg));
        end;
        16:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.CVRwatts;
        end;
        17:
        begin
            Set_Parameter('CVRwatts', FloatToStr(arg));
        end;
        18:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.CVRvars;
        end;
        19:
        begin
            Set_Parameter('CVRvars', FloatToStr(arg));
        end;
        20:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.kVABase;
        end;
        21:
        begin
            Set_Parameter('kva', FloatToStr(arg));
        end;
        22:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.kWh;
        end;
        23:
        begin
            Set_Parameter('kwh', FloatToStr(arg));
        end;
        24:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.kWhDays;
        end;
        25:
        begin
            Set_Parameter('kwhdays', FloatToStr(arg));
        end;
        26:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.Rneut;
        end;
        27:
        begin
            Set_Parameter('Rneut', FloatToStr(arg));
        end;
        28:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.MaxPU;
        end;
        29:
        begin
            Set_Parameter('VmaxPu', FloatToStr(arg));
        end;
        30:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.MinEmerg;
        end;
        31:
        begin
            Set_Parameter('VminEmerg', FloatToStr(arg));
        end;
        32:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.MinNormal;
        end;
        33:
        begin
            Set_Parameter('VminNorm', FloatToStr(arg));
        end;
        34:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.MinPU;
        end;
        35:
        begin
            Set_Parameter('VminPu', FloatToStr(arg));
        end;
        36:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.ConnectedkVA;
        end;
        37:
        begin
            Set_Parameter('XfKVA', FloatToStr(arg));
        end;
        38:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.Xneut;
        end;
        39:
        begin
            Set_Parameter('Xneut', FloatToStr(arg));
        end;
        40:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.puSeriesRL * 100.0;
        end;
        41:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                pload.puSeriesRL := arg / 100.0;
        end;
        42:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                Result := pload.RelWeighting;
        end;
        43:
        begin
            pload := ActiveLoad;
            if pload <> NIL then
                pload.RelWeighting := arg;
        end
    else
        Result := -1;               //The case is not identified or do not exists
    end;
end;

//*********************Properties String Type***********************************
function DSSLoadsS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
var
    pLoad: TDSSCktElement;
    Load: TLoadObj;
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
begin
    Result := Pansichar(Ansistring('')); // Default return value
    case mode of
        0:
        begin                                     // Loads.Name - Read
            load := ActiveLoad;
            if load <> NIL then
                Result := Pansichar(Ansistring(Load.Name))
        end;
        1:
        begin                                     // Loads.Name - Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin      // Search list of Loads in active circuit for name
                with ActiveCircuit[ActiveActor].Loads do
                begin
                    S := String(arg);  // Convert to Pascal String
                    Found := FALSE;
                    ActiveSave := ActiveIndex;
                    pLoad := First;
                    while pLoad <> NIL do
                    begin
                        if (CompareText(pLoad.Name, S) = 0) then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pLoad;
                            Found := TRUE;
                            Break;
                        end;
                        pLoad := Next;
                    end;
                    if not Found then
                    begin
                        DoSimpleMsg('Load "' + S + '" Not Found in Active Circuit.', 5003);
                        pLoad := Get(ActiveSave);    // Restore active Load
                        ActiveCircuit[ActiveActor].ActiveCktElement := pLoad;
                    end;
                end;
            end;
        end;
        2:
        begin                                     // Loads.CVRCurve - Read
            Result := Pansichar(Ansistring(ActiveLoad.CVRshape));
        end;
        3:
        begin                                     // Loads.CVRCurve - Write
            Set_Parameter('CVRcurve', String(arg));
        end;
        4:
        begin                                     // Loads.Daily - Read
            Result := Pansichar(Ansistring(ActiveLoad.DailyShape));
        end;
        5:
        begin                                     // Loads.Daily - Write
            Set_Parameter('Daily', String(arg));
        end;
        6:
        begin                                     // Loads.Duty - read
            Result := Pansichar(Ansistring(ActiveLoad.DailyShape));
        end;
        7:
        begin                                     // Loads.Duty - Write
            Set_Parameter('Duty', String(arg));
        end;
        8:
        begin                                     // Loads.Spectrum - Read
            Result := Pansichar(Ansistring(ActiveLoad.Spectrum));
        end;
        9:
        begin                                     // Loads.Spectrum - Write
            Set_Parameter('Spectrum', String(arg));
        end;
        10:
        begin                                    // Loads.Yearly - Read
            Result := Pansichar(Ansistring(ActiveLoad.YearlyShape));
        end;
        11:
        begin                                    // Loads.Yearly - Write
            Set_Parameter('Yearly', String(arg));
        end;
        12:
        begin                                    // Loads.Growth - read
            Result := Pansichar(Ansistring(ActiveLoad.GrowthShape));
        end;
        13:
        begin                                    // Loads.Growth - Write
            Set_Parameter('Growth', String(arg));
        end;
        14:
        begin                                    // Loads.Sensor - read
            if ActiveLoad.SensorObj <> NIL then
                Result := Pansichar(Ansistring(ActiveLoad.SensorObj.ElementName));
        end
    else
        Result := Pansichar(Ansistring('Error'));
    end
end;

//*********************Properties Variant Type***********************************
procedure DSSLoadsV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;
var
    pLoad: TLoadObj;
    k,
    i,
    Looplimit: Integer;
    PDouble: ^Double;
begin
    case mode of
        0:
        begin                                   // Loads.Allnames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                    if Loads.ListSize > 0 then
                    begin
                        pLoad := Loads.First;
                        while pLoad <> NIL do
                        begin
                            WriteStr2Array(pLoad.Name);
                            WriteStr2Array(Char(0));
                            pLoad := Loads.Next;
                        end;
                    end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        1:
        begin                   // Loads.ZIPV - read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
            begin
                if (pLoad.nZIPV >= 1) then
                begin
                    setlength(myDBLArray, pLoad.nZIPV);
                    for k := 0 to (pLoad.nZIPV - 1) do
                        myDBLArray[k] := pLoad.ZipV^[k + 1];
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        2:
        begin                   // Loads.ZIPV - write
            pLoad := ActiveLoad;
            myType := 2;        // Double
            k := 1;
            if pLoad <> NIL then
            begin
          // allocate space for 7
                pLoad.nZIPV := 7;
          // only put as many elements as proviced up to nZIPV
                if (mySize) > 7 then
                    LoopLimit := 6
                else
                    LoopLimit := mySize - 1;
                for i := 0 to LoopLimit do
                begin
                    PDouble := myPointer;
                    pLoad.ZIPV^[k] := PDouble^;
                    inc(k);
                    inc(Pbyte(myPointer), 8);
                end;
            end;
            mySize := k - 1;
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
