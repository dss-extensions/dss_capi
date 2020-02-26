unit DLoads;

interface

function DSSLoads(mode: Longint; arg: Longint): Longint; CDECL;
function DSSLoadsF(mode: Longint; arg: Double): Double; CDECL;
function DSSLoadsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure DSSLoadsV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    DSSGlobals,
    Executive,
    Load,
    Variants,
    SysUtils;

// Mode defines the property of the Loads Class
// Arg defines the argument and complementary data in case the property will be edited

function ActiveLoad: TLoadObj;
begin
    Result := NIL;
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Loads.Active;
end;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('load.%s.%s=%s', [ActiveLoad.Name, parm, val]);
    DSSExecutive.Command := cmd;
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
            if ActiveCircuit <> NIL then
            begin
                pLoad := ActiveCircuit.Loads.First;
                if pLoad <> NIL then
                begin
                    repeat
                        if pLoad.Enabled then
                        begin
                            ActiveCircuit.ActiveCktElement := pLoad;
                            Result := 1;
                        end
                        else
                            pLoad := ActiveCircuit.Loads.Next;
                    until (Result = 1) or (pLoad = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        1:
        begin                                    //Loads.Next  Read
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                pLoad := ActiveCircuit.Loads.Next;
                if pLoad <> NIL then
                begin
                    repeat
                        if pLoad.Enabled then
                        begin
                            ActiveCircuit.ActiveCktElement := pLoad;
                            Result := ActiveCircuit.Loads.ActiveIndex;
                        end
                        else
                            pLoad := ActiveCircuit.Loads.Next;
                    until (Result > 0) or (pLoad = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        2:
        begin                                   //Loads.Idx  Read
            if ActiveCircuit <> NIL then
                Result := ActiveCircuit.Loads.ActiveIndex
            else
                Result := 0;
        end;
        3:
        begin                                   //Loads.Idx  Write
            if ActiveCircuit <> NIL then
            begin
                pLoad := ActiveCircuit.Loads.Get(arg);
                if pLoad <> NIL then
                    ActiveCircuit.ActiveCktElement := pLoad;
            end;
            Result := 0;
        end;
        4:
        begin                                    //Loads.Count
            if Assigned(ActiveCircuit) then
                Result := ActiveCircuit.Loads.ListSize;
        end;
        5:
        begin                                   // Loads.Class  Read
            Result := 0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.LoadClass;
        end;
        6:
        begin                                   // Loads.Class  Write
            Set_Parameter('Class', IntToStr(arg));
            Result := 0;
        end;
        7:
        begin                                   // Loads.Model  Read
            Result := 1;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.FLoadModel;
        end;
        8:
        begin                                   // Loads.Model  Write
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                pLoad.FLoadModel := arg; // enums match the integer codes
            Result := 0;
        end;
        9:
        begin                                   // Loads.NumCust  Read
            Result := 0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.NumCustomers;
        end;
        10:
        begin                                   // Loads.NumCust  Write
            Set_Parameter('NumCust', IntToStr(arg));
        end;
        11:
        begin                                   // Loads.Status  Read
            Result := 0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
            begin
                if pLoad.ExemptLoad then
                    Result := 2
                else
                if pLoad.FixedLoad then
                    Result := 1;
            end;
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
            Result := 0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                if pLoad.Connection > 0 then
                    Result := 1;
        end;
        14:
        begin                                   // Loads.IsDelta  Write
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                pLoad.Connection := Integer(arg);
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
            Result := 0.0;
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.Loads do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TLoadObj(Active).kWBase;
                    end;
                end;
            end;
        end;
        1:
        begin                                   // Loads.kW  Write
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.Loads do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        TLoadObj(Active).kWBase := arg;
                        TLoadObj(Active).LoadSpecType := 0;
                        TLoadObj(Active).RecalcElementData; // sets kvar based on kW and pF
                    end;
                end;
            end;
            Result := 0;
        end;
        2:
        begin                                   // Loads.kV  read
            Result := 0.0;
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.Loads do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TLoadObj(Active).kVLoadBase;
                    end;
                end;
            end;
        end;
        3:
        begin                                   // Loads.kV  Write
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.Loads do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        TLoadObj(Active).kVLoadBase := arg;
                        TLoadObj(Active).UpdateVoltageBases;  // side effects
                    end;
                end;
            end;
            Result := 0;
        end;
        4:
        begin                                   // Loads.kvar  read
            Result := 0.0;
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.Loads do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TLoadObj(Active).kvarBase;
                    end;
                end;
            end;
        end;
        5:
        begin                                   // Loads.kvar  Write
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.Loads do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        TLoadObj(Active).kvarBase := arg;
                        TLoadObj(Active).LoadSpecType := 1;
                        TLoadObj(Active).RecalcElementData;  // set power factor based on kW, kvar
                    end;
                end;
            end;
            Result := 0;
        end;
        6:
        begin                                   // Loads.PF  read
            Result := 0.0;
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.Loads do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        Result := TLoadObj(Active).PFNominal;
                    end;
                end;
            end;
        end;
        7:
        begin                                   // Loads.PF  Write
            if ActiveCircuit <> NIL then
            begin
                with ActiveCircuit.Loads do
                begin
                    if ActiveIndex <> 0 then
                    begin
                        TLoadObj(Active).PFNominal := arg;
                        TLoadObj(Active).LoadSpecType := 0;
                        TLoadObj(Active).RecalcElementData;  //  sets kvar based on kW and pF
                    end;
                end;
            end;
            Result := 0;
        end;
        8:
        begin                                   // Loads.PctMean  read
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.puMean * 100.0;
        end;
        9:
        begin                                   // Loads.PctMean  Write
            Set_Parameter('%mean', FloatToStr(arg));
            Result := 0;
        end;
        10:
        begin                                   // Loads.PctStdDev  read
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.puStdDev * 100.0;
        end;
        11:
        begin
            Set_Parameter('%stddev', FloatToStr(arg));
            Result := 0;
        end;
        12:
        begin
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.AllocationFactor;
        end;
        13:
        begin
            Set_Parameter('AllocationFactor', FloatToStr(arg));
            Result := 0;
        end;
        14:
        begin
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.CFactor;
        end;
        15:
        begin
            Set_Parameter('Cfactor', FloatToStr(arg));
            Result := 0;
        end;
        16:
        begin
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.CVRwatts;
        end;
        17:
        begin
            Set_Parameter('CVRwatts', FloatToStr(arg));
            Result := 0;
        end;
        18:
        begin
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.CVRvars;
        end;
        19:
        begin
            Set_Parameter('CVRvars', FloatToStr(arg));
            Result := 0;
        end;
        20:
        begin
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.kVABase;
        end;
        21:
        begin
            Set_Parameter('kva', FloatToStr(arg));
            Result := 0;
        end;
        22:
        begin
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.kWh;
        end;
        23:
        begin
            Set_Parameter('kwh', FloatToStr(arg));
            Result := 0;
        end;
        24:
        begin
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.kWhDays;
        end;
        25:
        begin
            Set_Parameter('kwhdays', FloatToStr(arg));
            Result := 0;
        end;
        26:
        begin
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.Rneut;
        end;
        27:
        begin
            Set_Parameter('Rneut', FloatToStr(arg));
            Result := 0;
        end;
        28:
        begin
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.MaxPU;
        end;
        29:
        begin
            Set_Parameter('VmaxPu', FloatToStr(arg));
            Result := 0;
        end;
        30:
        begin
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.MinEmerg;
        end;
        31:
        begin
            Set_Parameter('VminEmerg', FloatToStr(arg));
            Result := 0;
        end;
        32:
        begin
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.MinNormal;
        end;
        33:
        begin
            Set_Parameter('VminNorm', FloatToStr(arg));
            Result := 0;
        end;
        34:
        begin
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.MinPU;
        end;
        35:
        begin
            Set_Parameter('VminPu', FloatToStr(arg));
        end;
        36:
        begin
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.ConnectedkVA;
        end;
        37:
        begin
            Set_Parameter('XfKVA', FloatToStr(arg));
        end;
        38:
        begin
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.Xneut;
        end;
        39:
        begin
            Set_Parameter('Xneut', FloatToStr(arg));
        end;
        40:
        begin
            Result := -1.0; // signify  bad request
            pLoad := ActiveLoad;
            if pLoad <> NIL then
            begin
                Result := pLoad.puSeriesRL * 100.0;
            end;
        end;
        41:
        begin
            pLoad := ActiveLoad;
            if pLoad <> NIL then
            begin
                pLoad.puSeriesRL := arg / 100.0;
            end;
        end;
        42:
        begin
            Result := 0.0;
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pLoad.RelWeighting;
        end;
        43:
        begin
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                pLoad.RelWeighting := arg;
        end
    else
        Result := -1;               //The case is not identified or do not exists
    end;
end;

//*********************Properties String Type***********************************
function DSSLoadsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
var
    pLoad: TLoadObj;
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
begin
    Result := pAnsiChar(Ansistring('')); // Default return value
    case mode of
        0:
        begin                                     // Loads.Name - Read
            Result := pAnsiChar(Ansistring(''));
            if ActiveCircuit <> NIL then
            begin
                pLoad := ActiveCircuit.Loads.Active;
                if pLoad <> NIL then
                    Result := pAnsiChar(Ansistring(pLoad.Name))
                else
                    Result := pAnsiChar(Ansistring(''));  // signify no name
            end;
        end;
        1:
        begin                                     // Loads.Name - Write
            if ActiveCircuit <> NIL then
            begin      // Search list of Loads in active circuit for name
                with ActiveCircuit.Loads do
                begin
                    S := String(arg);  // Convert to Pascal String
                    Found := FALSE;
                    ActiveSave := ActiveIndex;
                    pLoad := First;
                    while pLoad <> NIL do
                    begin
                        if (CompareText(pLoad.Name, S) = 0) then
                        begin
                            ActiveCircuit.ActiveCktElement := pLoad;
                            Found := TRUE;
                            Break;
                        end;
                        pLoad := Next;
                    end;
                    if not Found then
                    begin
                        DoSimpleMsg('Load "' + S + '" Not Found in Active Circuit.', 5003);
                        pLoad := Get(ActiveSave);    // Restore active Load
                        ActiveCircuit.ActiveCktElement := pLoad;
                    end;
                end;
            end;
            Result := pAnsiChar(Ansistring(''));
        end;
        2:
        begin                                     // Loads.CVRCurve - Read
            Result := pAnsiChar(Ansistring(''));
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pAnsiChar(Ansistring(pLoad.CVRshape));
        end;
        3:
        begin                                     // Loads.CVRCurve - Write
            Set_Parameter('CVRcurve', String(arg));
            Result := pAnsiChar(Ansistring(''));
        end;
        4:
        begin                                     // Loads.Daily - Read
            Result := pAnsiChar(Ansistring(''));
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pAnsiChar(Ansistring(pLoad.DailyShape));
        end;
        5:
        begin                                     // Loads.Daily - Write
            Set_Parameter('Daily', String(arg));
            Result := pAnsiChar(Ansistring(''));
        end;
        6:
        begin                                     // Loads.Duty - read
            Result := pAnsiChar(Ansistring(''));
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pAnsiChar(Ansistring(pLoad.DailyShape));
        end;
        7:
        begin                                     // Loads.Duty - Write
            Set_Parameter('Duty', String(arg));
            Result := pAnsiChar(Ansistring(''));
        end;
        8:
        begin                                     // Loads.Spectrum - Read
            Result := pAnsiChar(Ansistring(''));
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pAnsiChar(Ansistring(pLoad.Spectrum));
        end;
        9:
        begin                                     // Loads.Spectrum - Write
            Set_Parameter('Spectrum', String(arg));
            Result := pAnsiChar(Ansistring(''));
        end;
        10:
        begin                                    // Loads.Yearly - Read
            Result := pAnsiChar(Ansistring(''));
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pAnsiChar(Ansistring(pLoad.YearlyShape));
        end;
        11:
        begin                                    // Loads.Yearly - Write
            Set_Parameter('Yearly', String(arg));
            Result := pAnsiChar(Ansistring(''));
        end;
        12:
        begin                                    // Loads.Growth - read
            Result := pAnsiChar(Ansistring(''));
            pLoad := ActiveLoad;
            if pLoad <> NIL then
                Result := pAnsiChar(Ansistring(pLoad.GrowthShape));
        end;
        13:
        begin                                    // Loads.Growth - Write
            Set_Parameter('Growth', String(arg));
            Result := pAnsiChar(Ansistring(''));
        end
    else
        Result := pAnsiChar(Ansistring('Error'));
    end
end;

//*********************Properties Variant Type***********************************
procedure DSSLoadsV(mode: Longint; out arg: Variant); CDECL;
var
    pLoad: TLoadObj;
    k, i, Looplimit: Integer;
begin
    case mode of
        0:
        begin                                   // Loads.Allnames
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                    if Loads.ListSize > 0 then
                    begin
                        VarArrayRedim(arg, Loads.ListSize - 1);
                        k := 0;
                        pLoad := Loads.First;
                        while pLoad <> NIL do
                        begin
                            arg[k] := pLoad.Name;
                            Inc(k);
                            pLoad := Loads.Next;
                        end;
                    end;
        end;
        1:
        begin                                   // Loads.ZIPV - read
            arg := VarArrayCreate([0, 0], varDouble);
            arg[0] := 0.0;  // error condition: one element array=0
            pLoad := ActiveLoad;
            if pLoad <> NIL then
            begin
                VarArrayRedim(arg, pLoad.nZIPV - 1);
                for k := 0 to pLoad.nZIPV - 1 do
                    arg[k] := pLoad.ZipV^[k + 1];
            end;
        end;
        2:
        begin
            pLoad := ActiveLoad;
            if pLoad <> NIL then
            begin
             // allocate space for 7
                pLoad.nZIPV := 7;
             // only put as many elements as proviced up to nZIPV
                LoopLimit := VarArrayHighBound(arg, 1);
                if (LoopLimit - VarArrayLowBound(arg, 1) + 1) > 7 then
                    LoopLimit := VarArrayLowBound(arg, 1) + 6;

                k := 1;
                for i := VarArrayLowBound(arg, 1) to LoopLimit do
                begin
                    pLoad.ZIPV^[k] := arg[i];
                    inc(k);
                end;
            end;
        end
    else
        arg[0] := 'NONE';
    end;
end;

end.
