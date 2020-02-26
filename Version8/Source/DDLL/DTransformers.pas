unit DTransformers;

interface

function TransformersI(mode: Longint; arg: Longint): Longint; CDECL;
function TransformersF(mode: Longint; arg: Double): Double; CDECL;
function TransformersS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure TransformersV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    DSSGlobals,
    Executive,
    Transformer,
    Variants,
    SysUtils,
    PointerList,
    ucomplex;

function ActiveTransformer: TTransfObj;
begin
    Result := NIL;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Transformers.Active;
end;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('transformer.%s.%s=%s', [ActiveTransformer.Name, parm, val]);
    DSSExecutive[ActiveActor].Command := cmd;
end;

function TransformersI(mode: Longint; arg: Longint): Longint; CDECL;

var
    elem: TTransfObj;
    lst: TPointerList;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin  // Transformers.NumWindings read
            Result := 0;
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := elem.NumberOfWindings;
        end;
        1:
        begin  // Transformers.NumWindings write
            elem := ActiveTransformer;
            if elem <> NIL then
                elem.SetNumWindings(arg);
        end;
        2:
        begin  // Transformers.Wdg read
            Result := 0;
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := elem.ActiveWinding;
        end;
        3:
        begin  // Transformers.Wdg write
            elem := ActiveTransformer;
            if elem <> NIL then
                if (arg > 0) and (arg <= elem.NumberOfWindings) then
                    elem.ActiveWinding := arg;
        end;
        4:
        begin  // Transformers.NumTaps read
            Result := 0;
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := elem.NumTaps[elem.ActiveWinding];
        end;
        5:
        begin  // Transformers.NumTaps write
            Set_Parameter('NumTaps', IntToStr(arg));
        end;
        6:
        begin  // Transformers.IsDelta read
            Result := 0;
            elem := ActiveTransformer;
            if elem <> NIL then
                if elem.WdgConnection[elem.ActiveWinding] > 0 then
                    Result := 1;
        end;
        7:
        begin  // Transformers.IsDelta write
            if arg = 1 then
                Set_Parameter('Conn', 'Delta')
            else
                Set_Parameter('Conn', 'Wye')
        end;
        8:
        begin  // Transformers.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                lst := ActiveCircuit[ActiveActor].Transformers;
                elem := lst.First;
                if elem <> NIL then
                begin
                    repeat
                        if elem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                            Result := 1;
                        end
                        else
                            elem := lst.Next;
                    until (Result = 1) or (elem = NIL);
                end;
            end;
        end;
        9:
        begin  // Transformers.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                lst := ActiveCircuit[ActiveActor].Transformers;
                elem := lst.Next;
                if elem <> NIL then
                begin
                    repeat
                        if elem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                            Result := lst.ActiveIndex;
                        end
                        else
                            elem := lst.Next;
                    until (Result > 0) or (elem = NIL);
                end
            end;
        end;
        10:
        begin  // Transformers.Count
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := ActiveCircuit[ActiveActor].Transformers.ListSize;
        end
    else
        Result := -1;
    end;
end;

//*****************************Floating point type properties************************
function TransformersF(mode: Longint; arg: Double): Double; CDECL;

var
    elem: TTransfObj;

begin
    Result := 0.0;     // Default return value
    case mode of
        0:
        begin  // Transformers.R read
            Result := 0.0;
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := elem.WdgResistance[elem.ActiveWinding];
        end;
        1:
        begin  // Transformers.R write
            Set_Parameter('%R', FloatToStr(arg));
        end;
        2:
        begin  // Transformers.Tap read
            Result := 0.0;
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := elem.PresentTap[elem.ActiveWinding, ActiveActor];
        end;
        3:
        begin  // Transformers.Tap write
            Set_Parameter('Tap', FloatToStr(arg));
        end;
        4:
        begin  // Transformers.MinTap read
            Result := 0.0;
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := elem.Mintap[elem.ActiveWinding];
        end;
        5:
        begin  // Transformers.MinTap write
            Set_Parameter('MinTap', FloatToStr(arg));
        end;
        6:
        begin  // Transformers.MaxTap read
            Result := 0.0;
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := elem.Maxtap[elem.ActiveWinding];
        end;
        7:
        begin  // Transformers.MaxTap write
            Set_Parameter('MaxTap', FloatToStr(arg));
        end;
        8:
        begin  // Transformers.kV read
            Result := 0.0;
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := elem.Winding^[elem.ActiveWinding].kvll;
        end;
        9:
        begin  // Transformers.kV write
            Set_Parameter('kv', FloatToStr(arg));
        end;
        10:
        begin  // Transformers.kVA read
            Result := 0.0;
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := elem.WdgKVA[elem.ActiveWinding];
        end;
        11:
        begin  // Transformers.kVA write
            Set_Parameter('kva', FloatToStr(arg));
        end;
        12:
        begin  // Transformers.Xneut read
            Result := 0.0;
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := elem.WdgXneutral[elem.ActiveWinding];
        end;
        13:
        begin  // Transformers.Xneut write
            Set_Parameter('Xneut', FloatToStr(arg));
        end;
        14:
        begin  // Transformers.Rneut read
            Result := 0.0;
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := elem.WdgRneutral[elem.ActiveWinding];
        end;
        15:
        begin  // Transformers.Rneut write
            Set_Parameter('Rneut', FloatToStr(arg));
        end;
        16:
        begin  // Transformers.Xhl read
            Result := 0.0;
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := elem.XhlVal;
        end;
        17:
        begin  // Transformers.Xhl write
            Set_Parameter('Xhl', FloatToStr(arg));
        end;
        18:
        begin  // Transformers.Xht read
            Result := 0.0;
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := elem.XhtVal;
        end;
        19:
        begin  // Transformers.Xht write
            Set_Parameter('Xht', FloatToStr(arg));
        end;
        20:
        begin  // Transformers.Xlt read
            Result := 0.0;
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := elem.XltVal;
        end;
        21:
        begin  // Transformers.Xlt write
            Set_Parameter('Xlt', FloatToStr(arg));
        end;
        22:
        begin  // Transformers.RdCOhms read
            Result := 0.0;
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := elem.WdgRdc[elem.ActiveWinding];
        end;
        23:
        begin  // Transformers.RdCOhms write
            Set_Parameter('RdcOhms', FloatToStr(arg));
        end
    else
        Result := -1.0;
    end;
end;

//*******************************String type properties****************************
function TransformersS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    elem: TTransfObj;
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
    lst: TPointerList;

begin
    Result := pAnsiChar(Ansistring('0'));   // Default return value
    case mode of
        0:
        begin  // Transformers.XfmrCode read
            Result := pAnsiChar(Ansistring(''));
            elem := ActiveTransformer;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.XfmrCode));
        end;
        1:
        begin  // Transformers.XfmrCode write
            Set_Parameter('XfmrCode', arg);
        end;
        2:
        begin  // Transformers.Name read
            Result := pAnsiChar(Ansistring(''));
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                elem := ActiveCircuit[ActiveActor].Transformers.Active;
                if elem <> NIL then
                    Result := pAnsiChar(Ansistring(elem.Name));
            end;
        end;
        3:
        begin  // Transformers.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                lst := ActiveCircuit[ActiveActor].Transformers;
                S := Widestring(arg);  // Convert to Pascal String
                Found := FALSE;
                ActiveSave := lst.ActiveIndex;
                elem := lst.First;
                while elem <> NIL do
                begin
                    if (CompareText(elem.Name, S) = 0) then
                    begin
                        ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                        Found := TRUE;
                        Break;
                    end;
                    elem := lst.Next;
                end;
                if not Found then
                begin
                    DoSimpleMsg('Transformer "' + S + '" Not Found in Active Circuit.', 5003);
                    elem := lst.Get(ActiveSave);    // Restore active Load
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                end;
            end;
        end;
        4:
        begin // Transformers.StrWdgVoltages
            elem := ActiveTransformer;
            if elem <> NIL then
            begin
                Result := pAnsiChar(elem.GetWindingCurrentsResult(ActiveActor));
            end;
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not valid'));
    end;
end;

//*****************************Variant ype properties*****************************
procedure TransformersV(mode: Longint; out arg: Variant); CDECL;

var
    elem: TTransfObj;
    lst: TPointerList;
    i,
    iV,
    NumCurrents,
    k: Integer;
    TempCurrentBuffer,
    TempVoltageBuffer: pComplexArray;

begin
    case mode of
        0:
        begin  // Transformers.AllNames
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                    if Transformers.ListSize > 0 then
                    begin
                        lst := Transformers;
                        VarArrayRedim(arg, lst.ListSize - 1);
                        k := 0;
                        elem := lst.First;
                        while elem <> NIL do
                        begin
                            arg[k] := elem.Name;
                            Inc(k);
                            elem := lst.Next;
                        end;
                    end;
        end;
        1:
        begin // Transformers.WdgVoltages

            elem := ActiveTransformer;
            if elem <> NIL then
            begin
                if (elem.ActiveWinding > 0) and (elem.ActiveWinding <= elem.NumberOfWindings) then
                begin
                    arg := VarArrayCreate([0, 2 * elem.nphases - 1], varDouble);
                    TempVoltageBuffer := AllocMem(Sizeof(Complex) * elem.nphases);
                    elem.GetWindingVoltages(elem.ActiveWinding, TempVoltageBuffer, ActiveActor);
                    iV := 0;
                    for i := 1 to elem.Nphases do
                    begin
                        arg[iV] := TempVoltageBuffer^[i].re;
                        Inc(iV);
                        arg[iV] := TempVoltageBuffer^[i].im;
                        Inc(iV);
                    end;

                    Reallocmem(TempVoltageBuffer, 0);
                end
                else
                    arg := VarArrayCreate([0, 0], varDouble);
                ;

            end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end;
        2:
        begin  // Transformers.WdgCurrents
            elem := ActiveTransformer;
            if elem <> NIL then
            begin
                NumCurrents := 2 * elem.NPhases * elem.NumberOfWindings; // 2 currents per winding
                arg := VarArrayCreate([0, 2 * NumCurrents - 1], varDouble);
                TempCurrentBuffer := AllocMem(Sizeof(Complex) * NumCurrents);
                ;
                elem.GetAllWindingCurrents(TempCurrentBuffer, ActiveActor);
                iV := 0;
                for i := 1 to NumCurrents do
                begin
                    arg[iV] := TempCurrentBuffer^[i].re;
                    Inc(iV);
                    arg[iV] := TempCurrentBuffer^[i].im;
                    Inc(iV);
                end;

                Reallocmem(TempCurrentBuffer, 0);

            end
            else
                arg := VarArrayCreate([0, 0], varDouble);
        end
    else
        arg[0] := 'Error, parameter not valid';
    end;
end;

end.
