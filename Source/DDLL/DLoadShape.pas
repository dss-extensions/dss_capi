unit DLoadShape;

interface

function LoadShapeI(mode: Longint; arg: Longint): Longint; CDECL;
function LoadShapeF(mode: Longint; arg: Double): Double; CDECL;
function LoadShapeS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure LoadShapeV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    Loadshape,
    DSSGlobals,
    PointerList,
    Variants,
    ExecHelper,
    ucomplex;

var
    ActiveLSObject: TLoadshapeObj;

function LoadShapeI(mode: Longint; arg: Longint): Longint; CDECL;

var
    iElem: Integer;

begin
    Result := 0;   // Default return value
    case mode of
        0:
        begin  // LoadShapes.Count
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := LoadshapeClass[ActiveActor].ElementList.ListSize;
        end;
        1:
        begin  // LoadShapes.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                iElem := LoadshapeClass[ActiveActor].First;
                if iElem <> 0 then
                begin
                    ActiveLSObject := ActiveDSSObject[ActiveActor] as TLoadShapeObj;
                    Result := 1;
                end
            end;
        end;
        2:
        begin  // LoadShapes.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                iElem := LoadshapeClass[ActiveActor].Next;
                if iElem <> 0 then
                begin
                    ActiveLSObject := ActiveDSSObject[ActiveActor] as TLoadShapeObj;
                    Result := iElem;
                end
            end;
        end;
        3:
        begin  // LoadShapes.Npts read
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveLSObject <> NIL then
                    Result := ActiveLSObject.NumPoints;
        end;
        4:
        begin  // LoadShapes.Npts write
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveLSObject <> NIL then
                    ActiveLSObject.NumPoints := arg;
        end;
        5:
        begin  // LoadShapes.Normalize
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveLSObject <> NIL then
                    ActiveLSObject.Normalize;
        end;
        6:
        begin   // LoadShapes.UseActual read
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveLSObject <> NIL then
                    if ActiveLSObject.UseActual then
                        Result := 1;
        end;
        7:
        begin   // LoadShapes.UseActual write
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveLSObject <> NIL then
                begin
                    if arg = 1 then
                        ActiveLSObject.UseActual := TRUE
                    else
                        ActiveLSObject.UseActual := FALSE
                end;
        end
    else
        Result := -1;
    end;
end;

//**********************Floating point type properties***************************
function LoadShapeF(mode: Longint; arg: Double): Double; CDECL;
begin
    Result := 0.0;    // Default return value
    case mode of
        0:
        begin  // LoadShapes.HrInterval read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveLSObject <> NIL then
                    Result := ActiveLSObject.Interval;
        end;
        1:
        begin  // LoadShapes.HrInterval write
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveLSObject <> NIL then
                    ActiveLSObject.Interval := arg;
        end;
        2:
        begin  // LoadShapes.MinInterval read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveLSObject <> NIL then
                    Result := ActiveLSObject.Interval * 60.0;
        end;
        3:
        begin  // LoadShapes.MinInterval write
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveLSObject <> NIL then
                    ActiveLSObject.Interval := arg / 60.0;
        end;
        4:
        begin  // LoadShapes.PBase read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveLSObject <> NIL then
                    Result := ActiveLSObject.baseP;
        end;
        5:
        begin  // LoadShapes.PBase write
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveLSObject <> NIL then
                    ActiveLSObject.baseP := arg;
        end;
        6:
        begin  // LoadShapes.QBase read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveLSObject <> NIL then
                    Result := ActiveLSObject.baseQ;
        end;
        7:
        begin  // LoadShapes.QBase write
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveLSObject <> NIL then
                    ActiveLSObject.baseQ := arg;
        end;
        8:
        begin  // LoadShapes.Sinterval read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveLSObject <> NIL then
                    Result := ActiveLSObject.Interval * 3600.0;
        end;
        9:
        begin  // LoadShapes.Sinterval write
            if ActiveCircuit[ActiveActor] <> NIL then
                if ActiveLSObject <> NIL then
                    ActiveLSObject.Interval := arg / 3600.0;
        end
    else
        Result := -1.0;
    end;
end;

//**********************String type properties***************************
function LoadShapeS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    elem: TLoadshapeObj;

begin
    Result := Pansichar(Ansistring(''));      // Default return value
    case mode of
        0:
        begin  // LoadShapes.Name read
            Result := Pansichar(Ansistring(''));
            elem := LoadshapeClass[ActiveActor].GetActiveObj;
            if elem <> NIL then
                Result := Pansichar(Ansistring(elem.Name));
        end;
        1:
        begin  // LoadShapes.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if LoadshapeClass[ActiveActor].SetActive(String(arg)) then
                begin
                    ActiveLSObject := LoadshapeClass[ActiveActor].ElementList.Active;
                    ActiveDSSObject[ActiveActor] := ActiveLSObject;
                end
                else
                begin
                    DoSimpleMsg('Relay "' + arg + '" Not Found in Active Circuit.', 77003);
                end;
            end;
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//**********************Variant type properties***************************
procedure LoadShapeV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    i,
    k,
    LoopLimit: Integer;
    elem: TLoadshapeObj;
    pList: TPointerList;
    Sample: Complex;
    UseHour: Boolean;
    PDouble: ^Double;

begin
    case mode of
        0:
        begin  // LoadShapes.AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if LoadShapeClass[ActiveActor].ElementList.ListSize > 0 then
                begin
                    pList := LoadShapeClass[ActiveActor].ElementList;
                    elem := pList.First;
                    while elem <> NIL do
                    begin
                        WriteStr2Array(elem.Name);
                        WriteStr2Array(Char(0));
                        elem := pList.next;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        1:
        begin  // LoadShapes.PMult read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if ActiveLSObject <> NIL then
                begin
                    setlength(myDBLArray, ActiveLSObject.NumPoints);
                    UseHour := ActiveLSObject.Interval = 0;
                    for k := 1 to ActiveLSObject.NumPoints do
                    begin
                        if UseHour then
                            Sample := ActiveLSObject.GetMult(ActiveLSObject.Hours^[k]) // For variable step
                        else
                            Sample := ActiveLSObject.GetMult(k * ActiveLSObject.Interval);     // This change adds compatibility with MMF
                        myDBLArray[k - 1] := Sample.re;
                    end;
                end
                else
                begin
                    DoSimpleMsg('No active Loadshape Object found.', 61001);
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        2:
        begin  // LoadShapes.PMult write
            myType := 2;        // Double
            k := 1;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if ActiveLSObject <> NIL then
                    with ActiveLSObject do
                    begin
          // Only put in as many points as we have allocated
                        if (mySize > NumPoints) then
                            LoopLimit := NumPoints - 1
                        else
                            LoopLimit := mySize - 1;
                        ReallocMem(PMultipliers, Sizeof(PMultipliers^[1]) * NumPoints);
                        for i := 0 to LoopLimit do
                        begin
                            PDouble := myPointer;
                            ActiveLSObject.Pmultipliers^[k] := PDOuble^;
                            inc(k);
                            inc(Pbyte(myPointer), 8);
                        end;
                    end
                else
                begin
                    DoSimpleMsg('No active Loadshape Object found.', 61002);
                end;
            end;
            mySize := k - 1;
        end;
        3:
        begin  // LoadShapes.QMult read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if ActiveLSObject <> NIL then
                begin
                    if assigned(ActiveLSObject.QMultipliers) then
                    begin
                        setlength(myDBLArray, ActiveLSObject.NumPoints);    // This change adds compatibility with MMF
                        UseHour := ActiveLSObject.Interval = 0;
                        for k := 1 to ActiveLSObject.NumPoints do
                        begin
                            if UseHour then
                                Sample := ActiveLSObject.GetMult(ActiveLSObject.Hours^[k]) // For variable step
                            else
                                Sample := ActiveLSObject.GetMult(k * ActiveLSObject.Interval);
                            myDBLArray[k - 1] := Sample.im;
                        end;
                    end;
                end
                else
                begin
                    DoSimpleMsg('No active Loadshape Object found.', 61001);
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        4:
        begin  // LoadShapes.QMult write
            myType := 2;        // Double
            k := 1;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if ActiveLSObject <> NIL then
                    with ActiveLSObject do
                    begin

          // Only put in as many points as we have allocated
                        if (mySize > NumPoints) then
                            LoopLimit := NumPoints - 1
                        else
                            LoopLimit := mySize - 1;

                        ReallocMem(QMultipliers, Sizeof(QMultipliers^[1]) * NumPoints);
                        for i := 0 to LoopLimit do
                        begin
                            PDouble := myPointer;
                            ActiveLSObject.Qmultipliers^[k] := PDouble^;
                            inc(k);
                            inc(Pbyte(myPointer), 8);
                        end;

                    end
                else
                begin
                    DoSimpleMsg('No active Loadshape Object found.', 61002);
                end;
            end;
            mySize := k - 1;
        end;
        5:
        begin   // LoadShapes.Timearray read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if ActiveLSObject <> NIL then
                begin
                    if ActiveLSObject.hours <> NIL then
                    begin
                        setlength(myDBLArray, ActiveLSObject.NumPoints);
                        for k := 0 to ActiveLSObject.NumPoints - 1 do
                            myDBLArray[k] := ActiveLSObject.Hours^[k + 1];
                    end
                end
                else
                begin
                    DoSimpleMsg('No active Loadshape Object found.', 61001);
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        6:
        begin   // LoadShapes.Timearray write
            myType := 2;        // Double
            k := 1;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if ActiveLSObject <> NIL then
                    with ActiveLSObject do
                    begin
          // Only put in as many points as we have allocated
                        if (mySize > NumPoints) then
                            LoopLimit := NumPoints - 1
                        else
                            LoopLimit := mySize - 1;
                        ReallocMem(Hours, Sizeof(Hours^[1]) * NumPoints);
                        k := 1;
                        for i := 0 to LoopLimit do
                        begin
                            PDouble := myPointer;
                            ActiveLSObject.Hours^[k] := PDouble^;
                            inc(k);
                            inc(Pbyte(myPointer), 8);
                        end;
                    end
                else
                begin
                    DoSimpleMsg('No active Loadshape Object found.', 61002);
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
