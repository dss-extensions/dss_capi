unit DActiveClass;

interface

function ActiveClassI(mode: Longint; arg: Longint): Longint; CDECL;
function ActiveClassS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure ActiveClassV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;


implementation

uses
    DSSGlobals,
    DSSObject,
    Variants,
    CktElement,
    PCElement,
    DSSClass,
    PDClass,
    PCClass,
    MeterClass,
    ControlClass;

function ActiveClassI(mode: Longint; arg: Longint): Longint; CDECL;
begin
    case mode of
        0:
        begin  // ActiveClass.First
            Result := 0;
            if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass[ActiveActor]) then
            begin
                Result := ActiveDSSClass[ActiveActor].First;  // sets active objects
            end;
        end;
        1:
        begin  // ActiveClass.Next
            Result := 0;
            if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass[ActiveActor]) then
            begin
                Result := ActiveDSSClass[ActiveActor].Next;  // sets active objects
            end;
        end;
        2:
        begin  //ActiveClass.NumElements
            if Assigned(ActiveDSSClass[ActiveActor]) then
                Result := ActiveDSSCLass[ActiveActor].ElementCount
            else
                Result := 0;
        end;
        3:
        begin  //ActiveClass.Count
            if Assigned(ActiveDSSClass[ActiveActor]) then
                Result := ActiveDSSCLass[ActiveActor].ElementCount
            else
                Result := 0;
        end
    else
        Result := -1;
    end;
end;

//***************************String type properties*****************************
function ActiveClassS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    pelem: TDSSObject;

begin
    Result := Pansichar(Ansistring('0'));
    case mode of
        0:
        begin  // ActiveClass.Name read
            if Assigned(ActiveDSSObject[ActiveActor]) then
                Result := Pansichar(Ansistring(ActiveDSSObject[ActiveActor].Name))
            else
                Result := Pansichar(Ansistring(''));
        end;
        1:
        begin  // ActiveClass.Name write
            if Assigned(ActiveDSSClass[ActiveActor]) then
            begin
                pelem := ActiveDSSClass[ActiveActor].Find(String(arg));
                if pelem <> NIL then
                begin
                    if pelem is TDSSCktElement then
                        ActiveCircuit[ActiveActor].ActiveCktElement := TDSSCktElement(pelem)  // sets ActiveDSSobject
                    else
                        ActiveDSSObject[ActiveActor] := pelem;
                end;
            end;
        end;
        2:
        begin  // ActiveClass.ActiveClassName
            if Assigned(ActiveDSSClass[ActiveActor]) then
                Result := Pansichar(Ansistring(ActiveDSSCLass[ActiveActor].Name))
            else
                Result := Pansichar(Ansistring(''));
        end;
        3:
        begin  // ActiveClass.ActiveClassParent
            if Assigned(ActiveDSSClass[ActiveActor]) then
            begin
                Result := Pansichar('Generic Object');

                if ActiveDSSClass[ActiveActor].ClassType.InheritsFrom(TPCClass) then
                    Result := Pansichar('TPCClas');
                if ActiveDSSClass[ActiveActor].ClassType.InheritsFrom(TPDClass) then
                    Result := Pansichar('TPDClass');
                if ActiveDSSClass[ActiveActor].ClassType.InheritsFrom(TMeterClass) then
                    Result := Pansichar('TMeterClass');
                if ActiveDSSClass[ActiveActor].ClassType.InheritsFrom(TControlClass) then
                    Result := Pansichar('TControlClass');
            end
            else
                Result := Pansichar(Ansistring('Parent Class unknonwn'));
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not recognized'));
    end;
end;

//*****************************Variant type properties**************************
procedure ActiveClassV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    idx,
    i: Integer;
    S: String;

begin
    case mode of
        0:
        begin
            setlength(myStrArray, 0);
            mySize := 0;
            if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass[ActiveActor]) then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    setlength(myStrArray, 0);
                    idx := ActiveDSSClass[ActiveActor].First;
                    while idx > 0 do
                    begin
                        WriteStr2Array(ActiveDSSObject[ActiveActor].Name);
                        WriteStr2Array(Char(0));
                        idx := ActiveDSSClass[ActiveActor].Next;
                    end;
                end
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myType := 4;                  // String
            mySize := length(myStrArray);
            myPointer := @(myStrArray[0]);
        end
    else
        setlength(myStrArray, 1);
        myStrArray[0] := 0;
        myPointer := @(myStrArray[0]);
        myType := -1;                  // Error
    end;
end;

end.
