unit DIDSSProperty;

interface

function DSSProperties(mode: Longint; arg: Pansichar): Pansichar; CDECL;

implementation

uses
    DSSClass,
    DSSGlobals,
    Executive,
    SysUtils;

var
    FPropIndex: Integer;

function DSSProperties(mode: Longint; arg: Pansichar): Pansichar; CDECL;
begin
    Result := Pansichar(Ansistring('')); // Default return value
    FPropIndex := StrToInt(String(arg));
    case mode of
        0:
        begin                                           // DSSProperties.Name
            if (ActiveCircuit[ActiveActor] <> NIL) and (FPropIndex <> 0) {and (FPropClass <> Nil)} then
                with  ActiveDSSObject[ActiveActor].ParentClass do
                    if FPropIndex <= NumProperties then
                        Result := Pansichar(Ansistring(PropertyName^[FPropIndex]));
        end;
        1:
        begin                                           // DSSProperties.Description
            if (ActiveCircuit[ActiveActor] <> NIL) and (FPropIndex <> 0) {and (FPropClass <> Nil)} then
                with  ActiveDSSObject[ActiveActor].ParentClass do
                    if FPropIndex <= NumProperties then
                        Result := Pansichar(Ansistring(PropertyHelp^[FPropIndex]));
        end;
        2:
        begin                                           // DSSProperties.Value - read
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveDSSObject[ActiveActor] do
                    if FPropIndex <= ParentClass.NumProperties then
                        Result := Pansichar(Ansistring(PropertyValue[ParentClass.PropertyIdxMap^[FPropIndex]]));
        end;
        3:
        begin                                           // DSSProperties.Value - Write
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveDSSObject[ActiveActor] do
                    if FPropIndex <= ParentClass.NumProperties then
                        DSSExecutive[ActiveActor].Command := 'Edit ' + ParentClass.Name + '.' + Name + ' ' +
                            ParentClass.PropertyName^[FPropIndex] + '=' +
                            String(arg);
            Result := Pansichar(Ansistring(''));
        end
    else
        Result := Pansichar(Ansistring(''));
    end;
end;

end.
