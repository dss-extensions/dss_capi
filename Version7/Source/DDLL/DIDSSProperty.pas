unit DIDSSProperty;

interface

function DSSProperties(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

implementation

uses
    DSSClass,
    DSSGlobals,
    Executive,
    SysUtils;

var
    FPropIndex: Integer;

function DSSProperties(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
begin
    Result := pAnsiChar(Ansistring('')); // Default return value
    FPropIndex := StrToInt(String(arg));
    case mode of
        0:
        begin                                           // DSSProperties.Name
            Result := pAnsiChar(Ansistring(''));
            if (ActiveCircuit <> NIL) and (FPropIndex <> 0) {and (FPropClass <> Nil)} then
                with  ActiveDSSObject.ParentClass do
                    if FPropIndex <= NumProperties then
                        Result := pAnsiChar(Ansistring(PropertyName^[FPropIndex]));
        end;
        1:
        begin                                           // DSSProperties.Description
            Result := pAnsiChar(Ansistring(''));
            if (ActiveCircuit <> NIL) and (FPropIndex <> 0) {and (FPropClass <> Nil)} then
                with  ActiveDSSObject.ParentClass do
                    if FPropIndex <= NumProperties then
                        Result := pAnsiChar(Ansistring(PropertyHelp^[FPropIndex]));
        end;
        2:
        begin                                           // DSSProperties.Value - read
            Result := pAnsiChar(Ansistring(''));
            if (ActiveCircuit <> NIL) then
                with ActiveDSSObject do
                    if FPropIndex <= ParentClass.NumProperties then
                        Result := pAnsiChar(Ansistring(PropertyValue[ParentClass.PropertyIdxMap[FPropIndex]]));
        end;
        3:
        begin                                           // DSSProperties.Value - Write
            if (ActiveCircuit <> NIL) then
                with ActiveDSSObject do
                    if FPropIndex <= ParentClass.NumProperties then
                        DSSExecutive.Command := 'Edit ' + ParentClass.Name + '.' + Name + ' ' +
                            ParentClass.PropertyName^[FPropIndex] + '=' +
                            String(arg);
            Result := pAnsiChar(Ansistring(''));
        end
    else
        Result := pAnsiChar(Ansistring(''));
    end;
end;

end.
