unit ImplStorages;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TStorages = class(TAutoObject, IStorages)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_RegisterNames: Olevariant; SAFECALL;
        function Get_RegisterValues: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_idx: Integer; SAFECALL;
        procedure Set_idx(Value: Integer); SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_State: Integer; SAFECALL;
        procedure Set_State(Value: Integer); SAFECALL;
        function Get_puSOC: Double; SAFECALL;
        procedure Set_puSOC(Value: Double); SAFECALL;

    end;

implementation

uses
    ComServ,
    DSSGlobals,
    Storage,
    Variants,
    Sysutils;

function TStorages.Get_AllNames: Olevariant;
var
    StorageElem: TStorageObj;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if StorageElements.ListSize > 0 then
            begin
                VarArrayRedim(result, StorageElements.ListSize - 1);
                k := 0;
                StorageElem := StorageElements.First;
                while StorageElem <> NIL do
                begin
                    Result[k] := StorageElem.Name;
                    Inc(k);
                    StorageElem := StorageElements.Next;
                end;
            end;

end;

function TStorages.Get_RegisterNames: Olevariant;
var
    k: Integer;

begin
    Result := VarArrayCreate([0, NumStorageRegisters - 1], varOleStr);
    for k := 0 to NumStorageRegisters - 1 do
    begin
        Result[k] := StorageClass[ActiveActor].RegisterNames[k + 1];
    end;

end;

function TStorages.Get_RegisterValues: Olevariant;
var
    StorageElem: TStorageObj;
    k: Integer;
begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        StorageElem := TStorageObj(ActiveCircuit[ActiveActor].StorageElements.Active);
        if StorageElem <> NIL then
        begin
            Result := VarArrayCreate([0, numStorageRegisters - 1], varDouble);
            for k := 0 to numStorageRegisters - 1 do
            begin
                Result[k] := StorageElem.Registers[k + 1];
            end;
        end
        else
            Result := VarArrayCreate([0, 0], varDouble);
    end
    else
    begin
        Result := VarArrayCreate([0, 0], varDouble);
    end;


end;

function TStorages.Get_First: Integer;
var
    pStorageElem: TStorageObj;

begin

    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pStorageElem := ActiveCircuit[ActiveActor].StorageElements.First;
        if pStorageElem <> NIL then
        begin
            repeat
                if pStorageElem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pStorageElem;
                    Result := 1;
                end
                else
                    pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Next;
            until (Result = 1) or (pStorageElem = NIL);
        end
        else
            Result := 0;  // signify no more
    end;

end;

function TStorages.Get_Next: Integer;
var
    pStorageElem: TStorageObj;

begin

    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Next;
        if pStorageElem <> NIL then
        begin
            repeat
                if pStorageElem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pStorageElem;
                    Result := ActiveCircuit[ActiveActor].StorageElements.ActiveIndex;
                end
                else
                    pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Next;
            until (Result > 0) or (pStorageElem = NIL);
        end
        else
            Result := 0;  // signify no more
    end;

end;

function TStorages.Get_Count: Integer;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ActiveCircuit[ActiveActor].StorageElements.ListSize;

end;

function TStorages.Get_idx: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].StorageElements.ActiveIndex
    else
        Result := 0;
end;

procedure TStorages.Set_idx(Value: Integer);
var
    pStorage: TStorageObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Get(Value);
        if pStorage <> NIL then
            ActiveCircuit[ActiveActor].ActiveCktElement := pStorage;
    end;

end;

function TStorages.Get_Name: Widestring;
var
    pStorage: TStorageObj;

begin
    Result := '';
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> NIL then
        begin
            Result := pStorage.Name;
        end
        else
            Result := '';  // signify no name
    end;

end;

procedure TStorages.Set_Name(const Value: Widestring);
var
    activesave: Integer;
    StorageElem: TStorageObj;
    S: String;
    Found: Boolean;
begin


    if ActiveCircuit[ActiveActor] <> NIL then
    begin      // Search list of Storages in active circuit for name
        with ActiveCircuit[ActiveActor].StorageElements do
        begin
            S := Value;  // Convert to Pascal String
            Found := FALSE;
            ActiveSave := ActiveIndex;
            StorageElem := First;
            while StorageElem <> NIL do
            begin
                if (CompareText(StorageElem.Name, S) = 0) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := StorageElem;
                    Found := TRUE;
                    Break;
                end;
                StorageElem := Next;
            end;
            if not Found then
            begin
                DoSimpleMsg('Storage "' + S + '" Not Found in Active Circuit.', 5003);
                StorageElem := Get(ActiveSave);    // Restore active Storage
                ActiveCircuit[ActiveActor].ActiveCktElement := StorageElem;
            end;
        end;
    end;


end;

function TStorages.Get_State: Integer;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> NIL then
        begin
            Result := pStorage.StorageState;
        end;

    end;

end;

procedure TStorages.Set_State(Value: Integer);
{  Legal States
     STORE_CHARGING    = -1;
     STORE_IDLING      =  0;
     STORE_DISCHARGING =  1;
}
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> NIL then
        begin
            pStorage.StorageState := Value;
        end;

    end;

end;

function TStorages.Get_puSOC: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> NIL then
        begin
            Result := pStorage.Storagevars.kWhStored / pStorage.StorageVars.kWhRating;
        end;
    end;

end;

procedure TStorages.Set_puSOC(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> NIL then
        begin
            pStorage.Storagevars.kWhStored := pStorage.StorageVars.kWhRating * Value;
        end;
    end;

end;

initialization
    TAutoObjectFactory.Create(ComServer, TStorages, Class_Storages,
        ciInternal, tmApartment);
end.
