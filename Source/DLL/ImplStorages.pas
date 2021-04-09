unit ImplStorages;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TStorages = class(TAutoObject, IStorages)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_RegisterNames: OleVariant; safecall;
    function Get_RegisterValues: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_Count: Integer; safecall;
    function Get_idx: Integer; safecall;
    procedure Set_idx(Value: Integer); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_State: Integer; safecall;
    procedure Set_State(Value: Integer); safecall;
    function Get_puSOC: Double; safecall;
    procedure Set_puSOC(Value: Double); safecall;

  end;

implementation

uses ComServ, DSSGlobals, Storage, Variants, Sysutils;

function TStorages.Get_AllNames: OleVariant;
Var
  StorageElem:TStorageObj;
  k:Integer;

Begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     If StorageElements.ListSize>0 Then
     Begin
       VarArrayRedim(result, StorageElements.ListSize-1);
       k:=0;
       StorageElem := StorageElements.First;
       WHILE StorageElem<>Nil DO  Begin
          Result[k] := StorageElem.Name;
          Inc(k);
          StorageElem := StorageElements.Next;
       End;
     End;

end;

function TStorages.Get_RegisterNames: OleVariant;
Var
    k :integer;

Begin
    Result := VarArrayCreate([0, NumStorageRegisters - 1], varOleStr);
    For k := 0 to  NumStorageRegisters - 1  Do Begin
       Result[k] := StorageClass[ActiveActor].RegisterNames[k + 1];
    End;

end;

function TStorages.Get_RegisterValues: OleVariant;
Var
   StorageElem :TStorageObj;
   k     :Integer;
Begin

   IF ActiveCircuit[ActiveActor] <> Nil THEN
   Begin
        StorageElem :=  TStorageObj(ActiveCircuit[ActiveActor].StorageElements.Active);
        If StorageElem <> Nil Then
        Begin
            Result := VarArrayCreate([0, numStorageRegisters-1], varDouble);
            FOR k := 0 to numStorageRegisters-1 DO
            Begin
                Result[k] := StorageElem.Registers[k+1];
            End;
        End
        Else
            Result := VarArrayCreate([0, 0], varDouble);
   End
   ELSE Begin
        Result := VarArrayCreate([0, 0], varDouble);
   End;


end;

function TStorages.Get_First: Integer;
Var
   pStorageElem:TStorageObj;

Begin

   Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pStorageElem := ActiveCircuit[ActiveActor].StorageElements.First;
        If pStorageElem <> Nil Then
        Begin
          Repeat
            If pStorageElem.Enabled
            Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pStorageElem;
              Result := 1;
            End
            Else pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Next;
          Until (Result = 1) or (pStorageElem = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TStorages.Get_Next: Integer;
Var
   pStorageElem:TStorageObj;

Begin

   Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Next;
        If pStorageElem <> Nil Then
        Begin
          Repeat
            If pStorageElem.Enabled
            Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pStorageElem;
              Result := ActiveCircuit[ActiveActor].StorageElements.ActiveIndex;
            End
            Else pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Next;
          Until (Result > 0) or (pStorageElem = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TStorages.Get_Count: Integer;
begin
    If Assigned(ActiveCircuit[ActiveActor]) Then
          Result := ActiveCircuit[ActiveActor].StorageElements.ListSize;

end;

function TStorages.Get_idx: Integer;
begin
    if ActiveCircuit[ActiveActor] <> Nil then
       Result := ActiveCircuit[ActiveActor].StorageElements.ActiveIndex
    else Result := 0;
end;

procedure TStorages.Set_idx(Value: Integer);
Var
    pStorage:TStorageObj;
begin
    if ActiveCircuit[ActiveActor] <> Nil then   Begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Get(Value);
        If pStorage <> Nil Then ActiveCircuit[ActiveActor].ActiveCktElement := pStorage;
    End;

end;

function TStorages.Get_Name: WideString;
Var
   pStorage:TStorageObj;

Begin
   Result := '';
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        If pStorage <> Nil Then
        Begin
          Result := pStorage.Name;
        End
        Else
            Result := '';  // signify no name
   End;

end;

procedure TStorages.Set_Name(const Value: WideString);
VAR
    activesave :integer;
    StorageElem:TStorageObj;
    S: String;
    Found :Boolean;
Begin


  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin      // Search list of Storages in active circuit for name
       WITH ActiveCircuit[ActiveActor].StorageElements DO
         Begin
             S := Value;  // Convert to Pascal String
             Found := FALSE;
             ActiveSave := ActiveIndex;
             StorageElem := First;
             While StorageElem <> NIL Do
             Begin
                IF (CompareText(StorageElem.Name, S) = 0)
                THEN Begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := StorageElem;
                    Found := TRUE;
                    Break;
                End;
                StorageElem := Next;
             End;
             IF NOT Found
             THEN Begin
                 DoSimpleMsg('Storage "'+S+'" Not Found in Active Circuit.', 5003);
                 StorageElem := Get(ActiveSave);    // Restore active Storage
                 ActiveCircuit[ActiveActor].ActiveCktElement := StorageElem;
             End;
         End;
  End;


end;

function TStorages.Get_State: Integer;
Var
   pStorage:TStorageObj;

Begin
   Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        If pStorage <> Nil Then
        Begin
          Result := pStorage.StorageState;
        End;

   End;

end;

procedure TStorages.Set_State(Value: Integer);
{  Legal States
     STORE_CHARGING    = -1;
     STORE_IDLING      =  0;
     STORE_DISCHARGING =  1;
}
Var
   pStorage:TStorageObj;

Begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        If pStorage <> Nil Then
        Begin
          pStorage.StorageState := Value;
        End;

   End;

end;

function TStorages.Get_puSOC: Double;
Var
   pStorage:TStorageObj;

Begin
   Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        If pStorage <> Nil Then
        Begin
          Result := pStorage.Storagevars.kWhStored/pStorage.StorageVars.kWhRating;
        End;
   End;

end;

procedure TStorages.Set_puSOC(Value: Double);
Var
   pStorage:TStorageObj;

Begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        If pStorage <> Nil Then
        Begin
          pStorage.Storagevars.kWhStored := pStorage.StorageVars.kWhRating * Value;
        End;
   End;

end;

initialization
  TAutoObjectFactory.Create(ComServer, TStorages, Class_Storages,
    ciInternal, tmApartment);
end.
