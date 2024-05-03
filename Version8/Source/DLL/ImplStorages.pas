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
    function Get_AmpLimit: Double; safecall;
    function Get_AmpLimitGain: Double; safecall;
    function Get_ChargeTrigger: Double; safecall;
    function Get_ControlMode: Integer; safecall;
    function Get_DischargeTrigger: Double; safecall;
    function Get_EffCharge: Double; safecall;
    function Get_EffDischarge: Double; safecall;
    function Get_Kp: Double; safecall;
    function Get_kV: Double; safecall;
    function Get_kVA: Double; safecall;
    function Get_kvar: Double; safecall;
    function Get_kVDC: Double; safecall;
    function Get_kW: Double; safecall;
    function Get_kWhRated: Double; safecall;
    function Get_kWRated: Double; safecall;
    function Get_LimitCurrent: Double; safecall;
    function Get_PF: Double; safecall;
    function Get_PITol: Double; safecall;
    function Get_SafeMode: Integer; safecall;
    function Get_SafeVoltage: Double; safecall;
    function Get_TimeChargeTrig: Double; safecall;
    function Get_VarFollowInverter: Integer; safecall;
    procedure Set_AmpLimit(Value: Double); safecall;
    procedure Set_AmpLimitGain(Value: Double); safecall;
    procedure Set_ChargeTrigger(Value: Double); safecall;
    procedure Set_ControlMode(Value: Integer); safecall;
    procedure Set_DischargeTrigger(Value: Double); safecall;
    procedure Set_EffCharge(Value: Double); safecall;
    procedure Set_EffDischarge(Value: Double); safecall;
    procedure Set_Kp(Value: Double); safecall;
    procedure Set_kV(Value: Double); safecall;
    procedure Set_kVA(Value: Double); safecall;
    procedure Set_kvar(Value: Double); safecall;
    procedure Set_kVDC(Value: Double); safecall;
    procedure Set_kW(Value: Double); safecall;
    procedure Set_kWhRated(Value: Double); safecall;
    procedure Set_kWRated(Value: Double); safecall;
    procedure Set_LimitCurrent(Value: Double); safecall;
    procedure Set_PF(Value: Double); safecall;
    procedure Set_PITol(Value: Double); safecall;
    procedure Set_SafeVoltage(Value: Double); safecall;
    procedure Set_TimeChargeTrig(Value: Double); safecall;
    procedure Set_VarFollowInverter(Value: Integer); safecall;

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
  Begin
    WITH ActiveCircuit[ActiveActor] DO
    Begin
      If StorageElements.ListSize>0 Then
      Begin
        VarArrayRedim(result, StorageElements.ListSize-1);
        k:=0;
        StorageElem := StorageElements.First;
        WHILE StorageElem<>Nil DO
        Begin
          Result[k] := StorageElem.Name;
          Inc(k);
          StorageElem := StorageElements.Next;
        End;
      End;
    End;
  End;

end;

function TStorages.Get_RegisterNames: OleVariant;
Var
    k :integer;

Begin
  Result := VarArrayCreate([0, NumStorageRegisters - 1], varOleStr);
  For k := 0 to  NumStorageRegisters - 1  Do
  Begin
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
  ELSE
  Begin
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
        Then
        Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := pStorageElem;
          Result := ActiveCircuit[ActiveActor].StorageElements.ActiveIndex;
        End
        Else pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Next;
      Until (Result > 0) or (pStorageElem = nil);
    End
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
  if ActiveCircuit[ActiveActor] <> Nil then
  Begin
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
  THEN
  Begin      // Search list of Storages in active circuit for name
    WITH ActiveCircuit[ActiveActor].StorageElements DO
    Begin
      S := Value;  // Convert to Pascal String
      Found := FALSE;
      ActiveSave := ActiveIndex;
      StorageElem := First;
      While StorageElem <> NIL Do
      Begin
        IF (CompareText(StorageElem.Name, S) = 0)
        THEN
        Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := StorageElem;
          Found := TRUE;
          Break;
        End;
        StorageElem := Next;
      End;
      IF NOT Found
      THEN
      Begin
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

function TStorages.Get_AmpLimit: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.myDynVars.ILimit;
    End;
  End;

end;

function TStorages.Get_AmpLimitGain: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.myDynVars.VError;
    End;
  End;

end;

function TStorages.Get_ChargeTrigger: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.ChargeTrigger;
    End;
  End;

end;

function TStorages.Get_ControlMode: Integer;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      if pStorage.GFM_mode then
        Result := 1
    End;
  End;

end;

function TStorages.Get_DischargeTrigger: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.DisChargeTrigger;
    End;
  End;

end;

function TStorages.Get_EffCharge: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.pctChargeEff;
    End;
  End;

end;

function TStorages.Get_EffDischarge: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.pctDischargeEff;
    End;
  End;

end;

function TStorages.Get_Kp: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.myDynVars.kP * 1e3;
    End;
  End;

end;

function TStorages.Get_kV: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.PresentkV;
    End;
  End;

end;

function TStorages.Get_kVA: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.StorageVars.FkVArating;
    End;
  End;

end;

function TStorages.Get_kvar: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.kvarRequested;
    End;
  End;

end;

function TStorages.Get_kVDC: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.myDynVars.RatedVDC / 1e3;
    End;
  End;

end;

function TStorages.Get_kW: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.kW;
    End;
  End;

end;

function TStorages.Get_kWhRated: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.StorageVars.kWhrating;
    End;
  End;

end;

function TStorages.Get_kWRated: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.StorageVars.kWrating;
    End;
  End;

end;

function TStorages.Get_LimitCurrent: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      if pStorage.IsCurrentLimited then
        Result := 1;
    End;
  End;

end;

function TStorages.Get_PF: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.PFnominal;
    End;
  End;

end;

function TStorages.Get_PITol: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.myDynVars.CtrlTol * 100;
    End;
  End;

end;

function TStorages.Get_SafeMode: Integer;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      if pStorage.myDynVars.SafeMode then
        Result := 1;
    End;
  End;

end;

function TStorages.Get_SafeVoltage: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.myDynVars.SMThreshold;
    End;
  End;

end;

function TStorages.Get_TimeChargeTrig: Double;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      Result := pStorage.ChargeTime;
    End;
  End;

end;

function TStorages.Get_VarFollowInverter: Integer;
Var
   pStorage:TStorageObj;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      if pStorage.FVarFollowInverter then
        Result := 1;
    End;
  End;

end;

procedure TStorages.Set_AmpLimit(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.myDynVars.ILimit := Value;
    End;
  End;

end;

procedure TStorages.Set_AmpLimitGain(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.myDynVars.VError := Value;
    End;
  End;

end;

procedure TStorages.Set_ChargeTrigger(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.ChargeTrigger := Value;
    End;
  End;

end;

procedure TStorages.Set_ControlMode(Value: Integer);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.GFM_mode := False;
      if Value <> 0 then
        pStorage.GFM_mode := True
    End;
  End;

end;

procedure TStorages.Set_DischargeTrigger(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.DisChargeTrigger := value;
    End;
  End;

end;

procedure TStorages.Set_EffCharge(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.pctChargeEff := Value;
    End;
  End;

end;

procedure TStorages.Set_EffDischarge(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.pctDischargeEff := Value;
    End;
  End;

end;

procedure TStorages.Set_Kp(Value: Double);
Var
   pStorage:TStorageObj;

Begin

  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.myDynVars.kP := Value / 1e3;
    End;
  End;

end;

procedure TStorages.Set_kV(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.PresentkV := Value;
    End;
  End;

end;

procedure TStorages.Set_kVA(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.StorageVars.FkVArating := Value;
    End;
  End;

end;

procedure TStorages.Set_kvar(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.kvarRequested := Value;
    End;
  End;

end;

procedure TStorages.Set_kVDC(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.myDynVars.RatedVDC := Value * 1e3;
    End;
  End;

end;

procedure TStorages.Set_kW(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.kW := Value;
    End;
  End;

end;

procedure TStorages.Set_kWhRated(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.StorageVars.kWhrating := Value;
    End;
  End;

end;

procedure TStorages.Set_kWRated(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.StorageVars.kWrating := Value;
    End;
  End;


end;

procedure TStorages.Set_LimitCurrent(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.IsCurrentLimited := False;
      if Value <> 0 then
        pStorage.IsCurrentLimited := True;
    End;
  End;

end;

procedure TStorages.Set_PF(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.PFnominal := Value;
    End;
  End;

end;

procedure TStorages.Set_PITol(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.myDynVars.CtrlTol := value / 100;
    End;
  End;

end;

procedure TStorages.Set_SafeVoltage(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.myDynVars.SMThreshold := Value;
    End;
  End;

end;

procedure TStorages.Set_TimeChargeTrig(Value: Double);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.ChargeTime := Value;
    End;
  End;

end;

procedure TStorages.Set_VarFollowInverter(Value: Integer);
Var
   pStorage:TStorageObj;

Begin
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
    If pStorage <> Nil Then
    Begin
      pStorage.FVarFollowInverter := False;
      if Value <> 0 then
         pStorage.FVarFollowInverter := True
    End;
  End;

end;

initialization
  TAutoObjectFactory.Create(ComServer, TStorages, Class_Storages,
    ciInternal, tmApartment);
end.
