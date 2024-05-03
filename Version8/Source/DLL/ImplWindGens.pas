unit ImplWindGens;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TWindGens = class(TAutoObject, IWindGens)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_RegisterNames: OleVariant; safecall;
    function Get_RegisterValues: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_Count: Integer; safecall;
    function Get_Idx: Integer; safecall;
    procedure Set_Idx(Value: Integer); safecall;
    function Get_Ag: double; safecall;
    procedure Set_Ag(Value: double); safecall;
    function Get_Cp: double; safecall;
    procedure Set_Cp(Value: double); safecall;
    function Get_kV: Double; safecall;
    procedure Set_kV(Value: Double); safecall;
    function Get_kVA: Double; safecall;
    function Get_kvar: Double; safecall;
    function Get_kW: Double; safecall;
    procedure Set_kVA(Value: Double); safecall;
    procedure Set_kvar(Value: Double); safecall;
    procedure Set_kW(Value: Double); safecall;
    function Get_Lamda: Double; safecall;
    function Get_N_WTG: Integer; safecall;
    function Get_NPoles: Integer; safecall;
    function Get_pd: Double; safecall;
    function Get_PF: Double; safecall;
    function Get_PSS: Double; safecall;
    function Get_QFlag: Integer; safecall;
    function Get_QMode: Integer; safecall;
    function Get_QSS: Double; safecall;
    function Get_Rad: Double; safecall;
    function Get_RThev: Double; safecall;
    function Get_VCoutOut: Double; safecall;
    function Get_VCutIn: Double; safecall;
    function Get_Vss: Double; safecall;
    function Get_WindSpeed: Double; safecall;
    function Get_XThev: Double; safecall;
    procedure Set_Lamda(Value: Double); safecall;
    procedure Set_N_WTG(Value: Integer); safecall;
    procedure Set_NPoles(Value: Integer); safecall;
    procedure Set_pd(Value: Double); safecall;
    procedure Set_PF(Value: Double); safecall;
    procedure Set_PSS(Value: Double); safecall;
    procedure Set_QFlag(Value: Integer); safecall;
    procedure Set_QMode(Value: Integer); safecall;
    procedure Set_QSS(Value: Double); safecall;
    procedure Set_Rad(Value: Double); safecall;
    procedure Set_RThev(Value: Double); safecall;
    procedure Set_VCoutOut(Value: Double); safecall;
    procedure Set_VCutIn(Value: Double); safecall;
    procedure Set_Vss(Value: Double); safecall;
    procedure Set_WindSpeed(Value: Double); safecall;
    procedure Set_XThev(Value: Double); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
  end;

implementation

uses ComServ, DSSGlobals, WindGen, Variants, Pointerlist, Sysutils;

function TWindGens.Get_AllNames: OleVariant;
Var
  WindGenElem:  TWindGenObj;
  pList:        TPointerList;
  k:            Integer;

Begin
  Result := VarArrayCreate([0, 0], varOleStr);
  Result[0] := 'NONE';
  IF ActiveCircuit[ActiveActor] <> Nil THEN
  Begin
    WITH ActiveCircuit[ActiveActor] DO
    Begin

      If WindGenClass[ActiveActor].ElementList.ListSize > 0 Then
      Begin
        pList := WindGenClass[ActiveActor].ElementList;
        VarArrayRedim(result, pList.ListSize - 1);
        k:=0;
        WindGenElem := pList.First;
        WHILE WindGenElem<>Nil DO
        Begin
          Result[k] := WindGenElem.Name;
          Inc(k);
          WindGenElem := pList.Next;
        End;
      End;

    End;
  End;

end;

function TWindGens.Get_RegisterNames: OleVariant;
Var
    k :integer;

Begin

  Result := VarArrayCreate([0, NumWGenRegisters - 1], varOleStr);
  For k := 0 to  NumWGenRegisters - 1  Do
  Begin
     Result[k] := WindGenClass[ActiveActor].RegisterNames[k + 1];
  End;

end;

function TWindGens.Get_RegisterValues: OleVariant;
Var
  WindGenElem:  TWindGenObj;
  k     :Integer;
Begin
  IF ActiveCircuit[ActiveActor] <> Nil THEN
  Begin

    WindGenElem :=  TWindGenObj(ActiveCircuit[ActiveActor].ActiveCktElement);
    If WindGenElem <> Nil Then
    Begin
      Result := VarArrayCreate([0, NumWGenRegisters-1], varDouble);
      FOR k := 0 to NumWGenRegisters-1 DO
      Begin
          Result[k] := WindGenElem.Registers[k+1];
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

function TWindGens.Get_First: Integer;
Var
  WindGenElem:  TWindGenObj;
  pList:        TPointerList;

Begin

   Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin

        If WindGenClass[ActiveActor].ElementList.ListSize > 0 Then
        Begin
          pList := WindGenClass[ActiveActor].ElementList;
          WindGenElem := pList.First;
          Repeat
            If WindGenElem.Enabled
            Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;
              Result := 1;
            End
            Else WindGenElem := pList.Next;
          Until (Result = 1) or (WindGenElem = nil);
        End
        Else
            Result := 0;  // signify no more

   End;

end;

function TWindGens.Get_Next: Integer;
Var
  WindGenElem:  TWindGenObj;
  pList:        TPointerList;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin

    If WindGenClass[ActiveActor].ElementList.ListSize > 0 Then
    Begin
      pList := WindGenClass[ActiveActor].ElementList;
      WindGenElem := pList.First;
      Repeat
        If WindGenElem.Enabled
        Then
        Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;
          Result := pList.ActiveIndex;
        End
        Else WindGenElem := pList.Next;
      Until (Result > 0) or (WindGenElem = nil);
    End
    Else
      Result := 0;  // signify no more

  End;
end;

function TWindGens.Get_Count: Integer;
begin
  If Assigned(ActiveCircuit[ActiveActor]) Then
    Result := WindGenClass[ActiveActor].ElementList.ListSize;
end;

function TWindGens.Get_Idx: Integer;
begin
    if ActiveCircuit[ActiveActor] <> Nil then
       Result := WindGenClass[ActiveActor].ElementList.ActiveIndex
    else Result := 0;
end;

procedure TWindGens.Set_Idx(Value: Integer);
Var
  WindGenElem:  TWindGenObj;

begin
  if ActiveCircuit[ActiveActor] <> Nil then
  Begin

    WindGenElem := WindGenClass[ActiveActor].ElementList.Get(Value);
    If WindGenElem <> Nil Then ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;

  End;

end;

function TWindGens.Get_Ag: double; safecall;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0.0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindGenVars.ag
  End;

end;

procedure TWindGens.Set_Ag(Value: double); safecall;
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindGenVars.ag  := Value
  End;

end;

function TWindGens.Get_Cp: double; safecall;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0.0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindGenVars.Cp
  End;

end;

procedure TWindGens.Set_Cp(Value: double); safecall;
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindGenVars.Cp := Value
  End;

end;

function TWindGens.Get_kV: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0.0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.PresentkV
  End;

end;

procedure TWindGens.Set_kV(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.PresentkV := Value
  End;

end;

function TWindGens.Get_kVA: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0.0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindGenVars.kVArating
  End;

end;

function TWindGens.Get_kvar: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0.0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.Presentkvar
  End;

end;

function TWindGens.Get_kW: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0.0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.PresentkW
  End;

end;

procedure TWindGens.Set_kVA(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindGenVars.kVArating := Value;
    WindGenElem.WindModelDyn.EditProp(13,floattostr(Value));
  End;

end;

procedure TWindGens.Set_kvar(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.Presentkvar := Value
  End;

end;

procedure TWindGens.Set_kW(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.PresentkW := Value
  End;

end;

function TWindGens.Get_Lamda: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0.0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindGenVars.Lamda
  End;

end;

function TWindGens.Get_N_WTG: Integer;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindModelDyn.N_WTG
  End;

end;

function TWindGens.Get_NPoles: Integer;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := Trunc(WindGenElem.WindGenVars.Poles)
  End;

end;

function TWindGens.Get_pd: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindGenVars.pd
  End;

end;

function TWindGens.Get_PF: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.PFNominal
  End;

end;

function TWindGens.Get_PSS: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindModelDyn.Pss
  End;

end;

function TWindGens.Get_QFlag: Integer;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindModelDyn.QFlg
  End;

end;

function TWindGens.Get_QMode: Integer;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindModelDyn.QMode
  End;

end;

function TWindGens.Get_QSS: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindModelDyn.Qss
  End;

end;

function TWindGens.Get_Rad: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindGenVars.Rad
  End;

end;

function TWindGens.Get_RThev: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindModelDyn.Rthev
  End;

end;

function TWindGens.Get_VCoutOut: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindGenVars.VCutout
  End;

end;

function TWindGens.Get_VCutIn: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindGenVars.VCutin
  End;

end;

function TWindGens.Get_Vss: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindModelDyn.Vss
  End;

end;

function TWindGens.Get_WindSpeed: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindModelDyn.vwind
  End;

end;

function TWindGens.Get_XThev: Double;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := 0;
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.WindModelDyn.Xthev
  End;

end;

procedure TWindGens.Set_Lamda(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindGenVars.Lamda := Value
  End;

end;

procedure TWindGens.Set_N_WTG(Value: Integer);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindModelDyn.N_WTG := Value
  End;

end;

procedure TWindGens.Set_NPoles(Value: Integer);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindGenVars.Poles := Value
  End;

end;

procedure TWindGens.Set_pd(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindGenVars.pd  := Value
  End;

end;

procedure TWindGens.Set_PF(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.PFNominal := Value
  End;

end;


procedure TWindGens.Set_PSS(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindModelDyn.Pss := Value
  End;

end;

procedure TWindGens.Set_QFlag(Value: Integer);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindModelDyn.QFlg := Value
  End;

end;

procedure TWindGens.Set_QMode(Value: Integer);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindModelDyn.QMode := Value
  End;

end;

procedure TWindGens.Set_QSS(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindModelDyn.Qss := Value
  End;

end;

procedure TWindGens.Set_Rad(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindGenVars.Rad := Value
  End;

end;

procedure TWindGens.Set_RThev(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindModelDyn.Rthev := Value
  End;

end;

procedure TWindGens.Set_VCoutOut(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindGenVars.VCutout := Value
  End;

end;

procedure TWindGens.Set_VCutIn(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindGenVars.VCutin := Value
  End;

end;

procedure TWindGens.Set_Vss(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindModelDyn.Vss := Value
  End;

end;

procedure TWindGens.Set_WindSpeed(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindModelDyn.vwind := Value
  End;

end;

procedure TWindGens.Set_XThev(Value: Double);
Var
  WindGenElem:  TWindGenObj;

begin
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    WindGenElem.WindModelDyn.Xthev := Value
  End;

end;

function TWindGens.Get_Name: WideString;
Var
  WindGenElem:  TWindGenObj;

begin
  Result := '';
  WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
  if WindGenElem <> nil then
  Begin
    Result := WindGenElem.Name
  End;

end;

procedure TWindGens.Set_Name(const Value: WideString);
VAR
    activesave  :integer;
    WindgenElem :TWindGenObj;
    S           : String;
    Found       :Boolean;
    pList       :TPointerList;
Begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN
  Begin      // Search list of Storages in active circuit for name
    If WindGenClass[ActiveActor].ElementList.ListSize > 0 Then
    Begin
      S := Value;  // Convert to Pascal String
      Found := FALSE;
      pList := WindGenClass[ActiveActor].ElementList;
      activesave :=  pList.ActiveIndex;
      WindGenElem := pList.First;
      While WindGenElem <> NIL Do
      Begin
        IF (CompareText(WindGenElem.Name, S) = 0)
        THEN
        Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;
          Found := TRUE;
          Break;
        End;
        WindGenElem := pList.Next;
      End;
      IF NOT Found
      THEN
      Begin
        DoSimpleMsg('WindGen "'+S+'" Not Found in Active Circuit.', 20003);
        WindGenElem := pList.Get(activesave);    // Restore active Storage
        ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;
      End;
    End;
  End;

end;

initialization
  TAutoObjectFactory.Create(ComServer, TWindGens, CLASS_WindGens,
    ciInternal, tmApartment);
end.
