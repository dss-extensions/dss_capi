UNIT CAPI_PVSystems;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE PVSystems_Get_AllNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
PROCEDURE PVSystems_Get_RegisterNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
PROCEDURE PVSystems_Get_RegisterValues(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
function PVSystems_Get_First():Integer;cdecl;
function PVSystems_Get_Next():Integer;cdecl;
function PVSystems_Get_Count():Integer;cdecl;
function PVSystems_Get_idx():Integer;cdecl;
procedure PVSystems_Set_idx(Value: Integer);cdecl;
function PVSystems_Get_Name():PAnsiChar;cdecl;
procedure PVSystems_Set_Name(const Value: PAnsiChar);cdecl;
function PVSystems_Get_Irradiance():Double;cdecl;
procedure PVSystems_Set_Irradiance(Value: Double);cdecl;
function PVSystems_Get_kvar():Double;cdecl;
function PVSystems_Get_kVArated():Double;cdecl;
function PVSystems_Get_kW():Double;cdecl;
function PVSystems_Get_PF():Double;cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, PVSystem, SysUtils;

PROCEDURE PVSystems_Get_AllNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
VAR
  Result: PPAnsiCharArray;
  PVSystemElem:TPVSystemObj;
  k:Integer;

Begin
    Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     If PVSystems.ListSize>0 Then
     Begin
       DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (PVSystems.ListSize-1) + 1);
       k:=0;
       PVSystemElem := PVSystems.First;
       WHILE PVSystemElem<>Nil DO  Begin
          Result[k] := DSS_CopyStringAsPChar(PVSystemElem.Name);
          Inc(k);
          PVSystemElem := PVSystems.Next;
       End;
     End;
end;
//------------------------------------------------------------------------------
PROCEDURE PVSystems_Get_RegisterNames(var ResultPtr: PPAnsiChar; var ResultCount: Integer);cdecl;
VAR
  Result: PPAnsiCharArray;
    k :integer;

Begin
    Result := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumPVSystemRegisters - 1) + 1);
    For k := 0 to  NumPVSystemRegisters - 1  Do Begin
       Result[k] := DSS_CopyStringAsPChar(PVSystemClass.RegisterNames[k + 1]);
    End;
end;
//------------------------------------------------------------------------------
PROCEDURE PVSystems_Get_RegisterValues(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   PVSystem :TPVSystemObj;
   k     :Integer;
Begin

   IF ActiveCircuit <> Nil THEN
   Begin
        PVSystem :=  TPVSystemObj(ActiveCircuit.PVSystems.Active);
        If PVSystem <> Nil Then
        Begin
            Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (numPVSystemRegisters-1) + 1);
            FOR k := 0 to numPVSystemRegisters-1 DO
            Begin
                Result[k] := PVSystem.Registers[k+1];
            End;
        End
        Else
            Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
   End
   ELSE Begin
        Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
   End;



end;
//------------------------------------------------------------------------------
function PVSystems_Get_First():Integer;cdecl;
Var
   pPVSystem:TpVSystemObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pPVSystem := ActiveCircuit.pVSystems.First;
        If pPVSystem <> Nil Then
        Begin
          Repeat
            If pPVSystem.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pPVSystem;
              Result := 1;
            End
            Else pPVSystem := ActiveCircuit.pVSystems.Next;
          Until (Result = 1) or (pPVSystem = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;
//------------------------------------------------------------------------------
function PVSystems_Get_Next():Integer;cdecl;
Var
   pPVSystem:TPVSystemObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pPVSystem := ActiveCircuit.PVSystems.Next;
        If pPVSystem <> Nil Then
        Begin
          Repeat
            If pPVSystem.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pPVSystem;
              Result := ActiveCircuit.PVSystems.ActiveIndex;
            End
            Else pPVSystem := ActiveCircuit.PVSystems.Next;
          Until (Result > 0) or (pPVSystem = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;
//------------------------------------------------------------------------------
function PVSystems_Get_Count():Integer;cdecl;
begin
    If Assigned(Activecircuit) Then
          Result := ActiveCircuit.PVSystems.ListSize;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_idx():Integer;cdecl;
begin
    if ActiveCircuit <> Nil then
       Result := ActiveCircuit.PVSystems.ActiveIndex
    else Result := 0;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_idx(Value: Integer);cdecl;
Var
    pPVSystem:TPVSystemObj;
begin
    if ActiveCircuit <> Nil then   Begin
        pPVSystem := ActiveCircuit.PVSystems.Get(Value);
        If pPVSystem <> Nil Then ActiveCircuit.ActiveCktElement := pPVSystem;
    End;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Name_AnsiString():AnsiString;inline;
Var
   pPVSystem:TPVSystemObj;

Begin
   Result := '';
   If ActiveCircuit <> Nil Then
   Begin
        pPVSystem := ActiveCircuit.PVSystems.Active;
        If pPVSystem <> Nil Then
        Begin
          Result := pPVSystem.Name;
        End
        Else
            Result := '';  // signify no name
   End;

end;

function PVSystems_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(PVSystems_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Name(const Value: PAnsiChar);cdecl;
VAR
    activesave :integer;
    PVSystem:TPVSystemObj;
    S: String;
    Found :Boolean;
Begin


  IF ActiveCircuit <> NIL
  THEN Begin      // Search list of PVSystems in active circuit for name
       WITH ActiveCircuit.PVSystems DO
         Begin
             S := Value;  // Convert to Pascal String
             Found := FALSE;
             ActiveSave := ActiveIndex;
             PVSystem := First;
             While PVSystem <> NIL Do
             Begin
                IF (CompareText(PVSystem.Name, S) = 0)
                THEN Begin
                    ActiveCircuit.ActiveCktElement := PVSystem;
                    Found := TRUE;
                    Break;
                End;
                PVSystem := Next;
             End;
             IF NOT Found
             THEN Begin
                 DoSimpleMsg('PVSystem "'+S+'" Not Found in Active Circuit.', 5003);
                 PVSystem := Get(ActiveSave);    // Restore active PVSystem
                 ActiveCircuit.ActiveCktElement := PVSystem;
             End;
         End;
  End;

end;
//------------------------------------------------------------------------------
function PVSystems_Get_Irradiance():Double;cdecl;
begin
   Result := -1.0;  // not set
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TPVSystemObj(Active).PVSystemVars.FIrradiance;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Irradiance(Value: Double);cdecl;
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TPVSystemObj(Active).PVSystemVars.FIrradiance  := Value;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_kvar():Double;cdecl;
begin
   Result := 0.0;  // not set
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TPVSystemObj(Active).Presentkvar;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_kVArated():Double;cdecl;
begin
   Result := -1.0;  // not set
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TPVSystemObj(Active).kVARating ;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_kW():Double;cdecl;
begin
   Result := 0.0;  // not set
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TPVSystemObj(Active).PresentkW;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_PF():Double;cdecl;
begin
   Result := 0.0;  // not set
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.PVSystems Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TPVSystemObj(Active).PowerFactor ;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
END.
