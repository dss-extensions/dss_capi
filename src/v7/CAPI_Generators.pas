UNIT CAPI_Generators;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE Generators_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
function Generators_Get_First():Integer;cdecl;
function Generators_Get_Name():PAnsiChar;cdecl;
function Generators_Get_Next():Integer;cdecl;
PROCEDURE Generators_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Generators_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
function Generators_Get_ForcedON():WordBool;cdecl;
procedure Generators_Set_ForcedON(Value: WordBool);cdecl;
procedure Generators_Set_Name(const Value: PAnsiChar);cdecl;
function Generators_Get_kV():Double;cdecl;
function Generators_Get_kvar():Double;cdecl;
function Generators_Get_kW():Double;cdecl;
function Generators_Get_PF():Double;cdecl;
function Generators_Get_Phases():Integer;cdecl;
procedure Generators_Set_kV(Value: Double);cdecl;
procedure Generators_Set_kvar(Value: Double);cdecl;
procedure Generators_Set_kW(Value: Double);cdecl;
procedure Generators_Set_PF(Value: Double);cdecl;
procedure Generators_Set_Phases(Value: Integer);cdecl;
function Generators_Get_Count():Integer;cdecl;
function Generators_Get_idx():Integer;cdecl;
procedure Generators_Set_idx(Value: Integer);cdecl;
function Generators_Get_Model():Integer;cdecl;
procedure Generators_Set_Model(Value: Integer);cdecl;
function Generators_Get_kVArated():Double;cdecl;
procedure Generators_Set_kVArated(Value: Double);cdecl;
function Generators_Get_Vmaxpu():Double;cdecl;
function Generators_Get_Vminpu():Double;cdecl;
procedure Generators_Set_Vmaxpu(Value: Double);cdecl;
procedure Generators_Set_Vminpu(Value: Double);cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSCLassDefs, DSSGlobals, Generator, CktElement, SysUtils;

PROCEDURE Generators_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  GenElem:TGeneratorObj;
  k:Integer;

Begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     If Generators.ListSize>0 Then
     Begin
       DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (Generators.ListSize-1) + 1);
       k:=0;
       GenElem := Generators.First;
       WHILE GenElem<>Nil DO  Begin
          Result[k] := DSS_CopyStringAsPChar(GenElem.Name);
          Inc(k);
          GenElem := Generators.Next;
       End;
     End;
end;
//------------------------------------------------------------------------------
function Generators_Get_First():Integer;cdecl;
Var
   pGen:TGeneratorObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pGen := ActiveCircuit.Generators.First;
        If pGen <> Nil Then
        Begin
          Repeat
            If pGen.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pGen;
              Result := 1;
            End
            Else pGen := ActiveCircuit.Generators.Next;
          Until (Result = 1) or (pGen = nil);
        End
        Else
            Result := 0;  // signify no more
   End;
end;
//------------------------------------------------------------------------------
function Generators_Get_Name_AnsiString():AnsiString;inline;
Var
   pGen:TGeneratorObj;

Begin
   Result := '';
   If ActiveCircuit <> Nil Then
   Begin
        pGen := ActiveCircuit.Generators.Active;
        If pGen <> Nil Then
        Begin
          Result := pGen.Name;
        End
        Else
            Result := '';  // signify no name
   End;

end;

function Generators_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Generators_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Generators_Get_Next():Integer;cdecl;
Var
   pGen:TGeneratorObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pGen := ActiveCircuit.Generators.Next;
        If pGen <> Nil Then
        Begin
          Repeat
            If pGen.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pGen;
              Result := ActiveCircuit.Generators.ActiveIndex;
            End
            Else pGen := ActiveCircuit.Generators.Next;
          Until (Result > 0) or (pGen = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;
//------------------------------------------------------------------------------
PROCEDURE Generators_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
    GeneratorClass:TGenerator;
    k :integer;

Begin
    GeneratorClass := DssClassList.Get(Classnames.Find('Generator'));
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumGenRegisters - 1) + 1);
    For k := 0 to  NumGenRegisters - 1  Do Begin
       Result[k] := DSS_CopyStringAsPChar(GeneratorClass.RegisterNames[k + 1]);
    End;

end;
//------------------------------------------------------------------------------
PROCEDURE Generators_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
   Gen :TGeneratorObj;
   k     :Integer;
Begin

   IF ActiveCircuit <> Nil THEN
   Begin
        Gen :=  TGeneratorObj(ActiveCircuit.Generators.Active);
        If Gen <> Nil Then
        Begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (numGenRegisters-1) + 1);
            FOR k := 0 to numGenRegisters-1 DO
            Begin
                Result[k] := Gen.Registers[k+1];
            End;
        End
        Else
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
   End
   ELSE Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
   End;


end;
//------------------------------------------------------------------------------
function Generators_Get_ForcedON():WordBool;cdecl;
begin
     Result := FALSE;
     IF ActiveCircuit<> NIL
     THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0
             THEN Begin
                 Result := TGeneratorObj(Active).ForcedON;
             End;
         End;
     End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_ForcedON(Value: WordBool);cdecl;
begin

     IF ActiveCircuit<> NIL
     THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0
             THEN Begin
                 TGeneratorObj(Active).ForcedON := Value;
             End;
         End;
     End;

end;
//------------------------------------------------------------------------------
procedure Generators_Set_Name(const Value: PAnsiChar);cdecl;
VAR
    activesave :integer;
    Gen:TGeneratorObj;
    S: String;
    Found :Boolean;
Begin


  IF ActiveCircuit <> NIL
  THEN Begin      // Search list of generators in active circuit for name
       WITH ActiveCircuit.Generators DO
         Begin
             S := Value;  // Convert to Pascal String
             Found := FALSE;
             ActiveSave := ActiveIndex;
             Gen := First;
             While Gen <> NIL Do
             Begin
                IF (CompareText(Gen.Name, S) = 0)
                THEN Begin
                    ActiveCircuit.ActiveCktElement := Gen;
                    Found := TRUE;
                    Break;
                End;
                Gen := Next;
             End;
             IF NOT Found
             THEN Begin
                 DoSimpleMsg('Generator "'+S+'" Not Found in Active Circuit.', 5003);
                 Gen := Get(ActiveSave);    // Restore active generator
                 ActiveCircuit.ActiveCktElement := Gen;
             End;
         End;
  End;
end;
//------------------------------------------------------------------------------
function Generators_Get_kV():Double;cdecl;
begin
   Result := -1.0;  // not set
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).GenVars.kVGeneratorBase;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
function Generators_Get_kvar():Double;cdecl;
begin
  Result := 0.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).Presentkvar;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
function Generators_Get_kW():Double;cdecl;
begin
   Result := 0.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).PresentkW;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
function Generators_Get_PF():Double;cdecl;
begin
   Result := 0.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).PowerFactor;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
function Generators_Get_Phases():Integer;cdecl;
begin
   Result := 0;  // not set
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).nphases;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_kV(Value: Double);cdecl;
begin

   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).PresentkV := Value;
             End;
         End;
   End;

end;
//------------------------------------------------------------------------------
procedure Generators_Set_kvar(Value: Double);cdecl;
begin
    IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).Presentkvar := Value;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_kW(Value: Double);cdecl;
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).PresentkW := Value;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_PF(Value: Double);cdecl;
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).PowerFactor := Value;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Phases(Value: Integer);cdecl;
begin
   IF ActiveCircuit<> NIL THEN Begin
       WITH ActiveCircuit.Generators Do Begin
           IF ActiveIndex<>0 THEN Begin
                TGeneratorObj(Active).Nphases := Value;
           End;
       End;
   End;
end;
//------------------------------------------------------------------------------
function Generators_Get_Count():Integer;cdecl;
begin
    If Assigned(Activecircuit) Then
          Result := ActiveCircuit.Generators.ListSize;
end;
//------------------------------------------------------------------------------
function Generators_Get_idx():Integer;cdecl;
begin
    if ActiveCircuit <> Nil then
       Result := ActiveCircuit.Generators.ActiveIndex
    else Result := 0;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_idx(Value: Integer);cdecl;
Var
    pGen:TGeneratorObj;
begin
    if ActiveCircuit <> Nil then   Begin
        pGen := ActiveCircuit.Generators.Get(Value);
        If pGen <> Nil Then ActiveCircuit.ActiveCktElement := pGen;
    End;

end;
//------------------------------------------------------------------------------
function Generators_Get_Model():Integer;cdecl;
begin
   Result := -1;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).GenModel ;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Model(Value: Integer);cdecl;
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  With TGeneratorObj(Active) Do Begin
                     GenModel := Value;
                     // Handle side effect
                     If GenModel=3 Then ActiveCircuit.Solution.SolutionInitialized := FALSE ;
                  End;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
function Generators_Get_kVArated():Double;cdecl;
begin
   Result := -1.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).Genvars.kVArating  ;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_kVArated(Value: Double);cdecl;
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  With TGeneratorObj(Active) Do Begin
                     Genvars.kVArating  := Value;
                  End;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
function Generators_Get_Vmaxpu():Double;cdecl;
begin
   Result := -1.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).Vmaxpu ;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
function Generators_Get_Vminpu():Double;cdecl;
begin
   Result := -1.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).Vminpu ;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Vmaxpu(Value: Double);cdecl;
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  With TGeneratorObj(Active) Do Begin
                     VMaxPu  := Value;
                  End;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Vminpu(Value: Double);cdecl;
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  With TGeneratorObj(Active) Do Begin
                     VMinPu  := Value;
                  End;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
END.
