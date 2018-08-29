UNIT CAPI_Generators;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE Generators_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Generators_Get_AllNames_GR();cdecl;
function Generators_Get_First():Integer;cdecl;
function Generators_Get_Name():PAnsiChar;cdecl;
function Generators_Get_Next():Integer;cdecl;
PROCEDURE Generators_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Generators_Get_RegisterNames_GR();cdecl;
PROCEDURE Generators_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Generators_Get_RegisterValues_GR();cdecl;
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
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
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
PROCEDURE Generators_Get_AllNames_GR();cdecl;
// Same as Generators_Get_AllNames but uses global result (GR) pointers
begin
   Generators_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Generators_Get_First():Integer;cdecl;
Var
   pGen:TGeneratorObj;

Begin

   Result := 0;
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pGen := ActiveCircuit[ActiveActor].Generators.First;
        If pGen <> Nil Then
        Begin
          Repeat
            If pGen.Enabled
            Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pGen;
              Result := 1;
            End
            Else pGen := ActiveCircuit[ActiveActor].Generators.Next;
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
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pGen := ActiveCircuit[ActiveActor].Generators.Active;
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
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pGen := ActiveCircuit[ActiveActor].Generators.Next;
        If pGen <> Nil Then
        Begin
          Repeat
            If pGen.Enabled
            Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pGen;
              Result := ActiveCircuit[ActiveActor].Generators.ActiveIndex;
            End
            Else pGen := ActiveCircuit[ActiveActor].Generators.Next;
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
    GeneratorClass := DssClassList[ActiveActor].Get(Classnames[ActiveActor].Find('Generator'));
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumGenRegisters - 1) + 1);
    For k := 0 to  NumGenRegisters - 1  Do Begin
       Result[k] := DSS_CopyStringAsPChar(GeneratorClass.RegisterNames[k + 1]);
    End;

end;
PROCEDURE Generators_Get_RegisterNames_GR();cdecl;
// Same as Generators_Get_RegisterNames but uses global result (GR) pointers
begin
   Generators_Get_RegisterNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
PROCEDURE Generators_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
   Gen :TGeneratorObj;
   k     :Integer;
Begin

   IF ActiveCircuit[ActiveActor] <> Nil THEN
   Begin
        Gen :=  TGeneratorObj(ActiveCircuit[ActiveActor].Generators.Active);
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
PROCEDURE Generators_Get_RegisterValues_GR();cdecl;
// Same as Generators_Get_RegisterValues but uses global result (GR) pointers
begin
   Generators_Get_RegisterValues(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Generators_Get_ForcedON():WordBool;cdecl;
begin
     Result := FALSE;
     IF ActiveCircuit[ActiveActor]<> NIL
     THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
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

     IF ActiveCircuit[ActiveActor]<> NIL
     THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
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


  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin      // Search list of generators in active circuit for name
       WITH ActiveCircuit[ActiveActor].Generators DO
         Begin
             S := Value;  // Convert to Pascal String
             Found := FALSE;
             ActiveSave := ActiveIndex;
             Gen := First;
             While Gen <> NIL Do
             Begin
                IF (CompareText(Gen.Name, S) = 0)
                THEN Begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := Gen;
                    Found := TRUE;
                    Break;
                End;
                Gen := Next;
             End;
             IF NOT Found
             THEN Begin
                 DoSimpleMsg('Generator "'+S+'" Not Found in Active Circuit.', 5003);
                 Gen := Get(ActiveSave);    // Restore active generator
                 ActiveCircuit[ActiveActor].ActiveCktElement := Gen;
             End;
         End;
  End;
end;
//------------------------------------------------------------------------------
function Generators_Get_kV():Double;cdecl;
begin
   Result := -1.0;  // not set
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
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
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
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
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
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
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
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
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).nphases;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_kV(Value: Double);cdecl;
begin

   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).PresentkV := Value;
             End;
         End;
   End;

end;
//------------------------------------------------------------------------------
procedure Generators_Set_kvar(Value: Double);cdecl;
begin
    IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).Presentkvar := Value;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_kW(Value: Double);cdecl;
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).PresentkW := Value;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_PF(Value: Double);cdecl;
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TGeneratorObj(Active).PowerFactor := Value;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Phases(Value: Integer);cdecl;
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
       WITH ActiveCircuit[ActiveActor].Generators Do Begin
           IF ActiveIndex<>0 THEN Begin
                TGeneratorObj(Active).Nphases := Value;
           End;
       End;
   End;
end;
//------------------------------------------------------------------------------
function Generators_Get_Count():Integer;cdecl;
begin
    If Assigned(ActiveCircuit[ActiveActor]) Then
          Result := ActiveCircuit[ActiveActor].Generators.ListSize;
end;
//------------------------------------------------------------------------------
function Generators_Get_idx():Integer;cdecl;
begin
    if ActiveCircuit[ActiveActor] <> Nil then
       Result := ActiveCircuit[ActiveActor].Generators.ActiveIndex
    else Result := 0;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_idx(Value: Integer);cdecl;
Var
    pGen:TGeneratorObj;
begin
    if ActiveCircuit[ActiveActor] <> Nil then   Begin
        pGen := ActiveCircuit[ActiveActor].Generators.Get(Value);
        If pGen <> Nil Then ActiveCircuit[ActiveActor].ActiveCktElement := pGen;
    End;

end;
//------------------------------------------------------------------------------
function Generators_Get_Model():Integer;cdecl;
begin
   Result := -1;
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).GenModel ;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Model(Value: Integer);cdecl;
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                  With TGeneratorObj(Active) Do Begin
                     GenModel := Value;
                     // Handle side effect
                     If GenModel=3 Then ActiveCircuit[ActiveActor].Solution.SolutionInitialized := FALSE ;
                  End;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
function Generators_Get_kVArated():Double;cdecl;
begin
   Result := -1.0;
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).Genvars.kVArating  ;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_kVArated(Value: Double);cdecl;
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
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
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
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
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TGeneratorObj(Active).Vminpu ;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Generators_Set_Vmaxpu(Value: Double);cdecl;
begin
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
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
   IF ActiveCircuit[ActiveActor]<> NIL THEN Begin
         WITH ActiveCircuit[ActiveActor].Generators Do Begin
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
