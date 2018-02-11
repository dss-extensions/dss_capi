UNIT CAPI_Settings;
{$inline on}

INTERFACE

USES CAPI_Utils;

function Settings_Get_AllowDuplicates():WordBool;cdecl;
function Settings_Get_AutoBusList():PAnsiChar;cdecl;
function Settings_Get_CktModel():Integer;cdecl;
function Settings_Get_EmergVmaxpu():Double;cdecl;
function Settings_Get_EmergVminpu():Double;cdecl;
function Settings_Get_NormVmaxpu():Double;cdecl;
function Settings_Get_NormVminpu():Double;cdecl;
function Settings_Get_ZoneLock():WordBool;cdecl;
procedure Settings_Set_AllocationFactors(Value: Double);cdecl;
procedure Settings_Set_AllowDuplicates(Value: WordBool);cdecl;
procedure Settings_Set_AutoBusList(const Value: PAnsiChar);cdecl;
procedure Settings_Set_CktModel(Value: Integer);cdecl;
procedure Settings_Set_EmergVmaxpu(Value: Double);cdecl;
procedure Settings_Set_EmergVminpu(Value: Double);cdecl;
procedure Settings_Set_NormVmaxpu(Value: Double);cdecl;
procedure Settings_Set_NormVminpu(Value: Double);cdecl;
procedure Settings_Set_ZoneLock(Value: WordBool);cdecl;
PROCEDURE Settings_Get_LossRegs(var ResultPtr: PInteger; var ResultCount: Integer);cdecl;
function Settings_Get_LossWeight():Double;cdecl;
function Settings_Get_Trapezoidal():WordBool;cdecl;
PROCEDURE Settings_Get_UEregs(var ResultPtr: PInteger; var ResultCount: Integer);cdecl;
function Settings_Get_UEweight():Double;cdecl;
procedure Settings_Set_LossRegs(ValuePtr: PInteger; ValueCount: Integer);cdecl;
procedure Settings_Set_LossWeight(Value: Double);cdecl;
procedure Settings_Set_Trapezoidal(Value: WordBool);cdecl;
procedure Settings_Set_UEregs(ValuePtr: PInteger; ValueCount: Integer);cdecl;
procedure Settings_Set_UEweight(Value: Double);cdecl;
function Settings_Get_ControlTrace():WordBool;cdecl;
PROCEDURE Settings_Get_VoltageBases(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
procedure Settings_Set_ControlTrace(Value: WordBool);cdecl;
procedure Settings_Set_VoltageBases(ValuePtr: PDouble; ValueCount: Integer);cdecl;
function Settings_Get_PriceCurve():PAnsiChar;cdecl;
function Settings_Get_PriceSignal():Double;cdecl;
procedure Settings_Set_PriceCurve(const Value: PAnsiChar);cdecl;
procedure Settings_Set_PriceSignal(Value: Double);cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, ExecHelper;

function Settings_Get_AllowDuplicates():WordBool;cdecl;
begin

    IF ActiveCircuit[ActiveActor] <> NIL THEN

      Result := ActiveCircuit[ActiveActor].DuplicatesAllowed

    ELSE Result := FALSE;

end;
//------------------------------------------------------------------------------
function Settings_Get_AutoBusList_AnsiString():AnsiString;inline;
VAR
   i:Integer;
begin
    IF ActiveCircuit[ActiveActor] <> NIL THEN
     WITH ActiveCircuit[ActiveActor].AutoAddBusList Do
     Begin
       FOR i := 1 to ListSize Do AppendGlobalResult(Get(i));
       Result := GlobalResult;
     End
    ELSE Result := '';

end;

function Settings_Get_AutoBusList():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Settings_Get_AutoBusList_AnsiString());
end;
//------------------------------------------------------------------------------
function Settings_Get_CktModel():Integer;cdecl;
begin

    IF ActiveCircuit[ActiveActor] <> NIL THEN  Begin

      If ActiveCircuit[ActiveActor].PositiveSequence
      THEN  Result := dssPositiveSeq
      ELSE  Result := dssMultiPhase;
    End
    ELSE Result := 0;

end;
//------------------------------------------------------------------------------
function Settings_Get_EmergVmaxpu():Double;cdecl;
begin
     IF   ActiveCircuit[ActiveActor] <> NIL
     THEN Result := ActiveCircuit[ActiveActor].EmergMaxVolts
     ELSE Result := 0.0;;

end;
//------------------------------------------------------------------------------
function Settings_Get_EmergVminpu():Double;cdecl;
begin
     IF   ActiveCircuit[ActiveActor] <> NIL
     THEN Result := ActiveCircuit[ActiveActor].EmergMinVolts
     ELSE Result := 0.0;;

end;
//------------------------------------------------------------------------------
function Settings_Get_NormVmaxpu():Double;cdecl;
begin
     IF   ActiveCircuit[ActiveActor] <> NIL
     THEN Result := ActiveCircuit[ActiveActor].NormalMaxVolts
     ELSE Result := 0.0;;


end;
//------------------------------------------------------------------------------
function Settings_Get_NormVminpu():Double;cdecl;
begin
     IF   ActiveCircuit[ActiveActor] <> NIL
     THEN Result := ActiveCircuit[ActiveActor].NormalMinVolts
     ELSE Result := 0.0;;
end;
//------------------------------------------------------------------------------
function Settings_Get_ZoneLock():WordBool;cdecl;
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
       Result := ActiveCircuit[ActiveActor].ZonesLocked ;
   END
   ELSE    Result := FALSE;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AllocationFactors(Value: Double);cdecl;
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN   DoSetAllocationFactors(Value);
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AllowDuplicates(Value: WordBool);cdecl;
begin
    IF ActiveCircuit[ActiveActor] <> NIL THEN
      ActiveCircuit[ActiveActor].DuplicatesAllowed := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_AutoBusList(const Value: PAnsiChar);cdecl;
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN DoAutoAddBusList(Value);
end;
//------------------------------------------------------------------------------
procedure Settings_Set_CktModel(Value: Integer);cdecl;
begin

    IF ActiveCircuit[ActiveActor] <> NIL THEN

      CASE Value of
           dssPositiveSeq : ActiveCircuit[ActiveActor].PositiveSequence:= TRUE;
      ELSE
           ActiveCircuit[ActiveActor].PositiveSequence:= FALSE;
      END;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_EmergVmaxpu(Value: Double);cdecl;
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN ActiveCircuit[ActiveActor].EmergMaxVolts := Value;

end;
//------------------------------------------------------------------------------
procedure Settings_Set_EmergVminpu(Value: Double);cdecl;
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN ActiveCircuit[ActiveActor].EmergMinVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_NormVmaxpu(Value: Double);cdecl;
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN ActiveCircuit[ActiveActor].NormalMaxVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_NormVminpu(Value: Double);cdecl;
begin
     IF ActiveCircuit[ActiveActor] <> NIL
     THEN ActiveCircuit[ActiveActor].NormalMinVolts := Value;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_ZoneLock(Value: WordBool);cdecl;
begin
      If ActiveCircuit[ActiveActor] <> NIL THEN ActiveCircuit[ActiveActor].ZonesLocked := Value;
end;
//------------------------------------------------------------------------------
PROCEDURE Settings_Get_LossRegs(var ResultPtr: PInteger; var ResultCount: Integer);cdecl;
VAR
  Result: PIntegerArray;
   i:Integer;
begin
   If ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
       Result := DSS_CreateArray_PInteger(ResultPtr, ResultCount, (ActiveCircuit[ActiveActor].NumLossRegs - 1) + 1);
       FOR i := 0 to ActiveCircuit[ActiveActor].NumLossRegs - 1 DO
       Begin
           Result[i] := ActiveCircuit[ActiveActor].LossRegs^[i+1]
       End;
   END
   ELSE    Result := DSS_CreateArray_PInteger(ResultPtr, ResultCount, (0) + 1);

end;
//------------------------------------------------------------------------------
function Settings_Get_LossWeight():Double;cdecl;
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
        Result := ActiveCircuit[ActiveActor].LossWeight ;
   END
   ELSE    Result := 0.0;
end;
//------------------------------------------------------------------------------
function Settings_Get_Trapezoidal():WordBool;cdecl;
begin

   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
        Result := ActiveCircuit[ActiveActor].TrapezoidalIntegration ;
   END
   ELSE    Result := FALSE;
end;
//------------------------------------------------------------------------------
PROCEDURE Settings_Get_UEregs(var ResultPtr: PInteger; var ResultCount: Integer);cdecl;
VAR
  Result: PIntegerArray;
   i:Integer;
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
       Result := DSS_CreateArray_PInteger(ResultPtr, ResultCount, (ActiveCircuit[ActiveActor].NumUERegs - 1) + 1);
       FOR i := 0 to ActiveCircuit[ActiveActor].NumUERegs - 1 DO
       Begin
           Result[i] := ActiveCircuit[ActiveActor].UERegs^[i+1]
       End;
   END
   ELSE    Result := DSS_CreateArray_PInteger(ResultPtr, ResultCount, (0) + 1);
end;
//------------------------------------------------------------------------------
function Settings_Get_UEweight():Double;cdecl;
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
           Result := ActiveCircuit[ActiveActor].UEWeight
   END
   ELSE    Result := 0.0;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_LossRegs(ValuePtr: PInteger; ValueCount: Integer);cdecl;
VAR
  Value: PIntegerArray;
   i, j:Integer;
begin
    Value := PIntegerArray(ValuePtr);
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
       ReAllocMem(ActiveCircuit[ActiveActor].LossRegs, Sizeof(ActiveCircuit[ActiveActor].LossRegs^[1])*(1 - (0) + (ValueCount - 1)));
       j:=1;
       FOR i := (0) to (ValueCount - 1) DO
       Begin
            ActiveCircuit[ActiveActor].LossRegs^[j] := Value[i];
            Inc(j);
       End;
   End;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_LossWeight(Value: Double);cdecl;
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
       ActiveCircuit[ActiveActor].LossWeight := Value
   End;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_Trapezoidal(Value: WordBool);cdecl;
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
      ActiveCircuit[ActiveActor].TrapezoidalIntegration  := Value
   End;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_UEregs(ValuePtr: PInteger; ValueCount: Integer);cdecl;
VAR
  Value: PIntegerArray;
   i,j:Integer ;
begin
    Value := PIntegerArray(ValuePtr);
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
       ReAllocMem(ActiveCircuit[ActiveActor].UERegs, Sizeof(ActiveCircuit[ActiveActor].UERegs^[1])*(1 - (0) + (ValueCount - 1)));
       j:=1;
       FOR i := (0) to (ValueCount - 1) DO
       Begin
            ActiveCircuit[ActiveActor].UERegs^[j] := Value[i];
            Inc(j);
       End;
   End;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_UEweight(Value: Double);cdecl;
begin
   IF ActiveCircuit[ActiveActor] <> NIL
   THEN Begin
       ActiveCircuit[ActiveActor].UEWeight := Value
   End;
end;
//------------------------------------------------------------------------------
function Settings_Get_ControlTrace():WordBool;cdecl;
begin
     If ActiveCircuit[ActiveActor] <> NIL
     THEN Result := ActiveCircuit[ActiveActor].ControlQueue.TraceLog;
end;
//------------------------------------------------------------------------------
PROCEDURE Settings_Get_VoltageBases(var ResultPtr: PDouble; var ResultCount: Integer);cdecl;
VAR
  Result: PDoubleArray;
   i, Count :Integer;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN With ActiveCircuit[ActiveActor] Do
  Begin
      {Count the number of voltagebases specified}
      i := 0;
      Repeat
            Inc(i);
      Until LegalVoltageBases^[i] = 0.0;
      Count := i-1;

      Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (Count-1) + 1);

      FOR i := 0 to Count-1 Do Result[i] := LegalVoltageBases^[i+1];

  END
  ELSE Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
end;
//------------------------------------------------------------------------------
procedure Settings_Set_ControlTrace(Value: WordBool);cdecl;
begin

     If ActiveCircuit[ActiveActor] <> NIL
     THEN ActiveCircuit[ActiveActor].ControlQueue.TraceLog := Value;

end;
//------------------------------------------------------------------------------
procedure Settings_Set_VoltageBases(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
  Value: PDoubleArray;
   i, j, Num   :Integer;

Begin

     Num   := (ValueCount - 1) - (0) + 1;

     {LegalVoltageBases is a zero-terminated array, so we have to allocate
      one more than the number of actual values}

     WITH ActiveCircuit[ActiveActor] Do
     Begin
       Reallocmem(LegalVoltageBases, Sizeof(LegalVoltageBases^[1])*(Num+1));
       j := 1;
       FOR i := (0) to (ValueCount - 1) Do
       Begin
         LegalVoltageBases^[j] := Value[i];
         Inc(j)
       End;
       LegalVoltageBases^[Num+1] := 0.0;
     End;

end;
//------------------------------------------------------------------------------
function Settings_Get_PriceCurve_AnsiString():AnsiString;inline;
begin
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Result := ActiveCircuit[ActiveActor].PriceCurve
    ELSE Result := '';
end;

function Settings_Get_PriceCurve():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Settings_Get_PriceCurve_AnsiString());
end;
//------------------------------------------------------------------------------
function Settings_Get_PriceSignal():Double;cdecl;
begin
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Result := ActiveCircuit[ActiveActor].Pricesignal
    ELSE Result := 0.0;

end;
//------------------------------------------------------------------------------
procedure Settings_Set_PriceCurve(const Value: PAnsiChar);cdecl;
begin
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN WITH ActiveCircuit[ActiveActor] DO
               Begin
                  PriceCurve    := Value;
                  PriceCurveObj := LoadShapeClass[ActiveActor].Find(Pricecurve);
                  IF PriceCurveObj=nil THEN
                   DoSimpleMsg('Price Curve: "' +Pricecurve+ '" not found.', 5006);
               End;
end;
//------------------------------------------------------------------------------
procedure Settings_Set_PriceSignal(Value: Double);cdecl;
begin
   IF ActiveCircuit[ActiveActor] <> NIL
    THEN ActiveCircuit[ActiveActor].PriceSignal := Value ;
end;
//------------------------------------------------------------------------------
END.
