UNIT CAPI_TSData;
{$inline on}

INTERFACE

USES CAPI_Utils, TSData, CableData;

type
  TSDataProps = (DiaShield=1, TapeLayer, TapeLap);

// Common to all classes
FUNCTION TSData_Get_Count():Integer;cdecl;
FUNCTION TSData_Get_First():Integer;cdecl;
FUNCTION TSData_Get_Next():Integer;cdecl;
FUNCTION TSData_Get_Name():PAnsiChar;cdecl;
PROCEDURE TSData_Set_Name(const Value: PAnsiChar);cdecl;
PROCEDURE TSData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE TSData_Get_AllNames_GR();cdecl;

// From ConductorData
FUNCTION TSData_Get_Rdc():Double;cdecl;
PROCEDURE TSData_Set_Rdc(Value: Double);cdecl;
FUNCTION TSData_Get_Rac():Double;cdecl;
PROCEDURE TSData_Set_Rac(Value: Double);cdecl;
FUNCTION TSData_Get_GMRac():Double;cdecl;
PROCEDURE TSData_Set_GMRac(Value: Double);cdecl;
FUNCTION TSData_Get_GMRUnits():Integer;cdecl;
PROCEDURE TSData_Set_GMRUnits(Value: Integer);cdecl;
FUNCTION TSData_Get_Radius():Double;cdecl;
PROCEDURE TSData_Set_Radius(Value: Double);cdecl;
FUNCTION TSData_Get_RadiusUnits():Integer;cdecl;
PROCEDURE TSData_Set_RadiusUnits(Value: Integer);cdecl;
FUNCTION TSData_Get_ResistanceUnits():Integer;cdecl;
PROCEDURE TSData_Set_ResistanceUnits(Value: Integer);cdecl;
FUNCTION TSData_Get_Diameter():Double;cdecl;
PROCEDURE TSData_Set_Diameter(Value: Double);cdecl;
FUNCTION TSData_Get_NormAmps():Double;cdecl;
PROCEDURE TSData_Set_NormAmps(Value: Double);cdecl;
FUNCTION TSData_Get_EmergAmps():Double;cdecl;
PROCEDURE TSData_Set_EmergAmps(Value: Double);cdecl;

// From CableData
FUNCTION TSData_Get_EpsR():Double;cdecl;
PROCEDURE TSData_Set_EpsR(Value: Double);cdecl;
FUNCTION TSData_Get_InsLayer():Double;cdecl;
PROCEDURE TSData_Set_InsLayer(Value: Double);cdecl;
FUNCTION TSData_Get_DiaIns():Double;cdecl;
PROCEDURE TSData_Set_DiaIns(Value: Double);cdecl;
FUNCTION TSData_Get_DiaCable():Double;cdecl;
PROCEDURE TSData_Set_DiaCable(Value: Double);cdecl;
        
// From TSData
FUNCTION TSData_Get_DiaShield():Double;cdecl;
PROCEDURE TSData_Set_DiaShield(Value: Double);cdecl;
FUNCTION TSData_Get_TapeLayer():Double;cdecl;
PROCEDURE TSData_Set_TapeLayer(Value: Double);cdecl;
FUNCTION TSData_Get_TapeLap():Double;cdecl;
PROCEDURE TSData_Set_TapeLap(Value: Double);cdecl;

IMPLEMENTATION

USES CAPI_Constants, sysutils, DSSGlobals, LineUnits, ConductorData, CAPI_WireData, CAPI_CNData;

//------------------------------------------------------------------------------  
procedure TSDataSetDefaults(prop: TSDataProps; conductor: TTSDataObj);
begin
  {Set defaults}
  With conductor do
  begin
    {Check for critical errors}
    CASE prop OF
      TSDataProps.DiaShield: If (FDiaShield <= 0.0) Then DoSimpleMsg('Error: Diameter over shield must be positive for TapeShieldData ' + Name,999);
      TSDataProps.TapeLayer: If (FTapeLayer <= 0.0) Then DoSimpleMsg('Error: Tape shield thickness must be positive for TapeShieldData ' + Name,999);
      TSDataProps.TapeLap: If ((FTapeLap < 0.0) Or (FTapeLap > 100.0)) Then DoSimpleMsg('Error: Tap lap must range from 0 to 100 for TapeShieldData ' + Name,999);
    END;
  end;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_Count():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := TSDataClass[ActiveActor].ElementCount;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_First():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := TSDataClass[ActiveActor].First;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_Next():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := TSDataClass[ActiveActor].Next;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_Name_AnsiString():AnsiString;inline;
Var
   pTSData:TTSDataObj;

Begin
   Result := '';  // signify no name
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pTSData := TSDataClass[ActiveActor].GetActiveObj ;
        If pTSData <> Nil Then
        Begin
              Result := pTSData.Name;
        End;
   End;

end;

FUNCTION TSData_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(TSData_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_Name(const Value: PAnsiChar);cdecl;
// set LineCode active by name

Begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        If Not TSDataClass[ActiveActor].SetActive (Value) Then
         DoSimpleMsg('TSData "'+ Value +'" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
   End;

end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  TSDataElem:TTSDataObj;
  k:Integer;

Begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     If TSDataClass[ActiveActor].ElementList.ListSize  >0 Then
     Begin
       DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (TSDataClass[ActiveActor].ElementList.ListSize-1) + 1);
       k:=0;
       TSDataElem := TSDataClass[ActiveActor].ElementList.First;
       WHILE TSDataElem<>Nil DO
       Begin
          Result[k] := DSS_CopyStringAsPChar(TSDataElem.Name);
          Inc(k);
          TSDataElem := TSDataClass[ActiveActor].ElementList.Next;
       End;
     End;

end;
PROCEDURE TSData_Get_AllNames_GR();cdecl;
// Same as TSData_Get_AllNames but uses global result (GR) pointers
begin
   TSData_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_NormAmps():Double;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.NormAmps;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_NormAmps(Value: Double);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       pTSData.NormAmps := Value;
       ConductorSetDefaults(ConductorProps.NormAmps, pTSData);
  End

end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_EmergAmps():Double;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.EmergAmps;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_EmergAmps(Value: Double);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       pTSData.EmergAmps := Value;
       ConductorSetDefaults(ConductorProps.EmergAmps, pTSData);
  End;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_Diameter():Double;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.FRadius * 2.0;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_Diameter(Value: Double);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       with pTSData do 
       begin
          FRadius := Value / 2.0;
          ConductorSetDefaults(ConductorProps.diam, pTSData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_Radius():Double;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.FRadius;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_Radius(Value: Double);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       with pTSData do 
       begin
          FRadius := Value;
          ConductorSetDefaults(ConductorProps.Radius, pTSData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_GMRac():Double;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.FGMR60;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_GMRac(Value: Double);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       with pTSData do 
       begin
          FGMR60 := Value;
          ConductorSetDefaults(ConductorProps.GMRac, pTSData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_Rac():Double;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.FR60;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_Rac(Value: Double);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       with pTSData do 
       begin
          FR60 := Value;
          ConductorSetDefaults(ConductorProps.Rac, pTSData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_Rdc():Double;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.FRDC;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_Rdc(Value: Double);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       with pTSData do 
       begin
          FRDC := Value;
          ConductorSetDefaults(ConductorProps.Rdc, pTSData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_GMRUnits():Integer;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.FGMRUnits;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_GMRUnits(Value: Integer);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       with pTSData do 
       begin
          FGMRUnits := Value;
          ConductorSetDefaults(ConductorProps.GMRunits, pTSData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_RadiusUnits():Integer;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.FRadiusUnits;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_RadiusUnits(Value: Integer);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       with pTSData do 
       begin
          FRadiusUnits := Value;
          ConductorSetDefaults(ConductorProps.radunits, pTSData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_ResistanceUnits():Integer;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.FResistanceUnits;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_ResistanceUnits(Value: Integer);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       with pTSData do 
       begin
          FResistanceUnits := Value;
          ConductorSetDefaults(ConductorProps.Runits, pTSData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_EpsR():Double;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.FEpsR;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_EpsR(Value: Double);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       with pTSData do 
       begin
          FEpsR := Value;
          CableDataSetDefaults(CableDataProps.EpsR, pTSData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_InsLayer():Double;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.FInsLayer;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_InsLayer(Value: Double);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       with pTSData do 
       begin
          FInsLayer := Value;
          CableDataSetDefaults(CableDataProps.InsLayer, pTSData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_DiaIns():Double;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.FDiaIns;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_DiaIns(Value: Double);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       with pTSData do 
       begin
          FDiaIns := Value;
          CableDataSetDefaults(CableDataProps.DiaIns, pTSData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_DiaCable():Double;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.FDiaCable;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_DiaCable(Value: Double);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       with pTSData do 
       begin
          FDiaCable := Value;
          CableDataSetDefaults(CableDataProps.DiaCable, pTSData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_DiaShield():Double;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.FDiaShield;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_DiaShield(Value: Double);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       with pTSData do 
       begin
          FDiaShield:= Value;
          TSDataSetDefaults(TSDataProps.DiaShield, pTSData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_TapeLayer():Double;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.FTapeLayer;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_TapeLayer(Value: Double);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       with pTSData do 
       begin
          FTapeLayer:= Value;
          TSDataSetDefaults(TSDataProps.TapeLayer, pTSData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION TSData_Get_TapeLap():Double;cdecl;
Var
   pTSData:TTSDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       Result := pTSData.FTapeLap;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE TSData_Set_TapeLap(Value: Double);cdecl;
Var
   pTSData:TTSDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pTSData := TSDataClass[ActiveActor].GetActiveObj;
       with pTSData do 
       begin
          FTapeLap:= Value;
          TSDataSetDefaults(TSDataProps.TapeLap, pTSData);
       end;
  End;
end;
//------------------------------------------------------------------------------

END.
