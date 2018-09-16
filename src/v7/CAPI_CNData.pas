UNIT CAPI_CNData;
{$inline on}

INTERFACE

USES CAPI_Utils, CNData, CableData;

type
  CableDataProps = (EpsR=1, InsLayer, DiaIns, DiaCable);
  CNDataProps = (k=1, DiaStrand, GmrStrand, Rstrand);

procedure CableDataSetDefaults(prop: CableDataProps; conductor: TCableDataObj);

// Common to all classes
FUNCTION CNData_Get_Count():Integer;cdecl;
FUNCTION CNData_Get_First():Integer;cdecl;
FUNCTION CNData_Get_Next():Integer;cdecl;
FUNCTION CNData_Get_Name():PAnsiChar;cdecl;
PROCEDURE CNData_Set_Name(const Value: PAnsiChar);cdecl;
PROCEDURE CNData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE CNData_Get_AllNames_GR();cdecl;

// From ConductorData
FUNCTION CNData_Get_Rdc():Double;cdecl;
PROCEDURE CNData_Set_Rdc(Value: Double);cdecl;
FUNCTION CNData_Get_Rac():Double;cdecl;
PROCEDURE CNData_Set_Rac(Value: Double);cdecl;
FUNCTION CNData_Get_GMRac():Double;cdecl;
PROCEDURE CNData_Set_GMRac(Value: Double);cdecl;
FUNCTION CNData_Get_GMRUnits():Integer;cdecl;
PROCEDURE CNData_Set_GMRUnits(Value: Integer);cdecl;
FUNCTION CNData_Get_Radius():Double;cdecl;
PROCEDURE CNData_Set_Radius(Value: Double);cdecl;
FUNCTION CNData_Get_RadiusUnits():Integer;cdecl;
PROCEDURE CNData_Set_RadiusUnits(Value: Integer);cdecl;
FUNCTION CNData_Get_ResistanceUnits():Integer;cdecl;
PROCEDURE CNData_Set_ResistanceUnits(Value: Integer);cdecl;
FUNCTION CNData_Get_Diameter():Double;cdecl;
PROCEDURE CNData_Set_Diameter(Value: Double);cdecl;
FUNCTION CNData_Get_NormAmps():Double;cdecl;
PROCEDURE CNData_Set_NormAmps(Value: Double);cdecl;
FUNCTION CNData_Get_EmergAmps():Double;cdecl;
PROCEDURE CNData_Set_EmergAmps(Value: Double);cdecl;

// From CableData
FUNCTION CNData_Get_EpsR():Double;cdecl;
PROCEDURE CNData_Set_EpsR(Value: Double);cdecl;
FUNCTION CNData_Get_InsLayer():Double;cdecl;
PROCEDURE CNData_Set_InsLayer(Value: Double);cdecl;
FUNCTION CNData_Get_DiaIns():Double;cdecl;
PROCEDURE CNData_Set_DiaIns(Value: Double);cdecl;
FUNCTION CNData_Get_DiaCable():Double;cdecl;
PROCEDURE CNData_Set_DiaCable(Value: Double);cdecl;
        
// From CNData
FUNCTION CNData_Get_k():Integer;cdecl;
PROCEDURE CNData_Set_k(Value: Integer);cdecl;
FUNCTION CNData_Get_DiaStrand():Double;cdecl;
PROCEDURE CNData_Set_DiaStrand(Value: Double);cdecl;
FUNCTION CNData_Get_GmrStrand():Double;cdecl;
PROCEDURE CNData_Set_GmrStrand(Value: Double);cdecl;
FUNCTION CNData_Get_RStrand():Double;cdecl;
PROCEDURE CNData_Set_RStrand(Value: Double);cdecl;

IMPLEMENTATION

USES CAPI_Constants, sysutils, DSSGlobals, LineUnits, ConductorData, CAPI_WireData;

//------------------------------------------------------------------------------  
procedure CableDataSetDefaults(prop: CableDataProps; conductor: TCableDataObj);
begin
  {Set defaults}
  With conductor do
  begin
      {Check for critical errors}
      CASE prop OF
        CableDataProps.EpsR: If (FEpsR < 1.0) Then DoSimpleMsg('Error: Insulation permittivity must be greater than one for CableData ' + Name, 999);
        CableDataProps.InsLayer: If (FInsLayer <= 0.0) Then DoSimpleMsg('Error: Insulation layer thickness must be positive for CableData ' + Name, 999);
        CableDataProps.DiaIns: If (FDiaIns   <= 0.0) Then DoSimpleMsg('Error: Diameter over insulation layer must be positive for CableData ' + Name, 999);
        CableDataProps.DiaCable: If (FDiaCable <= 0.0) Then DoSimpleMsg('Error: Diameter over cable must be positive for CableData ' + Name, 999);
      END;
  end;
end;
//------------------------------------------------------------------------------  
procedure CNDataSetDefaults(prop: CNDataProps; conductor: TCNDataObj);
begin
  {Set defaults}
  With conductor do
  begin
    {Set defaults}
    CASE prop OF
        CNDataProps.DiaStrand: If FGmrStrand <=0.0 Then FGmrStrand := 0.7788 * 0.5 * FDiaStrand;
    END;
    
    {Check for critical errors}
    CASE prop OF
        CNDataProps.k: If (FkStrand < 2) Then DoSimpleMsg('Error: Must have at least 2 concentric neutral strands for CNData ' + Name,999);
        CNDataProps.DiaStrand: If (FDiaStrand <= 0.0) Then DoSimpleMsg('Error: Neutral strand diameter must be positive for CNData ' + Name,999);
        CNDataProps.GmrStrand: If (FGmrStrand <= 0.0) Then DoSimpleMsg('Error: Neutral strand GMR must be positive for CNData ' + Name,999);
    END;
  end;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_Count():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit <> Nil Then
        Result := CNDataClass.ElementCount;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_First():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit <> Nil Then
        Result := CNDataClass.First;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_Next():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit <> Nil Then
        Result := CNDataClass.Next;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_Name_AnsiString():AnsiString;inline;
Var
   pCNData:TCNDataObj;

Begin
   Result := '';  // signify no name
   If ActiveCircuit <> Nil Then
   Begin
        pCNData := CNDataClass.GetActiveObj ;
        If pCNData <> Nil Then
        Begin
              Result := pCNData.Name;
        End;
   End;

end;

FUNCTION CNData_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(CNData_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_Name(const Value: PAnsiChar);cdecl;
// set LineCode active by name

Begin
   If ActiveCircuit <> Nil Then
   Begin
        If Not CNDataClass.SetActive (Value) Then
         DoSimpleMsg('CNData "'+ Value +'" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
   End;

end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  CNDataElem:TCNDataObj;
  k:Integer;

Begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     If CNDataClass.ElementList.ListSize  >0 Then
     Begin
       DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (CNDataClass.ElementList.ListSize-1) + 1);
       k:=0;
       CNDataElem := CNDataClass.ElementList.First;
       WHILE CNDataElem<>Nil DO
       Begin
          Result[k] := DSS_CopyStringAsPChar(CNDataElem.Name);
          Inc(k);
          CNDataElem := CNDataClass.ElementList.Next;
       End;
     End;

end;
PROCEDURE CNData_Get_AllNames_GR();cdecl;
// Same as CNData_Get_AllNames but uses global result (GR) pointers
begin
   CNData_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_NormAmps():Double;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.NormAmps;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_NormAmps(Value: Double);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       pCNData.NormAmps := Value;
       ConductorSetDefaults(ConductorProps.NormAmps, pCNData);
  End

end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_EmergAmps():Double;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.EmergAmps;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_EmergAmps(Value: Double);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       pCNData.EmergAmps := Value;
       ConductorSetDefaults(ConductorProps.EmergAmps, pCNData);
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_Diameter():Double;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FRadius * 2.0;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_Diameter(Value: Double);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FRadius := Value / 2.0;
          ConductorSetDefaults(ConductorProps.diam, pCNData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_Radius():Double;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FRadius;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_Radius(Value: Double);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FRadius := Value;
          ConductorSetDefaults(ConductorProps.Radius, pCNData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_GMRac():Double;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FGMR60;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_GMRac(Value: Double);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FGMR60 := Value;
          ConductorSetDefaults(ConductorProps.GMRac, pCNData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_Rac():Double;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FR60;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_Rac(Value: Double);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FR60 := Value;
          ConductorSetDefaults(ConductorProps.Rac, pCNData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_Rdc():Double;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FRDC;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_Rdc(Value: Double);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FRDC := Value;
          ConductorSetDefaults(ConductorProps.Rdc, pCNData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_GMRUnits():Integer;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FGMRUnits;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_GMRUnits(Value: Integer);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FGMRUnits := Value;
          ConductorSetDefaults(ConductorProps.GMRunits, pCNData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_RadiusUnits():Integer;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FRadiusUnits;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_RadiusUnits(Value: Integer);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FRadiusUnits := Value;
          ConductorSetDefaults(ConductorProps.radunits, pCNData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_ResistanceUnits():Integer;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FResistanceUnits;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_ResistanceUnits(Value: Integer);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FResistanceUnits := Value;
          ConductorSetDefaults(ConductorProps.Runits, pCNData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_EpsR():Double;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FEpsR;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_EpsR(Value: Double);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FEpsR := Value;
          CableDataSetDefaults(CableDataProps.EpsR, pCNData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_InsLayer():Double;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FInsLayer;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_InsLayer(Value: Double);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FInsLayer := Value;
          CableDataSetDefaults(CableDataProps.InsLayer, pCNData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_DiaIns():Double;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FDiaIns;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_DiaIns(Value: Double);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FDiaIns := Value;
          CableDataSetDefaults(CableDataProps.DiaIns, pCNData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_DiaCable():Double;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FDiaCable;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_DiaCable(Value: Double);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FDiaCable := Value;
          CableDataSetDefaults(CableDataProps.DiaCable, pCNData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_k():Integer;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FkStrand;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_k(Value: Integer);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FkStrand := Value;
          CNDataSetDefaults(CNDataProps.k, pCNData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_DiaStrand():Double;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FDiaStrand;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_DiaStrand(Value: Double);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FDiaStrand := Value;
          CNDataSetDefaults(CNDataProps.DiaStrand, pCNData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_GmrStrand():Double;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FGmrStrand;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_GmrStrand(Value: Double);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FGmrStrand := Value;
          CNDataSetDefaults(CNDataProps.GmrStrand, pCNData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION CNData_Get_RStrand():Double;cdecl;
Var
   pCNData:TCNDataObj;
begin
  Result := 0;
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       Result := pCNData.FRStrand;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE CNData_Set_RStrand(Value: Double);cdecl;
Var
   pCNData:TCNDataObj;
begin
  IF ActiveCircuit <> NIL
  THEN Begin
       pCNData := CNDataClass.GetActiveObj;
       with pCNData do 
       begin
          FRStrand:= Value;
          CNDataSetDefaults(CNDataProps.RStrand, pCNData);
       end;
  End;
end;

END.
