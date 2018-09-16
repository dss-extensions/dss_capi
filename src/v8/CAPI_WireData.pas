UNIT CAPI_WireData;
{$inline on}

INTERFACE

USES CAPI_Utils, WireData, ConductorData;

type
  //TODO: create a simple script to extract this directly from the Pascal files from electricdss-src
  ConductorProps = (Rdc=1, Rac, Runits, GMRac, GMRunits, radius, radunits, normamps, emergamps, diam);

  
FUNCTION WireData_Get_Count():Integer;cdecl;
FUNCTION WireData_Get_First():Integer;cdecl;
FUNCTION WireData_Get_Next():Integer;cdecl;
FUNCTION WireData_Get_Name():PAnsiChar;cdecl;
PROCEDURE WireData_Set_Name(const Value: PAnsiChar);cdecl;
PROCEDURE WireData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE WireData_Get_AllNames_GR();cdecl;
FUNCTION WireData_Get_Rdc():Double;cdecl;
PROCEDURE WireData_Set_Rdc(Value: Double);cdecl;
FUNCTION WireData_Get_Rac():Double;cdecl;
PROCEDURE WireData_Set_Rac(Value: Double);cdecl;
FUNCTION WireData_Get_GMRac():Double;cdecl;
PROCEDURE WireData_Set_GMRac(Value: Double);cdecl;
FUNCTION WireData_Get_GMRUnits():Integer;cdecl;
PROCEDURE WireData_Set_GMRUnits(Value: Integer);cdecl;
FUNCTION WireData_Get_Radius():Double;cdecl;
PROCEDURE WireData_Set_Radius(Value: Double);cdecl;
FUNCTION WireData_Get_RadiusUnits():Integer;cdecl;
PROCEDURE WireData_Set_RadiusUnits(Value: Integer);cdecl;
FUNCTION WireData_Get_ResistanceUnits():Integer;cdecl;
PROCEDURE WireData_Set_ResistanceUnits(Value: Integer);cdecl;
FUNCTION WireData_Get_Diameter():Double;cdecl;
PROCEDURE WireData_Set_Diameter(Value: Double);cdecl;
FUNCTION WireData_Get_NormAmps():Double;cdecl;
PROCEDURE WireData_Set_NormAmps(Value: Double);cdecl;
FUNCTION WireData_Get_EmergAmps():Double;cdecl;
PROCEDURE WireData_Set_EmergAmps(Value: Double);cdecl;
 
procedure ConductorSetDefaults(prop: ConductorProps; conductor: TConductorDataObj);


IMPLEMENTATION

USES CAPI_Constants, sysutils, DSSGlobals, LineUnits;

procedure ConductorSetDefaults(prop: ConductorProps; conductor: TConductorDataObj);
begin
  {Set defaults}
  With conductor do
  begin
    CASE prop OF
      ConductorProps.Rdc:       If FR60<0.0       Then FR60 := 1.02* FRDC;
      ConductorProps.Rac:       If FRDC<0.0       Then FRDC := FR60 / 1.02;
      ConductorProps.GMRac:     If Fradius<0.0    Then Fradius := FGMR60 / 0.7788;
      ConductorProps.GMRunits:  If FradiusUnits=0 Then FradiusUnits := FGMRunits;
      ConductorProps.radius:    If FGMR60<0.0     Then FGMR60 := 0.7788 * FRadius;
      ConductorProps.radunits:  If FGMRUnits=0    Then FGMRunits := FradiusUnits;
      ConductorProps.normamps:  IF EmergAmps<0.0  Then EmergAmps := 1.5*NormAmps;
      ConductorProps.emergamps: If NormAmps<0.0   Then NormAmps := EmergAmps/1.5;
      ConductorProps.diam:      If FGMR60<0.0     Then FGMR60 := 0.7788 * FRadius;
    END;
    {Check for critical errors}
    CASE prop OF
      ConductorProps.GMRac: If (Fradius = 0.0)  Then DoSimpleMsg('Error: Radius is specified as zero for ConductorData.' + Name,999);
      ConductorProps.radius: If (FGMR60 = 0.0)   Then DoSimpleMsg('Error: GMR is specified as zero for ConductorData.' + Name,999);
    END;
  end;
end;
//------------------------------------------------------------------------------
FUNCTION WireData_Get_Count():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := WireDataClass[ActiveActor].ElementCount;
end;
//------------------------------------------------------------------------------
FUNCTION WireData_Get_First():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := WireDataClass[ActiveActor].First;
end;
//------------------------------------------------------------------------------
FUNCTION WireData_Get_Next():Integer;cdecl;
begin
      Result := 0;
      If ActiveCircuit[ActiveActor] <> Nil Then
        Result := WireDataClass[ActiveActor].Next;
end;
//------------------------------------------------------------------------------
FUNCTION WireData_Get_Name_AnsiString():AnsiString;inline;
Var
   pWireData:TWireDataObj;

Begin
   Result := '';  // signify no name
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj ;
        If pWireData <> Nil Then
        Begin
              Result := pWireData.Name;
        End;
   End;

end;

FUNCTION WireData_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(WireData_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
PROCEDURE WireData_Set_Name(const Value: PAnsiChar);cdecl;
// set LineCode active by name

Begin
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
        If Not WireDataClass[ActiveActor].SetActive (Value) Then
         DoSimpleMsg('WireData "'+ Value +'" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
   End;

end;
//------------------------------------------------------------------------------
PROCEDURE WireData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  WireDataElem:TWireDataObj;
  k:Integer;

Begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     If WireDataClass[ActiveActor].ElementList.ListSize  >0 Then
     Begin
       DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (WireDataClass[ActiveActor].ElementList.ListSize-1) + 1);
       k:=0;
       WireDataElem := WireDataClass[ActiveActor].ElementList.First;
       WHILE WireDataElem<>Nil DO
       Begin
          Result[k] := DSS_CopyStringAsPChar(WireDataElem.Name);
          Inc(k);
          WireDataElem := WireDataClass[ActiveActor].ElementList.Next;
       End;
     End;

end;
PROCEDURE WireData_Get_AllNames_GR();cdecl;
// Same as WireData_Get_AllNames but uses global result (GR) pointers
begin
   WireData_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;
//------------------------------------------------------------------------------
FUNCTION WireData_Get_NormAmps():Double;cdecl;
Var
   pWireData:TWireDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       Result := pWireData.NormAmps;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE WireData_Set_NormAmps(Value: Double);cdecl;
Var
   pWireData:TWireDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       pWireData.NormAmps := Value;
       ConductorSetDefaults(ConductorProps.NormAmps, pWireData);
  End

end;
//------------------------------------------------------------------------------
FUNCTION WireData_Get_EmergAmps():Double;cdecl;
Var
   pWireData:TWireDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       Result := pWireData.EmergAmps;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE WireData_Set_EmergAmps(Value: Double);cdecl;
Var
   pWireData:TWireDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       pWireData.EmergAmps := Value;
       ConductorSetDefaults(ConductorProps.EmergAmps, pWireData);
  End;
end;
//------------------------------------------------------------------------------
FUNCTION WireData_Get_Diameter():Double;cdecl;
Var
   pWireData:TWireDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       Result := pWireData.FRadius * 2.0;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE WireData_Set_Diameter(Value: Double);cdecl;
Var
   pWireData:TWireDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       with pWireData do 
       begin
          FRadius := Value / 2.0;
          ConductorSetDefaults(ConductorProps.diam, pWireData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION WireData_Get_Radius():Double;cdecl;
Var
   pWireData:TWireDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       Result := pWireData.FRadius;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE WireData_Set_Radius(Value: Double);cdecl;
Var
   pWireData:TWireDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       with pWireData do 
       begin
          FRadius := Value;
          ConductorSetDefaults(ConductorProps.Radius, pWireData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION WireData_Get_GMRac():Double;cdecl;
Var
   pWireData:TWireDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       Result := pWireData.FGMR60;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE WireData_Set_GMRac(Value: Double);cdecl;
Var
   pWireData:TWireDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       with pWireData do 
       begin
          FGMR60 := Value;
          ConductorSetDefaults(ConductorProps.GMRac, pWireData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION WireData_Get_Rac():Double;cdecl;
Var
   pWireData:TWireDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       Result := pWireData.FR60;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE WireData_Set_Rac(Value: Double);cdecl;
Var
   pWireData:TWireDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       with pWireData do 
       begin
          FR60 := Value;
          ConductorSetDefaults(ConductorProps.Rac, pWireData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION WireData_Get_Rdc():Double;cdecl;
Var
   pWireData:TWireDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       Result := pWireData.FRDC;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE WireData_Set_Rdc(Value: Double);cdecl;
Var
   pWireData:TWireDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       with pWireData do 
       begin
          FRDC := Value;
          ConductorSetDefaults(ConductorProps.Rdc, pWireData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION WireData_Get_GMRUnits():Integer;cdecl;
Var
   pWireData:TWireDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       Result := pWireData.FGMRUnits;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE WireData_Set_GMRUnits(Value: Integer);cdecl;
Var
   pWireData:TWireDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       with pWireData do 
       begin
          FGMRUnits := Value;
          ConductorSetDefaults(ConductorProps.GMRunits, pWireData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION WireData_Get_RadiusUnits():Integer;cdecl;
Var
   pWireData:TWireDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       Result := pWireData.FRadiusUnits;
  End;
end;

//------------------------------------------------------------------------------
PROCEDURE WireData_Set_RadiusUnits(Value: Integer);cdecl;
Var
   pWireData:TWireDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       with pWireData do 
       begin
          FRadiusUnits := Value;
          ConductorSetDefaults(ConductorProps.radunits, pWireData);
       end;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION WireData_Get_ResistanceUnits():Integer;cdecl;
Var
   pWireData:TWireDataObj;
begin
  Result := 0;
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       Result := pWireData.FResistanceUnits;
  End;
end;
//------------------------------------------------------------------------------
PROCEDURE WireData_Set_ResistanceUnits(Value: Integer);cdecl;
Var
   pWireData:TWireDataObj;
begin
  IF ActiveCircuit[ActiveActor] <> NIL
  THEN Begin
       pWireData := WireDataClass[ActiveActor].GetActiveObj;
       with pWireData do 
       begin
          FResistanceUnits := Value;
          ConductorSetDefaults(ConductorProps.Runits, pWireData);
       end;
  End;
end;
//------------------------------------------------------------------------------
END.
