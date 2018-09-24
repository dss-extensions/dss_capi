UNIT CAPI_Reactors;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE Reactors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Reactors_Get_AllNames_GR();cdecl;
FUNCTION Reactors_Get_Name():PAnsiChar;cdecl;
PROCEDURE Reactors_Set_Name(const Value: PAnsiChar);cdecl;
FUNCTION Reactors_Get_First():Integer;cdecl;
FUNCTION Reactors_Get_Next():Integer;cdecl;
FUNCTION Reactors_Get_Count():Integer;cdecl;
FUNCTION Reactors_Get_kV():Double;cdecl;
PROCEDURE Reactors_Set_kV(Value: Double);cdecl;
FUNCTION Reactors_Get_kvar():Double;cdecl;
PROCEDURE Reactors_Set_kvar(Value: Double);cdecl;
FUNCTION Reactors_Get_Phases():Integer;cdecl;
PROCEDURE Reactors_Set_Phases(Value: Integer);cdecl;
FUNCTION Reactors_Get_SpecType():Integer;cdecl;
FUNCTION Reactors_Get_IsDelta():WordBool;cdecl;
PROCEDURE Reactors_Set_IsDelta(Value: WordBool);cdecl;
FUNCTION Reactors_Get_Parallel():WordBool;cdecl;
PROCEDURE Reactors_Set_Parallel(Value: WordBool);cdecl;
FUNCTION Reactors_Get_LmH():Double;cdecl;
PROCEDURE Reactors_Set_LmH(Value: Double);cdecl;
FUNCTION Reactors_Get_Bus1():PAnsiChar;cdecl;
FUNCTION Reactors_Get_Bus2():PAnsiChar;cdecl;
PROCEDURE Reactors_Set_Bus1(const Value: PAnsiChar);cdecl;
PROCEDURE Reactors_Set_Bus2(const Value: PAnsiChar);cdecl;
FUNCTION Reactors_Get_R():Double;cdecl;
PROCEDURE Reactors_Set_R(Value: Double);cdecl;
FUNCTION Reactors_Get_X():Double;cdecl;
PROCEDURE Reactors_Set_X(Value: Double);cdecl;
FUNCTION Reactors_Get_Rp():Double;cdecl;
PROCEDURE Reactors_Set_Rp(Value: Double);cdecl;
FUNCTION Reactors_Get_RCurve():PAnsiChar;cdecl;
PROCEDURE Reactors_Set_RCurve(const Value: PAnsiChar);cdecl;
FUNCTION Reactors_Get_LCurve():PAnsiChar;cdecl;
PROCEDURE Reactors_Set_LCurve(const Value: PAnsiChar);cdecl;
PROCEDURE Reactors_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Reactors_Get_Rmatrix_GR();cdecl;
PROCEDURE Reactors_Set_Rmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
PROCEDURE Reactors_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Reactors_Get_Xmatrix_GR();cdecl;
PROCEDURE Reactors_Set_Xmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
PROCEDURE Reactors_Get_Z(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Reactors_Get_Z_GR();cdecl;
PROCEDURE Reactors_Set_Z(ValuePtr: PDouble; ValueCount: Integer);cdecl;
PROCEDURE Reactors_Get_Z1(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Reactors_Get_Z1_GR();cdecl;
PROCEDURE Reactors_Set_Z1(ValuePtr: PDouble; ValueCount: Integer);cdecl;
PROCEDURE Reactors_Get_Z2(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Reactors_Get_Z2_GR();cdecl;
PROCEDURE Reactors_Set_Z2(ValuePtr: PDouble; ValueCount: Integer);cdecl;
PROCEDURE Reactors_Get_Z0(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Reactors_Get_Z0_GR();cdecl;
PROCEDURE Reactors_Set_Z0(ValuePtr: PDouble; ValueCount: Integer);cdecl;


IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Executive, Reactor, SysUtils, PointerList, Utilities, ucomplex;

TYPE
    ReactorProps = (bus1=1, bus2, phases, kvar, kv, conn, Rmatrix, Xmatrix, Parallel, R, X, Rp, Z1, Z2, Z0, Z, RCurve, LCurve, LmH);
   
//------------------------------------------------------------------------------
FUNCTION ActiveReactor(var e: TReactorObj): TReactorObj;inline;
begin
  e := nil;
  if ActiveCircuit[ActiveActor] <> Nil then e := ActiveCircuit[ActiveActor].Reactors.Active;
  Result := e;
end;
//------------------------------------------------------------------------------
PROCEDURE ReactorPropSideEffects(prop: ReactorProps; reactor: TReactorObj);
begin
  With reactor do
  begin
    // Some specials ...
    CASE prop OF
       ReactorProps.bus1:Begin
           PropertyValue[2]     := GetBus(2);   // this gets modified
           PrpSequence^[2] := 0;       // Reset this for save function
         End;
       ReactorProps.bus2:If CompareText(StripExtension(GetBus(1)), StripExtension(GetBus(2))) <> 0
         Then Begin
           IsShunt     := FALSE;
           Bus2Defined := TRUE;
         End;
       ReactorProps.phases: //IF Fnphases <> Parser.IntValue THEN 
          BEGIN
            // Nphases := Parser.IntValue ;
            NConds := nphases;  // Force Reallocation of terminal info
            Yorder := Fnterms*Fnconds;
          END;
       ReactorProps.kvar:   SpecType := 1;   // X specified by kvar, kV
       ReactorProps.Rmatrix,ReactorProps.Xmatrix: SpecType := 3;
       ReactorProps.X:  SpecType := 2;   // X specified directly rather than computed from kvar
       ReactorProps.Rp:  RpSpecified := TRUE;
       ReactorProps.Z1:  Begin
               SpecType := 4;    // have to set Z1 to get this mode
               If Not Z2Specified  Then  Z2 := Z1;
               If Not Z0Specified  Then  Z0 := Z1;
            End;
       ReactorProps.Z2:  Z2Specified := TRUE;
       ReactorProps.Z0:  Z0Specified := TRUE;
       ReactorProps.Z:  Begin
               R := Z.re;
               X := Z.im;
               SpecType := 2;
            End;
       ReactorProps.RCurve: RCurveObj   := XYCurveClass[ActiveActor].Find(RCurve);
       ReactorProps.LCurve: LCurveObj   := XYCurveClass[ActiveActor].Find(LCurve);
       ReactorProps.LmH: Begin
              SpecType := 2;
              X := L * TwoPi * BaseFrequency;
           End
    ELSE
    END;
 
    //YPrim invalidation on anything that changes impedance values
    CASE prop OF
        ReactorProps.phases..ReactorProps.Z: YprimInvalid[ActiveActor] := True;
        ReactorProps.RCurve: If RCurveObj=nil Then DoSimpleMsg('Resistance-frequency curve XYCurve.'+RCurve+' not Found.', 2301);
        ReactorProps.LCurve: If LCurveObj=nil Then DoSimpleMsg('Inductance-frequency curve XYCurve.'+LCurve+' not Found.', 2301);
        ReactorProps.LmH: YprimInvalid[ActiveActor] := True;
    ELSE
    END;
  end;
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  elem: TReactorObj;
  lst: TPointerList;
  k: Integer;
Begin
  Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
  Result[0] := DSS_CopyStringAsPChar('NONE');
  IF ActiveCircuit[ActiveActor] <> Nil THEN WITH ActiveCircuit[ActiveActor] DO
  If Reactors.ListSize > 0 then
  Begin
    lst := Reactors;
    DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (lst.ListSize-1) + 1);
    k:=0;
    elem := lst.First;
    WHILE elem<>Nil DO Begin
      Result[k] := DSS_CopyStringAsPChar(elem.Name);
      Inc(k);
      elem := lst.Next;
    End;
  End;
end;
PROCEDURE Reactors_Get_AllNames_GR();cdecl;
// Same as Reactors_Get_AllNames but uses global result (GR) pointers
begin
   Reactors_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
FUNCTION Reactors_Get_First():Integer;cdecl;
Var
  elem: TReactorObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then begin
    lst := ActiveCircuit[ActiveActor].Reactors;
    elem := lst.First;
    If elem <> Nil Then Begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := elem;
          Result := 1;
        End
        Else elem := lst.Next;
      Until (Result = 1) or (elem = nil);
    End;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION Reactors_Get_Name_AnsiString():AnsiString;inline;
Var
  elem: TReactorObj;
Begin
  Result := '';
  if ActiveReactor(elem) <> Nil Then Result := elem.Name;
end;

FUNCTION Reactors_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Reactors_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_Name(const Value: PAnsiChar);cdecl;
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TReactorObj;
  lst: TPointerList;
Begin
  IF ActiveCircuit[ActiveActor] <> NIL THEN Begin
    lst := ActiveCircuit[ActiveActor].Reactors;
    S := Value;  // Convert to Pascal String
    Found := FALSE;
    ActiveSave := lst.ActiveIndex;
    elem := lst.First;
    While elem <> NIL Do Begin
      IF (CompareText(elem.Name, S) = 0) THEN Begin
        ActiveCircuit[ActiveActor].ActiveCktElement := elem;
        Found := TRUE;
        Break;
      End;
      elem := lst.Next;
    End;
    IF NOT Found THEN Begin
      DoSimpleMsg('Reactor "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);    // Restore active Reactor
      ActiveCircuit[ActiveActor].ActiveCktElement := elem;
    End;
  End;
end;
//------------------------------------------------------------------------------
FUNCTION Reactors_Get_Next():Integer;cdecl;
Var
  elem: TReactorObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then Begin
    lst := ActiveCircuit[ActiveActor].Reactors;
    elem := lst.Next;
    if elem <> nil then begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := elem;
          Result := lst.ActiveIndex;
        End
        Else elem := lst.Next;
      Until (Result > 0) or (elem = nil);
    End
  End;
end;
//------------------------------------------------------------------------------
FUNCTION Reactors_Get_Count():Integer;cdecl;
begin
    If Assigned(ActiveCircuit[ActiveActor]) Then
        Result := ActiveCircuit[ActiveActor].Reactors.ListSize;
end;
//------------------------------------------------------------------------------
function Reactors_Get_Bus1_AnsiString():AnsiString;inline;
var
    pReactor: TReactorObj;
begin
    Result := '';
    if ActiveReactor(pReactor) <> Nil    
    THEN Begin
        Result := pReactor.GetBus(1);
    End
end;

function Reactors_Get_Bus1():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Reactors_Get_Bus1_AnsiString());
end;
//------------------------------------------------------------------------------
function Reactors_Get_Bus2_AnsiString():AnsiString;inline;
var
    pReactor: TReactorObj;
begin
    Result := '';
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        Result := pReactor.GetBus(2);
    End
end;

function Reactors_Get_Bus2():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Reactors_Get_Bus2_AnsiString());
end;
//------------------------------------------------------------------------------
function Reactors_Get_LCurve_AnsiString():AnsiString;inline;
var
    pReactor: TReactorObj;
begin
    Result := '';
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        Result := pReactor.LCurve;
    End
end;

function Reactors_Get_LCurve():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Reactors_Get_LCurve_AnsiString());
end;
//------------------------------------------------------------------------------
function Reactors_Get_RCurve_AnsiString():AnsiString;inline;
var
    pReactor: TReactorObj;
begin
    Result := '';
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        Result := pReactor.RCurve;
    End
end;

function Reactors_Get_RCurve():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Reactors_Get_RCurve_AnsiString());
end;
//------------------------------------------------------------------------------
FUNCTION Reactors_Get_Parallel():WordBool;cdecl;
var
    pReactor: TReactorObj;
begin
    Result := FALSE;
    if ActiveReactor(pReactor) <> Nil    
    THEN Begin
        Result := pReactor.IsParallel;
    End
end;
//------------------------------------------------------------------------------
FUNCTION Reactors_Get_IsDelta():WordBool;cdecl;
var
    pReactor: TReactorObj;
begin
    Result := FALSE;
    if ActiveReactor(pReactor) <> Nil    
    THEN Begin
        if pReactor.Connection > 0 then Result := TRUE;
    End
end;
//------------------------------------------------------------------------------
FUNCTION Reactors_Get_kV():Double;cdecl;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if ActiveReactor(pReactor) <> Nil 
    THEN with pReactor do Begin
        Result := kvrating;
    End
end;
//------------------------------------------------------------------------------
FUNCTION Reactors_Get_kvar():Double;cdecl;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        Result := pReactor.kvarRating;
    End
end;
//------------------------------------------------------------------------------
FUNCTION Reactors_Get_LmH():Double;cdecl;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        Result := pReactor.L * 1000.0;
    End
end;
//------------------------------------------------------------------------------
function Reactors_Get_Phases():Integer;cdecl;
var
    pReactor: TReactorObj;
begin
    Result := 0;
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        Result := pReactor.Nphases;
    End
end;
//------------------------------------------------------------------------------
FUNCTION Reactors_Get_SpecType():Integer;cdecl;
var
    pReactor: TReactorObj;
begin
    Result := 0;
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        Result := pReactor.SpecType;
    End
end;
//------------------------------------------------------------------------------
FUNCTION Reactors_Get_R():Double;cdecl;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        Result := pReactor.R;
    End
end;
//------------------------------------------------------------------------------
FUNCTION Reactors_Get_X():Double;cdecl;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        Result := pReactor.X;
    End
end;
//------------------------------------------------------------------------------
FUNCTION Reactors_Get_Rp():Double;cdecl;
var
    pReactor: TReactorObj;
begin
    Result := 0.0;
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        Result := pReactor.Rp;
    End
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_IsDelta(Value: WordBool);cdecl;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        pReactor.Connection := Integer(Value);
        ReactorPropSideEffects(ReactorProps.conn, pReactor);
    End
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_Parallel(Value: WordBool);cdecl;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        pReactor.IsParallel := Value;
        ReactorPropSideEffects(ReactorProps.Parallel, pReactor);
    End
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Bus1(const Value: PAnsiChar);cdecl;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        pReactor.SetBus(1, Value);
        ReactorPropSideEffects(ReactorProps.bus1, pReactor);
    End
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_Bus2(const Value: PAnsiChar);cdecl;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        pReactor.SetBus(2, Value);
        ReactorPropSideEffects(ReactorProps.bus2, pReactor);
    End
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_LCurve(const Value: PAnsiChar);cdecl;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        pReactor.LCurve := Value;
        ReactorPropSideEffects(ReactorProps.LCurve, pReactor);
    End
end;
//------------------------------------------------------------------------------
procedure Reactors_Set_RCurve(const Value: PAnsiChar);cdecl;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        pReactor.RCurve := Value;
        ReactorPropSideEffects(ReactorProps.RCurve, pReactor);
    End
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_kV(Value: Double);cdecl;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        pReactor.kvrating := Value;
        ReactorPropSideEffects(ReactorProps.kv, pReactor);
    End
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_kvar(Value: Double);cdecl;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        pReactor.kvarRating := Value;
        ReactorPropSideEffects(ReactorProps.kvar, pReactor);
    End
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_LmH(Value: Double);cdecl;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        pReactor.L := Value / 1000.0;
        ReactorPropSideEffects(ReactorProps.LmH, pReactor);
    End
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_Phases(Value: Integer);cdecl;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        if (Value <> pReactor.NPhases) then
        begin
            pReactor.NPhases := Value;
            ReactorPropSideEffects(ReactorProps.phases, pReactor);
        end;
    End
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_R(Value: Double);cdecl;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        pReactor.R := Value;
        ReactorPropSideEffects(ReactorProps.R, pReactor);
    End
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_X(Value: Double);cdecl;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        pReactor.X := Value;
        ReactorPropSideEffects(ReactorProps.X, pReactor);
    End
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_Rp(Value: Double);cdecl;
var
    pReactor: TReactorObj;
begin
    if ActiveReactor(pReactor) <> Nil
    THEN Begin
        pReactor.Rp := Value;
        ReactorPropSideEffects(ReactorProps.Rp, pReactor);
    End
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_Rmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
    Value: PDoubleArray;
    k: Integer;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray(ValuePtr);
    if (ActiveReactor(pReactor) <> Nil) and (Sqr(pReactor.Nphases) = ValueCount)
    THEN WITH pReactor DO 
    Begin
        FOR k := 0 to ValueCount DO
        begin
            Rmatrix[k + 1] := ValuePtr[k];
        End;
        ReactorPropSideEffects(ReactorProps.Rmatrix, pReactor);
    End;
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_Xmatrix(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
    Value: PDoubleArray;
    k: Integer;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray(ValuePtr);
    if (ActiveReactor(pReactor) <> Nil) and (Sqr(pReactor.Nphases) = ValueCount)
    THEN WITH pReactor DO 
    Begin
        FOR k := 0 to ValueCount DO
        begin
            Xmatrix[k + 1] := ValuePtr[k];
        End;
        ReactorPropSideEffects(ReactorProps.Xmatrix, pReactor);
    End;
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
    Result: PDoubleArray;
    k:Integer;
    pReactor: TReactorObj;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if (ActiveReactor(pReactor) <> Nil) AND (pReactor.Rmatrix <> nil)
    THEN WITH pReactor DO 
    Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (Sqr(Nphases) - 1) + 1);
        FOR k := 0 to ResultCount[0] DO
        begin
            ResultPtr[k] := Rmatrix[k + 1];
        End;
    End;
end;
PROCEDURE Reactors_Get_Rmatrix_GR();cdecl;
// Same as Reactors_Get_Rmatrix but uses global result (GR) pointers
begin
   Reactors_Get_Rmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Reactors_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
    Result: PDoubleArray;
    k:Integer;
    pReactor: TReactorObj;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if (ActiveReactor(pReactor) <> Nil) AND (pReactor.Xmatrix <> nil)
    THEN WITH pReactor DO 
    Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (Sqr(Nphases) - 1) + 1);
        FOR k := 0 to ResultCount[0] DO
        begin
            ResultPtr[k] := Xmatrix[k + 1];
        End;
    End;
end;
PROCEDURE Reactors_Get_Xmatrix_GR();cdecl;
// Same as Reactors_Get_Xmatrix but uses global result (GR) pointers
begin
   Reactors_Get_Xmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Reactors_Get_Z1(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
    Result: PDoubleArray;
    pReactor: TReactorObj;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if ActiveReactor(pReactor) <> Nil
    THEN WITH pReactor DO 
    Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        Result[0] := Z1.Re;
        Result[1] := Z1.Im;
    End;
end;
PROCEDURE Reactors_Get_Z1_GR();cdecl;
// Same as Reactors_Get_Z1 but uses global result (GR) pointers
begin
   Reactors_Get_Z1(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Reactors_Get_Z2(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
    Result: PDoubleArray;
    pReactor: TReactorObj;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if ActiveReactor(pReactor) <> Nil
    THEN WITH pReactor DO 
    Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        Result[0] := Z2.Re;
        Result[1] := Z2.Im;
    End;
end;
PROCEDURE Reactors_Get_Z2_GR();cdecl;
// Same as Reactors_Get_Z2 but uses global result (GR) pointers
begin
   Reactors_Get_Z2(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Reactors_Get_Z0(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
    Result: PDoubleArray;
    pReactor: TReactorObj;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if ActiveReactor(pReactor) <> Nil
    THEN WITH pReactor DO 
    Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        Result[0] := Z0.Re;
        Result[1] := Z0.Im;
    End;
end;
PROCEDURE Reactors_Get_Z0_GR();cdecl;
// Same as Reactors_Get_Z0 but uses global result (GR) pointers
begin
   Reactors_Get_Z0(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Reactors_Get_Z(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
    Result: PDoubleArray;
    pReactor: TReactorObj;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if ActiveReactor(pReactor) <> Nil
    THEN WITH pReactor DO 
    Begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        Result[0] := R;
        Result[1] := X;
    End;
end;
PROCEDURE Reactors_Get_Z_GR();cdecl;
// Same as Reactors_Get_Z but uses global result (GR) pointers
begin
   Reactors_Get_Z(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_Z2(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
    Value: PDoubleArray;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray(ValuePtr);
    if (ActiveReactor(pReactor) <> Nil) and (ValueCount = 2)
    THEN WITH pReactor DO 
    Begin
        Z2 := Cmplx(Value[0], Value[1]);
        ReactorPropSideEffects(ReactorProps.Z2, pReactor);
    End;
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_Z1(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
    Value: PDoubleArray;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray(ValuePtr);
    if (ActiveReactor(pReactor) <> Nil) and (ValueCount = 2)
    THEN WITH pReactor DO 
    Begin
        Z1 := Cmplx(Value[0], Value[1]);
        ReactorPropSideEffects(ReactorProps.Z1, pReactor);
    End;
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_Z0(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
    Value: PDoubleArray;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray(ValuePtr);
    if (ActiveReactor(pReactor) <> Nil) and (ValueCount = 2)
    THEN WITH pReactor DO 
    Begin
        Z0 := Cmplx(Value[0], Value[1]);
        ReactorPropSideEffects(ReactorProps.Z0, pReactor);
    End;
end;
//------------------------------------------------------------------------------
PROCEDURE Reactors_Set_Z(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
    Value: PDoubleArray;
    pReactor: TReactorObj;
begin
    Value := PDoubleArray(ValuePtr);
    if (ActiveReactor(pReactor) <> Nil) and (ValueCount = 2)
    THEN WITH pReactor DO 
    Begin
        Z := Cmplx(Value[0], Value[1]);
        ReactorPropSideEffects(ReactorProps.Z, pReactor);
    End;
end;
//------------------------------------------------------------------------------

END.
