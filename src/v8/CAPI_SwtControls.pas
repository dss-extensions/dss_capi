UNIT CAPI_SwtControls;
{$inline on}

INTERFACE

USES CAPI_Utils;

function SwtControls_Get_Action():Integer;cdecl;
PROCEDURE SwtControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE SwtControls_Get_AllNames_GR();cdecl;
function SwtControls_Get_Delay():Double;cdecl;
function SwtControls_Get_First():Integer;cdecl;
function SwtControls_Get_IsLocked():WordBool;cdecl;
function SwtControls_Get_Name():PAnsiChar;cdecl;
function SwtControls_Get_Next():Integer;cdecl;
function SwtControls_Get_SwitchedObj():PAnsiChar;cdecl;
function SwtControls_Get_SwitchedTerm():Integer;cdecl;
procedure SwtControls_Set_Action(Value: Integer);cdecl;
procedure SwtControls_Set_Delay(Value: Double);cdecl;
procedure SwtControls_Set_IsLocked(Value: WordBool);cdecl;
procedure SwtControls_Set_Name(const Value: PAnsiChar);cdecl;
procedure SwtControls_Set_SwitchedObj(const Value: PAnsiChar);cdecl;
procedure SwtControls_Set_SwitchedTerm(Value: Integer);cdecl;
function SwtControls_Get_Count():Integer;cdecl;
function SwtControls_Get_NormalState():Integer;cdecl;
procedure SwtControls_Set_NormalState(Value: Integer);cdecl;
function SwtControls_Get_State():Integer;cdecl;
procedure SwtControls_Set_State(Value: Integer);cdecl;
procedure SwtControls_Reset();cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Executive, ControlElem, SwtControl, SysUtils, PointerList;

function ActiveSwtControl: TSwtControlObj;
begin
  Result := nil;
  if ActiveCircuit[ActiveActor] <> Nil then Result := ActiveCircuit[ActiveActor].SwtControls.Active;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit[ActiveActor]) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('swtcontrol.%s.%s=%s', [ActiveSwtControl.Name, parm, val]);
  DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Action():Integer;cdecl;
var
  elem: TSwtControlObj;
begin
  Result := dssActionNone;
  elem := ActiveSwtControl;
  if elem <> nil then begin
    Case elem.CurrentAction of
      CTRL_OPEN: Result := dssActionOpen;
      CTRL_CLOSE: Result := dssActionClose;
    End;
  end;
end;
//------------------------------------------------------------------------------
PROCEDURE SwtControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  elem: TSwtControlObj;
  lst: TPointerList;
  k: Integer;
Begin
  Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
  Result[0] := DSS_CopyStringAsPChar('NONE');
  IF ActiveCircuit[ActiveActor] <> Nil THEN WITH ActiveCircuit[ActiveActor] DO
  If SwtControls.ListSize > 0 Then
  Begin
    lst := SwtControls;
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (lst.ListSize-1) + 1);
    k:=0;
    elem := lst.First;
    WHILE elem<>Nil DO Begin
      Result[k] := DSS_CopyStringAsPChar(elem.Name);
      Inc(k);
      elem := lst.Next;
    End;
  End;
end;
PROCEDURE SwtControls_Get_AllNames_GR();cdecl;
// Same as SwtControls_Get_AllNames but uses global result (GR) pointers
begin
   SwtControls_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function SwtControls_Get_Delay():Double;cdecl;
var
  elem: TSwtControlObj;
begin
  Result := 0.0;
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.TimeDelay;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_First():Integer;cdecl;
Var
  elem: TSwtControlObj;
  lst:  TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then begin
    lst := ActiveCircuit[ActiveActor].SwtControls;
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
function SwtControls_Get_IsLocked():WordBool;cdecl;
var
  elem: TSwtControlObj;
begin
  Result := FALSE;
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.IsLocked;   // Fixed bug here
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Name_AnsiString():AnsiString;inline;
var
  elem: TSwtControlObj;
begin
  Result := '';
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.Name;
end;

function SwtControls_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(SwtControls_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Next():Integer;cdecl;
Var
  elem: TSwtControlObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then Begin
    lst := ActiveCircuit[ActiveActor].SwtControls;
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
function SwtControls_Get_SwitchedObj_AnsiString():AnsiString;inline;
var
  elem: TSwtControlObj;
begin
  Result := '';
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.ElementName;
end;

function SwtControls_Get_SwitchedObj():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(SwtControls_Get_SwitchedObj_AnsiString());
end;
//------------------------------------------------------------------------------
function SwtControls_Get_SwitchedTerm():Integer;cdecl;
var
  elem: TSwtControlObj;
begin
  Result := 0;
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_Action(Value: Integer);cdecl;
var
  elem: TSwtControlObj;
begin
  elem := ActiveSwtControl;
  if elem <> nil then begin
    Case Value of
      dssActionOpen:  elem.CurrentAction := CTRL_OPEN;
      dssActionClose: elem.CurrentAction := CTRL_CLOSE;
      dssActionReset: elem.Reset;
      dssActionLock:  elem.Locked := TRUE;
      dssActionUnlock: elem.Locked := FALSE;
      else // TapUp, TapDown, None have no effect
    End;
    // Make sure the NormalState has an initial value  before taking action
    if elem.NormalState = CTRL_NONE then
       case value of
          dssActionOpen:  elem.NormalState := CTRL_OPEN;
          dssActionClose: elem.NormalState := CTRL_CLOSE;
       end;
  end;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_Delay(Value: Double);cdecl;
var
  elem: TSwtControlObj;
begin
  elem := ActiveSwtControl;
  if elem <> nil then begin
      elem.TimeDelay  := Value;
  end;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_IsLocked(Value: WordBool);cdecl;
var
  elem: TSwtControlObj;
begin
  elem := ActiveSwtControl;
  if elem <> nil then begin
      elem.Locked := Value;
  end;

end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_Name(const Value: PAnsiChar);cdecl;
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TSwtControlObj;
  lst: TPointerList;
Begin
  IF ActiveCircuit[ActiveActor] <> NIL THEN Begin
    lst := ActiveCircuit[ActiveActor].SwtControls;
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
      DoSimpleMsg('SwtControl "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);    // Restore active Load
      ActiveCircuit[ActiveActor].ActiveCktElement := elem;
    End;
  End;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_SwitchedObj(const Value: PAnsiChar);cdecl;
begin
  Set_Parameter ('SwitchedObj', Value);
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_SwitchedTerm(Value: Integer);cdecl;
begin
  Set_Parameter ('SwitchedTerm', IntToStr (Value));
end;
//------------------------------------------------------------------------------
function SwtControls_Get_Count():Integer;cdecl;
begin
     If Assigned(ActiveCircuit[ActiveActor]) Then
             Result := ActiveCircuit[ActiveActor].SwtControls.ListSize;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_NormalState():Integer;cdecl;
Var
  elem: TSwtControlObj;
begin
  elem := ActiveSwtControl;
  if elem <> nil then begin
      case elem.NormalState  of
        CTRL_OPEN: Result := dssActionOpen;
      else
        Result := dssActionClose;
      end;
  end;

end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_NormalState(Value: Integer);cdecl;
Var
  elem: TSwtControlObj;
begin
  elem := ActiveSwtControl;
  if elem <> nil then begin
     case Value of
         dssActionOpen:  elem.NormalState := CTRL_OPEN;
     else
         elem.NormalState := CTRL_CLOSE;
     end;
  end;
end;
//------------------------------------------------------------------------------
function SwtControls_Get_State():Integer;cdecl;
var
  elem: TSwtControlObj;
begin
  Result := dssActionNone;
  elem   := ActiveSwtControl;
  if elem <> nil then begin
    Case elem.PresentState   of
      CTRL_OPEN:  Result := dssActionOpen;
      CTRL_CLOSE: Result := dssActionClose;
    End;
  end;
end;
//------------------------------------------------------------------------------
procedure SwtControls_Set_State(Value: Integer);cdecl;
var
  elem: TSwtControlObj;
begin
  elem   := ActiveSwtControl;
  if elem <> nil then begin
    Case value   of
      dssActionOpen:  elem.PresentState := CTRL_OPEN;
      dssActionClose: elem.PresentState := CTRL_CLOSE;
    End;
  end;

end;
//------------------------------------------------------------------------------
procedure SwtControls_Reset();cdecl;
var
  elem: TSwtControlObj;
begin
  elem   := ActiveSwtControl;
  if elem <> nil then begin
      elem.Locked := FALSE;
      elem.Reset;
  end;
end;
//------------------------------------------------------------------------------
END.
