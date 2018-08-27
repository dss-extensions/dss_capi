UNIT CAPI_DSS;
{$inline on}

INTERFACE

USES CAPI_Utils;

procedure DSS_NewCircuit(const Value: PAnsiChar);cdecl;
function DSS_Get_NumCircuits():Integer;cdecl;
procedure DSS_ClearAll();cdecl;
function DSS_Get_Version():PAnsiChar;cdecl;
function DSS_Start(code: Integer):WordBool;cdecl;
PROCEDURE DSS_Get_Classes(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE DSS_Get_UserClasses(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
function DSS_Get_NumClasses():Integer;cdecl;
function DSS_Get_NumUserClasses():Integer;cdecl;
function DSS_Get_DataPath():PAnsiChar;cdecl;
procedure DSS_Set_DataPath(const Value: PAnsiChar);cdecl;
procedure DSS_Reset();cdecl;
function DSS_Get_DefaultEditor():PAnsiChar;cdecl;
function DSS_SetActiveClass(const ClassName: PAnsiChar):Integer;cdecl;
function DSS_Get_AllowForms: WordBool;cdecl;
procedure DSS_Set_AllowForms(Value: WordBool);cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSClassDefs, DSSGlobals, DSSClass, Exechelper, sysUtils, Executive, CmdForms;


procedure DSS_NewCircuit(const Value: PAnsiChar);cdecl;
begin
    MakeNewCircuit(Value);
end;
//------------------------------------------------------------------------------
function DSS_Get_AllowForms: WordBool;cdecl;
begin
     Result := Not NoFormsAllowed;
end;
//------------------------------------------------------------------------------
procedure DSS_Set_AllowForms(Value: WordBool);cdecl;
begin
     {If Not Value Then} NoFormsAllowed := Not Value;
     If NoFormsAllowed Then CloseDownForms;  // DSSForms

end;
//------------------------------------------------------------------------------
function DSS_Get_NumCircuits():Integer;cdecl;
begin
    Result := ActiveCircuit[ActiveActor].NumCircuits;
end;
//------------------------------------------------------------------------------
procedure DSS_ClearAll();cdecl;
begin
    DoClearCmd;
end;
//------------------------------------------------------------------------------
function DSS_Get_Version_AnsiString():AnsiString;inline;
begin
     Result := VersionString +'; License Status: Open ' ;
end;

function DSS_Get_Version():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Get_Version_AnsiString());
end;
//------------------------------------------------------------------------------
function DSS_Start(code: Integer):WordBool;cdecl;
{Place any start code here}
begin
    Result :=  TRUE;
end;
//------------------------------------------------------------------------------
PROCEDURE DSS_Get_Classes(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  i,k:Integer;

Begin

   Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumIntrinsicClasses-1) + 1);
   k:=0;
   For i := 1 to NumIntrinsicClasses Do
   Begin
      Result[k] := DSS_CopyStringAsPChar(TDSSClass(DssClassList[ActiveActor].Get(i)).Name);
      Inc(k);
   End;

end;
//------------------------------------------------------------------------------
PROCEDURE DSS_Get_UserClasses(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  i,k:Integer;

Begin
     If NumUserClasses > 0 Then
     Begin
         Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumUserClasses-1) + 1);
         k:=0;
         For i := NumIntrinsicClasses+1 To DSSClassList[ActiveActor].ListSize   Do
         Begin
            Result[k] := DSS_CopyStringAsPChar(TDSSClass(DssClassList[ActiveActor].Get(i)).Name);
            Inc(k);
         End;
     End
     Else
     Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
end;
//------------------------------------------------------------------------------
function DSS_Get_NumClasses():Integer;cdecl;
begin

        Result := NumIntrinsicClasses;

end;
//------------------------------------------------------------------------------
function DSS_Get_NumUserClasses():Integer;cdecl;
begin
     Result := NumUserClasses;
end;
//------------------------------------------------------------------------------
function DSS_Get_DataPath_AnsiString():AnsiString;inline;
begin
     Result := DataDirectory[ActiveActor];
end;

function DSS_Get_DataPath():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Get_DataPath_AnsiString());
end;
//------------------------------------------------------------------------------
procedure DSS_Set_DataPath(const Value: PAnsiChar);cdecl;
begin
    SetDataPath(Value);
end;
//------------------------------------------------------------------------------
procedure DSS_Reset();cdecl;
begin
        {Put any code here necessary to reset for specific systems};
end;
//------------------------------------------------------------------------------
function DSS_Get_DefaultEditor_AnsiString():AnsiString;inline;
begin
     Result := DSSGlobals.DefaultEditor;
end;

function DSS_Get_DefaultEditor():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Get_DefaultEditor_AnsiString());
end;
//------------------------------------------------------------------------------
function DSS_SetActiveClass(const ClassName: PAnsiChar):Integer;cdecl;
Var
   DevClassIndex :Integer;

begin
     Result := 0;
     DevClassIndex := ClassNames[ActiveActor].Find(ClassName);
     If DevClassIndex = 0 Then  Begin
        DoSimplemsg('Error: Class ' + ClassName + ' not found.' , 5016);
        Exit;
     End;

     LastClassReferenced[ActiveActor] := DevClassIndex;
     ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
     Result := LastClassReferenced[ActiveActor];

end;
//------------------------------------------------------------------------------
END.
