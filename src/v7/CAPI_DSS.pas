unit CAPI_DSS;

{$inline on}

interface

uses
{$IFDEF WINDOWS}
    Windows,
{$ENDIF}
    CAPI_Utils;

procedure DSS_NewCircuit(const Value: PAnsiChar); CDECL;
function DSS_Get_NumCircuits(): Integer; CDECL;
procedure DSS_ClearAll(); CDECL;
function DSS_Get_Version(): PAnsiChar; CDECL;
function DSS_Start(code: Integer): Boolean; CDECL;
procedure DSS_Get_Classes(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure DSS_Get_Classes_GR(); CDECL;
procedure DSS_Get_UserClasses(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure DSS_Get_UserClasses_GR(); CDECL;
function DSS_Get_NumClasses(): Integer; CDECL;
function DSS_Get_NumUserClasses(): Integer; CDECL;
function DSS_Get_DataPath(): PAnsiChar; CDECL;
procedure DSS_Set_DataPath(const Value: PAnsiChar); CDECL;
procedure DSS_Reset(); CDECL;
function DSS_Get_DefaultEditor(): PAnsiChar; CDECL;
function DSS_SetActiveClass(const ClassName: PAnsiChar): Integer; CDECL;
function DSS_Get_AllowForms: Boolean; CDECL;
procedure DSS_Set_AllowForms(Value: Boolean); CDECL;
function DSS_Get_AllowEditor: Boolean; CDECL;
procedure DSS_Set_AllowEditor(Value: Boolean); CDECL;

implementation

uses
    CAPI_Constants,
    DSSClassDefs,
    DSSGlobals,
    DSSClass,
    Exechelper,
    sysUtils,
    Executive,
    ParserDel,
    CmdForms,
    DSSHelper;

procedure DSS_NewCircuit(const Value: PAnsiChar); CDECL;
begin
    MakeNewCircuit(DSSPrime, Value);
end;
//------------------------------------------------------------------------------
function DSS_Get_AllowForms: Boolean; CDECL;
begin
    Result := not NoFormsAllowed;
end;
//------------------------------------------------------------------------------
procedure DSS_Set_AllowForms(Value: Boolean); CDECL;
begin
{$IFDEF WINDOWS}
    if (Value) and (GetConsoleWindow() = 0) then
    begin
        DoSimpleMsg(DSSPrime, 'Cannot activate output with no console available!', 5096);
        Exit;
    end;
{$ENDIF}

    NoFormsAllowed := not Value;
    if NoFormsAllowed then
        CloseDownForms;  // DSSForms

end;
//------------------------------------------------------------------------------
function DSS_Get_AllowEditor: Boolean; CDECL;
begin
    Result := DSS_CAPI_ALLOW_EDITOR;
end;
//------------------------------------------------------------------------------
procedure DSS_Set_AllowEditor(Value: Boolean); CDECL;
begin
    DSS_CAPI_ALLOW_EDITOR := not (not Value);
end;
//------------------------------------------------------------------------------
function DSS_Get_NumCircuits(): Integer; CDECL;
begin
    Result := DSSPrime.NumCircuits;
end;
//------------------------------------------------------------------------------
procedure DSS_ClearAll(); CDECL;
begin
    DSSPrime.DSSExecutive.DoClearCmd;
end;
//------------------------------------------------------------------------------
function DSS_Get_Version_AnsiString(): Ansistring; inline;
begin
    Result := VersionString + '; License Status: Open ';
end;

function DSS_Get_Version(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Get_Version_AnsiString());
end;
//------------------------------------------------------------------------------
function DSS_Start(code: Integer): Boolean; CDECL;
{Place any start code here}
begin
    Result := TRUE;
 (*      Reverted to original method. 3/1/17. see dpr file
      InitializeInterfaces;
      IsDLL := TRUE;
    {Create one instance of DSS executive whenever the DSS Engine is init'd}
      DSSPrime.DSSExecutive := TExecutive.Create;  // Start the DSS when DSS interface is created
      DSSPrime.DSSExecutive.CreateDefaultDSSItems;
  *)
end;
//------------------------------------------------------------------------------
procedure DSS_Get_Classes(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    i, k: Integer;

begin

    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (DSSPrime.NumIntrinsicClasses - 1) + 1);
    k := 0;
    for i := 1 to DSSPrime.NumIntrinsicClasses do
    begin
        Result[k] := DSS_CopyStringAsPChar(TDSSClass(DSSPrime.DssClassList.Get(i)).Name);
        Inc(k);
    end;

end;

procedure DSS_Get_Classes_GR(); CDECL;
// Same as DSS_Get_Classes but uses global result (GR) pointers
begin
    DSS_Get_Classes(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
procedure DSS_Get_UserClasses(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    i, k: Integer;

begin
    if DSSPrime.NumUserClasses > 0 then
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (DSSPrime.NumUserClasses - 1) + 1);
        k := 0;
        for i := DSSPrime.NumIntrinsicClasses + 1 to DSSPrime.DSSClassList.ListSize do
        begin
            Result[k] := DSS_CopyStringAsPChar(TDSSClass(DSSPrime.DssClassList.Get(i)).Name);
            Inc(k);
        end;
    end
    else
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
end;

procedure DSS_Get_UserClasses_GR(); CDECL;
// Same as DSS_Get_UserClasses but uses global result (GR) pointers
begin
    DSS_Get_UserClasses(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function DSS_Get_NumClasses(): Integer; CDECL;
begin

    Result := DSSPrime.NumIntrinsicClasses;

end;
//------------------------------------------------------------------------------
function DSS_Get_NumUserClasses(): Integer; CDECL;
begin
    Result := DSSPrime.NumUserClasses;
end;
//------------------------------------------------------------------------------
function DSS_Get_DataPath_AnsiString(): Ansistring; inline;
begin
    Result := DSSPrime.DataDirectory;
end;

function DSS_Get_DataPath(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Get_DataPath_AnsiString());
end;
//------------------------------------------------------------------------------
procedure DSS_Set_DataPath(const Value: PAnsiChar); CDECL;
begin
    SetDataPath(DSSPrime, Value);
end;
//------------------------------------------------------------------------------
procedure DSS_Reset(); CDECL;
begin
     {Put any code here necessary to reset for specific systems};
 // revert to original -- DSSPrime.DSSExecutive.Free;

end;
//------------------------------------------------------------------------------
function DSS_Get_DefaultEditor_AnsiString(): Ansistring; inline;
begin
    Result := DSSGlobals.DefaultEditor;
end;

function DSS_Get_DefaultEditor(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSS_Get_DefaultEditor_AnsiString());
end;
//------------------------------------------------------------------------------
function DSS_SetActiveClass(const ClassName: PAnsiChar): Integer; CDECL;
var
    DevClassIndex: Integer;

begin
    Result := 0;
    DevClassIndex := DSSPrime.ClassNames.Find(ClassName);
    if DevClassIndex = 0 then
    begin
        DoSimpleMsg(DSSPrime, 'Error: Class ' + ClassName + ' not found.', 5016);
        Exit;
    end;

    DSSPrime.LastClassReferenced := DevClassIndex;
    DSSPrime.ActiveDSSClass := DSSPrime.DSSClassList.Get(DSSPrime.LastClassReferenced);
    Result := DSSPrime.LastClassReferenced;

end;
//------------------------------------------------------------------------------
end.
