unit CAPI_DSS;

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
procedure DSS_Get_Classes(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure DSS_Get_UserClasses(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
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
function DSS_Get_LegacyModels(): Boolean; CDECL;
procedure DSS_Set_LegacyModels(Value: Boolean); CDECL;

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
    CmdForms;

procedure DSS_NewCircuit(const Value: PAnsiChar); CDECL;
begin
    MakeNewCircuit(Value);
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
        DoSimplemsg('Cannot activate output with no console available!', 5096);
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
    Result := NumCircuits;
end;
//------------------------------------------------------------------------------
procedure DSS_ClearAll(); CDECL;
begin
    DoClearCmd;
end;
//------------------------------------------------------------------------------
function DSS_Get_Version(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(VersionString + '; License Status: Open ');
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
      DSSExecutive := TExecutive.Create;  // Start the DSS when DSS interface is created
      DSSExecutive.CreateDefaultDSSItems;
  *)
end;
//------------------------------------------------------------------------------
procedure DSS_Get_Classes(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray;
    i, k: Integer;

begin

    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumIntrinsicClasses - 1) + 1);
    k := 0;
    for i := 1 to NumIntrinsicClasses do
    begin
        Result[k] := DSS_CopyStringAsPChar(TDSSClass(DssClassList.Get(i)).Name);
        Inc(k);
    end;

end;
//------------------------------------------------------------------------------
procedure DSS_Get_UserClasses(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray;
    i, k: Integer;

begin
    if NumUserClasses > 0 then
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (NumUserClasses - 1) + 1);
        k := 0;
        for i := NumIntrinsicClasses + 1 to DSSClassList.ListSize do
        begin
            Result[k] := DSS_CopyStringAsPChar(TDSSClass(DssClassList.Get(i)).Name);
            Inc(k);
        end;
    end
    else
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
end;
//------------------------------------------------------------------------------
function DSS_Get_NumClasses(): Integer; CDECL;
begin
    Result := NumIntrinsicClasses;
end;
//------------------------------------------------------------------------------
function DSS_Get_NumUserClasses(): Integer; CDECL;
begin
    Result := NumUserClasses;
end;
//------------------------------------------------------------------------------
function DSS_Get_DataPath(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DataDirectory);
end;
//------------------------------------------------------------------------------
procedure DSS_Set_DataPath(const Value: PAnsiChar); CDECL;
begin
    SetDataPath(Value);
end;
//------------------------------------------------------------------------------
procedure DSS_Reset(); CDECL;
begin
     {Put any code here necessary to reset for specific systems};
 // revert to original -- DSSExecutive.Free;
end;
//------------------------------------------------------------------------------
function DSS_Get_DefaultEditor(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSGlobals.DefaultEditor);
end;
//------------------------------------------------------------------------------
function DSS_SetActiveClass(const ClassName: PAnsiChar): Integer; CDECL;
var
    DevClassIndex: Integer;
begin
    Result := 0;
    DevClassIndex := ClassNames.Find(ClassName);
    if DevClassIndex = 0 then
    begin
        DoSimplemsg('Error: Class ' + ClassName + ' not found.', 5016);
        Exit;
    end;

    LastClassReferenced := DevClassIndex;
    ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
    Result := LastClassReferenced;
end;
//------------------------------------------------------------------------------
function DSS_Get_LegacyModels(): Boolean; CDECL;
begin
    Result := DSS_CAPI_LEGACY_MODELS;
end;
//------------------------------------------------------------------------------
procedure DSS_Set_LegacyModels(Value: Boolean); CDECL;
begin
    if (Value <> DSS_CAPI_LEGACY_MODELS) then
    begin
        DSS_CAPI_LEGACY_MODELS := Value;
        DSSExecutive.Command := 'clear';
    end;
end;
//------------------------------------------------------------------------------
end.
