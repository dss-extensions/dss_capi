unit CAPI_DSS;

interface

uses
{$IFDEF WINDOWS}
    Windows,
{$ENDIF}
    CAPI_Utils,
    CAPI_Globals;

procedure DSS_NewCircuit(const Value: PAnsiChar); CDECL;
function DSS_Get_NumCircuits(): Integer; CDECL;
procedure DSS_ClearAll(); CDECL;
function DSS_Get_Version(): PAnsiChar; CDECL;
function DSS_Start(code: Integer): TAPIBoolean; CDECL;
procedure DSS_Get_Classes(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure DSS_Get_Classes_GR(); CDECL;
procedure DSS_Get_UserClasses(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure DSS_Get_UserClasses_GR(); CDECL;
function DSS_Get_NumClasses(): Integer; CDECL;
function DSS_Get_NumUserClasses(): Integer; CDECL;
function DSS_Get_DataPath(): PAnsiChar; CDECL;
procedure DSS_Set_DataPath(const Value: PAnsiChar); CDECL;
procedure DSS_Reset(); CDECL;
function DSS_Get_DefaultEditor(): PAnsiChar; CDECL;
function DSS_SetActiveClass(const ClassName: PAnsiChar): Integer; CDECL;
function DSS_Get_AllowForms: TAPIBoolean; CDECL;
procedure DSS_Set_AllowForms(Value: TAPIBoolean); CDECL;

// Extensions
function DSS_Get_AllowEditor: TAPIBoolean; CDECL;
procedure DSS_Set_AllowEditor(Value: TAPIBoolean); CDECL;
function DSS_Get_LegacyModels(): TAPIBoolean; CDECL;
procedure DSS_Set_LegacyModels(Value: TAPIBoolean); CDECL;
function DSS_Get_AllowChangeDir(): TAPIBoolean; CDECL;
procedure DSS_Set_AllowChangeDir(Value: TAPIBoolean); CDECL;
procedure DSS_RegisterPlotCallback(cb: dss_callback_plot_t); CDECL;
procedure DSS_RegisterMessageCallback(cb: dss_callback_message_t); CDECL;



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
function DSS_Get_AllowForms: TAPIBoolean; CDECL;
begin
    Result := not NoFormsAllowed;
end;
//------------------------------------------------------------------------------
procedure DSS_Set_AllowForms(Value: TAPIBoolean); CDECL;
begin
{$IFDEF WINDOWS}
    if (Value) and (GetConsoleWindow() = 0) and ((@DSSMessageCallback) = NIL) then
    begin
        DoSimplemsg('Cannot activate output with no console available! If you want to use a message output callback, register it before enabling AllowForms.', 5096);
        Exit;
    end;
{$ENDIF}

    NoFormsAllowed := not Value;
    if NoFormsAllowed then
        CloseDownForms;  // DSSForms

end;
//------------------------------------------------------------------------------
function DSS_Get_AllowEditor: TAPIBoolean; CDECL;
begin
    Result := DSS_CAPI_ALLOW_EDITOR;
end;
//------------------------------------------------------------------------------
procedure DSS_Set_AllowEditor(Value: TAPIBoolean); CDECL;
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
function DSS_Start(code: Integer): TAPIBoolean; CDECL;
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
        Result[k] := DSS_CopyStringAsPChar(TDSSClass(DSSClassList.Get(i)).Name);
        Inc(k);
    end;

end;

procedure DSS_Get_Classes_GR(); CDECL;
// Same as DSS_Get_Classes but uses global result (GR) pointers
begin
    DSS_Get_Classes(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
procedure DSS_Get_UserClasses(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray;
begin
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
    Result := NumIntrinsicClasses;
end;
//------------------------------------------------------------------------------
function DSS_Get_NumUserClasses(): Integer; CDECL;
begin
    Result := 0;
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
function DSS_Get_LegacyModels(): TAPIBoolean; CDECL;
begin
    Result := DSS_CAPI_LEGACY_MODELS;
end;
//------------------------------------------------------------------------------
procedure DSS_Set_LegacyModels(Value: TAPIBoolean); CDECL;
begin
    if (Value <> DSS_CAPI_LEGACY_MODELS) then
    begin
        DSS_CAPI_LEGACY_MODELS := Value;
        DSSExecutive.Command := 'clear';
    end;
end;
//------------------------------------------------------------------------------
function DSS_Get_AllowChangeDir(): TAPIBoolean; CDECL;
begin
    Result := DSS_CAPI_ALLOW_CHANGE_DIR;
end;
//------------------------------------------------------------------------------
procedure DSS_Set_AllowChangeDir(Value: TAPIBoolean); CDECL;
begin
    if (Value <> DSS_CAPI_ALLOW_CHANGE_DIR) then
    begin
        DSS_CAPI_ALLOW_CHANGE_DIR := Value;
        if not Value then
        begin
            SetCurrentDSSDir(GetCurrentDir());
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure DSS_RegisterPlotCallback(cb: dss_callback_plot_t); CDECL;
begin
    DSSPlotCallback := cb;
end;
//------------------------------------------------------------------------------
procedure DSS_RegisterMessageCallback(cb: dss_callback_message_t); CDECL;
begin
    DSSMessageCallback := cb;
    
{$IFDEF WINDOWS}
    // If we cannot get a console on Windows, disable text output when the
    // message callback is removed.
    if ((@DSSMessageCallback) = NIL) and (GetConsoleWindow() = 0) then
    begin
        NoFormsAllowed := True;
    end;
{$ENDIF}
    
end;
//------------------------------------------------------------------------------
end.
