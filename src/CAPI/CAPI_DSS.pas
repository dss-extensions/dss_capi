unit CAPI_DSS;

interface

uses
{$IFDEF WINDOWS}
    Windows,
{$ENDIF}
    DSSClass,
    CAPI_Utils,
    CAPI_Types;

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
function DSS_Get_AllowForms(): TAPIBoolean; CDECL;
procedure DSS_Set_AllowForms(Value: TAPIBoolean); CDECL;

// Extensions
function DSS_Get_AllowEditor(): TAPIBoolean; CDECL;
procedure DSS_Set_AllowEditor(Value: TAPIBoolean); CDECL;
function DSS_Get_LegacyModels(): TAPIBoolean; CDECL;
procedure DSS_Set_LegacyModels(Value: TAPIBoolean); CDECL;
function DSS_Get_COMErrorResults(): TAPIBoolean; CDECL;
procedure DSS_Set_COMErrorResults(Value: TAPIBoolean); CDECL;
function DSS_Get_AllowChangeDir(): TAPIBoolean; CDECL;
procedure DSS_Set_AllowChangeDir(Value: TAPIBoolean); CDECL;
procedure DSS_RegisterPlotCallback(cb: dss_callback_plot_t); CDECL;
procedure DSS_RegisterMessageCallback(cb: dss_callback_message_t); CDECL;
function DSS_Get_AllowDOScmd(): TAPIBoolean; CDECL;
procedure DSS_Set_AllowDOScmd(Value: TAPIBoolean); CDECL;

implementation

uses
    CAPI_Constants,
    DSSClassDefs,
    DSSGlobals,
    Exechelper,
    sysUtils,
    Executive,
    CmdForms,
    DSSHelper,
    Classes;

procedure DSS_NewCircuit(const Value: PAnsiChar); CDECL;
begin
    MakeNewCircuit(DSSPrime, Value);
end;
//------------------------------------------------------------------------------
function DSS_Get_AllowForms(): TAPIBoolean; CDECL;
begin
    Result := not NoFormsAllowed;
end;
//------------------------------------------------------------------------------
procedure DSS_Set_AllowForms(Value: TAPIBoolean); CDECL;
begin
{$IFDEF WINDOWS}
    if (Value) and (GetConsoleWindow() = 0) and ((@DSSPrime.DSSMessageCallback) = NIL) then
    begin
        DoSimplemsg(DSSPrime, _('Cannot activate output with no console available! If you want to use a message output callback, register it before enabling AllowForms.'), 5096);
        Exit;
    end;
{$ENDIF}

    NoFormsAllowed := not Value;
    if NoFormsAllowed then
        CloseDownForms;  // DSSForms

end;
//------------------------------------------------------------------------------
function DSS_Get_AllowEditor(): TAPIBoolean; CDECL;
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
    Result := DSSPrime.NumCircuits;
end;
//------------------------------------------------------------------------------
procedure DSS_ClearAll(); CDECL;
begin
{$IFDEF DSS_CAPI_PM}
    DSSPrime.DSSExecutive.DoClearAllCmd;
{$ELSE}
    DSSPrime.DSSExecutive.DoClearCmd;
{$ENDIF}
end;
//------------------------------------------------------------------------------
function DSS_Get_Version(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime, VersionString + '; License Status: Open ');
end;
//------------------------------------------------------------------------------
function DSS_Start(code: Integer): TAPIBoolean; CDECL;
begin
    Result := TRUE;
    try
        DSS_InitThreads();
    except
        Result := FALSE;
    end;
end;
//------------------------------------------------------------------------------
procedure DSS_Get_Classes(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    i, k: Integer;

begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, DSSPrime.NumIntrinsicClasses);
    k := 0;
    for i := 1 to DSSPrime.NumIntrinsicClasses do
    begin
        Result[k] := DSS_CopyStringAsPChar(TDSSClass(DSSPrime.DSSClassList.Get(i)).Name);
        Inc(k);
    end;

end;

procedure DSS_Get_Classes_GR(); CDECL;
// Same as DSS_Get_Classes but uses global result (GR) pointers
begin
    DSS_Get_Classes(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
procedure DSS_Get_UserClasses(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount, '');
end;

procedure DSS_Get_UserClasses_GR(); CDECL;
// Same as DSS_Get_UserClasses but uses global result (GR) pointers
begin
    DSS_Get_UserClasses(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function DSS_Get_NumClasses(): Integer; CDECL;
begin
    Result := DSSPrime.NumIntrinsicClasses;
end;
//------------------------------------------------------------------------------
function DSS_Get_NumUserClasses(): Integer; CDECL;
begin
    Result := 0;
end;
//------------------------------------------------------------------------------
function DSS_Get_DataPath(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.DataDirectory);
end;
//------------------------------------------------------------------------------
procedure DSS_Set_DataPath(const Value: PAnsiChar); CDECL;
begin
    DSSPrime.SetCurrentDSSDir(Value);
    SetDataPath(DSSPrime, Value);
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
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSGlobals.DefaultEditor);
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
        DoSimpleMsg(DSSPrime, 'Class %s not found.', [ClassName], 5016);
        Exit;
    end;

    DSSPrime.LastClassReferenced := DevClassIndex;
    DSSPrime.ActiveDSSClass := DSSPrime.DSSClassList.Get(DSSPrime.LastClassReferenced);
    Result := DSSPrime.LastClassReferenced;
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
        DSSPrime.DSSExecutive.Command := 'clear';
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
            DSSPrime.SetCurrentDSSDir(GetCurrentDir());
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure DSS_RegisterPlotCallback(cb: dss_callback_plot_t); CDECL;
begin
    DSSPrime.DSSPlotCallback := cb;
end;
//------------------------------------------------------------------------------
procedure DSS_RegisterMessageCallback(cb: dss_callback_message_t); CDECL;
begin
    DSSPrime.DSSMessageCallback := cb;
    
{$IFDEF WINDOWS}
    // If we cannot get a console on Windows, disable text output when the
    // message callback is removed.
    if ((@DSSPrime.DSSMessageCallback) = NIL) and (GetConsoleWindow() = 0) then
    begin
        NoFormsAllowed := True;
    end;
{$ENDIF}
    
end;
//------------------------------------------------------------------------------
function DSS_Get_COMErrorResults(): TAPIBoolean; CDECL;
begin
    Result := DSS_CAPI_COM_DEFAULTS;
end;
//------------------------------------------------------------------------------
procedure DSS_Set_COMErrorResults(Value: TAPIBoolean); CDECL;
begin
    DSS_CAPI_COM_DEFAULTS := Value;
end;
//------------------------------------------------------------------------------
function DSS_Get_AllowDOScmd(): TAPIBoolean; CDECL;
begin
    Result := DSS_CAPI_ALLOW_DOSCMD;
end;
//------------------------------------------------------------------------------
procedure DSS_Set_AllowDOScmd(Value: TAPIBoolean); CDECL;
begin
    DSS_CAPI_ALLOW_DOSCMD := Value;
end;
//------------------------------------------------------------------------------
end.
