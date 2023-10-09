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

// These were originally implemented (very differently) in the DSSEvents interface, 
// but they are simple functions here.
function DSSEvents_RegisterInitControls(cb: dss_callback_solution_t): TAPIBoolean; CDECL;
function DSSEvents_RegisterCheckControls(cb: dss_callback_solution_t): TAPIBoolean; CDECL;
function DSSEvents_RegisterStepControls(cb: dss_callback_solution_t): TAPIBoolean; CDECL;
function DSSEvents_UnregisterInitControls(cb: dss_callback_solution_t): TAPIBoolean; CDECL;
function DSSEvents_UnregisterCheckControls(cb: dss_callback_solution_t): TAPIBoolean; CDECL;
function DSSEvents_UnregisterStepControls(cb: dss_callback_solution_t): TAPIBoolean; CDECL;

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
function DSS_Get_EnableArrayDimensions(): TAPIBoolean; CDECL;
procedure DSS_Set_EnableArrayDimensions(Value: TAPIBoolean); CDECL;
function DSS_Get_CompatFlags(): LongWord; CDECL;
procedure DSS_Set_CompatFlags(Value: LongWord); CDECL;

implementation

uses
    CAPI_Constants,
    DSSClassDefs,
    DSSGlobals,
    Exechelper,
    sysUtils,
    Executive,
    DSSHelper,
    Classes,
    MathUtil;

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
    // if NoFormsAllowed then
    //     DSSPrime.CloseDownForms;  // DSSForms -- TODO: reimplement
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
    DSSPrime.DSSExecutive.ClearAll();
{$ELSE}
    DSSPrime.DSSExecutive.Clear();
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
     // Put any code here necessary to reset for specific systems;
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
    Result := False;
end;
//------------------------------------------------------------------------------
procedure DSS_Set_LegacyModels(Value: TAPIBoolean); CDECL;
begin
    DoSimpleMsg(DSSPrime, DSSTranslate('LegacyModels flag is not supported in this version.'), 5016);
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
function DSS_Get_EnableArrayDimensions(): TAPIBoolean; CDECL;
begin
    Result := DSS_EXTENSIONS_ARRAY_DIMS;
end;
//------------------------------------------------------------------------------
procedure DSS_Set_EnableArrayDimensions(Value: TAPIBoolean); CDECL;
begin
    DSS_EXTENSIONS_ARRAY_DIMS := Value;
    if not Value then
    begin
        // Clean-up any previous values to avoid issues in the consumers
        DSSPrime.GR_Counts_PPAnsiChar[2] := 0;
        DSSPrime.GR_Counts_PPAnsiChar[3] := 0;
        DSSPrime.GR_Counts_PDouble[2] := 0;
        DSSPrime.GR_Counts_PDouble[3] := 0;
        DSSPrime.GR_Counts_PInteger[2] := 0;
        DSSPrime.GR_Counts_PInteger[3] := 0;
        DSSPrime.GR_Counts_PByte[2] := 0;
        DSSPrime.GR_Counts_PByte[3] := 0;
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
function DSS_Get_CompatFlags(): LongWord; CDECL;
begin
    Result := DSS_EXTENSIONS_COMPAT;
end;
//------------------------------------------------------------------------------
procedure DSS_Set_CompatFlags(Value: LongWord); CDECL;
begin
    DSS_EXTENSIONS_COMPAT := Value;
    SelectAs2pVersion((DSS_EXTENSIONS_COMPAT and ord(TDSSCompatFlags.BadPrecision)) <> 0);
end;
//------------------------------------------------------------------------------
function removeFromArray(var cbs: dss_callbacks_solution_t; target: dss_callback_solution_t): Boolean;
var
    i, j: Integer;
begin
    Result := False;
    for i := 0 to High(cbs) do
    begin
        if @cbs[i] = @target then
        begin
            // Move any other callbacks back and resize
            for j := i + 1 to High(cbs) do
            begin
                cbs[j - 1] := @cbs[j];
            end;
            SetLength(cbs, Length(cbs) - 1);
            Result := True;
            Exit;
        end;
    end;
end;

function appendToArray(var cbs: dss_callbacks_solution_t; target: dss_callback_solution_t): Boolean;
var
    cb: dss_callback_solution_t;
begin
    Result := False;
    for cb in cbs do
    begin
        if @cb = @target then
            Exit; // already added, ignore
    end;
    SetLength(cbs, Length(cbs) + 1);
    cbs[High(cbs)] := @target;
    Result := True;
end;

function DSSEvents_RegisterInitControls(cb: dss_callback_solution_t): TAPIBoolean; CDECL;
begin
    Result := appendToArray(DSSPrime.DSSInitControlsCallbacks, @cb);
end;
function DSSEvents_RegisterCheckControls(cb: dss_callback_solution_t): TAPIBoolean; CDECL;
begin
    Result := appendToArray(DSSPrime.DSSCheckControlsCallbacks, @cb);
end;
function DSSEvents_RegisterStepControls(cb: dss_callback_solution_t): TAPIBoolean; CDECL;
begin
    Result := appendToArray(DSSPrime.DSSStepControlsCallbacks, @cb);
end;
function DSSEvents_UnregisterInitControls(cb: dss_callback_solution_t): TAPIBoolean; CDECL;
begin
    Result := removeFromArray(DSSPrime.DSSInitControlsCallbacks, @cb);
end;
function DSSEvents_UnregisterCheckControls(cb: dss_callback_solution_t): TAPIBoolean; CDECL;
begin
    Result := removeFromArray(DSSPrime.DSSCheckControlsCallbacks, @cb);
end;
function DSSEvents_UnregisterStepControls(cb: dss_callback_solution_t): TAPIBoolean; CDECL;
begin
    Result := removeFromArray(DSSPrime.DSSStepControlsCallbacks, @cb);
end;
//------------------------------------------------------------------------------
end.
