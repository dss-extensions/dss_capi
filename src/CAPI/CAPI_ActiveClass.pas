unit CAPI_ActiveClass;

interface

uses
    CAPI_Utils;

procedure ActiveClass_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure ActiveClass_Get_AllNames_GR(); CDECL;
function ActiveClass_Get_First(): Integer; CDECL;
function ActiveClass_Get_Next(): Integer; CDECL;
function ActiveClass_Get_Name(): PAnsiChar; CDECL;
procedure ActiveClass_Set_Name(const Value: PAnsiChar); CDECL;
function ActiveClass_Get_NumElements(): Integer; CDECL;
function ActiveClass_Get_ActiveClassName(): PAnsiChar; CDECL;
function ActiveClass_Get_Count(): Integer; CDECL;
function ActiveClass_Get_ActiveClassParent(): PAnsiChar; CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    DSSObject,
    CktElement,
    PCClass, 
    PDClass, 
    MeterClass, 
    ControlClass;

procedure ActiveClass_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray;
    idx: Integer;
    k: Integer;

begin
    if (InvalidCircuit) or (ActiveDSSClass = NIL) then
    begin
        DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
        Exit;
    end;
        
    with ActiveCircuit do
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, ActiveDSSClass.ElementCount);
        k := 0;
        idx := ActiveDSSClass.First;
        while idx > 0 do
        begin
            Result[k] := DSS_CopyStringAsPChar(ActiveDSSObject.Name);
            Inc(k);
            idx := ActiveDSSClass.Next;
        end;
    end;
end;

procedure ActiveClass_Get_AllNames_GR(); CDECL;
// Same as ActiveClass_Get_AllNames but uses global result (GR) pointers
begin
    ActiveClass_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function ActiveClass_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if (InvalidCircuit) or (ActiveDSSClass = NIL) then
        Exit;
    Result := ActiveDSSClass.First;  // sets active objects
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if (InvalidCircuit) or (ActiveDSSClass = NIL) then
        Exit;
    Result := ActiveDSSClass.Next;  // sets active objects
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_Name(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if ActiveDSSObject = NIL then
        Exit;
    
    Result := DSS_GetAsPAnsiChar(ActiveDSSObject.Name)
end;
//------------------------------------------------------------------------------
procedure ActiveClass_Set_Name(const Value: PAnsiChar); CDECL;
// set object active by name
var
    pelem: TDSSObject;
begin
    if ActiveDSSClass = NIL then
        Exit;
        
    pelem := ActiveDSSClass.Find(Value);
    if pelem = NIL then
        Exit;

    if pelem is TDSSCktElement then
        ActiveCircuit.ActiveCktElement := TDSSCktElement(pelem)  // sets ActiveDSSobject
    else
        ActiveDSSObject := pelem;
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_NumElements(): Integer; CDECL;
begin
    Result := 0;
    if ActiveDSSClass = NIL then
        Exit;
    Result := ActiveDSSCLass.ElementCount
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_ActiveClassName(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if ActiveDSSClass = NIL then
        Exit;
    Result := DSS_GetAsPAnsiChar(ActiveDSSCLass.Name)
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if ActiveDSSClass = NIL then
        Exit;
    Result := ActiveDSSCLass.ElementCount
end;
//------------------------------------------------------------------------------
function ActiveClass_Get_ActiveClassParent(): PAnsiChar;
begin
    if ActiveDSSClass = NIL then
    begin
        Result := DSS_GetAsPAnsiChar('Parent Class unknonwn');
        Exit;
    end;

    if ActiveDSSClass.ClassType.InheritsFrom(TMeterClass) then
        Result := DSS_GetAsPAnsiChar('TMeterClass')
    else if ActiveDSSClass.ClassType.InheritsFrom(TControlClass) then
        Result := DSS_GetAsPAnsiChar('TControlClass')
    else  if ActiveDSSClass.ClassType.InheritsFrom(TPDClass) then
        Result := DSS_GetAsPAnsiChar('TPDClass')
    else if ActiveDSSClass.ClassType.InheritsFrom(TPCClass) then
        Result := DSS_GetAsPAnsiChar('TPCClas') //NOTE: kept as "Clas" for compatibility
    else 
        Result := DSS_GetAsPAnsiChar('Generic Object');
end;
//------------------------------------------------------------------------------
end.
