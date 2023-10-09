unit CAPI_DSSProgress;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure DSSProgress_Close(); CDECL;
procedure DSSProgress_Set_Caption(const Value: PAnsiChar); CDECL;
procedure DSSProgress_Set_PctProgress(Value: Integer); CDECL;
procedure DSSProgress_Show(); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    DSSClass;

procedure DSSProgress_Close(); CDECL;
begin
    if NoFormsAllowed then
        Exit;
    DSSPrime.ProgressHide;
end;
//------------------------------------------------------------------------------
procedure DSSProgress_Set_Caption(const Value: PAnsiChar); CDECL;
begin
    if NoFormsAllowed then
        Exit;
    DSSPrime.InitProgressForm;
    DSSPrime.ProgressCaption(Value);
end;
//------------------------------------------------------------------------------
procedure DSSProgress_Set_PctProgress(Value: Integer); CDECL;
begin
    if NoFormsAllowed then
        Exit;
    DSSPrime.InitProgressForm;
    DSSPrime.ShowPctProgress(Value);
end;
//------------------------------------------------------------------------------
procedure DSSProgress_Show(); CDECL;
begin
    if NoFormsAllowed then
        Exit;
    DSSPrime.InitProgressForm;
    DSSPrime.ProgressFormCaption(' ');
    DSSPrime.ShowPctProgress(0);

end;
//------------------------------------------------------------------------------
end.
