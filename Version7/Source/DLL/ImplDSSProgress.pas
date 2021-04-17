unit ImplDSSProgress;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSEngine_TLB,
    StdVcl;

type
    TDSSProgress = class(TAutoObject, IDSSProgress)
    PROTECTED
        procedure Close; SAFECALL;
        procedure Set_Caption(const Value: Widestring); SAFECALL;
        procedure Set_PctProgress(Value: Integer); SAFECALL;
        procedure Show; SAFECALL;
    { Protected declarations }
    end;

implementation

uses
    ComServ,
    DSSForms, {Progressform,} DSSGlobals;

procedure TDSSProgress.Close;
begin
    if NoFormsAllowed then
        Exit;
    ProgressHide;
end;

procedure TDSSProgress.Set_Caption(const Value: Widestring);
begin
    if NoFormsAllowed then
        Exit;
    InitProgressForm;
    ProgressCaption(Value);
end;

procedure TDSSProgress.Set_PctProgress(Value: Integer);
begin
    if NoFormsAllowed then
        Exit;
    InitProgressForm;
    ShowPctProgress(Value);
end;

procedure TDSSProgress.Show;
begin
    if NoFormsAllowed then
        Exit;
    InitProgressForm;
    ProgressFormCaption(' ');
    ShowPctProgress(0);

end;

initialization
    TAutoObjectFactory.Create(ComServer, TDSSProgress, Class_DSSProgress,
        ciInternal, tmApartment);

    // Progress Form Creation moved to DSSGlobals


finalization


end.
