UNIT CAPI_DSSProgress;
{$inline on}

INTERFACE

USES CAPI_Utils;

procedure DSSProgress_Close();cdecl;
procedure DSSProgress_Set_Caption(const Value: PAnsiChar);cdecl;
procedure DSSProgress_Set_PctProgress(Value: Integer);cdecl;
procedure DSSProgress_Show();cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, CmdForms;

procedure DSSProgress_Close();cdecl;
begin
   If NoFormsAllowed Then Exit;
   ProgressHide(ActiveActor);
end;
//------------------------------------------------------------------------------
procedure DSSProgress_Set_Caption(const Value: PAnsiChar);cdecl;
begin
   If NoFormsAllowed Then Exit;
   InitProgressForm(ActiveActor);
   ProgressCaption ( Value, ActiveActor);
end;
//------------------------------------------------------------------------------
procedure DSSProgress_Set_PctProgress(Value: Integer);cdecl;
begin
   If NoFormsAllowed Then Exit;
   InitProgressForm(ActiveActor);
//   ShowPctProgress ( Value, ActiveActor);
end;
//------------------------------------------------------------------------------
procedure DSSProgress_Show();cdecl;
begin
   If NoFormsAllowed Then Exit;
        InitProgressForm(ActiveActor);
        ProgressFormCaption( ' ', ActiveActor);
//        ShowPctProgress(0, ActiveActor);

end;
//------------------------------------------------------------------------------
END.
