unit DDSSProgress;

interface

function DSSProgressI(mode: longint; arg: longint): longint; cdecl;
function DSSProgressS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;

implementation

uses {$IFDEF FPC}CmdForms{$ELSE}DSSForms{$ENDIF}, DSSGlobals;

{$IFDEF FPC}
function DSSProgressI(mode: longint; arg: longint): longint; cdecl;
begin
  Result:=0; // Default return value
  case mode of
  0: begin // DSSProgress.PctProgress
      If NoFormsAllowed Then Exit;
      InitProgressForm;
  end;
  1: begin // DSSProgress.Show()
     If NoFormsAllowed Then Exit;
        InitProgressForm;
        ProgressFormCaption( ' ');
  end;
  2: begin  // DSSProgress.Close()
      If NoFormsAllowed Then Exit;
      ProgressHide;
  end
  else
      Result:=-1;
  end;
end;

function DSSProgressS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;
begin
  Result:=pAnsiChar(AnsiString('0')); // Default return value
  case mode of
  0: begin // DSSProgress.Caption
     If NoFormsAllowed Then Exit;
     InitProgressForm;
     ProgressCaption (widestring(arg));
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not recognized'));
  end;
end;

{$ELSE}

function DSSProgressI(mode: longint; arg: longint): longint; cdecl;
begin
  Result:=0; // Default return value
  case mode of
  0: begin // DSSProgress.PctProgress
      If NoFormsAllowed Then Exit;
      InitProgressForm(ActiveActor);
//      ShowPctProgress (arg,ActiveActor);
  end;
  1: begin // DSSProgress.Show()
     If NoFormsAllowed Then Exit;
        InitProgressForm(ActiveActor);
        ProgressFormCaption( ' ',ActiveActor);
//        ShowPctProgress(0,ActiveActor);
  end;
  2: begin  // DSSProgress.Close()
      If NoFormsAllowed Then Exit;
      ProgressHide(ActiveActor);
  end
  else
      Result:=-1;
  end;
end;

//******************************String type properties*****************************
function DSSProgressS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;
begin
  Result:=pAnsiChar(AnsiString('0')); // Default return value
  case mode of
  0: begin // DSSProgress.Caption
     If NoFormsAllowed Then Exit;
     InitProgressForm(ActiveActor);
     ProgressCaption (widestring(arg),ActiveActor);
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not recognized'));
  end;
end;
{$ENDIF}
end.
