unit DDSSProgress;

interface

function DSSProgressI(mode: Longint; arg: Longint): Longint; CDECL;
function DSSProgressS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

implementation

uses
    DSSForms, {Progressform,} DSSGlobals;

function DSSProgressI(mode: Longint; arg: Longint): Longint; CDECL;
begin
    Result := 0; // Default return value
    case mode of
        0:
        begin // DSSProgress.PctProgress
            if NoFormsAllowed then
                Exit;
            InitProgressForm(ActiveActor);
//      ShowPctProgress (arg,ActiveActor);
        end;
        1:
        begin // DSSProgress.Show()
            if NoFormsAllowed then
                Exit;
            InitProgressForm(ActiveActor);
            ProgressFormCaption(' ', ActiveActor);
//        ShowPctProgress(0,ActiveActor);
        end;
        2:
        begin  // DSSProgress.Close()
            if NoFormsAllowed then
                Exit;
            ProgressHide(ActiveActor);
        end
    else
        Result := -1;
    end;
end;

//******************************String type properties*****************************
function DSSProgressS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
begin
    Result := pAnsiChar(Ansistring('0')); // Default return value
    case mode of
        0:
        begin // DSSProgress.Caption
            if NoFormsAllowed then
                Exit;
            InitProgressForm(ActiveActor);
            ProgressCaption(Widestring(arg), ActiveActor);
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not recognized'));
    end;
end;

end.
