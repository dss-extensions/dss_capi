unit DSSForms;

// Stub for DSS forms unit that requires no calls to windows functions except for registry

interface

uses
    Classes;

var

    ControlPanelCreated: Boolean;  // signify whether this is the DLL or EXE
  // ControlPanel: TControlPanel;

    RebuildHelpForm: Boolean;


procedure CreateControlPanel;
procedure ExitControlPanel;
procedure InitProgressForm;
procedure ProgressCaption(const S: String);
procedure ProgressFormCaption(const S: String);
procedure ProgressHide;
procedure ShowControlPanel;
procedure ShowHelpForm;
procedure ShowAboutBox;
procedure ShowPropEditForm;
procedure ShowTreeView(const FileNm: String);
procedure ShowMessageForm(S: TStrings);
procedure ShowPctProgress(Count: Integer);
function DSSMessageDlg(const Msg: String; err: Boolean): Integer;
procedure DSSInfoMessageDlg(const Msg: String);
function DSSInputQuery(const S1, S2: String; var Value: String): Boolean;
function GetDSSExeFile: String;
procedure CloseDownForms;
procedure ShowRegistrationForm;


implementation

uses
    Windows, {Forms, Controls, Dialogs,}
    DSSGlobals,
    Executive,
    DSSClass,
    ParserDel,
          {ProgressForm,
          Helpform,
          PropEdit,
          About,
          ComCtrls,   }
    Sysutils{, Registry, Validation};

procedure InitProgressForm;

begin

    // Do nothing

end;

procedure ShowPctProgress(Count: Integer);

begin
    // Do nothing

end;

procedure ShowTreeView(const FileNm: String);

begin
      // Do nothing
end;


procedure ProgressCaption(const S: String);

begin

         // Do nothing

end;

procedure ProgressFormCaption(const S: String);

begin

         // Do nothing

end;

procedure ProgressHide;
begin
         // Do nothing

end;

procedure ShowAboutBox;

begin

end;

function GetDSSExeFile: String;

var
    TheFileName: array[0..MAX_PATH] of Char;

begin

    FillChar(TheFileName, SizeOF(TheFileName), #0);  // Fill it with nulls
    GetModuleFileName(HInstance, TheFileName, SizeOF(TheFileName));
    Result := TheFileName;

    if IsLibrary then
        IsDLL := TRUE;


end;


function DSSMessageDlg(const Msg: String; err: Boolean): Integer;

begin
    WriteLn(Msg);
     // Do Nothing
    Result := 0;
end;

procedure DSSInfoMessageDlg(const Msg: String);
begin
    WriteLn(Msg);
   // Do Nothing
end;

function DSSInputQuery(const S1, S2: String; var Value: String): Boolean;

begin
      // Do Nothing
    Result := TRUE;
end;


procedure CreateControlPanel;

begin
    // Do Nothing
end;

procedure ExitControlPanel;

begin
    // Do Nothing
end;

procedure ShowControlPanel;

begin
    DoSimpleMsg('Illegal command: Show Control Panel.', 9904);
end;

procedure ShowHelpForm;
var
    ParamName, Param: String;

begin
    ParamName := Parser.NextParam;
    Param := Parser.StrValue;
    DoSimpleMsg('Illegal command: Show Help Form.', 9903);

end;

procedure ShowMessageForm(S: TStrings);

begin
       // Do nothing
end;

procedure ShowPropEditForm;

begin
    DoSimpleMsg('Illegal command: Show Property Edit Form.', 9902);
end;


procedure CloseDownForms;

begin
    DoSimpleMsg('No Forms to Close.', 9901);
end;

procedure ShowRegistrationForm;

begin
    DoSimpleMsg('Registration Form Not Valid for this Version', 9900);
end;

{
Procedure MessageDlg(const Message  : string; DialogType  : TMsgDlgType; Buttons : TMsgDlgButtons; HelpContext : Longint ) : Integer;;

Begin
     DoSimpleMsg( Message );
End;

Const mtError := 0;
Const mbOK := 0;
}

initialization


    ControlPanelCreated := FALSE;
  //WriteDLLDebugFile('DSSForms');
    RebuildHelpForm := TRUE;

finalization


end.
