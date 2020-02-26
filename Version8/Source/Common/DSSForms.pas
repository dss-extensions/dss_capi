unit DSSForms;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

{$IFNDEF CMD}
uses
    Panel,
    Classes;

var

    ControlPanelCreated: Boolean;  // signify whether this is the DLL or EXE
    ControlPanel: TControlPanel;

    RebuildHelpForm: Boolean;


procedure CreateControlPanel;
procedure ExitControlPanel;
procedure InitProgressForm(Actor_ID: Integer);
procedure ProgressCaption(const S: String; Actor_ID: Integer);
procedure ProgressFormCaption(const S: String; Actor_ID: Integer);
procedure ProgressHide(Actor_ID: Integer);
procedure ShowControlPanel;
procedure ShowHelpForm;
procedure ShowAboutBox;
procedure ShowPropEditForm;
procedure ShowPctProgress(Count: Integer; Actor_ID: Integer);
procedure ShowMessageForm(S: TStrings);
function DSSMessageDlg(const Msg: String; err: Boolean): Integer;
procedure DSSInfoMessageDlg(const Msg: String);
function GetDSSExeFile: String;
procedure CloseDownForms;
procedure ShowTreeView(const Fname: String);
function MakeChannelSelection(NumFieldsToSkip: Integer; const Filename: String): Boolean;
function GetDSSProgress(SourcePath: String): Boolean;

{$ENDIF}
implementation

{$IFNDEF CMD}
uses
    ExecCommands,
    ExecOptions,
    Windows,
    Forms,
    Controls,
    Dialogs,
    DSSGlobals,
    Executive,
    DSSClass,
    ParserDel,
    ProgressForm,
    Helpform,
    PropEdit,
    About,
    Diakoptics,
//          MessageForm,
    ComCtrls,
    TViewer,
    Sysutils,
    FrmCSVchannelSelect,
    System.UITypes,
    ScriptEdit,
    StrUtils;

procedure InitProgressForm(Actor_ID: Integer);

begin
    // Start up progressform if not already started.
    if (not NoFormsAllowed) and (ActorProgress[Actor_ID] = NIL) then
        ActorProgress[Actor_ID] := TProgress.Create(NIL);
end;

procedure ShowPctProgress(Count: Integer; Actor_ID: Integer);
// To be reviewed by Davis
begin
    if NoFormsAllowed then
        Exit;      // added RCD 12-5-2010
//     ActorProgress[Actor_ID].PctProgress := Count;
//     Application.ProcessMessages;
end;

procedure ProgressCaption(const S: String; Actor_ID: Integer);

begin
    if NoFormsAllowed then
        Exit;
    ActorProgress[Actor_ID].Caption := S;
    ActorProgress[Actor_ID].Show;
end;

procedure ProgressFormCaption(const S: String; Actor_ID: Integer);

begin
    if NoFormsAllowed then
        Exit;
    ActorProgress[Actor_ID].FormCaption.Caption := S;
    ActorProgress[Actor_ID].Show;
end;

procedure ProgressHide(Actor_ID: Integer);
begin
    if not NoFormsAllowed and (ActorProgress[Actor_ID] <> NIL) then
    begin
        ActorProgress[Actor_ID].Free;
        ActorProgress[Actor_ID] := NIL;
    end;
end;

procedure ShowAboutBox;

begin
    if NoFormsAllowed then
        Exit;
    with TAboutBox.Create(NIL) do
        try
            ShowModal;
            GlobalResult := VersionString;
        finally
            Free;
        end;

end;


procedure ShowTreeView(const Fname: String);
begin
    if NoFormsAllowed then
        Exit;

    if not Assigned(TViewForm) then
        TViewForm := TTViewForm.Create(NIL);

    TViewForm.Left := 0;
    TViewForm.Top := 0;
    TViewForm.TreeView1.Items.Clear;
    TViewForm.ShowFile(Fname);
    TViewForm.Show;
    TViewForm.SetFocus;
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

function GetDSSProgress(SourcePath: String): Boolean;
var
    RefPos: Integer;
    newPath: String;
begin

    newPath := AnsiReverseString(SourcePath);   // reverses the string
    RefPos := pos('\', AnsiLowerCase(newPath)); // detects the file name
    newPath := AnsiReverseString(newPath);      // reverse again
    newPath := newPath.Substring(0, length(newPath) - RefPos);  // Leaves only the folder name

    if fileexists(newPath + '\DSSProgress.exe') then
    begin
        Result := TRUE;            // The progress app is installed
        DSSProgressPath := newPath + '\DSSProgress.exe';
    end
    else
    begin
        Result := FALSE;           // No way, is not installed
        DSSProgressPath := '';
    end;
end;


function DSSMessageDlg(const Msg: String; err: Boolean): Integer;

var
    ScriptEd: TScriptEdit;
    Str: String;

    function IntResult(R: Integer): Integer;
    begin
        if R = mrAbort then
            IntResult := -1
        else
            IntResult := 0;
    end;

begin
    if Length(msg) > 1024 then
        Str := 'Message too long; See Result Form.'
    else
        Str := msg;
    if isDLL then
    begin
        if Err then
            Result := MessageDlg(Str, mtError, [mbOK], 0)
        else
            Result := IntResult(MessageDlg(Str, mtInformation, [mbAbort, mbIgnore], 0))
    end;
//     else
//      ScriptEd.PublishMessage(Str);
    Result := -1;
    SolutionAbort := TRUE;
end;

procedure DSSInfoMessageDlg(const Msg: String);
var
    ScriptEd: TScriptEdit;
    Str: String;
begin

    if length(msg) <= 1024 then
        Str := Msg
    else
        Str := 'Message too long; See Result Form.';
    if isDLL then
    begin
        if length(msg) <= 1024 then
            MessageDlg(Msg, mtInformation, [mbOK], 0)
        else
            MessageDlg('Message too long; See Result Form.', mtInformation, [mbOK], 0);
    end;
//     else
//     ScriptEd.PublishMessage(Str);
    SolutionAbort := TRUE;
end;


procedure CreateControlPanel;

begin
    if NoFormsAllowed or isDLL then
        Exit;
    ControlPanel := TControlPanel.Create(NIL);
    ControlPanelCreated := TRUE;
    ControlPanel.InitializeForm;
end;

procedure ExitControlPanel;

begin
    if NoFormsAllowed or IsDLL then
        Exit;
    ControlPanel.Exit1Click(NIL);
end;

procedure ShowControlPanel;

begin
    if NoFormsAllowed or IsDLL then
        Exit;
    if not ControlPanelCreated then
        CreateControlPanel;
    ControlPanel.Show;
end;

procedure ShowHelpForm;

var
    Param, ParamName: String;


begin
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;

    if NoFormsAllowed then
        Exit;

     // build tree view WITH nodelist containing data pointing to help strings
     // so that when you click on a node, the help string will come up.

    if HelpFormObj <> NIL then   // It's already created.  Let's not do another
    begin
        if RebuildHelpForm then
            HelpFormObj.BuildTreeViewList;
        RebuildHelpForm := FALSE;
        HelpFormObj.Show;
        Exit;
    end;

    if Length(param) = 0 then
    begin
         // Executive help
        HelpFormObj := THelpForm1.Create(NIL);
        HelpFormObj.BuildTreeViewList;
        HelpFormObj.Show;
    end;
end;

procedure ShowMessageForm(S: TStrings);

begin
    if NoFormsAllowed then
        Exit;
//          If Not Assigned (MessageForm1) Then MessageForm1 := TMessageForm1.Create(Nil);
//          MessageForm1.Editor.Clear;
//          MessageForm1.Editor.Lines := S;
//          MessageForm1.WindowState := wsNormal;
//          MessageForm1.Show;
    ControlPanel.ResultsEdit.Clear;
    ControlPanel.ResultsEdit.Lines := s;
end;

procedure ShowPropEditForm;

begin
    if NoFormsAllowed then
        Exit;
       // Create Edit form on the fly
    PropEditForm := TPropEditForm.Create(NIL);
    PropEditForm.ShowModal;
    PropEditForm.Free;
    PropEditForm := NIL;
end;

procedure CloseDownForms;
var
    I: Integer;
begin
    for I := 1 to CPU_Cores do
    begin
        if ActorProgress[I] <> NIL then
        begin
            ActorProgress[I].Free;
            ActorProgress[I] := NIL;
        end;
    end;

    if HelpFormObj <> NIL then
    begin
        HelpFormObj.Free;
        HelpFormObj := NIL;
    end;
    if ControlPanelCreated then
    begin
        ControlPanel.Free;
        ControlPanelCreated := FALSE;
    end;
end;

//----------------------------------------------------------------------------
function MakeChannelSelection(NumFieldsToSkip: Integer; const Filename: String): Boolean;
var
    F: TextFile;
    S: String;
    iCounter: Integer;
    i: Integer;
    SaveWhiteSpaceChars: String;

begin
    AssignFile(F, FileName);
    Reset(F);
    Readln(F, S);  // Read first line in file
    CloseFile(F);

    SaveWhiteSpaceChars := AuxParser[ActiveActor].Whitespace;
    AuxParser[ActiveActor].Whitespace := #9;
    AuxParser[ActiveActor].CmdString := S;  // Load up Parser
   // Skip specified number of columns in CSV file
    for i := 1 to NumFieldsToSkip do
        Auxparser[ActiveActor].NextParam;
    with ChannelSelectForm.ListBox1 do
    begin
        Clear;
        iCounter := 0;
        repeat
            Auxparser[ActiveActor].NextParam;
            S := Auxparser[ActiveActor].StrValue;
            if Length(S) > 0 then
            begin
                iCounter := iCounter + 1;
                AddItem(Format('%d. %s', [iCounter, S]), NIL);
            end;
        until Length(S) = 0;
    end;
    if ChannelSelectForm.ShowModal = mrOK then
        Result := TRUE
    else
        Result := FALSE;
    AuxParser[ActiveActor].Whitespace := SaveWhiteSpaceChars;
end;


initialization

    HelpFormObj := NIL;
    Progress := NIL;   // Created in Solution and ImplDSSProgress
    ControlPanelCreated := FALSE;
    PropEditForm := NIL;
    RebuildHelpForm := TRUE;
    IsMultiThread := TRUE;

finalization

    if PropEditForm <> NIL then
        PropEditForm.Free;
    if HelpFormObj <> NIL then
        HelpFormObj.Free;
    if IsDLL then
    begin
        if Assigned(Progress) then
            Progress.Free;
        if (ControlPanelCreated) then
            ControlPanel.Free;
        if Assigned(TViewForm) then
            TViewForm.Free;
//    If Assigned(MessageForm1) Then MessageForm1.Free;
    end;
{$ENDIF}
end.
