
unit ScriptEdit;

{
  ----------------------------------------------------------
  Copyright (c) 2016, University of Pittsburgh
  All rights reserved.
  ----------------------------------------------------------
}

interface
//{$IFDEF MSWINDOWS}
uses
    Windows,
    Messages,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    ComCtrls,
    SysUtils,
    Classes,
    contnrs;

type
    TScriptEdit = class(TObject)
        Editor: TRichEdit;
        Tab: TTabSheet;
        Caption: String;
        cmdList: TStringList;
        line1, line2, col: Integer;
        HasFileName, isMainWindow: Boolean;
    // wire to editor events
        procedure EditorSelectionChange(Sender: TObject);
        function CheckEditorClose: Boolean;
        procedure EditorChange(Sender: TObject);
        procedure DoSelection;
        procedure SaveSelection;
        function GetSelectedFileName: String;
        procedure EditSelectedFile;
        procedure ChangeToThisDir;

    PRIVATE
        procedure SetFormColor;
        function Get_HasBeenModified: Boolean;
        procedure Set_HasBeenModified(const Value: Boolean);
        function TrimParens(S: String): String;
        procedure ExtendSelection;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;
        procedure UpdateCursorPos;
        function BuildCommandList: Boolean;
        procedure ExecuteCommandList;
        procedure ExecuteDSSCommand(const S: String);
        procedure UpdateResultform;
        procedure SaveEditorContents;
        procedure UpdateSummaryForm(const s: String);
        property HasBeenModified: Boolean READ Get_HasBeenModified WRITE Set_HasBeenModified;
        procedure UpdateProgressSummary;
        procedure PublishMessage(Msg: String);
    end;

var
    MainEditForm: TScriptEdit;
    ActiveScriptForm: TScriptEdit;
    ScriptWindowList: TObjectList;
    RecordCommands: Boolean;
//{$ENDIF}
implementation
//{$IFDEF MSWINDOWS}
uses
    RichEdit,
    Executive,
    DSSGlobals,
    DSSForms,
    Panel,
    Utilities,
    uComplex,
    System.Types,
    System.UITypes;

const
    ModifiedColor = 13434879;

constructor TScriptEdit.Create;
begin
    inherited Create;
    cmdList := TStringList.Create;
//  Editor.Clear;
//  UpdateCursorPos;
    HasFileName := FALSE;
    IsMainWindow := FALSE;
end;

destructor TScriptEdit.Destroy;
begin
    cmdList.Free;
    inherited Destroy;
end;

procedure TScriptEdit.UpdateCursorPos;
begin
    line1 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0, Editor.SelStart);
    line2 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0, Editor.SelStart + Editor.SelLength);
    col := (Editor.SelStart - SendMessage(Editor.Handle, EM_LINEINDEX, line1, 0));
end;

procedure TScriptEdit.EditorSelectionChange(Sender: TObject);
begin
    UpdateCursorPos;
end;

function TScriptEdit.BuildCommandList: Boolean;
var
    i: Integer;
    str: String;
    InBlockComment: Boolean;
begin
    result := FALSE;
    InBlockComment := FALSE;
    cmdList.Clear;
    for i := line1 to line2 do
    begin
        str := Trim(Editor.Lines.Strings[i]);
        if Length(str) > 0 then
        begin
            if not InBlockComment then     // look for '/*'  at baginning of line
                case str[1] of
                    '/':
                        if (Length(str) > 1) and (str[2] = '*') then
                            InBlockComment := TRUE;
                end;
            if not InBlockComment then
                cmdList.Add(str);
        // in block comment ... look for */   and cancel block comment (whole line)
            if InBlockComment then
                if Pos('*/', str) > 0 then
                    InBlockComment := FALSE;
        end;
      {
        NOTE:  InBlockComment resets to FALSE upon leaving this routine
        So if you fail to select a line containing the end of the block comment,
        the next selection will not be blocked.
      }
    end;
    if cmdList.Count > 0 then
        result := TRUE;
end;

procedure TScriptEdit.ExecuteCommandList;
var
    i, imax: Integer;
begin
    SolutionAbort := FALSE;
    imax := cmdList.Count - 1;
    if imax < 0 then
        Exit;

    SolutionWasAttempted[ActiveActor] := FALSE;      // Global variable

    Screen.Cursor := crHourglass;
    for i := 0 to imax do
    begin
        if not SolutionAbort then
        begin  // If script involves step that gets aborted, just flush the script
            DSSExecutive[ActiveActor].Command := ActiveScriptForm.cmdList.Strings[i];
            if LastCommandWasCompile and not IsDLL then
            begin
                ControlPanel.AddCompiledFile(LastFileCompiled);
                ControlPanel.UpdateElementBox;
            end;
        end;
    end;

    Screen.Cursor := crDefault;

    if not IsDLL then
    begin
        if ActorHandle[1] <> NIL then
        begin
            UpdateResultForm;
            if not ProgressCmd then
                UpdateSummaryForm('1');
        end;

        if Assigned(ActiveCircuit[ActiveActor]) then
            with ActiveCircuit[ActiveActor] do
                if (SolutionWasAttempted[ActiveActor]) and (not IsSolved) then
                begin
                    Beep;
                    ControlPanel.ResultPages.ActivePage := ControlPanel.SummaryTab;
                end;
        ControlPanel.UpdateStatus;
    end;
end;

procedure TScriptEdit.ExecuteDSSCommand(const S: String);
begin
    SolutionAbort := FALSE;
    DSSExecutive[ActiveActor].Command := S;
    if RecordCommands then
        Editor.Lines.Append(S);
    if (ActorStatus[1] = 1) then
    begin
        if not IsDLL then
        begin
            UpdateResultForm;
            if not ProgressCmd then
                UpdateSummaryForm('1');
        end;
    end;
end;

procedure TScriptEdit.UpdateResultform;
begin
    ControlPanel.ResultsEdit.Clear;
    ControlPanel.ResultsEdit.Lines.Add(GlobalResult);
    if Length(GlobalResult) > 0 then
        ControlPanel.ResultPages.ActivePage := ControlPanel.ResultsTab;
    if not IsDLL then
        ControlPanel.Edit_Result.Text := GlobalResult;
end;

procedure TScriptEdit.UpdateSummaryForm(const s: String);
var
    cLosses, cPower: Complex;
    ActorBusy,
    ActorsRdy: Boolean;
    CallerID,
    USIdx,
    APointer: Integer;
    TStr: String;
begin
    ProgressCmd := FALSE;
    CallerID := ActiveActor;
    with ControlPanel.SummaryEdit do
    begin

        ActorsRdy := TRUE;
        for USIdx := 1 to NumOfActors do
            ActorsRdy := ActorsRdy and (ActorStatus[USIdx] = 1);
        if ActorsRdy then
        begin
            cLosses := CZERO;
            Clear;
            if ADiakoptics and (ActiveActor = 1) then
                APointer := 1
            else
                APointer := NumOfActors;

            for USIdx := 1 to APointer do
            begin
                ActiveActor := USIdx;
                if ActiveCircuit[ActiveActor] <> NIL then
                begin
                    Lines.BeginUpdate;
                    if ActiveCircuit[ActiveActor] <> NIL then
                        with Lines do
                        begin
                            Add('Results for Actor ID # ' + inttostr(ActiveActor));
                            Add('CPU selected : ' + inttostr(ActorCPU[ActiveActor]));
                            if ActiveCircuit[ActiveActor].Issolved then
                                Add('Status = SOLVED')
                            else
                            begin
                                SelAttributes.Color := clRed;
                                Add('Status = NOT Solved');
                                SelAttributes.Color := clBlack;
                            end;
                            Add('Solution Mode = ' + GetSolutionModeID);
                            Add('Number = ' + IntToStr(ActiveCircuit[ActiveActor].Solution.NumberofTimes));
                            Add('Load Mult = ' + Format('%5.3f', [ActiveCircuit[ActiveActor].LoadMultiplier]));
                            Add('Devices = ' + Format('%d', [ActiveCircuit[ActiveActor].NumDevices]));
                            Add('Buses = ' + Format('%d', [ActiveCircuit[ActiveActor].NumBuses]));
                            Add('Nodes = ' + Format('%d', [ActiveCircuit[ActiveActor].NumNodes]));
                            Add('Control Mode =' + GetControlModeID);
                            Add('Total Iterations = ' + IntToStr(ActiveCircuit[ActiveActor].Solution.Iteration));
                            Add('Control Iterations = ' + IntToStr(ActiveCircuit[ActiveActor].Solution.ControlIteration));
                            Add('Max Sol Iter = ' + IntToStr(ActiveCircuit[ActiveActor].Solution.MostIterationsDone));
                            Add(' ');
                            Add(' - Circuit Summary -');
                            Add(' ');
                            if ActiveCircuit[ActiveActor] <> NIL then
                                if ActiveCircuit[ActiveActor].Issolved and not ActiveCircuit[ActiveActor].BusNameRedefined then
                                begin
                                    try
                                        Add(Format('Year = %d ', [ActiveCircuit[ActiveActor].Solution.Year]));
                                        Add(Format('Hour = %d ', [ActiveCircuit[ActiveActor].Solution.DynaVars.intHour]));
                                        Add('Max pu. voltage = ' + Format('%-.5g ', [GetMaxPUVoltage]));
                                        Add('Min pu. voltage = ' + Format('%-.5g ', [GetMinPUVoltage(TRUE)]));
                                        cPower := CmulReal(GetTotalPowerFromSources(ActiveActor), 0.000001);  // MVA
                                        Add(Format('Total Active Power:   %-.6g MW', [cpower.re]));
                                        Add(Format('Total Reactive Power: %-.6g Mvar', [cpower.im]));
                                        cLosses := CmulReal(ActiveCircuit[ActiveActor].Losses[ActiveActor], 0.000001);
                                        if cPower.re <> 0.0 then
                                            Add(Format('Total Active Losses:   %-.6g MW, (%-.4g %%)', [cLosses.re, (Closses.re / cPower.re * 100.0)]))
                                        else
                                        begin
                                            Add('Total Active Losses:   ****** MW, (**** %%)');
                                        end;
                                        Add(Format('Total Reactive Losses: %-.6g Mvar', [cLosses.im]));
                                        Add(Format('Frequency = %-g Hz', [ActiveCircuit[ActiveActor].Solution.Frequency]));
                                        Add('Mode = ' + GetSolutionModeID);
                                        Add('Control Mode = ' + GetControlModeID);
                                        Add('Load Model = ' + GetLoadModel);
                                        Add('------------------------');
                                    except
                                        On E: Exception do
                                            Add('Error encountered. Re-solve circuit.');
                                    end;
                                end;
                            if not IsDLL then
                                ControlPanel.Caption := 'DSS Main Control Panel: Active Circuit = ' + ActiveCircuit[ActiveActor].Name;
                        end
                    else
                        with Lines do
                        begin
                            Add('No Circuits Have been Defined for this Actor');
                        end;

                    Lines.EndUpdate;
                end;
            end;
        end
        else
        begin
            Clear;
            Lines.BeginUpdate;
            with Lines do
            begin
                Add('Process Status');
                for USIdx := 1 to NumOfActors do
                begin
                    if (ActorStatus[USIdx] = 0) then
                        TStr := 'Processing'
                    else
                        TStr := 'Ready';
                    Add('Actor ' + inttostr(USIdx) + ' CPU ' + inttostr(ActorCPU[USIdx]) + ': ' + TStr);
                    if not IsDLL then
                        ControlPanel.UpdateStatus;
                    Lines.EndUpdate;
                end;
            end;
        end;
        ControlPanel.ResultPages.ActivePage := ControlPanel.SummaryTab;
        if not IsDLL then
            ControlPanel.UpdateStatus;
    end;
    ActiveActor := CallerID;
end;

function TScriptEdit.CheckEditorClose: Boolean;
begin
    result := TRUE;
    if Self <> MainEditForm then
    begin
        if HasBeenModified then
            case MessageDlg('File ' + Caption + ' has changed.  Save?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
                mrYes:
                begin
                    SaveEditorContents;
                    result := TRUE;
                end;
                mrCancel:
                begin
                    result := FALSE;
                end
            else    {no}
                result := TRUE;
            end;
//    ScriptWindowList.Remove(Self);
    end
    else
    begin
        MessageDlg('Main script window cannot be closed', mtInformation, [mbOK], 0);
        result := FALSE;
    end;
end;

procedure TScriptEdit.SetFormColor;
begin
    if Editor.Modified then
        Editor.Color := ModifiedColor
    else
        Editor.Color := clWindow;
end;

procedure TScriptEdit.EditorChange(Sender: TObject);
begin
    if Editor.Color <> ModifiedColor then
        SetFormColor;
end;

function TScriptEdit.Get_HasBeenModified: Boolean;
begin
    Result := Editor.Modified;
end;

procedure TScriptEdit.Set_HasBeenModified(const Value: Boolean);
begin
    Editor.Modified := Value;
    SetFormColor;
end;

procedure TScriptEdit.DoSelection;
begin
    if Editor.SelLength > 0 then
    begin
        if BuildCommandList then
        begin // execute selection
            ExecuteCommandList;
        end
    end
    else
    begin // select and execute current line
        Line1 := Editor.CaretPos.y;
        line2 := line1;
        if BuildCommandList then
        begin
            ExecuteCommandList;
        end;
    end;
end;

procedure TScriptEdit.SaveEditorContents;
var
    Save_Cursor: TCursor;
begin
    Save_Cursor := Screen.Cursor;
    Screen.Cursor := crHourglass;    { Show hourglass cursor }
    try
        try
            Editor.PlainText := TRUE;
            Editor.Lines.SaveToFile(Caption, TEncoding.ANSI);
            Editor.PlainText := FALSE;
            HasBeenModified := FALSE;
            HasFileName := TRUE;
        except
            On E: Exception do
                DoSimpleMsg('Error saving file: ' + E.message, 310);
        end;
    finally
        Screen.Cursor := Save_Cursor;  { Always restore to normal }
    end;
end;

procedure TScriptEdit.SaveSelection;
begin
  // precondition is HasFileName, otherwise ControlPanel should Save As
    if HasFileName then
        SaveEditorContents;
end;

procedure TScriptEdit.ChangeToThisDir;
var
    CurrDir: String;
begin
    CurrDir := ExtractFileDir(Caption);
    SetCurrentDir(CurrDir);
    SetDataPath(CurrDir);  // change datadirectory
    if not IsDLL then
        ControlPanel.UpdateStatus;
end;

function TScriptEdit.GetSelectedFileName: String;
var
    FileName: String;
begin
    Result := '';
    ExtendSelection;
    if Editor.SelLength > 0 then
    begin
        FileName := TrimParens(Trim(Editor.SelText));
        if FileExists(FileName) then
            Result := FileName
        else
            DoSimpleMsg('File "' + Editor.SelText + '" not found in currentdirectory: "' + GetCurrentDir + '"', 311);
    end;
end;

procedure TScriptEdit.EditSelectedFile;
var
    FileName: String;
begin
    ExtendSelection;
    if Editor.SelLength > 0 then
    begin
        try
            FileName := TrimParens(Trim(Editor.SelText));
            if FileExists(FileName) then
                FireOffEditor(FileName)
            else
                DoSimpleMsg('File "' + FileName + '" not found in currentdirectory: "' + GetCurrentDir + '"', 313);
        except
            On E: Exception do
                DoSimpleMsg('Error opening Editor: ' + E.Message, 314);
        end;
    end;
end;

function TScriptEdit.TrimParens(S: String): String;
begin
{Get rid of leading and trailing Parens}
    Result := S;
    case S[1] of
        '(':
        begin
            Result := Copy(S, 2, Length(S) - 1);
            if Result[Length(Result)] = ')' then
                SetLength(Result, Length(Result) - 1);
        end;
        '"':
        begin
            Result := Copy(S, 2, Length(S) - 1);
            if Result[Length(Result)] = '"' then
                SetLength(Result, Length(Result) - 1);
        end;
        '''':
        begin
            Result := Copy(S, 2, Length(S) - 1);
            if Result[Length(Result)] = '''' then
                SetLength(Result, Length(Result) - 1);
        end;
        '[':
        begin
            Result := Copy(S, 2, Length(S) - 1);
            if Result[Length(Result)] = ']' then
                SetLength(Result, Length(Result) - 1);
        end;
        '{':
        begin
            Result := Copy(S, 2, Length(S) - 1);
            if Result[Length(Result)] = '}' then
                SetLength(Result, Length(Result) - 1);
        end;
    end;
end;

procedure TScriptEdit.ExtendSelection;
var
    i, LineIdx, Slen: Integer;
    Pos: TPoint;
begin
    if Editor.SelLength = 0 then
    begin
        Pos := Editor.CaretPos;
        LineIdx := Pos.y;
    // Backup from cursor position until blank found or BOL
        i := Pos.x;
        while (i > 0) and not
            ((Editor.Lines.Strings[LineIdx][i] = ' ') or (Editor.Lines.Strings[LineIdx][i] = '=')) do
        begin
            Dec(i);
            Editor.SelStart := Editor.SelStart - 1;
        end;
    // Now go forward until a blank or EOL
        inc(i);
        slen := Length(Editor.Lines.Strings[LineIdx]);
        while (i <= slen) and (Editor.Lines.Strings[LineIdx][i] <> ' ') do
        begin
            inc(i);
            Editor.SelLength := Editor.SelLength + 1;
        end;
    end;
end;

procedure TScriptEdit.UpdateProgressSummary;
var
    USPIdx: Integer;
begin
    with ControlPanel.SummaryEdit do
    begin
        Clear;
        Lines.BeginUpdate;
        with Lines do
        begin
            Add('Process Progress');
            for USPIdx := 1 to NumOfActors do
            begin
                Add('Actor ' + inttostr(USPIdx) + ' CPU ' + inttostr(ActorCPU[USPIdx]) + ': ' +
                    IntToStr(ActorPctProgress[USPIdx]) + ' %');
                if not IsDLL then
                    ControlPanel.UpdateStatus;
                Lines.EndUpdate;
            end;
        end;
        ProgressCmd := TRUE;
    end;
end;

procedure TScriptEdit.PublishMessage(Msg: String);
begin
    with ControlPanel.SummaryEdit do
    begin
        Clear;
        Lines.BeginUpdate;
        with Lines do
        begin
            Add(Msg);
            Lines.EndUpdate;
        end;
    end;
end;
//{$ENDIF}
end.
