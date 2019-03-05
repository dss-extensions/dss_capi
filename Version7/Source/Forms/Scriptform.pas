unit Scriptform;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Windows,
    Messages,
    SysUtils,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    ComCtrls,
    contnrs,
    Menus,
    ToolWin;

type
    TMainEditForm = class(TForm)
        Editor: TRichEdit;
        PopupMenu2: TPopupMenu;
        Do2: TMenuItem;
        Save3: TMenuItem;
        CloseWindow1: TMenuItem;
        OpenSelectedFile1: TMenuItem;
        EditSelectedFile1: TMenuItem;
        FontDialog1: TFontDialog;
        ToolBar1: TToolBar;
        FontBtn: TButton;
        ChangetothisDir1: TMenuItem;
        procedure EditorKeyDown(Sender: TObject; var Key: Word;
            Shift: TShiftState);
        procedure EditorSelectionChange(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure EditorChange(Sender: TObject);
        procedure Do2Click(Sender: TObject);
        procedure Save3Click(Sender: TObject);
        procedure CloseWindow1Click(Sender: TObject);
        procedure OpenSelectedFile1Click(Sender: TObject);
        procedure EditSelectedFile1Click(Sender: TObject);
        procedure FontDialog1Apply(Sender: TObject; Wnd: HWND);
        procedure FontBtnClick(Sender: TObject);
        procedure ChangetothisDir1Click(Sender: TObject);

    PRIVATE
    { Private declarations }
        procedure SetFormColor;
        function Get_HasBeenModified: Boolean;
        procedure Set_HasBeenModified(const Value: Boolean);
        function TrimParens(S: String): String;
        procedure ExtendSelection;
    PUBLIC
    { Public declarations }
        cmdList: TStringList;
        line1, line2, col: Integer;
        HasFileName, isMainWindow: Boolean;
        procedure UpdateCursorPos;
        function BuildCommandList: Boolean;
        procedure ExecuteCommandList;
        procedure ExecuteDSSCommand(const S: String);
        procedure UpdateResultform;
        procedure SaveEditorContents;
        procedure UpdateSummaryForm;

        property HasBeenModified: Boolean READ Get_HasBeenModified WRITE Set_HasBeenModified;
    end;

var
    MainEditForm: TMainEditForm;
    ActiveScriptForm: TMainEditForm;
    ScriptWindowList: TObjectList;
    RecordCommands: Boolean;

implementation

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

{$R *.DFM}

const
    ModifiedColor = 13434879;

procedure TMainEditForm.EditorKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
{
  if (Key = VK_RETURN) then begin
    if not (ssCtrl in Shift) then begin
      if Editor.SelLength > 0 then begin
        if BuildCommandList then begin  // execute and then replace selection
          ExecuteCommandList;
          Editor.Undo
          end
        end
      else begin  // execute the current line
        if BuildCommandList then begin
          ExecuteCommandList;
        end
      end
    end
  end;
}
end;


procedure TMainEditForm.UpdateCursorPos;
begin
    line1 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0,
        Editor.SelStart);
    line2 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0,
        Editor.SelStart + Editor.SelLength);
    col := (Editor.SelStart -
        SendMessage(Editor.Handle, EM_LINEINDEX, line1, 0));
end;

procedure TMainEditForm.EditorSelectionChange(Sender: TObject);
begin
    UpdateCursorPos;
end;

function TMainEditForm.BuildCommandList: Boolean;
var
    i: Integer;
    str: String;
    InBlockComment: Boolean;
begin
    result := FALSE;
    InBlockComment := FALSE;
    cmdList.Clear;

  {  6-28-2012
     Block Comment  Handling added
     /*       (beginning of line
     ...
        */    (anywhere)
  }

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

procedure TMainEditForm.ExecuteCommandList;
var
    i, imax: Integer;
begin
    SolutionAbort := FALSE;
    imax := cmdList.Count - 1;
    if imax < 0 then
        Exit;

    SolutionWasAttempted := FALSE;      // Global variable

    Screen.Cursor := crHourglass;
    for i := 0 to imax do
    begin
        if not SolutionAbort then
        begin  // If script involves step that gets aborted, just flush the script
            DSSExecutive.Command := ActiveScriptForm.cmdList.Strings[i];
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
        UpdateResultForm;
        UpdateSummaryForm;
        if Assigned(ActiveCircuit) then
            with ActiveCircuit do
                if (SolutionWasAttempted) and (not IsSolved) then
                begin
                    Beep;
                    ControlPanel.ResultPages.ActivePage := ControlPanel.SummaryTab;
                end;
        ControlPanel.UpdateStatus;
    end;
end;

procedure TMainEditForm.ExecuteDSSCommand(const S: String);
begin
    SolutionAbort := FALSE;
    DSSExecutive.Command := S;
    if RecordCommands then
        Editor.Lines.Append(S);

    if not IsDLL then
    begin
        UpdateResultForm;
        UpdateSummaryForm;
    end;
end;


procedure TMainEditForm.FormActivate(Sender: TObject);
begin
    ActiveScriptForm := Self;
    SetFormColor;
end;

procedure TMainEditForm.FormCreate(Sender: TObject);
begin
    cmdList := TStringList.Create;
    Editor.Clear;
    UpdateCursorPos;
    HasFileName := FALSE;
    IsMainWindow := FALSE;
    Editor.Font.Size := DefaultFontSize;
    Editor.Font.Name := DefaultFontName;
    Editor.Font.Style := DefaultFontStyles;
end;

procedure TMainEditForm.FormDestroy(Sender: TObject);
begin
    cmdList.Free;
end;

procedure TMainEditForm.UpdateResultform;
begin
    ControlPanel.ResultsEdit.Clear;
    ControlPanel.ResultsEdit.Lines.Add(GlobalResult);
    if Length(GlobalResult) > 0 then
        ControlPanel.ResultPages.ActivePage := ControlPanel.ResultsTab;
    if not IsDLL then
        ControlPanel.Edit_Result.Text := GlobalResult;
end;

procedure TMainEditForm.UpdateSummaryForm;
var
    cLosses, cPower: Complex;
begin

    with ControlPanel.SummaryEdit do
    begin
        Clear;
        Lines.BeginUpdate;

        if ActiveCircuit <> NIL then
            with Lines do
            begin
                if ActiveCircuit.Issolved then
                    Add('Status = SOLVED')
                else
                begin
                    SelAttributes.Color := clRed;
                    Add('Status = NOT Solved');
                    SelAttributes.Color := clBlack;
                end;
                Add('Solution Mode = ' + GetSolutionModeID);
                Add('Number = ' + IntToStr(ActiveCircuit.Solution.NumberofTimes));
                Add('Load Mult = ' + Format('%5.3f', [ActiveCircuit.LoadMultiplier]));
                Add('Devices = ' + Format('%d', [ActiveCircuit.NumDevices]));
                Add('Buses = ' + Format('%d', [ActiveCircuit.NumBuses]));
                Add('Nodes = ' + Format('%d', [ActiveCircuit.NumNodes]));
                Add('Control Mode =' + GetControlModeID);
                Add('Total Iterations = ' + IntToStr(ActiveCircuit.Solution.Iteration));
                Add('Control Iterations = ' + IntToStr(ActiveCircuit.Solution.ControlIteration));
                Add('Max Sol Iter = ' + IntToStr(ActiveCircuit.Solution.MostIterationsDone));
                Add(' ');
                Add(' - Circuit Summary -');
                Add(' ');
                if ActiveCircuit <> NIL then
                    if ActiveCircuit.Issolved and not ActiveCircuit.BusNameRedefined then
                    begin
                        try
                            Add(Format('Year = %d ', [ActiveCircuit.Solution.Year]));
                            Add(Format('Hour = %d ', [ActiveCircuit.Solution.DynaVars.intHour]));
                            Add('Max pu. voltage = ' + Format('%-.5g ', [GetMaxPUVoltage]));
                            Add('Min pu. voltage = ' + Format('%-.5g ', [GetMinPUVoltage(TRUE)]));
                            cPower := CmulReal(GetTotalPowerFromSources, 0.000001);  // MVA
                            Add(Format('Total Active Power:   %-.6g MW', [cpower.re]));
                            Add(Format('Total Reactive Power: %-.6g Mvar', [cpower.im]));
                            cLosses := CmulReal(ActiveCircuit.Losses, 0.000001);
                            if cPower.re <> 0.0 then
                                Add(Format('Total Active Losses:   %-.6g MW, (%-.4g %%)', [cLosses.re, (Closses.re / cPower.re * 100.0)]))
                            else
                                Add('Total Active Losses:   ****** MW, (**** %%)');
                            Add(Format('Total Reactive Losses: %-.6g Mvar', [cLosses.im]));
                            Add(Format('Frequency = %-g Hz', [ActiveCircuit.Solution.Frequency]));
                            Add('Mode = ' + GetSolutionModeID);
                            Add('Control Mode = ' + GetControlModeID);
                            Add('Load Model = ' + GetLoadModel);
                        except
                            On E: Exception do
                                Add('Error encountered. Re-solve circuit.');
                        end;
                    end;

                if not IsDLL then
                    ControlPanel.Caption := 'DSS Main Control Panel: Active Circuit = ' + ActiveCircuit.Name;
            end
        else
            with Lines do
            begin
                Add('No Circuits Defined');
            end;

      {Status ...}

        if not IsDLL then
            ControlPanel.UpdateStatus;

        Lines.EndUpdate;
    end;

end;

procedure TMainEditForm.FormClose(Sender: TObject;
    var Action: TCloseAction);
begin
    if Self <> MainEditForm then
    begin
        if HasBeenModified then
            case MessageDlg('File ' + Caption + ' has changed.  Save ?', mtConfirmation, [mbYes, mbNo], 0) of
                mrYes:
                begin
                    SaveEditorContents;
                end;
            else    {no}
            end;
(*
       Case MessageDlg('Do you want to close file '+Caption+'  ?', mtConfirmation, [mbYes, mbNo], 0) of
            mrYes: Begin
*)
        ScriptWindowList.Remove(Self);
        Action := caFree;
(*
                   End
       Else
          Action := caNone; {do Nothing}
       End;
*)

    end;


end;

procedure TMainEditForm.SetFormColor;
begin
    if Editor.Modified then
        Editor.Color := ModifiedColor
    else
        Editor.Color := clWindow;
end;

procedure TMainEditForm.EditorChange(Sender: TObject);
begin
    if Editor.Color <> ModifiedColor then
        SetFormColor;
end;

function TMainEditForm.Get_HasBeenModified: Boolean;
begin
    Result := Editor.Modified;
end;

procedure TMainEditForm.Set_HasBeenModified(const Value: Boolean);
begin
    Editor.Modified := Value;
    SetFormColor;
end;

procedure TMainEditForm.Do2Click(Sender: TObject);

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
{      loc.x := x;
      loc.y := y;
      ret := LoWord (SendMessage (Editor.Handle, EM_CHARFROMPOS, 0, Integer (@loc)));
      line1 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0, ret);
}
        Line1 := Editor.CaretPos.y;
        line2 := line1;
     { col := (ret - SendMessage(Editor.Handle, EM_LINEINDEX, line1, 0));}
        if BuildCommandList then
        begin
            ExecuteCommandList;
        end;
      {Editor.SelStart := ret}
    end;

end;

procedure TMainEditForm.SaveEditorContents;
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

procedure TMainEditForm.Save3Click(Sender: TObject);
begin
    if not HasFileName then
        ControlPanel.SaveScriptWindow1Click(Sender)
    else
        SaveEditorContents;
end;

procedure TMainEditForm.ChangetothisDir1Click(Sender: TObject);
var
    CurrDir: String;
begin
    CurrDir := ExtractFileDir(Caption);
    SetCurrentDir(CurrDir);
    SetDataPath(CurrDir);  // change datadirectory
    if not IsDLL then
        ControlPanel.UpdateStatus;
end;

procedure TMainEditForm.CloseWindow1Click(Sender: TObject);
begin
    Close;
end;

procedure TMainEditForm.OpenSelectedFile1Click(Sender: TObject);
var
    TempActiveForm: TMainEditForm;
    FileName: String;
begin
    ExtendSelection;
    if Editor.SelLength > 0 then
    begin

        try
            FileName := TrimParens(Trim(Editor.SelText));
            if FileExists(FileName) then
            begin
                TempActiveForm := TMainEditForm.Create(NIL);
                TempActiveForm.Editor.Lines.LoadFromFile(FileName);
                TempActiveForm.Caption := ExpandFileName(FileName);
                ScriptWindowList.Add(TempActiveForm);
                ActiveScriptForm := TempActiveForm;
                ActiveScriptForm.HasBeenModified := FALSE;
                ActiveScriptForm.HasFileName := TRUE;
            end
            else
                DoSimpleMsg('File "' + Editor.SelText + '" not found in currentdirectory: "' + GetCurrentDir + '"', 311);
        except
            On E: Exception do
                DoSimpleMsg('Error opening new window: ' + E.Message, 312);
        end;
    end;
end;

procedure TMainEditForm.EditSelectedFile1Click(Sender: TObject);
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

function TMainEditForm.TrimParens(S: String): String;
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

procedure TMainEditForm.ExtendSelection;
var
    i, LineIdx, Slen: Integer;
    Pos: TPoint;
begin
    if Editor.SelLength = 0 then
    begin
        Pos := Editor.CaretPos;
        LineIdx := Pos.y; // SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0, Editor.SelStart);
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


procedure TMainEditForm.FontDialog1Apply(Sender: TObject; Wnd: HWND);
begin

    Editor.SelAttributes.Assign(TFontDialog(Sender).Font)

end;

procedure TMainEditForm.FontBtnClick(Sender: TObject);
var
    FontSave: TFont;
begin
        // First select all
    Editor.SelStart := 0;
    Editor.SelLength := Editor.GetTextLen;
    with FontDialog1 do
    begin
        FontSave := Editor.Font;
        Font := Editor.Font;
        Options := Options + [fdApplyButton];
        if Execute then
        begin
            Editor.SelAttributes.Assign(Font);
            Editor.Font := Font;
            DefaultFontSize := Editor.Font.Size;
            DefaultFontName := Editor.Font.Name;
            DefaultFontStyles := Editor.Font.Style;
        end
        else
            Editor.Font := FontSave;
    end;
end;

end.
