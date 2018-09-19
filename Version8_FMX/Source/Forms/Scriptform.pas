unit Scriptform;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

//** Converted with Mida 560     http://www.midaconverter.com - MONTENEGRO.MARTINEZ



uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IniFiles,
  System.Generics.Collections,
  Data.DB,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Menus,
  FMX.Grid,
  FMX.ExtCtrls,
  FMX.ListBox,
  FMX.TreeView,
  FMX.Memo,
  FMX.TabControl,
  FMX.Layouts,
  FMX.Edit,
  FMX.Platform,
  FMX.Bind.DBEngExt,
  FMX.Bind.Editors,
  FMX.Bind.DBLinks,
  FMX.Bind.Navigator,
  Data.Bind.EngExt,
  Data.Bind.Components,
  Data.Bind.DBScope,
  Data.Bind.DBLinks,
  Datasnap.DBClient,
  _Mida_FM_Lib,
  Fmx.Bind.Grid,
  System.Rtti,
  System.Bindings.Outputs,
  Data.Bind.Grid,
  Fmx.StdCtrls,
  FMX.Header,
  FMX.Graphics, FMX.Controls.Presentation,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, Vcl.Controls,
  System.Contnrs,
  Windows, Messages, Graphics, Forms, Dialogs,
  Menus, ToolWin;

//**   Original VCL Uses section : 


//**   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
//**   StdCtrls, ComCtrls,contnrs, Menus, ToolWin;


type
  TMainEditForm = class(TForm)
StyleBook1: TStyleBook;
    PopupMenu2 :TPopupMenu;
    Do2: TMenuItem;
    Save3: TMenuItem;
    CloseWindow1: TMenuItem;
    OpenSelectedFile1: TMenuItem;
    EditSelectedFile1: TMenuItem;
//DavisMMP
//    FontDialog1: TFontDialog;
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
//DavisMMP
//    procedure FontDialog1Apply(Sender: TObject; Wnd: HWND);
    procedure FontBtnClick(Sender: TObject);
    procedure ChangetothisDir1Click(Sender: TObject);

  private
    { Private declarations }
    Procedure SetFormColor;
    function  Get_HasBeenModified: Boolean;
    procedure Set_HasBeenModified(const Value: Boolean);
    Function  TrimParens( S:String):String;
    Procedure ExtendSelection;
  public
    { Public declarations }
    cmdList: TStringList;
    line1, line2, col: integer;
    HasFileName, isMainWindow:Boolean;
    procedure UpdateCursorPos;
    function  BuildCommandList: Boolean;
    procedure ExecuteCommandList;
    Procedure ExecuteDSSCommand(Const S:String);
    Procedure UpdateResultform;
    Procedure SaveEditorContents;
    Procedure UpdateSummaryForm;

    Property HasBeenModified:Boolean Read Get_HasBeenModified Write Set_HasBeenModified;
   end;

var
    MainEditForm      :TMainEditForm;
    ActiveScriptForm  :TMainEditForm;
    ScriptWindowList  :TObjectList;
    RecordCommands    :Boolean;

implementation

Uses RichEdit, Executive, DSSGlobals, DSSForms,  Panel,Utilities, uComplex;

{$R *.FMX}

const
     ModifiedColor =  13434879;

procedure TMainEditForm.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{
  if (Key = vkReturn) then begin
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
//DavisMMP
{  line1 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0,
    Editor.SelStart);
  line2 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0,
    Editor.SelStart + Editor.SelLength);
  col := (Editor.SelStart -
    SendMessage(Editor.Handle, EM_LINEINDEX, line1, 0));  }
end;

procedure TMainEditForm.EditorSelectionChange(Sender: TObject);
begin
   UpdateCursorPos;
end;

function TMainEditForm.BuildCommandList: Boolean;
var
  i : Integer;
  str : string;
  InBlockComment : Boolean;
begin
  result := False;
  InBlockComment := False;
  cmdList.Clear;

  {  6-28-2012
     Block Comment  Handling added
     /*       (beginning of line
     ...
        */    (anywhere)
  }

  for i := line1 to line2 do begin
  //DavisMMP
  //      str := Trim (Editor.Lines.Strings[i]);

      if Length(str) > 0 then
      Begin
         if Not InBlockComment then     // look for '/*'  at baginning of line
            case str[1] of
               '/': if (Length(str) > 1) and (str[2]='*')then
                    InBlockComment := TRUE;
            end;
            If Not InBlockComment Then cmdList.Add (str);
        // in block comment ... look for */   and cancel block comment (whole line)
        if InBlockComment then
          if Pos('*/', str)>0 then  InBlockComment := FALSE;
      End;

      {
        NOTE:  InBlockComment resets to FALSE upon leaving this routine
        So if you fail to select a line containing the end of the block comment,
        the next selection will not be blocked.
      }
  end;
  if cmdList.Count > 0 then result := True;
end;

procedure TMainEditForm.ExecuteCommandList;
var
   i, imax  :Integer;
begin
  SolutionAbort := FALSE;
  imax := cmdList.Count - 1;
  if imax < 0 then Exit;

  SolutionWasAttempted[ActiveActor] := FALSE;      // Global variable

Screen_Cursor_crHourGlass;
  for i := 0 to imax do begin
    If Not SolutionAbort Then Begin  // If script involves step that gets aborted, just flush the script
      DSSExecutive.Command := ActiveScriptForm.cmdList.Strings[i];
      If LastCommandWasCompile and Not IsDLL Then Begin
         ControlPanel.AddCompiledFile(LastFileCompiled);
         ControlPanel.UpdateElementBox;
      End;
    End;
  end;
Screen_Cursor_crDefault;

  If Not IsDLL Then
  Begin
        UpdateResultForm;
        UpdateSummaryForm;
        If Assigned(ActiveCircuit[ActiveActor]) Then With ActiveCircuit[ActiveActor] Do
        if (SolutionWasAttempted[ActiveActor]) and (Not IsSolved) then Begin
System.SysUtils.Beep;
//DavisMMP
//            ControlPanel.ResultPages.ActivePage := ControlPanel.SummaryTab;
        End;
        ControlPanel.UpdateStatus;
  End;
end;

Procedure TMainEditForm.ExecuteDSSCommand(Const S:String);
Begin
  SolutionAbort := FALSE;
  DSSExecutive.Command := S;
//DavisMMP
//  If RecordCommands then Editor.Lines.Append(S);

  If Not IsDLL Then  Begin
      UpdateResultForm;
      UpdateSummaryForm;
  End;
End;


procedure TMainEditForm.FormActivate(Sender: TObject);
begin
     ActiveScriptForm := Self;
     SetFormColor;
end;

procedure TMainEditForm.FormCreate(Sender: TObject);
begin
   cmdList := TStringList.Create;
   //DavisMMP
{   Editor.Clear;
   UpdateCursorPos;
   HasFileName := FALSE;
   IsMainWindow := FALSE;
   Editor.Font.Size  := DefaultFontSize;
   Editor.Font.Name  := DefaultFontName;
   Editor.Font.Style := DefaultFontStyles;   }
end;

procedure TMainEditForm.FormDestroy(Sender: TObject);
begin
  cmdList.Free;
end;

procedure TMainEditForm.UpdateResultform;
begin
//DavisMMP
{     ControlPanel.ResultsEdit.Clear;
     ControlPanel.ResultsEdit.Lines.Add(GlobalResult);
     If Length(GlobalResult)>0 Then  ControlPanel.ResultPages.ActivePage := ControlPanel.ResultsTab;
     If Not IsDLL Then ControlPanel.Edit_Result.Text := GlobalResult; }
end;

procedure TMainEditForm.UpdateSummaryForm;
Var
     cLosses, cPower:Complex;
begin
//DavisMMP
 {
  With ControlPanel.SummaryEdit Do
  Begin
      Clear;
      Lines.BeginUpdate;

      If ActiveCircuit[ActiveActor]<>nil Then
        With Lines Do
        Begin
           IF ActiveCircuit[ActiveActor].Issolved Then Add('Status = SOLVED')
           Else Begin
             SelAttributes.Color := clRed;
             Add('Status = NOT Solved');
             SelAttributes.Color := clBlack;
           End;
           Add('Solution Mode = ' + GetSolutionModeID);
           Add('Number = ' + IntToStr(ActiveCircuit[ActiveActor].Solution.NumberofTimes));
           Add('Load Mult = '+ Format('%5.3f', [ActiveCircuit[ActiveActor].LoadMultiplier]));
           Add('Devices = '+ Format('%d', [ActiveCircuit[ActiveActor].NumDevices]));
           Add('Buses = ' + Format('%d', [ActiveCircuit[ActiveActor].NumBuses]));
           Add('Nodes = ' + Format('%d', [ActiveCircuit[ActiveActor].NumNodes]));
           Add('Control Mode =' + GetControlModeID);
           Add('Total Iterations = '+IntToStr(ActiveCircuit[ActiveActor].Solution.Iteration));
           Add('Control Iterations = '+IntToStr(ActiveCircuit[ActiveActor].Solution.ControlIteration));
           Add('Max Sol Iter = ' +IntToStr(ActiveCircuit[ActiveActor].Solution.MostIterationsDone ));
           Add(' ');
           Add(' - Circuit Summary -');
           Add(' ');
           If ActiveCircuit[ActiveActor] <> Nil Then
             If ActiveCircuit[ActiveActor].Issolved and not ActiveCircuit[ActiveActor].BusNameRedefined Then
             Begin
               TRY
                 Add(Format('Year = %d ',[ActiveCircuit[ActiveActor].Solution.Year]));
                 Add(Format('Hour = %d ',[ActiveCircuit[ActiveActor].Solution.DynaVars.intHour]));
                 Add('Max pu. voltage = '+Format('%-.5g ',[GetMaxPUVoltage]));
                 Add('Min pu. voltage = '+Format('%-.5g ',[GetMinPUVoltage(TRUE)]));
                 cPower :=  CmulReal(GetTotalPowerFromSources(ActiveActor), 0.000001);  // MVA
                 Add(Format('Total Active Power:   %-.6g MW',[cpower.re]));
                 Add(Format('Total Reactive Power: %-.6g Mvar',[cpower.im]));
                 cLosses := CmulReal(ActiveCircuit[ActiveActor].Losses[ActiveActor], 0.000001);
                 If cPower.re <> 0.0 Then Add(Format('Total Active Losses:   %-.6g MW, (%-.4g %%)',[cLosses.re,(Closses.re/cPower.re*100.0)]))
                                     Else Add('Total Active Losses:   ****** MW, (**** %%)');
                 Add(Format('Total Reactive Losses: %-.6g Mvar',[cLosses.im]));
                 Add(Format('Frequency = %-g Hz',[ActiveCircuit[ActiveActor].Solution.Frequency]));
                 Add('Mode = '+GetSolutionModeID);
                 Add('Control Mode = '+GetControlModeID);
                 Add('Load Model = '+GetLoadModel);
               EXCEPT
                  On E:Exception Do Add('Error encountered. Re-solve circuit.');
               END;
            End;

         If Not IsDLL Then   ControlPanel.Text  := 'DSS Main Control Panel: Active Circuit = ' + ActiveCircuit[ActiveActor].Name;
        End
        Else
        With Lines Do Begin
          Add('No Circuits Have been Defined for this Actor');
        End;

      {Status ...}

 //     If Not IsDLL Then ControlPanel.UpdateStatus;

 //     Lines.EndUpdate;
//  End;

end;

procedure TMainEditForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
    IF Self <> MainEditForm Then Begin
       If HasBeenModified Then
         Case MessageDlg('File '+Caption+' has changed.  Save ?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) of
              mrYes: Begin
                        SaveEditorContents;
                     End;
         Else    {no}
         End;
(*
       Case MessageDlg('Do you want to close file '+Caption+'  ?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) of
            mrYes: Begin
*)
                    ScriptWindowList.Remove(Self);
//** MIDA : Action := caFree;
(*
                   End
       Else
          Action := caNone; {do Nothing}
       End;
*)

    End;


end;

procedure TMainEditForm.SetFormColor;
begin
//DavisMMP
//  If Editor.Modified Then Editor.Color := ModifiedColor Else Editor.Color := clWindow;
end;

procedure TMainEditForm.EditorChange(Sender: TObject);
begin
//DavisMMP
//      If Editor.Color <> ModifiedColor Then  SetFormColor;
end;

function TMainEditForm.Get_HasBeenModified: Boolean;
begin
//DavisMMP
//        Result := Editor.Modified;
end;

procedure TMainEditForm.Set_HasBeenModified(const Value: Boolean);
begin
//DavisMMP
//    Editor.Modified := Value;
    SetFormColor;
end;

procedure TMainEditForm.Do2Click(Sender: TObject);

begin
//DavisMMP
{   if Editor.SelLength > 0 then begin
      if BuildCommandList then begin // execute selection
          ExecuteCommandList;
      end
    end
    else begin // select and execute current line
{      loc.x := x;
      loc.y := y;
      ret := LoWord (SendMessage (Editor.Handle, EM_CHARFROMPOS, 0, Integer (@loc)));
      line1 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0, ret);
}
{      Line1 := Editor.CaretPos.y;
      line2 := line1;
     { col := (ret - SendMessage(Editor.Handle, EM_LINEINDEX, line1, 0));}
{      if BuildCommandList then begin
          ExecuteCommandList;
      end;
      {Editor.SelStart := ret}
{    end;
    }
end;

Procedure TMainEditForm.SaveEditorContents;
Var
        Save_Cursor:TCursor;
Begin

  Save_Cursor := Screen.Cursor;

  Screen.Cursor := crHourglass;    { Show hourglass cursor }

  Try

   Try
   //DavisMMP
{       Editor.PlainText := True;
       Editor.Lines.SaveToFile (Caption, TEncoding.ANSI);
       Editor.PlainText := False;     }
       HasBeenModified := FALSE;
       HasFileName := TRUE;

   Except
       On E:Exception Do DoSimpleMsg('Error saving file: '+E.message, 310);
   End;

  Finally
    Screen.Cursor := Save_Cursor;  { Always restore to normal }
  end;
End;

procedure TMainEditForm.Save3Click(Sender: TObject);
begin
      If Not HasFileName Then  ControlPanel.SaveScriptWindow1Click(Sender)
      Else SaveEditorContents;
end;

procedure TMainEditForm.ChangetothisDir1Click(Sender: TObject);
Var
    CurrDir :String;
begin
    CurrDir := ExtractFileDir(Caption);
    SetCurrentDir(CurrDir);
    SetDataPath(CurrDir);  // change datadirectory
    If Not IsDLL Then ControlPanel.UpdateStatus;
end;

procedure TMainEditForm.CloseWindow1Click(Sender: TObject);
begin
        Close;
end;

procedure TMainEditForm.OpenSelectedFile1Click(Sender: TObject);
Var
    TempActiveForm:TMainEditForm;
    FileName:String;
begin
//DavisMMP
{        ExtendSelection;
        If Editor.SelLength>0 Then Begin

        Try
              FileName :=  TrimParens(Trim(Editor.SelText));
              If FileExists(FileName)Then Begin
                TempActiveForm := TMainEditForm.Create(nil);
                TempActiveForm.Editor.Lines.LoadFromFile(FileName);
                TempActiveForm.Text  := ExpandFileName(FileName);
                ScriptWindowList.Add(TempActiveForm);
                ActiveScriptForm := TempActiveForm;
                ActiveScriptForm.HasBeenModified := FALSE;
                ActiveScriptForm.HasFileName := TRUE;
              End
              Else DoSimpleMsg('File "'+Editor.SelText+'" not found in currentdirectory: "'+GetCurrentDir+'"', 311);
        Except
            On E:Exception Do DoSimpleMsg('Error opening new window: '+ E.Message, 312);
        End;
        End;   }
end;

procedure TMainEditForm.EditSelectedFile1Click(Sender: TObject);
Var
    FileName:String;
begin
        ExtendSelection;
        //DavisMMP
{
        If Editor.SelLength>0 Then Begin

        Try
              FileName :=  TrimParens(Trim(Editor.SelText));
              If FileExists(FileName)Then  FireOffEditor(FileName)
              Else DoSimpleMsg('File "' + FileName + '" not found in currentdirectory: "'+GetCurrentDir+'"', 313);
        Except
            On E:Exception Do DoSimpleMsg('Error opening Editor: '+ E.Message, 314);
        End;
        End;     }

end;

function TMainEditForm.TrimParens( S: String): String;
begin
{Get rid of leading and trailing Parens}
        Result := S;
        Case S[1] of
          '(': Begin
                Result := Copy(S, 2, Length(S)-1);
                If Result[Length(Result)]=')' Then SetLength(Result, Length(Result)-1);
               End;
          '"': Begin
                Result := Copy(S, 2, Length(S)-1);
                If Result[Length(Result)]='"' Then SetLength(Result, Length(Result)-1);
               End;
          '''':Begin
                Result := Copy(S, 2, Length(S)-1);
                If Result[Length(Result)]='''' Then SetLength(Result, Length(Result)-1);
               End;
          '[': Begin
                Result := Copy(S, 2, Length(S)-1);
                If Result[Length(Result)]=']' Then SetLength(Result, Length(Result)-1);
               End;
          '{': Begin
                Result := Copy(S, 2, Length(S)-1);
                If Result[Length(Result)]='}' Then SetLength(Result, Length(Result)-1);
               End;
        End;

end;

procedure TMainEditForm.ExtendSelection;
Var
    i, LineIdx, Slen:Integer;
    Pos:TPoint;
begin
//DavisMMP
{
        If Editor.SelLength=0 Then Begin
          Pos := Editor.CaretPos ;
          LineIdx := Pos.y; // SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0, Editor.SelStart);
           // Backup from cursor position until blank found or BOL
           i := Pos.x;
           While (i>0) and Not
             ((Editor.Lines.Strings[LineIdx][i] = ' ') or (Editor.Lines.Strings[LineIdx][i] = '=') )
           Do Begin
              Dec(i);
              Editor.SelStart := Editor.SelStart-1 ;
           End;

           // Now go forward until a blank or EOL
           inc(i);
           slen := Length(Editor.Lines.Strings[LineIdx]);
           While (i <= slen) and (Editor.Lines.Strings[LineIdx][i] <> ' ') Do Begin
            inc(i);
            Editor.SelLength := Editor.SelLength + 1;
           End;

        End;
}
end;

//DavisMMP
{
procedure TMainEditForm.FontDialog1Apply(Sender: TObject; Wnd: HWND);
begin

   Editor.SelAttributes.Assign(TFontDialog(Sender).Font)

end;
}
procedure TMainEditForm.FontBtnClick(Sender: TObject);
Var
    FontSave :TFont;
begin
        // First select all
//DavisMMP
{        Editor.SelStart := 0;
        Editor.SelLength := Editor.GetTextLen;
        With FontDialog1 do  Begin
           FontSave := Editor.Font;
           Font := Editor.Font;
           Options := Options + [fdApplyButton];
           If Execute then Begin
              Editor.SelAttributes.Assign(Font);
              Editor.Font := Font;
              DefaultFontSize   := Editor.Font.Size;
              DefaultFontName   := Editor.Font.Name;
              DefaultFontStyles := Editor.Font.Style;
           End
           Else Editor.Font := FontSave;
        End;}
end;

end.
