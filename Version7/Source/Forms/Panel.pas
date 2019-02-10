unit Panel;

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
    Menus,
    ToolWin,
    ImgList,
    ScriptEdit,
    ExtCtrls,
    PsAPI
  {$IFDEF VER320}    // Tokyo
    ,
    System.ImageList
  {$ENDIF}
  {$IFDEF VER300}    // Seattle
    ,
    System.ImageList
  {$ENDIF}
    ;

type
    TControlPanel = class(TForm)
        OpenDialog1: TOpenDialog;
        MainMenu1: TMainMenu;
        file1: TMenuItem;
        Edit1: TMenuItem;
        show1: TMenuItem;
        Exit1: TMenuItem;
        N1: TMenuItem;
        Compile1: TMenuItem;
        Redirect1: TMenuItem;
        NewScriptWindow1: TMenuItem;
        ToolBar1: TToolBar;
        ToolButton1: TToolButton;
        ToolButton2: TToolButton;
        ToolButton3: TToolButton;
        ToolButton4: TToolButton;
        ToolButton5: TToolButton;
        Clear1: TMenuItem;
        VoltagesLN1: TMenuItem;
        VoltagesLL1: TMenuItem;
        Currents1: TMenuItem;
        PowerskVA1: TMenuItem;
        PowersMVA1: TMenuItem;
        N4: TMenuItem;
        Zone1: TMenuItem;
        Meters1: TMenuItem;
        Plot1: TMenuItem;
        Circuit1: TMenuItem;
        Zone2: TMenuItem;
        Daisy1: TMenuItem;
        Help1: TMenuItem;
        Set1: TMenuItem;
        Mode1: TMenuItem;
        DSSHelp1: TMenuItem;
        ScriptEditorHelp1: TMenuItem;
        N2: TMenuItem;
        AboutDSS1: TMenuItem;
        Isolated1: TMenuItem;
        Generators1: TMenuItem;
        Elementsw1: TMenuItem;
        Buses1: TMenuItem;
        EventLog1: TMenuItem;
        Monitor1: TMenuItem;
        ImageList1: TImageList;
        LastFile1: TMenuItem;
        File2: TMenuItem;
        ResultFile1: TMenuItem;
        N3: TMenuItem;
        N5: TMenuItem;
        Reset1: TMenuItem;
        All1: TMenuItem;
        N6: TMenuItem;
        Interpolate1: TMenuItem;
        Save1: TMenuItem;
        Monitors1: TMenuItem;
        EnergyMeters1: TMenuItem;
        Zones1: TMenuItem;
        Controls1: TMenuItem;
        EventLog2: TMenuItem;
        KeepList1: TMenuItem;
        LoadMultiplier1: TMenuItem;
        AllocationFactors1: TMenuItem;
        ToolButton6: TToolButton;
        ToolButton7: TToolButton;
        ToolButton8: TToolButton;
        ToolButton9: TToolButton;
        StatusBar1: TStatusBar;
        RecordScript1: TMenuItem;
        ToolButton10: TToolButton;
        ToolButton11: TToolButton;
        ClassBox: TComboBox;
        ElementBox: TComboBox;
        ToolButton12: TToolButton;
        ToolButton13: TToolButton;
        EditActive1: TMenuItem;
        SelectActive1: TMenuItem;
        Options1: TMenuItem;
        Option1: TMenuItem;
        Bus1: TMenuItem;
        LoadModel1: TMenuItem;
        Editor1: TMenuItem;
        Datapath1: TMenuItem;
        DefaultDaily1: TMenuItem;
        Number1: TMenuItem;
        Growth1: TMenuItem;
        Year1: TMenuItem;
        ToolButton14: TToolButton;
        ToolButton15: TToolButton;
        MakeBusList1: TMenuItem;
        SaveAllMonitors1: TMenuItem;
        N8: TMenuItem;
        ToolButton16: TToolButton;
        ElementsinClass1: TMenuItem;
        Do1: TMenuItem;
        AlignFile1: TMenuItem;
        N9: TMenuItem;
        Make1: TMenuItem;
        BusList1: TMenuItem;
        PosSeqEquiv1: TMenuItem;
        NewYMatrix1: TMenuItem;
        Selection1: TMenuItem;
        CalcVoltageBases1: TMenuItem;
        TakeSample1: TMenuItem;
        Initialize1: TMenuItem;
        RebuildYMatrix1: TMenuItem;
        Interpolate2: TMenuItem;
        Reduce1: TMenuItem;
        AllocateLoads1: TMenuItem;
        Default1: TMenuItem;
        Stubs1: TMenuItem;
        TapsandEnds1: TMenuItem;
        BreakLoops1: TMenuItem;
        MergeParallel1: TMenuItem;
        ZonemeterTreeview1: TMenuItem;
        Zone3: TMenuItem;
        N10: TMenuItem;
        Command1: TMenuItem;
        N11: TMenuItem;
        N12: TMenuItem;
        TrueFalse1: TMenuItem;
        DemandInterval1: TMenuItem;
        ZonesLocked1: TMenuItem;
        DuplicatesAllowed1: TMenuItem;
        TraceLog1: TMenuItem;
        Trapezoidal1: TMenuItem;
        SaveScriptWindow1: TMenuItem;
        SaveDialog1: TSaveDialog;
        AutoaddLog1: TMenuItem;
        GeneralBusData1: TMenuItem;
        CircuitPlot1: TMenuItem;
        N13: TMenuItem;
        Monitor2: TMenuItem;
        Loadshape1: TMenuItem;
        TCCCurve1: TMenuItem;
        Losses1: TMenuItem;
        ToolButton17: TToolButton;
        ToolButton18: TToolButton;
        List1: TMenuItem;
        Powers1: TMenuItem;
        Voltages1: TMenuItem;
        Loops1: TMenuItem;
        VoltagesLNNodes1: TMenuItem;
        VoltagesLNElements1: TMenuItem;
        VoltageLLNodes1: TMenuItem;
        PowerkVAElem1: TMenuItem;
        PowersMVAElem1: TMenuItem;
        Currents2: TMenuItem;
        CurrentsElem1: TMenuItem;
        Open1: TMenuItem;
        Save2: TMenuItem;
        N7: TMenuItem;
        ToolBar2: TToolBar;
        EditFileBtn: TToolButton;
        CompileCombo: TComboBox;
        CompileBtn: TToolButton;
        ToolButton22: TToolButton;
        PopupMenuCombo: TPopupMenu;
        Popup1Compile: TMenuItem;
        Popup1Edit: TMenuItem;
        PopUp1Delete: TMenuItem;
        Popup1ClearList: TMenuItem;
        PopUp1ChDir: TMenuItem;
        GeneralLineData1: TMenuItem;
        ToolButton19: TToolButton;
        Converged1: TMenuItem;
        BusFlow1: TMenuItem;
        LineConstants1: TMenuItem;
        Export1: TMenuItem;
        Voltages2: TMenuItem;
        Currents3: TMenuItem;
        Powers2: TMenuItem;
        FaultCurrents1: TMenuItem;
        Overloads1: TMenuItem;
        Unserved1: TMenuItem;
        Generators2: TMenuItem;
        Loads1: TMenuItem;
        Meters2: TMenuItem;
        Monitors2: TMenuItem;
        Visual1: TMenuItem;
        CurrentsElem2: TMenuItem;
        VoltagesElement1: TMenuItem;
        PowersElement1: TMenuItem;
        ToolButton21: TToolButton;
        ToolButton23: TToolButton;
        ToolButton24: TToolButton;
        ToolButton25: TToolButton;
        CurrentsElemResid1: TMenuItem;
        RPNEvaluator1: TMenuItem;
        Yprims1: TMenuItem;
        Y1: TMenuItem;
        ToolButton20: TToolButton;
        LBL_DefaultFreq: TLabel;
        Edit_Result: TEdit;
        Capacity1: TMenuItem;
        SeqZ1: TMenuItem;
        Estimation1: TMenuItem;
        Buscoords1: TMenuItem;
        Sort1: TMenuItem;
        Losses2: TMenuItem;
        Mismatch1: TMenuItem;
        kVBaseMismatch1: TMenuItem;
        Summary2: TMenuItem;
        OpenDSSWiki1: TMenuItem;
        NodeNames1: TMenuItem;
        Taps1: TMenuItem;
        NodeOrder1: TMenuItem;
        Result1: TMenuItem;
        Phase1: TMenuItem;
        Sequence1: TMenuItem;
        Element1: TMenuItem;
        Phase2: TMenuItem;
        Sequence2: TMenuItem;
        Element2: TMenuItem;
        erminal1: TMenuItem;
        ByOhase1: TMenuItem;
        Sequence3: TMenuItem;
        Element3: TMenuItem;
        BaseClassCombo: TComboBox;
        BaseFrequency1: TMenuItem;
        BaseFrequcney501: TMenuItem;
        YMatrix1: TMenuItem;
        NodeList1: TMenuItem;
        VoltArray1: TMenuItem;
        CurrArray1: TMenuItem;
        EnergyMeters2: TMenuItem;
        Generators3: TMenuItem;
        PVSystems1: TMenuItem;
        Storage1: TMenuItem;
        P1: TMenuItem;
        Panel1: TPanel;
        Panel2: TPanel;
        Splitter23: TSplitter;
        Panel3: TPanel;
        Panel4: TPanel;
        Splitter45: TSplitter;
        Panel5: TPanel;
        ResultPages: TPageControl;
        EditPages: TPageControl;
        SummaryTab: TTabSheet;
        ResultsTab: TTabSheet;
        SummaryEdit: TRichEdit;
        ResultsEdit: TRichEdit;
        MessageEdit: TRichEdit;
        Label1: TLabel;
        PopupMenuScript: TPopupMenu;
        ScriptDoMnu: TMenuItem;
        ScriptSaveMnu: TMenuItem;
        ScriptCloseMnu: TMenuItem;
        ScriptDirMnu: TMenuItem;
        ScriptOpenMnu: TMenuItem;
        ScriptEditMnu: TMenuItem;
        FontDialog1: TFontDialog;
        N14: TMenuItem;
        ScriptFontMnu: TMenuItem;
        Summary1: TMenuItem;
        LinkstoHelpFiles1: TMenuItem;
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure DSSHelp1Click(Sender: TObject);
        procedure AboutDSS1Click(Sender: TObject);
        procedure Isolated1Click(Sender: TObject);
        procedure Compile1Click(Sender: TObject);
        procedure Redirect1Click(Sender: TObject);
        procedure Clear1Click(Sender: TObject);
        procedure Exit1Click(Sender: TObject);
        procedure VoltagesLN1Click(Sender: TObject);
        procedure VoltagesLL1Click(Sender: TObject);
        procedure Currents1Click(Sender: TObject);
        procedure PowerskVA1Click(Sender: TObject);
        procedure PowersMVA1Click(Sender: TObject);
        procedure Meters1Click(Sender: TObject);
        procedure Generators1Click(Sender: TObject);
        procedure Elementsw1Click(Sender: TObject);
        procedure Buses1Click(Sender: TObject);
        procedure EventLog1Click(Sender: TObject);
        procedure ToolButton3Click(Sender: TObject);
        procedure ToolButton2Click(Sender: TObject);
        procedure ScriptEditorHelp1Click(Sender: TObject);
        procedure LastFile1Click(Sender: TObject);
        procedure File2Click(Sender: TObject);
        procedure Interpolate1Click(Sender: TObject);
        procedure ResultFile1Click(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure NewScriptWindow1Click(Sender: TObject);
        procedure RecordScript1Click(Sender: TObject);
        procedure Zone1Click(Sender: TObject);
        procedure ClassBoxChange(Sender: TObject);
        procedure Zone2Click(Sender: TObject);
        procedure Options1Click(Sender: TObject);
        procedure Daisy1Click(Sender: TObject);
        procedure All1Click(Sender: TObject);
        procedure Monitors1Click(Sender: TObject);
        procedure EnergyMeters1Click(Sender: TObject);
        procedure Zones1Click(Sender: TObject);
        procedure Controls1Click(Sender: TObject);
        procedure EventLog2Click(Sender: TObject);
        procedure KeepList1Click(Sender: TObject);
        procedure Monitor1Click(Sender: TObject);
        procedure AutoAdded1Click(Sender: TObject);
        procedure Overloads1Click(Sender: TObject);
        procedure Variables1Click(Sender: TObject);
        procedure Faults1Click(Sender: TObject);
        procedure Convergence1Click(Sender: TObject);
        procedure Option1Click(Sender: TObject);
        procedure Mode1Click(Sender: TObject);
        procedure LoadMultiplier1Click(Sender: TObject);
        procedure AllocationFactors1Click(Sender: TObject);
        procedure EditActive1Click(Sender: TObject);
        procedure SelectActive1Click(Sender: TObject);
        procedure ElementBoxChange(Sender: TObject);
        procedure Number1Click(Sender: TObject);
        procedure Growth1Click(Sender: TObject);
        procedure Year1Click(Sender: TObject);
        procedure Bus1Click(Sender: TObject);
        procedure MakeBusList1Click(Sender: TObject);
        procedure LoadModel1Click(Sender: TObject);
        procedure LDCurve1Click(Sender: TObject);
        procedure DefaultDaily1Click(Sender: TObject);
        procedure Editor1Click(Sender: TObject);
        procedure Datapath1Click(Sender: TObject);
        procedure Save1Click(Sender: TObject);
        procedure ElementsinClass1Click(Sender: TObject);
        procedure AlignFile1Click(Sender: TObject);
        procedure BusList1Click(Sender: TObject);
        procedure PosSeqEquiv1Click(Sender: TObject);
        procedure NewYMatrix1Click(Sender: TObject);
        procedure Selection1Click(Sender: TObject);
        procedure CalcVoltageBases1Click(Sender: TObject);
        procedure TakeSample1Click(Sender: TObject);
        procedure Initialize1Click(Sender: TObject);
        procedure RebuildYMatrix1Click(Sender: TObject);
        procedure Interpolate2Click(Sender: TObject);
        procedure AllocateLoads1Click(Sender: TObject);
        procedure Default1Click(Sender: TObject);
        procedure Stubs1Click(Sender: TObject);
        procedure TapsandEnds1Click(Sender: TObject);
        procedure BreakLoops1Click(Sender: TObject);
        procedure MergeParallel1Click(Sender: TObject);
        procedure ZonemeterTreeview1Click(Sender: TObject);
        procedure Zone3Click(Sender: TObject);
        procedure Command1Click(Sender: TObject);
        procedure DemandInterval1Click(Sender: TObject);
        procedure ZonesLocked1Click(Sender: TObject);
        procedure DuplicatesAllowed1Click(Sender: TObject);
        procedure TraceLog1Click(Sender: TObject);
        procedure Trapezoidal1Click(Sender: TObject);
        procedure SaveScriptWindow1Click(Sender: TObject);
        procedure GeneralBusData1Click(Sender: TObject);
        procedure AutoaddLog1Click(Sender: TObject);
        procedure SaveAllMonitors1Click(Sender: TObject);
        procedure CircuitPlot1Click(Sender: TObject);
        procedure Loadshape1Click(Sender: TObject);
        procedure Ratings1Click(Sender: TObject);
        procedure Losses1Click(Sender: TObject);
        procedure Summary1Click(Sender: TObject);
        procedure List1Click(Sender: TObject);
        procedure Monitor2Click(Sender: TObject);
        procedure Loops1Click(Sender: TObject);
        procedure VoltagesLNNodes1Click(Sender: TObject);
        procedure VoltagesLNElements1Click(Sender: TObject);
        procedure VoltageLLNodes1Click(Sender: TObject);
        procedure PowerkVAElem1Click(Sender: TObject);
        procedure PowersMVAElem1Click(Sender: TObject);
        procedure CurrentsElem1Click(Sender: TObject);
        procedure Open1Click(Sender: TObject);
        procedure Save2Click(Sender: TObject);
        procedure PopUp1DeleteClick(Sender: TObject);
        procedure Popup1EditClick(Sender: TObject);
        procedure Popup1CompileClick(Sender: TObject);
        procedure PopUp1ChDirClick(Sender: TObject);
        procedure Popup1ClearListClick(Sender: TObject);
        procedure EditFileBtnClick(Sender: TObject);
        procedure CompileBtnClick(Sender: TObject);
        procedure GeneralLineData1Click(Sender: TObject);
        procedure Converged1Click(Sender: TObject);
        procedure BusFlow1Click(Sender: TObject);
        procedure LineConstants1Click(Sender: TObject);
        procedure SeqVoltages1Click(Sender: TObject);
        procedure FaultCurrents1Click(Sender: TObject);
        procedure Unserved1Click(Sender: TObject);
        procedure Generators2Click(Sender: TObject);
        procedure Loads1Click(Sender: TObject);
        procedure Monitors2Click(Sender: TObject);
        procedure ToolButton21Click(Sender: TObject);
        procedure CurrentsElem2Click(Sender: TObject);
        procedure VoltagesElement1Click(Sender: TObject);
        procedure PowersElement1Click(Sender: TObject);
        procedure CurrentsElemResid1Click(Sender: TObject);
        procedure RPNEvaluator1Click(Sender: TObject);
        procedure Yprims1Click(Sender: TObject);
        procedure Capacity1Click(Sender: TObject);
        procedure SeqZ1Click(Sender: TObject);
        procedure PowersByPhase1Click(Sender: TObject);
        procedure Estimation1Click(Sender: TObject);
        procedure Buscoords1Click(Sender: TObject);
        procedure Sort1Click(Sender: TObject);
        procedure Losses2Click(Sender: TObject);
        procedure Mismatch1Click(Sender: TObject);
        procedure kVBaseMismatch1Click(Sender: TObject);
        procedure Summary2Click(Sender: TObject);
        procedure OpenDSSWiki1Click(Sender: TObject);
        procedure TechNotes1Click(Sender: TObject);
        procedure NodeNames1Click(Sender: TObject);
        procedure TCCCurve1Click(Sender: TObject);
        procedure Taps1Click(Sender: TObject);
        procedure NodeOrder1Click(Sender: TObject);
        procedure Result1Click(Sender: TObject);
        procedure Phase1Click(Sender: TObject);
        procedure Sequence1Click(Sender: TObject);
        procedure Element1Click(Sender: TObject);
        procedure Phase2Click(Sender: TObject);
        procedure Sequence2Click(Sender: TObject);
        procedure Element2Click(Sender: TObject);
        procedure erminal1Click(Sender: TObject);
        procedure ByOhase1Click(Sender: TObject);
        procedure Sequence3Click(Sender: TObject);
        procedure Element3Click(Sender: TObject);
        procedure BaseClassComboChange(Sender: TObject);
        procedure BaseFrequency1Click(Sender: TObject);
        procedure BaseFrequcney501Click(Sender: TObject);
        procedure YMatrix1Click(Sender: TObject);
        procedure NodeList1Click(Sender: TObject);
        procedure VoltArray1Click(Sender: TObject);
        procedure CurrArray1Click(Sender: TObject);
        procedure EnergyMeters2Click(Sender: TObject);
        procedure Generators3Click(Sender: TObject);
        procedure PVSystems1Click(Sender: TObject);
        procedure Storage1Click(Sender: TObject);
        procedure P1Click(Sender: TObject);
        procedure ScriptFontClick(Sender: TObject);
        procedure ApplyFont(Sender: TObject; Wnd: HWND);
        procedure ScriptDoClick(Sender: TObject);
        procedure ScriptSaveClick(Sender: TObject);
        procedure ScriptCloseClick(Sender: TObject);
        procedure ScriptDirClick(Sender: TObject);
        procedure ScriptOpenClick(Sender: TObject);
        procedure ScriptEditClick(Sender: TObject);
        procedure EditPagesChange(Sender: TObject);
        procedure LinkstoHelpFiles1Click(Sender: TObject);
    PRIVATE
    { Private declarations }
        PlotOptionString: String;
        function MakeANewEditForm(const Cap: String): TScriptEdit;
        procedure UpdateCaptions;

    PUBLIC
    { Public declarations }

        EditFormCount: Integer;
        procedure InitializeForm;
        procedure AddCompiledFile(const filename: String);
        procedure UpdateStatus;
        procedure UpdateClassBox; // also UpdateCaptions
        procedure UpdateElementBox;
        procedure MakeBaseClassBox;
        procedure PopulateClassList(BaseClass: Word);
    end;


implementation

uses
    Executive,
    DSSClassDefs,
    DSSGlobals,
    ClipBrd,
    Utilities,
    contnrs,
    DlgPlotOptions,
    DSSPlot,
    FrmCSVchannelSelect,
    DlgComboBox,
    dlgNumber,
    ExecOptions,
    ExecCommands,
    ExecHelper,
    Dynamics,
    DSSClass,
    ListForm,
    Lineunits,
    Monitor,
    FrmDoDSSCommand,
    Frm_RPNcalc,
    DSSForms,
    showOptions,
    ShellAPI,
    IniRegSave,
    System.UITypes,
    System.Types;

{$R *.DFM}

var
    SelectedMonitor: String;

function WinStateToInt(WindowState: TWindowState): Integer;
begin
    case WindowState of
        wsNormal:
            Result := 0;
        wsMinimized:
            Result := 1;
        wsMaximized:
            Result := 2;
    else
        Result := 0;
    end;
end;

function IntToWinstate(Value: Integer): TWindowState;
begin
    case Value of
        0:
            Result := wsNormal;
        1:
            Result := wsMinimized;
        2:
            Result := wsMaximized;
    else
        Result := wsNormal;
    end;
end;

procedure DoSavePrompt(ActiveScriptForm: TScriptEdit);
begin
    case MessageDlg('File ' + ActiveScriptForm.Caption + ' has changed.  Save ?', mtConfirmation, [mbYes, mbNo], 0) of
        mrYes:
            ActiveScriptForm.SaveEditorContents;
    else
    end;
end;

function WritePanelRecord(idx: Integer; ActiveScriptForm: TScriptEdit): Boolean;
begin
    Result := FALSE;
    if ActiveScriptForm.HasFileName then
    begin
        DSS_Registry.WriteString(Format('File%d', [idx]), ActiveScriptForm.Caption);
        Result := TRUE;
    end;
end;

function BoolToInt(test: Boolean): Integer;
begin
    if test then
        result := -1
    else
        result := 0;
end;

procedure WriteWindowRecord(idx: Integer; ActiveScriptForm: TScriptEdit);
var
    i, imax: Integer;
    IsFileWindow: Boolean;
begin
    IsFileWindow := WritePanelRecord(idx, ActiveScriptForm);
    with ActiveScriptForm do
    begin
        DSS_Registry.WriteString(Format('Window%d', [idx]), Format(' %d', [idx]));
        if not IsFileWindow then
        begin   // just a general unsaved script window
            imax := Editor.Lines.Count - 1;
            DSS_Registry.WriteInteger(Format('LineCount%d', [idx]), Editor.Lines.Count);
            for i := 0 to imax do
                DSS_Registry.WriteString(Format('Lines%d_%d', [idx, i]), Editor.Lines.Strings[i]);
        end
        else
        begin  // Check for changes to Editor
            if HasBeenModified then
                DoSavePrompt(ActiveScriptForm);
        end;
    end;
end;

procedure TControlPanel.FormClose(Sender: TObject;
    var Action: TCloseAction);
  {Write present contents of the history box to registry}
var
    j: Integer;
begin
  // Main control panel
  //   script windows numbered 1..ScriptCount
  //   the main script window is #1 in the list
    DSS_Registry.Section := 'Panels';
    DSS_Registry.ClearSection;
    DSS_Registry.WriteString('MainWindow', Format(' %d, %d, %d, %d, %d',
        [Top, Left, Height, Width, WinStateToInt(WindowState)]));
    DSS_Registry.WriteInteger('ScriptCount', ScriptWindowList.Count);

    for j := 1 to ScriptWindowList.Count do
    begin
        ActiveScriptForm := TScriptEdit(ScriptWindowList.Items[j - 1]);
        WriteWindowRecord(j, ActiveScriptForm);
    end;

  {Write compile file combo}
    DSS_Registry.Section := 'Compiled';
    DSS_Registry.ClearSection;
    DSS_Registry.WriteInteger('Count', CompileCombo.Items.Count);
    for j := 0 to CompileCombo.Items.Count - 1 do
        DSS_Registry.WriteString(Format('Item%d', [j]), CompileCombo.Items[j]);
end;

procedure TControlPanel.DSSHelp1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('help');
end;

procedure TControlPanel.AboutDSS1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('about');
end;

procedure TControlPanel.Isolated1Click(Sender: TObject);
begin
    Screen.Cursor := crHourglass;
    ActiveScriptForm.ExecuteDSSCommand('show isolated');
    Screen.Cursor := crDefault;
end;

procedure TControlPanel.Compile1Click(Sender: TObject);
begin

    with OpenDialog1 do
    begin
        FileName := '';
        DefaultExt := 'dss';
        Filter := 'DSS files (*.dss)|*.DSS|Text files (*.txt)|*.TXT|All files (*.*)|*.*';
        Title := 'Select File Containing DSS Commands';
        if Execute then
        begin
            Screen.Cursor := crHourglass;
      // Enclose string in DSS quotes to handle blanks in name
            ActiveScriptForm.ExecuteDSSCommand('Compile (' + Filename + ')');
            Screen.Cursor := crDefault;
            LastFileCompiled := FileName;
            AddCompiledFile(LastFileCompiled);
        end;
    end;


end;

procedure TControlPanel.Redirect1Click(Sender: TObject);
begin

    with OpenDialog1 do
    begin
        FileName := '';
        DefaultExt := 'dss';
        Filter := 'DSS files (*.dss)|*.DSS|Text files (*.txt)|*.TXT|All files (*.*)|*.*';
        Title := 'Select File Containing DSS Commands';
        if Execute then
        begin
            Screen.Cursor := crHourglass;
      // Enclose string in DSS quotes to handle blanks in name
            ActiveScriptForm.ExecuteDSSCommand('redirect (' + Filename + ')');
            Screen.Cursor := crDefault;
            LastFileCompiled := FileName;
        end;
    end;
end;

procedure TControlPanel.Clear1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('clear');
end;

procedure TControlPanel.Exit1Click(Sender: TObject);
begin
    Close;
end;

procedure TControlPanel.VoltagesLN1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show Voltage');
end;

procedure TControlPanel.VoltagesLL1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show Voltage LL');
end;

procedure TControlPanel.CurrArray1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export YCurrents');
end;

procedure TControlPanel.Currents1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show current');
end;

procedure TControlPanel.PowerskVA1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show Power');
end;

procedure TControlPanel.PowersMVA1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show Power MVA');
end;

procedure TControlPanel.Meters1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show Meters');
end;

procedure TControlPanel.Generators1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show Generators');
end;

procedure TControlPanel.Elementsw1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show elements');

end;

procedure TControlPanel.Buses1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show buses');
end;

procedure TControlPanel.EventLog1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show Eventlog');
end;

procedure TControlPanel.ToolButton3Click(Sender: TObject);
begin
    Screen.Cursor := crHourglass;
    ActiveScriptForm.ExecuteDSSCommand('solve');
    if not ActiveCircuit.IsSolved then
        ResultPages.ActivePage := SummaryTab;
    Screen.Cursor := crDefault;
end;

procedure TControlPanel.ToolButton2Click(Sender: TObject);
begin
    Screen.Cursor := crHourglass;
    with ActiveScriptForm do
        if BuildCommandList then
            ExecuteCommandList;
    Screen.Cursor := crDefault;
end;

procedure TControlPanel.ScriptDirClick(Sender: TObject);
begin
    ActiveScriptForm.ChangeToThisDir;
end;

procedure TControlPanel.ScriptCloseClick(Sender: TObject);
begin
    if ActiveScriptForm.CheckEditorClose = TRUE then
    begin
        EditPages.Pages[EditPages.ActivePageIndex].Free;
        ScriptWindowList.Remove(ActiveScriptForm);
        ActiveScriptForm := TScriptEdit(EditPages.ActivePage.Tag);
    end;
end;

procedure TControlPanel.ScriptDoClick(Sender: TObject);
begin
    ActiveScriptForm.DoSelection;
end;

procedure TControlPanel.ScriptEditClick(Sender: TObject);
begin
    ActiveScriptForm.EditSelectedFile;
end;

procedure TControlPanel.ScriptEditorHelp1Click(Sender: TObject);
begin
    with ActiveScriptForm do
    begin
        Editor.Lines.Add('/**** Begin Block Comment ****');
        Editor.Lines.Add('This is a Windows Richedit form with typical formatting and cut, copy, and paste features');
        Editor.Lines.Add('');
        Editor.Lines.Add('You may edit text as you would for many standard text editors.');
        Editor.Lines.Add('You may execute one or more lines of script by selecting all or part');
        Editor.Lines.Add('of a line or simply positioning the cursor in a line.');
        Editor.Lines.Add('');
        Editor.Lines.Add('Right-click to see your options.');
        Editor.Lines.Add('');
        Editor.Lines.Add('You may open files by selecting all or part of the filename in the script.');
        Editor.Lines.Add('');
        Editor.Lines.Add('With no selection (cursor in a line):');
        Editor.Lines.Add('==============');
        Editor.Lines.Add('  Right-click and selected Do Selected to execute the whole line.');
        Editor.Lines.Add('  Whole lines are sent to the OpenDSS command interpreter.');
        Editor.Lines.Add('  File names without blanks are automatically selected.');
        Editor.Lines.Add('  If the file name contains blanks, select the entire name manually.');
        Editor.Lines.Add('');
        Editor.Lines.Add('With a selection:');
        Editor.Lines.Add('=============');
        Editor.Lines.Add('  Right-click to execute all selected lines.');
        Editor.Lines.Add('  Lines partially selected are sent as whole lines.');
        Editor.Lines.Add('');
        Editor.Lines.Add('**** End Block Comment ****/');
    end;
end;

procedure TControlPanel.ApplyFont(Sender: TObject; Wnd: HWND);
begin
    ActiveScriptForm.Editor.SelAttributes.Assign(TFontDialog(Sender).Font)
end;

procedure TControlPanel.ScriptFontClick(Sender: TObject);
var
    FontSave: TFont;
begin
    with ActiveScriptForm do
    begin
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
end;

procedure TControlPanel.ScriptOpenClick(Sender: TObject);
var
    FileName: String;
begin
    FileName := ActiveScriptForm.GetSelectedFileName;
    if Length(FileName) > 0 then
    begin
        try
            ActiveScriptForm := MakeANewEditForm(FileName);
            ActiveScriptForm.Editor.Lines.LoadFromFile(FileName);
            ActiveScriptForm.Caption := ExpandFileName(FileName);
            ActiveScriptForm.HasBeenModified := FALSE;
            ActiveScriptForm.HasFileName := TRUE;
        except
            On E: Exception do
                DoSimpleMsg('Error opening new window: ' + E.Message, 312);
        end;
    end;
    UpdateCaptions;
end;

procedure TControlPanel.ScriptSaveClick(Sender: TObject);
begin
    ActiveScriptForm.SaveSelection;
end;

procedure TControlPanel.LastFile1Click(Sender: TObject);
begin
    FireOffEditor(LastFileCompiled);
end;

procedure TControlPanel.File2Click(Sender: TObject);
begin
    with OpenDialog1 do
    begin
        FileName := '';
        DefaultExt := 'dss';
        Filter := 'DSS files {*.dss)|*.dss|Text files (*.txt)|*.TXT|All files (*.*)|*.*';
        Title := 'Select File to Edit';
        if Execute then
            FireOffEditor(FileName);
    end;
end;

procedure TControlPanel.Interpolate1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('interpolate');
end;

procedure TControlPanel.Result1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Result');
end;

procedure TControlPanel.ResultFile1Click(Sender: TObject);
begin
 //   ActiveScriptForm.ExecuteDSSCommand('fileedit ['+ ResultsEdit.Lines.Strings[0]+']');
    ActiveScriptForm.ExecuteDSSCommand('fileedit [' + Edit_Result.Text + ']');
end;

function TControlPanel.MakeANewEditForm(const Cap: String): TScriptEdit;
var
    ts: TTabSheet;
    re: TRichEdit;
    id: Integer;
begin
    ts := TTabSheet.Create(EditPages);
    ts.PageControl := EditPages;
    id := EditPages.PageCount;
    if id > 1 then
        ts.Caption := 'Script' + IntToStr(id)
    else
        ts.Caption := 'Main';
    re := TRichEdit.Create(ts);
    re.Parent := ts;
    re.Align := alClient;
    re.ScrollBars := ssBoth;
    re.Font.Size := DefaultFontSize;
    re.Font.Name := DefaultFontName;
    re.Font.Style := DefaultFontStyles;
    Result := TScriptEdit.Create;
    Result.Editor := re;
    Result.Tab := ts;
    re.OnChange := Result.EditorChange;
    re.OnSelectionChange := Result.EditorSelectionChange;
    Result.Caption := Cap;
    ScriptWindowList.Add(Result);
    EditPages.ActivePage := ts;
    ts.Tag := NativeInt(Result);
end;

procedure TControlPanel.FormDestroy(Sender: TObject);
var
    i: Integer;
begin
   {Free All the forms not created by the main program}
    for i := 1 to ScriptWindowList.Count do
    begin
        TScriptEdit(ScriptWindowList.Items[i - 1]).Free;
    end;
end;

procedure TControlPanel.InitializeForm;
{Reinitialize contents of script forms box from last usage.}

var
    CmdLineFileName: String;
    TextLine: String;
    FileName: String;

    CmdLineFileFound,
    WindowExistsAlready: Boolean;

    i, j: Integer;
    TestForm: TScriptEdit;
    nScripts, nLines, nCompiled: Integer;

begin
    ScriptWindowList := TObjectList.Create;
    ScriptWindowList.Clear;
    ScriptWindowList.OwnsObjects := FALSE;

  // make sure the MainEditForm is created, even if not in the registry
  // below, we load MainEditForm from the registry script window #1
    if not Assigned(MainEditForm) then
        MainEditForm := MakeANewEditForm('Main');
    MainEditForm.isMainWindow := TRUE;
    ActiveScriptForm := MainEditForm;

    PlotOptionString := ' max=2000 n n';

    CompileCombo.Clear;

  // Main Form
    DSS_Registry.Section := 'Panels';
    TextLine := DSS_Registry.ReadString('MainWindow', '100, 100, 600, 800, 0');
    nScripts := DSS_Registry.ReadInteger('ScriptCount', 0);
//  ProcessWindowState (Self, TextLine);
  {Make sure the Main Form is on screen}
    if (Self.Left > Screen.Width) or (Self.Left < 0) then
        Self.Left := 0;

  // Now process script forms and other child forms
    EditFormCount := 1; // main script window was created above
    for i := 1 to nScripts do
    begin // the MainEditForm is #1
        nLines := DSS_Registry.ReadInteger(Format('LineCount%d', [i]), 0);
        if i > 1 then
        begin // need to make a new edit form
            Inc(EditFormCount);
            if nLines < 1 then
            begin   // if no lines stored in Registry
                FileName := DSS_Registry.ReadString(Format('File%d', [i]), '');
                ActiveScriptForm := MakeANewEditForm(FileName);
            end
            else
            begin
                ActiveScriptForm := MakeANewEditForm('Script' + InttoStr(EditFormCount))
            end;
        end;
        ActiveScriptForm.Editor.Lines.BeginUpdate;
        if (nLines < 1) and FileExists(FileName) then
        begin // try loading a file
            try
                ActiveScriptForm.Editor.Lines.LoadFromFile(FileName);
                ActiveScriptForm.HasFileName := TRUE;
                UpdateCaptions;
            except  // ignore error -- likely file got moved
            end;
        end
        else
        begin // read collection of saved lines into the script window
            for j := 0 to nLines - 1 do
            begin
                TextLine := DSS_Registry.ReadString(Format('Lines%d_%d', [i, j]), '');
                ActiveScriptForm.Editor.Lines.Add(TextLine);
            end;
        end;
        ActiveScriptForm.HasBeenModified := FALSE; // should not be yellow after restoration
        ActiveScriptForm.Editor.Lines.EndUpdate;
    end;


  // compiled combo box section
    DSS_Registry.Section := 'Compiled';
    nCompiled := DSS_Registry.ReadInteger('Count', 0);
    for i := 0 to nCompiled - 1 do
    begin
        TextLine := DSS_Registry.ReadString(Format('Item%d', [i]), '');
        if FileExists(TextLine) then
        begin
            CompileCombo.Items.Add(TextLine);
        end;
    end;
    if Length(LastFileCompiled) > 0 then
        AddCompiledFile(LastFileCompiled); // Make this first or selected

//  ActiveScriptForm := FormOnTop as TScriptEdit;

  // Check for a file (ONE ONLY) name on the cmdline.
  // If a file is found, make a new script window, make active, select all and execute
    i := 1;
    WindowExistsAlready := FALSE;
    CmdLineFileFound := FALSE;
    repeat
        CmdLineFileName := ParamStr(i);
        if Length(CmdLineFileName) > 0 then
            if (CmdLineFileName[1] <> '/') and (CmdLineFileName[1] <> '-') then   // not a switch
                if FileExists(CmdLineFileName) then
                begin
       // Check if it already exists
                    for j := 0 to ScriptWindowList.Count - 1 do
                    begin
                        TestForm := TScriptEdit(ScriptWindowList.Items[j]);
                        if CompareText(TestForm.Caption, CmdLineFileName) = 0 then
                        begin
                            ActiveScriptForm := TestForm;
                            WindowExistsAlready := TRUE;
                            CmdLineFileFound := TRUE;
                            Break;
                        end;
                    end;
                    if not WindowExistsAlready then
                    begin   // Make a new one
                        ActiveScriptForm := MakeANewEditForm(CmdLineFileName);  // Load from file
                        ActiveScriptForm.Editor.Lines.LoadFromFile(CmdLineFileName);
                        ActiveScriptForm.HasBeenModified := FALSE;
                        ActiveScriptForm.HasFileName := TRUE;
                        CmdLineFileFound := TRUE;
                    end;
                    Break;
                end;
        i := i + 1;
    until Length(CmdLineFileName) = 0;

    MainEditForm.HasBeenModified := FALSE; // so it doesn't show yellow
    ActiveScriptForm.UpdateSummaryForm;

// If a command line file name give, attempt to execute the script
    if CmdLineFileFound then
    begin
        ActiveScriptForm.Editor.SelectAll;
        ToolButton2Click(NIL);   // Execute all the commands in the window
    end;


  {Tile;}
    UpdateStatus;
    Recordcommands := FALSE;
    MakeBaseClassBox;
    UpdateClassBox;
    Edit_Result.Text := VersionString;

end;

procedure TControlPanel.NewScriptWindow1Click(Sender: TObject);
begin
    Inc(EditFormCount);
    ActiveScriptForm := MakeANewEditForm('Script Window ' + InttoStr(EditFormCount));
    UpdateCaptions;
end;

procedure TControlPanel.UpdateStatus;
var
    pmc: PPROCESS_MEMORY_COUNTERS;
    cb: Integer;
begin
    cb := sizeof(_PROCESS_MEMORY_COUNTERS);
    GetMem(pmc, cb);
    pmc^.cb := cb;
    if GetProcessMemoryInfo(GetCurrentProcess(), pmc, cb) then
        StatusBar1.Panels[0].Text := Format('Memory: %dK', [pmc^.WorkingSetSize div 1024])
    else
        StatusBar1.Panels[0].Text := 'Memory: ?';
    FreeMem(pmc);
//     StatusBar1.Panels[1].Text := Format('Blocks: %d',[AllocMemCount]);
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit do
        begin
            if IsSolved then
                StatusBar1.Panels[1].Text := 'Circuit Status: SOLVED'
            else
                StatusBar1.Panels[1].Text := 'Circuit Status: NOT SOLVED';
            StatusBar1.Panels[2].Text := Format('Total Iterations = %d, Control Iterations = %d,  Max Solution Iterations = %d', [solution.iteration, Solution.ControlIteration, Solution.MostIterationsDone]);
        end;
    end
    else
    begin
        StatusBar1.Panels[1].Text := 'No Active Circuit';
        StatusBar1.Panels[2].Text := ' ';
    end;

    DemandInterval1.Checked := EnergyMeterclass.SaveDemandInterval;
    Caption := ProgramName + ' Data Directory: ' + DataDirectory; // NOTE: not necessarily same as output directory
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            ZonesLocked1.Checked := ZonesLocked;
            DuplicatesAllowed1.checked := DuplicatesAllowed;
            TraceLog1.Checked := ControlQueue.TraceLog;
            Trapezoidal1.checked := TrapezoidalIntegration;
        end;
    LBL_DefaultFreq.Caption := Format(' Base Frequency = %d Hz', [Round(DefaultBaseFreq)]);
    UpdateCaptions;
end;

procedure TControlPanel.RecordScript1Click(Sender: TObject);
begin
    RecordScript1.Checked := not RecordScript1.Checked;
    RecordCommands := RecordScript1.Checked;
end;

procedure TControlPanel.Zone1Click(Sender: TObject);
begin
    if activeCircuit = NIL then
        Exit;

    if compareText(classbox.text, 'energymeter') = 0 then
    begin
        Screen.Cursor := crHourglass;
        ActiveScriptForm.ExecuteDSSCommand('Show zone ' + Elementbox.Text);
        Screen.Cursor := crDefault;
    end
    else
        DoSimpleMsg('Select "energymeter" element before executing this command.', 210);

end;

procedure TControlPanel.UpdateClassBox;
begin
    case BaseClassCombo.ItemIndex of

        0:
            PopulateClassList(NON_PCPD_ELEM);
        1:
            PopulateClassList(PD_ELEMENT);
        2:
            PopulateClassList(PC_ELEMENT);
        3:
            PopulateClassList(CTRL_ELEMENT);
        4:
            PopulateClassList(METER_ELEMENT);
        5:
            PopulateClassList(0);

    end;

    ClassBox.ItemIndex := 0;
    UpdateElementBox;
end;

procedure TControlPanel.UpdateElementBox;
var
    idx: Integer;
begin
    ElementBox.Clear;
    if ActiveCircuit <> NIL then
    begin

        if SetObjectClass(Classbox.Items[Classbox.ItemIndex]) then
        begin
            ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
            idx := ActiveDSSClass.First;
            while idx > 0 do
            begin
                ElementBox.Items.Add(UpperCase(ActiveDSSObject.Name));
                idx := ActiveDSSClass.Next;
            end;
            ElementBox.Sorted := TRUE;
            ElementBox.ItemIndex := 0;
        end;
        Activecircuit.SetElementActive(Classbox.text + '.' + elementbox.text);
    end;
end;


procedure TControlPanel.ClassBoxChange(Sender: TObject);
begin
    UpdateElementBox;
end;

procedure TControlPanel.Zone2Click(Sender: TObject);
var
    S: String;
begin
    if activeCircuit = NIL then
        Exit;

    if compareText(classbox.text, 'energymeter') = 0 then
    begin
        with PlotOptionsForm do
        begin
            S := 'plot zone ' + QtyCombo.text + PlotOptionString;
            if LoopCheck.Checked then
                S := S + ' showloops=y';
            S := S + ' object=(' + Elementbox.Text + ') C1=' + Editcolor1.Text;
        end;
        ActiveScriptForm.ExecuteDSSCommand(S);
    end
    else
        DoSimpleMsg('Select "energymeter" element before executing this command.', 211);

end;

procedure TControlPanel.Options1Click(Sender: TObject);
begin
    with PlotOptionsForm do
    begin
        ShowModal;
        PlotOptionString := ' Max=' + EdtPlotMax.text;
        if dotscheck.checked then
            PlotOptionString := PlotOptionString + ' dots=y'
        else
            PlotOptionString := PlotOptionString + ' dots=n';
        if labelscheck.checked then
            PlotOptionString := PlotOptionString + ' labels=y'
        else
            PlotOptionString := PlotOptionString + ' labels=n';
        if Subcheckbox.checked then
            PlotOptionString := PlotOptionString + ' subs=y'
        else
            PlotOptionString := PlotOptionString + ' subs=n';
    end;
end;


procedure TControlPanel.Daisy1Click(Sender: TObject);
begin
    if activeCircuit <> NIL then

        with PlotOptionsForm do
            ActiveScriptForm.ExecuteDSSCommand('plot daisy ' + QtyCombo.text + PlotOptionString + ' C1=' + EditColor1.Text);

end;

procedure TControlPanel.All1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Reset');
end;

procedure TControlPanel.Monitors1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Reset Monitors');
end;

procedure TControlPanel.EnergyMeters1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Reset meters');
end;

procedure TControlPanel.EnergyMeters2Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export meters');
end;

procedure TControlPanel.erminal1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Powers');
end;

procedure TControlPanel.Zones1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Reset faults');
end;

procedure TControlPanel.Controls1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Reset controls');
end;

procedure TControlPanel.EventLog2Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Reset eventlog');
end;

procedure TControlPanel.KeepList1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Reset keeplist');
end;

procedure TControlPanel.kVBaseMismatch1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('show kvbasemismatch');
end;

procedure TControlPanel.Monitor1Click(Sender: TObject);
begin
    if activeCircuit = NIL then
        Exit;

    if compareText(classbox.text, 'monitor') = 0 then
    begin
        ActiveScriptForm.ExecuteDSSCommand('Show monitor ' + Elementbox.Text);
    end
    else
        DoSimpleMsg('Select "monitor" element before executing this command.', 212);

end;

procedure TControlPanel.AutoAdded1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show autoadded');
end;


procedure TControlPanel.Overloads1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export overloads');
end;

procedure TControlPanel.Variables1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show variables');

end;

procedure TControlPanel.Faults1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show faults');

end;

procedure TControlPanel.Convergence1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show convergence');

end;

procedure TControlPanel.Option1Click(Sender: TObject);
var
    i: Integer;
begin
    with TOptionComboForm.Create(NIL) do
    begin
        Caption := 'Select an Option and Enter a Value';
        Combobox1.Clear;
        for i := 1 to NumExecOptions do
            ComboBox1.Items.Add(ExecOption[i]);

        Combobox1.sorted := TRUE;
        ComboBox1.ItemIndex := 0;
        ComboLabel.Caption := 'Option';
        ValueLabel.Caption := 'Value';
        ValueLabel.visible := TRUE;
        Edit1.visible := TRUE;

        ShowModal;

        if OKPressed then
        begin
            with ActiveScriptForm do
            begin
                ExecuteDSSCommand('set ' + combobox1.text + '=' + Edit1.text);
            end;
        end;

        Free;
    end;
end;

procedure TControlPanel.Mode1Click(Sender: TObject);
var
    i: Integer;
begin
    with TOptionComboForm.Create(NIL) do
    begin
        Caption := 'Select Solution Mode';
        Combobox1.Clear;
        for i := 0 to NumSolutionModes - 1 do
            ComboBox1.Items.Add(GetSolutionModeIDName(i));

        Combobox1.Sorted := FALSE;
        ComboBox1.ItemIndex := 0;

        ShowModal;

        if OKPressed then
        begin
            ActiveScriptForm.ExecuteDSSCommand('set mode=' + combobox1.text);
        end;

        Free;
    end;
end;

procedure TControlPanel.LoadMultiplier1Click(Sender: TObject);
begin
    with TValueEntryForm.Create(NIL) do
    begin
        Caption := 'Enter Load multiplier';
        Edit1.text := Format('%-.6g', [ActiveCircuit.loadmultiplier]);
        Showmodal;
        if OKPressed then
        begin
            ActiveScriptForm.ExecuteDSSCommand('set Loadmult=' + Edit1.text);
        end;
        Free;
    end;
end;

procedure TControlPanel.AllocationFactors1Click(Sender: TObject);
begin
    with TValueEntryForm.Create(NIL) do
    begin
        Caption := 'Enter Allocation Factor for ALL Loads';
        Edit1.text := '1';
        Showmodal;
        if OKPressed then
        begin
            ActiveScriptForm.ExecuteDSSCommand('set Allocationfactors=' + Edit1.text);
        end;
        Free;
    end;

end;

procedure TControlPanel.Taps1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('Show Taps ');
end;

procedure TControlPanel.EditActive1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('FormEdit "' + classbox.text + '.' + elementbox.text + '"');
end;

procedure TControlPanel.SelectActive1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('select ' + classbox.text + '.' + elementbox.text);
end;

procedure TControlPanel.Element1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export ElemVoltages');
end;

procedure TControlPanel.Element2Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Elemcurrents');
end;

procedure TControlPanel.Element3Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Elempowers');
end;

procedure TControlPanel.ElementBoxChange(Sender: TObject);
begin
    if activeCircuit = NIL then
        exit;  // do nothing

    Activecircuit.SetElementActive(Classbox.text + '.' + elementbox.text);
end;

procedure TControlPanel.Number1Click(Sender: TObject);
begin
    with TValueEntryForm.Create(NIL) do
    begin
        Caption := 'Enter Number of Solutions';
        Edit1.text := '1';
        Showmodal;
        if OKPressed then
        begin
            ActiveScriptForm.ExecuteDSSCommand('set number=' + Edit1.text);
        end;
        Free;
    end;
end;

procedure TControlPanel.Growth1Click(Sender: TObject);
begin
    with TValueEntryForm.Create(NIL) do
    begin
        Caption := 'Enter default % growth factor';
        Edit1.text := Format('%-g', [(ActiveCircuit.DefaultGrowthRate - 1.0) * 100.0]);
        Showmodal;
        if OKPressed then
        begin
            ActiveScriptForm.ExecuteDSSCommand('set %growth=' + Edit1.text);
        end;
        Free;
    end;
end;

procedure TControlPanel.Year1Click(Sender: TObject);
begin
    with TValueEntryForm.Create(NIL) do
    begin
        Caption := 'Enter Year for next solution [1, 2, 3, ...]';
        Edit1.text := '1';
        Showmodal;
        if OKPressed then
        begin
            ActiveScriptForm.ExecuteDSSCommand('set Year=' + Edit1.text);
        end;
        Free;
    end;

end;

procedure TControlPanel.YMatrix1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Y');
end;

procedure TControlPanel.Yprims1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Yprims');
end;

procedure TControlPanel.Bus1Click(Sender: TObject);
var
    i: Integer;
begin
    if ActiveCircuit = NIL then
        Exit;

    with TOptionComboForm.Create(NIL) do
    begin
        Caption := 'Select Active Bus';
        ComBoLabel.caption := 'Buses:';
        Combobox1.Clear;
        Combobox1.Items.BeginUpdate;
        for i := 1 to ActiveCircuit.NumBuses do
            ComboBox1.Items.Add(ActiveCircuit.BusList.Get(i));
        ComboBox1.Items.EndUpdate;

        Combobox1.Sorted := TRUE;
        ComboBox1.ItemIndex := 0;

        ShowModal;

        if OKPressed then
        begin
            ActiveScriptForm.ExecuteDSSCommand('set Bus=' + combobox1.text);
            GlobalResult := 'Active Bus = "' + Combobox1.text + '"';
            ActiveScriptForm.UpdateResultForm;
        end;

        Free;
    end;
end;

procedure TControlPanel.Buscoords1Click(Sender: TObject);
begin

{Export Bus Coordinates}
    ActiveScriptForm.ExecuteDSSCommand('Export buscoords');
end;

procedure TControlPanel.MakeBaseClassBox;
begin

    BaseClassCombo.Clear;

    with BaseClassCombo.Items do
    begin
        Add('Source/Fault');
        Add('PDelements');
        Add('PCelements');
        Add('Controls');
        Add('Meters');
        Add('General');
    end;
    BaseClassCombo.ItemIndex := 0;
    UpdateClassBox;

end;

procedure TControlPanel.MakeBusList1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('MakebusList')
    else
        DoSimpleMsg('Define a Circuit first', 213);
end;

procedure TControlPanel.LoadModel1Click(Sender: TObject);
begin
    if ActiveCircuit = NIL then
        Exit;

    with TOptionComboForm.Create(NIL) do
    begin
        Caption := 'Select Load Model';
        Combolabel.caption := 'Choices:';
        Combobox1.Clear;

        ComboBox1.Items.Add('PowerFlow');
        ComboBox1.Items.Add('Admittance');

        Combobox1.Sorted := FALSE;
        ComboBox1.ItemIndex := 0;

        ShowModal;

        if OKPressed then
        begin
            ActiveScriptForm.ExecuteDSSCommand('set Loadmodel=' + combobox1.text);
            GlobalResult := 'Load model = "' + Combobox1.text + '"';
            ActiveScriptForm.UpdateResultForm;
        end;

        Free;
    end;
end;

procedure TControlPanel.LDCurve1Click(Sender: TObject);
var
    i: Integer;
    DSScls: TDSSClass;

begin
    if ActiveCircuit = NIL then
        Exit;

    with TOptionComboForm.Create(NIL) do
    begin
        Caption := 'Select Load-Duration Curve';
        ComboLabel.caption := 'Curves:';
        Combobox1.Clear;
        DSSCls := GetDSSClassPtr('loadshape');
        Combobox1.Items.BeginUpdate;
        i := DssCls.First;
        while i > 0 do
        begin
            ComboBox1.Items.Add(ActiveDSSObject.Name);
            i := DSSCls.Next;
        end;
        ComboBox1.Items.EndUpdate;

        ComboBox1.Sorted := TRUE;
        ComboBox1.ItemIndex := 0;

        ShowModal;

        if OKPressed then
        begin
            ActiveScriptForm.ExecuteDSSCommand('set LDCurve="' + combobox1.text + '"');
            GlobalResult := 'LD Curve = "' + Combobox1.text + '"';
            ActiveScriptForm.UpdateResultForm;
        end;

        Free;
    end;
end;

procedure TControlPanel.DefaultDaily1Click(Sender: TObject);
var
    i: Integer;
    DSScls: TDSSClass;

begin
    if ActiveCircuit = NIL then
        Exit;

    with TOptionComboForm.Create(NIL) do
    begin
        Caption := 'Select Load-Duration Curve';
        ComboLabel.caption := 'Curves:';
        Combobox1.Clear;
        DSSCls := GetDSSClassPtr('loadshape');
        Combobox1.Items.BeginUpdate;
        i := DssCls.First;
        while i > 0 do
        begin
            ComboBox1.Items.Add(ActiveDSSObject.Name);
            i := DSSCls.Next;
        end;
        ComboBox1.Items.EndUpdate;

        ComboBox1.Sorted := TRUE;
        ComboBox1.ItemIndex := 0;

        ShowModal;

        if OKPressed then
        begin
            ActiveScriptForm.ExecuteDSSCommand('set DefaultDaily="' + combobox1.text + '"');
            GlobalResult := 'Default Daily Load Shape = "' + Combobox1.text + '"';
            ActiveScriptForm.UpdateResultForm;
        end;

        Free;
    end;


end;

procedure TControlPanel.Editor1Click(Sender: TObject);
begin
    with OpenDialog1 do
    begin

        Title := 'Select Editor Exe File:';
        Filter := 'EXE Files|*.exe';
        FileName := DefaultEditor;

        if Execute then
            ActiveScriptForm.ExecuteDSSCommand('set Editor=(' + FileName + ')');

    end;
end;

procedure TControlPanel.UpdateCaptions;
begin
    MessageEdit.Clear;
    if ActiveScriptForm.HasFileName then
    begin
    // Caption := ProgramName + ' - ' + ActiveScriptForm.Caption;
        MessageEdit.Lines.Add(ProgramName + ' - ' + ActiveScriptForm.Caption);
        ActiveScriptForm.Tab.Caption := ExtractFileName(ActiveScriptForm.Caption);
    end
    else
    begin
        MessageEdit.Lines.Add(ActiveScriptForm.Caption);
    end;
  {Refresh Form Caption}
    Caption := ProgramName + ' Data Directory: ' + DataDirectory;
end;

procedure TControlPanel.EditPagesChange(Sender: TObject);
begin
    ActiveScriptForm := TScriptEdit(EditPages.ActivePage.Tag);
    UpdateCaptions;
end;

procedure TControlPanel.Datapath1Click(Sender: TObject);
begin
    with OpenDialog1 do
    begin
        Title := 'Select any File in the Desired directory:';
        Filter := 'any File|*.*';
        FileName := DataDirectory + '*.*';
        if Execute then
            ActiveScriptForm.ExecuteDSSCommand('set Datapath=(' + ExtractFilePath(FileName) + ')');

    end;
end;

procedure TControlPanel.Save1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('Save Circuit')
end;

procedure TControlPanel.ElementsinClass1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('Show Elements ' + Classbox.Text)
end;


procedure TControlPanel.AlignFile1Click(Sender: TObject);
begin
    with OpenDialog1 do
    begin
        Filename := '';
        DefaultExt := 'dss';
        Filter := 'DSS Files (*.dss)|*.dss|Text files (*.txt)|*.txt|CSV files (*.csv)|*.csv|All files (*.*)|*.*';
        Title := 'Select File to be Column-aligned';
        if Execute then
        begin
            Screen.Cursor := crHourglass;
      // Enclose string in DSS quotes to handle blanks in name
            ActiveScriptForm.ExecuteDSSCommand('Alignfile (' + Filename + ')');
            Screen.Cursor := crDefault;
        end;
    end;

end;

procedure TControlPanel.BusList1Click(Sender: TObject);
begin
    MakeBusList1Click(Sender);
end;

procedure TControlPanel.ByOhase1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export p_byphase');
end;

procedure TControlPanel.PosSeqEquiv1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('makeposseq');

end;

procedure TControlPanel.NewYMatrix1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('buildy');
end;

procedure TControlPanel.NodeList1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export YNodeList');
end;

procedure TControlPanel.NodeNames1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export NodeNames');
end;

procedure TControlPanel.NodeOrder1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export NodeOrder');
end;

procedure TControlPanel.Selection1Click(Sender: TObject);
begin
    Screen.Cursor := crHourglass;
    with ActiveScriptForm do
        if BuildCommandList then
            ExecuteCommandList;
    Screen.Cursor := crDefault;
end;

procedure TControlPanel.CalcVoltageBases1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('calcvoltagebases');
end;

procedure TControlPanel.Capacity1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Capacity');
end;

procedure TControlPanel.TakeSample1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('sample');
end;

procedure TControlPanel.Initialize1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('init');
end;

procedure TControlPanel.RebuildYMatrix1Click(Sender: TObject);
begin
    NewYMatrix1Click(Sender);
end;

procedure TControlPanel.Interpolate2Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('interpolate');
end;


procedure TControlPanel.AllocateLoads1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('allocate');
end;

procedure TControlPanel.Default1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveScriptForm.ExecuteDSSCommand('Set reduceoption=default');
        ActiveScriptForm.ExecuteDSSCommand('reduce');
    end;
end;

procedure TControlPanel.Storage1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Storage_Meters');
end;

procedure TControlPanel.Stubs1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveScriptForm.ExecuteDSSCommand('Set reduceoption=Stubs');
        ActiveScriptForm.ExecuteDSSCommand('reduce');
    end;

end;

procedure TControlPanel.TapsandEnds1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveScriptForm.ExecuteDSSCommand('Set reduceoption=Tapends');
        ActiveScriptForm.ExecuteDSSCommand('reduce');
    end;
end;

procedure TControlPanel.TCCCurve1Click(Sender: TObject);
begin
    DoSimpleMsg('This function currently inactive.', 999123);
end;

procedure TControlPanel.BaseClassComboChange(Sender: TObject);
begin

    UpdateClassBox;

end;

procedure TControlPanel.BaseFrequcney501Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Set DefaultBaseFrequency=50');
end;

procedure TControlPanel.BaseFrequency1Click(Sender: TObject);
begin

    ActiveScriptForm.ExecuteDSSCommand('Set DefaultBaseFrequency=60');

end;

procedure TControlPanel.BreakLoops1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveScriptForm.ExecuteDSSCommand('Set reduceoption=Break');
        ActiveScriptForm.ExecuteDSSCommand('reduce');
    end;

end;

procedure TControlPanel.MergeParallel1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
    begin
        ActiveScriptForm.ExecuteDSSCommand('Set reduceoption=Mergeparallel');
        ActiveScriptForm.ExecuteDSSCommand('reduce');
    end;

end;

procedure TControlPanel.ZonemeterTreeview1Click(Sender: TObject);
begin
    if activeCircuit = NIL then
        Exit;

    if compareText(classbox.text, 'energymeter') = 0 then
    begin
        Screen.Cursor := crHourglass;
        ActiveScriptForm.ExecuteDSSCommand('Show zone ' + Elementbox.Text + ' Treeview');
        Screen.Cursor := crDefault;
    end
    else
        DoSimpleMsg('Select "energymeter" element before executing this command.', 214);

end;

procedure TControlPanel.Zone3Click(Sender: TObject);
var
    S: String;
begin
    if activeCircuit = NIL then
        Exit;
    with PlotOptionsForm do
    begin
        S := 'plot zone ' + QtyCombo.Text + PlotOptionString;
        if Loopcheck.Checked then
            S := S + ' ShowLoops=y';
    end;
    ActiveScriptForm.ExecuteDSSCommand(S);
end;

procedure TControlPanel.Command1Click(Sender: TObject);
// Var i:Integer;
begin

(*
        With TOptionComboForm.Create(nil) do
        Begin
            Caption:='Select a command and Enter a parameter string';
            Combobox1.Clear;
            For i := 1 to NumExecCommands Do
              ComboBox1.Items.Add(ExecCommand[i]);

            Combobox1.sorted := True;
            ComboBox1.ItemIndex := 0;

            Combolabel.Caption := 'Command';

            ValueLabel.visible := True;
            ValueLabel.Caption := 'Parameter';
            Edit1.visible := True;

            ShowModal;

            If OKPressed Then
            Begin
             With ActiveScriptForm Do Begin
               ExecuteDSSCommand(combobox1.text+' '+Edit1.text);
             End;
            End;

            Free;
        End;
 *)

    if DoDSSCommandForm.ShowModal = mrOK then
    begin
        with ActiveScriptForm do
        begin
            ExecuteDSSCommand(DoDSSCommandForm.sCommand);
        end;
    end;
end;

procedure TControlPanel.DemandInterval1Click(Sender: TObject);
begin
    DemandInterval1.Checked := not DemandInterval1.checked;
    EnergyMeterclass.SaveDemandInterval := DemandInterval1.Checked;
end;

procedure TControlPanel.ZonesLocked1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            ZonesLocked1.Checked := not ZonesLocked1.Checked;
            ZonesLocked := ZonesLocked1.Checked;
        end;
end;

procedure TControlPanel.DuplicatesAllowed1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            DuplicatesAllowed1.Checked := not DuplicatesAllowed1.Checked;
            DuplicatesAllowed := DuplicatesAllowed1.Checked;
        end;
end;

procedure TControlPanel.TechNotes1Click(Sender: TObject);
begin
    shellexecute(handle, 'open', 'http://sourceforge.net/apps/mediawiki/electricdss/index.php?title=List_of_DSS_tech_notes', NIL, NIL, 1);
end;

procedure TControlPanel.TraceLog1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            TraceLog1.Checked := not TraceLog1.Checked;
            ControlQueue.TraceLog := TraceLog1.Checked;
        end;

end;

procedure TControlPanel.Trapezoidal1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            Trapezoidal1.Checked := not Trapezoidal1.Checked;
            TrapezoidalIntegration := Trapezoidal1.Checked;
        end;
end;

procedure TControlPanel.SaveScriptWindow1Click(Sender: TObject);

begin
    if ActiveScriptForm.isMainWindow then
        DoSimpleMsg('Cannot save the Main Window.' + CRLF + 'Make a new script window, copy contents, and then save.', 215)
    else
        with SaveDialog1 do
        begin
            DefaultExt := 'dss';
            Filter := 'DSS files (*.dss)|*.dss|Text files (*.txt)|*.TXT|All files (*.*)|*.*';
            FileName := ActiveScriptForm.caption;
            Title := 'Save Active Script Window to File';
            Options := [ofOverwritePrompt];
            if Execute then
            begin
                ActiveScriptForm.Caption := FileName;
                ActiveScriptForm.SaveEditorContents;
            end; {Execute}
        end;  {WITH}

end;

procedure TControlPanel.GeneralBusData1Click(Sender: TObject);

var
    NameofFile: String;
    F: TextFile;
    Line: String;
    FieldIndex: Integer;
    SaveDelims: String;
    FieldName: String;
    MaxScale: String;

begin

    with OpenDialog1 do
    begin
        FileName := '';
        Filter := 'CSV Files (*.csv)|*.csv|TXT Files (*.txt)|*.txt|All Files *.*|*.*';
        Title := 'Select File with Bus names and data';
        if Execute then
            with PlotOptionsForm do
            begin
                NameOfFile := FileName;
            end
        else
            Exit;
    end;

    try
        AssignFile(F, NameOfFile);
        Reset(F);
        Readln(F, Line);  // Read First Line   for field names
        CloseFile(F);
    except
        On E: Exception do
            DoSimpleMsg('Error with General Bus Data File:' + E.Message, 5444);
    end;

    SaveDelims := AuxParser.Delimiters;
    AuxParser.Delimiters := ',=' + #9;
    AuxParser.CmdString := Line;
    AuxParser.AutoIncrement := FALSE;
    AuxParser.NextParam;
    AuxParser.NextParam;

    with TListBoxForm.Create(NIL) do
    begin
        Caption := 'Field to Plot';
        FieldName := AuxParser.StrValue;
        while Length(FieldName) > 0 do
        begin
            ComboBox1.Items.Add(FieldName);
            AuxParser.NextParam;
            FieldName := AuxParser.StrValue;
        end;

        ComboBox1.ItemIndex := 0;
        ShowModal;

        if CancelPressed then
            FieldIndex := 0  // Do nothing
        else
            FieldIndex := ComboBox1.ItemIndex + 1;
        Free;
    end;
    AuxParser.Delimiters := SaveDelims;

    MaxScale := ' 0 ';
    with TValueEntryForm.Create(NIL) do
    begin
        Caption := 'Enter Max Scale Value';
        Edit1.text := MaxScale;
        Showmodal;
        if OKPressed then
        begin
            MaxScale := Edit1.text;
        end;
        Free;
    end;

    MaxScale := ' max=' + MaxScale;

    if FieldIndex > 0 then
        with PlotOptionsForm do
            ActiveScriptForm.ExecuteDSSCommand('plot General ' + IntToStr(FieldIndex) + MaxScale + ' dots=n labels=n object=(' + NameOfFile + ') C1=' + EditColor1.text + ' C2=' + EditColor2.Text);


end;

procedure TControlPanel.AutoaddLog1Click(Sender: TObject);
begin
    if assigned(ActiveCircuit) then
        with PlotOptionsForm do
            ActiveScriptForm.ExecuteDSSCommand('plot Auto ' + EditAutoIndex.Text + PlotOptionString +
                ' max=0 ' + {override plot option string}
                ' C1=' + InttoStr(AutoColor1) +
                ' C2=' + InttoStr(AutoColor2) +
                ' C3=' + InttoStr(AutoColor3) +
                ' R3=' + Format('%.g', [PlotOptionsForm.RangeMax]) +
                ' R2=' + Format('%.g', [PlotOptionsForm.RangeMid])
                );
end;

procedure TControlPanel.SaveAllMonitors1Click(Sender: TObject);
begin
    MonitorClass.SaveAll;
end;

procedure TControlPanel.CircuitPlot1Click(Sender: TObject);
begin
    if activeCircuit <> NIL then
        with PlotOptionsForm do
            ActiveScriptForm.ExecuteDSSCommand('plot circuit ' + QtyCombo.text + PlotOptionString + ' C1=' + EditColor1.Text);
end;

procedure TControlPanel.Loadshape1Click(Sender: TObject);
begin
    if activeCircuit = NIL then
        Exit;

     {  If Not Assigned(DSSPlotObj) Then DSSPlotObj := TDSSPlot.Create;
       DSSPlotObj.SetDefaults;  }

    if compareText(classbox.text, 'loadshape') = 0 then
    begin
         {Screen.Cursor := crHourglass;
         DSSPlotObj.DoLoadShapePlot(elementbox.Text );
         Screen.Cursor := crDefault;  }
        ActiveScriptForm.ExecuteDSSCommand(Format('plot Loadshape Object=%s', [elementbox.text]));
    end
    else
        DoSimpleMsg('Select "loadshape" element before executing this command.', 216);

end;

procedure TControlPanel.Ratings1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('show ratings');
end;

procedure TControlPanel.Losses1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('show losses');
end;

procedure TControlPanel.Losses2Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('export losses');
end;

procedure TControlPanel.Summary1Click(Sender: TObject);
begin
    if ActiveCircuit <> NIL then
        ActiveScriptForm.UpdateSummaryForm;
    ResultPages.ActivePage := SummaryTab;
end;

procedure TControlPanel.Summary2Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Summary');
end;

procedure TControlPanel.List1Click(Sender: TObject);

var
    i: Integer;

begin
    with TOptionComboForm.Create(NIL) do
    begin
        Caption := 'Select a Show command and optional parameter(s)';
        Combobox1.Clear;
        for i := 1 to ShowCommands.NumCommands do
            ComboBox1.Items.Add(Showcommands.Get(i));

        Combobox1.sorted := TRUE;
        ComboBox1.ItemIndex := 0;

        Combolabel.Caption := 'Command';

        ValueLabel.visible := TRUE;
        ValueLabel.Caption := 'Parameter';
        Edit1.visible := TRUE;

        ShowModal;

        if OKPressed then
        begin
            with ActiveScriptForm do
            begin
                ExecuteDSSCommand('Show ' + combobox1.text + ' ' + Edit1.text);
            end;
        end;

        Free;
    end;

end;

procedure TControlPanel.Monitor2Click(Sender: TObject);
begin

    if activeCircuit = NIL then
        Exit;

    Monitors2Click(Sender); // Export monitor  to CSV file

       {Open Result File and Parse first line}
//       if FileExists(ResultsEdit.Lines.Strings[0]) then  Begin
    if FileExists(Edit_Result.text) then
    begin

//         if MakeChannelSelection(2, ResultsEdit.Lines.Strings[0]) Then
        if MakeChannelSelection(2, Edit_Result.text) then
        begin
            Screen.Cursor := crHourglass;
            ActiveScriptForm.ExecuteDSSCommand('Plot monitor object= ' + SelectedMonitor + ' channels=(' + ChannelSelectForm.ResultString + ')');
            Screen.Cursor := crDefault;
        end;
    end;

end;

procedure TControlPanel.Loops1Click(Sender: TObject);
begin
    Screen.Cursor := crHourglass;
    if ActiveCircuit <> NIL then
        ActiveScriptForm.ExecuteDSSCommand('show loops');
    Screen.Cursor := crDefault;
end;

procedure TControlPanel.VoltagesLNNodes1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show Voltage LN Nodes');
end;

procedure TControlPanel.VoltArray1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Yvoltages');
end;

procedure TControlPanel.VoltagesLNElements1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show Voltage LN Elements');
end;

procedure TControlPanel.VoltageLLNodes1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show Voltage LL Nodes');
end;

procedure TControlPanel.PowerkVAElem1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show Powers kva Elements');
end;

procedure TControlPanel.PowersMVAElem1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show Powers MVA Elements');
end;

procedure TControlPanel.PVSystems1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export PVSystem_Meters');
end;

procedure TControlPanel.CurrentsElem1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show Currents Elements');
end;

procedure TControlPanel.Open1Click(Sender: TObject);
var
    CurrDir: String;
begin
    with OpenDialog1 do
    begin
        Filename := '';
        DefaultExt := 'dss';
        Filter := 'DSS files (*.dss)|*.dss|Text files (*.txt)|*.TXT|All files (*.*)|*.*';
        Title := 'Open DSS Script File';
        if Execute then
        begin
            try
                ActiveScriptForm := MakeANewEditForm(FileName);
                ActiveScriptForm.Editor.Lines.LoadFromFile(FileName);
                ActiveScriptForm.HasBeenModified := FALSE;
                ActiveScriptForm.HasFileName := TRUE;
                AddCompiledFile(FileName);  // Stick it in combobox
                CurrDir := ExtractFileDir(FileName);
                SetCurrentDir(CurrDir);
                SetDataPath(CurrDir);  // change datadirectory
                UpdateStatus;
            except
                On E: Exception do
                    DoSimpleMsg('Error: ' + E.Message, 218);
            end;
        end; {Execute}
    end;  {WITH}
end;

procedure TControlPanel.OpenDSSWiki1Click(Sender: TObject);
begin
    shellexecute(handle, 'open', 'http://smartgrid.epri.com/SimulationTool.aspx', NIL, NIL, 1);
end;

procedure TControlPanel.Save2Click(Sender: TObject);
begin
    if not ActiveScriptForm.HasFileName then
        SaveScriptWindow1Click(Sender)
    else
        ActiveScriptForm.SaveEditorContents;
end;

procedure TControlPanel.PopUp1DeleteClick(Sender: TObject);
var
    i: Integer;

begin
    i := CompileCombo.ItemIndex;
    if i >= 0 then
        CompileCombo.Items.Delete(i);
end;

procedure TControlPanel.Popup1EditClick(Sender: TObject);
begin
    if CompileCombo.ItemIndex >= 0 then
    begin
        FireoffEditor(CompileCombo.text);
    end;
end;

procedure TControlPanel.Popup1CompileClick(Sender: TObject);
begin

    if CompileCombo.ItemIndex >= 0 then
    begin
        Screen.Cursor := crHourglass;
        LastFileCompiled := CompileCombo.text;
        // Enclose string in DSS quotes to handle blanks in name
        ActiveScriptForm.ExecuteDSSCommand('Compile (' + LastFileCompiled + ')');
        Screen.Cursor := crDefault;
    end;

end;

procedure TControlPanel.P1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Plot Profile Phases=All');
end;

procedure TControlPanel.Phase1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Voltages');
end;

procedure TControlPanel.Phase2Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Currents');

end;

procedure TControlPanel.PopulateClassList(BaseClass: Word);
var
    pDSSClass: TDSSClass;
begin

    ClassBox.Items.clear;
    ClassBox.Sorted := FALSE;

    with ClassBox do
    begin

 // put the  DSS Classes in alphabetical order within Base Class
        pDSSClass := DSSClassList.First;
        while pDSSClass <> NIL do
        begin
            if (pDSSClass.DSSClassType and BASECLASSMASK) = BaseClass then
                Items.Add(pDSSClass.Name);
            pDSSClass := DSSClassList.Next;
        end;

        if BaseClass <> NON_PCPD_ELEM then
            ClassBox.Sorted := TRUE;

    end

end;

procedure TControlPanel.PopUp1ChDirClick(Sender: TObject);
begin
    if CompileCombo.ItemIndex >= 0 then
    begin
        ActiveScriptForm.ExecuteDSSCommand('CD (' + ExtractFilePath(CompileCombo.text) + ')');
    end;
end;

procedure TControlPanel.Popup1ClearListClick(Sender: TObject);
begin
    CompileCombo.Clear;
end;

procedure TControlPanel.AddCompiledFile(const filename: String);
var
    i: Integer;
begin
{Check compilecombo to see if filename is in the list.  If not, add it.  If so, make it active}
    for i := 1 to CompileCombo.Items.Count do
    begin

        if CompareText(FileName, CompileCombo.Items[i - 1]) = 0 then
        begin
            CompileCombo.ItemIndex := i - 1;
            Exit;  // Get outta here.  We're done
        end;

    end;

       {Not found.  Add to list}
    CompileCombo.Items.Insert(0, Filename); // insert at beginning
    CompileCombo.ItemIndex := 0; // position to beginning


end;

procedure TControlPanel.EditFileBtnClick(Sender: TObject);
begin
    Popup1EditClick(Sender);
end;

procedure TControlPanel.CompileBtnClick(Sender: TObject);
begin
    Popup1CompileClick(Sender);
end;


procedure TControlPanel.GeneralLineData1Click(Sender: TObject);
var
    NameofFile: String;
    F: TextFile;
    Line: String;
    FieldIndex: Integer;
    SaveDelims: String;
    FieldName: String;
    MaxScale: String;

begin

    with OpenDialog1 do
    begin
        FileName := '';
        Filter := 'CSV Files (*.csv)|*.csv|TXT Files (*.txt)|*.txt|All Files *.*|*.*';
        Title := 'Select File with Bus names and data';
        if Execute then
            with PlotOptionsForm do
            begin
                NameOfFile := FileName;
            end
        else
            Exit;
    end;

    try
        AssignFile(F, NameOfFile);
        Reset(F);
        Readln(F, Line);  // Read First Line   for field names
        CloseFile(F);
    except
        On E: Exception do
            DoSimpleMsg('Error with General Line Data File:' + E.Message, 5444);
    end;

    SaveDelims := AuxParser.Delimiters;
    AuxParser.Delimiters := ',=' + #9;
    AuxParser.CmdString := Line;
    AuxParser.AutoIncrement := FALSE;
    AuxParser.NextParam;
    AuxParser.NextParam;

    with TListBoxForm.Create(NIL) do
    begin
        Caption := 'Field to Plot';
        FieldName := AuxParser.StrValue;
        while Length(FieldName) > 0 do
        begin
            ComboBox1.Items.Add(FieldName);
            AuxParser.NextParam;
            FieldName := AuxParser.StrValue;
        end;

        ComboBox1.ItemIndex := 0;
        ShowModal;

        if CancelPressed then
            FieldIndex := 0  // Do nothing
        else
            FieldIndex := ComboBox1.ItemIndex + 1;
        Free;
    end;
    AuxParser.Delimiters := SaveDelims;

    MaxScale := ' 1 ';
    with TValueEntryForm.Create(NIL) do
    begin
        Caption := 'Enter Max Scale Value';
        Edit1.text := MaxScale;
        Showmodal;
        if OKPressed then
        begin
            MaxScale := Edit1.text;
        end;
        Free;
    end;

    MaxScale := ' max=' + MaxScale;

    if FieldIndex > 0 then
        with PlotOptionsForm do
            ActiveScriptForm.ExecuteDSSCommand('plot Circuit ' + IntToStr(FieldIndex) + MaxScale + ' dots=n labels=n subs=n object=(' + NameOfFile + ') C1=' + EditColor1.text + ' C2=' + EditColor2.Text);


end;

procedure TControlPanel.Converged1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Show convergence');
end;

procedure TControlPanel.BusFlow1Click(Sender: TObject);

var
    i: Integer;
    BusName: String;
    Optionstring: String;
    Cancelled: Boolean;

begin


{ Pop up Bus List

   Show BusFlow busname [MVA|kVA*] [Seq* | Elements] }
    Cancelled := FALSE;

    with TListBoxForm.Create(NIL) do
    begin
        Caption := 'Pick a bus';

        with ActiveCircuit do
            for i := 1 to Numbuses do
                ComboBox1.Items.Add(BusList.get(i));

        ComboBox1.Sorted := TRUE;
        ComboBox1.ItemIndex := 0;
        ShowModal;

        if CancelPressed then
            Cancelled := TRUE  // Do nothing
        else
            BusName := ComboBox1.Items.Strings[ComboBox1.ItemIndex];
        Free;
    end;


    if not Cancelled then
    begin

        with TOptionComboForm.Create(NIL) do
        begin

            Caption := 'Select Options';
            Combolabel.caption := 'Choices:';
            Combobox1.Clear;

            ComboBox1.Items.Add('kVA Elem');
            ComboBox1.Items.Add('MVA Elem');
            ComboBox1.Items.Add('kVA seq');
            ComboBox1.Items.Add('MVA seq');

            Combobox1.Sorted := FALSE;
            ComboBox1.ItemIndex := 0;

            ShowModal;

            if OKPressed then
                Optionstring := combobox1.text
            else
                Cancelled := TRUE;

            Free;
        end;

        if not Cancelled then
            ActiveScriptForm.ExecuteDSSCommand('Show BusFlow ' + BusName + ' ' + Optionstring);
    end;

end;

procedure TControlPanel.LineConstants1Click(Sender: TObject);

var
    Freq, LenUnits: String;
    UnitsIndex: Integer;
    Cancelled: Boolean;
    i: Integer;
    rho_earth: String;

begin

     { Show LineConstants  Freq   [none|mi|km|kft|m|me|ft|in|cm] }

    Cancelled := FALSE;
    Freq := Format('%d', [Round(DefaultBaseFreq)]);
    UnitsIndex := 0;
    with TValueEntryForm.Create(NIL) do
    begin
        Caption := 'Freq (Hz)';
        Edit1.text := Freq;
        Showmodal;
        if OKPressed then
        begin
            Freq := Edit1.text;
        end
        else
            Cancelled := TRUE;
        Free;
    end;

    Rho_Earth := '100.0';
    if not Cancelled then
        with TValueEntryForm.Create(NIL) do
        begin
            Caption := 'Earth resistivity (ohm-m)';
            Edit1.text := Rho_Earth;
            Showmodal;
            if OKPressed then
            begin
                rho_earth := Edit1.text;
            end
            else
                Cancelled := TRUE;
            Free;
        end;

    if not Cancelled then
        with TListBoxForm.Create(NIL) do
        begin
            Caption := 'Specify Units';

            for i := 0 to UNITS_MAXNUM do
                ComboBox1.Items.Add(LineUnitsStr(i));

            ComboBox1.ItemIndex := 0;
            ShowModal;

            if CancelPressed then
                Cancelled := TRUE  // Do nothing
            else
                UnitsIndex := ComboBox1.ItemIndex;
            LenUnits := LineUnitsStr(UnitsIndex);
            Free;
        end;

    if not Cancelled then
        ActiveScriptForm.ExecuteDSSCommand(Format('Show LineConstants %s %s %s', [Freq, LenUnits, rho_earth]));

end;


procedure TControlPanel.LinkstoHelpFiles1Click(Sender: TObject);
var
    retval: Word;
    FileNm: String;

begin
    FileNm := StartupDirectory + '..\Doc\Help Links.htm';
    try
        if FileExists(FileNm) then
        begin
            retval := ShellExecute(0, NIL, Pchar(encloseQuotes(FileNm)), NIL, NIL, SW_SHOW);
            SetLastResultFile(FileNm);

        end;
    except
        On E: Exception do
            DoErrorMsg('Links to Help Files.', E.Message,
                'Browser not found of help files moved or not installed.', 23704);
    end;
end;

procedure TControlPanel.SeqVoltages1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export seqVoltages');
end;

procedure TControlPanel.SeqZ1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export seqz');
end;

procedure TControlPanel.Sort1Click(Sender: TObject);
begin
    CompileCombo.Sorted := TRUE; // Force Box to sort;
    CompileCombo.Sorted := FALSE; // set back to unsorted (last in first out)
end;

procedure TControlPanel.Sequence1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export seqVoltages');
end;

procedure TControlPanel.Sequence2Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export seqcurrents');
end;

procedure TControlPanel.Sequence3Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export seqpowers');
end;

procedure TControlPanel.PowersByPhase1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export p_byphase');
end;

procedure TControlPanel.FaultCurrents1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export fault');
end;

procedure TControlPanel.Estimation1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Estimation');
end;

procedure TControlPanel.Unserved1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export Unserved');
end;

procedure TControlPanel.Generators2Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export generators');
end;

procedure TControlPanel.Generators3Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export generators');
end;

procedure TControlPanel.Loads1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('Export loads');
end;

procedure TControlPanel.Mismatch1Click(Sender: TObject);
begin
    ActiveScriptForm.ExecuteDSSCommand('show mismatch');
end;

procedure TControlPanel.Monitors2Click(Sender: TObject);

var
    pmon: TMonitorObj;
    Cancelled: Boolean;
begin
    Cancelled := FALSE;
    SelectedMonitor := '';
    with TListBoxForm.Create(NIL) do
    begin
        Caption := 'Select Monitor';

        pMon := ActiveCircuit.Monitors.First;
        while pMon <> NIL do
        begin
            ComboBox1.Items.add(pMon.name);
            pMon := ActiveCircuit.Monitors.Next;
        end;

        ComboBox1.ItemIndex := 0;
        ShowModal;

        if CancelPressed then
            Cancelled := TRUE  // Do nothing
        else
            with ComboBox1 do
                SelectedMonitor := Items.strings[ItemIndex];
        Free;
    end;

    if not Cancelled then
        ActiveScriptForm.ExecuteDSSCommand('Export monitors ' + SelectedMonitor);
end;

procedure TControlPanel.ToolButton21Click(Sender: TObject);
var
    CurrDir: String;
{Open File Listed in combobox a Window}
begin
    if CompileCombo.ItemIndex >= 0 then
    begin
        if FileExists(CompileCombo.text) then
        begin
            try
                ActiveScriptForm := MakeANewEditForm(CompileCombo.text);
                ActiveScriptForm.Editor.Lines.LoadFromFile(CompileCombo.text);
                ActiveScriptForm.HasBeenModified := FALSE;
                ActiveScriptForm.HasFileName := TRUE;
                CurrDir := ExtractFileDir(CompileCombo.text);
                SetCurrentDir(CurrDir);
                SetDataPath(CurrDir);  // change datadirectory
                UpdateStatus;
            except
                On E: Exception do
                    DoSimpleMsg('Error Loading File: ' + E.Message, 218);
            end;
        end
        else
        begin
            DoSimpleMsg('File "' + CompileCombo.Text + '" Not Found.', 218);
        end; {File Exists}
    end;
end;

procedure TControlPanel.CurrentsElem2Click(Sender: TObject);
{ Visualize Command }
begin
    if Assigned(activeCircuit) then
    begin
        ActiveScriptForm.ExecuteDSSCommand('Visualize currents ' + EncloseQuotes(Classbox.text + '.' + Elementbox.Text));
    end;
end;

procedure TControlPanel.VoltagesElement1Click(Sender: TObject);
{ Visualize Command }
begin
    if Assigned(activeCircuit) then
    begin
        ActiveScriptForm.ExecuteDSSCommand('Visualize voltages ' + EncloseQuotes(Classbox.text + '.' + Elementbox.Text));
    end;
end;

procedure TControlPanel.PowersElement1Click(Sender: TObject);
{ Visualize Command }
begin
    if Assigned(activeCircuit) then
    begin
        ActiveScriptForm.ExecuteDSSCommand('Visualize powers ' + EncloseQuotes(Classbox.text + '.' + Elementbox.Text));
    end;
end;

procedure TControlPanel.CurrentsElemResid1Click(Sender: TObject);
begin

    ActiveScriptForm.ExecuteDSSCommand('Show Currents residual=yes Elements');

end;

procedure TControlPanel.RPNEvaluator1Click(Sender: TObject);
begin
    RPNform.ShowModal;
end;


initialization
    SelectedMonitor := '';

finalization


end.
