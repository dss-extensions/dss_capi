unit DlgPlotOptions;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

uses
    Windows,
    SysUtils,
    Classes,
    Graphics,
    Forms,
    Controls,
    StdCtrls,
    Buttons,
    ExtCtrls,
    Dialogs,
    ComCtrls;

type
    TPlotOptionsForm = class(TForm)
        OKBtn: TButton;
        Bevel1: TBevel;
        GroupBox1: TGroupBox;
        GroupBox2: TGroupBox;
        DotsCheck: TCheckBox;
        LabelsCheck: TCheckBox;
        QtyCombo: TComboBox;
        EdtPlotMax: TEdit;
        ColorDialog1: TColorDialog;
        Label4: TLabel;
        GroupBox3: TGroupBox;
        Label1: TLabel;
        Label2: TLabel;
        EditColor2: TEdit;
        EditColor1: TEdit;
        GroupBox4: TGroupBox;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        EditMaxRange: TEdit;
        Label8: TLabel;
        EditMidRange: TEdit;
        ShapeMaxRange: TShape;
        ShapeMidrange: TShape;
        ShapeMinRange: TShape;
        EditValueIndex: TEdit;
        Label9: TLabel;
        Label3: TLabel;
        Label10: TLabel;
        Label11: TLabel;
        Label12: TLabel;
        EditAutoIndex: TEdit;
        UpDown1: TUpDown;
        LoopCheck: TCheckBox;
        SubCheckBox: TCheckBox;
        procedure FormCreate(Sender: TObject);
        procedure OKBtnClick(Sender: TObject);

        procedure EditColor1ContextPopup(Sender: TObject; MousePos: TPoint;
            var Handled: Boolean);
        procedure EditColor2Exit(Sender: TObject);
        procedure EditColor1Exit(Sender: TObject);
        procedure EditColor2ContextPopup(Sender: TObject; MousePos: TPoint;
            var Handled: Boolean);
        procedure ShapeMaxRangeContextPopup(Sender: TObject; MousePos: TPoint;
            var Handled: Boolean);
        procedure ShapeMidrangeContextPopup(Sender: TObject; MousePos: TPoint;
            var Handled: Boolean);
        procedure ShapeMinRangeContextPopup(Sender: TObject; MousePos: TPoint;
            var Handled: Boolean);
        procedure FormShow(Sender: TObject);
    PRIVATE
    { Private declarations }
        procedure GetValuesFromForm;
    PUBLIC
    { Public declarations }
        AutoColor1, AutoColor2, AutoColor3: TColor;
        RangeMax, RangeMid: Double;

    end;

var
    PlotOptionsForm: TPlotOptionsForm;

implementation

uses
    system.UITypes;

{$R *.DFM}

function IntToHexA(i, Digits: Integer): String;

begin
    Result := '$' + IntToHex(i, Digits);
end;

procedure TPlotOptionsForm.FormCreate(Sender: TObject);
begin
    Caption := 'Plot Options';

    QtyCombo.Clear;
    QtyCombo.Items.Add('None');
    QtyCombo.Items.Add('Voltage');
    QtyCombo.Items.Add('Current');
    QtyCombo.Items.Add('Power');
    QtyCombo.Items.Add('Losses');
    QtyCombo.Items.Add('Capacity');

    EditValueIndex.Text := '1';
    EditAutoIndex.Text := '3';
    EditColor1.Color := clBlue;
    EditColor1.Text := IntToHexA(EditColor1.Color, 8);
    EditColor2.Color := clRed;
    EditColor2.Text := IntToHexA(EditColor2.Color, 8);
    QtyCombo.ItemIndex := 3;

    GetValuesFromForm;

    RangeMax := 0.90;
    RangeMid := 0.75;

end;

procedure TPlotOptionsForm.OKBtnClick(Sender: TObject);
begin
    GetValuesFromForm;
    Close;
end;

procedure TPlotOptionsForm.EditColor1ContextPopup(Sender: TObject;
    MousePos: TPoint; var Handled: Boolean);
begin
    ColorDialog1.Color := Editcolor1.Color;
    if ColorDialog1.Execute then
    begin
        Editcolor1.Color := ColorDialog1.Color;
        EditColor1.Text := IntToHexA(ColorDialog1.Color, 8);
        Handled := TRUE;  {To stop default popup}
    end;
end;


procedure TPlotOptionsForm.EditColor2Exit(Sender: TObject);
begin
    Editcolor2.Color := StrToInt(EditColor2.Text);
end;

procedure TPlotOptionsForm.EditColor1Exit(Sender: TObject);
begin
    Editcolor1.Color := StrToInt(EditColor1.Text);
end;

procedure TPlotOptionsForm.EditColor2ContextPopup(Sender: TObject;
    MousePos: TPoint; var Handled: Boolean);
begin
    ColorDialog1.Color := Editcolor2.Color;
    if ColorDialog1.Execute then
    begin
        Editcolor2.Color := ColorDialog1.Color;
        EditColor2.Text := IntToHexA(ColorDialog1.Color, 8);
        Handled := TRUE;
    end;
end;

procedure TPlotOptionsForm.ShapeMaxRangeContextPopup(Sender: TObject;
    MousePos: TPoint; var Handled: Boolean);
begin
    ColorDialog1.Color := ShapeMaxRange.Brush.Color;
    if ColorDialog1.Execute then
    begin
        ShapeMaxRange.Brush.Color := ColorDialog1.Color;
    end;
end;

procedure TPlotOptionsForm.ShapeMidrangeContextPopup(Sender: TObject;
    MousePos: TPoint; var Handled: Boolean);
begin
    ColorDialog1.Color := ShapeMidrange.Brush.Color;
    if ColorDialog1.Execute then
    begin
        ShapeMidrange.Brush.Color := ColorDialog1.Color;
    end;
end;

procedure TPlotOptionsForm.ShapeMinRangeContextPopup(Sender: TObject;
    MousePos: TPoint; var Handled: Boolean);
begin
    ColorDialog1.Color := ShapeMinrange.Brush.Color;
    if ColorDialog1.Execute then
    begin
        ShapeMinrange.Brush.Color := ColorDialog1.Color;
    end;
end;

procedure TPlotOptionsForm.FormShow(Sender: TObject);
begin
    EditMaxRange.Text := IntToStr(Round(RangeMax * 100.0));
    EditMidRange.Text := IntToStr(Round(RangeMid * 100.0));
end;

procedure TPlotOptionsForm.GetValuesFromForm;

var
    Code: Integer;
    Test: Double;

begin
    AutoColor3 := ShapeMaxRange.Brush.Color;
    AutoColor2 := ShapeMidRange.Brush.Color;
    AutoColor1 := ShapeMinRange.Brush.Color;

    Val(EditMaxRange.text, Test, Code);
    if Code = 0 then
        RangeMax := Test * 0.01;
    Val(EditMidRange.text, Test, Code);
    if Code = 0 then
        RangeMid := Test * 0.01;
end;

end.
