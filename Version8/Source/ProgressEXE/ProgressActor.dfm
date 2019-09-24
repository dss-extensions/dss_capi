object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 103
  ClientWidth = 434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 417
    Height = 25
  end
  object Button1: TButton
    Left = 350
    Top = 39
    Width = 75
    Height = 58
    Caption = 'Abort'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 336
    Height = 74
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 336
    Top = 192
  end
end
