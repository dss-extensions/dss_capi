object myStdMsg: TmyStdMsg
  Left = 0
  Top = 0
  Caption = 'Error'
  ClientHeight = 130
  ClientWidth = 368
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = ShowForm
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 150
    Top = 96
    Width = 77
    Height = 26
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object myMessage: TRichEdit
    Left = 8
    Top = 8
    Width = 352
    Height = 81
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    HideScrollBars = False
    Lines.Strings = (
      'LicenseText')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    Zoom = 100
  end
end
