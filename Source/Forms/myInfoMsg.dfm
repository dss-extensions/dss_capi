object myInfoMessage: TmyInfoMessage
  Left = 0
  Top = 0
  Caption = 'Information'
  ClientHeight = 127
  ClientWidth = 363
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = OnShow
  PixelsPerInch = 96
  TextHeight = 13
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
    TabOrder = 0
    Zoom = 100
  end
  object AbortBtn: TButton
    Left = 94
    Top = 95
    Width = 77
    Height = 26
    Caption = '&Abort'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = AbortOption
  end
  object IgnoreBtn: TButton
    Left = 190
    Top = 95
    Width = 77
    Height = 26
    Caption = '&Ignore'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = IgnoreOption
  end
end
