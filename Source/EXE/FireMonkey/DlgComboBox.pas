unit DlgComboBox;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

//** Converted with Mida 560     http://www.midaconverter.com - MONTENEGRO.MARTINEZ



Uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IniFiles,
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
  Datasnap.DBClient;

//**   Original VCL Uses section : 


//** WINDOWS, SYSUTILS, CLASSES, GRAPHICS, FORMS, CONTROLS, STDCTRLS,
//**   Buttons, ExtCtrls;


type
  TOptionComboForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    ValueLabel: TLabel;
    ComboLabel: TLabel;
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    OKPressed:Boolean;
  end;

var
  OptionComboForm: TOptionComboForm;

implementation

{$R *.DFM}

procedure TOptionComboForm.OKBtnClick(Sender: TObject);
begin
        OKPressed := TRUE;
end;

procedure TOptionComboForm.CancelBtnClick(Sender: TObject);
begin
        OKPressed := False;
end;

end.
