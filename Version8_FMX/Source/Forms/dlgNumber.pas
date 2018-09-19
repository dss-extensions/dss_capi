unit dlgNumber;
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
  Datasnap.DBClient,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, Vcl.Controls;

//**   Original VCL Uses section : 


//** WINDOWS, SYSUTILS, CLASSES, GRAPHICS, FORMS, CONTROLS, STDCTRLS,
//**   Buttons, ExtCtrls;


type
  TValueEntryForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    Edit1: TEdit;
    ValueLabel: TLabel;
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    OKPressed :Boolean;
  end;

var
  ValueEntryForm: TValueEntryForm;

implementation

{$R *.DFM}

procedure TValueEntryForm.OKBtnClick(Sender: TObject);
begin
       OKPressed := True;
       Close;
end;

procedure TValueEntryForm.CancelBtnClick(Sender: TObject);
begin
        OKPressed := False;
        Close;
end;

end.
