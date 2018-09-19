unit ListForm;
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
  FMX.Graphics;

//**   Original VCL Uses section : 


//**   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
//**   StdCtrls;


type
  TListBoxForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    ComboBox1: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CancelPressed:Boolean;
    SelectedValue:String;
    SelectedIndex:Integer;
  end;

var
  ListBoxForm: TListBoxForm;

implementation

{$R *.FMX}

procedure TListBoxForm.FormCreate(Sender: TObject);
begin
     CancelPressed := FALSE;
end;

procedure TListBoxForm.OKBtnClick(Sender: TObject);
begin
     CancelPressed := FALSE;
     SelectedIndex := ComboBox1.ItemIndex;
     SelectedValue := ComboBox1.Items.Strings[SelectedIndex];
     Close;
end;

procedure TListBoxForm.CancelBtnClick(Sender: TObject);
begin
    CancelPressed := TRUE;
    Close;
end;

procedure TListBoxForm.ListBox1DblClick(Sender: TObject);
begin
     OKBtnClick(Sender);
end;

end.
