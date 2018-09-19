unit FrmCSVchannelSelect;
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
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, Vcl.Controls;

//**   Original VCL Uses section : 


//**   Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
//**   Dialogs, StdCtrls;


type
  TChannelSelectForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ResultString:String;
  end;

var
  ChannelSelectForm: TChannelSelectForm;

implementation

{$R *.dfm}

procedure TChannelSelectForm.Button1Click(Sender: TObject);
Var
   i:Integer;
begin
       ResultString := '';
       for i := 0 to ListBox1.Count - 1 do  Begin
         if ListBox1.Selected[i]  then  Begin
           ResultString := ResultString + Format('%d ', [i+1]);
         End;
       End;

       ModalResult := mrOK;
end;

procedure TChannelSelectForm.Button2Click(Sender: TObject);
begin
       ResultString := '';
       ModalResult := mrCancel;
end;

procedure TChannelSelectForm.FormCreate(Sender: TObject);
begin
     ResultString := '';
end;

end.
