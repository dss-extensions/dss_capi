unit FrmDoDSSCommand;
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
  FMX.Graphics, FMX.ScrollBox, FMX.Controls.Presentation;

//**   Original VCL Uses section : 


//**   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
//**   StdCtrls;


type
  TDoDSSCommandForm = class(TForm)
StyleBook1: TStyleBook;

    DSSCMDCombo: TComboBox;
    Edit1: TEdit;
    Btn_Doit: TButton;
    Btn_Cancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    Btn_AddFileName: TButton;
    E_Command: TEdit;
    procedure Btn_CancelClick(Sender: TObject);
    procedure Btn_DoitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DSSCMDComboChange(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Btn_AddFileNameClick(Sender: TObject);
  private
    { Private declarations }
    Function FindCmdIndex(s:String):Integer;
  public
    { Public declarations }
    sCommand:String;
  end;

var
  DoDSSCommandForm: TDoDSSCommandForm;

implementation

Uses ExecCommands, Executive;

{$R *.FMX}

procedure TDoDSSCommandForm.Btn_CancelClick(Sender: TObject);
begin
     sCommand := '';
     ModalResult := mrCancel;
end;

procedure TDoDSSCommandForm.Btn_DoitClick(Sender: TObject);
begin
     sCommand :=  E_Command.Text;
     ModalResult := mrOK;
end;

procedure TDoDSSCommandForm.FormCreate(Sender: TObject);

Var
   i:Integer;
begin
     {Populate CommandCombo}
     With DSSCMDCombo Do Begin
         Clear;
         For i := 1 to NumExecCommands Do Items.Add(ExecCommand[i]);
         ItemIndex := 3;
     End;

     DSSCMDComboChange(Sender);

end;

procedure TDoDSSCommandForm.DSSCMDComboChange(Sender: TObject);
begin
//DavisMMP
//     E_Command.Text := DSSCMDCombo.Text + ' ' + Edit1.Text;
//     Memo1.Text := CommandHelp[FindCmdIndex(DSSCMDCombo.Text)];
end;

procedure TDoDSSCommandForm.Edit1Change(Sender: TObject);
begin
//DavisMMP
//    E_Command.Text := DSSCMDCombo.Text + ' ' + Edit1.Text;
end;

function TDoDSSCommandForm.FindCmdIndex(s: String): Integer;
begin
     For Result := 1 to NumExecCommands Do
       If CompareText(S, ExecCommand[result])=0 Then Break;
end;

procedure TDoDSSCommandForm.Btn_AddFileNameClick(Sender: TObject);
begin
     With OpenDialog1 Do begin
         Filter := 'DSS Files|*.dss|Text (*.txt)|*.txt|All Files (*.*)|*.*';
         FileName := '';
         If Execute Then Begin
             Edit1.Text := Edit1.Text + ' "' + Filename +'"';
         End;
     End;
end;

end.
