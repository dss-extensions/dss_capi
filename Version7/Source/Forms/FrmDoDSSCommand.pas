unit FrmDoDSSCommand;

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
    StdCtrls;

type
    TDoDSSCommandForm = class(TForm)
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
    PRIVATE
    { Private declarations }
        function FindCmdIndex(s: String): Integer;
    PUBLIC
    { Public declarations }
        sCommand: String;
    end;

var
    DoDSSCommandForm: TDoDSSCommandForm;

implementation

uses
    ExecCommands,
    Executive;

{$R *.DFM}

procedure TDoDSSCommandForm.Btn_CancelClick(Sender: TObject);
begin
    sCommand := '';
    ModalResult := mrCancel;
end;

procedure TDoDSSCommandForm.Btn_DoitClick(Sender: TObject);
begin
    sCommand := E_Command.Text;
    ModalResult := mrOK;
end;

procedure TDoDSSCommandForm.FormCreate(Sender: TObject);

var
    i: Integer;
begin
     {Populate CommandCombo}
    with DSSCMDCombo do
    begin
        Clear;
        for i := 1 to NumExecCommands do
            Items.Add(ExecCommand[i]);
        ItemIndex := 3;
    end;

    DSSCMDComboChange(Sender);

end;

procedure TDoDSSCommandForm.DSSCMDComboChange(Sender: TObject);
begin
    E_Command.Text := DSSCMDCombo.Text + ' ' + Edit1.Text;
    Memo1.Text := CommandHelp[FindCmdIndex(DSSCMDCombo.Text)];
end;

procedure TDoDSSCommandForm.Edit1Change(Sender: TObject);
begin
    E_Command.Text := DSSCMDCombo.Text + ' ' + Edit1.Text;
end;

function TDoDSSCommandForm.FindCmdIndex(s: String): Integer;
begin
    for Result := 1 to NumExecCommands do
        if CompareText(S, ExecCommand[result]) = 0 then
            Break;
end;

procedure TDoDSSCommandForm.Btn_AddFileNameClick(Sender: TObject);
begin
    with OpenDialog1 do
    begin
        Filter := 'DSS Files|*.dss|Text (*.txt)|*.txt|All Files (*.*)|*.*';
        FileName := '';
        if Execute then
        begin
            Edit1.Text := Edit1.Text + ' "' + Filename + '"';
        end;
    end;
end;

end.
