unit FrmCSVchannelSelect;

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
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls;

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
    PRIVATE
    { Private declarations }
    PUBLIC
    { Public declarations }
        ResultString: String;
    end;

var
    ChannelSelectForm: TChannelSelectForm;

implementation

{$R *.dfm}

procedure TChannelSelectForm.Button1Click(Sender: TObject);
var
    i: Integer;
begin
    ResultString := '';
    for i := 0 to ListBox1.Count - 1 do
    begin
        if ListBox1.Selected[i] then
        begin
            ResultString := ResultString + Format('%d ', [i + 1]);
        end;
    end;

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
