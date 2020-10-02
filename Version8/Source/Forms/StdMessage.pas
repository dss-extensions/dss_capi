unit StdMessage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TmyStdMsg = class(TForm)
    OKButton: TButton;
    myMessage: TRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure ShowForm(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    myMsg     : string;
  end;

var
  myStdMsg   : TmyStdMsg;

implementation

uses DssGlobals;

{$R *.dfm}


procedure TmyStdMsg.FormCreate(Sender: TObject);
begin
  Left:=(Screen.Width-Width)  div 2;
  Top:=(Screen.Height-Height) div 2;
  myMessage.Lines.Clear();
end;

procedure TmyStdMsg.ShowForm(Sender: TObject);
begin
  myMessage.Lines.Add(myMsg);
  myMessage.SelStart := 0;
end;

end.
