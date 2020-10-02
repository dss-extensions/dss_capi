unit myInfoMsg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TmyInfoMessage = class(TForm)
    myMessage: TRichEdit;
    AbortBtn: TButton;
    IgnoreBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure AbortOption(Sender: TObject);
    procedure IgnoreOption(Sender: TObject);
    procedure OnShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    myMsg     : String;
    MyResult  : integer;
  end;

var
  myInfoMessage: TmyInfoMessage;

implementation

{$R *.dfm}

procedure TmyInfoMessage.AbortOption(Sender: TObject);
begin
  myResult  :=  3;
end;

procedure TmyInfoMessage.FormCreate(Sender: TObject);
begin
  Left:=(Screen.Width-Width)  div 2;
  Top:=(Screen.Height-Height) div 2;
  myMessage.Lines.Clear();
end;

procedure TmyInfoMessage.IgnoreOption(Sender: TObject);
begin
  myResult  :=  5;
end;

procedure TmyInfoMessage.OnShow(Sender: TObject);
begin
  myMessage.Lines.Add(myMsg);
  myMessage.SelStart := 0;
end;

end.
