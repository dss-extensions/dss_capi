unit ProgressForm;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2019, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{ Change Log

   Created 8-14-99

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Vcl.ExtCtrls;

type
  TProgress = class(TForm)
    Label1: TLabel;
    AbortBtn: TButton;
    FormCaption: TLabel;
    Image2: TImage;
    Edit1: TEdit;
    procedure AbortBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);

  private
    { Private declarations }
    Procedure Set_Caption(Const Value :String);

  public
    { Public declarations }
    Property Caption :String Write Set_Caption;
    Procedure Set_PctProgress();
  end;

var
  Progress: TProgress;

implementation

uses
    DSSGlobals;

{$R *.DFM}

procedure TProgress.AbortBtnClick(Sender: TObject);
begin
     SolutionAbort := True;
end;

Procedure TProgress.Set_PctProgress();
var
  deltaBar  : Double;
  divBar    : array of Integer;
  X1,
  X2,
  Progress,
  I         : Integer;
  Running   : Boolean;
Begin
  IsProgressON        := True;
  Visible             :=  True;
  with Image2 do
  begin

    deltaBar            :=  round(width/NumOfActors);
    setlength(divBar,NumOfActors);

    for I := 0 to High(divBar) do
      divBar[I] :=  round(I * deltaBar);

    Canvas.Pen.Color    := clBlue;
    Canvas.Brush.Color  := clBlue;

    Running           := True;
    while Running do
    Begin
      Running   :=  False;
      for I := 0 to High(divBar) do
      Begin
        Progress      :=  round((ActorPctProgress[I + 1] / 100) * deltaBar);
        X1            :=  divBar[I];
        X2            :=  (divBar[I] + Progress);
        Canvas.Rectangle(X1, 0, X2, Height);
        Running       :=  Running or (ActorStatus[I + 1] = 0);
      End;
      sleep(100);
    End;

  End
End;

Procedure TProgress.Set_Caption(Const Value :String);
Begin
     Formcaption.Caption := Value;
     Application.ProcessMessages;
End;


procedure TProgress.FormCreate(Sender: TObject);
begin
   FormCaption.Caption := '';
   // Clears the image container
   with Image2 do
   begin
    Canvas.Pen.Color := clWhite;
    Canvas.Brush.Color := clWhite;
    Canvas.Rectangle(0, 0, Width, Height);
   end;
   With Edit1 do
   Begin
     Height   :=  48;
   End;
end;

procedure TProgress.FormShow(Sender: TObject);
begin
     SolutionAbort := False;
end;


procedure TProgress.FormHide(Sender: TObject);
begin
    { SolutionAbort := False;
      reset this wherever a new command is entered
    }
end;

end.
