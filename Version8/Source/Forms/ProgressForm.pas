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
    Windows,
    Messages,
    SysUtils,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    ComCtrls,
    Vcl.ExtCtrls;

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

    PRIVATE
    { Private declarations }
        procedure Set_Caption(const Value: String);

    PUBLIC
    { Public declarations }
        property Caption: String WRITE Set_Caption;
        procedure Set_PctProgress();
    end;

var
    Progress: TProgress;

implementation

uses
    DSSGlobals;

{$R *.DFM}

procedure TProgress.AbortBtnClick(Sender: TObject);
begin
    SolutionAbort := TRUE;
end;

procedure TProgress.Set_PctProgress();
var
    deltaBar: Double;
    divBar: array of Integer;
    X1,
    X2,
    Progress,
    I: Integer;
    Running: Boolean;
begin
    IsProgressON := TRUE;
    Visible := TRUE;
    with Image2 do
    begin

        deltaBar := round(width / NumOfActors);
        setlength(divBar, NumOfActors);

        for I := 0 to High(divBar) do
            divBar[I] := round(I * deltaBar);

        Canvas.Pen.Color := clBlue;
        Canvas.Brush.Color := clBlue;

        Running := TRUE;
        while Running do
        begin
            Running := FALSE;
            for I := 0 to High(divBar) do
            begin
                Progress := round((ActorPctProgress[I + 1] / 100) * deltaBar);
                X1 := divBar[I];
                X2 := (divBar[I] + Progress);
                Canvas.Rectangle(X1, 0, X2, Height);
                Running := Running or (ActorStatus[I + 1] = 0);
            end;
            sleep(100);
        end;

    end
end;

procedure TProgress.Set_Caption(const Value: String);
begin
    Formcaption.Caption := Value;
    Application.ProcessMessages;
end;


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
    with Edit1 do
    begin
        Height := 48;
    end;
end;

procedure TProgress.FormShow(Sender: TObject);
begin
    SolutionAbort := FALSE;
end;


procedure TProgress.FormHide(Sender: TObject);
begin
    { SolutionAbort := False;
      reset this wherever a new command is entered
    }
end;

end.
