unit ProgressForm;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
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
    ComCtrls;

type
    TProgress = class(TForm)
        ProgressBar1: TProgressBar;
        Label1: TLabel;
        AbortBtn: TButton;
        FormCaption: TLabel;
        procedure AbortBtnClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormHide(Sender: TObject);
    PRIVATE
    { Private declarations }
        procedure Set_PctProgress(Value: Integer);
        procedure Set_Caption(const Value: String);

    PUBLIC
    { Public declarations }
        property Caption: String WRITE Set_Caption;
        property PctProgress: Integer WRITE Set_PctProgress;
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

procedure TProgress.Set_PctProgress(Value: Integer);
begin
    Progressbar1.Position := Value;
end;

procedure TProgress.Set_Caption(const Value: String);
begin
    Formcaption.Caption := Value;
    Application.ProcessMessages;
end;


procedure TProgress.FormCreate(Sender: TObject);
begin
    FormCaption.Caption := '';
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
