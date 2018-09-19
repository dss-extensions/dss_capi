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
//**   StdCtrls, ComCtrls;


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
  private
    { Private declarations }
    Procedure Set_PctProgress(Value : Integer);
    Procedure Set_Caption(Const Value :String);

  public
    { Public declarations }
    Property Caption :String Write Set_Caption;
    Property PctProgress :Integer Write Set_PctProgress;
  end;

var
  Progress: TProgress;

implementation

uses
    DSSGlobals;

{$R *.FMX}

procedure TProgress.AbortBtnClick(Sender: TObject);
begin
     SolutionAbort := True;
end;

Procedure TProgress.Set_PctProgress(Value : Integer);
Begin
     Progressbar1.Position := Value;
End;

Procedure TProgress.Set_Caption(Const Value :String);
Begin
     Formcaption.Text  := Value;
     Application.ProcessMessages;
End;


procedure TProgress.FormCreate(Sender: TObject);
begin
   FormCaption.Text  := '';
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
