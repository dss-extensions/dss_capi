program DSSProgress;

uses
    Vcl.Forms,
    ProgressActor in 'ProgressActor.pas' {Form1},
    djson in '..\Common\djson.pas';

{$R *.res}

begin
    Application.Initialize;
    Application.MainFormOnTaskbar := TRUE;
    Application.CreateForm(TForm1, Form1);
    Application.Run;
end.
