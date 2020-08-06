unit ReportForm;

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
    TReport = class(TForm)
        Memo1: TMemo;
    PRIVATE
    { Private declarations }
    PUBLIC
    { Public declarations }
    end;

var
    Report: TReport;

implementation

{$R *.DFM}

end.
