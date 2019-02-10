unit PropEdit;

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
    Grids,
    DSSObject,
    StdCtrls;

type
    TPropEditForm = class(TForm)
        StringGrid1: TStringGrid;
        Button1: TButton;
        Button2: TButton;
        Edit1: TEdit;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
            const Value: String);
        procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
            var CanSelect: Boolean);
        procedure StringGrid1KeyPress(Sender: TObject; var Key: Char);
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        function QuotedIfBlanks(const S: String): String;
    PRIVATE
        strActiveCellValue: String;
    { Private declarations }
        procedure RefreshPropValues;
        procedure UpdateSelectedRow;
    PUBLIC
    { Public declarations }
        ObjectBeingEdited: TDSSObject;
    end;

var
    PropEditForm: TPropEditForm;

implementation

{$R *.DFM}

uses
    DSSGlobals,
    Executive,
    ParserDel;

var
    SelectedRow: Integer;
    CellEdited: Boolean;

procedure TPropEditForm.FormCreate(Sender: TObject);
begin

    StringGrid1.ColWidths[0] := 80;
    StringGrid1.ColWidths[1] := 300;
    StringGrid1.Width := 300 + 80 + 2;
    Width := StringGrid1.Width + 20;
    StringGrid1.DefaultRowHeight := Abs(StringGrid1.Font.Height) + 4;
    StringGrid1.colcount := 2;
    Edit1.Width := StringGrid1.Width;
    Edit1.Left := StringGrid1.Left;

end;

function Min(A, B: Integer): Integer;
begin
    if A < B then
        Result := A
    else
        Result := B;
end;

procedure TPropEditForm.FormShow(Sender: TObject);

var
    i: Integer;

begin

    try
        ObjectBeingEdited := ActiveDSSObject;
        Caption := Uppercase(ObjectBeingEdited.ParentClass.name + '.' + ObjectBeingEdited.Name);
        Edit1.Text := Caption;
        StringGrid1.rowcount := ObjectBeingEdited.ParentClass.NumProperties + 1;
        StringGrid1.Cells[0, 0] := 'Property';
        StringGrid1.Cells[1, 0] := 'Value';

        StringGrid1.Height := min(StringGrid1.DefaultRowHeight * (StringGrid1.RowCount + 2), (Screen.Height div 10) * 7);
        Height := StringGrid1.Height + Button1.Height + Edit1.Height + 50;
        Button1.Top := 25;
        Button2.Top := Button1.Top;
        with  ObjectBeingEdited.ParentClass do
            for i := 1 to NumProperties do
            begin
                StringGrid1.Cells[0, i] := ObjectBeingEdited.ParentClass.PropertyName^[i];
                StringGrid1.Cells[1, i] := QuotedIfBlanks(ObjectBeingEdited.GetPropertyValue(PropertyIdxMap[i]));
            end;

        SelectedRow := 0;
        CellEdited := FALSE;
        strActiveCellValue := '';

     {Make sure it is on screen}
 //       Top := 50;
 //       If Height > Screen.Height  then  Height := (Screen.Height div 10) * 9;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error attempting to show editing Form: ' + E.Message, 143);
            StringGrid1.RowCount := 2;
        end;
    end;
end;

procedure TPropEditForm.RefreshPropValues;

var
    i: Integer;

begin
    with ObjectBeingEdited.ParentClass do
        for i := 1 to NumProperties do
        begin
            StringGrid1.Cells[1, i] := QuotedIfBlanks(ObjectBeingEdited.GetPropertyValue(PropertyIdxMap[i]));
        end;
end;


procedure TPropEditForm.StringGrid1SetEditText(Sender: TObject; ACol,
    ARow: Integer; const Value: String);
begin

    if (comparetext(strActiveCellValue, Value) <> 0) then
        CellEdited := TRUE
    else
        CellEdited := FALSE;  //  keep track of the fact that the selected cell has been edited
    SelectedRow := Arow;
end;

procedure TPropEditForm.StringGrid1SelectCell(Sender: TObject; ACol,
    ARow: Integer; var CanSelect: Boolean);
begin

    if SelectedRow <> 0 then
        if (SelectedRow <> ARow) and CellEdited then
            UpdateSelectedRow;

    SelectedRow := ARow;
    CanSelect := TRUE;
    strActiveCellValue := StringGrid1.Cells[1, SelectedRow];
end;

procedure TPropEditForm.StringGrid1KeyPress(Sender: TObject;
    var Key: Char);
begin
    if (Ord(Key) = 13) and CellEdited then
        UpdateSelectedRow;   // on CR
end;

procedure TPropEditForm.UpdateSelectedRow;
begin
    Parser.CmdString := StringGrid1.Cells[0, SelectedRow] + '=' + StringGrid1.Cells[1, SelectedRow];
    ObjectBeingEdited.Edit;
    RefreshPropValues;
    CellEdited := FALSE;
end;

procedure TPropEditForm.Button1Click(Sender: TObject);
begin
    if CellEdited then
        UpdateSelectedRow;

end;

procedure TPropEditForm.Button2Click(Sender: TObject);
begin
    Close;
end;

function TPropEditForm.QuotedIfBlanks(const S: String): String;
begin
    Result := S;

    if Pos(' ', S) > 0 then
    begin
        if S[1] <> '(' then  // Ignore if already quoted
            if S[1] <> '[' then  // Ignore if already quoted
                if S[1] <> '{' then  // Ignore if already quoted
                    Result := '"' + S + '"';
    end;
end;

end.
