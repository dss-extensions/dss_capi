unit ProgressActor;

interface

uses
    djson,
    StrUtils,
    math,
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.ExtCtrls,
    Vcl.StdCtrls,
    System.Types,
    System.UITypes,
    IdTCPServer,
    IdBaseComponent,
    IdComponent,
    IdContext,
    IdGlobal,
    IdCustomTCPServer;

type

    TMyServer = class(TIdCustomTCPServer)
    PROTECTED
        function DoExecute(AContext: TIdContext): Boolean; OVERRIDE;
    end;

    TForm1 = class(TForm)
        Image1: TImage;
        Button1: TButton;
        Memo1: TMemo;
        Timer1: TTimer;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
        procedure Button1Click(Sender: TObject);
    PRIVATE
        Server: TMyServer;
        NumActors,
        divDelta: Integer;
        divSize: array of Integer;
        procedure Set_Progress(cmd: String);
        procedure Setup_Wnd(cmd: String);
        procedure ResetTimeOut;
        function GetDSSExeFile: String;
    { Private declarations }
    PUBLIC
        Timer: Integer;
        AbortON: Boolean;
        property WriteProgress: String WRITE Set_Progress;
        property SetupWnd: String WRITE Setup_Wnd;
    { Public declarations }
    end;

const
    NUMDIVISIONS = 1;
    UPDTPROGRESS = 2;
    QUITPRG = 3;

var
    Form1: TForm1;

implementation

const
    MaxTimeOutValue = 50;

var
    TimeOut: Integer;

{$R *.dfm}

function TForm1.GetDSSExeFile: String;

var
    TheFileName: array[0..MAX_PATH] of Char;

begin

    FillChar(TheFileName, SizeOF(TheFileName), #0);  // Fill it with nulls
    GetModuleFileName(HInstance, TheFileName, SizeOF(TheFileName));
    Result := TheFileName;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
    AbortON := TRUE;
end;

procedure TForm1.FormCreate(Sender: TObject);
var

    F: TextFile;
    JSONCfg: TdJSON;
    JSONStr,
    newPath: String;
    RefPos: Integer;
begin
    AbortON := FALSE;
    with Memo1 do
    begin
        Height := 74;
        Text := '';
    end;
    newPath := GetDSSExeFile();
    newPath := AnsiReverseString(newPath);   // reverses the string
    RefPos := pos('\', newPath); // detects the file name
    newPath := AnsiReverseString(newPath);      // reverse again
    newPath := newPath.Substring(0, length(newPath) - RefPos);  // Leaves only the folder name
    newPath := newPath + '\ComPorts.ini';

    Server := TMyServer.Create;
    Server.Tag := 0;
    Server.ListenQueue := 15;
    Server.TerminateWaitTime := 5000;

    if fileexists(newPath) then
    begin
        AssignFile(F, newPath);
        Reset(F);
        ReadLn(F, JSONStr);
        CloseFile(F);
    // parse the JSON string and extract the values
        JSONCfg := TdJSON.Parse(JSONStr);
        Server.DefaultPort := JSONCfg['dssprogress'].AsInteger;
    end
    else
        Server.DefaultPort := 20010;

    Server.Active := TRUE;
    TimeOut := 0;

    with Image1 do
    begin
        Canvas.Pen.Color := clWhite;
        Canvas.Brush.Color := clWhite;
        Canvas.Rectangle(0, 0, Width, Height);
    end;

    Caption := 'Simulation progress'

end;

procedure TForm1.ResetTimeOut;
begin
    TimeOut := 0;
end;

procedure TForm1.Set_Progress(cmd: String);
var
    ImgWidth,
    G1Width,
    G2Width,
    CRCount,
    I,
    X1,
    X2,
    Temp,
    StrLen: Integer;
    B: TBitmap;
    Separator,
    TxtProg: String;

begin
    B := TBitmap.Create;
    try
        B.Width := Image1.Width;
        B.Height := Image1.Height;
        B.Canvas.Brush.Color := clWhite;
        B.Canvas.Brush.Style := bsSolid;
        B.Canvas.Pen.Style := psClear;
        B.Canvas.Pen.Width := 1;
        B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
    // Format the variables for start drawing
        StrLen := length(cmd);
        B.Canvas.Brush.Color := clBlue;
        TxtProg := 'Progress (%):' + #13 + #10;
    // Starts drawing the progress for each actor
        if (StrLen < NumActors * 3) then
        begin
            B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
        end
        else
        begin
            CRCount := 0;
            for I := 0 to (NumActors - 1) do
            begin
                if I >= 9 then
                    Separator := '= '
                else
                    Separator := ' = ';
                TxtProg := TxtProg + 'Actor ' + inttostr(I + 1) + Separator + cmd.Substring(3 * I, 3) + ' ';
                inc(CRCount);
                if CRCount >= 4 then
                begin
                    TxtProg := TxtProg + #13 + #10;
                    CRCount := 0;
                end;
                Temp := strtoint(cmd.Substring(3 * I, 3));
                X1 := divSize[I];
                X2 := X1 + (Temp * divDelta) div 100;
                B.Canvas.FillRect(Rect(X1, 0, X2, B.Height));
            end;
        end;

        B.Canvas.Pen.Color := clBlack;
        B.Canvas.Pen.Style := psSolid;
        B.Canvas.Brush.Style := bsClear;
        B.Canvas.Rectangle(0, 0, B.Width, B.Height);
    // Puts the draw in the image indicator
        Image1.Picture.Assign(B);
    finally
        B.Free;
    end;
    Memo1.Text := TxtProg;
end;

procedure TForm1.Timer1Timer(Sender: TObject); // Waits for 5 secs, if there is no
begin                                          // interactions with the caller, the
    Inc(TimeOut);                                // app will close
    if TimeOut >= MaxTimeOutValue then
    begin
        Timer1.Enabled := FALSE;
        Close;
    end;
end;

procedure TForm1.Setup_Wnd(cmd: String);
var
    I: Integer;
begin
    SetRoundMode(rmUp);
    NumActors := strtoint(cmd);
    divDelta := round(Image1.Width div NumActors);
    setlength(divSize, NumActors);
    for I := 0 to High(divSize) do
    begin
        divSize[I] := I * divDelta;
    end;

end;

function TMyServer.DoExecute(AContext: TIdContext): Boolean;
var
    msgToClient,
    msgFromClient: String;
    msgType: Integer;
begin
    Result := inherited;
    if AContext.Connection.IOHandler.InputBufferIsEmpty then
    begin
        IndySleep(10);
        Exit;
    end;
    Form1.ResetTimeOut;
    msgFromClient := AContext.Connection.IOHandler.ReadLn(#10, 200);

    msgType := -1;
    if msgFromClient.Substring(0, 3) = 'num' then
        msgType := NUMDIVISIONS;
    if msgFromClient.Substring(0, 3) = 'prg' then
        msgType := UPDTPROGRESS;
    if msgFromClient.Substring(0, 3) = 'ext' then
        msgType := QUITPRG;

    msgToClient := '';
  // evaluates the message type
    case msgType of
        NUMDIVISIONS:
            Form1.SetupWnd := msgFromClient.Substring(3);
        UPDTPROGRESS:
            Form1.WriteProgress := msgFromClient.Substring(3);
        QUITPRG:
        begin
            msgToClient := 'Q'
        end
    else
    begin
        msgToClient := 'E'
    end;
    end;
    if msgToClient <> 'Q' then
    begin
        if msgToClient <> 'E' then
        begin
            if Form1.AbortON then
                msgToClient := 'T'
            else
                msgToClient := 'F';
        end;
        AContext.Connection.IOHandler.WriteLn(msgToClient);
    end
    else
    begin
        TimeOut := 60;  // Closes the app
    end;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    Server.Free;
end;

end.
