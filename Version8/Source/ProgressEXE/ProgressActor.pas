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

  TMyServer = class (TIdCustomTCPServer)
  protected
    function DoExecute(AContext: TIdContext): Boolean; override;
  end;

  TForm1 = class(TForm)
    Image1      : TImage;
    Button1     : TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    Server      : TMyServer;
    NumActors,
    divDelta    : Integer;
    divSize     : array of integer;
    procedure Set_Progress(cmd : string);
    procedure Setup_Wnd(cmd : string);
    procedure ResetTimeOut;
    FUNCTION GetDSSExeFile: String;
    { Private declarations }
  public
    Timer       : Integer;
    AbortON     : Boolean;
    property WriteProgress:string write Set_Progress;
    property SetupWnd: string write Setup_Wnd;
    { Public declarations }
  end;

CONST
  NUMDIVISIONS  = 1;
  UPDTPROGRESS  = 2;
  QUITPRG       = 3;

var
  Form1: TForm1;

implementation

const
  MaxTimeOutValue = 50;
var
  TimeOut : Integer;

{$R *.dfm}

FUNCTION TForm1.GetDSSExeFile: String;

Var
   TheFileName:Array[0..MAX_PATH] of char;

Begin

    FillChar(TheFileName, SizeOF(TheFileName), #0);  // Fill it with nulls
    GetModuleFileName(HInstance, TheFileName, SizeOF(TheFileName));
    Result := TheFileName;

End;

procedure TForm1.Button1Click(Sender: TObject);
begin
  AbortON :=  True;
end;

procedure TForm1.FormCreate(Sender: TObject);
var

  F         : TextFile;
  JSONCfg   : TdJSON;
  JSONStr,
  newPath   : String;
  RefPos    : Integer;
begin
  AbortON       :=  False;
  With Memo1 do
  Begin
    Height        :=  64;
    Text          :=  '';
  End;
  newPath             :=  GetDSSExeFile();
  newPath             :=  AnsiReverseString(newPath);   // reverses the string
  RefPos              :=  pos('\',newPath); // detects the file name
  newPath             :=  AnsiReverseString(newPath);      // reverse again
  newPath             :=  newPath.Substring(0,length(newPath) - RefPos);  // Leaves only the folder name
  newPath             :=  newPath + '\ComPorts.ini';

  Server              :=  TMyServer.Create;
  Server.Tag          := 0;
  Server.ListenQueue  := 15;
  Server.TerminateWaitTime := 5000;

  if  fileexists(newPath) then
  Begin
    AssignFile(F, newPath);
    Reset(F);
    ReadLn(F, JSONStr);
    CloseFile(F);
    // parse the JSON string and extract the values
    JSONCfg             :=  TdJSON.Parse(JSONStr);
    Server.DefaultPort  :=  JSONCfg['dssprogress'].AsInteger;
  End
  else
    Server.DefaultPort  :=  20010;

  Server.Active       :=  True;
  TimeOut             :=  0;

  with Image1 do
  begin
    Canvas.Pen.Color := clWhite;
    Canvas.Brush.Color := clWhite;
    Canvas.Rectangle(0, 0, Width, Height);
  end;

  Caption :=  'Simulation progress'

end;

procedure TForm1.ResetTimeOut;
begin
  TimeOut := 0;
end;

procedure TForm1.Set_Progress(cmd : string);
var
  ImgWidth,
  G1Width,
  G2Width,
  CRCount,
  I,
  X1,
  X2,
  Temp,
  StrLen  : Integer;
  B       : TBitmap;
  Separator,
  TxtProg : String;

Begin
  B := TBitmap.Create;
  try
    B.Width               := Image1.Width;
    B.Height              := Image1.Height;
    B.Canvas.Brush.Color  := clWhite;
    B.Canvas.Brush.Style  := bsSolid;
    B.Canvas.Pen.Style    := psClear;
    B.Canvas.Pen.Width    := 1;
    B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
    // Format the variables for start drawing
    StrLen                :=  length(cmd);
    B.Canvas.Brush.Color  := clBlue;
    TxtProg               :=  'Progress (%):' + #13 + #10;
    // Starts drawing the progress for each actor
    if (StrLen < NumActors*3) then
    Begin
      B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
    End
    else
    Begin
      CRCount :=  0;
      for I := 0 to (NumActors - 1) do
      Begin
        if I >= 9 then Separator := '= '
        else Separator := ' = ';
        TxtProg     :=  TxtProg + 'Actor ' + inttostr(I + 1) + Separator + cmd.Substring(3*I,3) + ' ';
        inc(CRCount);
        if CRCount >= 4 then
        Begin
          TxtProg     :=  TxtProg + #13 + #10;
          CRCount     :=  0;
        End;
        Temp        :=  strtoint(cmd.Substring(3*I,3));
        X1          :=  divSize[I];
        X2          :=  X1 + ( Temp * divDelta) div 100;
        B.Canvas.FillRect(Rect(X1, 0, X2, B.Height));
      End;
    End;

    B.Canvas.Pen.Color := clBlack;
    B.Canvas.Pen.Style := psSolid;
    B.Canvas.Brush.Style := bsClear;
    B.Canvas.Rectangle(0, 0, B.Width, B.Height);
    // Puts the draw in the image indicator
    Image1.Picture.Assign(B);
  finally
    B.Free;
  end;
  Memo1.Text  :=  TxtProg;
End;

procedure TForm1.Timer1Timer(Sender: TObject); // Waits for 5 secs, if there is no
begin                                          // interactions with the caller, the
  Inc(TimeOut);                                // app will close
  if TimeOut >= MaxTimeOutValue then begin
    Timer1.Enabled := False;
    Close;
  end;
end;

procedure TForm1.Setup_Wnd(cmd : string);
var
  I   :   Integer;
Begin
  SetRoundMode(rmUp);
  NumActors :=  strtoint(cmd);
  divDelta  :=  round(Image1.Width div NumActors);
  setlength(divSize,NumActors);
  for I := 0 to High(divSize) do
  Begin
    divSize[I]  :=  I * divDelta;
  End;

End;

function TMyServer.DoExecute(AContext: TIdContext): Boolean;
var
  msgToClient,
  msgFromClient : string;
  msgType       : Integer;
begin
  Result  :=  inherited;
  if AContext.Connection.IOHandler.InputBufferIsEmpty then
  Begin
    IndySleep(10);
    Exit;
  End;
  Form1.ResetTimeOut;
  msgFromClient := AContext.Connection.IOHandler.ReadLn(#10, 200);

  msgType :=  -1;
  if msgFromClient.Substring(0,3) = 'num' then
    msgType :=  NUMDIVISIONS;
  if msgFromClient.Substring(0,3) = 'prg' then
    msgType :=  UPDTPROGRESS;
  if msgFromClient.Substring(0,3) = 'ext' then
    msgType :=  QUITPRG;

  msgToClient  :=  '';
  // evaluates the message type
  case msgType of
    NUMDIVISIONS  : Form1.SetupWnd       :=  msgFromClient.Substring(3);
    UPDTPROGRESS  : Form1.WriteProgress  :=  msgFromClient.Substring(3);
    QUITPRG       : Begin
                      msgToClient := 'Q'
    End
    else
    Begin
      msgToClient  :=  'E'
    End;
  end;
  if msgToClient <> 'Q' then
  Begin
    if msgToClient <> 'E' then
    Begin
      if Form1.AbortON then msgToClient  :=  'T'
      else msgToClient  :=  'F';
    End;
    AContext.Connection.IOHandler.WriteLn(msgToClient);
  End
  else
  Begin
    TimeOut :=  60;  // Closes the app
  End;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Server.Free;
end;

end.
