unit ExceptionTrace;

interface

Uses sysutils;

procedure ShowHeapUsage;
procedure DumpExceptionCallStack(E: Exception);

implementation

procedure DumpExceptionCallStack(E: Exception);
{$IFDEF FPC}
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  Writeln(Report);
  Halt; // End of program execution
{$ELSE}
var
  Report: string;
begin
// TODO: https://www.testrail.com/blog/working-with-delphis-new-exception-stacktrace/
  Report := 'Exception Class:' + E.ClassName + ' Message:' + E.Message;
  Writeln (Report);
{$ENDIF}
end;

procedure ShowHeapUsage;
{$IFDEF FPC}
var
   hstat: TFPCHeapStatus;
   s: string;
{$ENDIF}
begin
{$IFDEF FPC}
  hstat := GetFPCHeapStatus;
  s := Format('Heap Memory Used: %dK',[hstat.CurrHeapUsed div 1024]);
  Writeln(s);
{$ENDIF}
end;

end.
