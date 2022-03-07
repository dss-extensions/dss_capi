unit RollAvgWindow;

interface
{$PUSH}
uses
    gqueue;

type
    TRollAvgWindow = class(TObject)
    PRIVATE
        bufferlength: Integer;
        sample: TQueue<Double>;
        sampletime: TQueue<Double>;
        runningsumsample: Double;
        runningsumsampletime: Double;
        bufferfull: Boolean;

    PUBLIC
        constructor Create();
        destructor Destroy; OVERRIDE;
        procedure Add(IncomingSampleValue: Double; IncomingSampleTime: Double; VAvgWindowLengthSec: Double);
        procedure SetLength(const Value: Integer);
        function AvgVal: Double;
        function AccumSec: Double;
    end;
{$POP}

implementation

procedure TRollAvgWindow.Add(IncomingSampleValue: Double; IncomingSampleTime: Double; VAvgWindowLengthSec: Double);
begin
    if (sample.size > 0) and (bufferfull) then
    begin
        runningsumsample := runningsumsample - sample.front;
        if (bufferlength = 0) then
        begin
            IncomingSampleValue := 0.0;
        end;
        sample.pop;sample.push(IncomingSampleValue);
        sampletime.pop;sampletime.push(IncomingSampleTime);

        runningsumsample := runningsumsample + IncomingSampleValue;
        runningsumsampletime := runningsumsampletime - sampletime.front;
        runningsumsampletime := runningsumsampletime + IncomingSampleTime;
    end
    else
    begin
        if (bufferlength = 0) then
        begin
            IncomingSampleValue := 0.0;
        end;

        sample.push(IncomingSampleValue);
        sampletime.push(IncomingSampleTime);

        runningsumsample := runningsumsample + IncomingSampleValue;
        runningsumsampletime := runningsumsampletime + IncomingSampleTime;

        if (runningsumsampletime > VAvgWindowLengthSec) then
            bufferfull := TRUE;
        if (sample.size = bufferlength) then
            bufferfull := TRUE;
    end;
end;

constructor TRollAvgWindow.Create();
begin
    sample := TQueue<Double>.Create();
    sampletime := TQueue<Double>.Create();

    runningsumsample := 0.0;
    runningsumsampletime := 0.0;
    bufferlength := 0;
    bufferfull := FALSE;
end;

destructor TRollAvgWindow.Destroy;
begin
    sample := NIL;
    sampletime := NIL;

    inherited;
end;

procedure TRollAvgWindow.SetLength(const Value: Integer);
begin
    bufferlength := Value;
end;

function TRollAvgWindow.AvgVal: Double;
begin
    if (sample.size = 0) then
        Result := 0.0
    else
        Result := runningsumsample / sample.size;
end;

function TRollAvgWindow.AccumSec: Double;
begin
    if (sample.size = 0) then
        Result := 0.0
    else
        Result := runningsumsampletime;
end;

end.