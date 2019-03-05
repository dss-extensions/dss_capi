unit ControlQueue;

{
   ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   11-1-00 added Handle and delete function
}

{$M+}

interface

uses
    Arraydef,
    ControlElem,
    Classes;

type

    TTimeRec = record
        Hour: Integer;
        Sec: Double;
    end;

    pActionRecord = ^TActionRecord;

    TActionRecord = record
        ActionTime: TTimeRec;
        ActionCode: Integer;
        ActionHandle: Integer;
        ProxyHandle: Integer;
        ControlElement: TControlElem;
    end;

    TControlQueue = class(Tobject)
    PRIVATE
        ActionList: TList;
        DebugTrace: Boolean;
        Tracefile: TextFile;
        ctrlHandle: Integer;
        Temp_Int: array[0..3] of Integer; // Temporary registers, Int Type
        Temp_dbl: array[0..7] of Double;  // Temporary registers, dbl type
        Ltimer: TTimeRec;

        function Pop(const ActionTime: TTimeRec; var Code, ProxyHdl, Hdl: Integer): TControlElem;  // Pop action from queue <= given time
        function Pop_Time(const ActionTime: TTimeRec; var Code, ProxyHdl, Hdl: Integer; var ATime: Double; KeepIn: Boolean): TControlElem;  // Pop action from queue <= given time
        procedure DeleteFromQueue(i: Integer; popped: Boolean);
        function TimeRecToTime(Trec: TTimeRec): Double;
        procedure Set_Trace(const Value: Boolean);
        procedure WriteTraceRecord(const ElementName: String; const Code: Integer; TraceParameter: Double; const s: String);
        function Get_QueueSize: Integer;
        procedure Recalc_Time_Step;
        procedure Restore_Time_Step;

    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Push(const Hour: Integer; const Sec: Double; const Code, ProxyHdl: Integer; const Owner: TControlElem): Integer; OVERLOAD;
        function Push(const Hour: Integer; const Sec: Double; const Code: EControlAction; const ProxyHdl: Integer; const Owner: TControlElem): Integer; OVERLOAD;
        procedure Clear;
        procedure DoAllActions;
        function DoNearestActions(var Hour: Integer; var Sec: Double): Boolean;  // Do only actions with lowest time
        function DoActions(const Hour: Integer; const sec: Double): Boolean;  // Do actions with time <= t
        function DoMultiRate(const Hour: Integer; const sec: Double): Boolean;  // Do actions with time <= t
        function IsEmpty: Boolean;
        procedure Delete(Hdl: Integer);  // Delete queue item by handle

        procedure ShowQueue(const Filenm: String);

        property TraceLog: Boolean READ DebugTrace WRITE Set_Trace;
        property QueueSize: Integer READ Get_QueueSize;
        function QueueItem(Qidx: Integer): String;


    end;


implementation

uses
    DSSGlobals,
    sysutils,
    Utilities,
    YMatrix;

{ TControlQueue }

function TControlQueue.Push(const Hour: Integer; const Sec: Double; const code: EControlAction; const ProxyHdl: Integer; const Owner: TControlElem): Integer;
begin
    Result := Push(Hour, Sec, Integer(code), ProxyHdl, Owner);
end;

function TControlQueue.Push(const Hour: Integer; const Sec: Double; const code, ProxyHdl: Integer; const Owner: TControlElem): Integer;

{Add a control action to the queue, sorted by lowest time first}
{Returns handle to the action}

var
    i,
    Hr: Integer;
    ThisActionTime,
    S: Double;
    Trec: TTimeRec;
    pAction: pActionRecord;
    ActionInserted: Boolean;

begin


    Inc(ctrlHandle); // just a serial number

     {Normalize the time }
    Hr := Hour;
    S := Sec;
    if S > 3600.0 then
        repeat
            Hr := Hr + 1;
            S := S - 3600.0;
        until S < 3600.0;

    Trec.Hour := Hr;
    Trec.Sec := S;

    ThisActionTime := TimeRecToTime(Trec);
    pAction := Allocmem(Sizeof(TActionRecord));  // Make a new Action

     {Insert the action in the list in order of time}
    ActionInserted := FALSE;
    for i := 0 to ActionList.Count - 1 do
    begin
        if ThisActionTime <= TimeRecToTime(pActionRecord(ActionList.Items[i])^.ActionTime) then
        begin
            ActionList.Insert(i, pAction);
            ActionInserted := TRUE;
            Break;
        end;
    end;

    if not ActionInserted then
        ActionList.Add(pAction);

    with pAction^ do
    begin
        ActionTime := Trec;
        ActionCode := Code;
        ActionHandle := ctrlHandle;
        ProxyHandle := ProxyHdl;
        ControlElement := Owner;
    end;

    Result := ctrlHandle;

    if (DebugTrace) then
        WriteTraceRecord(Owner.Name, Code, Owner.DblTraceParameter,
            Format('Handle %d Pushed onto Stack', [ctrlHandle]));
end;


procedure TControlQueue.Clear;
var
    i: Integer;
begin
    with ActionList do  {Free Allocated memory}
        for i := 0 to Count - 1 do
            Freemem(ActionList.Items[i], Sizeof(TActionRecord));

    ActionList.Clear;
end;

constructor TControlQueue.Create;
begin
    inherited Create;
    ActionList := TList.Create;
    ActionList.Clear;

    ctrlHandle := 0;

    DebugTrace := FALSE;
end;

destructor TControlQueue.Destroy;
begin
    Clear;
    ActionList.Free;
    inherited Destroy;
end;

procedure TControlQueue.DoAllActions;

var
    i: Integer;

begin
    with ActionList do
        for i := 0 to Count - 1 do
            with pActionRecord(Items[i])^ do
                ControlElement.DoPendingAction(ActionCode, ProxyHandle);
    Clear;
end;

function TControlQueue.DoNearestActions(var Hour: Integer; var Sec: Double): Boolean;

// Do only those actions with the same delay time as the first action time
// Return time

var
    pElem: TControlElem;
    t: TTimeRec;
    Code,
    hdl,
    ProxyHdl: Integer;

begin
    Result := FALSE;
    with ActionList do
        if Count > 0 then
        begin
            t := pActionRecord(Items[0])^.ActionTime;
            Hour := t.Hour;
            Sec := t.Sec;
            pElem := Pop(t, Code, ProxyHdl, hdl);
            while pElem <> NIL do
            begin
                if DebugTrace then
                    WriteTraceRecord(pElem.Name, Code, pElem.DblTraceParameter, Format('Pop Handle %d Do Nearest Action', [hdl]));
                pElem.DoPendingAction(Code, ProxyHdl);
                Result := TRUE;
                pElem := Pop(t, Code, ProxyHdl, hdl);
            end;
        end;
end;

function TControlQueue.IsEmpty: Boolean;
begin
    if ActionList.Count = 0 then
        Result := TRUE
    else
        Result := FALSE;
end;


function TControlQueue.Pop(const ActionTime: TTimeRec; var Code, ProxyHdl, Hdl: Integer): TControlElem;
 // pop off next control action with an action time <= ActionTime (sec)

var
    i: Integer;
    t: Double;

begin
    Result := NIL;
    t := TimeRecToTime(ActionTime);

    with ActionList do
        for i := 0 to Count - 1 do
        begin
            with pActionRecord(Items[i])^ do
                if TimeRecToTime(ActionTime) <= t then
                begin
                    Result := ControlElement;
                    Code := ActionCode;
                    ProxyHdl := ProxyHandle;
                    Hdl := ActionHandle;
                    DeleteFromQueue(i, TRUE);
                    Break;
                end;
        end;
end;

function TControlQueue.Pop_Time(const ActionTime: TTimeRec; var Code, ProxyHdl, Hdl: Integer; var ATime: Double; keepIn: Boolean): TControlElem;  // Pop action from queue <= given time
 // pop off next control action with an action time <= ActionTime (sec)

var
    i: Integer;
    t: Double;

begin
    Result := NIL;
    t := TimeRecToTime(ActionTime);

    with ActionList do
        for i := 0 to Count - 1 do
        begin
            with pActionRecord(Items[i])^ do
                if TimeRecToTime(ActionTime) <= t then
                begin
                    Result := ControlElement;
                    Code := ActionCode;
                    ProxyHdl := ProxyHandle;
                    Hdl := ActionHandle;
                    ATime := TimeRecToTime(ActionTime);
                    if not keepIn then
                        DeleteFromQueue(i, TRUE);
                    Break;
                end;
        end;
end;

procedure TControlQueue.DeleteFromQueue(i: Integer; popped: Boolean);
// Delete i-th element from the Queue
var
    pElem: TControlElem;
    S: String;

begin
    with pActionRecord(ActionList.Items[i])^ do
    begin
        pElem := ControlElement;
        if (DebugTrace) then
        begin
            if Popped then
                S := 'by Pop function'
            else
                S := 'by control device';
            WriteTraceRecord(pElem.Name, ActionCode, pelem.dbltraceParameter,
                Format('Handle %d deleted from Queue %s', [ActionHandle, S]));
        end;
    end;

    Freemem(ActionList.Items[i], Sizeof(TActionRecord));
    ActionList.Delete(i);

end;

function TControlQueue.DoActions(const Hour: Integer; const sec: Double): Boolean;

// Do all actions having an action time <= t

var
    pElem: TControlElem;
    t: TTimeRec;
    Code,
    hdl,
    ProxyHdl: Integer;

begin
    Result := FALSE;
    if ActionList.Count > 0 then
    begin

        t.Hour := Hour;
        t.Sec := Sec;
        pElem := Pop(t, Code, ProxyHdl, hdl);
        while pElem <> NIL do
        begin
            if (DebugTrace) then
                WriteTraceRecord(pElem.Name, Code, pelem.dbltraceParameter, Format('Pop Handle %d Do Action', [Hdl]));
            pElem.DoPendingAction(code, ProxyHdl);
            Result := TRUE;
            pElem := Pop(t, Code, ProxyHdl, hdl);
        end;
    end;

end;

function TControlQueue.DoMultiRate(const Hour: Integer; const sec: Double): Boolean;

// Do all actions having an action time <= t and solves the circuit after each control action

var
    pElem: TControlElem;
    Code,
    hdl,
    ProxyHdl,
    Idx: Integer;

begin
    Result := FALSE;
    for Idx := 0 to 1 do
        Temp_Int[Idx] := 0;    // Temporary register for hour
    for Idx := 0 to 3 do
        Temp_dbl[Idx] := 0.0;
{  Temp_dbl[0]  Temporary register for the secs
   Temp_dbl[1]  Temporary register for Time accumulator
   Temp_dbl[2]  Temporary register for Time upper boundary
   Temp_dbl[3]  Temporary register for the control action time }
    if ActionList.Count > 0 then
    begin
        Ltimer.Hour := Hour;
        Ltimer.Sec := Sec;
        Temp_dbl[4] := ActiveCircuit.solution.DynaVars.h;                        // Simulation step time (Time window size)
        Temp_dbl[6] := TimeRecToTime(Ltimer);                                    // Simulation step time incremental
        pElem := Pop_Time(Ltimer, Code, ProxyHdl, hdl, Temp_dbl[3], FALSE);
        while pElem <> NIL do
        begin
            if (DebugTrace) then
                WriteTraceRecord(pElem.Name, Code, pelem.dbltraceParameter, Format('Pop Handle %d Do Action', [Hdl]));
            pElem.DoPendingAction(code, ProxyHdl);
            Result := TRUE;
            pElem := Pop_Time(Ltimer, Code, ProxyHdl, hdl, Temp_dbl[3], FALSE);
        end;
//**************After this point, the additional control actions are performed************
        Temp_dbl[7] := ActiveCircuit.solution.DynaVars.t;                        // Saving the current time (secs)
        with ActiveCircuit.solution.DynaVars do
            Temp_Int[2] := intHour;          // Saving the current time (hour)
        Temp_dbl[2] := Temp_dbl[6];
//*************** Simulation time is recalculated considering the next control action event ************
        Recalc_Time_Step;
        pElem := Pop_Time(Ltimer, Code, ProxyHdl, hdl, Temp_dbl[3], TRUE);         // Downloads the next CtrlAction without
        while pElem <> NIL do                                                      // removing it from the Queue
        begin
            while Temp_Dbl[3] >= 3600.0 do
                Temp_dbl[3] := Temp_dbl[3] - 3600.0;    // CtrlAction Time is adjusted
            Temp_dbl[5] := (Temp_dbl[3] - Temp_dbl[6]) + Temp_dbl[1];              // Recalculates the CtrlAction occurrence time
            if Temp_dbl[5] < Temp_dbl[4] then                                       // Checks if the CtrlAction is within the
            begin                                                                   // time window
                pElem := Pop_Time(Ltimer, Code, ProxyHdl, hdl, Temp_dbl[3], FALSE);  // Removes the CtrlAction from The Queue
                if (DebugTrace) then
                    WriteTraceRecord(pElem.Name, Code, pelem.dbltraceParameter, Format('Pop Handle %d Do Action', [Hdl]));
                pElem.DoPendingAction(code, ProxyHdl);
                pElem := Pop_Time(Ltimer, Code, ProxyHdl, hdl, Temp_dbl[3], TRUE);   // Downloads the next CtrlAction without
            end                                                                     // removing it from the Queue
            else
            begin
                pElem.DoPendingAction(code, ProxyHdl);                               // Executes the CtrlAction
                pElem := NIL;                                                  // The next CtrlAction is outside the time window
                Temp_Int[1] := 1;                                                    // Preparing everything to exit
            end;
            if (pElem = NIL) and (Temp_Int[1] = 0) then                             // The last CtrlAction was within the time
            begin                                                                   // Time window, keep scanning
                with ActiveCircuit.Solution do
                begin
                    Temp_dbl[1] := Temp_dbl[1] + (Temp_dbl[3] - Temp_dbl[6]);         // The Accumulated time is calculated
                    Temp_dbl[6] := Temp_dbl[6] + Temp_dbl[4];                         // Time reference moves forward
                    while Temp_Dbl[6] >= 3600.0 do
                        Temp_dbl[6] := Temp_dbl[6] - 3600.0;// Time reference is adjusted
//******************** Updates the circuit after applying the control actions **************************
                    SolveCircuit;
                    Restore_Time_Step;                                                  // Restores Time for sampling devices
                    SampleControlDevices;
                    Recalc_Time_Step;                                                   // Recalculating Time for next iteration
                    pElem := Pop_Time(Ltimer, Code, ProxyHdl, hdl, Temp_dbl[3], TRUE);  // Downloads the next CtrlAction without
                end;                                                                  // removing it from the Queue
            end;
        end;
        Restore_Time_Step;                                                         // Restores Time to keep going with the simulation
    end;
end;

procedure TControlQueue.Recalc_Time_Step;
begin
    Temp_dbl[2] := Temp_dbl[2] + Temp_dbl[4];                                     // Time window moves forward
    while Temp_Dbl[2] >= 3600.0 do                                                  // Adjusts the window
    begin
        Inc(Temp_Int[0]);
        Temp_dbl[2] := Temp_dbl[2] - 3600.0;
    end;
    Ltimer.Hour := Temp_Int[0];
    Ltimer.sec := Temp_dbl[2];
    with ActiveCircuit.solution.DynaVars do
        intHour := Temp_Int[0];               // Sets the simulation time
    ActiveCircuit.solution.DynaVars.t := Temp_dbl[2];
    ActiveCircuit.solution.Update_dblHour;
end;

procedure TControlQueue.Restore_Time_Step;
begin
    with ActiveCircuit.solution.DynaVars do
        intHour := Temp_Int[2];
    ActiveCircuit.solution.DynaVars.t := Temp_dbl[7];
    ActiveCircuit.solution.Update_dblHour;
end;

function TControlQueue.TimeRecToTime(Trec: TTimeRec): Double;
begin
    with Trec do
        Result := Hour * 3600.0 + Sec
end;

procedure TControlQueue.Set_Trace(const Value: Boolean);
begin

    DebugTrace := Value;

    if DebugTrace then
    begin
        AssignFile(TraceFile, GetOutputDirectory + 'Trace_ControlQueue.CSV');
        ReWrite(TraceFile);
        Writeln(TraceFile, '"Hour", "sec", "Control Iteration", "Element", "Action Code", "Trace Parameter", "Description"');
        CloseFile(Tracefile);
    end;

end;

procedure TControlQueue.ShowQueue(const Filenm: String);
var
    F: TextFile;
    i: Integer;
    pAction: pActionRecord;

begin
    try
        Assignfile(F, FileNm);
        ReWrite(F);

        Writeln(F, 'Handle, Hour, Sec, ActionCode, ProxyDevRef, Device');

        for i := 0 to ActionList.Count - 1 do
        begin
            pAction := ActionList.Items[i];
            if pAction <> NIL then
                with Paction^ do
                begin
                    Writeln(F, Format('%d, %d, %-.g, %d, %d, %s ',
                        [ActionHandle, ActionTime.Hour, ActionTime.sec, ActionCode, ProxyHandle, ControlElement.Name]));
                end;
        end;
    finally
        CloseFile(F);
        FireOffEditor(FileNm);
    end;


end;

procedure TControlQueue.WriteTraceRecord(const ElementName: String; const Code: Integer; TraceParameter: Double; const s: String);

begin

    try
        if (not InshowResults) then
        begin
            Append(TraceFile);
            Writeln(TraceFile, Format('%d, %.6g, %d, %s, %d, %-.g, %s', [
                ActiveCircuit.Solution.DynaVars.intHour,
                ActiveCircuit.Solution.DynaVars.t,
                ActiveCircuit.Solution.ControlIteration,
                ElementName,
                Code,
                TraceParameter,
                S]));

            CloseFile(TraceFile);
        end;

    except
        On E: Exception do
        begin
        end;

    end;

end;

procedure TControlQueue.Delete(Hdl: Integer);

{Delete an item by its Handle reference}

var
    i: Integer;
begin
    with ActionList do
        for i := 0 to Count - 1 do
        begin
            if pActionRecord(Items[i])^.ActionHandle = Hdl then
            begin
                DeleteFromQueue(i, FALSE);
                Exit;
            end;
        end;
end;

function TControlQueue.Get_QueueSize: Integer;
begin
    Result := ActionList.Count;
end;

function TControlQueue.QueueItem(Qidx: Integer): String;
var
    pAction: pActionRecord;
begin
    pAction := ActionList.Items[Qidx];
    if pAction <> NIL then
        with Paction^ do
        begin
            Result := Format('%d, %d, %-.g, %d, %d, %s ',
                [ActionHandle, ActionTime.Hour, ActionTime.sec, ActionCode, ProxyHandle, ControlElement.Name]);
        end
    else
        Result := '';
end;

end.
