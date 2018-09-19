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

Uses Arraydef, ControlElem, Classes;

Type

    TTimeRec = RECORD
          Hour :Integer;
          Sec  :Double;
    END;

    pActionRecord = ^TActionRecord;
    TActionRecord = Record
        ActionTime    :TTimeRec;
        ActionCode    :Integer;
        ActionHandle  :Integer;
        ProxyHandle   :Integer;
        ControlElement:TControlElem;
    End;

    TControlQueue = class(Tobject)
    private
       ActionList :TList;
       DebugTrace :Boolean;
       Tracefile  :TextFile;
       ctrlHandle :Integer;
       Temp_Int     : Array[0..3] of Integer; // Temporary registers, Int Type
       Temp_dbl     : Array[0..7] of double;  // Temporary registers, dbl type
       Ltimer       : TTimeRec;

       FUNCTION  Pop(const ActionTime:TTimeRec; Var Code, ProxyHdl, Hdl:Integer; ActorID: Integer): TControlElem;  // Pop action from queue <= given time
       FUNCTION  Pop_Time(const ActionTime:TTimeRec; Var Code, ProxyHdl, Hdl:Integer; var ATime : Double; KeepIn : Boolean; ActorID : Integer): TControlElem;  // Pop action from queue <= given time
       PROCEDURE DeleteFromQueue(i: Integer; popped:Boolean; ActorID: Integer);
       FUNCTION  TimeRecToTime(Trec:TTimeRec):Double;
       PROCEDURE Set_Trace(const Value: Boolean);
       PROCEDURE WriteTraceRecord(const ElementName: String;const Code:Integer; TraceParameter:Double;const s:String; ActorID : Integer);
       FUNCTION Get_QueueSize:Integer;
       PROCEDURE Recalc_Time_Step(ActorID: Integer);
       PROCEDURE Restore_Time_Step(ActorID: Integer);


    public
      constructor Create;
      destructor Destroy; override;

      FUNCTION  Push(Const Hour:Integer; Const Sec:Double; Const Code, ProxyHdl:Integer; Const Owner:TControlElem; ActorID : Integer):Integer; overload;
      FUNCTION  Push(Const Hour:Integer; Const Sec:Double; Const Code:EControlAction; Const ProxyHdl:Integer; Const Owner:TControlElem; ActorID : Integer):Integer; overload;
      PROCEDURE Clear;
      PROCEDURE DoAllActions(ActorID : Integer);
      FUNCTION  DoNearestActions(VAR Hour:Integer; VAR Sec:Double; ActorID : Integer):Boolean;  // Do only actions with lowest time
      FUNCTION  DoActions(const Hour:Integer; const sec: Double; ActorID : Integer):Boolean;  // Do actions with time <= t
      FUNCTION  DoMultiRate(const Hour:Integer; const sec: Double; ActorID : Integer):Boolean;  // Do actions with time <= t

      FUNCTION  IsEmpty:Boolean;
      PROCEDURE Delete(Hdl:Integer; ActorID : Integer);  // Delete queue item by handle

      PROCEDURE ShowQueue(Const Filenm:String);

      Property  TraceLog:Boolean  Read DebugTrace Write Set_Trace;
      Property  QueueSize:Integer Read Get_QueueSize;
      function QueueItem(Qidx:integer):String;


    End;


implementation

Uses DSSGlobals, sysutils, Utilities;

{ TControlQueue }

Function TControlQueue.Push(Const Hour:Integer; const Sec: Double; Const code:EControlAction; Const ProxyHdl:Integer; const Owner: TControlElem; ActorID : Integer):Integer;
begin
  Result := Push (Hour, Sec, Integer(code), ProxyHdl, Owner, ActorID);
end;

Function TControlQueue.Push(Const Hour:Integer; const Sec: Double; Const code, ProxyHdl:Integer;   const Owner: TControlElem; ActorID: Integer):Integer;

{Add a control action to the queue, sorted by lowest time first}
{Returns handle to the action}

VAR
   i,
   Hr         :Integer;
   ThisActionTime,
   S          :Double;
   Trec       :TTimeRec;
   pAction    :pActionRecord;
   ActionInserted :Boolean;

Begin


     Inc(ctrlHandle); // just a serial number

     {Normalize the time }
     Hr := Hour;
     S  := Sec;
     If S > 3600.0
     THEN REPEAT
           Hr := Hr +1;
           S := S - 3600.0;
     UNTIL S < 3600.0;

     Trec.Hour := Hr;
     Trec.Sec  := S;

     ThisActionTime := TimeRecToTime(Trec);
     pAction := Allocmem(Sizeof(TActionRecord));  // Make a new Action

     {Insert the action in the list in order of time}
     ActionInserted := FALSE;
     FOR i := 0 to ActionList.Count-1 Do
       Begin
           If ThisActionTime <= TimeRecToTime(pActionRecord(ActionList.Items[i])^.ActionTime)
           THEN Begin
               ActionList.Insert(i, pAction);
               ActionInserted := TRUE;
               Break;
           End;
       End;

     If Not ActionInserted then  ActionList.Add(pAction);
     
     With pAction^ Do Begin
       ActionTime     := Trec;
       ActionCode     := Code;
       ActionHandle   := ctrlHandle;
       ProxyHandle    := ProxyHdl;
       ControlElement := Owner;
     End;

     Result := ctrlHandle;

     IF (DebugTrace)  THEN WriteTraceRecord(Owner.Name, Code, Owner.DblTraceParameter,
                               Format('Handle %d Pushed onto Stack',[ctrlHandle]), ActorID);
End;


PROCEDURE TControlQueue.Clear;
VAR
   i:Integer;
Begin
    With ActionList Do  {Free Allocated memory}
      For i := 0 to Count-1 do
        Freemem(ActionList.Items[i], Sizeof(TActionRecord));

    ActionList.Clear;
End;

constructor TControlQueue.Create;
Begin
     Inherited Create;
     ActionList := TList.Create;
     ActionList.Clear;

     ctrlHandle:=0;

     DebugTrace := FALSE;
End;

destructor TControlQueue.Destroy;
Begin
   Clear;
   ActionList.Free;
   Inherited Destroy;
End;

PROCEDURE TControlQueue.DoAllActions(ActorID : Integer);

VAR
   i:Integer;

Begin
    With ActionList Do
     FOR i := 0 to Count-1 Do
       With pActionRecord(Items[i])^ Do
          ControlElement.DoPendingAction(ActionCode, ProxyHandle, ActorID);
     Clear;
End;

FUNCTION TControlQueue.DoNearestActions( VAR Hour:Integer; VAR Sec:Double; ActorID : Integer):Boolean;

// Do only those actions with the same delay time as the first action time
// Return time

VAR
   pElem      :TControlElem;
   t          :TTimeRec;
   Code,
   hdl,
   ProxyHdl        :Integer;

Begin
   Result := FALSE;
   With ActionList Do
   IF Count > 0 THEN Begin
       t := pActionRecord(Items[0])^.ActionTime;
       Hour := t.Hour;
       Sec  := t.Sec;
       pElem := Pop(t, Code, ProxyHdl, hdl, ActorID);
       While pElem <> NIL Do
       Begin
           IF DebugTrace Then WriteTraceRecord(pElem.Name, Code, pElem.DblTraceParameter, Format('Pop Handle %d Do Nearest Action',[hdl]),ActorID);
           pElem.DoPendingAction(Code, ProxyHdl, ActorID);
           Result := TRUE;
           pElem := Pop(t, Code, ProxyHdl, hdl, ActorID);
       End;
   End;
End;

function TControlQueue.IsEmpty: Boolean;
begin
     IF ActionList.Count = 0
     THEN Result := True
     ELSE Result := False;
end;


FUNCTION TControlQueue.Pop(const ActionTime: TTimeRec; Var Code, ProxyHdl, Hdl:Integer; ActorID : Integer): TControlElem;
 // pop off next control action with an action time <= ActionTime (sec)

VAR
   i    :Integer;
   t    :Double;

Begin
      Result := NIL;
      t := TimeRecToTime(ActionTime);

      With ActionList Do
      FOR i := 0 to Count-1 Do
      Begin
          With pActionRecord(Items[i])^ Do
          IF TimeRecToTime(ActionTime) <= t
          THEN Begin
              Result   :=  ControlElement;
              Code     := ActionCode;
              ProxyHdl := ProxyHandle;
              Hdl      := ActionHandle;
              DeleteFromQueue(i, TRUE, ActorID);
              Break;
          End;
      End;
End;

FUNCTION  TControlQueue.Pop_Time(const ActionTime:TTimeRec; Var Code, ProxyHdl, Hdl:Integer; var ATime : Double; keepIn : boolean; ActorID : Integer): TControlElem;  // Pop action from queue <= given time
 // pop off next control action with an action time <= ActionTime (sec)

VAR
   i        :Integer;
   t        :Double;

Begin
      Result := NIL;
      t := TimeRecToTime(ActionTime);

      With ActionList Do
      FOR i := 0 to Count-1 Do
      Begin
          With pActionRecord(Items[i])^ Do
          IF TimeRecToTime(ActionTime) <= t
          THEN Begin
              Result   :=  ControlElement;
              Code     := ActionCode;
              ProxyHdl := ProxyHandle;
              Hdl      := ActionHandle;
              ATime    := TimeRecToTime(ActionTime);
              if not keepIn then  DeleteFromQueue(i, TRUE, ActorID);
              Break;
          End;
      End;
End;

PROCEDURE TControlQueue.DeleteFromQueue(i: Integer; popped:Boolean; ActorID : Integer);
// Delete i-th element from the Queue
VAR
   pElem     :TControlElem;
   S         :String;

Begin
     With pActionRecord(ActionList.Items[i])^ Do Begin
       pElem := ControlElement;
       IF (DebugTrace)  THEN Begin
             If Popped Then S := 'by Pop function' Else S := 'by control device' ;
             WriteTraceRecord(pElem.Name, ActionCode, pelem.dbltraceParameter,
                             Format('Handle %d deleted from Queue %s',[ActionHandle, S]), ActorID);
       End;
     End;

     Freemem(ActionList.Items[i], Sizeof(TActionRecord));
     ActionList.Delete(i);

End;

FUNCTION TControlQueue.DoActions(const Hour:Integer; const sec: Double; ActorID : Integer):Boolean;

// Do all actions having an action time <= t

VAR
   pElem     :TControlElem;
   t         :TTimeRec;
   Code,
   hdl,
   ProxyHdl      :Integer;

Begin
   Result := FALSE;
   IF ActionList.Count > 0
   THEN Begin

       t.Hour := Hour;
       t.Sec  := Sec;
       pElem := Pop(t, Code, ProxyHdl, hdl, ActorID);
       While pElem <> NIL Do
       Begin
           IF (DebugTrace)  THEN WriteTraceRecord(pElem.Name, Code, pelem.dbltraceParameter, Format('Pop Handle %d Do Action',[Hdl]), ActorID);
           pElem.DoPendingAction(code, ProxyHdl, ActorID);
           Result := TRUE;
           pElem := Pop(t, Code, ProxyHdl, hdl, ActorID);
       End;
   End;

end;

FUNCTION TControlQueue.DoMultiRate(const Hour:Integer; const sec: Double; ActorID : Integer):Boolean;

// Do all actions having an action time <= t and solves the circuit after each control action

VAR
   pElem        : TControlElem;
   Code,
   hdl,
   ProxyHdl,
   Idx          : Integer;

Begin
   Result       :=  FALSE;
   for Idx := 0 to 1 do Temp_Int[Idx]  :=  0;    // Temporary register for hour
   for Idx := 0 to 3 do Temp_dbl[Idx]  :=  0.0;
{  Temp_dbl[0]  Temporary register for the secs
   Temp_dbl[1]  Temporary register for Time accumulator
   Temp_dbl[2]  Temporary register for Time upper boundary
   Temp_dbl[3]  Temporary register for the control action time }
   IF ActionList.Count > 0
   THEN Begin
       Ltimer.Hour  := Hour;
       Ltimer.Sec   := Sec;
       Temp_dbl[4]  :=  ActiveCircuit[ActorID].solution.DynaVars.h;                        // Simulation step time (Time window size)
       Temp_dbl[6]  :=  TimeRecToTime(Ltimer);                                    // Simulation step time incremental
       pElem        :=  Pop_Time(Ltimer, Code, ProxyHdl, hdl, Temp_dbl[3], FALSE, ActorID);
       While pElem <> NIL Do
       Begin
           IF (DebugTrace)  THEN WriteTraceRecord(pElem.Name, Code, pelem.dbltraceParameter, Format('Pop Handle %d Do Action',[Hdl]), ActorID);
           pElem.DoPendingAction(code, ProxyHdl, ActorID);
           Result     :=  TRUE;
           pElem      :=  Pop_Time(Ltimer, Code, ProxyHdl, hdl, Temp_dbl[3], FALSE, ActorID);
       End;
//**************After this point, the additional control actions are performed************
       Temp_dbl[7]  :=  ActiveCircuit[ActorID].solution.DynaVars.t;                        // Saving the current time (secs)
       With ActiveCircuit[ActorID].solution.DynaVars do Temp_Int[2]  :=  intHour;          // Saving the current time (hour)
       Temp_dbl[2]  :=  Temp_dbl[6];
//*************** Simulation time is recalculated considering the next control action event ************
       Recalc_Time_Step(ActorID);
       pElem := Pop_Time(Ltimer, Code, ProxyHdl, hdl, Temp_dbl[3], TRUE, ActorID);         // Downloads the next CtrlAction without
       while pElem <> nil do                                                      // removing it from the Queue
       begin
          while Temp_Dbl[3] >= 3600.0 do Temp_dbl[3]  := Temp_dbl[3] - 3600.0;    // CtrlAction Time is adjusted
          Temp_dbl[5] :=  (Temp_dbl[3] - Temp_dbl[6]) + Temp_dbl[1];              // Recalculates the CtrlAction occurrence time
          if Temp_dbl[5] < Temp_dbl[4] then                                       // Checks if the CtrlAction is within the
          begin                                                                   // time window
             pElem := Pop_Time(Ltimer, Code, ProxyHdl, hdl, Temp_dbl[3], FALSE, ActorID);  // Removes the CtrlAction from The Queue
             IF (DebugTrace)  THEN WriteTraceRecord(pElem.Name, Code, pelem.dbltraceParameter, Format('Pop Handle %d Do Action',[Hdl]), ActorID);
             pElem.DoPendingAction(code, ProxyHdl, ActorID);
             pElem := Pop_Time(Ltimer, Code, ProxyHdl, hdl, Temp_dbl[3], TRUE, ActorID);   // Downloads the next CtrlAction without
          end                                                                     // removing it from the Queue
          else
          begin
            pElem.DoPendingAction(code, ProxyHdl, ActorID);                               // Executes the CtrlAction
            pElem       :=  nil;                                                  // The next CtrlAction is outside the time window
            Temp_Int[1] :=  1;                                                    // Preparing everything to exit
          end;
          if (pElem = nil) and (Temp_Int[1] = 0) then                             // The last CtrlAction was within the time
          begin                                                                   // Time window, keep scanning
            with ActiveCircuit[ActorID].Solution do
            Begin
              Temp_dbl[1]  :=  Temp_dbl[1] + (Temp_dbl[3] - Temp_dbl[6]);         // The Accumulated time is calculated
              Temp_dbl[6]  :=  Temp_dbl[6] + Temp_dbl[4];                         // Time reference moves forward
              while Temp_Dbl[6] >= 3600.0 do Temp_dbl[6]  := Temp_dbl[6] - 3600.0;// Time reference is adjusted
//******************** Updates the circuit after applying the control actions **************************
              SolveCircuit(ActorID);
              Restore_Time_Step(ActorID);                                                  // Restores Time for sampling devices
              SampleControlDevices(ActorID);
              Recalc_Time_Step(ActorID);                                                   // Recalculating Time for next iteration
              pElem := Pop_Time(Ltimer, Code, ProxyHdl, hdl, Temp_dbl[3], TRUE, ActorID);  // Downloads the next CtrlAction without
            end;                                                                  // removing it from the Queue
          end;
       end;
       Restore_Time_Step(ActorID);                                                         // Restores Time to keep going with the simulation
   End;
end;

PROCEDURE TControlQueue.Recalc_Time_Step(ActorID: Integer);
Begin
  Temp_dbl[2]  :=  Temp_dbl[2] + Temp_dbl[4];                                     // Time window moves forward
  while Temp_Dbl[2] >= 3600.0 do                                                  // Adjusts the window
  Begin
    Inc(Temp_Int[0]);
    Temp_dbl[2]  := Temp_dbl[2] - 3600.0;
  End;
  Ltimer.Hour  :=  Temp_Int[0];
  Ltimer.sec   :=  Temp_dbl[2];
  With ActiveCircuit[ActorID].solution.DynaVars do intHour  :=  Temp_Int[0];               // Sets the simulation time
  ActiveCircuit[ActorID].solution.DynaVars.t  :=  Temp_dbl[2];
  ActiveCircuit[ActorID].solution.Update_dblHour;
End;

PROCEDURE TControlQueue.Restore_Time_Step(ActorID : Integer);
Begin
  With ActiveCircuit[ActorID].solution.DynaVars do intHour  :=  Temp_Int[2];
  ActiveCircuit[ActorID].solution.DynaVars.t  :=  Temp_dbl[7];
  ActiveCircuit[ActorID].solution.Update_dblHour;
End;

FUNCTION TControlQueue.TimeRecToTime(Trec: TTimeRec): Double;
begin
     With Trec Do Result := Hour * 3600.0 + Sec
end;

PROCEDURE TControlQueue.Set_Trace(const Value: Boolean);
begin

     DebugTrace := Value;

     If DebugTrace
     THEN Begin
          AssignFile(TraceFile, GetOutputDirectory + 'Trace_ControlQueue.CSV');
          ReWrite(TraceFile);
          Writeln(TraceFile, '"Hour", "sec", "Control Iteration", "Element", "Action Code", "Trace Parameter", "Description"');
          CloseFile(Tracefile);
     End;

end;

procedure TControlQueue.ShowQueue(const Filenm: String);
Var
   F:TextFile;
   i:Integer;
   pAction:pActionRecord;

begin
  Try
    Assignfile(F,FileNm);
    ReWrite(F);

    Writeln(F,'Handle, Hour, Sec, ActionCode, ProxyDevRef, Device');

    For i := 0 to ActionList.Count-1 do Begin
        pAction := ActionList.Items[i];
        If pAction<>Nil then With Paction^ Do Begin
           Writeln(F, Format('%d, %d, %-.g, %d, %d, %s ',
           [ActionHandle, ActionTime.Hour, ActionTime.sec, ActionCode, ProxyHandle, ControlElement.Name  ]));
        End;
    End;
  Finally
    CloseFile(F);
    FireOffEditor(FileNm);
  End;


end;

PROCEDURE TControlQueue.WriteTraceRecord(const ElementName: String;const Code:Integer; TraceParameter:Double;const s:String; ActorID : Integer);

Begin

      Try
        IF (Not InshowResults)
        THEN Begin
             Append(TraceFile);
             Writeln(TraceFile, Format('%d, %.6g, %d, %s, %d, %-.g, %s', [
                      ActiveCircuit[ActiveActor].Solution.DynaVars.intHour,
                      ActiveCircuit[ActiveActor].Solution.DynaVars.t,
                      ActiveCircuit[ActiveActor].Solution.ControlIteration,
                      ElementName,
                      Code,
                      TraceParameter,
                      S ]));

             CloseFile(TraceFile);
        End;

      Except
            On E:Exception Do Begin End;

      End;

end;

PROCEDURE TControlQueue.Delete(Hdl: Integer; ActorID : Integer);

{Delete an item by its Handle reference}

Var
   i:Integer;
begin
     With ActionList Do
     For i := 0 to Count-1 Do Begin
         IF pActionRecord(Items[i])^.ActionHandle = Hdl THEN  Begin
              DeleteFromQueue(i, FALSE, ActorID);
              Exit;
            End;
     End;
end;

FUNCTION TControlQueue.Get_QueueSize:Integer;
Begin
     Result := ActionList.Count;
End;

FUNCTION TControlQueue.QueueItem(Qidx : integer):String;
Var
     pAction:pActionRecord;
Begin
     pAction := ActionList.Items[Qidx];
     If pAction<>Nil then With Paction^ Do Begin
       Result  := Format('%d, %d, %-.g, %d, %d, %s ',
       [ActionHandle, ActionTime.Hour, ActionTime.sec, ActionCode, ProxyHandle, ControlElement.Name]);
     End
     else
        Result  :=  '';
End;

end.
