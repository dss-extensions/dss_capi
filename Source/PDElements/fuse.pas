unit Fuse;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
    Created 11-1-00 from Recloser Control
}
{
  A Fuse is a control element that is connected to a terminal of a
  circuit element and controls the switches in the same or another terminal.

  The control is usually placed in the
  terminal of a line or transformer, but it could be any element

  CktElement to be controlled must already exist.

}

INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex,
      utilities, TCC_Curve, Math;

CONST
      FUSEMAXDIM = 6;

TYPE

   pStateArray = ^StateArray;
   StateArray = Array[1..FUSEMAXDIM] of EControlAction;  // 0 = open 1 = close

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TFuse = class(TControlClass)
     private

     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const FuseName:String):Integer; override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit(ActorID : Integer):Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;
       
   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TFuseObj = class(TControlElem)
     private

            MonitoredElement	 : TDSSCktElement;

            hAction            : Array[1..FUSEMAXDIM] of Integer;         // handle to control queue actions

            FPresentState      : pStateArray;
            FNormalState       : pStateArray;

            ReadyToBlow        : Array[1..FUSEMAXDIM] of Boolean;

            CondOffset         : Integer; // Offset for monitored terminal
            cBuffer            : pComplexArray;    // Complexarray buffer

            NormalStateSet: Boolean;

            PROCEDURE InterpretFuseState(ActorID : Integer; const param: string; const property_name: string);
            FUNCTION  get_States(Idx: Integer): EControlAction;
            PROCEDURE set_States(Idx: Integer; const Value: EControlAction);
            FUNCTION  get_NormalStates(Idx: Integer): EControlAction;
            PROCEDURE set_NormalStates(Idx: Integer; const Value: EControlAction);


     public

            FuseCurve     : TTCC_CurveObj;
            RatedCurrent  : Double;
            DelayTime     : Double;

            MonitoredElementName     :String;
            MonitoredElementTerminal :Integer;

       constructor Create(ParClass:TDSSClass; const FuseName:String);
       destructor Destroy; override;

       PROCEDURE RecalcElementData(ActorID : Integer); Override;
       PROCEDURE CalcYPrim(ActorID : Integer); Override;    // Always Zero for a Fuse

       PROCEDURE Sample(ActorID : Integer);  Override;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Phs, ProxyHdl:Integer; ActorID : Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset(ActorID : Integer); Override;  // Reset to initial defined state

       PROCEDURE GetCurrents(Curr: pComplexArray; ActorID : Integer); Override; // Get present value of terminal Curr
       PROCEDURE GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override;   // Returns Injextion currents

       FUNCTION  GetPropertyValue(Index:Integer):String;Override;
       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

       Property States[Idx:Integer]:EControlAction Read get_States write set_States;
       Property NormalStates[Idx:Integer]:EControlAction Read get_NormalStates write set_NormalStates;

   end;

VAR
    ActiveFuseObj : TFuseObj;
    FuseClass     : TFuse;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit,   Sysutils, uCmatrix, MathUtil;

CONST

    NumPropsThisClass = 10;

VAR
   TCC_CurveClass:TDSSClass;

{General Module Function}

Function GetTccCurve(Const CurveName:String):TTCC_CurveObj;

Begin

     Result := TCC_CurveClass.Find(CurveName);

     IF Result = NIL
     THEN DoSimpleMsg('TCC Curve object: "'+CurveName+'" not found.', 401);

End;


{--------------------------------------------------------------------------}
constructor TFuse.Create;  // Creates superstructure for all Fuse objects
Begin
     Inherited Create;

     Class_name   := 'Fuse';
     DSSClassType := DSSClassType + FUSE_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;

     TCC_CurveClass := GetDSSClassPtr('TCC_Curve');
     FuseClass := Self;
End;

{--------------------------------------------------------------------------}
destructor TFuse.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TFuse.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName[1]  := 'MonitoredObj';
     PropertyName[2]  := 'MonitoredTerm';
     PropertyName[3]  := 'SwitchedObj';
     PropertyName[4]  := 'SwitchedTerm';
     PropertyName[5]  := 'FuseCurve';
     PropertyName[6]  := 'RatedCurrent';
     PropertyName[7]  := 'Delay';
     PropertyName[8]  := 'Action';
     PropertyName[9]  := 'Normal';
     PropertyName[10] := 'State';

     PropertyHelp[1] :=  'Full object name of the circuit element, typically a line, transformer, load, or generator, '+
                         'to which the Fuse is connected.' +
                         ' This is the "monitored" element. ' +
                         'There is no default; must be specified.';
     PropertyHelp[2] :=  'Number of the terminal of the circuit element to which the Fuse is connected. '+
                         '1 or 2, typically.  Default is 1.';
     PropertyHelp[3] :=  'Name of circuit element switch that the Fuse controls. '+
                         'Specify the full object name.' +
                         'Defaults to the same as the Monitored element. '+
                         'This is the "controlled" element.';
     PropertyHelp[4] :=  'Number of the terminal of the controlled element in which the switch is controlled by the Fuse. '+
                         '1 or 2, typically.  Default is 1.  Assumes all phases of the element have a fuse of this type.';
     PropertyHelp[5] :=  'Name of the TCC Curve object that determines the fuse blowing.  Must have been previously defined as a TCC_Curve object.'+
                         ' Default is "Tlink". '+
                         'Multiplying the current values in the curve by the "RatedCurrent" value gives the actual current.';
     PropertyHelp[6] :=  'Multiplier or actual phase amps for the phase TCC curve.  Defaults to 1.0.';
     PropertyHelp[7] :=  'Fixed delay time (sec) added to Fuse blowing time determined from the TCC curve. Default is 0.0. Used to represent fuse clearing time or any other delay.' ;
     PropertyHelp[8] :=  'DEPRECATED. See "State" property.';
     PropertyHelp[9] :=  'ARRAY of strings {Open | Closed} representing the Normal state of the fuse in each phase of the controlled element. ' +
                         'The fuse reverts to this state for reset, change of mode, etc. ' +
                         'Defaults to "State" if not specifically declared.';
     PropertyHelp[10] := 'ARRAY of strings {Open | Closed} representing the Actual state of the fuse in each phase of the controlled element. ' +
                         'Upon setting, immediately forces state of fuse(s). Simulates manual control on Fuse. Defaults to Closed for all phases.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TFuse.NewObject(const ObjName:String):Integer;
Begin
    // Make a new Fuse and add it to Fuse class list
    WITH ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TFuseObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;
{--------------------------------------------------------------------------}


{--------------------------------------------------------------------------}
FUNCTION TFuse.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer   :Integer;
   ParamName      :String;
   Param          :String;
   Devindex, i    :Integer;

Begin

  // continue parsing WITH contents of Parser
  ActiveFuseObj := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveFuseObj;

  Result := 0;

  WITH ActiveFuseObj Do Begin

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param := Parser[ActorID].StrValue;
     WHILE Length(Param)>0 Do
      Begin
         IF Length(ParamName) = 0
         THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties)
         THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"',402);
            1: MonitoredElementName     := lowercase(param);
            2: MonitoredElementTerminal := Parser[ActorID].IntValue;
            3: ElementName              := lowercase(param);
            4: ElementTerminal          := Parser[ActorID].IntValue;
            5: FuseCurve                := GetTCCCurve(Param);
            6: RatedCurrent             := Parser[ActorID].Dblvalue;
            7: DelayTime                := Parser[ActorID].DblValue;
            9: Begin
                 InterpretFuseState(ActorID, Param, ParamName);  // set the normal state
                 if not NormalStateSet then NormalStateSet := TRUE;
               End;
         8,10: InterpretFuseState(ActorID, Param, ParamName);  // set the present state

         ELSE
           // Inherited parameters
           ClassEdit( ActiveFuseObj, ParamPointer - NumPropsthisClass)
         End;

         {Supplemental Actions}
         CASE ParamPointer OF
              {Default the controlled element to the monitored element}
              1: ElementName     := MonitoredElementName;
              2: ElementTerminal := MonitoredElementTerminal;
             10: Begin
                      For i := 1 to FNPhases Do If not NormalStateSet then FNormalState^[i] := FPresentState^[i];
                      NormalStateSet := TRUE;   // normal state will default to state only the 1st state is specified.
                 End

         End;

         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
      End;

     RecalcElementData(ActorID);
  End;

End;



{--------------------------------------------------------------------------}
FUNCTION TFuse.MakeLike(const FuseName:String):Integer;
VAR
   OtherFuse:TFuseObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this Fuse name in the present collection}
   OtherFuse := Find(FuseName);
   IF OtherFuse<>Nil THEN
   WITH ActiveFuseObj Do
     Begin

        NPhases := OtherFuse.Fnphases;
        NConds  := OtherFuse.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherFuse.ElementName;
        ElementTerminal   := OtherFuse.ElementTerminal;
        ControlledElement := OtherFuse.ControlledElement;  // Pointer to target circuit element

        MonitoredElement          := OtherFuse.MonitoredElement;  // Pointer to target circuit element
        MonitoredElementName      := OtherFuse.MonitoredElementName;  // Pointer to target circuit element
        MonitoredElementTerminal  := OtherFuse.MonitoredElementTerminal;  // Pointer to target circuit element

        FuseCurve        := OtherFuse.FuseCurve;
        RatedCurrent     := OtherFuse.RatedCurrent;

        // can't copy action handles
        For i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) Do Begin
          FPresentState^[i] := OtherFuse.FPresentState^[i];
          FNormalState^[i]  := OtherFuse.FNormalState^[i];
        End;

        CondOffset     := OtherFuse.CondOffset;

        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherFuse.PropertyValue[i];

     End
   ELSE  DoSimpleMsg('Error in Fuse MakeLike: "' + FuseName + '" Not Found.', 403);

End;




{==========================================================================}
{                    TFuseObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TFuseObj.Create(ParClass:TDSSClass; const FuseName:String);

VAR
   i :Integer;

Begin
     Inherited Create(ParClass);
     Name       := LowerCase(FuseName);
     DSSObjType := ParClass.DSSClassType;

     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms  := 1;  // this forces allocation of terminals and conductors
                         // in base class



     ElementName       := '';
     ControlledElement := NIL;
     ElementTerminal   := 1;

     MonitoredElementName := '';
     MonitoredElementTerminal := 1;
     MonitoredElement := NIL;

     FuseCurve   := GetTccCurve('tlink');

     RatedCurrent      := 1.0;


     FPresentState := Nil;
     FNormalState  := Nil;

     // Reallocate arrays  (Must be initialized to nil for first call)
     Reallocmem(FPresentState, Sizeof(FPresentState^[1]) * FNPhases);
     Reallocmem(FNormalState,  Sizeof(FNormalState^[1])  * FNPhases);

     For i := 1 to Min(FUSEMAXDIM, FNPhases) Do Begin
        FPresentState^[i] := CTRL_CLOSE;
        FNormalState^[i]  := CTRL_CLOSE;  // default to present state;
        ReadyToBlow[i]    := FALSE;
        hAction[i]        := 0;
     End;

     NormalStateSet := FALSE;
     cBuffer := Nil; // Complex buffer

     DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

     InitPropertyValues(0);

   //  RecalcElementData;

End;

destructor TFuseObj.Destroy;
Begin
     MonitoredElementName := '';
     ReallocMem(FPresentState,0);
     ReallocMem(FNormalState,0);
     Reallocmem(cBuffer, 0);
     Inherited Destroy;
End;

{--------------------------------------------------------------------------}
PROCEDURE TFuseObj.RecalcElementData(ActorID : Integer);

VAR
   DevIndex, i :Integer;

Begin

         Devindex := GetCktElementIndex(MonitoredElementName); // Global function
         IF   DevIndex>0 THEN
           Begin
             MonitoredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
             Nphases := MonitoredElement.NPhases;       // Force number of phases to be same
             If Fnphases > FUSEMAXDIM Then DosimpleMsg('Warning: Fuse '+Self.Name+': Number of phases > Max fuse dimension.', 404);
             IF MonitoredElementTerminal > MonitoredElement.Nterms THEN
               Begin
                 DoErrorMsg('Fuse: "' + Name + '"',
                                 'Terminal no. "' +'" does not exist.',
                                 'Re-specify terminal no.', 404);
               End
             ELSE
               Begin
               // Sets name of i-th terminal's connected bus in Fuse's buslist
                 Setbus(1, MonitoredElement.GetBus(MonitoredElementTerminal));
               // Allocate a buffer bigenough to hold everything from the monitored element
                 ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder );
                 CondOffset := (MonitoredElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling
               End;
          End;

{Check for existence of Controlled Element}

         // If previously assigned, reset HasOCPDevice flag in case this is a move
         If Assigned(ControlledElement) Then  ControlledElement.HasOCPDevice := FALSE;

         Devindex := GetCktElementIndex(ElementName); // Global function
         IF   DevIndex>0   THEN
           Begin  // Both CktElement and monitored element must already exist
               ControlledElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
               ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Make the 1 st terminal active

               If Enabled then ControlledElement.HasOCPDevice := TRUE;  // For Reliability calcs

               // Open/Close State of controlled element based on state assigned to the control
               For i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) Do
                 If FPresentState^[i] = CTRL_OPEN Then
                   Begin
                     ControlledElement.Closed[i,ActorID] := FALSE;
                   End
                 Else
                   Begin
                     ControlledElement.Closed[i,ActorID] := TRUE;
                   End;

               For i := 1 to ControlledElement.Nphases Do hAction[i] := 0;
               For i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) Do ReadyToBlow[i] := FALSE;
           End
         ELSE
           Begin
            ControlledElement := nil;   // element not found
            DoErrorMsg('Fuse: "' + Self.Name + '"', 'CktElement Element "'+ ElementName + '" Not Found.',
                            ' Element must be defined previously.', 405);
           End;
End;

{--------------------------------------------------------------------------}
PROCEDURE TFuseObj.CalcYPrim(ActorID : Integer);
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;

{--------------------------------------------------------------------------}
PROCEDURE TFuseObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;
{--------------------------------------------------------------------------}

PROCEDURE TFuseObj.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

{--------------------------------------------------------------------------}
PROCEDURE TFuseObj.DoPendingAction(Const Phs, ProxyHdl:Integer;ActorID : Integer);
// Do what we're instructed by the control queue
// Theoretically, there shouldn't be anything on the queue unless we have to do something
{Only legal action is to open one phase}


begin
    IF Phs <= FUSEMAXDIM  Then
    WITH  ControlledElement Do
    Begin
         ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal of CktElement to terminal 1
         CASE FPresentState^[Phs] of
           CTRL_CLOSE:IF ReadyToBlow[Phs] THEN
                   Begin   // ignore if we became disarmed in meantime
                      ControlledElement.Closed[Phs,ActorID] := FALSE;   // Open phases of active terminal
                      AppendtoEventLog('Fuse.'+Self.Name, 'Phase '+IntToStr(Phs)+' Blown',ActorID);
                      hAction[phs] := 0;
                   END;
         ELSE
            {Do Nothing }
         END;

    End;
end;

{--------------------------------------------------------------------------}


PROCEDURE TFuseObj.InterpretFuseState(ActorID : Integer; const param: String; const property_name: String);
var
  i: Integer;
  DataStr1, DataStr2: String;

Begin

    if (LowerCase(property_name[1]) = 'a') then
    Begin // action (deprecated)  Will be removed

      for i := 1 to FUSEMAXDIM Do Begin

         case LowerCase(param)[1] of
            'o': States[i] := CTRL_OPEN;
            'c': States[i] := CTRL_CLOSE;
         end;

      End;
    End
    Else Begin
          AuxParser[ActorID].CmdString := param;  // Load up Parser

          DataStr1 := AuxParser[ActorID].NextParam;  // ignore
          DataStr2 := AuxParser[ActorID].StrValue;

          i := 1;
          While (Length(DataStr2)>0) and (i<FUSEMAXDIM) Do Begin


              if (LowerCase(property_name[1]) = 's') then begin  // state
                 case LowerCase(DataStr2)[1] of
                  'o': States[i] := CTRL_OPEN;
                  'c': States[i] := CTRL_CLOSE;
                 end;
              end
              else // 'normal'
              begin
                  case LowerCase(DataStr2)[1] of
                  'o': NormalStates[i] := CTRL_OPEN;
                  'c': NormalStates[i] := CTRL_CLOSE;
                 end;
              end;

              DataStr1 := AuxParser[ActorID].NextParam;  // ignore
              DataStr2 := AuxParser[ActorID].StrValue;
              inc(i);
          end;
    End;

End;

{--------------------------------------------------------------------------}
PROCEDURE TFuseObj.Sample(ActorID : Integer);

VAR
   i        :Integer;
   Cmag     :Double;
   TripTime :Double;

begin


     ControlledElement.ActiveTerminalIdx := ElementTerminal;
     MonitoredElement.GetCurrents(cBuffer, ActorID);

     WITH   MonitoredElement Do
     FOR i := 1 to Min(FUSEMAXDIM, MonitoredElement.Nphases) Do
      Begin

         IF  ControlledElement.Closed [i,ActorID]      // Check state of phases of active terminal
         THEN FPresentState[i] := CTRL_CLOSE
         ELSE FPresentState[i] := CTRL_OPEN;

         IF FPresentState[i] = CTRL_CLOSE THEN
           Begin
               TripTime := -1.0;

               {Check Phase Trip, if any}

               IF FuseCurve <> NIL  Then
                 Begin
                   Cmag      := Cabs( cBuffer^[i]);
                   TripTime := FuseCurve.GetTCCTime(Cmag/RatedCurrent);
                 End;

               IF   TripTime > 0.0  THEN
                Begin
                  IF Not ReadyToBlow[i]  THEN
                  WITH ActiveCircuit[ActorID] Do
                   Begin  // Then arm for an open operation
                         hAction[i] := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Delaytime,i, 0, Self, ActorID);
                         ReadyToBlow[i] := TRUE;
                   End; {With}
                End
               ELSE
                Begin
                  IF ReadyToBlow[i] THEN
                    Begin  //  Current has dropped below pickup and it hasn't blown yet
                       ActiveCircuit[ActorID].ControlQueue.Delete(hAction[i], ActorID);  // Delete the fuse blow action
                       ReadyToBlow[i] := FALSE;
                    End;
                End;

           End;  {IF PresentState=CLOSE}
       End; {With}
end;

PROCEDURE TFuseObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i:Integer;

Begin
    Inherited DumpProperties(F,Complete);

    WITH ParentClass Do
     For i := 1 to NumProperties Do
     Begin
           Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    If Complete THEN  Writeln(F);

End;

FUNCTION TFuseObj.GetPropertyValue(Index: Integer): String;
var i: Integer;
begin

    Case Index of
      9..10: Result := '[';

    Else
        Result := '';
    End;
    CASE Index of  // Special cases
       10:  If ControlledElement <> Nil Then
               Begin
                  For i := 1 to ControlledElement.NPhases Do
                  Begin
                    case FPresentState^[i] of
                      CTRL_OPEN: Result := Result + 'open' + ', ';
                    else
                      {CTRL_CLOSE:} Result := Result + 'closed' + ', ';
                    end;
                  End;
               End;
       9:      If ControlledElement <> Nil Then
               Begin
                  For i := 1 to ControlledElement.NPhases Do
                  Begin
                    case FNormalState^[i] of
                      CTRL_OPEN: Result := Result + 'open' + ', ';
                    else
                      {CTRL_CLOSE:} Result := Result + 'closed' + ', ';
                    end;
                  End;
               End;
    ELSE
       Result := Inherited GetPropertyValue(index);
    END;
    Case Index of
      9..10: Result := Result + ']';
    Else
    End;
end;


Procedure TFuseObj.Reset;
VAR
   i:integer;
Begin

    IF ControlledElement <> NIL  THEN
      Begin

         ControlledElement.ActiveTerminalIdx  := ElementTerminal;  // Set active terminal

         For i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) Do Begin
          FPresentState[i]   := FNormalState[i];  // reset to normal state
          ReadyToBlow[i]     := FALSE;
          hAction[i]         := 0;

          case FNormalState[i] of
            CTRL_OPEN: ControlledElement.Closed[i,ActiveActor] := FALSE;
          else
            {CTRL_CLOSE:} ControlledElement.Closed[i,ActiveActor] := TRUE;
          end;

         End;

      End;
end;

Function TFuseObj.get_States(Idx: Integer): EControlAction;
Begin

      IF ControlledElement <> NIL  THEN
      Begin

         ControlledElement.ActiveTerminalIdx  := ElementTerminal;  // Set active terminal
          case ControlledElement.Closed[Idx,ActiveActor] of
            FALSE:  FPresentState^[Idx]:= CTRL_OPEN;
          else
            {TRUE:} FPresentState^[Idx]:= CTRL_CLOSE;
          end;

      End;

      Result := FPresentState^[Idx];
End;

Procedure TFuseObj.set_States(Idx: Integer; const Value: EControlAction);
Begin

        If States[Idx] <> Value Then Begin

            IF ControlledElement <> NIL  THEN
            Begin
              ControlledElement.ActiveTerminalIdx  := ElementTerminal;  // Set active terminal
              case Value of
                CTRL_OPEN: ControlledElement.Closed[Idx,ActiveActor] := FALSE;
              else
                {CTRL_CLOSE:} ControlledElement.Closed[Idx,ActiveActor] := TRUE;
              end;
            End;

            FPresentState^[Idx] := Value;
        End;
End;

Function TFuseObj.get_NormalStates(Idx: Integer): EControlAction;
Begin
        Result := FNormalState^[Idx];
End;

Procedure TFuseObj.set_NormalStates(Idx: Integer; const Value: EControlAction);
Begin
      If FNormalState^[Idx] <> Value Then Begin
          FNormalState^[Idx] := Value;
      End;
End;

procedure TFuseObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1]  := ''; //'element';
     PropertyValue[2]  := '1'; //'terminal';
     PropertyValue[3]  := '';
     PropertyValue[4]  := '1'; //'terminal';
     PropertyValue[5]  := 'Tlink';
     PropertyValue[6]  := '1.0';
     PropertyValue[7]  := '0';
     PropertyValue[8]  := '';  // action
     PropertyValue[9]  := '[close, close, close]';  // normal
     PropertyValue[10] := '[close,close,close]';  // state

  inherited  InitPropertyValues(NumPropsThisClass);

end;

INITIALIZATION

end.
