unit GenController;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
  A GenController is a control element that is connected to a terminal of another
  circuit element and Controls a generators

  A GenController is defined by a New command:



 
}

INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex,
     utilities, PointerList, Classes;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TGenController = class(TControlClass)
     private

     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const GenControlName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit(ActorID : Integer):Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;

   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TGenControllerObj = class(TControlElem)
     private

            FkWLimit,
            FkWBand,
            HalfkWBand,
            FkvarLimit :Double;

            FWeights:pDoubleArray;

     public

       constructor Create(ParClass:TDSSClass; const GenControlName:String);
       destructor Destroy; override;

       PROCEDURE MakePosSequence(ActorID : Integer); Override;  // Make a positive Sequence Model
       PROCEDURE RecalcElementData(ActorID : Integer); Override;
       PROCEDURE CalcYPrim(ActorID : Integer); Override;    // Always Zero for a GenControl

       PROCEDURE Sample(ActorID : Integer);  Override;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset(ActorID : Integer); Override;  // Reset to initial defined state

       PROCEDURE GetCurrents(Curr: pComplexArray;ActorID : Integer); Override; // Get present value of terminal Curr
       PROCEDURE GetInjCurrents(Curr: pComplexArray;ActorID : Integer); Override;   // Returns Injextion currents

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

   end;


VAR
    ActiveGenControlObj:TGenControllerObj;
    GenControllerClass :TGenController;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit,  Generator, Sysutils, uCmatrix, MathUtil, Math;

CONST

    NumPropsThisClass = 6;


{--------------------------------------------------------------------------}
constructor TGenController.Create;  // Creates superstructure for all GenControl objects
Begin
     Inherited Create;

     Class_name   := 'GenController';
     DSSClassType := DSSClassType + GEN_CONTROLLER;

     DefineProperties;

     CommandList := TCommandList.Create(PropertyName, NumProperties);
     CommandList.Abbrev := TRUE;

     GenControllerClass := Self;
End;

{--------------------------------------------------------------------------}
destructor TGenController.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TGenController.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName^[1] := 'Element';
     PropertyName^[2] := 'Terminal';
     PropertyName^[3] := 'kWLimit';
     PropertyName^[4] := 'kWBand';
     PropertyName^[5] := 'kvarlimit';
     PropertyName^[6] := 'GenList';
     PropertyName^[7] := 'Weights';

     PropertyHelp^[1] := 'Full object name of the Generator element '+
                        'that the control is monitoring. There is no default; must be specified and must exist.';
     PropertyHelp^[2] := 'Number of the terminal of the Generator element to which the GenControl control is connected. '+
                        '1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit.';
     PropertyHelp^[3] := 'kW Limit for the monitored element. The generators are dispatched to hold the power in band.';
     PropertyHelp^[4] := 'Bandwidth (kW) of the dead band around the target limit.' +
                        'No dispatch changes are attempted if the power in the monitored terminal stays within this band.';
     PropertyHelp^[5] := 'Max kvar to be delivered through the element.  Uses same dead band as kW.';
     PropertyHelp^[6] := 'Array list of generators to be dispatched.  If not specified, all generators in the circuit are assumed dispatchable.';
     PropertyHelp^[7] := 'Array of proportional weights corresponding to each generator in the GenList.' +
                        ' The needed kW to get back to center band is dispatched to each generator according to these weights. ' +
                        'Default is to set all weights to 1.0.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TGenController.NewObject(const ObjName:String):Integer;
Begin
    // Make a new GenControl and add it to GenControl class list
    WITH ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TGenControllerobj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;

{--------------------------------------------------------------------------}
FUNCTION TGenController.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;
   i:Integer;

Begin

  // continue parsing WITH contents of Parser
  ActiveGenControlObj := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveGenControlObj;

  Result := 0;

  WITH ActiveGenControlObj Do Begin

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param := Parser[ActorID].StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties)
         THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 364);
            1: ElementName     := lowercase(param);
            2: ElementTerminal := Parser[ActorID].IntValue;
            3: FkWLimit := Parser[ActorID].DblValue;
            4: FkWBand := Parser[ActorID].DblValue;
            5: FkvarLimit := Parser[ActorID].DblValue;
            6: {InterpretTStringListArray(Param, FGeneratorNameList)};
            7: Begin
               End;

         ELSE
           // Inherited parameters
           ClassEdit( ActiveGenControlObj, ParamPointer - NumPropsthisClass)
         End;

         CASE ParamPointer OF
            4: HalfkWBand := FkWBand / 2.0;
            6: Begin   // levelize the list
               End;
         ELSE

         END;

         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
     End;

     RecalcElementData(ActorID);
  End;

End;



{--------------------------------------------------------------------------}
FUNCTION TGenController.MakeLike(const GenControlName:String):Integer;
VAR
   OtherGenControl:TGenControllerobj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this GenControl name in the present collection}
   OtherGenControl := Find(GenControlName);
   IF OtherGenControl<>Nil THEN
   WITH ActiveGenControlObj Do Begin

        NPhases := OtherGenControl.Fnphases;
        NConds  := OtherGenControl.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherGenControl.ElementName;
        ControlledElement := OtherGenControl.ControlledElement;  // Pointer to target circuit element
        MonitoredElement  := OtherGenControl.MonitoredElement;  // Pointer to target circuit element

        ElementTerminal   := OtherGenControl.ElementTerminal;


        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherGenControl.PropertyValue[i];

   End
   ELSE  DoSimpleMsg('Error in GenControl MakeLike: "' + GenControlName + '" Not Found.', 370);

End;




{==========================================================================}
{                    TGenControlObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TGenControllerObj.Create(ParClass:TDSSClass; const GenControlName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(GenControlName);
     DSSObjType := ParClass.DSSClassType;

     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms  := 1;  // this forces allocation of terminals and conductors
                         // in base class



     ElementName   := '';
     ControlledElement := nil;  // not used in this control
     ElementTerminal  := 1;
     MonitoredElement := Nil;

     FkWLimit    := 8000.0;
     FkWBand     := 100.0;
     HalfkWBand  := FkWBand/2.0;
     InitPropertyValues(0);
     FkvarLimit  := FkWLimit/2.0;


   //  RecalcElementData;

End;

destructor TGenControllerobj.Destroy;
Begin
     ElementName := '';
     Inherited Destroy;
End;

{--------------------------------------------------------------------------}
PROCEDURE TGenControllerobj.RecalcElementData(ActorID : Integer);

VAR
   DevIndex :Integer;

Begin


{Check for existence of monitored element}

         Devindex := GetCktElementIndex(ElementName); // Global function
         IF   DevIndex>0  THEN Begin
             MonitoredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
             IF ElementTerminal > MonitoredElement.Nterms
             THEN Begin
                 DoErrorMsg('GenControl: "' + Name + '"',
                                 'Terminal no. "' +'" does not exist.',
                                 'Re-specify terminal no.', 371);
             End
             ELSE Begin
               // Sets name of i-th terminal's connected bus in GenControl's buslist
                 Setbus(1, MonitoredElement.GetBus(ElementTerminal));
             End;
         End
         ELSE DoSimpleMsg('Monitored Element in GenControl.'+Name+ ' does not exist:"'+ElementName+'"', 372);


End;

procedure TGenControllerobj.MakePosSequence(ActorID : Integer);
begin
  if MonitoredElement <> Nil then begin
    Nphases := ControlledElement.NPhases;
    Nconds := FNphases;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));
  end;
  inherited;
end;

{--------------------------------------------------------------------------}
PROCEDURE TGenControllerobj.CalcYPrim(ActorID : Integer);
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;






{--------------------------------------------------------------------------}
PROCEDURE TGenControllerobj.GetCurrents(Curr: pComplexArray;ActorID : Integer);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;

PROCEDURE TGenControllerobj.GetInjCurrents(Curr: pComplexArray;ActorID : Integer);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

{--------------------------------------------------------------------------}
PROCEDURE TGenControllerobj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i:Integer;

Begin
    Inherited DumpProperties(F,Complete);

    WITH ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    If Complete THEN
    Begin
      Writeln(F);
    End;

End;


{--------------------------------------------------------------------------}
PROCEDURE TGenControllerobj.DoPendingAction;
begin

        {Do Nothing}
end;

{--------------------------------------------------------------------------}
PROCEDURE TGenControllerobj.Sample(ActorID : Integer);

VAR
   i           :Integer;
   PDiff,
   QDiff       :Double;
   S           :Complex ;
   Gen         :TGeneratorObj;
   GenkWChanged, Genkvarchanged: Boolean;
   GenkW, Genkvar :Double;

begin


       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
       S := MonitoredElement.Power[ElementTerminal,ActorID];  // Power in active terminal

       PDiff := S.re * 0.001 - FkWLimit;

       QDiff := S.im * 0.001 - FkvarLimit;

       // Redispatch the vars.

       GenkWChanged := FALSE;
       GenkvarChanged := FALSE;

       If Abs(PDiff) > HalfkWBand Then
          Begin // Redispatch Generators
          // PDiff is kW needed to get back into band

          End;
 {
       If Abs(QDiff) > HalfkWBand Then Begin // Redispatch Generators
          // QDiff is kvar needed to get back into band
          For i := 1 to FListSize Do Begin
              Gen := FGenPointerList.Get(i);
              // compute new dispatch value for this generator ...
              Genkvar := Max(0.0, (Gen.kvarBase + QDiff *(FWeights^[i]/TotalWeight)));
              If Genkvar <> Gen.kvarBase Then Begin
                  Gen.kvarBase := Genkvar;
                  Genkvarchanged := TRUE;
              End;
          End;
       End;



       If GenkWChanged or Genkvarchanged Then  // Only push onto controlqueue if there has been a change
          With ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution Do Begin
            LoadsNeedUpdating := TRUE; // Force recalc of power parms
            // Push present time onto control queue to force re solve at new dispatch value
            ControlQueue.Push(DynaVars.intHour, DynaVars.t, 0, 0, Self, ActorID);
          End;

  }


end;


procedure TGenControllerobj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1]  := '';   //'element';
     PropertyValue[2]  := '1';   //'terminal';
     PropertyValue[3]  := '8000';
     PropertyValue[4]  := '100';
     PropertyValue[5]  := '0';
     PropertyValue[6]  := '';
     PropertyValue[7]  := '';



  inherited  InitPropertyValues(NumPropsThisClass);

end;



procedure TGenControllerobj.Reset;
begin
  // inherited;

end;



INITIALIZATION




end.
