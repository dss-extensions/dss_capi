unit UPFCControl;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
  A UPFCControl is a control element that is connected to a terminal of another
  circuit element and sends dispatch kW signals to a set of generators it controls

  A UPFCControl is defined by a New command:

  New UPFCControl.Name=myname Element=devclass.name terminal=[ 1|2|...] CapacitorList = (gen1  gen2 ...)

 
}

{$HINTS OFF}
INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex,
     utilities, PointerList, Classes;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TUPFCControl = class(TControlClass)
     private

     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const UPFCControlName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit(ActorID : Integer):Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;

   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
  TUPFCControlObj = class(TControlElem)
    private

      FUPFCNameList         : TStringList;
      FUPFCList             : PointerList.TPointerList;
      FListSize             : Integer;
      FWeights              : pDoubleArray;
      TotalWeight           : Double;

    public
      constructor Create(ParClass:TDSSClass; const UPFCControlName:String);
      destructor Destroy; override;

      PROCEDURE MakePosSequence(ActorID : Integer); Override;  // Make a positive Sequence Model
      PROCEDURE RecalcElementData(ActorID : Integer); Override;
      PROCEDURE CalcYPrim(ActorID : Integer); Override;    // Always Zero for a UPFCControl
      PROCEDURE Sample(ActorID : Integer);  Override;    // Sample control quantities and set action times in Control Queue
      PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer); Override;   // Do the action that is pending from last sample
      PROCEDURE Reset(ActorID : Integer); Override;  // Reset to initial defined state
      PROCEDURE GetCurrents(Curr: pComplexArray; ActorID : Integer); Override; // Get present value of terminal Curr
      PROCEDURE GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override;   // Returns Injextion currents
      PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
      PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

      FUNCTION MakeUPFCList:Boolean;

      // Public properties
      Property UPFCList     :PointerList.TPointerList   Read FUPFCList write FUPFCList;
      Property UPFCListSize : Integer                   Read FListSize write FListSize;

  end;


VAR
    ActiveUPFCControlObj:TUPFCControlObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit,  UPFC, Sysutils, uCmatrix, MathUtil, Math;

CONST

    NumPropsThisClass = 1;


{--------------------------------------------------------------------------}
constructor TUPFCControl.Create;  // Creates superstructure for all UPFCControl objects
Begin
     Inherited Create;

     Class_name   := 'UPFCControl';
     DSSClassType := DSSClassType + UPFC_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(PropertyName, NumProperties);
     CommandList.Abbrev := TRUE;
End;

{--------------------------------------------------------------------------}
destructor TUPFCControl.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TUPFCControl.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName^[1] := 'UPFCList';


     PropertyHelp^[1] := 'The list of all the UPFC devices to be controlled by this controller, '+
                        'If left empty, this control will apply for all UPFCs in the model.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TUPFCControl.NewObject(const ObjName:String):Integer;
Begin
    // Make a new UPFCControl and add it to UPFCControl class list
    WITH ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TUPFCControlObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;

{--------------------------------------------------------------------------}
FUNCTION TUPFCControl.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

Begin

  // continue parsing WITH contents of Parser
  ActiveUPFCControlObj := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveUPFCControlObj;

  Result := 0;

  WITH ActiveUPFCControlObj Do Begin

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
            1: InterpretTStringListArray(Param, FUPFCNameList);
         ELSE
           // Inherited parameters
           ClassEdit( ActiveUPFCControlObj, ParamPointer - NumPropsthisClass)
         End;

     END;

         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
  End;

  //RecalcElementData(ActorID);

End;



{--------------------------------------------------------------------------}
FUNCTION TUPFCControl.MakeLike(const UPFCControlName:String):Integer;
VAR
   OtherUPFCControl:TUPFCControlObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this UPFCControl name in the present collection}
   OtherUPFCControl := Find(UPFCControlName);
   IF OtherUPFCControl<>Nil THEN
   WITH ActiveUPFCControlObj Do Begin

        NPhases := OtherUPFCControl.Fnphases;
        NConds  := OtherUPFCControl.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherUPFCControl.ElementName;
        ControlledElement := OtherUPFCControl.ControlledElement;  // Pointer to target circuit element
        MonitoredElement  := OtherUPFCControl.MonitoredElement;  // Pointer to target circuit element

        ElementTerminal   := OtherUPFCControl.ElementTerminal;


        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherUPFCControl.PropertyValue[i];

   End
   ELSE  DoSimpleMsg('Error in UPFCControl MakeLike: "' + UPFCControlName + '" Not Found.', 370);

End;




{==========================================================================}
{                    TUPFCControlObj                                           }
{==========================================================================}
{--------------------------------------------------------------------------}
constructor TUPFCControlObj.Create(ParClass:TDSSClass; const UPFCControlName:String);

Begin
  Inherited Create(ParClass);

  Name                := LowerCase(UPFCControlName);
  DSSObjType          := ParClass.DSSClassType;

  FUPFCNameList       := TSTringList.Create;
  FUPFCList           := PointerList.TPointerList.Create(20);  // Default size and increment
  TotalWeight         := 1.0;
  FWeights            := Nil;
  FListSize           := 0;

   //  RecalcElementData;

End;

destructor TUPFCControlObj.Destroy;
Begin
     ElementName := '';
     Inherited Destroy;
End;

{--------------------------------------------------------------------------}
PROCEDURE TUPFCControlObj.RecalcElementData(ActorID : Integer);

VAR
   DevIndex :Integer;

Begin
{Check for existence of monitored element}

  Devindex := GetCktElementIndex(ElementName); // Global function
  IF   DevIndex>0  THEN
  Begin
    MonitoredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
    IF ElementTerminal > MonitoredElement.Nterms THEN
    Begin
      DoErrorMsg('UPFCControl: "' + Name + '"',
                     'Terminal no. "' +'" does not exist.',
                     'Re-specify terminal no.', 371);
    End
    ELSE
    Begin
     // Sets name of i-th terminal's connected bus in UPFCControl's buslist
      Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    End;
  End
  ELSE
    DoSimpleMsg('Monitored Element in UPFCControl.'+Name+ ' does not exist:"'+ElementName+'"', 372);
End;

{--------------------------------------------------------------------------}
procedure TUPFCControlObj.MakePosSequence(ActorID : Integer);
begin
  if MonitoredElement <> Nil then
  begin
    Nphases := ControlledElement.NPhases;
    Nconds := FNphases;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));
  end;
  inherited;
end;

{--------------------------------------------------------------------------}
PROCEDURE TUPFCControlObj.CalcYPrim(ActorID : Integer);
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;

{--------------------------------------------------------------------------}
PROCEDURE TUPFCControlObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;

PROCEDURE TUPFCControlObj.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

{--------------------------------------------------------------------------}
PROCEDURE TUPFCControlObj.DumpProperties(Var F:TextFile; Complete:Boolean);

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
PROCEDURE TUPFCControlObj.DoPendingAction;
VAR
  i           : Integer;
  myUPFC      : TUPFCObj;

begin
  If FListSize>0 Then
  Begin
    For i := 1 to FListSize Do
    Begin
      myUPFC  :=  FUPFCList.Get(i);
      myUPFC.UploadCurrents(ActorID);
    End;
  End;
end;

{--------------------------------------------------------------------------}
PROCEDURE TUPFCControlObj.Sample(ActorID : Integer);
VAR
  Update      : Boolean;
  i           : Integer;
  myUPFC      : TUPFCObj;

begin
  // If list is not define, go make one from all generators in circuit
  IF FUPFCList.ListSize=0 Then  MakeUPFCList;
  Update  :=  False;
  If FListSize>0 Then
  Begin
    For i := 1 to FListSize Do
    Begin
      myUPFC  :=  FUPFCList.Get(i);
      Update  :=  Update or myUPFC.CheckStatus(ActorID);
    End;
   {Checks if at least one UPFC needs to be updated}
    if Update then
    Begin
      With ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution Do
        ControlQueue.Push(DynaVars.intHour, DynaVars.t, 0, 0, Self, ActorID);

    End;
  End;

end;


procedure TUPFCControlObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1]  := '[]';   //'UPFC List';


  inherited  InitPropertyValues(NumPropsThisClass);

end;

Function TUPFCControlObj.MakeUPFCList:Boolean;

VAR
  MyClass  : TDSSClass;
  UPFC      : TUPFCObj;
  i         : Integer;

begin

  Result := FALSE;
  // Clears everything
  FUPFCNameList.Clear;
  FUPFCList.Clear;
  MyClass := GetDSSClassPtr('upfc');

  If FListSize>0 Then Begin    // Name list is defined - Use it

    For i := 1 to FListSize Do Begin
      UPFC := MyClass.Find(FUPFCNameList.Strings[i-1]);
      If Assigned(UPFC) and UPFC.Enabled Then FUPFCList.New := UPFC;
    End;

  End
  Else  // No list given
  Begin
   {Search through the entire circuit for enabled generators and add them to the list}

    For i := 1 to MyClass.ElementCount Do
    Begin
      UPFC  :=  MyClass.ElementList.Get(i);
      // Checks if it's enabled
      if UPFC.Enabled then
        FUPFCList.New :=  UPFC;
    End;

    {Allocate uniform weights}
    FListSize := FUPFCList.ListSize;
    Reallocmem(FWeights, Sizeof(FWeights^[1])*FListSize);
    For i := 1 to FListSize Do FWeights^[i] := 1.0;

  End;

  // Add up total weights
  TotalWeight := 0.0;
  For i := 1 to FListSize Do
    TotalWeight := TotalWeight + FWeights^[i];

  If FUPFCList.ListSize>0 Then Result := TRUE;

end;



procedure TUPFCControlObj.Reset;
begin
  // inherited;

end;

end.
