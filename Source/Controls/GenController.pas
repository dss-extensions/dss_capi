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

interface

uses
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    Arraydef,
    ucomplex,
    utilities,
    PointerList,
    Classes;

type

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TGenController = class(TControlClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const GenControlName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TGenControllerObj = class(TControlElem)
    PRIVATE

        FkWLimit,
        FkWBand,
        HalfkWBand,
        FkvarLimit: Double;

            // FWeights:pDoubleArray;

    PUBLIC

        constructor Create(ParClass: TDSSClass; const GenControlName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;    // Always Zero for a GenControl

        procedure Sample(ActorID: Integer); OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset(ActorID: Integer); OVERRIDE;  // Reset to initial defined state

        procedure GetCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE; // Get present value of terminal Curr
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;   // Returns Injextion currents

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

    end;


var
    ActiveGenControlObj: TGenControllerObj;
    GenControllerClass: TGenController;

{--------------------------------------------------------------------------}
implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Generator,
    Sysutils,
    uCmatrix,
    MathUtil,
    Math;

const

    NumPropsThisClass = 6;


{--------------------------------------------------------------------------}
constructor TGenController.Create;  // Creates superstructure for all GenControl objects
begin
    inherited Create;

    Class_name := 'GenController';
    DSSClassType := DSSClassType + GEN_CONTROLLER;

    DefineProperties;

    CommandList := TCommandList.Create(PropertyName, NumProperties);
    CommandList.Abbrev := TRUE;

    GenControllerClass := Self;
end;

{--------------------------------------------------------------------------}
destructor TGenController.Destroy;

begin
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGenController.DefineProperties;
begin

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

    PropertyHelp^[1] := 'Full object name of the Generator element ' +
        'that the control is monitoring. There is no default; must be specified and must exist.';
    PropertyHelp^[2] := 'Number of the terminal of the Generator element to which the GenControl control is connected. ' +
        '1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit.';
    PropertyHelp^[3] := 'kW Limit for the monitored element. The generators are dispatched to hold the power in band.';
    PropertyHelp^[4] := 'Bandwidth (kW) of the dead band around the target limit.' +
        'No dispatch changes are attempted if the power in the monitored terminal stays within this band.';
    PropertyHelp^[5] := 'Max kvar to be delivered through the element.  Uses same dead band as kW.';
    PropertyHelp^[6] := 'Array list of generators to be dispatched.  If not specified, all generators in the circuit are assumed dispatchable.';
    PropertyHelp^[7] := 'Array of proportional weights corresponding to each generator in the GenList.' +
        ' The needed kW to get back to center band is dispatched to each generator according to these weights. ' +
        'Default is to set all weights to 1.0.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

{--------------------------------------------------------------------------}
function TGenController.NewObject(const ObjName: String): Integer;
begin
    // Make a new GenControl and add it to GenControl class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TGenControllerobj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

{--------------------------------------------------------------------------}
function TGenController.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin

  // continue parsing WITH contents of Parser
    ActiveGenControlObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveGenControlObj;

    Result := 0;

    with ActiveGenControlObj do
    begin

        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;
        Param := Parser[ActorID].StrValue;
        while Length(Param) > 0 do
        begin
            if Length(ParamName) = 0 then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[ParamPointer] := Param;

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 364);
                1:
                    ElementName := lowercase(param);
                2:
                    ElementTerminal := Parser[ActorID].IntValue;
                3:
                    FkWLimit := Parser[ActorID].DblValue;
                4:
                    FkWBand := Parser[ActorID].DblValue;
                5:
                    FkvarLimit := Parser[ActorID].DblValue;
                6:
{InterpretTStringListArray(Param, FGeneratorNameList)};
                7:
                begin
                end;

            else
           // Inherited parameters
                ClassEdit(ActiveGenControlObj, ParamPointer - NumPropsthisClass)
            end;

            case ParamPointer of
                4:
                    HalfkWBand := FkWBand / 2.0;
                6:
                begin   // levelize the list
                end;
            else

            end;

            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

        RecalcElementData(ActorID);
    end;

end;


{--------------------------------------------------------------------------}
function TGenController.MakeLike(const GenControlName: String): Integer;
var
    OtherGenControl: TGenControllerobj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this GenControl name in the present collection}
    OtherGenControl := Find(GenControlName);
    if OtherGenControl <> NIL then
        with ActiveGenControlObj do
        begin

            NPhases := OtherGenControl.Fnphases;
            NConds := OtherGenControl.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherGenControl.ElementName;
            ControlledElement := OtherGenControl.ControlledElement;  // Pointer to target circuit element
            MonitoredElement := OtherGenControl.MonitoredElement;  // Pointer to target circuit element

            ElementTerminal := OtherGenControl.ElementTerminal;


            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherGenControl.PropertyValue[i];

        end
    else
        DoSimpleMsg('Error in GenControl MakeLike: "' + GenControlName + '" Not Found.', 370);

end;


{==========================================================================}
{                    TGenControlObj                                           }
{==========================================================================}


{--------------------------------------------------------------------------}
constructor TGenControllerObj.Create(ParClass: TDSSClass; const GenControlName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(GenControlName);
    DSSObjType := ParClass.DSSClassType;

    NPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class


    ElementName := '';
    ControlledElement := NIL;  // not used in this control
    ElementTerminal := 1;
    MonitoredElement := NIL;

    FkWLimit := 8000.0;
    FkWBand := 100.0;
    HalfkWBand := FkWBand / 2.0;
    InitPropertyValues(0);
    FkvarLimit := FkWLimit / 2.0;


   //  RecalcElementData;

end;

destructor TGenControllerobj.Destroy;
begin
    ElementName := '';
    inherited Destroy;
end;

{--------------------------------------------------------------------------}
procedure TGenControllerobj.RecalcElementData(ActorID: Integer);

var
    DevIndex: Integer;

begin


{Check for existence of monitored element}

    Devindex := GetCktElementIndex(ElementName); // Global function
    if DevIndex > 0 then
    begin
        MonitoredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
        if ElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg('GenControl: "' + Name + '"',
                'Terminal no. "' + '" does not exist.',
                'Re-specify terminal no.', 371);
        end
        else
        begin
               // Sets name of i-th terminal's connected bus in GenControl's buslist
            Setbus(1, MonitoredElement.GetBus(ElementTerminal));
        end;
    end
    else
        DoSimpleMsg('Monitored Element in GenControl.' + Name + ' does not exist:"' + ElementName + '"', 372);


end;

procedure TGenControllerobj.MakePosSequence(ActorID: Integer);
begin
    if MonitoredElement <> NIL then
    begin
        Nphases := ControlledElement.NPhases;
        Nconds := FNphases;
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    end;
    inherited;
end;

{--------------------------------------------------------------------------}
procedure TGenControllerobj.CalcYPrim(ActorID: Integer);
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;


{--------------------------------------------------------------------------}
procedure TGenControllerobj.GetCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;

procedure TGenControllerobj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

{--------------------------------------------------------------------------}
procedure TGenControllerobj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;

    if Complete then
    begin
        Writeln(F);
    end;

end;


{--------------------------------------------------------------------------}
procedure TGenControllerobj.DoPendingAction;
begin

        {Do Nothing}
end;

{--------------------------------------------------------------------------}
procedure TGenControllerobj.Sample(ActorID: Integer);

var
   //i           :Integer;
    PDiff: Double;
   //QDiff       :Double;
    S: Complex;
   //Gen         :TGeneratorObj;
   //GenkWChanged, Genkvarchanged: Boolean;
   //GenkW, Genkvar :Double;

begin


       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
    S := MonitoredElement.Power[ElementTerminal, ActorID];  // Power in active terminal

    PDiff := S.re * 0.001 - FkWLimit;

       //QDiff := S.im * 0.001 - FkvarLimit;

       // Redispatch the vars.

       //GenkWChanged := FALSE;
       //GenkvarChanged := FALSE;

    if Abs(PDiff) > HalfkWBand then
    begin // Redispatch Generators
          // PDiff is kW needed to get back into band

    end;
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

    PropertyValue[1] := '';   //'element';
    PropertyValue[2] := '1';   //'terminal';
    PropertyValue[3] := '8000';
    PropertyValue[4] := '100';
    PropertyValue[5] := '0';
    PropertyValue[6] := '';
    PropertyValue[7] := '';


    inherited  InitPropertyValues(NumPropsThisClass);

end;


procedure TGenControllerobj.Reset;
begin
  // inherited;

end;

end.
