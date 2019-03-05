unit GenDispatcher;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
  A GenDispatcher is a control element that is connected to a terminal of another
  circuit element and sends dispatch kW signals to a set of generators it controls

  A GenDispatcher is defined by a New command:

  New GenDispatcher.Name=myname Element=devclass.name terminal=[ 1|2|...] CapacitorList = (gen1  gen2 ...)

 
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
    TGenDispatcher = class(TControlClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const GenDispatcherName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TGenDispatcherObj = class(TControlElem)
    PRIVATE

        FkWLimit,
        FkWBand,
        HalfkWBand,
        FkvarLimit,
        TotalWeight: Double;
        FListSize: Integer;
        FGeneratorNameList: TStringList;
        FGenPointerList: PointerList.TPointerList;
        FWeights: pDoubleArray;

        MonitoredElement: TDSSCktElement;

    PUBLIC

        constructor Create(ParClass: TDSSClass; const GenDispatcherName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;    // Always Zero for a GenDispatcher

        procedure Sample(ActorID: Integer); OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state

        procedure GetCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE; // Get present value of terminal Curr
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;   // Returns Injextion currents

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

        function MakeGenList: Boolean;
    end;


var
    ActiveGenDispatcherObj: TGenDispatcherObj;

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
constructor TGenDispatcher.Create;  // Creates superstructure for all GenDispatcher objects
begin
    inherited Create;

    Class_name := 'GenDispatcher';
    DSSClassType := DSSClassType + GEN_CONTROL;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

{--------------------------------------------------------------------------}
destructor TGenDispatcher.Destroy;

begin
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGenDispatcher.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names

    PropertyName[1] := 'Element';
    PropertyName[2] := 'Terminal';
    PropertyName[3] := 'kWLimit';
    PropertyName[4] := 'kWBand';
    PropertyName[5] := 'kvarlimit';
    PropertyName[6] := 'GenList';
    PropertyName[7] := 'Weights';

    PropertyHelp[1] := 'Full object name of the circuit element, typically a line or transformer, ' +
        'which the control is monitoring. There is no default; must be specified.';
    PropertyHelp[2] := 'Number of the terminal of the circuit element to which the GenDispatcher control is connected. ' +
        '1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit.';
    PropertyHelp[3] := 'kW Limit for the monitored element. The generators are dispatched to hold the power in band.';
    PropertyHelp[4] := 'Bandwidth (kW) of the dead band around the target limit.' +
        'No dispatch changes are attempted if the power in the monitored terminal stays within this band.';
    PropertyHelp[5] := 'Max kvar to be delivered through the element.  Uses same dead band as kW.';
    PropertyHelp[6] := 'Array list of generators to be dispatched.  If not specified, all generators in the circuit are assumed dispatchable.';
    PropertyHelp[7] := 'Array of proportional weights corresponding to each generator in the GenList.' +
        ' The needed kW to get back to center band is dispatched to each generator according to these weights. ' +
        'Default is to set all weights to 1.0.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

{--------------------------------------------------------------------------}
function TGenDispatcher.NewObject(const ObjName: String): Integer;
begin
    // Make a new GenDispatcher and add it to GenDispatcher class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TGenDispatcherObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

{--------------------------------------------------------------------------}
function TGenDispatcher.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    i: Integer;

begin

  // continue parsing WITH contents of Parser
    ActiveGenDispatcherObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveGenDispatcherObj;

    Result := 0;

    with ActiveGenDispatcherObj do
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
                    InterpretTStringListArray(Param, FGeneratorNameList);
                7:
                begin
                    FListSize := FGeneratorNameList.count;
                    if FListSize > 0 then
                    begin
                        Reallocmem(FWeights, Sizeof(FWeights^[1]) * FListSize);
                        InterpretDblArray(Param, FListSize, FWeights);
                    end;
                end;

            else
           // Inherited parameters
                ClassEdit(ActiveGenDispatcherObj, ParamPointer - NumPropsthisClass)
            end;

            case ParamPointer of
                4:
                    HalfkWBand := FkWBand / 2.0;
                6:
                begin   // levelize the list
                    FGenPointerList.Clear;  // clear this for resetting on first sample
                    FListSize := FGeneratorNameList.count;
                    Reallocmem(FWeights, Sizeof(FWeights^[1]) * FListSize);
                    for i := 1 to FListSize do
                        FWeights^[i] := 1.0;
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
function TGenDispatcher.MakeLike(const GenDispatcherName: String): Integer;
var
    OtherGenDispatcher: TGenDispatcherObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this GenDispatcher name in the present collection}
    OtherGenDispatcher := Find(GenDispatcherName);
    if OtherGenDispatcher <> NIL then
        with ActiveGenDispatcherObj do
        begin

            NPhases := OtherGenDispatcher.Fnphases;
            NConds := OtherGenDispatcher.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherGenDispatcher.ElementName;
            ControlledElement := OtherGenDispatcher.ControlledElement;  // Pointer to target circuit element
            MonitoredElement := OtherGenDispatcher.MonitoredElement;  // Pointer to target circuit element

            ElementTerminal := OtherGenDispatcher.ElementTerminal;


            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherGenDispatcher.PropertyValue[i];

        end
    else
        DoSimpleMsg('Error in GenDispatcher MakeLike: "' + GenDispatcherName + '" Not Found.', 370);

end;


{==========================================================================}
{                    TGenDispatcherObj                                           }
{==========================================================================}


{--------------------------------------------------------------------------}
constructor TGenDispatcherObj.Create(ParClass: TDSSClass; const GenDispatcherName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(GenDispatcherName);
    DSSObjType := ParClass.DSSClassType;

    NPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class


    ElementName := '';
    ControlledElement := NIL;  // not used in this control
    ElementTerminal := 1;
    MonitoredElement := NIL;

    FGeneratorNameList := TSTringList.Create;
    FWeights := NIL;
    FGenPointerList := PointerList.TPointerList.Create(20);  // Default size and increment
    FListSize := 0;
    FkWLimit := 8000.0;
    FkWBand := 100.0;
    TotalWeight := 1.0;
    HalfkWBand := FkWBand / 2.0;
    InitPropertyValues(0);
    FkvarLimit := FkWLimit / 2.0;


   //  RecalcElementData;

end;

destructor TGenDispatcherObj.Destroy;
begin
    ElementName := '';
    inherited Destroy;
end;

{--------------------------------------------------------------------------}
procedure TGenDispatcherObj.RecalcElementData(ActorID: Integer);

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
            DoErrorMsg('GenDispatcher: "' + Name + '"',
                'Terminal no. "' + '" does not exist.',
                'Re-specify terminal no.', 371);
        end
        else
        begin
               // Sets name of i-th terminal's connected bus in GenDispatcher's buslist
            Setbus(1, MonitoredElement.GetBus(ElementTerminal));
        end;
    end
    else
        DoSimpleMsg('Monitored Element in GenDispatcher.' + Name + ' does not exist:"' + ElementName + '"', 372);


end;

procedure TGenDispatcherObj.MakePosSequence(ActorID: Integer);
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
procedure TGenDispatcherObj.CalcYPrim(ActorID: Integer);
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;


{--------------------------------------------------------------------------}
procedure TGenDispatcherObj.GetCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;

procedure TGenDispatcherObj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

{--------------------------------------------------------------------------}
procedure TGenDispatcherObj.DumpProperties(var F: TextFile; Complete: Boolean);

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
procedure TGenDispatcherObj.DoPendingAction;
begin

        {Do Nothing}
end;

{--------------------------------------------------------------------------}
procedure TGenDispatcherObj.Sample(ActorID: Integer);

var
    i: Integer;
    PDiff,
    QDiff: Double;
    S: Complex;
    Gen: TGeneratorObj;
    GenkWChanged, Genkvarchanged: Boolean;
    GenkW, Genkvar: Double;

begin
     // If list is not define, go make one from all generators in circuit
    if FGenPointerList.ListSize = 0 then
        MakeGenList;

    if FListSize > 0 then
    begin

       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
        S := MonitoredElement.Power[ElementTerminal, ActorID];  // Power in active terminal

        PDiff := S.re * 0.001 - FkWLimit;

        QDiff := S.im * 0.001 - FkvarLimit;

       // Redispatch the vars.

        GenkWChanged := FALSE;
        GenkvarChanged := FALSE;

        if Abs(PDiff) > HalfkWBand then
        begin // Redispatch Generators
          // PDiff is kW needed to get back into band
            for i := 1 to FListSize do
            begin
                Gen := FGenPointerList.Get(i);
              // compute new dispatch value for this generator ...
                GenkW := Max(1.0, (Gen.kWBase + PDiff * (FWeights^[i] / TotalWeight)));
                if GenkW <> Gen.kWBase then
                begin
                    Gen.kWBase := GenkW;
                    GenkWChanged := TRUE;
                end;
            end;
        end;

        if Abs(QDiff) > HalfkWBand then
        begin // Redispatch Generators
          // QDiff is kvar needed to get back into band
            for i := 1 to FListSize do
            begin
                Gen := FGenPointerList.Get(i);
              // compute new dispatch value for this generator ...
                Genkvar := Max(0.0, (Gen.kvarBase + QDiff * (FWeights^[i] / TotalWeight)));
                if Genkvar <> Gen.kvarBase then
                begin
                    Gen.kvarBase := Genkvar;
                    Genkvarchanged := TRUE;
                end;
            end;
        end;

        if GenkWChanged or Genkvarchanged then  // Only push onto controlqueue if there has been a change
            with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
            begin
                LoadsNeedUpdating := TRUE; // Force recalc of power parms
            // Push present time onto control queue to force re solve at new dispatch value
                ControlQueue.Push(DynaVars.intHour, DynaVars.t, 0, 0, Self, ActorID);
            end;


       {Else just continue}
    end;


end;


procedure TGenDispatcherObj.InitPropertyValues(ArrayOffset: Integer);
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

function TGenDispatcherObj.MakeGenList: Boolean;

var
    GenClass: TDSSClass;
    Gen: TGeneratorObj;
    i: Integer;

begin

    Result := FALSE;
    GenClass := GetDSSClassPtr('generator');

    if FListSize > 0 then
    begin    // Name list is defined - Use it

        for i := 1 to FListSize do
        begin
            Gen := GenClass.Find(FGeneratorNameList.Strings[i - 1]);
            if Assigned(Gen) and Gen.Enabled then
                FGenPointerList.New := Gen;
        end;

    end
    else
    begin
     {Search through the entire circuit for enabled generators and add them to the list}

        for i := 1 to GenClass.ElementCount do
        begin
            Gen := GenClass.ElementList.Get(i);
            if Gen.Enabled then
                FGenPointerList.New := Gen;
        end;

     {Allocate uniform weights}
        FListSize := FGenPointerList.ListSize;
        Reallocmem(FWeights, Sizeof(FWeights^[1]) * FListSize);
        for i := 1 to FListSize do
            FWeights^[i] := 1.0;

    end;

   // Add up total weights
    TotalWeight := 0.0;
    for i := 1 to FlistSize do
        TotalWeight := TotalWeight + FWeights^[i];

    if FGenPointerList.ListSize > 0 then
        Result := TRUE;
end;


procedure TGenDispatcherObj.Reset;
begin
  // inherited;

end;


initialization


end.
