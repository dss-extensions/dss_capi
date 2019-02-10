unit ESPVLControl;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2016, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
  An ESPVLControl is a control element that is connected to a terminal of another
  circuit element (a PVSystem) and sends dispatch kW signals to a set of Storage Elements it controls

  An ESPVLControl is either a System Controller or a Local Controller, set by the "Type" property.
  A System Controller controls one or more Local Controllers
  A Local Controller controls one or more PVSystem elements and one or more Storage elements.

  An ESPVLControl is defined by a New command:

  New ESPVLControl.Name=myname Element=devclass.name terminal=[ 1|2|...] StorageList = (gen1  gen2 ...)

 
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
    Classes,
    Loadshape;

type

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TESPVLControl = class(TControlClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const ESPVLControlName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TESPVLControlObj = class(TControlElem)
    PRIVATE

        Ftype: Integer;   {1=System controller; 2=Local controller}

     {System Controller Variables}

            // Local Controllers under supervision of System Controller
        FLocalControlListSize: Integer;
        FLocalControlNameList: TStringList;
        FLocalControlPointerList: PointerList.TPointerList;
        FLocalControlWeights: pDoubleArray;


     {Local Controller Variables}

             // PVSystems under supervision of this Local Controller
        FPVsystemListSize: Integer;
        FPVsystemNameList: TStringList;
        FPVsystemPointerList: PointerList.TPointerList;
        FPVSystemWeights: pDoubleArray;

             // Storage Devices under supervision of this Local Controller
        FStorageListSize: Integer;
        FStorageNameList: TStringList;
        FStoragePointerList: PointerList.TPointerList;
        FStorageWeights: pDoubleArray;

// dead band control parameters
        FkWLimit,
        FkWBand,
        HalfkWBand,
        FkvarLimit,
        TotalWeight: Double;


  //          YearlyShape     :String;  // ='fixed' means no variation  on all the time
   //         YearlyShapeObj  :TLoadShapeObj;  // Shape for this Storage element
        DailyForecastShape: String;  // Daily (24 HR) Storage element shape
        DailyForecasstShapeObj: TLoadShapeObj;  // Daily Storage element Shape for this load
  //          DutyShape       :String;  // Duty cycle load shape for changes typically less than one hour
  //          DutyShapeObj    :TLoadShapeObj;  // Shape for this Storage element

        LoadShapeMult: Complex;

        MonitoredElement: TDSSCktElement;

    PUBLIC

        constructor Create(ParClass: TDSSClass; const ESPVLControlName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;    // Always Zero for a ESPVLControl

        procedure Sample(ActorID: Integer); OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state

        procedure GetCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE; // Get present value of terminal Curr
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;   // Returns Injextion currents

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

        function MakeLocalControlList: Boolean;
    end;


var
    ESPVLControlClass: TESPVLControl;
    ActiveESPVLControlObj: TESPVLControlObj;

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

    NumPropsThisClass = 12;


{--------------------------------------------------------------------------}
constructor TESPVLControl.Create;  // Creates superstructure for all ESPVLControl objects
begin
    inherited Create;

    Class_name := 'ESPVLControl';
    DSSClassType := DSSClassType + ESPVL_CONTROL;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
    ESPVLControlClass := Self;
end;

{--------------------------------------------------------------------------}
destructor TESPVLControl.Destroy;

begin
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TESPVLControl.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

     // Define Property names

    PropertyName[1] := 'Element';
    PropertyName[2] := 'Terminal';
    PropertyName[3] := 'Type';
    PropertyName[4] := 'kWBand';
    PropertyName[5] := 'kvarlimit';
    PropertyName[6] := 'LocalControlList';
    PropertyName[7] := 'LocalControlWeights';
    PropertyName[8] := 'PVSystemList';
    PropertyName[9] := 'PVSystemWeights';
    PropertyName[10] := 'StorageList';
    PropertyName[11] := 'StorageWeights';
    PropertyName[12] := 'Forecast';

    PropertyHelp[1] := 'Full object name of the circuit element, typically a line or transformer, ' +
        'which the control is monitoring. There is no default; must be specified.';
    PropertyHelp[2] := 'Number of the terminal of the circuit element to which the ESPVLControl control is connected. ' +
        '1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit.';
    PropertyHelp[3] := 'Type of controller.  1= System Controller; 2= Local controller. ';
    PropertyHelp[4] := 'Bandwidth (kW) of the dead band around the target limit.' +
        'No dispatch changes are attempted if the power in the monitored terminal stays within this band.';
    PropertyHelp[5] := 'Max kvar to be delivered through the element.  Uses same dead band as kW.';
    PropertyHelp[6] := 'Array list of ESPVLControl local controller objects to be dispatched by System Controller. ' +
        'If not specified, all ESPVLControl devices with type=local in the circuit not attached to another ' +
        'controller are assumed to be part of this controller''s fleet.';
    PropertyHelp[7] := 'Array of proportional weights corresponding to each ESPVLControl local controller in the LocalControlList.';
    ;
    PropertyHelp[8] := 'Array list of PVSystem objects to be dispatched by a Local Controller. ';
    PropertyHelp[9] := 'Array of proportional weights corresponding to each PVSystem in the PVSystemList.';
    ;
    PropertyHelp[10] := 'Array list of Storage objects to be dispatched by Local Controller. ';
    PropertyHelp[11] := 'Array of proportional weights corresponding to each Storage object in the StorageControlList.';
    PropertyHelp[12] := 'Loadshape object containing daily forecast.';
    ;

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

{--------------------------------------------------------------------------}
function TESPVLControl.NewObject(const ObjName: String): Integer;
begin
    // Make a new ESPVLControl and add it to ESPVLControl class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TESPVLControlObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

{--------------------------------------------------------------------------}
function TESPVLControl.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    i: Integer;

begin

  // continue parsing WITH contents of Parser
    ActiveESPVLControlObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveESPVLControlObj;

    Result := 0;

    with ActiveESPVLControlObj do
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
                    case Lowercase(Param)[1] of
                        's':
                            Ftype := 1;    {for System Controller}
                        'l':
                            Ftype := 2;    {for Local Controller}
                    end;


                4:
                    FkWBand := Parser[ActorID].DblValue;
                5:
                    FkvarLimit := Parser[ActorID].DblValue;
                6:
                    InterpretTStringListArray(Param, FLocalControlNameList);
                7:
                begin
                    FLocalControlListSize := FLocalControlNameList.count;
                    if FLocalControlListSize > 0 then
                    begin
                        Reallocmem(FLocalControlWeights, Sizeof(FLocalControlWeights^[1]) * FLocalControlListSize);
                        InterpretDblArray(Param, FLocalControlListSize, FLocalControlWeights);
                    end;
                end;
                8:
                    InterpretTStringListArray(Param, FPVSystemNameList);
                9:
                begin
                    FPVSystemListSize := FPVSystemNameList.count;
                    if FPVSystemListSize > 0 then
                    begin
                        Reallocmem(FPVSystemWeights, Sizeof(FPVSystemWeights^[1]) * FPVSystemListSize);
                        InterpretDblArray(Param, FPVSystemListSize, FPVSystemWeights);
                    end;
                end;
                10:
                    InterpretTStringListArray(Param, FStorageNameList);
                11:
                begin
                    FStorageListSize := FStorageNameList.count;
                    if FStorageListSize > 0 then
                    begin
                        Reallocmem(FStorageWeights, Sizeof(FStorageWeights^[1]) * FStorageListSize);
                        InterpretDblArray(Param, FStorageListSize, FStorageWeights);
                    end;
                end;

            else
           // Inherited parameters
                ClassEdit(ActiveESPVLControlObj, ParamPointer - NumPropsthisClass)
            end;

         // Side Effects
            case ParamPointer of
                6:
                begin   // levelize the list
                    FLocalControlPointerList.Clear;  // clear this for resetting on first sample
                    FLocalControlListSize := FLocalControlNameList.count;
                    Reallocmem(FLocalControlWeights, Sizeof(FLocalControlWeights^[1]) * FLocalControlListSize);
                    for i := 1 to FLocalControlListSize do
                        FLocalControlWeights^[i] := 1.0;
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
function TESPVLControl.MakeLike(const ESPVLControlName: String): Integer;
var
    OtherESPVLControl: TESPVLControlObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this ESPVLControl name in the present collection}
    OtherESPVLControl := Find(ESPVLControlName);
    if OtherESPVLControl <> NIL then
        with ActiveESPVLControlObj do
        begin

            NPhases := OtherESPVLControl.Fnphases;
            NConds := OtherESPVLControl.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherESPVLControl.ElementName;
            ControlledElement := OtherESPVLControl.ControlledElement;  // Pointer to target circuit element
            MonitoredElement := OtherESPVLControl.MonitoredElement;  // Pointer to target circuit element

            ElementTerminal := OtherESPVLControl.ElementTerminal;


            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherESPVLControl.PropertyValue[i];

        end
    else
        DoSimpleMsg('Error in ESPVLControl MakeLike: "' + ESPVLControlName + '" Not Found.', 370);

end;


{==========================================================================}
{                    TESPVLControlObj                                           }
{==========================================================================}


{--------------------------------------------------------------------------}
constructor TESPVLControlObj.Create(ParClass: TDSSClass; const ESPVLControlName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(ESPVLControlName);
    DSSObjType := ParClass.DSSClassType;

    NPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class


    ElementName := '';
    ControlledElement := NIL;  // not used in this control
    ElementTerminal := 1;
    MonitoredElement := NIL;

    FLocalControlNameList := TSTringList.Create;
    FLocalControlWeights := NIL;
    FLocalControlPointerList := PointerList.TPointerList.Create(20);  // Default size and increment
    FLocalControlListSize := 0;

    FPVSystemNameList := TSTringList.Create;
    FPVSystemWeights := NIL;
    FPVSystemPointerList := PointerList.TPointerList.Create(20);  // Default size and increment
    FPVSystemListSize := 0;

    FStorageNameList := TSTringList.Create;
    FStorageWeights := NIL;
    FStoragePointerList := PointerList.TPointerList.Create(20);  // Default size and increment
    FStorageListSize := 0;

    FkWLimit := 8000.0;
    FkWBand := 100.0;
    TotalWeight := 1.0;
    HalfkWBand := FkWBand / 2.0;
    InitPropertyValues(0);
    FkvarLimit := FkWLimit / 2.0;


   //  RecalcElementData;

end;

destructor TESPVLControlObj.Destroy;
begin
    ElementName := '';
    inherited Destroy;
end;

{--------------------------------------------------------------------------}
procedure TESPVLControlObj.RecalcElementData(ActorID: Integer);

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
            DoErrorMsg('ESPVLControl: "' + Name + '"',
                'Terminal no. "' + '" does not exist.',
                'Re-specify terminal no.', 371);
        end
        else
        begin
               // Sets name of i-th terminal's connected bus in ESPVLControl's buslist
            Setbus(1, MonitoredElement.GetBus(ElementTerminal));
        end;
    end
    else
        DoSimpleMsg('Monitored Element in ESPVLControl.' + Name + ' does not exist:"' + ElementName + '"', 372);


end;

procedure TESPVLControlObj.MakePosSequence;
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
procedure TESPVLControlObj.CalcYPrim;
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;


{--------------------------------------------------------------------------}
procedure TESPVLControlObj.GetCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;

procedure TESPVLControlObj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

{--------------------------------------------------------------------------}
procedure TESPVLControlObj.DumpProperties(var F: TextFile; Complete: Boolean);

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
procedure TESPVLControlObj.DoPendingAction;
begin

        {Do Nothing}
end;

{--------------------------------------------------------------------------}
procedure TESPVLControlObj.Sample;

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
    if FLocalControlPointerList.ListSize = 0 then
        MakeLocalControlList;

    if FLocalControlListSize > 0 then
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
            for i := 1 to FLocalControlListSize do
            begin
                Gen := FLocalControlPointerList.Get(i);
              // compute new dispatch value for this generator ...
                GenkW := Max(1.0, (Gen.kWBase + PDiff * (FLocalControlWeights^[i] / TotalWeight)));
                if GenkW <> Gen.kWBase then
                begin
                    Gen.kWBase := GenkW;
                    GenkWChanged := TRUE;
                end;
            end;
        end;
      (*
       If Abs(QDiff) > HalfkWBand Then Begin // Redispatch Generators
          // QDiff is kvar needed to get back into band
          For i := 1 to FLocalControlListSize Do Begin
              Gen := FLocalControlPointerList.Get(i);
              // compute new dispatch value for this generator ...
              Genkvar := Max(0.0, (Gen.kvarBase + QDiff *(FWeights^[i]/TotalWeight)));
              If Genkvar <> Gen.kvarBase Then Begin
                  Gen.kvarBase := Genkvar;
                  Genkvarchanged := TRUE;
              End;
          End;
       End;

       If GenkWChanged or Genkvarchanged Then  // Only push onto controlqueue if there has been a change
          With ActiveCircuit, ActiveCircuit.Solution Do Begin
            LoadsNeedUpdating := TRUE; // Force recalc of power parms
            // Push present time onto control queue to force re solve at new dispatch value
            ControlQueue.Push(DynaVars.intHour, DynaVars.t, 0, 0, Self);
          End;
      *)

       {Else just continue}
    end;


end;


procedure TESPVLControlObj.InitPropertyValues(ArrayOffset: Integer);
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

function TESPVLControlObj.MakeLocalControlList: Boolean;

var
    pESPVLControl: TESPVLControlObj;
    i: Integer;

begin

    Result := FALSE;
    if Ftype = 1 then
    begin    // only for System controller


        if FLocalControlListSize > 0 then
        begin    // Name list is defined - Use it

            for i := 1 to FLocalControlListSize do
            begin
                pESPVLControl := ESPVLControlClass.Find(FLocalControlNameList.Strings[i - 1]);
                if Assigned(pESPVLControl) and pESPVLControl.Enabled then
                    FLocalControlPointerList.New := pESPVLControl;
            end;

        end
        else
        begin
         {Search through the entire circuit for enabled generators and add them to the list}

            for i := 1 to ESPVLControlClass.ElementCount do
            begin
                pESPVLControl := ESPVLControlClass.ElementList.Get(i);
                if pESPVLControl.Enabled then
                    FLocalControlPointerList.New := pESPVLControl;
            end;

         {Allocate uniform weights}
            FLocalControlListSize := FLocalControlPointerList.ListSize;
            Reallocmem(FLocalControlWeights, Sizeof(FLocalControlWeights^[1]) * FLocalControlListSize);
            for i := 1 to FLocalControlListSize do
                FLocalControlWeights^[i] := 1.0;

        end;

       // Add up total weights    ??????
        TotalWeight := 0.0;
        for i := 1 to FLocalControlListSize do
            TotalWeight := TotalWeight + FLocalControlWeights^[i];

        if FLocalControlPointerList.ListSize > 0 then
            Result := TRUE;
    end;
end;


procedure TESPVLControlObj.Reset;
begin
  // inherited;

end;


initialization


end.
