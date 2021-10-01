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
    DSSPointerList,
    Classes;

type

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TUPFCControl = class(TControlClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const UPFCControlName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TUPFCControlObj = class(TControlElem)
    PRIVATE
        FUPFCNameList: TStringList;
        FWeights: pDoubleArray;
        TotalWeight: Double;

    PUBLIC
        UPFCList: TDSSPointerList;
        ListSize: Integer;

        constructor Create(ParClass: TDSSClass; const UPFCControlName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;    // Always Zero for a UPFCControl

        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state

        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Get present value of terminal Curr
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(F: TFileStream; Complete: Boolean); OVERRIDE;

        function MakeUPFCList: Boolean;
    end;

{--------------------------------------------------------------------------}
implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    UPFC,
    Sysutils,
    uCmatrix,
    MathUtil,
    Math,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

const
    NumPropsThisClass = 1;

{--------------------------------------------------------------------------}
constructor TUPFCControl.Create(dssContext: TDSSContext);  // Creates superstructure for all UPFCControl objects
begin
    inherited Create(dssContext);

    Class_name := 'UPFCControl';
    DSSClassType := DSSClassType + UPFC_CONTROL;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

{--------------------------------------------------------------------------}
destructor TUPFCControl.Destroy;

begin
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TUPFCControl.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names

    PropertyName[1] := 'UPFCList';

    PropertyHelp[1] := 'The list of all the UPFC devices to be controlled by this controller, ' +
        'If left empty, this control will apply for all UPFCs in the model.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

{--------------------------------------------------------------------------}
function TUPFCControl.NewObject(const ObjName: String): Integer;
begin
    // Make a new UPFCControl and add it to UPFCControl class list
    with ActiveCircuit do
    begin
        ActiveCktElement := TUPFCControlObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

{--------------------------------------------------------------------------}
function TUPFCControl.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
begin

  // continue parsing WITH contents of Parser
    DSS.ActiveUPFCControlObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := DSS.ActiveUPFCControlObj;

    Result := 0;

    with DSS.ActiveUPFCControlObj do
    begin

        ParamPointer := 0;
        ParamName := Parser.NextParam;
        Param := Parser.StrValue;
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
                    InterpretTStringListArray(Param, FUPFCNameList);
            else
                // Inherited parameters
                ClassEdit(DSS.ActiveUPFCControlObj, ParamPointer - NumPropsthisClass)
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        // RecalcElementData;
    end;

end;


{--------------------------------------------------------------------------}
function TUPFCControl.MakeLike(const UPFCControlName: String): Integer;
var
    OtherUPFCControl: TUPFCControlObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this UPFCControl name in the present collection}
    OtherUPFCControl := Find(UPFCControlName);
    if OtherUPFCControl <> NIL then
        with DSS.ActiveUPFCControlObj do
        begin

            NPhases := OtherUPFCControl.Fnphases;
            NConds := OtherUPFCControl.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherUPFCControl.ElementName;
            ControlledElement := OtherUPFCControl.ControlledElement;  // Pointer to target circuit element
            MonitoredElement := OtherUPFCControl.MonitoredElement;  // Pointer to target circuit element

            ElementTerminal := OtherUPFCControl.ElementTerminal;


            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherUPFCControl.PropertyValue[i];

        end
    else
        DoSimpleMsg('Error in UPFCControl MakeLike: "' + UPFCControlName + '" Not Found.', 370);

end;


{==========================================================================}
{                    TUPFCControlObj                                           }
{==========================================================================}


{--------------------------------------------------------------------------}
constructor TUPFCControlObj.Create(ParClass: TDSSClass; const UPFCControlName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(UPFCControlName);
    DSSObjType := ParClass.DSSClassType;

    FUPFCNameList := TSTringList.Create;
    UPFCList := TDSSPointerList.Create(20);  // Default size and increment
    TotalWeight := 1.0;
    FWeights := NIL;
    ListSize := 0;

    // RecalcElementData;
end;

destructor TUPFCControlObj.Destroy;
begin
    ElementName := '';
    inherited Destroy;
end;

{--------------------------------------------------------------------------}
procedure TUPFCControlObj.RecalcElementData;
var
    DevIndex: Integer;
begin
    {Check for existence of monitored element}

    DevIndex := GetCktElementIndex(ElementName); // Global function
    if DevIndex <= 0 then
    begin
        DoSimpleMsg('Monitored Element in UPFCControl.' + Name + ' does not exist:"' + ElementName + '"', 372);
        Exit;
    end;

    MonitoredElement := ActiveCircuit.CktElements.Get(DevIndex);
    if ElementTerminal > MonitoredElement.Nterms then
    begin
        DoErrorMsg('UPFCControl: "' + Name + '"',
            'Terminal no. "' + '" does not exist.',
            'Re-specify terminal no.', 371);
    end
    else
    begin
        // Sets name of i-th terminal's connected bus in UPFCControl's buslist
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    end;
end;

procedure TUPFCControlObj.MakePosSequence;
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
procedure TUPFCControlObj.CalcYPrim;
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;


{--------------------------------------------------------------------------}
procedure TUPFCControlObj.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;

{--------------------------------------------------------------------------}
procedure TUPFCControlObj.DumpProperties(F: TFileStream; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[i]);
        end;

    if Complete then
    begin
        FSWriteln(F);
    end;

end;


{--------------------------------------------------------------------------}
procedure TUPFCControlObj.DoPendingAction;
var
    i: Integer;
    obj: TUPFCObj;
begin
    if ListSize <= 0 Then
        Exit;

    for i := 1 to ListSize do
    begin
        obj := UPFCList.Get(i);
        obj.UploadCurrents();
    end;
end;

{--------------------------------------------------------------------------}
procedure TUPFCControlObj.Sample;
var
    Update: Boolean;
    i: Integer;
    obj: TUPFCObj;
begin
    // If list is not define, go make one from all UPFCs in circuit
    if UPFCList.Count = 0 then
        MakeUPFCList;

    Update := False;

    if ListSize <= 0 then
        Exit;

    for i := 1 to ListSize do
    begin
        obj := UPFCList.Get(i);
        Update := Update or obj.CheckStatus();
    end;

    {Checks if at least one UPFC needs to be updated}
    if Update then
    begin
        with ActiveCircuit, ActiveCircuit.Solution do
            ControlQueue.Push(DynaVars.intHour, DynaVars.t, 0, 0, Self);
    end;
end;


procedure TUPFCControlObj.InitPropertyValues(ArrayOffset: Integer);
begin
    PropertyValue[1] := '[]';   //'UPFC List';

    inherited InitPropertyValues(NumPropsThisClass);
end;

function TUPFCControlObj.MakeUPFCList: Boolean;
var
    obj: TUPFCObj;
    i: Integer;
begin
    Result := FALSE;
  
    // Clears everything
    FUPFCNameList.Clear;
    UPFCList.Clear;

    if ListSize > 0 then
    begin    // Name list is defined - Use it
        for i := 1 to ListSize do 
        begin
            obj := DSS.UPFCClass.Find(FUPFCNameList.Strings[i - 1]);
            if Assigned(obj) and obj.Enabled then 
                UPFCList.New := obj;
        end;
    end
    else  // No list given
    begin
        {Search through the entire circuit for enabled UPFCs and add them to the list}

        for i := 1 to DSS.UPFCClass.ElementCount do
        begin
            obj := DSS.UPFCClass.ElementList.Get(i);
         
            // Checks if it's enabled
            if obj.Enabled then
                UPFCList.New := obj;
        end;

        {Allocate uniform weights}
        ListSize := UPFCList.Count;
        Reallocmem(FWeights, Sizeof(FWeights^[1]) * ListSize);
        for i := 1 to ListSize do 
            FWeights^[i] := 1.0;
    end;

    // Add up total weights
    TotalWeight := 0.0;
    for i := 1 to ListSize do
        TotalWeight := TotalWeight + FWeights^[i];

    if UPFCList.Count > 0 then 
        Result := TRUE;

end;


procedure TUPFCControlObj.Reset;
begin
  // inherited;

end;


initialization


end.
