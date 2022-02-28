unit UPFCControl;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

//  A UPFCControl is a control element that is connected to a terminal of another
//  circuit element and sends dispatch kW signals to a set of generators it controls
//
//  A UPFCControl is defined by a New command:
//
//  New UPFCControl.Name=myname Element=devclass.name terminal=[ 1|2|...] CapacitorList = (gen1  gen2 ...)

interface

uses
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    Arraydef,
    UComplex, DSSUcomplex,
    utilities,
    DSSPointerList,
    Classes;

type
{$SCOPEDENUMS ON}
    TUPFCControlProp = (
        INVALID = 0,
        UPFCList = 1
    );
{$SCOPEDENUMS OFF}

    TUPFCControl = class(TControlClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

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
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData; OVERRIDE;

        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state

        function MakeUPFCList: Boolean;
    end;

implementation

uses
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

type
    TObj = TUPFCControlObj;
    TProp = TUPFCControlProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TUPFCControl.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, UPFC_CONTROL, 'UPFCControl');
end;

destructor TUPFCControl.Destroy;
begin
    inherited Destroy;
end;

procedure TUPFCControl.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // string lists
    PropertyType[ord(TProp.UPFCList)] := TPropertyType.StringListProperty;
    PropertyOffset[ord(TProp.UPFCList)] := ptruint(@obj.FUPFCNameList);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TUPFCControl.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TUPFCControlObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff

    ControlledElement := Other.ControlledElement;  // Pointer to target circuit element
    MonitoredElement := Other.MonitoredElement;  // Pointer to target circuit element

    ElementTerminal := Other.ElementTerminal;
end;

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
    inherited Destroy;
end;

procedure TUPFCControlObj.RecalcElementData;
begin
end;

procedure TUPFCControlObj.MakePosSequence();
begin
    if MonitoredElement <> NIL then
    begin
        FNphases := ControlledElement.NPhases;
        Nconds := FNphases;
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    end;
    inherited;
end;

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
            obj := ParentClass.Find(FUPFCNameList.Strings[i - 1]);
            if Assigned(obj) and obj.Enabled then 
                UPFCList.Add(obj);
        end;
    end
    else  // No list given
    begin
        {Search through the entire circuit for enabled UPFCs and add them to the list}

        for i := 1 to ParentClass.ElementCount do
        begin
            obj := ParentClass.ElementList.Get(i);
         
            // Checks if it's enabled
            if obj.Enabled then
                UPFCList.Add(obj);
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

end.