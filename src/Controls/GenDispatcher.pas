unit GenDispatcher;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
//  A GenDispatcher is a control element that is connected to a terminal of another
//  circuit element and sends dispatch kW signals to a set of generators it controls
//
//  A GenDispatcher is defined by a New command:
//
//  New GenDispatcher.Name=myname Element=devclass.name terminal=[ 1|2|...] CapacitorList = (gen1  gen2 ...)

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
    TGenDispatcherProp = (
        INVALID = 0,
        Element = 1,
        Terminal = 2,
        kWLimit = 3,
        kWBand = 4,
        kvarlimit = 5,
        GenList = 6,
        Weights = 7
     );
{$SCOPEDENUMS OFF}

    TGenDispatcher = class(TControlClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TGenDispatcherObj = class(TControlElem)
    PRIVATE
        FkWLimit,
        FkWBand,
        HalfkWBand,
        FkvarLimit,
        TotalWeight: Double;
        FListSize: Integer;
        FGeneratorNameList: TStringList;
        FGenPointerList: DSSPointerList.TDSSPointerList;
        FWeights: pDoubleArray;

    PUBLIC
        constructor Create(ParClass: TDSSClass; const GenDispatcherName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData; OVERRIDE;

        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state

        function MakeGenList: Boolean;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Generator,
    Sysutils,
    uCmatrix,
    MathUtil,
    Math,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TGenDispatcherObj;
    TProp = TGenDispatcherProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TGenDispatcher.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, GEN_CONTROL, 'GenDispatcher');
end;

destructor TGenDispatcher.Destroy;
begin
    inherited Destroy;
end;

procedure TGenDispatcher.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // string list
    PropertyType[ord(TProp.GenList)] := TPropertyType.StringListProperty;
    PropertyOffset[ord(TProp.GenList)] := ptruint(@obj.FGeneratorNameList);

    // double arrays
    PropertyType[ord(TProp.Weights)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.Weights)] := ptruint(@obj.FWeights);
    PropertyOffset2[ord(TProp.Weights)] := ptruint(@obj.FListSize);

    // object reference
    PropertyType[ord(TProp.Element)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.Element)] := ptruint(@obj.FMonitoredElement);
    PropertyOffset2[ord(TProp.Element)] := 0;
    PropertyWriteFunction[ord(TProp.Element)] := @SetMonitoredElement;
    PropertyFlags[ord(TProp.Element)] := [TPropertyFlag.WriteByFunction];//[TPropertyFlag.CheckForVar]; // not required for general cktelements

    // integer properties
    PropertyType[ord(TProp.Terminal)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Terminal)] := ptruint(@obj.ElementTerminal);

    // double properties (default type)
    PropertyOffset[ord(TProp.kWLimit)] := ptruint(@obj.FkWLimit);
    PropertyOffset[ord(TProp.kWBand)] := ptruint(@obj.FkWBand);
    PropertyOffset[ord(TProp.kvarlimit)] := ptruint(@obj.FkvarLimit);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TGenDispatcher.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TGenDispatcherObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    i: Integer;
begin
    case Idx of
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
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TGenDispatcherObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff
    // ControlledElement := Other.ControlledElement;  // Pointer to target circuit element
    MonitoredElement := Other.MonitoredElement;  // Pointer to target circuit element
    ElementTerminal := Other.ElementTerminal;
end;

constructor TGenDispatcherObj.Create(ParClass: TDSSClass; const GenDispatcherName: String);
begin
    inherited Create(ParClass);
    Name := LowerCase(GenDispatcherName);
    DSSObjType := ParClass.DSSClassType;

    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class

    ControlledElement := NIL;  // not used in this control
    ElementTerminal := 1;
    MonitoredElement := NIL;

    FGeneratorNameList := TSTringList.Create;
    FWeights := NIL;
    FGenPointerList := TDSSPointerList.Create(20);  // Default size and increment
    FListSize := 0;
    FkWLimit := 8000.0;
    FkWBand := 100.0;
    TotalWeight := 1.0;
    HalfkWBand := FkWBand / 2.0;
    FkvarLimit := FkWLimit / 2.0;

   //  RecalcElementData;
end;

destructor TGenDispatcherObj.Destroy;
begin
    inherited Destroy;
end;

procedure TGenDispatcherObj.RecalcElementData;
begin
    {Check for existence of monitored element}
    if MonitoredElement <> NIL then
    begin
        if ElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg(Format(_('GenDispatcher: "%s"'), [Name]),
                Format(_('Terminal no. "%d" does not exist.'), [ElementTerminal]),
                _('Re-specify terminal no.'), 371);
        end
        else
        begin
               // Sets name of i-th terminal's connected bus in GenDispatcher's buslist
            Setbus(1, MonitoredElement.GetBus(ElementTerminal));
        end;
    end
    else
        DoSimpleMsg('Monitored Element in %s is not set', [FullName], 372);
end;

procedure TGenDispatcherObj.MakePosSequence();
begin
    if MonitoredElement <> NIL then
    begin
        FNphases := ControlledElement.NPhases;
        Nconds := FNphases;
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    end;
    inherited;
end;

procedure TGenDispatcherObj.DoPendingAction;
begin
        {Do Nothing}
end;

procedure TGenDispatcherObj.Sample;
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
    if FGenPointerList.Count = 0 then
        MakeGenList;

    if FListSize > 0 then
    begin
       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
        S := MonitoredElement.Power[ElementTerminal];  // Power in active terminal

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
            with ActiveCircuit, ActiveCircuit.Solution do
            begin
                LoadsNeedUpdating := TRUE; // Force recalc of power parms
            // Push present time onto control queue to force re solve at new dispatch value
                ControlQueue.Push(DynaVars.intHour, DynaVars.t, 0, 0, Self);
            end;
       {Else just continue}
    end;
end;

function TGenDispatcherObj.MakeGenList: Boolean;
var
    GenClass: TDSSClass;
    Gen: TGeneratorObj;
    i: Integer;

begin
    Result := FALSE;
    GenClass := GetDSSClassPtr(DSS, 'generator');

    if FListSize > 0 then
    begin    // Name list is defined - Use it
        for i := 1 to FListSize do
        begin
            Gen := GenClass.Find(FGeneratorNameList.Strings[i - 1]);
            if Assigned(Gen) and Gen.Enabled then
                FGenPointerList.Add(Gen);
        end;
    end
    else
    begin
        // Search through the entire circuit for enabled generators and add them to the list
        for i := 1 to GenClass.ElementCount do
        begin
            Gen := GenClass.ElementList.Get(i);
            if Gen.Enabled then
                FGenPointerList.Add(Gen);
        end;
        // Allocate uniform weights
        FListSize := FGenPointerList.Count;
        Reallocmem(FWeights, Sizeof(FWeights^[1]) * FListSize);
        for i := 1 to FListSize do
            FWeights^[i] := 1.0;
    end;

    // Add up total weights
    TotalWeight := 0.0;
    for i := 1 to FlistSize do
        TotalWeight := TotalWeight + FWeights^[i];

    if FGenPointerList.Count > 0 then
        Result := TRUE;
end;


procedure TGenDispatcherObj.Reset;
begin
  // inherited;
end;

end.