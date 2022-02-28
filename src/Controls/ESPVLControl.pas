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
    UComplex, DSSUcomplex,
    utilities,
    DSSPointerList,
    Classes,
    Loadshape;

type
{$SCOPEDENUMS ON}
    TESPVLControlProp = (
        INVALID = 0,
        Element = 1,
        Terminal = 2,
        Typ = 3,
        kWBand = 4,
        kvarlimit = 5,
        LocalControlList = 6,
        LocalControlWeights = 7,
        PVSystemList = 8,
        PVSystemWeights = 9, 
        StorageList = 10,
        StorageWeights = 11
        // Forecast = 12 -- Unused
    );
{$SCOPEDENUMS OFF}

    TESPVLControl = class(TControlClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TESPVLControlObj = class(TControlElem)
    PRIVATE

        Ftype: Integer;   {1=System controller; 2=Local controller}

        {System Controller Variables}

        // Local Controllers under supervision of System Controller
        FLocalControlListSize: Integer;
        FLocalControlNameList: TStringList;
        FLocalControlPointerList: TDSSPointerList;
        FLocalControlWeights: pDoubleArray;

        {Local Controller Variables}

        // PVSystems under supervision of this Local Controller
        FPVsystemListSize: Integer;
        FPVsystemNameList: TStringList;
        FPVsystemPointerList: TDSSPointerList;
        FPVSystemWeights: pDoubleArray;

        // Storage Devices under supervision of this Local Controller
        FStorageListSize: Integer;
        FStorageNameList: TStringList;
        FStoragePointerList: TDSSPointerList;
        FStorageWeights: pDoubleArray;

        // dead band control parameters
        FkWLimit,
        FkWBand,
        HalfkWBand,
        FkvarLimit,
        TotalWeight: Double;

    PUBLIC

        constructor Create(ParClass: TDSSClass; const ESPVLControlName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData; OVERRIDE;

        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state

        function MakeLocalControlList: Boolean;
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
    TObj = TESPVLControlObj;
    TProp = TESPVLControlProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    TypeEnum: TDSSEnum;
    PropInfo: Pointer = NIL;    

constructor TESPVLControl.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        TypeEnum := TDSSEnum.Create('ESPVLControl: Type', True, 1, 1, 
            ['SystemController', 'LocalController'], [1, 2]);
    end;
    inherited Create(dssContext, ESPVL_CONTROL, 'ESPVLControl');
end;

destructor TESPVLControl.Destroy;
begin
    inherited Destroy;
end;

procedure TESPVLControl.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // object references
    PropertyType[ord(TProp.element)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.element)] := ptruint(@obj.FMonitoredElement);
    PropertyOffset2[ord(TProp.element)] := 0;
    PropertyWriteFunction[ord(TProp.element)] := @SetMonitoredElement;
    PropertyFlags[ord(TProp.element)] := [TPropertyFlag.WriteByFunction];//[TPropertyFlag.CheckForVar]; // not required for general cktelements

    // enum properties
    PropertyType[ord(TProp.typ)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.typ)] := ptruint(@obj.Ftype);
    PropertyOffset2[ord(TProp.typ)] := PtrInt(TypeEnum);

    // string lists
    PropertyType[ord(TProp.PVSystemList)] := TPropertyType.StringListProperty;
    PropertyOffset[ord(TProp.PVSystemList)] := ptruint(@obj.FPVSystemNameList);

    PropertyType[ord(TProp.LocalControlList)] := TPropertyType.StringListProperty;
    PropertyOffset[ord(TProp.LocalControlList)] := ptruint(@obj.FLocalControlNameList);

    PropertyType[ord(TProp.StorageList)] := TPropertyType.StringListProperty;
    PropertyOffset[ord(TProp.StorageList)] := ptruint(@obj.FStorageNameList);

    // double arrays
    PropertyType[ord(TProp.LocalControlWeights)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.LocalControlWeights)] := ptruint(@obj.FLocalControlWeights);
    PropertyOffset2[ord(TProp.LocalControlWeights)] := ptruint(@obj.FLocalControlListSize); // FLocalControlNameList.count

    PropertyType[ord(TProp.PVSystemWeights)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.PVSystemWeights)] := ptruint(@obj.FPVSystemWeights);
    PropertyOffset2[ord(TProp.PVSystemWeights)] := ptruint(@obj.FPVSystemListSize);

    PropertyType[ord(TProp.StorageWeights)] := TPropertyType.DoubleArrayProperty;
    PropertyOffset[ord(TProp.StorageWeights)] := ptruint(@obj.FStorageWeights);
    PropertyOffset2[ord(TProp.StorageWeights)] := ptruint(@obj.FStorageListSize);

    // integer properties
    PropertyType[ord(TProp.Terminal)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Terminal)] := ptruint(@obj.ElementTerminal);

    // double properties (default type)
    PropertyOffset[ord(TProp.kWBand)] := ptruint(@obj.FkWBand);
    PropertyOffset[ord(TProp.kvarlimit)] := ptruint(@obj.FkvarLimit);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TESPVLControl.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TESPVLControlObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    i: Integer;
begin
    case Idx of
        8:
        begin
            FPVSystemListSize := FPVSystemNameList.count;
            if FPVSystemWeights <> NIL then
                Reallocmem(FPVSystemWeights, Sizeof(Double) * FPVSystemListSize);
        end;
        10:
        begin
            FStorageListSize := FStorageNameList.count;
            if FStorageWeights <> NIL then
                Reallocmem(FStorageWeights, Sizeof(Double) * FStorageListSize);                
        end;
        6:
        begin   // levelize the list
            FLocalControlPointerList.Clear;  // clear this for resetting on first sample
            FLocalControlListSize := FLocalControlNameList.count;
            Reallocmem(FLocalControlWeights, Sizeof(FLocalControlWeights^[1]) * FLocalControlListSize);
            for i := 1 to FLocalControlListSize do
                FLocalControlWeights^[i] := 1.0;
        end;
    end;

    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TESPVLControlObj.MakeLike(OtherPtr: Pointer);
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

constructor TESPVLControlObj.Create(ParClass: TDSSClass; const ESPVLControlName: String);
begin
    inherited Create(ParClass);
    Name := LowerCase(ESPVLControlName);
    DSSObjType := ParClass.DSSClassType;

    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class

    ControlledElement := NIL;  // not used in this control
    ElementTerminal := 1;
    MonitoredElement := NIL;

    FLocalControlNameList := TSTringList.Create;
    FLocalControlWeights := NIL;
    FLocalControlPointerList := TDSSPointerList.Create(20);  // Default size and increment
    FLocalControlListSize := 0;

    FPVSystemNameList := TSTringList.Create;
    FPVSystemWeights := NIL;
    FPVSystemPointerList := TDSSPointerList.Create(20);  // Default size and increment
    FPVSystemListSize := 0;

    FStorageNameList := TSTringList.Create;
    FStorageWeights := NIL;
    FStoragePointerList := TDSSPointerList.Create(20);  // Default size and increment
    FStorageListSize := 0;

    FkWLimit := 8000.0;
    FkWBand := 100.0;
    TotalWeight := 1.0;
    HalfkWBand := FkWBand / 2.0;
    FkvarLimit := FkWLimit / 2.0;

   //  RecalcElementData;
end;

destructor TESPVLControlObj.Destroy;
begin
    inherited Destroy;
end;

procedure TESPVLControlObj.RecalcElementData;
begin
    {Check for existence of monitored element}
    if MonitoredElement = NIL then
    begin
        DoSimpleMsg('Monitored Element in "%s" is not set', [FullName], 372);
        Exit;
    end;

    if ElementTerminal > MonitoredElement.Nterms then
    begin
        DoErrorMsg(Format(_('ESPVLControl: "%s"'), [Name]),
            Format(_('Terminal no. "%d" does not exist.'), [ElementTerminal]),
            _('Re-specify terminal no.'), 371);
    end
    else
    begin
        // Sets name of i-th terminal's connected bus in ESPVLControl's buslist
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    end;
end;

procedure TESPVLControlObj.MakePosSequence();
begin
    if MonitoredElement <> NIL then
    begin
        FNphases := ControlledElement.NPhases;
        Nconds := FNphases;
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    end;
    inherited;
end;

procedure TESPVLControlObj.DoPendingAction;
begin
    {Do Nothing}
end;

procedure TESPVLControlObj.Sample;
var
    i: Integer;
    PDiff: Double;
    S: Complex;
    Gen: TGeneratorObj;
    GenkW: Double;
begin
     // If list is not define, go make one from all generators in circuit
    if FLocalControlPointerList.Count = 0 then
        MakeLocalControlList;

    if FLocalControlListSize > 0 then
    begin
        S := MonitoredElement.Power[ElementTerminal];  // Power in active terminal

        PDiff := S.re * 0.001 - FkWLimit;

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
                end;
            end;
        end;
       {Else just continue}
    end;
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
                pESPVLControl := ParentClass.Find(FLocalControlNameList.Strings[i - 1]);
                if Assigned(pESPVLControl) and pESPVLControl.Enabled then
                    FLocalControlPointerList.Add(pESPVLControl);
            end;
        end
        else
        begin
            {Search through the entire circuit for enabled generators and add them to the list}
            for i := 1 to ParentClass.ElementCount do
            begin
                pESPVLControl := ParentClass.ElementList.Get(i);
                if pESPVLControl.Enabled then
                    FLocalControlPointerList.Add(pESPVLControl);
            end;

            {Allocate uniform weights}
            FLocalControlListSize := FLocalControlPointerList.Count;
            Reallocmem(FLocalControlWeights, Sizeof(FLocalControlWeights^[1]) * FLocalControlListSize);
            for i := 1 to FLocalControlListSize do
                FLocalControlWeights^[i] := 1.0;

        end;

        // Add up total weights    ??????
        TotalWeight := 0.0;
        for i := 1 to FLocalControlListSize do
            TotalWeight := TotalWeight + FLocalControlWeights^[i];

        if FLocalControlPointerList.Count > 0 then
            Result := TRUE;
    end;
end;

procedure TESPVLControlObj.Reset;
begin
  // inherited;
end;

finalization    TypeEnum.Free;
end.
