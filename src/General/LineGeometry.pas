unit LineGeometry;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

// The LineGeometry object is a general DSS object used by all circuits
// as a reference for obtaining line impedances.
//
// The values are set by the normal New and Edit procedures for any DSS object.
//
// The values are retrieved by setting the Code Property in the LineGeometry Class.
// This sets the active LineGeometry object to be the one referenced by the Code Property;
//
// Then the values of that code can be retrieved via the public variables.

uses
    Classes,
    Sysutils,
    Arraydef,
    Command,
    DSSClass,
    DSSObject,
    uCMatrix,
    LineConstants,
    conductorData,
    CNData,
    TSData,
    LineSpacing;

type
{$SCOPEDENUMS ON}
    TLineGeometryPropLegacy = (
        INVALID = 0,
        nconds = 1,
        nphases = 2,
        cond = 3,
        wire = 4,
        x = 5,
        h = 6,
        units = 7,
        normamps = 8,
        emergamps = 9,
        reduce = 10,
        spacing = 11,
        wires = 12,
        cncable = 13,
        tscable = 14,
        cncables = 15,
        tscables = 16,
        Seasons = 17,
        Ratings = 18,
        LineType = 19,
        conductors = 20
    );
    TLineGeometryProp = (
        INVALID = 0,
        NConds = 1,
        NPhases = 2,
        Cond = 3,
        Wire = 4,
        X = 5,
        H = 6,
        Units = 7,
        NormAmps = 8,
        EmergAmps = 9,
        Reduce = 10,
        Spacing = 11,
        Wires = 12,
        CNCable = 13,
        TSCable = 14,
        CNCables = 15,
        TSCables = 16,
        Seasons = 17,
        Ratings = 18,
        LineType = 19,
        Conductors = 20
    );
{$SCOPEDENUMS OFF}

    ELineGeometryProblem = class(Exception);

    TLineGeometry = class(TDSSClass)
    PROTECTED
        ConductorProxyClass: TProxyClass;

        procedure DefineProperties(); override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TLineGeometryObj = class(TDSSObject)
    PUBLIC
        phaseChoice: pConductorChoiceArray; // TODO: remove -- somewhat redundant with conductorData (FWireData)
        FNConds: Integer;
        FNPhases: Integer;
        conductorData: pConductorDataArray; // was originally FWireData
        xCoord: pDoubleArray;
        yCoord: pDoubleArray;
        units: pIntegerArray;
        FLastUnit: Integer;
        dataChanged: Boolean;
        FReduce: LongBool;
        FActiveCond: Integer;

        lineConstants: TLineConstants;

        NormAmps: Double;
        EmergAmps: Double;
        NumAmpRatings: Integer;
        AmpRatings: array of Double;
        FLineType: Integer; // Pointer to code for type of line
        LineSpacingObj: TLineSpacingObj;

        procedure ChangeLineConstantsType(newPhaseChoice: ConductorChoice);

        procedure SetNPhases(const Value: Integer);
        procedure SetActiveCond(const Value: Integer);

        function NConds(): Integer;
        procedure SetNConds(const Value: Integer);

        procedure UpdateLineGeometryData(f: Double; earthModel: Integer);   // call this before using the line data

        constructor Create(ParClass: TDSSClass; const LineGeometryName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;
        
        procedure DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;
        procedure SaveWrite(F: TStream); OVERRIDE;

        // called from a Line object that has its own Spacing and Wires input
        // automatically sets reduce=y if the spacing has more wires than phases
        procedure LoadSpacingAndWires(Spc: TLineSpacingObj; Wires: pConductorDataArray; earthModel: Integer);
        function GetYCMatrix(f, Lngth: Double; Units: Integer; earthModel: Integer): Tcmatrix;
        function GetZMatrix(f, Lngth: Double; Units: Integer; earthModel: Integer): Tcmatrix;        
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    UComplex, DSSUcomplex,
    Utilities,
    LineUnits,
    WireData,
    OHLineConstants,
    CableConstants,
    Math,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TLineGeometryObj;
    TProp = TLineGeometryProp;
    TPropLegacy = TLineGeometryPropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;    

constructor TLineGeometry.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
    end;

    ConductorProxyClass := TProxyClass.Create(dssContext, ['WireData', 'CNData', 'TSData']);

    inherited Create(dssContext, DSS_OBJECT, 'LineGeometry');
    RequiresCircuit := true;
end;

destructor TLineGeometry.Destroy;
begin
    ConductorProxyClass.Free;
    inherited Destroy;
end;

procedure SetWires(obj: TObj; Value: TDSSObjectPtr; ValueCount: Integer; setterFlags: TDSSPropertySetterFlags);
var
    i, istart, istop: Integer;
begin
    istart := 1;
    istop := obj.FNConds;

    if ((TDSSPropertySetterFlag.AllowAllConductors in setterFlags) and (obj.FNConds = ValueCount)) and (ValueCount > 0) then
    begin
        for i := istart to istop do
        begin
            obj.conductorData[i] := TConductorDataObj(Value^);
            Inc(Value);
        end;
        // TLineGeometryObj.PropertySideEffects should handle the other side-effects
        Exit;
    end
    else if obj.phaseChoice[obj.FActiveCond] = Unknown then
        obj.ChangeLineConstantsType(Overhead)
    else if obj.phaseChoice[obj.FActiveCond] <> Overhead then
        // these are buried neutral wires 
        // (only when the phase conductors not overhead)
        istart := obj.FNPhases + 1;

    // Validate number of elements
    if (istop - istart + 1) <> ValueCount then
    begin
        obj.DoSimpleMsg('%s: Unexpected number (%d) of objects; expected %d objects.', 
            [obj.FullName(), ValueCount, (istop - istart + 1)], 18102);
        Exit;
    end;

    for i := istart to istop do
    begin
        obj.conductorData[i] := TConductorDataObj(Value^);
        Inc(Value);
    end;
    obj.FActiveCond := istop;
end;

procedure TLineGeometry.DefineProperties();
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    SpecSetNames := ArrayOfString.Create(
        'LineSpacing',
        'x, h'
    );
    SpecSets := TSpecSets.Create(
        TSpecSet.Create(ord(TProp.spacing)),
        TSpecSet.Create(ord(TProp.x), ord(TProp.h))
    );

    PropertyStructArrayCountOffset := ptruint(@obj.FNConds);
    PropertyStructArrayIndexOffset := ptruint(@obj.FActiveCond);
    PropertyStructArrayIndexOffset2 := ptruint(@obj.FNPhases);

    // list of objects
    PropertyType[ord(TProp.tscables)] := TPropertyType.DSSObjectReferenceArrayProperty;
    PropertyOffset[ord(TProp.tscables)] := ptruint(@obj.conductorData);
    PropertyOffset2[ord(TProp.tscables)] := ptruint(DSS.TSDataClass);
    PropertyFlags[ord(TProp.tscables)] := [TPropertyFlag.AltIndex, TPropertyFlag.Redundant, TPropertyFlag.SuppressJSON];
    PropertyRedundantWith[ord(TProp.tscables)] := ord(TProp.tscable);
    PropertyArrayAlternative[ord(TProp.tscable)] := ord(TProp.tscables);
    
    PropertyType[ord(TProp.cncables)] := TPropertyType.DSSObjectReferenceArrayProperty;
    PropertyOffset[ord(TProp.cncables)] := ptruint(@obj.conductorData);
    PropertyOffset2[ord(TProp.cncables)] := ptruint(DSS.CNDataClass);
    PropertyFlags[ord(TProp.cncables)] := [TPropertyFlag.AltIndex, TPropertyFlag.Redundant, TPropertyFlag.SuppressJSON];
    PropertyRedundantWith[ord(TProp.cncables)] := ord(TProp.cncable);
    PropertyArrayAlternative[ord(TProp.cncable)] := ord(TProp.cncables);

    PropertyType[ord(TProp.wires)] := TPropertyType.DSSObjectReferenceArrayProperty;
    PropertyOffset[ord(TProp.wires)] := ptruint(@obj.conductorData);
    PropertyOffset2[ord(TProp.wires)] := ptruint(DSS.WireDataClass);
    PropertyOffset3[ord(TProp.wires)] := ptruint(@obj.FNConds);
    PropertyWriteFunction[ord(TProp.wires)] := @SetWires;
    PropertyFlags[ord(TProp.wires)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.SuppressJSON];
    // PropertyFlags[ord(TProp.wires)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.FullNameAsArray, TPropertyFlag.FullNameAsJSONArray];
    // PropertyRedundantWith[ord(TProp.wires)] := ord(TProp.wire);
    // PropertyNameJSON[ord(TProp.wires)] := 'Conductors';

    PropertyArrayAlternative[ord(TProp.wire)] := ord(TProp.wires);

    PropertyType[ord(TProp.conductors)] := TPropertyType.DSSObjectReferenceArrayProperty;
    PropertyOffset[ord(TProp.conductors)] := ptruint(@obj.conductorData);
    PropertyOffset2[ord(TProp.conductors)] := ptruint(ConductorProxyClass);
    PropertyOffset3[ord(TProp.conductors)] := ptruint(@obj.FNConds);
    PropertyWriteFunction[ord(TProp.conductors)] := @SetWires;
    PropertyFlags[ord(TProp.conductors)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.FullNameAsArray, TPropertyFlag.FullNameAsJSONArray, TPropertyFlag.AllowNoneItem];

    // enums
    PropertyType[ord(TProp.units)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.units)] := ptruint(@obj.units);
    PropertyOffset2[ord(TProp.units)] := PtrInt(DSS.UnitsEnum);
    PropertyFlags[ord(TProp.units)] := [TPropertyFlag.OnArray];

    PropertyType[ord(TProp.linetype)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.linetype)] := ptruint(@obj.FLineType);
    PropertyOffset2[ord(TProp.linetype)] := PtrInt(DSS.LineTypeEnum);

    // object properties
    PropertyType[ord(TProp.spacing)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.spacing)] := ptruint(@obj.LineSpacingObj);
    PropertyOffset2[ord(TProp.spacing)] := ptruint(DSS.LineSpacingClass);
    PropertyFlags[ord(TProp.spacing)] := [TPropertyFlag.RequiredInSpecSet];

    PropertyType[ord(TProp.Ratings)] := TPropertyType.DoubleDArrayProperty;
    PropertyOffset[ord(TProp.Ratings)] := ptruint(@obj.AmpRatings);
    PropertyOffset2[ord(TProp.Ratings)] := ptruint(@obj.NumAmpRatings);

    PropertyType[ord(TProp.reduce)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.reduce)] := ptruint(@obj.Freduce);

    PropertyType[ord(TProp.nphases)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.nconds)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.cond)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.nphases)] := ptruint(@obj.FNphases);
    PropertyOffset[ord(TProp.nconds)] := ptruint(@obj.FNConds);
    PropertyOffset[ord(TProp.cond)] := ptruint(@obj.FActiveCond);
    PropertyFlags[ord(TProp.nphases)] := [TPropertyFlag.NonNegative]; // phases can be zero (e.g. only neutral cables)
    PropertyFlags[ord(TProp.nconds)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];
    PropertyFlags[ord(TProp.cond)] := [TPropertyFlag.IntegerStructIndex];

    PropertyType[ord(TProp.Seasons)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Seasons)] := ptruint(@obj.NumAmpRatings);
    PropertyFlags[ord(TProp.Seasons)] := [TPropertyFlag.SuppressJSON]; // can be derived trivially from length(Ratings)

    PropertyType[ord(TProp.wire)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.cncable)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.tscable)] := TPropertyType.DSSObjectReferenceProperty;

    PropertyOffset[ord(TProp.wire)] := ptruint(@obj.conductorData); 
    PropertyOffset[ord(TProp.cncable)] := ptruint(@obj.conductorData); 
    PropertyOffset[ord(TProp.tscable)] := ptruint(@obj.conductorData); 
    
    PropertyOffset2[ord(TProp.wire)] := ptruint(DSS.WireDataClass);
    PropertyOffset2[ord(TProp.cncable)] := ptruint(DSS.CNDataClass);
    PropertyOffset2[ord(TProp.tscable)] := ptruint(DSS.TSDataClass);

    PropertyFlags[ord(TProp.wire)] := [TPropertyFlag.Redundant, TPropertyFlag.OnArray, TPropertyFlag.FullNameAsArray];
    PropertyFlags[ord(TProp.cncable)] := [TPropertyFlag.Redundant, TPropertyFlag.OnArray, TPropertyFlag.SuppressJSON];
    PropertyFlags[ord(TProp.tscable)] := [TPropertyFlag.Redundant, TPropertyFlag.OnArray, TPropertyFlag.SuppressJSON];
    PropertyRedundantWith[ord(TProp.cncable)] := ord(TProp.wires);
    PropertyRedundantWith[ord(TProp.tscable)] := ord(TProp.wires);
    PropertyRedundantWith[ord(TProp.wire)] := ord(TProp.wires);

    PropertyType[ord(TProp.x)] := TPropertyType.DoubleOnArrayProperty; //TODO: use TPropertyFlag.OnArray instead
    PropertyOffset[ord(TProp.x)] := ptruint(@obj.xCoord); 
    PropertyOffset2[ord(TProp.x)] := ptruint(@obj.FActiveCond);
    PropertyFlags[ord(TProp.x)] := [TPropertyFlag.RequiredInSpecSet];

    PropertyType[ord(TProp.h)] := TPropertyType.DoubleOnArrayProperty; //TODO: use TPropertyFlag.OnArray instead
    PropertyOffset[ord(TProp.h)] := ptruint(@obj.yCoord); 
    PropertyOffset2[ord(TProp.h)] := ptruint(@obj.FActiveCond);
    PropertyFlags[ord(TProp.h)] := [TPropertyFlag.RequiredInSpecSet];

    PropertyOffset[ord(TProp.NormAmps)] := ptruint(@obj.NormAmps);
    PropertyOffset[ord(TProp.EmergAmps)] := ptruint(@obj.EmergAmps);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties();
end;

function TLineGeometry.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    obj: TObj;
begin
    obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := obj;
    obj.ClassIndex := AddObjectToList(obj, Activate);
    Result := obj;
end;

procedure TLineGeometryObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
var
    tmpName: String;
    i: Integer;
    conductorObj: TConductorDataObj = NIL;
begin
    case Idx of
        ord(TProp.nphases):
            if lineConstants <> NIL then
            begin
                lineConstants.Nphases := FNPhases;
                if (lineConstants.Nphases > FNConds) then
                    lineConstants.Nphases := FNConds;
            end;
        ord(TProp.cond):
            if units[FactiveCond] = -1 then
                units[FactiveCond] := FLastUnit;  // makes this a sticky value so you don't have to repeat it
        ord(TProp.wire):
            if phaseChoice[FActiveCond] = Unknown then
                ChangeLineConstantsType(Overhead);
        ord(TProp.units):
            FLastUnit := units[FActiveCond];
        ord(TProp.cncable), ord(TProp.cncables):
            ChangeLineConstantsType(ConcentricNeutral);
        ord(TProp.tscable), ord(TProp.tscables):
            ChangeLineConstantsType(TapeShield);
        ord(TProp.nconds):
        begin
            if previousIntVal <> FNConds then
            begin
                if Assigned(lineConstants) then
                    FreeAndNil(lineConstants);

                // Allocations
                Reallocmem(conductorData, Sizeof(conductorData[1]) * FNConds);
                for i := max(1, previousIntVal) to FNConds do
                    conductorData[i] := NIL;

                Reallocmem(xCoord, Sizeof(xCoord[1]) * FNConds);
                Reallocmem(yCoord, Sizeof(yCoord[1]) * FNConds);
                Reallocmem(units, Sizeof(units[1]) * FNConds);
                Reallocmem(phaseChoice, Sizeof(phaseChoice[1]) * FNConds);
            end
            else
            begin
                for i := 1 to FNConds do
                    conductorData[i] := NIL;
            end;
                
            if FNConds > previousIntVal then
                for i := Max(1, previousIntVal) to FNConds do
                    phaseChoice[i] := Unknown;

            for i := 1 to FNConds do
            begin
                FActiveCond := i;
                ChangeLineConstantsType(Overhead); // works on activecond
            end;
            // Reset the active conductor
            FActiveCond := 1;

            // Initialize Allocations
            for i := 1 to FNConds do
                phaseChoice[i] := Overhead;
            for i := 1 to FNConds do
                conductorData[i] := NIL;
            for i := 1 to FNConds do
                xCoord[i] := 0.0;
            for i := 1 to FNConds do
                yCoord[i] := 0.0;
            for i := 1 to FNConds do
                units[i] := -1;  // default to ft
            FLastUnit := UNITS_FT;
        end;
        ord(TProp.spacing):
            if LineSpacingObj <> NIL then
            begin
                if (FNConds = LineSpacingObj.NConds) then
                begin
                    FLastUnit := LineSpacingObj.Units;
                    for i := 1 to FNConds do
                    begin
                        xCoord[i] := LineSpacingObj.GetXCoord(i);
                        yCoord[i] := LineSpacingObj.GetYCoord(i);
                        units[i] := FLastUnit;
                    end;
                    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.NoPropertyTracking)) = 0 then
                    begin
                        PrpSequence[ord(TProp.X)] := 0;
                        PrpSequence[ord(TProp.H)] := 0;
                    end;
                end
                else
                    DoSimpleMsg('LineSpacing object %s has the wrong number of wires.', [LineSpacingObj.Name], 10103);
            end;
    end;
    case Idx of 
        ord(TProp.wires), ord(TProp.cncables), ord(TProp.tscables):
        begin
            if (TSetterFlag.AllowAllConductors in setterFlags) then // Special handling for "Conductors"
            begin
                // Simulate setting the conductors one by one
                // Much easier/safer than reproducing the whole code paths
                for i := 1 to NConds() do
                begin
                    SetActiveCond(i);
                    if conductorData[FActiveCond] is TWireDataObj then 
                    begin
                        PropertySideEffects(ord(TProp.wire), 0, setterFlags);
                        continue;
                    end;
                    if conductorData[FActiveCond] is TCNDataObj then 
                    begin
                        PropertySideEffects(ord(TProp.cncable), 0, setterFlags);
                        continue;
                    end;
                    if conductorData[FActiveCond] is TTSDataObj then 
                    begin
                        PropertySideEffects(ord(TProp.tscable), 0, setterFlags);
                        continue;
                    end;
                end;
            end
            else
            begin
                // Traditional wires/cncables/tscables            
                i := 1;
                if Idx = ord(TProp.wires) then
                begin
                    if phaseChoice[FActiveCond] = Unknown then
                    begin
                        // no other cables set for ActiveCond
                    end
                    else 
                    if phaseChoice[FActiveCond] <> Overhead then
                        // these are buried neutral wires
                        // (only when the phase conductors not overhead)
                        i := FNPhases + 1;
                end;
                if i = 1 then
                begin
                    conductorObj := conductorData[1];
                    if (conductorObj.NormAmps > 0.0) and (Normamps = 0.0) then 
                        Normamps  := conductorObj.NormAmps;
                    
                    if (conductorObj.Emergamps > 0.0) and (Emergamps = 0.0) then 
                        Emergamps := conductorObj.EmergAmps;
                    
                    if (conductorObj.NumAmpRatings > 1) and (NumAmpRatings = 1) then 
                        NumAmpRatings  := conductorObj.NumAmpRatings;

                    if (Length(conductorObj.AmpRatings) > 1) and (length(AmpRatings) = 1) then
                    begin
                        AmpRatings := Copy(conductorObj.AmpRatings, 0, Length(conductorObj.AmpRatings));
                    end;
                end;
            end;
        end;
        ord(TProp.wire), ord(TProp.cncable), ord(TProp.tscable):
        begin
            conductorObj := conductorData[FActiveCond];
            if Assigned(conductorObj) then
            begin
                // conductorData[ActiveCond] := conductorObj;
                // Default the current ratings for this geometry to the rating of the first conductor
                if (FActiveCond = 1) then
                begin
                    if (conductorObj.NormAmps > 0.0) and (Normamps = 0.0) then
                        Normamps := conductorObj.NormAmps;
                    if (conductorObj.Emergamps > 0.0) and (Emergamps = 0.0) then
                        Emergamps := conductorObj.EmergAmps;
                    if (conductorObj.NumAmpRatings > 1) and (NumAmpRatings = 1) then
                        NumAmpRatings := conductorObj.NumAmpRatings;
                    if (length(conductorObj.AmpRatings) > 1) and (length(AmpRatings) = 1) then
                    begin
                        SetLength(AmpRatings, NumAmpRatings);
                        AmpRatings := Copy(conductorObj.AmpRatings, 
                            0, Min(Length(conductorObj.AmpRatings), NumAmpRatings)
                        );
                    end;
                end;
            end
            else
                DoSimpleMsg('WireData/CNData/TSData object was not defined. Must be previously defined.', [tmpName], 10103);
        end;
        ord(TProp.Seasons):
            setlength(AmpRatings, NumAmpRatings);
    end;

    case Idx of
        ord(TProp.NConds),
        ord(TProp.Wire),
        ord(TProp.X),
        ord(TProp.H),
        ord(TProp.Units),
        ord(TProp.Spacing),
        ord(TProp.Wires),
        ord(TProp.CNCable),
        ord(TProp.TSCable),
        ord(TProp.CNCables),
        ord(TProp.TSCables):
            dataChanged := TRUE;
    end;

    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

procedure TLineGeometryObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    SetNConds(Other.FNConds); // allocates
    FNphases := Other.FNphases;
    LineSpacingObj := Other.LineSpacingObj;
    FLineType := Other.FLineType;
    for i := 1 to FNConds do
        phaseChoice[i] := Other.phaseChoice[i];
    for i := 1 to FNConds do
        conductorData[i] := Other.conductorData[i];
    for i := 1 to FNConds do
        xCoord[i] := Other.xCoord[i];
    for i := 1 to FNConds do
        yCoord[i] := Other.yCoord[i];
    for i := 1 to FNConds do
        units[i] := Other.units[i];
    dataChanged := TRUE;
    NormAmps := Other.NormAmps;
    EmergAmps := Other.EmergAmps;
    FReduce := Other.FReduce;

    UpdateLineGeometryData(activecircuit.Solution.Frequency(), DSS.ActiveEarthModel);
end;

constructor TLineGeometryObj.Create(ParClass: TDSSClass; const LineGeometryName: String);
begin
    inherited Create(ParClass, LineGeometryName);
    DSSObjType := ParClass.DSSClassType;

    dataChanged := TRUE;

    phaseChoice := NIL;
    conductorData := NIL;
    xCoord := NIL;
    yCoord := NIL;
    units := NIL;
    lineConstants := NIL;
    LineSpacingObj := NIL;

    // was causing unnecessary allocations (was leaving dangling memory)
    // Nconds      := 3;  // Allocates terminals
    // FNphases    := 3;

    FNConds := 0;
    FNPhases := 0;
    // SetActiveCond(1);
    FActiveCond := 1;
    FLastUnit := UNITS_FT;
    Normamps := 0.0;
    EmergAmps := 0.0;
    FLineType := 1;  // Default to OH Line

    FReduce := FALSE;
    NumAmpRatings := 1;
    setlength(AmpRatings, NumAmpRatings);
    AmpRatings[0] := NormAmps;
end;

destructor TLineGeometryObj.Destroy;
begin
    if lineConstants <> NIL then
        lineConstants.Free;
    Reallocmem(conductorData, 0);
    Reallocmem(yCoord, 0);
    Reallocmem(xCoord, 0);
    Reallocmem(units, 0);
    Reallocmem(phaseChoice, 0);

    inherited destroy;
end;

procedure TLineGeometryObj.DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean);
var
    i, j: Integer;
begin
    FSWriteln(F, '! WARNING: when mixing wire/cable types, "wires", "cncables" and "tscables" may not make sense in this dump');

    inherited DumpProperties(F, Complete);

    for i := 1 to 2 do
    begin
        FSWriteln(F, '~ ' + ParentClass.PropertyName[i] + '=' + PropertyValue(i));
    end;
    for j := 1 to FNConds do
    begin
        SetActiveCond(j);
        FSWriteln(F, '~ ' + ParentClass.PropertyName[3] + '=' + PropertyValue(3));
        FSWriteln(F, '~ ' + ParentClass.PropertyName[4] + '=' + PropertyValue(4));
        FSWriteln(F, '~ ' + ParentClass.PropertyName[5] + '=' + PropertyValue(5));
        FSWriteln(F, '~ ' + ParentClass.PropertyName[6] + '=' + PropertyValue(6));
        FSWriteln(F, '~ ' + ParentClass.PropertyName[7] + '=' + PropertyValue(7));
    end;
    for i := 8 to ParentClass.NumProperties do
    begin
        FSWriteln(F, '~ ' + ParentClass.PropertyName[i] + '=' + PropertyValue(i));
    end;
end;

function TLineGeometryObj.NConds(): Integer;
begin
    if Freduce then
        Result := FNPhases
    else
        Result := FNConds;
end;

function TLineGeometryObj.GetYCMatrix(f, Lngth: Double; Units: Integer; earthModel: Integer): Tcmatrix;
begin
    Result := NIL;
    if dataChanged then
        UpdateLineGeometryData(f, earthModel);
    if not DSS.SolutionAbort() then
        Result := lineConstants.GetYCMatrix(f, Lngth, Units);
end;

function TLineGeometryObj.GetZMatrix(f, Lngth: Double; Units: Integer; earthModel: Integer): Tcmatrix;
begin
    Result := NIL;
    if dataChanged then
        UpdateLineGeometryData(f, earthModel);
    if not DSS.SolutionAbort() then
        Result := lineConstants.GetZMatrix(F, Lngth, Units, earthModel);
end;

procedure TLineGeometryObj.SaveWrite(F: TStream);
// Override standard SaveWrite
// Linegeometry structure not conducive to standard means of saving
var
    strPhaseChoice: String;
    iprop: Integer;
    i: Integer;
    wroteConds: Boolean = False;
begin
    // Write only properties that were explicitly set in the
    // final order they were actually set
    iProp := GetNextPropertySet(0);
    if iProp > 0 then
        FSWriteln(F);

    while iProp > 0 do
    begin
        case iProp of
            ord(TProp.cond), ord(TProp.spacing), ord(TProp.wires):
                if not wroteConds then
                begin   // if cond=, spacing, or wires were ever used write out arrays ...
                    for i := 1 to FNConds do
                    begin
                        if conductorData[i] = NIL then
                            continue; // shouldn't happen in normal conditions
                        if conductorData[i].ParentClass = DSS.TSDataClass then
                            strPhaseChoice := 'tscable'
                        else if conductorData[i].ParentClass = DSS.CNDataClass then
                            strPhaseChoice := 'cncable'
                        else
                            strPhaseChoice := 'wire';
                        FSWriteln(F, Format('~ Cond=%d %s=%s X=%.7g h=%.7g units=%s',
                            [i, strPhaseChoice, conductorData[i].Name(), xCoord[i], yCoord[i], LineUnitsStr(units[i])]));
                    end;
                    wroteConds := True;
                end;
            ord(TProp.reduce):
                if FReduce then
                    FSWriteln(F, '~ Reduce=Yes');
            ord(TProp.wire), ord(TProp.x), ord(TProp.h), ord(TProp.units),
            ord(TProp.cncable), ord(TProp.tscable):
                ; // Ignore these properties;
        else
            FSWriteln(F, Format('~ %s=%s', [ParentClass.PropertyName[iProp], CheckForBlanks(PropertyValue(iProp))]));
        end;
        iProp := GetNextPropertySet(iProp);
    end;
end;

procedure TLineGeometryObj.SetActiveCond(const Value: Integer);
begin
    if Value > 0 then
        if Value <= FNConds then
        begin
            FActiveCond := Value;
            if units[FactiveCond] = -1 then
                units[FactiveCond] := FLastUnit;  // makes this a sticky value so you don't have to repeat it
        end;
end;

procedure TLineGeometryObj.ChangeLineConstantsType(newPhaseChoice: ConductorChoice);
var
    newLineData: TLineConstants;
    needNew: Boolean;
begin
    newLineData := NIL;
    needNew := FALSE;

    if (FActiveCond > 0) and (FActiveCond <= FNConds) and 
       (newPhaseChoice <> phaseChoice[FActiveCond]) then
        needNew := TRUE
    else
    if (lineConstants = NIL) or (FNConds <> lineConstants.numConductors) then
        needNew := TRUE;

    if needNew then
        case newPhaseChoice of
            Overhead:
                newLineData := TOHLineConstants.Create(FNConds);
            ConcentricNeutral:
                newLineData := TCableConstants.Create(FNConds);
            TapeShield:
                newLineData := TCableConstants.Create(FNConds);
        end;

    if Assigned(newLineData) then
    begin
        if Assigned(lineConstants) then
        begin
            newLineData.Nphases := lineConstants.Nphases;
            newLineData.SetRhoEarth(lineConstants.FrhoEarth);
        end;
        FreeAndNil(lineConstants);
        lineConstants := newLineData;
    end;
    if (FActiveCond > 0) and (FActiveCond <= FNConds) then
        phaseChoice[FActiveCond] := newPhaseChoice;
end;

procedure TLineGeometryObj.SetNConds(const Value: Integer);
var
    prev: Integer;
begin
    prev := FNConds;
    FNConds := Value;
    PropertySideEffects(ord(TProp.nconds), prev, [])
end;

procedure TLineGeometryObj.SetNPhases(const Value: Integer);
begin
    // TODO: remove/comment this block if using only neutrals is acceptable
    if Value < 1 then
    begin
        DoSimpleMsg(_('Invalid number of phases sent via DSS command. Please enter a value within range.'), 186);
        Exit;
    end;

    FNphases := Value;
    lineConstants.Nphases := Value;
end;

procedure TLineGeometryObj.UpdateLineGeometryData(f: Double; earthModel: Integer);
var
    i: Integer;
    LineGeomErrMsg: String;
    cnd: TCNDataObj;
    tsd: TTSDataObj;
    cableconsts: TCableConstants;
begin
    for i := 1 to FNConds do
    begin
        if conductorData[i] = NIL then
            raise Exception.Create(Format(_('%s: WireData is not correctly initialized. Check the object definition.'), [FullName()]));

        lineConstants.SetX(i, units[i], xCoord[i]);
        lineConstants.SetY(i, units[i], yCoord[i]);
        lineConstants.SetRadius(i, conductorData[i].radiusUnits, conductorData[i].Radius);
        lineConstants.SetCapRadius(i, conductorData[i].radiusUnits, conductorData[i].capRadius);
        lineConstants.SetGMR(i, conductorData[i].GMRUnits, conductorData[i].GMRAC);
        lineConstants.SetRdc(i, conductorData[i].resistanceUnits, conductorData[i].RDC);
        lineConstants.SetRac(i, conductorData[i].resistanceUnits, conductorData[i].RAC);
        if (conductorData[i] is TCNDataObj) then
        begin
            cnconsts := (lineConstants as TCableConstants);
            cnd := (conductorData[i] as TCNDataObj);
            cnconsts.SetEpsR(i, cnd.EpsR);
            cnconsts.SetInsLayer(i, cnd.radiusUnits, cnd.insLayer);
            cnconsts.SetDiaIns(i, cnd.radiusUnits, cnd.diaIns);
            cnconsts.SetDiaCable(i, cnd.radiusUnits, cnd.diaCable);
            cnconsts.SetkStrand(i, cnd.kStrand);
            cnconsts.SetDiaStrand(i, cnd.radiusUnits, cnd.DiaStrand);
            cnconsts.SetGmrStrand(i, cnd.GMRUnits, cnd.GmrStrand);
            cnconsts.SetRStrand(i, cnd.resistanceUnits, cnd.RStrand);
        end
        else
        if (conductorData[i] is TTSDataObj) then
        begin
            tsconsts := (lineConstants as TCableConstants);
            tsd := (conductorData[i] as TTSDataObj);
            tsconsts.SetEpsR(i, tsd.EpsR);
            tsconsts.SetInsLayer(i, tsd.radiusUnits, tsd.insLayer);
            tsconsts.SetDiaIns(i, tsd.radiusUnits, tsd.diaIns);
            tsconsts.SetDiaCable(i, tsd.radiusUnits, tsd.diaCable);
            tsconsts.SetDiaShield(i, tsd.radiusUnits, tsd.DiaShield);
            tsconsts.SetTapeLayer(i, tsd.radiusUnits, tsd.TapeLayer);
            tsconsts.SetTapeLap(i, tsd.TapeLap);
        end;
    end;

    lineConstants.Nphases := FNphases;
    dataChanged := FALSE;

    // Before we calc, check for bad conductor definitions
    if lineConstants.ConductorsInSameSpace(LineGeomErrMsg) then
    begin
        raise ELineGeometryProblem.Create(Format(_('Error in %s: %s'), [FullName(), LineGeomErrMsg]));
        DSS.SetSolutionAbort(true);
    end
    else
    begin
        lineConstants.Calc(f, earthModel); // ***** Line impedance calc'd here ****
        if FReduce then
            lineConstants.Reduce; // reduce out neutrals
    end;
end;

procedure TLineGeometryObj.LoadSpacingAndWires(Spc: TLineSpacingObj; Wires: pConductorDataArray; earthModel: Integer);
var
    i: Integer;
    newPhaseChoice: ConductorChoice;
begin
    SetNConds(Spc.NConds);   // allocates
    FNphases := Spc.Nphases;
    LineSpacingObj := Spc;
    if FNConds > FNPhases then
        FReduce := TRUE;

    newPhaseChoice := Overhead;
    for i := 1 to FNConds do
    begin
        if Wires[i] is TCNDataObj then
            newPhaseChoice := ConcentricNeutral;
        if Wires[i] is TTSDataObj then
            newPhaseChoice := TapeShield;
    end;
    ChangeLineConstantsType(newPhaseChoice);

    for i := 1 to FNConds do
        conductorData[i] := Wires[i];
    for i := 1 to FNConds do
        xCoord[i] := Spc.GetXCoord(i);
    for i := 1 to FNConds do
        yCoord[i] := Spc.GetYCoord(i);
    for i := 1 to FNConds do
        units[i] := Spc.Units;
    dataChanged := TRUE;
    NormAmps := Wires[1].NormAmps;
    EmergAmps := Wires[1].EmergAmps;

    UpdateLineGeometryData(activecircuit.Solution.Frequency(), earthModel);
end;

end.