unit LineGeometry;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

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
    ConductorData,
    CNData,
    TSData,
    LineSpacing;

type
{$SCOPEDENUMS ON}
    TLineGeometryProp = (
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
        LineType = 19
    );
{$SCOPEDENUMS OFF}

    ELineGeometryProblem = class(Exception);

    TLineGeometry = class(TDSSClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TLineGeometryObj = class(TDSSObject)
    PUBLIC
        FPhaseChoice: pConductorChoiceArray;
        FNConds: Integer;
        FNPhases: Integer;
        FCondName: ArrayOfString;
        FWireData: pConductorDataArray;
        FX: pDoubleArray;
        FY: pDoubleArray;
        FUnits: pIntegerArray;
        FLastUnit: Integer;
        DataChanged: Boolean;
        FReduce: Boolean;
        FActiveCond: Integer;

        FLineData: TLineConstants;

        NormAmps: Double;
        EmergAmps: Double;
        NumAmpRatings: Integer;
        AmpRatings: array of Double;
        FLineType: Integer; // Pointer to code for type of line
        LineSpacingObj: TLineSpacingObj;

        procedure ChangeLineConstantsType(newPhaseChoice: ConductorChoice);

        procedure set_Nconds(const Value: Integer);
        procedure set_Nphases(const Value: Integer);
        procedure set_ActiveCond(const Value: Integer);
        function Get_YCmatrix(f, Lngth: Double; Units: Integer): Tcmatrix;
        function Get_Zmatrix(f, Lngth: Double; Units: Integer): Tcmatrix;
        function Get_RhoEarth: Double;
        procedure Set_RhoEarth(const Value: Double);
        function get_Nconds: Integer;
        procedure UpdateLineGeometryData(f: Double);   // call this before using the line data

        // CIM Accessors
        function Get_FX(i: Integer): Double;
        function Get_FY(i: Integer): Double;
        function Get_FUnits(i: Integer): Integer;
        function Get_ConductorName(i: Integer): String;
        function Get_ConductorData(i: Integer): TConductorDataObj;
        //TODO: remove
        procedure Set_FX(i: Integer; Value: Double);
        procedure Set_FY(i: Integer; Value: Double);
        procedure Set_FUnits(i: Integer; Value: Integer);

        function Get_PhaseChoice(i: Integer): ConductorChoice;

        constructor Create(ParClass: TDSSClass; const LineGeometryName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;
        
        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;
        procedure SaveWrite(F: TFileStream); OVERRIDE;

        // called from a Line object that has its own Spacing and Wires input
        // automatically sets reduce=y if the spacing has more wires than phases
        procedure LoadSpacingAndWires(Spc: TLineSpacingObj; Wires: pConductorDataArray);

        property Nconds: Integer READ get_Nconds WRITE set_Nconds;
        property Nphases: Integer READ FNphases WRITE set_Nphases;
        property ActiveCond: Integer READ FActiveCond WRITE set_ActiveCond;
        property Zmatrix[f, Lngth: Double; Units: Integer]: Tcmatrix READ Get_Zmatrix;
        property YCmatrix[f, Lngth: Double; Units: Integer]: Tcmatrix READ Get_YCmatrix;
        property RhoEarth: Double READ Get_RhoEarth WRITE Set_RhoEarth;

        // CIM XML accessors
        property Xcoord[i: Integer]: Double READ Get_FX WRITE Set_FX;
        property Ycoord[i: Integer]: Double READ Get_FY WRITE Set_FY;
        property Units[i: Integer]: Integer READ Get_FUnits WRITE Set_FUnits;
        property ConductorName[i: Integer]: String READ Get_ConductorName;
        property ConductorData[i: Integer]: TConductorDataObj READ Get_ConductorData;
        property NWires: Integer READ FNConds;
        property PhaseChoice[i: Integer]: ConductorChoice READ Get_PhaseChoice;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    UComplex, DSSUcomplex,
    Utilities,
    LineUnits,
    OHLineConstants,
    CNLineConstants,
    TSLineConstants,
    Math,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TLineGeometryObj;
    TProp = TLineGeometryProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer;    

constructor TLineGeometry.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, DSS_OBJECT, 'LineGeometry');
end;

destructor TLineGeometry.Destroy;
begin
    inherited Destroy;
end;

procedure SetWires(obj: TObj; Value: TDSSObjectPtr; ValueCount: Integer);
var
    i, istart, istop: Integer;
begin
    with Obj do
    begin
        istart := 1;
        istop := FNConds;
        if FPhaseChoice[ActiveCond] = Unknown then
            ChangeLineConstantsType(Overhead)
        else if FPhaseChoice[ActiveCond] <> Overhead then
            // these are buried neutral wires 
            // (only when the phase conductors not overhead)
            istart := FNPhases + 1;

        // Validate number of elements
        if (istop - istart + 1) <> ValueCount then
        begin
            DoSimpleMsg('%s: Unexpected number (%d) of objects; expected %d objects.', 
                [FullName, ValueCount, (istop - istart + 1)], 18102);
            Exit;
        end;

        for i := istart to istop do
        begin
            FCondName[i - 1] := Value^.Name;
            FWireData[i] := TConductorDataObj(Value^);
            Inc(Value);
        end;
        FActiveCond := istop;
    end;
end;

procedure TLineGeometry.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    PropertyStructArrayCountOffset := ptruint(@obj.FNconds);
    PropertyStructArrayIndexOffset := ptruint(@obj.FActiveCond);
    PropertyStructArrayIndexOffset2 := ptruint(@obj.FNPhases);

    // list of objects
    //TODO: for these lists, a better get (only return when the type matches) would fix the save bug?
    PropertyType[ord(TProp.tscables)] := TPropertyType.DSSObjectReferenceArrayProperty;
    PropertyOffset[ord(TProp.tscables)] := ptruint(@obj.FWireData);
    PropertyOffset2[ord(TProp.tscables)] := ptruint(DSS.TSDataClass);
    PropertyFlags[ord(TProp.tscables)] := [TPropertyFlag.AltIndex, TPropertyFlag.Redundant];
    
    PropertyType[ord(TProp.cncables)] := TPropertyType.DSSObjectReferenceArrayProperty;
    PropertyOffset[ord(TProp.cncables)] := ptruint(@obj.FWireData);
    PropertyOffset2[ord(TProp.cncables)] := ptruint(DSS.CNDataClass);
    PropertyFlags[ord(TProp.cncables)] := [TPropertyFlag.AltIndex, TPropertyFlag.Redundant];

    PropertyType[ord(TProp.wires)] := TPropertyType.DSSObjectReferenceArrayProperty;
    PropertyOffset[ord(TProp.wires)] := ptruint(@obj.FWireData);
    PropertyOffset2[ord(TProp.wires)] := ptruint(DSS.WireDataClass);
    PropertyWriteFunction[ord(TProp.wires)] := @SetWires;
    PropertyFlags[ord(TProp.wires)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.Redundant];

    // enums
    PropertyType[ord(TProp.units)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.units)] := ptruint(@obj.FUnits);
    PropertyOffset2[ord(TProp.units)] := PtrInt(DSS.UnitsEnum);
    PropertyFlags[ord(TProp.units)] := [TPropertyFlag.IntegerOnArray];

    PropertyType[ord(TProp.linetype)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.linetype)] := ptruint(@obj.FLineType);
    PropertyOffset2[ord(TProp.linetype)] := PtrInt(DSS.LineTypeEnum);

    // object properties
    PropertyType[ord(TProp.spacing)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.spacing)] := ptruint(@obj.LineSpacingObj);
    PropertyOffset2[ord(TProp.spacing)] := ptruint(DSS.LineSpacingClass);

    PropertyType[ord(TProp.Ratings)] := TPropertyType.DoubleDArrayProperty;
    PropertyOffset[ord(TProp.Ratings)] := ptruint(@obj.AmpRatings);
    PropertyOffset2[ord(TProp.Ratings)] := ptruint(@obj.NumAmpRatings);

    PropertyType[ord(TProp.reduce)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.reduce)] := ptruint(@obj.Freduce);

    PropertyType[ord(TProp.nphases)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.Seasons)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.nconds)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.cond)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.nphases)] := ptruint(@obj.FNphases);
    PropertyOffset[ord(TProp.Seasons)] := ptruint(@obj.NumAmpRatings);
    PropertyOffset[ord(TProp.nconds)] := ptruint(@obj.FNconds);
    PropertyOffset[ord(TProp.cond)] := ptruint(@obj.FActiveCond);
    PropertyFlags[ord(TProp.nphases)] := [TPropertyFlag.NonNegative]; // phases can be zero (e.g. only neutral cables)
    PropertyFlags[ord(TProp.nconds)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];
    PropertyFlags[ord(TProp.cond)] := [TPropertyFlag.IntegerStructIndex];

    PropertyType[ord(TProp.wire)] := TPropertyType.StringOnArrayProperty;
    PropertyType[ord(TProp.cncable)] := TPropertyType.StringOnArrayProperty;
    PropertyType[ord(TProp.tscable)] := TPropertyType.StringOnArrayProperty;

    //TODO: for these, validate that the conductor is actually of the target type instead of returning for all of them
    PropertyOffset[ord(TProp.wire)] := ptruint(@obj.FCondName); PropertyOffset2[ord(TProp.wire)] := ptruint(@obj.FActiveCond);
    PropertyOffset[ord(TProp.cncable)] := ptruint(@obj.FCondName); PropertyOffset2[ord(TProp.cncable)] := ptruint(@obj.FActiveCond);
    PropertyOffset[ord(TProp.tscable)] := ptruint(@obj.FCondName); PropertyOffset2[ord(TProp.tscable)] := ptruint(@obj.FActiveCond);

    PropertyType[ord(TProp.x)] := TPropertyType.DoubleOnArrayProperty;
    PropertyType[ord(TProp.h)] := TPropertyType.DoubleOnArrayProperty;
    PropertyOffset[ord(TProp.x)] := ptruint(@obj.FX); PropertyOffset2[ord(TProp.x)] := ptruint(@obj.FActiveCond);
    PropertyOffset[ord(TProp.h)] := ptruint(@obj.FY); PropertyOffset2[ord(TProp.h)] := ptruint(@obj.FActiveCond);

    PropertyOffset[ord(TProp.NormAmps)] := ptruint(@obj.NormAmps);
    PropertyOffset[ord(TProp.EmergAmps)] := ptruint(@obj.EmergAmps);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TLineGeometry.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TLineGeometryObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    tmpName: String;
    i: Integer;
    conductorObj: TConductorDataObj = NIL;
begin
    case Idx of
        ord(TProp.nphases):
            if FLineData <> NIL then
            begin
                FLineData.Nphases := FNPhases;
                if (FLineData.Nphases > FNconds) then
                    FLineData.Nphases := FNConds;
            end;
        ord(TProp.cond):
            if Funits^[FactiveCond] = -1 then
                Funits^[FactiveCond] := FLastUnit;  // makes this a sticky value so you don't have to repeat it
        ord(TProp.wire):
            if FPhaseChoice^[ActiveCond] = Unknown then
                ChangeLineConstantsType(Overhead);
        ord(TProp.units):
            FLastUnit := FUnits^[ActiveCond];
        ord(TProp.cncable), ord(TProp.cncables):
            ChangeLineConstantsType(ConcentricNeutral);
        ord(TProp.tscable), ord(TProp.tscables):
            ChangeLineConstantsType(TapeShield);
        ord(TProp.nconds):
        begin
            if previousIntVal <> FNConds then
            begin
                if Assigned(FLineData) then
                    FreeAndNil(FLineData);

                // Allocations
                Reallocmem(FWireData, Sizeof(FWireData^[1]) * FNconds);
                Reallocmem(FX, Sizeof(FX^[1]) * FNconds);
                Reallocmem(FY, Sizeof(FY^[1]) * FNconds);
                Reallocmem(FUnits, Sizeof(Funits^[1]) * FNconds);
                Reallocmem(FPhaseChoice, Sizeof(FPhaseChoice^[1]) * FNconds);
                SetLength(FCondName, FNconds);
            end
            else
            begin
                for i := 1 to FNconds do
                    FCondName[i - 1] := '';
            end;
                
            if FNconds > previousIntVal then
                for i := Max(1, previousIntVal) to FNconds do
                    FPhaseChoice^[i] := Unknown;

            for i := 1 to FNconds do
            begin
                FActiveCond := i;
                ChangeLineConstantsType(Overhead); // works on activecond
            end;
            // Reset the active conductor
            FActiveCond := 1;

            // Initialize Allocations
            for i := 1 to FNconds do
                FPhaseChoice^[i] := Overhead;
            for i := 1 to FNconds do
                FWireData^[i] := NIL;
            for i := 1 to FNconds do
                FX^[i] := 0.0;
            for i := 1 to FNconds do
                FY^[i] := 0.0;
            for i := 1 to FNconds do
                FUnits^[i] := -1;  // default to ft
            FLastUnit := UNITS_FT;
        end;
        ord(TProp.spacing):
            if LineSpacingObj <> NIL then
            begin
                if (FNConds = LineSpacingObj.NWires) then
                begin
                    FLastUnit := LineSpacingObj.Units;
                    for i := 1 to FNConds do
                    begin
                        FX^[i] := LineSpacingObj.Xcoord[i];
                        FY^[i] := LineSpacingObj.Ycoord[i];
                        FUnits^[i] := FLastUnit;
                    end
                end
                else
                    DoSimpleMsg('LineSpacing object %s has the wrong number of wires.', [LineSpacingObj.Name], 10103);
            end;
    end;
    case Idx of 
        ord(TProp.wires), ord(TProp.cncables), ord(TProp.tscables):
        begin
            i := 1;
            if Idx = ord(TProp.wires) then
            begin
                if FPhaseChoice^[ActiveCond] = Unknown then
                begin
                    // no other cables set for ActiveCond
                end
                else 
                if FPhaseChoice^[ActiveCond] <> Overhead then
                    // these are buried neutral wires
                    // (only when the phase conductors not overhead)
                    i := FNPhases + 1;
            end;
            if i = 1 then
            begin
                conductorObj := FWireData^[1];
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
        ord(TProp.wire), ord(TProp.cncable), ord(TProp.tscable):
        begin
            tmpName := FCondName[ActiveCond - 1];
            if Idx = ord(TProp.wire) then
                conductorObj := DSS.WireDataClass.Find(tmpName, False)
            else
            if Idx = ord(TProp.cncable) then
                conductorObj := DSS.CNDataClass.Find(tmpName, False)
            else
                conductorObj := DSS.TSDataClass.Find(tmpName, False);

            if Assigned(conductorObj) then
            begin
                FWireData^[ActiveCond] := conductorObj;
                // Default the current ratings for this geometry to the rating of the first conductor
                if (ActiveCond = 1) then
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
            if Idx = ord(TProp.wire) then
                DoSimpleMsg('WireData Object "%s" not defined. Must be previously defined.', [tmpName], 10103)
            else
            if Idx = ord(TProp.cncable) then
                DoSimpleMsg('CNData Object "%s" not defined. Must be previously defined.', [tmpName], 10103)
            else
                DoSimpleMsg('TSData Object "%s" not defined. Must be previously defined.', [tmpName], 10103);
        end;
        ord(TProp.Seasons):
            setlength(AmpRatings, NumAmpRatings);
    end;

    case Idx of
        1, 4..7, 11..16:
            DataChanged := TRUE;
    end;

    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TLineGeometryObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    NConds := Other.NWires;   // allocates
    FNphases := Other.FNphases;
    LineSpacingObj := Other.LineSpacingObj;
    FLineType := Other.FLineType; //TODO: check original
    for i := 1 to FNConds do
        FPhaseChoice^[i] := Other.FPhaseChoice^[i];
    for i := 1 to FNConds do
        FCondName[i - 1] := Other.FCondName[i - 1];
    for i := 1 to FNConds do
        FWireData^[i] := Other.FWireData^[i];
    for i := 1 to FNConds do
        FX^[i] := Other.FX^[i];
    for i := 1 to FNConds do
        FY^[i] := Other.FY^[i];
    for i := 1 to FNConds do
        FUnits^[i] := Other.FUnits^[i];
    DataChanged := TRUE;
    NormAmps := Other.NormAmps;
    EmergAmps := Other.EmergAmps;
    FReduce := Other.FReduce;

    UpdateLineGeometryData(activecircuit.solution.Frequency);
end;

constructor TLineGeometryObj.Create(ParClass: TDSSClass; const LineGeometryName: String);
begin
    inherited Create(ParClass);
    Name := LowerCase(LineGeometryName);
    DSSObjType := ParClass.DSSClassType;

    DataChanged := TRUE;

    FPhaseChoice := NIL;
    FCondName := NIL;
    FWireData := NIL;
    FX := NIL;
    FY := NIL;
    Funits := NIL;
    FLineData := NIL;
    LineSpacingObj := NIL;

    // was causing unnecessary allocations (was leaving dangling memory)
    // Nconds      := 3;  // Allocates terminals
    // FNphases    := 3;

    FNconds := 0;
    FNPhases := 0;
    // ActiveCond  := 1;
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
    if FLineData <> NIL then
        FLineData.Free;
    SetLength(FCondName, 0);
    Reallocmem(Fwiredata, 0);
    Reallocmem(FY, 0);
    Reallocmem(FX, 0);
    Reallocmem(Funits, 0);
    Reallocmem(FPhaseChoice, 0);

    inherited destroy;
end;

procedure TLineGeometryObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);
var
    i, j: Integer;
begin
    inherited DumpProperties(F, Complete);

    for i := 1 to 2 do
    begin
        FSWriteln(F, '~ ' + ParentClass.PropertyName^[i] + '=' + GetPropertyValue(i));
    end;
    for j := 1 to FNConds do
    begin
        ActiveCond := j;
        FSWriteln(F, '~ ' + ParentClass.PropertyName^[3] + '=' + GetPropertyValue(3));
        FSWriteln(F, '~ ' + ParentClass.PropertyName^[4] + '=' + GetPropertyValue(4));
        FSWriteln(F, '~ ' + ParentClass.PropertyName^[5] + '=' + GetPropertyValue(5));
        FSWriteln(F, '~ ' + ParentClass.PropertyName^[6] + '=' + GetPropertyValue(6));
        FSWriteln(F, '~ ' + ParentClass.PropertyName^[7] + '=' + GetPropertyValue(7));
    end;
    for i := 8 to ParentClass.NumProperties do
    begin
        FSWriteln(F, '~ ' + ParentClass.PropertyName^[i] + '=' + GetPropertyValue(i));
    end;
end;

function TLineGeometryObj.Get_FX(i: Integer): Double;
begin
    if i <= FNConds then
        Result := FX^[i]
    else
        Result := 0.0;
end;

function TLineGeometryObj.Get_FY(i: Integer): Double;
begin
    if i <= FNConds then
        Result := FY^[i]
    else
        Result := 0.0;
end;

function TLineGeometryObj.Get_FUnits(i: Integer): Integer;
begin
    if i <= FNConds then
        Result := FUnits^[i]
    else
        Result := 0;
end;

procedure TLineGeometryObj.Set_FX(i: Integer; Value: Double);
begin
    if i <= FNConds then
        FX^[i] := Value;
end;

procedure TLineGeometryObj.Set_FY(i: Integer; Value: Double);
begin
    if i <= FNConds then
        FY^[i] := Value;
end;

procedure TLineGeometryObj.Set_FUnits(i: Integer; Value: Integer);
begin
    if i <= FNConds then
        FUnits^[i] := Value;
end;

function TLineGeometryObj.Get_ConductorName(i: Integer): String;
begin
    if i <= FNConds then
        Result := FCondName[i - 1]
    else
        Result := '';
end;

function TLineGeometryObj.Get_ConductorData(i: Integer): TConductorDataObj;
begin
    if i <= FNConds then
        Result := FWireData^[i]
    else
        Result := NIL;
end;

function TLineGeometryObj.get_Nconds: Integer;
begin
    if Freduce then
        Result := FNPhases
    else
        Result := FNConds;
end;

function TLineGeometryObj.Get_PhaseChoice(i: Integer): ConductorChoice;
begin
    Result := FPhaseChoice^[i];
end;

function TLineGeometryObj.Get_RhoEarth: Double;
begin
    Result := FLineData.rhoearth;
end;

function TLineGeometryObj.Get_YCmatrix(f, Lngth: Double;
    Units: Integer): Tcmatrix;
begin
    Result := NIL;
    if DataChanged then
        UpdateLineGeometryData(f);
    if not DSS.SolutionAbort then
        Result := FLineData.YCMatrix[f, Lngth, Units];
end;

function TLineGeometryObj.Get_Zmatrix(f, Lngth: Double;
    Units: Integer): Tcmatrix;
begin
    Result := NIL;
    if DataChanged then
        UpdateLineGeometryData(f);
    if not DSS.SolutionAbort then
        Result := FLineData.ZMatrix[F, Lngth, Units, DSS.ActiveEarthModel];
end;

procedure TLineGeometryObj.SaveWrite(F: TFileStream);
// Override standard SaveWrite
// Linegeometry structure not conducive to standard means of saving
var
    strPhaseChoice: String;
    iprop: Integer;
    i: Integer;
begin
    // Write only properties that were explicitly set in the
    // final order they were actually set
    iProp := GetNextPropertySet(0);
    if iProp > 0 then
        FSWriteln(F);

    while iProp > 0 do
    begin
        with ParentClass do
            case iProp of
                3, 11, 12:
                begin   // if cond=, spacing, or wires were ever used write out arrays ...
                    for i := 1 to Fnconds do
                    begin
                        case PhaseChoice[i] of
                            Overhead:
                                strPhaseChoice := 'wire';
                            ConcentricNeutral:
                                strPhaseChoice := 'cncable';
                            TapeShield:
                                strPhaseChoice := 'tscable';
                        else
                            strPhaseChoice := 'wire';
                        end;
                        FSWriteln(F, Format('~ Cond=%d %s=%s X=%.7g h=%.7g units=%s',
                            [i, strPhaseChoice, FCondName[i - 1], FX^[i], FY^[i], LineUnitsStr(FUnits^[i])]));
                    end;
                end;
                4..7:
                    ; // Ignore these properties;
                10:
                    if FReduce then
                        FSWriteln(F, '~ Reduce=Yes');
                13..14: ;   // do nothing; Ignore these properties;
            else
                FSWriteln(F, Format('~ %s=%s', [PropertyName[iProp], CheckForBlanks(PropertyValue[iProp])]));
            end;
            iProp := GetNextPropertySet(iProp);
    end;
end;

procedure TLineGeometryObj.set_ActiveCond(const Value: Integer);
begin
    if Value > 0 then
        if Value <= FNconds then
        begin
            FActiveCond := Value;
            if Funits^[FactiveCond] = -1 then
                Funits^[FactiveCond] := FLastUnit;  // makes this a sticky value so you don't have to repeat it
        end;
end;

procedure TLineGeometryObj.ChangeLineConstantsType(newPhaseChoice: ConductorChoice);
var
    newLineData: TLineConstants;
    needNew: Boolean;
begin
    newLineData := NIL;
    needNew := FALSE;

    if (ActiveCond > 0) and (ActiveCond <= FNConds) and 
       (newPhaseChoice <> FPhaseChoice^[ActiveCond]) then
        needNew := TRUE
    else
    if (FLineData = NIL) or (FNConds <> FLineData.Nconductors) then
        needNew := TRUE;

    if needNew then
        case newPhaseChoice of
            Overhead:
                newLineData := TOHLineConstants.Create(FNConds);
            ConcentricNeutral:
                newLineData := TCNLineConstants.Create(FNConds);
            TapeShield:
                newLineData := TTSLineConstants.Create(FNConds);
        end;

    if Assigned(newLineData) then
    begin
        if Assigned(FLineData) then
        begin
            newLineData.Nphases := FLineData.Nphases;
            newLineData.rhoearth := FLineData.rhoearth;
        end;
        FreeAndNil(FLineData);
        FLineData := newLineData;
    end;
    if (ActiveCond > 0) and (ActiveCond <= FNConds) then
        FPhaseChoice^[ActiveCond] := newPhaseChoice;
end;

procedure TLineGeometryObj.set_Nconds(const Value: Integer);
var
    prev: Integer;
begin
    prev := Fnconds;
    Fnconds := Value;
    PropertySideEffects(ord(TProp.nconds), prev)
end;

procedure TLineGeometryObj.set_Nphases(const Value: Integer);
begin
    if Value < 1 then
    begin
        DoSimpleMsg(_('Invalid number of phases sent via DSS command. Please enter a value within range.'), 186);
        Exit;
    end;

    FNphases := Value;
    FLineData.Nphases := Value;
end;

procedure TLineGeometryObj.Set_RhoEarth(const Value: Double);
begin
    FLineData.RhoEarth := Value;
end;

procedure TLineGeometryObj.UpdateLineGeometryData(f: Double);
var
    i: Integer;
    LineGeomErrMsg: String;
    cnd: TCNDataObj;
    tsd: TTSDataObj;
begin
    for i := 1 to FNconds do
    begin
        FLineData.X[i, Funits^[i]] := FX^[i];
        FLineData.Y[i, Funits^[i]] := FY^[i];
        FLineData.radius[i, FWireData^[i].RadiusUnits] := FWireData^[i].Radius;
        FLineData.capradius[i, FWireData^[i].RadiusUnits] := FWireData^[i].capRadius;
        FLineData.GMR[i, FWireData^[i].GMRUnits] := FWireData^[i].GMR;
        FLineData.Rdc[i, FWireData^[i].ResUnits] := FWireData^[i].Rdc;
        FLineData.Rac[i, FWireData^[i].ResUnits] := FWireData^[i].Rac;
        if (FWireData^[i] is TCNDataObj) then
        begin
            with (FLineData as TCNLineConstants) do
            begin
                cnd := (FWireData^[i] as TCNDataObj);
                EpsR[i] := cnd.EpsR;
                InsLayer[i, cnd.RadiusUnits] := cnd.InsLayer;
                DiaIns[i, cnd.RadiusUnits] := cnd.DiaIns;
                DiaCable[i, cnd.RadiusUnits] := cnd.DiaCable;
                kStrand[i] := cnd.NStrand;
                DiaStrand[i, cnd.RadiusUnits] := cnd.DiaStrand;
                GmrStrand[i, cnd.GMRUnits] := cnd.GmrStrand;
                RStrand[i, cnd.ResUnits] := cnd.RStrand;
            end;
        end
        else
        if (FWireData^[i] is TTSDataObj) then
        begin
            with (FLineData as TTSLineConstants) do
            begin
                tsd := (FWireData^[i] as TTSDataObj);
                EpsR[i] := tsd.EpsR;
                InsLayer[i, tsd.RadiusUnits] := tsd.InsLayer;
                DiaIns[i, tsd.RadiusUnits] := tsd.DiaIns;
                DiaCable[i, tsd.RadiusUnits] := tsd.DiaCable;
                DiaShield[i, tsd.RadiusUnits] := tsd.DiaShield;
                TapeLayer[i, tsd.RadiusUnits] := tsd.TapeLayer;
                TapeLap[i] := tsd.TapeLap;
            end;
        end;
    end;

    FLineData.Nphases := FNphases;
    DataChanged := FALSE;

    // Before we calc, check for bad conductor definitions
    if FLineData.ConductorsInSameSpace(LineGeomErrMsg) then
    begin
        raise ELineGeometryProblem.Create(Format(_('Error in %s: %s'), [FullName, LineGeomErrMsg]));
        DSS.SolutionAbort := TRUE;
    end
    else
    begin
        FLineData.Calc(f, DSS.ActiveEarthModel); // ***** Line impedance calc'd here ****
        if FReduce then
            FLineData.Reduce; // reduce out neutrals
    end;
end;

procedure TLineGeometryObj.LoadSpacingAndWires(Spc: TLineSpacingObj; Wires: pConductorDataArray);
var
    i: Integer;
    newPhaseChoice: ConductorChoice;
begin
    NConds := Spc.NWires;   // allocates
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
        FCondName[i - 1] := Wires^[i].Name;
    for i := 1 to FNConds do
        FWireData^[i] := Wires^[i];
    for i := 1 to FNConds do
        FX^[i] := Spc.Xcoord[i];
    for i := 1 to FNConds do
        FY^[i] := Spc.Ycoord[i];
    for i := 1 to FNConds do
        FUnits^[i] := Spc.Units;
    DataChanged := TRUE;
    NormAmps := Wires^[1].NormAmps;
    EmergAmps := Wires^[1].EmergAmps;

    UpdateLineGeometryData(activecircuit.solution.Frequency);
end;

initialization
    PropInfo := NIL;
end.
