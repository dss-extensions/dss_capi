unit Isource;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

// Ideal current source
// 
// Stick'em on wherever you want as many as you want
// 
// ISource maintains a positive sequence for harmonic scans.  If you want zero sequence,
// use three single-phase ISource.

interface

uses
    Classes,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    UComplex, DSSUcomplex,
    Spectrum,
    Loadshape;

type
{$SCOPEDENUMS ON}
    TIsourceProp = (
        INVALID = 0,
        bus1 = 1,
        amps = 2,
        angle = 3,
        frequency = 4,
        phases = 5,
        scantype = 6,
        sequence = 7,
        Yearly = 8,
        Daily = 9,
        Duty = 10,
        Bus2 = 11
    );
{$SCOPEDENUMS OFF}

    TIsource = class(TPCClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TIsourceObj = class(TPCElement)
    PRIVATE

        FphaseShift: Double;
        ShapeIsActual: Boolean;
        ShapeFactor: Complex;
        Bus2Defined: Boolean;

        function GetBaseCurr: Complex;
        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);
        procedure CalcYearlyMult(Hr: Double);
        procedure GetInjCurrents(Curr: pComplexArray);

    PUBLIC

        Amps: Double;
        Angle: Double;
        SrcFrequency: Double;
        ScanType,
        SequenceType: Integer;
        PerUnit: Double;
        DailyShapeObj: TLoadShapeObj;  // Daily (24 HR) load shape
        DutyShapeObj: TLoadShapeObj;  // Duty cycle load shape FOR changes typically less than one hour
        YearlyShapeObj: TLoadShapeObj;  // Shape for this load

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;        
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        function InjCurrents: Integer; OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE;
    end;

implementation

uses
    Circuit,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    Sysutils,
    Command,
    dynamics,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TISourceObj;
    TProp = TIsourceProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer;    

constructor TIsource.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, SOURCE or NON_PCPD_ELEM, 'Isource');
end;

destructor TIsource.Destroy;
begin
    inherited Destroy;
end;

procedure TIsource.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // integer properties
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyType[ord(TProp.bus2)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;
    PropertyOffset[ord(TProp.bus2)] := 2;

    // enum properties
    PropertyType[ord(TProp.ScanType)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.ScanType)] := ptruint(@obj.ScanType);
    PropertyOffset2[ord(TProp.ScanType)] := PtrInt(DSS.ScanTypeEnum);

    PropertyType[ord(TProp.Sequence)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Sequence)] := ptruint(@obj.Sequencetype);
    PropertyOffset2[ord(TProp.Sequence)] := PtrInt(DSS.SequenceEnum);

    // object properties
    PropertyType[ord(TProp.yearly)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.daily)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.duty)] := TPropertyType.DSSObjectReferenceProperty;
    
    PropertyOffset[ord(TProp.yearly)] := ptruint(@obj.YearlyShapeObj);
    PropertyOffset[ord(TProp.daily)] := ptruint(@obj.DailyShapeObj);
    PropertyOffset[ord(TProp.duty)] := ptruint(@obj.DutyShapeObj);

    PropertyOffset2[ord(TProp.yearly)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.daily)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.duty)] := ptruint(DSS.LoadShapeClass);

    // double properties
    PropertyOffset[ord(TProp.amps)] := ptruint(@obj.Amps);
    PropertyOffset[ord(TProp.angle)] := ptruint(@obj.Angle);
    PropertyOffset[ord(TProp.frequency)] := ptruint(@obj.SrcFrequency);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TIsource.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TIsourceObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    S, S2: String;
    i, dotpos: Integer;
begin
    case Idx of
        ord(TProp.Phases):
        begin
            case FNphases of
                1:
                    FphaseShift := 0.0;
                2, 3:
                    FphaseShift := 120.0;
            else     // higher order systems
                FphaseShift := 360.0 / FNphases;
            end;
            NConds := Fnphases;  // Force Reallocation of terminal info
        end;
        ord(TProp.bus1):
            // Special handling for Bus 1
            // Set Bus2 = Bus1.0.0.0
            if not Bus2Defined then // Default Bus2 to zero node of Bus1. (Grounded-Y connection)
            begin
            // Strip node designations from S
                S := GetBus(1);
                dotpos := Pos('.', S);
                if dotpos > 0 then
                    S2 := Copy(S, 1, dotpos - 1)
                else
                    S2 := Copy(S, 1, Length(S));  // copy up to Dot
                for i := 1 to Fnphases do
                    S2 := S2 + '.0';   // append series of ".0"'s

                SetBus(2, S2);    // default setting for Bus2
            end;
        9:
            // If Yearly load shape is not yet defined, make it the same as Daily
            if YearlyShapeObj = NIL then
                YearlyShapeObj := DailyShapeObj;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TIsource.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        RecalcElementData;
        YPrimInvalid := TRUE;
        Exclude(Flags, Flg.EditionActive);
    end;
    Result := True;
end;

procedure TIsourceObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr); // set spectrum,  base frequency

    Other := TObj(OtherPtr);
    if Fnphases <> Other.Fnphases then
    begin
        FNphases := Other.Fnphases;
        NConds := Fnphases;  // Forces reallocation of terminal stuff

        Yorder := Fnconds * Fnterms;
        YPrimInvalid := TRUE;
    end;

    Amps := Other.Amps;
    Angle := Other.Angle;
    SrcFrequency := Other.SrcFrequency;
    Scantype := Other.Scantype;
    Sequencetype := Other.Sequencetype;

    ShapeIsActual := Other.ShapeIsActual;
    DailyShapeObj := Other.DailyShapeObj;
    DutyShapeObj := Other.DutyShapeObj;
    YearlyShapeObj := Other.YearlyShapeObj;

    Bus2Defined := Other.Bus2Defined;
end;

constructor TIsourceObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; // SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

    FNphases := 3;
    Fnconds := 3;
    Nterms := 2;   // 4/27/2018 made a 2-terminal I source

    Amps := 0.0;
    Angle := 0.0;
    // TODO: remember to check if PerUnit is correctly applied if is exposed in the future
    PerUnit := 1.0;  // for future use if pu property added,
    SrcFrequency := BaseFrequency;
    FphaseShift := 120.0;
    ScanType := 1;  // Pos Sequence
    Sequencetype := 1;
    Bus2Defined := FALSE;
    ShapeIsActual := FALSE;
    YearlyShapeObj := NIL;
    DailyShapeObj := NIL;
    DutyShapeObj := NIL;

    Yorder := Fnterms * Fnconds;
    RecalcElementData;
end;

destructor TIsourceObj.Destroy;
begin
    inherited Destroy;
end;

procedure TIsourceObj.RecalcElementData;
begin
    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);
end;

procedure TIsourceObj.CalcYPrim;
begin
     // Build only YPrim Series
    if (Yprim = NIL) OR (Yprim.order <> Yorder) OR (Yprim_Series = NIL) {YPrimInvalid} then
    begin
        if YPrim_Series <> NIL then
            YPrim_Series.Free;
        YPrim_Series := TcMatrix.CreateMatrix(Yorder);
        if YPrim <> NIL then
            YPrim.Free;
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin
        YPrim_Series.Clear;
        YPrim.Clear;
    end;

    // Yprim = 0  for Ideal Current Source;  just leave it zeroed

    // Now Account for Open Conductors
    // For any conductor that is open, zero out row and column
    inherited CalcYPrim;

    YPrimInvalid := FALSE;
end;

function TIsourceObj.GetBaseCurr: Complex;
var
    SrcHarmonic: Double;
    NAmps: Double;
begin
    try
        with ActiveCircuit.Solution do
            // Get first Phase Current
            if IsHarmonicModel then
            begin
                SrcHarmonic := Frequency / SrcFrequency;
                Result := SpectrumObj.GetMult(SrcHarmonic) * Amps;  // Base current for this harmonic
                RotatePhasorDeg(Result, SrcHarmonic, Angle);
            end
            else
            begin
                case Mode of
                    // Uses same logic as LOAD
                    TSolveMode.DAILYMODE:
                    begin
                        CalcDailyMult(DynaVars.dblHour);
                    end;
                    TSolveMode.YEARLYMODE:
                    begin
                        CalcYearlyMult(DynaVars.dblHour);
                    end;
                    TSolveMode.DUTYCYCLE:
                    begin
                        CalcDutyMult(DynaVars.dblHour);
                    end;
                    TSolveMode.DYNAMICMODE:
                    begin
                        // This mode allows use of one class of load shape in DYNAMIC mode
                        case ActiveCircuit.ActiveLoadShapeClass of
                            USEDAILY:
                                CalcDailyMult(DynaVars.dblHour);
                            USEYEARLY:
                                CalcYearlyMult(DynaVars.dblHour);
                            USEDUTY:
                                CalcDutyMult(DynaVars.dblHour);
                        else
                            ShapeFactor := Cmplx(1.0, 0.0);     // default to 1 + j0 if not known
                        end;
                    end;
                end;
                NAmps := Amps;
                if (Mode = TSolveMode.DAILYMODE) or     // If a loadshape mode simulation
                    (Mode = TSolveMode.YEARLYMODE) or
                    (Mode = TSolveMode.DUTYCYCLE) or 
                    (Mode = TSolveMode.DYNAMICMODE) then
                    NAmps := Amps * ShapeFactor.re;
                if abs(Frequency - SrcFrequency) < EPSILON2 then
                    Result := pdegtocomplex(NAmps, Angle)
                else
                    Result := CZERO;
            end;

    except
        DoSimpleMsg('Error computing current for "%s". Check specification. Aborting.', [FullName], 334);
        if DSS.In_Redirect then
            DSS.Redirect_Abort := TRUE;
    end;
end;

function TIsourceObj.InjCurrents: Integer;
// Sum Currents directly into solution array
begin
    GetInjCurrents(InjCurrent);

    Result := inherited Injcurrents;  // Adds into system array

end;

procedure TIsourceObj.GetCurrents(Curr: pComplexArray);
// Total currents into a device
var
    i: Integer;
begin
    try
        GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
        // Add Together  with yprim currents
        for i := 1 to Yorder do
            Curr^[i] := -ComplexBuffer^[i];

    except
        On E: Exception do
            DoErrorMsg(Format(_('GetCurrents for Element: %s.'), [FullName]), 
                E.Message, _('Inadequate storage allotted for circuit element?'), 335);
    end;
end;

procedure TIsourceObj.GetInjCurrents(Curr: pComplexArray);
// Fill Up an array of injection currents
var
    i: Integer;
    BaseCurr: complex;
begin
    with ActiveCircuit.solution do
    begin
        BaseCurr := GetBaseCurr;   // this func applies spectrum if needed

        for i := 1 to Fnphases do
        begin
            Curr^[i] := BaseCurr;
            Curr^[i + FnPhases] := -BaseCurr;  // 2nd Terminal
            if (i < Fnphases) then
            begin
                if IsHarmonicModel then

                    case ScanType of
                        1:
                            RotatePhasorDeg(BaseCurr, 1.0, -FphaseShift); // maintain positive sequence for isource
                        0: ;  // Do not rotate for zero sequence
                    else
                        RotatePhasorDeg(BaseCurr, Harmonic, -FphaseShift) // rotate by frequency
                        // Harmonic 1 will be pos; 2 is neg; 3 is zero, and so on.
                    end

                else
                    case SequenceType of
                        -1:
                            RotatePhasorDeg(BaseCurr, 1.0, FphaseShift); // Neg seq
                        0: ;  // Do not rotate for zero sequence
                    else
                        RotatePhasorDeg(BaseCurr, 1.0, -FphaseShift); // Maintain pos seq
                    end;
            end;
        end;
    end;
end;

procedure TIsourceObj.MakePosSequence();
begin
    if Fnphases > 1 then
        SetInteger(ord(TProp.Phases), 1);
    inherited;
end;

procedure TISourceObj.CalcDailyMult(Hr: Double);
begin
    if DailyShapeObj <> NIL then
    begin
        ShapeFactor := DailyShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := DailyShapeObj.UseActual;
    end
    else
        ShapeFactor := cmplx(PerUnit, 0.0); // CDOUBLEONE;  // Default to no daily variation
end;

procedure TISourceObj.CalcDutyMult(Hr: Double);
begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := DutyShapeObj.UseActual;
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult IF no duty curve specified
end;

procedure TISourceObj.CalcYearlyMult(Hr: Double);
begin
    // Yearly curve is assumed to be hourly only
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := YearlyShapeObj.UseActual;
    end
    else
        ShapeFactor := cmplx(PerUnit, 0.0); // CDOUBLEONE;   // Defaults to no variation
end;

initialization
    PropInfo := NIL;
end.
