unit XfmrCode;

{
  ----------------------------------------------------------
  Copyright (c) 2009-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Classes,
    Command,
    DSSClass,
    DSSObject,
    UcMatrix,
    arraydef,
    Transformer;

type
{$SCOPEDENUMS ON}
    TXfmrCodeProp = (
        INVALID = 0,
    
        phases = 1,
        windings = 2,

        // Winding Definition
        wdg = 3,
        conn = 4,
        kV = 5, // FOR 2-and 3- always kVLL ELSE actual winding KV
        kVA = 6,
        tap = 7,
        pctR = 8,
        Rneut = 9,
        Xneut = 10,

        // General Data
        conns = 11,
        kVs = 12,
        kVAs = 13,
        taps = 14,
        Xhl = 15,
        Xht = 16,
        Xlt = 17,
        Xscarray = 18, // x12 13 14... 23 24.. 34 ..
        thermal = 19,
        n = 20,
        m = 21,
        flrise = 22,
        hsrise = 23,
        pctloadloss = 24,
        pctnoloadloss = 25,
        normhkVA = 26,
        emerghkVA = 27,
        MaxTap = 28,
        MinTap = 29,
        NumTaps = 30,
        pctimag = 31,
        ppm_antifloat = 32,
        pctRs = 33,
        X12 = 34,
        X13 = 35,
        X23 = 36,
        RdcOhms = 37,
        Seasons = 38,
        Ratings = 39
    );
{$SCOPEDENUMS OFF}

    TXfmrCode = class(TDSSClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;
        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TXfmrCodeObj = class(TDSSObject)
    PUBLIC
        FNPhases: Integer;
        ActiveWinding: Integer;
        NumWindings: Integer;
        MaxWindings: Integer;
        XHL, XHT, XLT: Double;  // per unit
        XSC: pDoubleArray;     // per unit SC measurements
        VABase: Double;    // FOR impedances
        NormMaxHKVA: Double;
        EmergMaxHKVA: Double;
        ThermalTimeConst: Double;  // hr
        n_thermal: Double;
        m_thermal: Double;  // Exponents
        FLrise: Double;
        HSrise: Double;
        pctLoadLoss: Double;
        pctNoLoadLoss: Double;
        ppm_FloatFactor: Double; //  parts per million winding float factor
        pctImag: Double;
        Winding: pWindingArray;

        NumkVARatings: Integer; //TODO: remove, redundant as kVARatings already contains it
        kVARatings: Array Of Double;

        procedure SetNumWindings(N: Integer);
        procedure PullFromTransformer(obj: TTransfObj);

        constructor Create(ParClass: TDSSClass; const XfmrCodeName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;
        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    UComplex, DSSUcomplex,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TXfmrCodeObj;
    TProp = TXfmrCodeProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TXfmrCode.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, DSS_OBJECT, 'XfmrCode');
end;

destructor TXfmrCode.Destroy;
begin
    inherited Destroy;
end;

function XscSize(obj: TObj): Integer;
begin
    with obj do
        Result := (NumWindings - 1) * NumWindings div 2;
end;

procedure TXfmrCode.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    PropertyStructArrayOffset := ptruint(@obj.Winding);
    PropertyStructArrayStep := SizeOf(TWinding);
    PropertyStructArrayIndexOffset := ptruint(@obj.ActiveWinding);
    PropertyStructArrayCountOffset := ptruint(@obj.NumWindings);

    // double arrays
    PropertyType[ord(TProp.Ratings)] := TPropertyType.DoubleDArrayProperty;
    PropertyOffset[ord(TProp.Ratings)] := ptruint(@obj.kVARatings);
    PropertyOffset2[ord(TProp.Ratings)] := ptruint(@obj.NumkVARatings);

    PropertyType[ord(TProp.Xscarray)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.Xscarray)] := ptruint(@obj.Xsc);
    PropertyOffset3[ord(TProp.Xscarray)] := ptruint(@XscSize);
    PropertyFlags[ord(TProp.Xscarray)] := [TPropertyFlag.SizeIsFunction];
    PropertyScale[ord(TProp.Xscarray)] := 0.01;

    // integer properties
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.Seasons)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNphases);
    PropertyOffset[ord(TProp.Seasons)] := ptruint(@obj.NumkVARatings);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    PropertyType[ord(TProp.windings)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.windings)] := ptruint(@obj.NumWindings);
    PropertyFlags[ord(TProp.windings)] := [TPropertyFlag.GreaterThanOne];

    PropertyType[ord(TProp.wdg)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.wdg)] := ptruint(@obj.ActiveWinding);
    PropertyFlags[ord(TProp.wdg)] := [TPropertyFlag.IntegerStructIndex];

    // double-on-struct array properties
    PropertyType[ord(TProp.kV)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.kV)] := ptruint(@TWinding(nil^).kVLL);

    PropertyType[ord(TProp.kVA)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.kVA)] := ptruint(@TWinding(nil^).kVA);

    PropertyType[ord(TProp.tap)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.tap)] := ptruint(@TWinding(nil^).puTap);

    PropertyType[ord(TProp.Rneut)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.Rneut)] := ptruint(@TWinding(nil^).Rneut);

    PropertyType[ord(TProp.Xneut)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.Xneut)] := ptruint(@TWinding(nil^).Xneut);

    PropertyType[ord(TProp.MaxTap)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.MaxTap)] := ptruint(@TWinding(nil^).MaxTap);

    PropertyType[ord(TProp.MinTap)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.MinTap)] := ptruint(@TWinding(nil^).MinTap);

    PropertyType[ord(TProp.RdcOhms)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.RdcOhms)] := ptruint(@TWinding(nil^).RdcOhms);

    PropertyType[ord(TProp.pctR)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.pctR)] := ptruint(@TWinding(nil^).Rpu);
    PropertyScale[ord(TProp.pctR)] := 0.01;

    // integer on struct array
    PropertyType[ord(TProp.NumTaps)] := TPropertyType.IntegerOnStructArrayProperty;
    PropertyOffset[ord(TProp.NumTaps)] := ptruint(@TWinding(nil^).NumTaps);

    // enum on array of structs
    PropertyType[ord(TProp.conn)] := TPropertyType.MappedStringEnumOnStructArrayProperty;
    PropertyOffset[ord(TProp.conn)] := ptruint(@TWinding(nil^).Connection);
    PropertyOffset2[ord(TProp.conn)] := PtrInt(DSS.ConnectionEnum);

    // array of enums on array of structs
    PropertyType[ord(TProp.conns)] := TPropertyType.MappedStringEnumArrayOnStructArrayProperty;
    PropertyOffset[ord(TProp.conns)] := ptruint(@TWinding(nil^).Connection);
    PropertyOffset2[ord(TProp.conns)] := PtrInt(DSS.ConnectionEnum);
    PropertyFlags[ord(TProp.conns)] := [TPropertyFlag.Redundant];

    // double percent properties
    PropertyScale[ord(TProp.XHL)] := 0.01;
    PropertyScale[ord(TProp.XHT)] := 0.01;
    PropertyScale[ord(TProp.XLT)] := 0.01;
    PropertyScale[ord(TProp.X12)] := 0.01;
    PropertyScale[ord(TProp.X13)] := 0.01;
    PropertyScale[ord(TProp.X23)] := 0.01;
    PropertyOffset[ord(TProp.XHL)] := ptruint(@obj.XHL);
    PropertyOffset[ord(TProp.XHT)] := ptruint(@obj.XHT);
    PropertyOffset[ord(TProp.XLT)] := ptruint(@obj.XLT);
    PropertyOffset[ord(TProp.X12)] := ptruint(@obj.XHL);
    PropertyOffset[ord(TProp.X13)] := ptruint(@obj.XHT);
    PropertyOffset[ord(TProp.X23)] := ptruint(@obj.XLT);
    PropertyFlags[ord(TProp.X12)] := [TPropertyFlag.Redundant];
    PropertyFlags[ord(TProp.X13)] := [TPropertyFlag.Redundant];
    PropertyFlags[ord(TProp.X23)] := [TPropertyFlag.Redundant];

    // scaled double
    PropertyOffset[ord(TProp.ppm_antifloat)] := ptruint(@obj.ppm_FloatFactor);
    PropertyScale[ord(TProp.ppm_antifloat)] := 1.0e-6;

    // double properties
    PropertyOffset[ord(TProp.thermal)] := ptruint(@obj.ThermalTimeConst);
    PropertyOffset[ord(TProp.n)] := ptruint(@obj.n_thermal);
    PropertyOffset[ord(TProp.m)] := ptruint(@obj.m_thermal);
    PropertyOffset[ord(TProp.flrise)] := ptruint(@obj.FLrise);
    PropertyOffset[ord(TProp.hsrise)] := ptruint(@obj.HSRise);
    PropertyFlags[ord(TProp.thermal)] := [TPropertyFlag.Unused];
    PropertyFlags[ord(TProp.n)] := [TPropertyFlag.Unused];
    PropertyFlags[ord(TProp.m)] := [TPropertyFlag.Unused];
    PropertyFlags[ord(TProp.flrise)] := [TPropertyFlag.Unused];
    PropertyFlags[ord(TProp.hsrise)] := [TPropertyFlag.Unused];

    PropertyOffset[ord(TProp.pctloadloss)] := ptruint(@obj.pctLoadLoss);
    PropertyOffset[ord(TProp.pctnoloadloss)] := ptruint(@obj.pctNoLoadLoss);
    PropertyOffset[ord(TProp.normhkVA)] := ptruint(@obj.NormMaxHkVA);
    PropertyOffset[ord(TProp.emerghkVA)] := ptruint(@obj.EmergMaxHkVA);
    PropertyOffset[ord(TProp.pctimag)] := ptruint(@obj.pctImag);

    // double arrays via struct array
    PropertyType[ord(TProp.pctRs)] := TPropertyType.DoubleArrayOnStructArrayProperty;
    PropertyOffset[ord(TProp.pctRs)] := ptruint(@TWinding(nil^).Rpu); 
    PropertyOffset2[ord(TProp.pctRs)] := ptruint(@obj.NumWindings);
    PropertyScale[ord(TProp.pctRs)] := 0.01;
    PropertyFlags[ord(TProp.pctRs)] := [TPropertyFlag.Redundant];

    PropertyType[ord(TProp.kVs)] := TPropertyType.DoubleArrayOnStructArrayProperty;
    PropertyOffset[ord(TProp.kVs)] := ptruint(@TWinding(nil^).kVLL); 
    PropertyOffset2[ord(TProp.kVs)] := ptruint(@obj.NumWindings);
    PropertyFlags[ord(TProp.kVs)] := [TPropertyFlag.Redundant];

    PropertyType[ord(TProp.kVAs)] := TPropertyType.DoubleArrayOnStructArrayProperty;
    PropertyOffset[ord(TProp.kVAs)] := ptruint(@TWinding(nil^).kVA); 
    PropertyOffset2[ord(TProp.kVAs)] := ptruint(@obj.NumWindings);
    PropertyFlags[ord(TProp.kVAs)] := [TPropertyFlag.Redundant];

    PropertyType[ord(TProp.taps)] := TPropertyType.DoubleArrayOnStructArrayProperty;
    PropertyOffset[ord(TProp.taps)] := ptruint(@TWinding(nil^).puTap); 
    PropertyOffset2[ord(TProp.taps)] := ptruint(@obj.NumWindings);
    PropertyFlags[ord(TProp.taps)] := [TPropertyFlag.Redundant];

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TXfmrCode.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TXfmrCodeObj.SetNumWindings(N: Integer);
var
    prev: Integer;
begin
    prev := NumWindings;
    NumWindings := N;
    PropertySideEffects(ord(TProp.windings), prev);
end;

procedure TXfmrCodeObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    i: Integer;
    OldXSCSize, NewXSCSize: Integer;
begin
    case Idx of
        // default all winding kvas to first winding so latter Donot have to be specified
        ord(TProp.windings):
        begin
            // Reallocate stuff if bigger
            OldXSCSize := (previousIntVal - 1) * previousIntVal div 2;
            MaxWindings := NumWindings;
            NewXSCSize := (NumWindings - 1) * NumWindings div 2;
            Reallocmem(Winding, Sizeof(TWinding) * MaxWindings);  // Reallocate collector array
            for i := 1 to MaxWindings do
                Winding[i].Init();
            ReAllocmem(XSC, SizeOF(XSC^[1]) * NewXSCSize);
            for i := OldXSCSize + 1 to NewXSCSize do
            begin
                XSC^[i] := 0.30;   // default to something
            end
        end;
        ord(TProp.kVA):
            if (ActiveWinding = 1) then
            begin
                for i := 2 to NumWindings do
                    Winding^[i].kVA := Winding^[1].kVA;
                NormMaxHkVA := 1.1 * Winding^[1].kVA;    // Defaults for new winding rating.
                EmergMaxHkVA := 1.5 * Winding^[1].kVA;
            end
            else
            if NumWindings = 2 then
            begin
                Winding^[1].kVA := Winding^[2].kVA;  // For 2-winding, force both kVAs to be same
            end;
        // Update LoadLosskW if winding %r changed. Using only windings 1 and 2
        ord(TProp.pctR):
            pctLoadLoss := (Winding^[1].Rpu + Winding^[2].Rpu) * 100.0;
        ord(TProp.kVAs):
        begin
            NormMaxHkVA := 1.1 * Winding^[1].kVA;    // Defaults for new winding rating.
            EmergMaxHkVA := 1.5 * Winding^[1].kVA;
        end;
        15..17:
            Include(Flags, Flg.NeedsRecalc);
        24:
        begin    // Assume load loss is split evenly  between windings 1 and 2
            Winding^[1].Rpu := pctLoadLoss / 2.0 / 100.0;
            Winding^[2].Rpu := Winding^[1].Rpu;
        end;
        33:
            pctLoadLoss := (Winding^[1].Rpu + Winding^[2].Rpu) * 100.0; // Keep this up to date
        34..36:
            Include(Flags, Flg.NeedsRecalc);
        37:
            Winding^[ActiveWinding].RdcSpecified := TRUE;
        38:
            SetLength(kVARatings, NumkVARatings);
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TXfmrCode.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
var
    i: Integer;
begin
    with TObj(ptr) do
    begin
        if Flg.NeedsRecalc in Flags then
        begin
            Exclude(Flags, Flg.NeedsRecalc);
            if NumWindings <= 3 then
                for i := 1 to (NumWindings - 1) * NumWindings div 2 do
                    case i of
                        1:
                            XSC^[1] := XHL;
                        2:
                            XSC^[2] := XHT;
                        3:
                            XSC^[3] := XLT;
                    end;
        end;
        Exclude(Flags, Flg.EditionActive);
    end;
    Result := True;
end;

procedure TXfmrCodeObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    Other := TObj(OtherPtr);
    FNphases := Other.FNphases;
    SetNumWindings(Other.NumWindings);
    for i := 1 to NumWindings do
        Winding^[i] := Other.Winding^[i];

    XHL := Other.XHL;
    XHT := Other.XHT;
    XLT := Other.XLT;
    for i := 1 to (NumWindings * (NumWindings - 1) div 2) do
        XSc^[i] := Other.XSC^[i];
    ThermalTimeConst := Other.ThermalTimeConst;
    n_thermal := Other.n_thermal;
    m_thermal := Other.m_thermal;
    FLrise := Other.FLrise;
    HSrise := Other.HSrise;
    pctLoadLoss := Other.pctLoadLoss;
    pctNoLoadLoss := Other.pctNoLoadLoss;
    NormMaxHkVA := Other.NormMaxHkVA;
    EmergMaxHkVA := Other.EmergMaxHkVA;

    NumkVARatings := Other.NumkVARatings;
    SetLength(kVARatings, NumkVARatings);
    for i := 0 to High(kVARatings) do
        kVARatings[i] := Other.kVARatings[i];
end;

constructor TXfmrCodeObj.Create(ParClass: TDSSClass; const XfmrCodeName: String);
var
    i: Integer;
begin
    inherited Create(ParClass);
    Name := LowerCase(XfmrCodeName);
    DSSObjType := ParClass.DSSClassType;

    // default values and sizes
    FNPhases := 3;
    NumWindings := 2;
    MaxWindings := 2;
    ActiveWinding := 1;
    Winding := Allocmem(Sizeof(TWinding) * MaxWindings);
    for i := 1 to MaxWindings do
        Winding[i].Init();
    XHL := 0.07;
    XHT := 0.35;
    XLT := 0.30;
    XSC := Allocmem(SizeOF(XSC^[1]) * ((NumWindings - 1) * NumWindings div 2));
    VABase := Winding^[1].kVA * 1000.0;
    ThermalTimeconst := 2.0;
    n_thermal := 0.8;
    m_thermal := 0.8;
    FLrise := 65.0;
    HSrise := 15.0;  // Hot spot rise
    NormMaxHkVA := 1.1 * Winding^[1].kVA;
    EmergMaxHkVA := 1.5 * Winding^[1].kVA;
    pctLoadLoss := 2.0 * Winding^[1].Rpu * 100.0; //  assume two windings
    ppm_FloatFactor := 0.000001;
    // Compute antifloat added for each winding
    for i := 1 to NumWindings do
        Winding^[i].ComputeAntiFloatAdder(ppm_FloatFactor, VABase / FNPhases);
    pctNoLoadLoss := 0.0;
    pctImag := 0.0;

    NumkVARatings := 1;
    SetLength(kVARatings, NumkVARatings);
    kVARatings[0] := 600;
end;

destructor TXfmrCodeObj.Destroy;
begin
    Reallocmem(Winding, 0);
    Reallocmem(XSC, 0);
    inherited destroy;
end;

procedure TXfmrCodeObj.PullFromTransformer(obj: TTransfObj);
var
    i: Integer;
begin
    SetNumWindings(obj.NumWindings);
    FNPhases := obj.NPhases;
    XHL := obj.Xhl;
    XHT := obj.Xht;
    XLT := obj.Xlt;
    VABase := obj.VABase;
    NormMaxHKVA := obj.NormMaxHKVA;
    EmergMaxHKVA := obj.EmergMaxHKVA;
    ThermalTimeConst := obj.ThermalTimeConst;
    n_thermal := obj.n_thermal;
    m_thermal := obj.m_thermal;
    FLrise := obj.FLrise;
    HSrise := obj.HSrise;
    pctLoadLoss := obj.pctLoadLoss;
    pctNoLoadLoss := obj.pctNoLoadLoss;
    ppm_FloatFactor := obj.ppm_FloatFactor;
    pctImag := obj.pctImag;
    for i := 1 to (NumWindings - 1) * NumWindings div 2 do
        XSC[i] := obj.XscVal[i];
    for i := 1 to NumWindings do
        Winding[i] := obj.Winding[i];
end;

procedure TXfmrCodeObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);
var
    i: Integer;
begin
    inherited DumpProperties(F, Complete);

    // Basic Property Dump

    FSWriteln(F, '~ NumWindings=' + IntToStr(NumWindings));
    FSWriteln(F, '~ phases=' + IntToStr(Fnphases));

    for i := 1 to NumWindings do
    begin
        with Winding^[i] do
        begin
            if i = 1 then //TODO: check why these two lines are the same
                FSWriteln(F, '~ Wdg=' + IntToStr(i))
            else
                FSWriteln(F, '~ Wdg=' + IntToStr(i));
            case Connection of
                0:
                    FSWriteln(F, '~ conn=wye');
                1:
                    FSWriteln(F, '~ conn=delta');
            end;
            FSWriteln(F, Format('~ kV=%.2f', [kvll]));
            FSWriteln(F, Format('~ kVA=%.1f', [kva]));
            FSWriteln(F, Format('~ tap=%.3f', [putap]));
            FSWriteln(F, Format('~ %R=%.2f', [(Rpu * 100.0)]));
            FSWriteln(F, Format('~ RdcOhms=%.7g', [Rdcohms]));
            FSWriteln(F, Format('~ rneut=%.3f', [rneut]));
            FSWriteln(F, Format('~ xneut=%.3f', [xneut]));
        end;
    end;

    FSWriteln(F, Format('~ XHL=%.3f', [xhl * 100.0]));
    FSWriteln(F, Format('~ XHT=%.3f', [xht * 100.0]));
    FSWriteln(F, Format('~ XLT=%.3f', [xlt * 100.0]));
    FSWriteln(F, Format('~ X12=%.3f', [xhl * 100.0]));
    FSWriteln(F, Format('~ X13=%.3f', [xht * 100.0]));
    FSWriteln(F, Format('~ X23=%.3f', [xlt * 100.0]));
    FSWrite(F, '~ Xscmatrix= "');
    for i := 1 to (NumWindings - 1) * NumWindings div 2 do
        FSWrite(F, Format('%.2f ', [Xsc^[i] * 100.0]));
    FSWriteln(F, '"');
    FSWriteln(F, Format('~ NormMAxHkVA=%.0f', [NormMAxHkVA]));
    FSWriteln(F, Format('~ EmergMAxHkVA=%.0f', [EmergMAxHkVA]));
    FSWriteln(F, Format('~ thermal=%.1f', [thermalTimeConst]));
    FSWriteln(F, Format('~ n=%.1f', [n_thermal]));
    FSWriteln(F, Format('~ m=%.1f', [m_thermal]));
    FSWriteln(F, Format('~ flrise=%.0f', [flrise]));
    FSWriteln(F, Format('~ hsrise=%.0f', [hsrise]));
    FSWriteln(F, Format('~ %loadloss=%.0f', [pctLoadLoss]));
    FSWriteln(F, Format('~ %noloadloss=%.0f', [pctNoLoadLoss]));

    for i := 28 to NumPropsThisClass do
        FSWriteln(F, '~ ' + ParentClass.PropertyName^[i] + '=' + PropertyValue[i]);

    with ParentClass do
    begin
        for i := NumPropsthisClass + 1 to NumProperties do
            FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[i]);
    end;
end;

end.