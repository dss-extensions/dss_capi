unit Transformer;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Classes,
    Command,
    DSSClass,
    PDClass,
    Circuit,
    PDElement,
    UComplex, DSSUcomplex,
    UcMatrix,
    Arraydef,
    DSSObject,
    math;

type
{$SCOPEDENUMS ON}
    TTransfProp = (
        INVALID = 0,
        phases=1,
        windings=2,

        // Winding Definition
        wdg=3,
        bus=4,
        conn=5,
        kV=6,
        kVA=7,
        tap=8,
        pctR=9,
        Rneut=10,
        Xneut=11,

        // General Data
        buses=12,
        conns=13,
        kVs=14,
        kVAs=15,
        taps=16,
        XHL=17,
        XHT=18,
        XLT=19,
        Xscarray=20,

        thermal=21,
        n=22,
        m=23,
        flrise=24,
        hsrise=25,
        pctloadloss=26,
        pctnoloadloss=27,
        normhkVA=28,
        emerghkVA=29,
        sub=30,
        MaxTap=31,
        MinTap=32,
        NumTaps=33,
        subname=34,
        pctimag=35,
        ppm_antifloat=36,

        pctRs=37,

        bank=38,

        XfmrCode=39,

        XRConst=40,
        X12=41,
        X13=42,
        X23=43,
        LeadLag=44,
        WdgCurrents=45,
        Core=46,
        RdcOhms=47,

        Seasons=48,
        Ratings=49
    );
{$SCOPEDENUMS OFF}

    TTransf = class(TPDClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;
        function BeginEdit(ptr: Pointer; SetActive: Boolean=True): Pointer; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TWinding = record
        Connection: Integer;
        kVLL,
        VBase,
        kVA,
        puTap,
        Rpu,      // on transformer MVABase  (1st winding)
        Rdcpu,    // for GIC solutions; default to 85% of Rpu
        RdcOhms,
        Rneut,
        Xneut: Double;
        Y_PPM: Double;  // Anti Float reactance adder
        RdcSpecified: Boolean;

        // Tap Changer Data
        TapIncrement,
        MinTap,
        MaxTap: Double;
        NumTaps: Integer;

        procedure ComputeAntiFloatAdder(PPM_Factor, VABase1ph: Double);
        procedure Init();
    end;

    WindingArray = array[1..3] of TWinding;
    pWindingArray = ^WindingArray;

    TTransfObj = class(TPDElement)
    PUBLIC
        DeltaDirection: Integer;
        XRConst: LongBool;

        function Get_PresentTap(i: Integer): Double;
        procedure Set_PresentTap(i: Integer; const Value: Double);
        function Get_MinTap(i: Integer): Double;
        function Get_MaxTap(i: Integer): Double;
        function Get_TapIncrement(i: Integer): Double;
        function Get_BaseVoltage(i: Integer): Double;
        function Get_BasekVLL(i: Integer): Double;
        // CIM accessors
        function Get_NumTaps(i: Integer): Integer;
        function Get_WdgResistance(i: Integer): Double;
        function Get_WdgConnection(i: Integer): Integer;
        function Get_WdgkVA(i: Integer): Double;
        function Get_Xsc(i: Integer): Double;
        function Get_WdgRneutral(i: Integer): Double;
        function Get_WdgXneutral(i: Integer): Double;

        procedure CalcY_Terminal(FreqMult: Double);
        procedure GICBuildYTerminal;

        procedure BuildYPrimComponent(YPrim_Component, Y_Terminal: TCMatrix);
        procedure AddNeutralToY(FreqMultiplier: Double);

        procedure FetchXfmrCode();


    PROTECTED
        MaxWindings: Integer;
        TermRef: pIntegerArray;  // keeps track of terminal connections

        Zbase: Double;
        XSC: pDoubleArray;     // per unit SC measurements

        ZB: TCMatrix;
        Y_1Volt: TCMatrix;
        Y_Term: TCMatrix;
        Y_1Volt_NL: TCMatrix;   // No Load Y's
        Y_Term_NL: TCMatrix;

        Y_Terminal_Freqmult: Double;

        HVLeadsLV: LongBool;

        XHLChanged: Boolean;
        kVARatings: Array Of Double;

        procedure SetTermRef;
    PUBLIC
        NumWindings: Integer;
        ActiveWinding: Integer;  // public for COM interface

        IsSubstation: LongBool;
        SubstationName: String;
        Winding: pWindingArray;
        XfmrBank: String;
        XfmrCodeObj: TDSSObject;
        CoreType: Integer; // 0=Shell; 1=1ph; 3-3leg; 5=5-leg

        n_thermal: Double;
        m_thermal: Double; // Exponents
        XHL, XHT, XLT: Double;  // per unit
        NormMaxHkVA: Double;
        EmergMaxHkVA: Double;
        ThermalTimeConst: Double; // hr
        FLrise: Double;
        HSrise: Double;
        pctLoadLoss: Double;
        pctNoLoadLoss: Double;
        ppm_FloatFactor: Double; //  parts per million winding float factor
        pctImag: Double;
        VABase: Double;    // FOR impedances

        constructor Create(ParClass: TDSSClass; const TransfName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure SetNumWindings(N: Integer);

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        // GetLosses override for Transformer
        procedure GetLosses(var TotalLosses, LoadLosses, NoLoadLosses: Complex); OVERRIDE;

        function RotatePhases(iPhs: Integer): Integer;
        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;
        procedure SaveWrite(F: TFileStream); OVERRIDE;
        procedure GetWindingVoltages(iWind: Integer; VBuffer: pComplexArray);
        procedure GetAllWindingCurrents(CurrBuffer: pComplexArray);  // All Winding currents in complex array

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        // TODO: remove most of these
        property PresentTap[i: Integer]: Double READ Get_PresentTap WRITE Set_PresentTap;
        property Mintap[i: Integer]: Double READ Get_MinTap;
        property Maxtap[i: Integer]: Double READ Get_MaxTap;
        property TapIncrement[i: Integer]: Double READ Get_TapIncrement;
        property BaseVoltage[i: Integer]: Double READ Get_BaseVoltage;  // Winding VBase
        property BasekVLL[i: Integer]: Double READ Get_BasekVLL;  // Winding VBase
        property NumTaps[i: Integer]: Integer READ Get_NumTaps;
        property WdgResistance[i: Integer]: Double READ Get_WdgResistance;
        property WdgkVA[i: Integer]: Double READ Get_WdgkVA;
        property WdgConnection[i: Integer]: Integer READ Get_WdgConnection;
        property WdgRneutral[i: Integer]: Double READ Get_WdgRneutral;
        property WdgXneutral[i: Integer]: Double READ Get_WdgXneutral;
        property XscVal[i: Integer]: Double READ Get_Xsc;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Utilities,
    XfmrCode,
    Solution,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TTransfObj;
    TProp = TTransfProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TTransf.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, XFMR_ELEMENT, 'Transformer');
end;

destructor TTransf.Destroy;
begin
    inherited Destroy;
end;

function XscSize(obj: TObj): Integer;
begin
    with obj do
        Result := (NumWindings - 1) * NumWindings div 2;
end;

function GetWindingCurrentsResult(Obj: TObj): String;
// Returns string mag, angle
var
    WindingCurrents: pComplexArray;
    i, j, k: Integer;
begin
    with Obj do
    begin
        WindingCurrents := AllocMem(Sizeof(Complex) * 2 * FNPhases * NumWindings);

        GetAllWindingCurrents(WindingCurrents);

        Result := '';
        k := 0;
        for  i := 1 to Fnphases do
        begin
            for j := 1 to NumWindings do
            begin
                k := k + 1;
                Result := Result + Format('%.7g, (%.5g), ', [Cabs(WindingCurrents^[k]), Cdang(WindingCurrents^[k])]);
                k := k + 1;
                // Skip currents from other end of the winding
            end;
        end;

        Reallocmem(WindingCurrents, 0);  // throw away temp array
    end;
end;

procedure TTransf.DefineProperties;
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

    // RO string
    PropertyType[ord(TProp.WdgCurrents)] := TPropertyType.StringSilentROFunctionProperty;
    PropertyOffset[ord(TProp.WdgCurrents)] := ptruint(@GetWindingCurrentsResult);

    // object properties
    PropertyType[ord(TProp.XfmrCode)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.XfmrCode)] := ptruint(@obj.XfmrCodeObj);
    PropertyOffset2[ord(TProp.XfmrCode)] := ptruint(DSS.XfmrCodeClass);

    // double arrays
    PropertyType[ord(TProp.Ratings)] := TPropertyType.DoubleDArrayProperty;
    PropertyOffset[ord(TProp.Ratings)] := ptruint(@obj.kVARatings);
    PropertyOffset2[ord(TProp.Ratings)] := ptruint(@obj.NumAmpRatings);

    PropertyType[ord(TProp.Xscarray)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.Xscarray)] := ptruint(@obj.Xsc);
    PropertyOffset3[ord(TProp.Xscarray)] := ptruint(@XscSize); // (NumWindings - 1) * NumWindings div 2
    PropertyFlags[ord(TProp.Xscarray)] := [TPropertyFlag.SizeIsFunction];
    PropertyScale[ord(TProp.Xscarray)] := 0.01;

    // enums
    PropertyType[ord(TProp.Core)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Core)] := ptruint(@obj.CoreType);
    PropertyOffset2[ord(TProp.Core)] := PtrInt(DSS.CoreTypeEnum);

    PropertyType[ord(TProp.LeadLag)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.LeadLag)] := ptruint(@obj.HVLeadsLV); // LongBool as Integer
    PropertyOffset2[ord(TProp.LeadLag)] := PtrInt(DSS.LeadLagEnum);

    // boolean properties
    PropertyType[ord(TProp.sub)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.XRConst)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.sub)] := ptruint(@obj.IsSubstation);
    PropertyOffset[ord(TProp.XRConst)] := ptruint(@obj.XRConst);

    // string properties
    PropertyType[ord(TProp.bank)] := TPropertyType.StringProperty;
    PropertyType[ord(TProp.subname)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.bank)] := ptruint(@obj.XfmrBank);
    PropertyOffset[ord(TProp.subname)] := ptruint(@obj.SubstationName);

    // integer properties
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    PropertyType[ord(TProp.Seasons)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Seasons)] := ptruint(@obj.NumAmpRatings);

    PropertyType[ord(TProp.windings)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.windings)] := ptruint(@obj.NumWindings);
    PropertyFlags[ord(TProp.windings)] := [TPropertyFlag.GreaterThanOne];

    PropertyType[ord(TProp.wdg)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.wdg)] := ptruint(@obj.ActiveWinding);
    PropertyFlags[ord(TProp.wdg)] := [TPropertyFlag.IntegerStructIndex];

    // double on struct array properties
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

    // double arrays via struct array
    PropertyType[ord(TProp.pctRs)] := TPropertyType.DoubleArrayOnStructArrayProperty;
    PropertyOffset[ord(TProp.pctRs)] := ptruint(@TWinding(nil^).Rpu); 
    PropertyOffset2[ord(TProp.pctRs)] := ptruint(@obj.NumWindings);
    PropertyScale[ord(TProp.pctRs)] := 0.01;
    PropertyFlags[ord(TProp.pctRs)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.pctRs)] := ord(TProp.pctR);

    PropertyType[ord(TProp.kVs)] := TPropertyType.DoubleArrayOnStructArrayProperty;
    PropertyOffset[ord(TProp.kVs)] := ptruint(@TWinding(nil^).kVLL); 
    PropertyOffset2[ord(TProp.kVs)] := ptruint(@obj.NumWindings);
    PropertyFlags[ord(TProp.kVs)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.kVs)] := ord(TProp.kV);

    PropertyType[ord(TProp.kVAs)] := TPropertyType.DoubleArrayOnStructArrayProperty;
    PropertyOffset[ord(TProp.kVAs)] := ptruint(@TWinding(nil^).kVA); 
    PropertyOffset2[ord(TProp.kVAs)] := ptruint(@obj.NumWindings);
    PropertyFlags[ord(TProp.kVAs)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.kVAs)] := ord(TProp.kVA);

    PropertyType[ord(TProp.taps)] := TPropertyType.DoubleArrayOnStructArrayProperty;
    PropertyOffset[ord(TProp.taps)] := ptruint(@TWinding(nil^).puTap); 
    PropertyOffset2[ord(TProp.taps)] := ptruint(@obj.NumWindings);
    PropertyFlags[ord(TProp.taps)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.taps)] := ord(TProp.tap);

    // bus, indirect
    PropertyType[ord(TProp.bus)] := TPropertyType.BusOnStructArrayProperty;
    PropertyOffset[ord(TProp.bus)] := 1; // dummy value, just to mark the property as handled
    
    PropertyType[ord(TProp.buses)] := TPropertyType.BusesOnStructArrayProperty;
    PropertyOffset[ord(TProp.buses)] := ptruint(@obj.NumWindings);
    PropertyFlags[ord(TProp.buses)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.buses)] := ord(TProp.bus);

    // enum on array of structs
    PropertyType[ord(TProp.conn)] := TPropertyType.MappedStringEnumOnStructArrayProperty;
    PropertyOffset[ord(TProp.conn)] := ptruint(@TWinding(nil^).Connection);
    PropertyOffset2[ord(TProp.conn)] := PtrInt(DSS.ConnectionEnum);

    // array of enums on array of structs
    PropertyType[ord(TProp.conns)] := TPropertyType.MappedStringEnumArrayOnStructArrayProperty;
    PropertyOffset[ord(TProp.conns)] := ptruint(@TWinding(nil^).Connection);
    PropertyOffset2[ord(TProp.conns)] := PtrInt(DSS.ConnectionEnum);
    PropertyFlags[ord(TProp.conns)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.conns)] := ord(TProp.conn);

    // integer on struct array
    PropertyType[ord(TProp.NumTaps)] := TPropertyType.IntegerOnStructArrayProperty;
    PropertyOffset[ord(TProp.NumTaps)] := ptruint(@TWinding(nil^).NumTaps);

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

    // scaled double
    PropertyOffset[ord(TProp.ppm_antifloat)] := ptruint(@obj.ppm_FloatFactor);
    PropertyScale[ord(TProp.ppm_antifloat)] := 1.0e-6;
    
    // adv double percent properties
    PropertyOffset[ord(TProp.XHL)] := ptruint(@obj.XHL);
    PropertyOffset[ord(TProp.XHT)] := ptruint(@obj.XHT);
    PropertyOffset[ord(TProp.XLT)] := ptruint(@obj.XLT);
    PropertyOffset[ord(TProp.X12)] := ptruint(@obj.XHL);
    PropertyOffset[ord(TProp.X13)] := ptruint(@obj.XHT);
    PropertyOffset[ord(TProp.X23)] := ptruint(@obj.XLT);
    PropertyScale[ord(TProp.XHL)] := 0.01;
    PropertyScale[ord(TProp.XHT)] := 0.01;
    PropertyScale[ord(TProp.XLT)] := 0.01;
    PropertyScale[ord(TProp.X12)] := 0.01;
    PropertyScale[ord(TProp.X13)] := 0.01;
    PropertyScale[ord(TProp.X23)] := 0.01;
    PropertyTrapZero[ord(TProp.XHL)] := 7.0;
    PropertyTrapZero[ord(TProp.XHT)] := 35.0;
    PropertyTrapZero[ord(TProp.XLT)] := 30.0;
    PropertyTrapZero[ord(TProp.X12)] := 7.0;
    PropertyTrapZero[ord(TProp.X13)] := 35.0;
    PropertyTrapZero[ord(TProp.X23)] := 30.0;
    PropertyFlags[ord(TProp.X12)] := [TPropertyFlag.Redundant];
    PropertyFlags[ord(TProp.X13)] := [TPropertyFlag.Redundant];
    PropertyFlags[ord(TProp.X23)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.X12)] := ord(TProp.XHL);
    PropertyRedundantWith[ord(TProp.X13)] := ord(TProp.XHT);
    PropertyRedundantWith[ord(TProp.X23)] := ord(TProp.XLT);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TTransf.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TTransfObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    i: Integer;
    OldXSCSize, NewXSCSize: Integer;
begin
    case Idx of
        ord(TProp.phases):
        if FNPhases <> previousIntVal then
            NConds := Fnphases + 1;  // Force redefinition of number of conductors and reallocation of matrices
            // default all winding kVAs to first winding so latter Donot have to be specified
        ord(TProp.conn):
        begin
            Yorder := fNConds * fNTerms;
            YPrimInvalid := TRUE;
        end;
        ord(TProp.windings):
        begin
            OldXSCSize := (previousIntVal - 1) * previousIntVal div 2;
            MaxWindings := NumWindings;
            NewXSCSize := (NumWindings - 1) * NumWindings div 2;
            FNconds := Fnphases + 1;
            Nterms := NumWindings;
            Reallocmem(Winding, Sizeof(TWinding) * MaxWindings);  // Reallocate collector array
            for i := 1 to MaxWindings do
                Winding[i].Init();

            // array of short circuit measurements between pairs of windings
            ReAllocmem(XSC, SizeOF(XSC^[1]) * NewXSCSize);
            for i := OldXSCSize + 1 to NewXSCSize do
                XSC^[i] := 0.30;
            Reallocmem(TermRef, SizeOf(TermRef^[1]) * 2 * NumWindings * Fnphases);

            // Reallocate impedance matrices
            if ZB <> NIL then
            begin
                ZB.Free;
                Y_1Volt.Free;
                Y_1Volt_NL.Free;
                Y_Term.Free;
                Y_Term_NL.Free;
            end;

            ZB := TCMatrix.CreateMatrix(NumWindings - 1);
            Y_1Volt := TCMatrix.CreateMatrix(NumWindings);
            Y_1Volt_NL := TCMatrix.CreateMatrix(NumWindings);
            Y_Term := TCMatrix.CreateMatrix(2 * NumWindings);
            Y_Term_NL := TCMatrix.CreateMatrix(2 * NumWindings);
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
        ord(TProp.XHL), ord(TProp.XHT), ord(TProp.XLT),
        ord(TProp.X12), ord(TProp.X13), ord(TProp.X23):
            XHLChanged := TRUE;

        ord(TProp.pctloadloss):
        begin    // Assume load loss is split evenly  between windings 1 and 2
            Winding^[1].Rpu := pctLoadLoss / 2.0 / 100.0;
            Winding^[2].Rpu := Winding^[1].Rpu;
        end;
        ord(TProp.pctRs):
            pctLoadLoss := (Winding^[1].Rpu + Winding^[2].Rpu) * 100.0;  // Update
        ord(TProp.XfmrCode):
            FetchXfmrCode();
        ord(TProp.RdcOhms):
            Winding^[ActiveWinding].RdcSpecified := TRUE;
        ord(TProp.Seasons):
            SetLength(kVARatings, NumAmpRatings);
    end;

    //YPrim invalidation on anything that changes impedance values
    case Idx of
        ord(TProp.tap), ord(TProp.taps):
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
            // Try to handle tap changes incrementally
            if ((ActiveCircuit.Solution.SolverOptions and $FFFFFFFF) <> ord(TSolverOptions.ReuseNothing)) and 
                (not ActiveCircuit.Solution.SystemYChanged) and 
                (YPrim <> NIL) and 
                (not YPrimInvalid)
            then
                // Mark this to incrementally update the matrix.
                // If the matrix is already being rebuilt, there is 
                // no point in doing this, just rebuild it as usual.
                ActiveCircuit.IncrCktElements.Add(self) 
            else
{$ENDIF}
            YprimInvalid := TRUE;
        ord(TProp.kV), ord(TProp.kVA),
        9..15,
        ord(TProp.pctloadloss), ord(TProp.pctnoloadloss),
        ord(TProp.pctimag), ord(TProp.ppm_antifloat), ord(TProp.pctRs), 
        ord(TProp.XHL), ord(TProp.XHT), ord(TProp.XLT),
        ord(TProp.X12), ord(TProp.X13), ord(TProp.X23):
            YprimInvalid := TRUE;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TTransf.BeginEdit(ptr: Pointer; SetActive: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj(inherited BeginEdit(ptr, SetActive));
    Obj.XHLChanged := FALSE;
    Result := Obj;
end;

procedure TTransfObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);

    Other := TObj(OtherPtr);
    FNphases := Other.Fnphases;
    SetNumWindings(Other.NumWindings);
    NConds := Fnphases + 1; // forces reallocation of terminals and conductors

    Yorder := fNConds * fNTerms;
    YPrimInvalid := TRUE;

    for i := 1 to NumWindings do
        Winding[i] := Other.Winding[i];

    SetTermRef;

    XHL := Other.XHL;
    XHT := Other.XHT;
    XLT := Other.XLT;

    for i := 1 to (NumWindings * (NumWindings - 1) div 2) do
        XSc^[i] := Other.XSC^[i];

    ZB.CopyFrom(Other.ZB);
    Y_1Volt.CopyFrom(Other.Y_1Volt);
    Y_Term.CopyFrom(Other.Y_Term);
    Y_1Volt_NL.CopyFrom(Other.Y_1Volt_NL);
    Y_Term_NL.CopyFrom(Other.Y_Term_NL);

    ThermalTimeConst := Other.ThermalTimeConst;
    n_thermal := Other.n_thermal;
    m_thermal := Other.m_thermal;
    FLrise := Other.FLrise;
    HSrise := Other.HSrise;
    pctLoadLoss := Other.pctLoadLoss;
    pctNoLoadLoss := Other.pctNoLoadLoss;
    NormMaxHkVA := Other.NormMaxHkVA;
    EmergMaxHkVA := Other.EmergMaxHkVA;
    XRConst := Other.XRConst;

    XfmrBank := Other.XfmrBank;
    XfmrCodeObj := Other.XfmrCodeObj;

    NumAmpratings := Other.NumAmpRatings;
    Setlength(kVARatings, NumAmpRatings);
    for i := 0 to High(kVARatings) do
        kVARatings[i] := Other.kVARatings[i];
end;

constructor TTransfObj.Create(ParClass: TDSSClass; const TransfName: String);
var
    i: Integer;
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(TransfName);
    DSSObjType := ParClass.DSSClassType; //DSSObjType + XFMR; // override PDElement   (kept in both actually)

    FNphases := 3;  // Directly set conds and phases
    fNConds := Fnphases + 1;

    ZB := NIL;
    Y_1Volt := NIL;
    Y_1Volt_NL := NIL;;
    Y_Term := NIL;
    Y_Term_NL := NIL;
    SetNumWindings(2);  // must do this after setting number of phases
    ActiveWinding := 1;

    Nterms := NumWindings;  // Force allocation of terminals and conductors

    XHL := 0.07;
    XHT := 0.35;
    XLT := 0.30;
    XHLChanged := TRUE;  // Set flag to for calc of XSC array from XHL, etc.

    DeltaDirection := 1;
    SubstationName := '';
    XfmrBank := '';
    XfmrCodeObj := NIL;

    CoreType := 0;

    VABase := Winding^[1].kVA * 1000.0;
    ThermalTimeconst := 2.0;
    n_thermal := 0.8;
    m_thermal := 0.8;
    FLrise := 65.0;
    HSrise := 15.0;  // Hot spot rise
    NormMaxHkVA := 1.1 * Winding^[1].kVA;
    EmergMaxHkVA := 1.5 * Winding^[1].kVA;
    pctLoadLoss := 2.0 * Winding^[1].Rpu * 100.0; //  assume two windings for init'ing
    ppm_FloatFactor := 0.000001;
    // Compute antifloat added for each winding
    for i := 1 to NumWindings do
        Winding^[i].ComputeAntiFloatAdder(ppm_FloatFactor, VABase / FNPhases);

    // Default the no load properties to zero
    pctNoLoadLoss := 0.0;
    pctImag := 0.0;

    FaultRate := 0.007;
    IsSubstation := FALSE;
    XRConst := FALSE;

    HVLeadsLV := FALSE; // Defaults to ANSI connection

    Y_Terminal_FreqMult := 0.0;

    Yorder := fNTerms * fNconds;
    
    NumAmpRatings := 1;
    SetLength(kVARatings, NumAmpRatings);
    kVARatings[0] := NormMaxHkVA;
    
    RecalcElementData;
end;


procedure TTransfObj.SetNumWindings(N: Integer);
var
    prev: Integer;
begin
    prev := NumWindings;
    NumWindings := N;
    PropertySideEffects(ord(TProp.windings), prev);
end;

destructor TTransfObj.Destroy;
begin
    {Throw away stuff allocated for this object}
    Reallocmem(Winding, 0);
    Reallocmem(XSC, 0);
    Reallocmem(TermRef, 0);
    ZB.Free;
    Y_1Volt.Free;
    Y_1Volt_NL.Free;
    Y_Term.Free;
    Y_Term_NL.Free;
    inherited Destroy;
end;

procedure TTransfObj.RecalcElementData;
var
    i,
    ihvolt: Integer;
    VFactor: Double;
begin
    // Determine Delta Direction
    // If high voltage is delta, delta leads y
    // If high voltage is wye, delta lags wye
    if Winding^[1].connection = Winding^[2].connection then
        DeltaDirection := 1
    else
    begin
        if Winding^[1].kVLL >= Winding^[2].kVLL then
            iHvolt := 1
        else
            iHVolt := 2;
        case Winding^[iHvolt].Connection of
            0:
                if HVLeadsLV then
                    DeltaDirection := -1
                else
                    DeltaDirection := 1;
            1:
                if HVLeadsLV then
                    DeltaDirection := 1
                else
                    DeltaDirection := -1;
        else
         // ---old code --- If Winding^[2].Connection = 0 Then DeltaDirection := -1 Else DeltaDirection := 1;
        end;
    end;

    SetTermRef;   // Re-establish TermRef IF num windings or connection changed

    for i := 1 to NumWindings do
    begin
        with Winding^[i] do
            if (NumTaps > 0) then
                TapIncrement := (MaxTap - MinTap) / NumTaps
            else
                TapIncrement := 0.0;
    end;

    if XHLChanged then
    begin
        // should only happen for 2- and 3-winding transformers
        if NumWindings <= 3 then
            for i := 1 to (NumWindings * (NumWindings - 1) div 2) do
                case i of
                    1:
                        XSC^[1] := XHL;
                    2:
                        XSC^[2] := XHT;
                    3:
                        XSC^[3] := XLT;
                else
                end;
        XHLChanged := FALSE;
    end;

    // Set winding voltage bases (in volts)
    for i := 1 to NumWindings do
        with Winding^[i] do  // Get the actual turns voltage base for each winding
            case Connection of
                0:
                    case Fnphases of   // Wye
                        2, 3:
                            VBase := kVLL * InvSQRT3x1000;   // assume 3-phase for 2-phase designation
                    else
                        VBase := kVLL * 1000.0;
                    end;
                1:
                    VBase := kVLL * 1000.0;     // delta

            end;

    // Base rating of Winding 1
    VABase := Winding^[1].kVA * 1000.0;

    // Set Rdc parameters for each winding.
    for i := 1 to NumWindings do
        with Winding^[i] do
        begin
            if RdcSpecified then
                Rdcpu := RdcOhms / (SQR(VBase) / VABase)
            else
            begin
                Rdcpu := abs(0.85 * Rpu); // use abs in case this resistance comes out negative.
                RdcOhms := Rdcpu * SQR(VBase) / VABase;
            end;
        end;


    for i := 1 to NumWindings do
        Winding^[i].ComputeAntiFloatAdder(ppm_FloatFactor, VABase / FNPhases);

    // Normal and Emergency terminal current Rating for UE check
    Vfactor := 1.0;  // ensure initialization
    case Winding^[1].connection of
        0:
            VFactor := Winding^[1].VBase * 0.001;   // wye
        1:
            case Fnphases of
                1:
                    VFactor := Winding^[1].VBase * 0.001;
                2, 3:
                    VFactor := Winding^[1].VBase * 0.001 / SQRT3;
            else
                VFactor := Winding^[1].VBase * 0.001 * 0.5 / sin(pi / Fnphases);
            end;
    end;

    // Divide per phase kVA by voltage to neutral
    NormAmps := NormMaxHkVA / Fnphases / Vfactor;
    EmergAmps := EmergMaxHkVA / Fnphases / Vfactor;

    SetLength(AmpRatings, NumAmpRatings);
    for i := 0 to High(AmpRatings) do
        AmpRatings[i] := 1.1 * kVARatings[i] / Fnphases / Vfactor;

    CalcY_Terminal(1.0);   // Calc Y_Terminal at base frequency
end;

procedure TTransfObj.SaveWrite(F: TFileStream);
// Override standard SaveWrite
// Transformer structure not conducive to standard means of saving
var
    iprop: Integer;
    i: Integer;
begin
    // Write only properties that were explicitly set in the
    // final order they were actually set
    iProp := GetNextPropertySet(-9999999);
    while iProp > 0 do
    begin
        // Trap wdg= and write out array properties instead
        case iProp of
            3: //TODO: automate this
            begin   // if WDG= was ever used write out arrays ...
                for i := 12 to 16 do
                    FSWrite(F, Format(' %s=%s', [ParentClass.PropertyName^[i], GetPropertyValue(i)]));
                for i := 1 to Numwindings do
                    with Winding^[i] do
                        FSWrite(F, Format(' wdg=%d %sR=%.7g RdcOhms=%.7g', [i, '%', Rpu * 100.0, RdcOhms]));
            end;
            4..9: //TODO: mark to avoid in "savewrite"
                ; // Ignore these properties; use arrays instead
        else
            if Length(PropertyValue[iProp]) > 0 then
                FSWrite(F, Format(' %s=%s', [ParentClass.PropertyName[iProp], CheckForBlanks(PropertyValue[iProp])]));
        end;
        iProp := GetNextPropertySet(iProp);
    end;
end;

procedure TTransfObj.SetTermRef;
// sets an array which maps the two conductors of each winding to the
// phase and neutral conductors of the transformer according to the winding connection
var
    i, j, k: Integer;

begin
    k := 0;

    case Fnphases of
        1:
            for j := 1 to NumWindings do
            begin
                Inc(k);
                TermRef^[k] := (j - 1) * fNconds + 1;
                Inc(k);
                TermRef^[k] := j * fNconds;
            end;
    else
        for i := 1 to Fnphases do
        begin
            for  j := 1 to NumWindings do
            begin
                Inc(k);
                case Winding^[j].Connection of
                    0:
                    begin      // Wye
                        TermRef^[k] := (j - 1) * fNconds + i;
                        Inc(k);
                        TermRef^[k] := j * fNconds;
                    end;
                    // **** WILL THIS WORK for 2-PHASE OPEN DELTA ???? Need to check this sometime
                    1:
                    begin   // Delta
                        TermRef^[k] := (j - 1) * fNconds + i;
                        Inc(k);
                        TermRef^[k] := (j - 1) * fNconds + RotatePhases(i);  // connect to next phase in sequence
                    end;
                end;
            end;
        end;
    end;
end;

procedure TTransfObj.CalcYPrim;
var
    FreqMultiplier: Double;
begin
    if (Yprim = NIL) OR (Yprim.order <> Yorder) OR (Yprim_Shunt = NIL) OR (Yprim_Series = NIL) {YPrimInvalid} then
    begin
         // Reallocate YPrim if something has invalidated old allocation
        if YPrim_Series <> NIL then
            YPrim_Series.Free;
        if YPrim_Shunt <> NIL then
            YPrim_Shunt.Free;
        if YPrim <> NIL then
            YPrim.Free;

        YPrim_Series := TcMatrix.CreateMatrix(Yorder);
        YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin  // Same size as last time; just zero out to start over
        YPrim_Series.Clear; // zero out YPrim
        YPrim_Shunt.Clear; // zero out YPrim
        Yprim.Clear;
    end;

    // Set frequency multipliers for this calculation
    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;
    // Check for rebuilding Y_Terminal; Only rebuild if freq is different than last time
    if FreqMultiplier <> Y_Terminal_Freqmult then
        CalcY_Terminal(FreqMultiplier);

    BuildYPrimComponent(YPrim_Series, Y_Term);
    BuildYPrimComponent(YPrim_Shunt, Y_Term_NL);

    AddNeutralToY(FreqMultiplier);

    // Combine the two Yprim components into Yprim
    YPrim.CopyFrom(YPrim_Series);
    Yprim.AddFrom(Yprim_Shunt);

    // Now Account for Open Conductors
    // For any conductor that is open, zero out row and column
    inherited CalcYPrim;

    YprimInvalid := FALSE;
end;

procedure TTransfObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);
var
    i, j: Integer;
    ZBtemp: Tcmatrix;
begin
    inherited DumpProperties(F, Complete);

    // Basic Property Dump

    FSWriteln(F, Format('~ NumWindings=%d', [NumWindings]));
    FSWriteln(F, Format('~ phases=%d', [Fnphases]));

    for i := 1 to NumWindings do
    begin
        with Winding^[i] do
        begin
            if i = 1 then
                FSWriteln(F, Format('~ Wdg=%d bus=%s', [i, firstbus]))
            else
                FSWriteln(F, Format('~ Wdg=%s bus=%s', [i, nextbus]));
            case Connection of
                0:
                    FSWriteln(F, '~ conn=wye');
                1:
                    FSWriteln(F, '~ conn=delta');
            end;
            FSWriteln(F, Format('~ kv=%.2f', [kVLL]));
            FSWriteln(F, Format('~ kVA=%.1f', [kVA]));
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

    if Complete then
    begin
        FSWriteln(F);
        ZBTemp := TCmatrix.CreateMatrix(NumWindings - 1);
        ZBTemp.CopyFrom(ZB);
        ZBTemp.Invert;

        FSWriteln(F, 'ZB:');
        with ZBTemp do
        begin
            for i := 1 to NumWindings - 1 do
            begin
                for j := 1 to i do
                    FSWrite(F, format('%g ', [GetElement(i, j).re]));
                FSWriteln(F);
            end;
            for i := 1 to NumWindings - 1 do
            begin
                for j := 1 to i do
                    FSWrite(F, format('%g ', [GetElement(i, j).im]));
                FSWriteln(F);
            end;
        end;

        ZBTemp.Free;

        FSWriteln(F);
        FSWriteln(F, 'ZB: (inverted)');
        with ZB do
        begin
            for i := 1 to NumWindings - 1 do
            begin
                for j := 1 to i do
                    FSWrite(F, Format('%.4f ', [GetElement(i, j).re]));
                FSWriteln(F);
            end;
            for i := 1 to NumWindings - 1 do
            begin
                for j := 1 to i do
                    FSWrite(F, Format('%.4f ', [GetElement(i, j).im]));
                FSWriteln(F);
            end;
        end;

        FSWriteln(F);
        FSWriteln(F, 'Y_OneVolt');
        with Y_1Volt do
        begin
            for i := 1 to NumWindings do
            begin
                for j := 1 to i do
                    FSWrite(F, Format('%.4f ', [GetElement(i, j).re]));
                FSWriteln(F);
            end;
            for i := 1 to NumWindings do
            begin
                for j := 1 to i do
                    FSWrite(F, Format('%.4f ', [GetElement(i, j).im]));
                FSWriteln(F);
            end;
        end;

        FSWriteln(F);
        FSWriteln(F, 'Y_Terminal');
        with Y_Term do
        begin
            for i := 1 to 2 * NumWindings do
            begin
                for j := 1 to i do
                    FSWrite(F, Format('%.4f ', [GetElement(i, j).re]));
                FSWriteln(F);
            end;
            for i := 1 to 2 * NumWindings do
            begin
                for j := 1 to i do
                    FSWrite(F, Format('%.4f ', [GetElement(i, j).im]));
                FSWriteln(F);
            end;
        end;
        FSWriteln(F);
        FSWrite(F, 'TermRef= ');
        for i := 1 to 2 * NumWindings * Fnphases do
            FSWrite(F, IntToStr(TermRef^[i]) + ' ');
        FSWriteln(F);
    end;
end;

procedure TWinding.ComputeAntiFloatAdder(PPM_Factor, VABase1ph: Double);
begin
    Y_PPM := -PPM_Factor / (SQR(VBase) / VABase1ph) / 2.0;  //12-11-12 divided by two
    // put half on each terminal of the winding.
end;

procedure TWinding.Init();
// Make a new winding
begin
    Connection := 0;
    kVLL := 12.47;
    VBase := kVLL / SQRT3 * 1000.0;
    kVA := 1000.0;
    puTap := 1.0;
    Rpu := 0.002;
    Rdcpu := Rpu * 0.85;  // default value
    RdcOhms := Sqr(kVLL) / (kVA / 1000.0) * Rdcpu;
    RdcSpecified := FALSE;
    Rneut := -1.0;    // default to open - make user specify connection
    Xneut := 0.0;
    ComputeAntiFloatAdder(1.0e-6, kVA / 3.0 / 1000.0);     //  1 PPM

    TapIncrement := 0.00625;
    NumTaps := 32;
    MaxTap := 1.10;
    MinTap := 0.90;
end;

function TTransfObj.Get_PresentTap(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].puTap
    else
        Result := 0.0;
end;

procedure TTransfObj.Set_PresentTap(i: Integer; const Value: Double);
var
    TempVal: Double;
begin
    if (i > 0) and (i <= NumWindings) then
        with Winding^[i] do
        begin
            // Range Checking
            TempVal := Value;
            if (TempVal < MinTap) then
                TempVal := MinTap
            else
            if (TempVal > MaxTap) then
                TempVal := MaxTap;

            if TempVal <> puTap then
            begin  // Only if there's been a change
                puTap := TempVal;
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
                if ((ActiveCircuit.Solution.SolverOptions and $FFFFFFFF) <> ord(TSolverOptions.ReuseNothing)) and 
                   (not ActiveCircuit.Solution.SystemYChanged) and 
                   (YPrim <> NIL) and 
                   (not YPrimInvalid)
                then
                    // Mark this to incrementally update the matrix.
                    // If the matrix is already being rebuilt, there is 
                    // no point in doing this, just rebuild it as usual.
                    ActiveCircuit.IncrCktElements.Add(Self) 
                else
{$ENDIF}
                    YPrimInvalid := TRUE;  // this property triggers setting SystemYChanged=true

                RecalcElementData;
            end;
        end;
end;

function TTransfObj.Get_WdgResistance(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].Rpu
    else
        Result := 0.0;
end;

function TTransfObj.Get_WdgkVA(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].kVA
    else
        Result := 0.0;
end;

function TTransfObj.Get_WdgRneutral(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].Rneut
    else
        Result := 0.0;
end;

function TTransfObj.Get_WdgXneutral(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].Xneut
    else
        Result := 0.0;
end;

function TTransfObj.Get_Xsc(i: Integer): Double;
var
    imax: Integer;
begin
    imax := (NumWindings - 1) * NumWindings div 2;
    if (i > 0) and (i <= imax) then
        Result := XSC^[i]
    else
        Result := 0.0;
end;


function TTransfObj.Get_WdgConnection(i: Integer): Integer;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].Connection
    else
        Result := 0;
end;

function TTransfObj.Get_MinTap(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].MinTap
    else
        Result := 0.0;
end;

function TTransfObj.Get_MaxTap(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].MaxTap
    else
        Result := 0.0;
end;

function TTransfObj.Get_NumTaps(i: Integer): Integer;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].NumTaps
    else
        Result := 0;
end;

function TTransfObj.Get_TapIncrement(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].TapIncrement
    else
        Result := 0.0;
end;

procedure TTransfObj.GetAllWindingCurrents(CurrBuffer: pComplexArray);
//  Return a vector of complex currents for each Winding of all phases
//
//  Iterm = Yterm * Vterm
//
//  Yterm order is 2*NumWindings.  Each phase has same Yterm.
//  Vterm order is 2*NumWindings .
//
//  Calculate Iterm phase-by-phase and concatenate into CurrBuffer.
var
    i, jphase, k, iPhase, iWind, NeutTerm: Integer;
    VTerm: pComplexArray;
    ITerm: pComplexArray;
    ITerm_NL: pComplexArray;

begin
    if (not Enabled) or (NodeRef = NIL) or (ActiveCircuit.Solution.NodeV = NIL) then
        Exit;

    try
        Vterm := Allocmem(SizeOf(Complex) * 2 * NumWindings);
        Iterm := Allocmem(SizeOf(Complex) * 2 * NumWindings);
        ITerm_NL := Allocmem(SizeOf(Complex) * 2 * NumWindings);

        // Load up Vterminal - already allocated for all cktelements
        with ActiveCircuit.Solution do
            if Assigned(NodeV) then
                for i := 1 to Yorder do
                    Vterminal^[i] := NodeV^[NodeRef^[i]]
            else
                for i := 1 to Yorder do
                    Vterminal^[i] := CZERO;


        k := 0;
        for iPhase := 1 to Fnphases do
        begin
            for iWind := 1 to NumWindings do
            begin
                NeutTerm := iWind * FNConds;
                i := 2 * iWind - 1;

                case Winding^[iWind].Connection of
                    0:
                    begin   // Wye
                        VTerm^[i] := Vterminal^[iphase + (iWind - 1) * FNconds];
                        VTerm^[i + 1] := Vterminal^[NeutTerm];
                    end;
                    1:
                    begin   // Delta
                        jphase := RotatePhases(iphase);      // Get next phase in sequence
                        VTerm^[i] := Vterminal^[iphase + (iWind - 1) * FNconds];
                        VTerm^[i + 1] := Vterminal^[jphase + (iWind - 1) * FNconds];
                    end
                end; {CASE}

            end;
            Y_Term.MVmult(ITerm, VTerm);  // ITerm = Y_Term Vterm
            Y_Term_NL.MVmult(ITerm_NL, Vterm);// no load part
        // Add into Currbuffer
            for i := 1 to 2 * NumWindings do
            begin
                k := k + 1;
                CurrBuffer^[k] := ITerm^[i] + ITerm_NL^[i];
            end;
        end;

        ReallocMem(Vterm, 0);
        ReallocMem(Iterm, 0);
        ReallocMem(Iterm_NL, 0);


    except
        On E: Exception do
            DoSimpleMsg(Format(_('Error filling voltage buffer in GetAllWindingCurrents for Circuit Element: %s'), [FullName]) + CRLF +
                _('Probable Cause: Invalid definition of element.') + CRLF +
                _('System Error Message: ') + E.Message, 100114);
    end;
end;

procedure TTransfObj.GetWindingVoltages(iWind: Integer; VBuffer: pComplexArray);
//  Voltages across indicated winding
// Fill Vbuffer array which must be adequately allocated by calling routine
// Order is Number of Phases
var
    i, ii, k, NeutTerm: Integer;
begin
    try

     {return Zero if winding number improperly specified}
        if (iWind < 1) or (iWind > NumWindings) then
        begin
            for i := 1 to FNconds do
                VBuffer^[i] := CZERO;
            Exit;
        end;

     {Load up VTerminal - already allocated for all cktelements}
        with ActiveCircuit.Solution do
            for i := 1 to Yorder do
                Vterminal^[i] := NodeV^[NodeRef^[i]];


        k := (iWind - 1) * FNconds;    // Offset for winding
        NeutTerm := Fnphases + k + 1;
        for i := 1 to Fnphases do
            case Winding^[iWind].Connection of
                0:
                begin      // Wye
                    VBuffer^[i] := Vterminal^[i + k] - Vterminal^[NeutTerm];
                end;
                1:
                begin   // Delta
                    ii := RotatePhases(i);      // Get next phase in sequence
                    VBuffer^[i] := Vterminal^[i + k] - Vterminal^[ii + k];
                end
            end; {CASE}

    except
        On E: Exception do
            DoSimpleMsg(
                _('Error filling voltage buffer in GetWindingVoltages for Circuit Element: %s') + CRLF +
                _('Probable Cause: Invalid definition of element.') + CRLF +
                _('System Error Message: %s'),
                [FullName, E.Message], 114);
    end;
end;


function TTransfObj.Get_BaseVoltage(i: Integer): Double;
begin
    if (i < 1) or (i > NumWindings) then
        Result := Winding^[1].VBase
    else
        Result := Winding^[i].VBase;
end;

procedure TTransfObj.GetLosses(var TotalLosses, LoadLosses, NoLoadLosses: Complex);
var
    cTempIterminal: pComplexArray;
    i: Integer;
begin
    if not FEnabled then
    begin
        TotalLosses := CZERO;
        LoadLosses := CZERO;
        NoLoadLosses := CZERO;
        Exit;
    end;

    // Calculates losses in watts, vars
    TotalLosses := Losses;   // Side effect: computes Iterminal

    // Compute No load losses in Yprim_Shunt
    cTempIterminal := AllocMem(Sizeof(Complex) * Yorder);
    ComputeVterminal;
    Yprim_Shunt.MVmult(cTempIterminal, Vterminal);
    // No Load Losses are sum of all powers coming into YPrim_Shunt from each terminal
    NoLoadLosses := CZERO;
    for i := 1 to Yorder do
        NoLoadLosses += VTerminal^[i] * cong(cTempIterminal^[i]);

    LoadLosses := TotalLosses - NoLoadLosses;

    Reallocmem(cTempIterminal, 0);
end;

function TTransfObj.RotatePhases(iPhs: Integer): Integer;
// For Delta connections or Line-Line voltages
begin
    Result := iPhs + DeltaDirection;

     // make sure result is within limits
    if FnPhases > 2 then
    begin
         // Assumes 2 phase delta is open delta
        if Result > Fnphases then
            Result := 1;
        if Result < 1 then
            Result := Fnphases;
    end
    else
    if Result < 1 then
        Result := 3;    // For 2-phase delta, next phase will be 3rd phase

end;

procedure TTransfObj.MakePosSequence();
// Converts default 3-phase transformer model into equivalent positive-sequence
var
    iW, i, N: Integer;
    Nodes: array[1..50] of Integer; // big integer buffer
    OnPhase1: Boolean;
    new_norm, new_emerg: Double;
    new_conns: Array of Integer;
    new_buses: Array of String;
    new_kVs, new_kVAs: Array of Double;
begin
    // First, determine if we can convert this one.
    if (FnPhases = 1) or (FNphases = 2) then
    begin // disable if any terminal not connected to phase one
        for iW := 1 to NumWindings do
        begin
            OnPhase1 := FALSE;
            DSS.AuxParser.ParseAsBusName(GetBus(iW), N, pIntegerArray(@Nodes));
            if N = 0 then
                OnPhase1 := TRUE;
            for i := 1 to N do
                if Nodes[i] = 1 then
                    OnPhase1 := TRUE;
            if not OnPhase1 then
            begin
                Enabled := FALSE;   // We won't use this one
                Exit;
            end;
        end;
    end;

    // Construct AutoTrans definition string
    SetLength(new_conns, NumWindings);
    SetLength(new_buses, NumWindings);
    SetLength(new_kVs, NumWindings);
    SetLength(new_kVAs, NumWindings);

    for i := 1 to NumWindings do
        new_conns[i - 1] := 0;
    for i := 1 to NumWindings do
        new_buses[i - 1] := Getbus(i);

    for i := 1 to NumWindings do
        with Winding^[i] do
            if (NPhases > 1) or (Connection <> 0) then
                new_kVs[i - 1] := kVLL / SQRT3
            else
                new_kVs[i - 1] := kVLL;

    for i := 1 to NumWindings do
        with Winding^[i] do
            new_kVAs[i - 1] := kVA / FNPhases;
    
    new_norm := NormMaxHkVA / FNPhases;
    new_emerg := EmergMaxHkVA / FNPhases;

    BeginEdit(True);
    SetInteger(ord(TProp.Phases), 1);
    SetIntegers(ord(TProp.Conns), new_conns);
    SetStrings(ord(TProp.Buses), new_buses);
    SetDoubles(ord(TProp.kVs), new_kVs);
    SetDoubles(ord(TProp.kVAs), new_kVAs);
    SetDouble(ord(TProp.NormHkVA), new_norm);
    SetDouble(ord(TProp.EmergHkVA), new_emerg);
    EndEdit(7);

    inherited;
end;

procedure TTransfObj.AddNeutralToY(FreqMultiplier: Double);
var
    i: Integer;
    Value: complex;
    j: Integer;
begin
    // Account for neutral impedances
    with YPrim_Series do
    begin
        for i := 1 to NumWindings do
        begin
            with Winding^[i] do
            begin
                if Connection = 0 then
                begin
                    // handle wye, but ignore delta  (and open wye)
                    if Rneut >= 0 then
                    begin
                        // <0 is flag for open neutral  (Ignore)
                        if (Rneut = 0) and (Xneut = 0) then
                            // Solidly Grounded
                            Value := Cmplx(1000000, 0)
                        else
                            // 1 microohm resistor
                            Value := Cinv(Cmplx(Rneut, XNeut * FreqMultiplier));
                        j := i * fNconds;
                        AddElement(j, j, Value);
                    end

                    else
                    begin
                        // Bump up neutral admittance a bit in case neutral is floating
                        j := i * fNconds;
                        if ppm_FloatFactor <> 0.0 then
                            SetElement(j, j, GetElement(j, j) + Cmplx(0.0, Y_PPM));
                            // SetElement(j, j, CmulReal_im(GetElement(j, j), ppm_FloatFactorPlusOne));
                    end;

                end;
            end;
        end;
    end;
end;

procedure TTransfObj.BuildYPrimComponent(YPrim_Component, Y_Terminal: TCMatrix);
var
    NW2: Integer;
    i: Integer;
    k: Integer;
    Value: complex;
    j: Integer;

begin
    with YPrim_Component do
    begin
        // Now, Put in Yprim matrix
        // have to add every element of Y_terminal into Yprim somewhere
        NW2 := 2 * NumWindings;
        for i := 1 to NW2 do
        begin
            for j := 1 to i do
            begin
                Value := Y_Terminal.GetElement(i, j);
                // This value goes in Yprim nphases times
                for k := 0 to Fnphases - 1 do
                    AddElemSym(TermRef^[i + k * NW2], TermRef^[j + k * NW2], Value);
            end;
        end;
    end;
end;

function TTransfObj.Get_BasekVLL(i: Integer): Double;
begin
    Result := Winding^[i].kVLL;
end;


procedure TTransfObj.GICBuildYTerminal;
// Build YTerminal considering on resistance and no coupling to other winding.
var
    i, j, idx: Integer;
    yR: Complex;
    Yadder: Complex;
begin
    Y_Term.Clear;
    Y_Term_NL.Clear;

    for i := 1 to NumWindings do
    begin
        // Use Rdc to build GIC model
        yR := Cmplx(1.0 / (Winding^[i].Rdcohms), 0.0); // convert to Siemens
        with Y_Term do
        begin
            idx := 2 * i - 1;
            SetElement(idx, idx, yR);
            SetElement(idx + 1, idx + 1, yR);
            SetElemSym(idx, idx + 1, -yR);   // set off-diagonals
        end;
    end;

    // For GIC add a small *Conductance* to both conductors of each winding so that
    // the matrix will always invert even if the user neglects to define a voltage
    // reference on all sides
    if ppm_FloatFactor <> 0.0 then
        with Y_Term do
            for i := 1 to NumWindings do
            begin
                Yadder := cmplx(-Winding^[i].Y_PPM, 0.0);    // G + j0
                for j := (2 * i - 1) to (2 * i) do
                    SetElement(j, j, GetElement(j, j) + Yadder);
                    // SetElement(j, j, CmulReal_im(GetElement(j, j) , ppm_FloatFactorPlusOne));
            end;
end;

procedure TTransfObj.CalcY_Terminal(FreqMult: Double);
var
    i,
    j,
    k: Integer;
    A: pComplexArray;
    ctempArray1,
    ctempArray2: pComplexArray;
    cMinusOne: Complex;
    AT: TcMatrix;
    Yadder: Complex;
    Rmult: Double;
    // Function to fix a specification of a pu tap of 0.0
    // Regcontrol can attempt to force zero tap position in some models
    function ZeroTapFix(const tapvalue: Double): Double;
    begin
        if TapValue = 0.0 then
            Result := 0.0001
        else
            Result := Tapvalue;
    end;

begin
    if ActiveCircuit.Solution.Frequency < 0.51 then
        // Build Yterminal for GIC ~dc simulation
        GICBuildYTerminal
    else
    begin  
        // Normal Y matrix build
        if XRConst then
            RMult := FreqMult
        else
            RMult := 1.0;

        // Construct ZBMatrix;
        ZB.Clear;
        ZBase := 1.0 / (VABase / Fnphases); // base ohms on 1.0 volt basis
        for i := 1 to Numwindings - 1 do
            // convert pu to ohms on one volt base as we go...
            ZB.SetElement(i, i, Cmplx(Rmult * (Winding^[1].Rpu + Winding^[i + 1].Rpu), Freqmult * XSC^[i]) * ZBase);

       // Off diagonals
        k := NumWindings;
        with ZB do
            for  i := 1 to Numwindings - 1 do
            begin
                for j := i + 1 to Numwindings - 1 do
                begin
                    SetElemSym(i, j,
                        (
                            GetElement(i, i)
                            + GetElement(j, j)
                            - Cmplx(
                                Rmult * (Winding^[i + 1].Rpu + Winding^[j + 1].Rpu), 
                                Freqmult * XSC^[k]
                            ) * ZBase
                        ) * 0.5);
                    Inc(k);
                end;
            end;

        ZB.Invert;   // mhos on one volt base

        if ZB.InvertError > 0 then
        begin
            DoErrorMsg('TTransformerObj.CalcYPrim', 
                Format(_('Matrix Inversion Error for Transformer "%s"'), [Name]),
                _('Invalid impedance specified. Replaced with tiny conductance to ground.'), 117);
            ZB.Clear;
            for i := 1 to ZB.Order do
                ZB.SetElement(i, i, Cmplx(EPSILON, 0.0));
        end;

        // Now construct Y_Oneturn = AT * ZB.Invert * A
        //     -1 1 0 ...
        // A = -1 0 1 ..   order:  N-1 x N   N = NumWindings
        //     ...
        //                       -1 -1 ...
        // AT = Transpose of A =  1  0 ...    N X N-1
        //                        0  1 ..

        Y_1Volt.Clear;
        Y_1Volt_NL.Clear;

        // Allocate temp complex arrays
        ctempArray1 := AllocMem(SizeOf(Complex) * NumWindings * 2);
        ctempArray2 := AllocMem(SizeOf(Complex) * NumWindings * 2);

        A := AllocMem(SizeOf(Complex) * NumWindings * 2);
        cMinusOne := cmplx(-1.0, 0.0);
        AT := TcMatrix.Creatematrix(NumWindings);
        for i := 1 to NumWindings - 1 do
            AT.SetElement(i + 1, i, cONE);
        for i := 1 to NumWindings - 1 do
            AT.SetElement(1, i, cMinusOne);
        ctemparray1^[NumWindings] := CZERO;
        for i := 1 to Numwindings do
        begin
            if i = 1 then
                for k := 1 to NumWindings - 1 do
                    A^[k] := cMinusOne
            else
                for k := 1 to NumWindings - 1 do
                    if k = (i - 1) then
                        A^[k] := cONE
                    else
                        A^[k] := cZERO;
            ZB.MVmult(ctemparray1, A); // Zb.invert * A
            AT.MVmult(ctempArray2, ctemparray1); // AT * Result
            for j := 1 to NumWindings do
                Y_1Volt.SetElement(j, i, ctempArray2^[j]);
        end;

        // Add magnetizing Reactance to 2nd winding, assuming it is closest to the core
        // Add both resistive element representing core losses and a reactive element representing
        // magnetizing current
        Y_1Volt_NL.AddElement(2, 2, Cmplx((pctNoLoadLoss / 100.0 / Zbase), -pctImag / 100.0 / Zbase / Freqmult));

        // should have admittance of one phase of the transformer on a one-volt, wye-connected base

        // Now make into terminal admittance matrix and correct for actual voltage ratings
        // Y_Terminal = AT * Y_onevolt * A  where V_onevolt = A * V_terminal

        AT.Free;

        Y_Term.Clear;
        Y_Term_NL.Clear;
        AT := TcMatrix.Creatematrix(NumWindings * 2);

        for i := 1 to NumWindings do
            with Winding^[i] do
                AT.SetElement(2 * i - 1, i, Cmplx(1.0 / (VBase * ZeroTapFix(puTap)), 0.0));
        for i := 1 to NumWindings do
            with Winding^[i] do
                AT.SetElement(2 * i, i, Cmplx(-1.0 / (VBase * ZeroTapFix(puTap)), 0.0));
        for i := 1 to 2 * Numwindings do
            ctemparray1^[i] := CZERO;

        for i := 1 to 2 * Numwindings do
        begin
            for k := 1 to NumWindings do
                with Winding^[k] do
                begin
                    if i = (2 * k - 1) then
                        A^[k] := Cmplx((1.0 / (VBase * ZeroTapFix(puTap))), 0.0)
                    else
                    if i = 2 * k then
                        A^[k] := Cmplx((-1.0 / (VBase * ZeroTapFix(puTap))), 0.0)
                    else
                        A^[k] := cZERO;
                end;
            // Main Transformer part
            Y_1Volt.MVmult(ctemparray1, A);
            AT.MVmult(ctemparray2, ctemparray1); // AT * Result
            for j := 1 to 2 * NumWindings do
                Y_Term.SetElement(j, i, ctemparray2^[j]);
       
            // No Load part
            Y_1Volt_NL.MVmult(ctemparray1, A);
            AT.MVmult(ctemparray2, ctemparray1); // AT * Result
            for j := 1 to 2 * NumWindings do
                Y_Term_NL.SetElement(j, i, ctemparray2^[j]);
        end;

        // Add a small Admittance to both conductors of each winding so that
        // the matrix will always invert even if the user neglects to define a voltage
        // reference on all sides
        if ppm_FloatFactor <> 0.0 then
            with Y_Term do
                for i := 1 to NumWindings do
                begin
                    Yadder := cmplx(0.0, Winding^[i].Y_PPM);
                    for j := (2 * i - 1) to (2 * i) do
                        SetElement(j, j, GetElement(j, j) + Yadder);
                        // SetElement(j, j, CmulReal_im(GetElement(j, j) , ppm_FloatFactorPlusOne));
                end;

        AT.Free;
        Reallocmem(A, 0);
        Reallocmem(ctemparray1, 0);
        Reallocmem(ctemparray2, 0);
    end;

    Y_Terminal_FreqMult := Freqmult;
end;

procedure TTransfObj.FetchXfmrCode();
var
    Obj: TXfmrCodeObj;
    i: Integer;
begin
    if XfmrCodeObj = NIL then
        Exit;

    Obj := TXfmrCodeObj(XfmrCodeObj);
    // set sizes and copy parameters
    FNphases := Obj.Fnphases;
    SetNumWindings(Obj.NumWindings);
    NConds := Fnphases + 1; // forces reallocation of terminals and conductors
    for i := 1 to NumWindings do
        // Records can be copied
        Winding^[i] := Obj.Winding^[i];

    SetTermRef;

    // Parameters for all windings
    XHL := Obj.XHL;
    XHT := Obj.XHT;
    XLT := Obj.XLT;
    for i := 1 to (NumWindings * (NumWindings - 1) div 2) do
        XSc^[i] := Obj.XSC^[i];

    ThermalTimeConst := Obj.ThermalTimeConst;
    n_thermal := Obj.n_thermal;
    m_thermal := Obj.m_thermal;
    FLrise := Obj.FLrise;
    HSrise := Obj.HSrise;
    pctLoadLoss := Obj.pctLoadLoss;
    pctNoLoadLoss := Obj.pctNoLoadLoss;
    pctImag := Obj.pctImag;  // Omission corrected 12-14-18
    NormMaxHkVA := Obj.NormMaxHkVA;
    EmergMaxHkVA := Obj.EmergMaxHkVA;
    ppm_FloatFactor := Obj.ppm_FloatFactor;
    Yorder := fNConds * fNTerms;
    YPrimInvalid := TRUE;
    Y_Terminal_FreqMult := 0.0;

    NumAmpRatings := Obj.NumkVARatings;
    SetLength(kVARatings, NumAmpRatings);
    for i := 0 to High(kVARatings) do
        kVARatings[i] := Obj.kVARatings[i];

    RecalcElementData
end;

end.