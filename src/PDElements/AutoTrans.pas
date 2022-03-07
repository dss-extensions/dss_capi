unit AutoTrans;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2018, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

//   Change log
//   7-14-2018  Created from Transformer
//   9-19-2018  committed
//   12-4-2018  Corrected indices for mapping into Yprim
//   1-3-2019   Default last nphase nodes of X terminal (2) to same as first neutral node
//   3-6-2021  Added code for readability

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
    math;

type
{$SCOPEDENUMS ON}
    TAutoTransProp = (
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
        Rdcohms=10,
        Core=11,

        // General Data
        buses=12,
        conns=13,
        kVs=14,
        kVAs=15,
        taps=16,
        XHX=17,
        XHT=18,
        XXT=19,
        XSCarray=20,
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

        //bank=38, // removed, unused
        //XfmrCode=39, // removed, unused
        XRConst=38, //- was 40
        LeadLag=39, // was 41
        WdgCurrents=40 // was 42
    );

    TAutoTransConnection = (
        Wye = 0,
        Delta = 1,
        Series = 2
    );
{$SCOPEDENUMS OFF}


    TAutoTrans = class(TPDClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function BeginEdit(ptr: Pointer; SetActive: Boolean=True): Pointer; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TAutoWinding = object
        Connection: TAutoTransConnection;
        kVLL,
        VBase,
        kVA,
        puTap: Double;
        Rpu: Double;    // on AutoTrans MVABase  (H-X Rating)
        Rdcpu: Double;    // on AutoTrans MVABase  (H-X Rating)
        Rdcohms: Double;    // for GIC solutions; default to 85% of Rpu
        RdcSpecified: Boolean;

        Y_PPM: Double;  // Anti Float reactance adder

        {Tap Changer Data}
        TapIncrement,
        MinTap,
        MaxTap: Double;
        NumTaps: Integer;

        procedure ComputeAntiFloatAdder(PPM_Factor, VABase1ph: Double);
        procedure Init(iWinding: Integer);
    end;

    AutoWindingArray = array[1..3] of TAutoWinding;
    pAutoWindingArray = ^AutoWindingArray;

    TAutoTransObj = class(TPDElement)
    PRIVATE

        DeltaDirection: Integer;
        ppm_FloatFactor: Double; //  parts per million winding float factor
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

        procedure CalcY_Terminal(FreqMult: Double);
        procedure GICBuildYTerminal;

        procedure BuildYPrimComponent(YPrim_Component, Y_Terminal: TCMatrix);

    PROTECTED
        MaxWindings: Integer;
        TermRef: pIntegerArray;  // keeps track of terminal connections

        puXHX, puXHT,
        puXXT: Double;  // per unit
        Zbase: Double;
        puXSC: pDoubleArray;     // per unit SC measurements
        VABase: Double;    // FOR impedances
        kVSeries: Double;   // Rating for Series winding

        ZB: TCMatrix;
        Y_1Volt: TCMatrix;
        Y_Term: TCMatrix;
        Y_1Volt_NL: TCMatrix;   // No Load Y's
        Y_Term_NL: TCMatrix;

        Y_Terminal_Freqmult: Double;

        NormMaxHkVA: Double;
        EmergMaxHkVA: Double;
        ThermalTimeConst: Double;  {hr}
        n_thermal: Double;
        m_thermal: Double;  {Exponents}
        FLrise: Double;
        HSrise: Double;

        HVLeadsLV: LongBool;

        XHXChanged: Boolean;

        procedure SetTermRef;
    PUBLIC
        NumWindings: Integer;
        ActiveWinding: Integer;  // public for COM interface

        IsSubstation: LongBool;
        SubstationName: String;
        Winding: pAutoWindingArray;
        XfmrBank: String;
        XfmrCode: String;
        CoreType: Integer; {0=Shell; 1=1ph; 3-3leg; 5=5-leg}
        pctImag: Double;
        pctLoadLoss: Double;
        pctNoLoadLoss: Double;

        constructor Create(ParClass: TDSSClass; const TransfName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;
        procedure SetBus(iwdg: Integer; const s: String); override;

        procedure SetNumWindings(N: Integer);

        procedure RecalcElementData; OVERRIDE;
        procedure SetNodeRef(iTerm: Integer; NodeRefArray: pIntegerArray); OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        // GetLosses override for AutoTrans
        procedure GetLosses(var TotalLosses, LoadLosses, NoLoadLosses: Complex); OVERRIDE;
        // Getcurrents Override for AutoTrans
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Get present values of terminal

        function RotatePhases(iPhs: Integer): Integer;
        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;
        procedure SaveWrite(F: TFileStream); OVERRIDE;
        procedure GetAutoWindingVoltages(iWind: Integer; VBuffer: pComplexArray);
        procedure GetAllWindingCurrents(CurrBuffer: pComplexArray);


        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        property PresentTap[i: Integer]: Double READ Get_PresentTap WRITE Set_PresentTap;
        property Mintap[i: Integer]: Double READ Get_MinTap;
        property Maxtap[i: Integer]: Double READ Get_MaxTap;
        property TapIncrement[i: Integer]: Double READ Get_TapIncrement;
        property BaseVoltage[i: Integer]: Double READ Get_BaseVoltage;  // Winding VBase
        property BasekVLL[i: Integer]: Double READ Get_BasekVLL;  // Winding VBase

        // CIM accessors
        property NumTaps[i: Integer]: Integer READ Get_NumTaps;
        property WdgResistance[i: Integer]: Double READ Get_WdgResistance;
        property WdgkVA[i: Integer]: Double READ Get_WdgkVA;
        property WdgConnection[i: Integer]: Integer READ Get_WdgConnection;
        property XscVal[i: Integer]: Double READ Get_Xsc;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Utilities,
    XfmrCode,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TAutoTransObj;
    TProp = TAutoTransProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    
    AutoTransConnectionEnum: TDSSEnum;

constructor TAutoTrans.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        AutoTransConnectionEnum := TDSSEnum.Create('AutoTrans: Connection', True, 1, 2,
            ['wye', 'delta', 'series', 'y', 'ln', 'll'],
            [0, 1, 2, 0, 0, 1]);
    end;

    inherited Create(dssContext, AUTOTRANS_ELEMENT, 'AutoTrans');
end;

destructor TAutoTrans.Destroy;
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

procedure TAutoTrans.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    PropertyStructArrayOffset := ptruint(@obj.Winding);
    PropertyStructArrayStep := SizeOf(TAutoWinding);
    PropertyStructArrayIndexOffset := ptruint(@obj.ActiveWinding);
    PropertyStructArrayCountOffset := ptruint(@obj.NumWindings);

    // RO string
    PropertyType[ord(TProp.WdgCurrents)] := TPropertyType.StringSilentROFunctionProperty;
    PropertyOffset[ord(TProp.WdgCurrents)] := ptruint(@GetWindingCurrentsResult);

    // double array
    PropertyType[ord(TProp.Xscarray)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.Xscarray)] := ptruint(@obj.puXSC);
    PropertyOffset3[ord(TProp.Xscarray)] := ptruint(@XscSize);
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

    // double-on-struct array properties
    PropertyType[ord(TProp.kV)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.kV)] := ptruint(@TAutoWinding(nil^).kVLL);

    PropertyType[ord(TProp.kVA)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.kVA)] := ptruint(@TAutoWinding(nil^).kVA);

    PropertyType[ord(TProp.tap)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.tap)] := ptruint(@TAutoWinding(nil^).puTap);

    PropertyType[ord(TProp.MaxTap)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.MaxTap)] := ptruint(@TAutoWinding(nil^).MaxTap);

    PropertyType[ord(TProp.MinTap)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.MinTap)] := ptruint(@TAutoWinding(nil^).MinTap);

    PropertyType[ord(TProp.RdcOhms)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.RdcOhms)] := ptruint(@TAutoWinding(nil^).RdcOhms);

    PropertyType[ord(TProp.pctR)] := TPropertyType.DoubleOnStructArrayProperty;
    PropertyOffset[ord(TProp.pctR)] := ptruint(@TAutoWinding(nil^).Rpu);
    PropertyScale[ord(TProp.pctR)] := 0.01;

    // bus, indirect
    PropertyType[ord(TProp.bus)] := TPropertyType.BusOnStructArrayProperty;
    PropertyOffset[ord(TProp.bus)] := 1; // dummy value, just to mark the property as handled
    
    PropertyType[ord(TProp.buses)] := TPropertyType.BusesOnStructArrayProperty;
    PropertyOffset[ord(TProp.buses)] := ptruint(@obj.NumWindings);
    PropertyFlags[ord(TProp.buses)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.buses)] := ord(TProp.bus);

    // enum on array of structs
    PropertyType[ord(TProp.conn)] := TPropertyType.MappedStringEnumOnStructArrayProperty;
    PropertyOffset[ord(TProp.conn)] := ptruint(@TAutoWinding(nil^).Connection);
    PropertyOffset2[ord(TProp.conn)] := PtrInt(AutoTransConnectionEnum);

    // array of enums on array of structs
    PropertyType[ord(TProp.conns)] := TPropertyType.MappedStringEnumArrayOnStructArrayProperty;
    PropertyOffset[ord(TProp.conns)] := ptruint(@TAutoWinding(nil^).Connection);
    PropertyOffset2[ord(TProp.conns)] := PtrInt(AutoTransConnectionEnum);
    PropertyFlags[ord(TProp.conns)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.conns)] := ord(TProp.conn);

    // integer on struct array
    PropertyType[ord(TProp.NumTaps)] := TPropertyType.IntegerOnStructArrayProperty;
    PropertyOffset[ord(TProp.NumTaps)] := ptruint(@TAutoWinding(nil^).NumTaps);

    // integer properties
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    PropertyType[ord(TProp.windings)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.windings)] := ptruint(@obj.NumWindings);
    PropertyFlags[ord(TProp.windings)] := [TPropertyFlag.NonZero, TPropertyFlag.NonNegative];

    PropertyType[ord(TProp.wdg)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.wdg)] := ptruint(@obj.ActiveWinding);
    PropertyFlags[ord(TProp.wdg)] := [TPropertyFlag.IntegerStructIndex];

    // string properties
    PropertyType[ord(TProp.subname)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.subname)] := ptruint(@obj.SubstationName);

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
    PropertyOffset[ord(TProp.XHX)] := ptruint(@obj.puXHX);
    PropertyOffset[ord(TProp.XHT)] := ptruint(@obj.puXHT);
    PropertyOffset[ord(TProp.XXT)] := ptruint(@obj.puXXT);
    
    PropertyScale[ord(TProp.XHX)] := 0.01;
    PropertyScale[ord(TProp.XHT)] := 0.01;
    PropertyScale[ord(TProp.XXT)] := 0.01;

    PropertyTrapZero[ord(TProp.XHX)] := 7.0;
    PropertyTrapZero[ord(TProp.XHT)] := 35.0;
    PropertyTrapZero[ord(TProp.XXT)] := 30.0;

    // double arrays via struct array
    PropertyType[ord(TProp.pctRs)] := TPropertyType.DoubleArrayOnStructArrayProperty;
    PropertyOffset[ord(TProp.pctRs)] := ptruint(@TAutoWinding(nil^).Rpu); 
    PropertyOffset2[ord(TProp.pctRs)] := ptruint(@obj.NumWindings);
    PropertyScale[ord(TProp.pctRs)] := 0.01;
    PropertyFlags[ord(TProp.pctRs)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.pctRs)] := ord(TProp.pctR);

    PropertyType[ord(TProp.kVs)] := TPropertyType.DoubleArrayOnStructArrayProperty;
    PropertyOffset[ord(TProp.kVs)] := ptruint(@TAutoWinding(nil^).kVLL); 
    PropertyOffset2[ord(TProp.kVs)] := ptruint(@obj.NumWindings);
    PropertyFlags[ord(TProp.kVs)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.kVs)] := ord(TProp.kV);

    PropertyType[ord(TProp.kVAs)] := TPropertyType.DoubleArrayOnStructArrayProperty;
    PropertyOffset[ord(TProp.kVAs)] := ptruint(@TAutoWinding(nil^).kVA); 
    PropertyOffset2[ord(TProp.kVAs)] := ptruint(@obj.NumWindings);
    PropertyFlags[ord(TProp.kVAs)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.kVAs)] := ord(TProp.kVA);

    PropertyType[ord(TProp.taps)] := TPropertyType.DoubleArrayOnStructArrayProperty;
    PropertyOffset[ord(TProp.taps)] := ptruint(@TAutoWinding(nil^).puTap); 
    PropertyOffset2[ord(TProp.taps)] := ptruint(@obj.NumWindings);
    PropertyFlags[ord(TProp.taps)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.taps)] := ord(TProp.tap);


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TAutoTrans.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TAutoTransObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    i: Integer;
    OldWdgSize: Integer;
    NewWdgSize: Integer;
begin
    case Idx of
        ord(TProp.phases):
            if FNPhases <> previousIntVal then
                NConds := 2 * Fnphases;  // Force redefinition of number of conductors and reallocation of matrices
            // YPrim is built with windings not connected.  Connected in NodeRef
            // default all winding kVAs to first winding so latter Donot have to be specified

        ord(TProp.conn):
        begin
            // TODO: warn if the user tries to change the connecton for first two windings?
            with Winding[ActiveWinding] do
                case ActiveWinding of
                    1:
                        Connection := TAutoTransConnection.Series;  // First Winding always Series
                    2:
                        Connection := TAutoTransConnection.Wye;  // Second Winding is always Common and Wye
                end;
            Yorder := fNConds * fNTerms;
            // YPrimInvalid := TRUE; -- already done below
        end;
        ord(TProp.conns):
        begin
            // TODO: warn if the user tries to change the connecton for first two windings?
            for i := 1 to NumWindings do
                with Winding[i] do
                    case i of
                        1:
                            Connection := TAutoTransConnection.Series;  // First Winding always Series
                        2:
                            Connection := TAutoTransConnection.Wye;  // Second Winding is always Common and Wye
                    end;
            
            Yorder := fNConds * fNTerms;
            // YPrimInvalid := TRUE; -- already done below
        end;

        ord(TProp.windings):
        begin
            if NumWindings <= 0 then
            begin
                i := NumWindings;
                NumWindings := previousIntVal;
                DoSimpleMsg('Invalid number of windings: (%d) for "%s"', [i, FullName], 100111);
                Exit;
            end;
            OldWdgSize := (previousIntVal - 1) * previousIntVal div 2;
            MaxWindings := NumWindings;
            NewWdgSize := (NumWindings - 1) * NumWindings div 2;
            FNconds := 2 * Fnphases;
            Nterms := NumWindings;
            Reallocmem(Winding, Sizeof(TAutoWinding) * MaxWindings);  // Reallocate collector array
            for i := 1 to MaxWindings do
                Winding[i].Init(i);

            // array of short circuit measurements between pairs of windings
            ReAllocmem(puXSC, SizeOF(puXSC^[1]) * NewWdgSize);
            for i := OldWdgSize + 1 to NewWdgSize do
                puXSC^[i] := 0.30;
            Reallocmem(TermRef, SizeOf(TermRef^[1]) * 2 * NumWindings * Fnphases);

            // Reallocate impedance matrices
            ZB.Free;
            Y_1Volt.Free;
            Y_1Volt_NL.Free;
            Y_Term.Free;
            Y_Term_NL.Free;

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
        ord(TProp.Rdcohms):
            Winding^[ActiveWinding].RdcSpecified := TRUE;
        ord(TProp.kVAs):
        begin
            NormMaxHkVA := 1.1 * Winding^[1].kVA;    // Defaults for new winding rating.
            EmergMaxHkVA := 1.5 * Winding^[1].kVA;
        end;
        17..19:
            XHXChanged := TRUE;
        26:
        begin    // Assume load loss is split evenly  between windings 1 and 2
            Winding^[1].Rpu := pctLoadLoss / 2.0 / 100.0;
            Winding^[2].Rpu := Winding^[1].Rpu;
        end;
        37:
            pctLoadLoss := (Winding^[1].Rpu + Winding^[2].Rpu) * 100.0;  // Update
        // 38:
        //     DoSimpleMsg('Bank Property not used with AutoTrans object.', 100130);
        // 39:
        //     DoSimpleMsg('XFmrCode Property not used with AutoTrans object.', 100131);
    end;

    //YPrim invalidation on anything that changes impedance values
    case Idx of
        5..19:
            YprimInvalid := TRUE;
        26..27:
            YprimInvalid := TRUE;
        35..37:
            YprimInvalid := TRUE;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TAutoTransObj.SetBus(iwdg: Integer; const s: String); 
// Previously "SetBusAuto"
// Added Jan 3, 2019
var
    NNodes: array[1..50] of Integer; // big integer buffer
    NumNodes: Integer;
    ii: Integer;
    strBusName,
    strNewBusName: String;
begin
    // For winding 2 set all nodes on second end of winding to same as 1st value
    // so all neutral ends of common winding get connected to same neutral node
    case iwdg of
        2:
        begin
            for ii := 1 to nphases do
                NNodes[ii] := ii; // set up buffer with defaults
               // Default all other conductors to a ground connection
               // If user wants them ungrounded, must be specified explicitly!
            for ii := nphases + 1 to NConds do
                NNodes[ii] := 0;

            strBusName := DSS.AuxParser.ParseAsBusName(s, NumNodes, pIntegerArray(@NNodes));

            // Check for non-zero neutral specification
            if NNodes[nphases + 1] > 0 then
            begin
                // Reconstruct new bus name
                strNewBusName := strBusName;
                for ii := 1 to Nphases do
                    strNewBusName := strNewBusName + Format('.%d', [NNodes[ii]]);
                for ii := nphases + 1 to Nconds do
                    strNewBusName := strNewBusName + Format('.%d', [NNodes[nphases + 1]]);
                inherited SetBus(iwdg, strNewBusName);
            end
            else
                inherited SetBus(iwdg, s);
        end;
    else
        inherited Setbus(iwdg, s);  // all other windings
    end;
end;

function TAutoTrans.BeginEdit(ptr: Pointer; SetActive: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj(inherited BeginEdit(ptr, SetActive));
    Obj.XHXChanged := FALSE;
    Result := Obj;
end;

procedure TAutoTransObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNphases := Other.Fnphases;
    SetNumWindings(Other.NumWindings);
    NConds := 2 * Fnphases; // forces reallocation of terminals and conductors

    Yorder := fNConds * fNTerms;
    YPrimInvalid := TRUE;

    for i := 1 to NumWindings do
        Winding^[i] := Other.Winding^[i];
        
    SetTermRef;

    puXHX := Other.puXHX;
    puXHT := Other.puXHT;
    puXXT := Other.puXXT;

    for i := 1 to (NumWindings * (NumWindings - 1) div 2) do
        puXSC^[i] := Other.puXSC^[i];

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
    XfmrCode := Other.XfmrCode;
end;

constructor TAutoTransObj.Create(ParClass: TDSSClass; const TransfName: String);
var
    i: Integer;
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(TransfName);
    DSSObjType := ParClass.DSSClassType; //DSSObjType + XFMR; // override PDElement   (kept in both actually)

    FNphases := 3;  // Directly set conds and phases
    fNConds := 2 * Fnphases; // 2 conductors per phase; let NodeRef connect neutral, etc.
    SetNumWindings(2);  // must do this after setting number of phases
    ActiveWinding := 1;

    Nterms := NumWindings;  // Force allocation of terminals and conductors

    puXHX := 0.10;
    puXHT := 0.35;
    puXXT := 0.30;
    XHXChanged := TRUE;  // Set flag to for calc of XSC array from XHL, etc.

    DeltaDirection := 1;
    SubstationName := '';
    XfmrBank := '';
    XfmrCode := '';

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
    // ompute antifloat added for each winding
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
    RecalcElementData;
end;

procedure TAutoTransObj.SetNodeRef(iTerm: Integer; NodeRefArray: pIntegerArray);
// Overrides standard function
var
    i: Integer;
begin
    inherited SetNodeRef(iTerm, NodeRefArray);

    // Now fixup noderefs for series winding of AutoTrans
    // Warning **** Magic happens here
    // Redefine 2nd node of Series winding to same as first node of 2nd winding (common winding)

    if iTerm = 2 then
        if Winding^[1].Connection = TAutoTransConnection.Series then
        begin
            for i := 1 to Fnphases do
            begin
                NodeRef^[Fnphases + i] := NodeRef^[i + Fnconds];
                Terminals[iTerm - 1].TermNodeRef[Fnphases + i - 1] := NodeRef^[i + Fnconds];
            end;
        end;
end;

procedure TAutoTransObj.SetNumWindings(N: Integer);
var
    prev: Integer;
begin
    prev := NumWindings;
    NumWindings := N;
    PropertySideEffects(ord(TProp.windings), prev);
end;

destructor TAutoTransObj.Destroy;
begin
    Reallocmem(Winding, 0);
    Reallocmem(puXSC, 0);
    Reallocmem(TermRef, 0);
    ZB.Free;
    Y_1Volt.Free;
    Y_1Volt_NL.Free;
    Y_Term.Free;
    Y_Term_NL.Free;
    inherited Destroy;
end;

procedure TAutoTransObj.RecalcElementData;
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
    if Winding^[1].connection = TAutoTransConnection.Series then
        DeltaDirection := 1  // Auto
    else
    begin
        if Winding^[1].kVLL >= Winding^[2].kVLL then
            iHvolt := 1
        else
            iHVolt := 2;
        case Winding^[iHvolt].Connection of
            TAutoTransConnection.Wye:
                if HVLeadsLV then
                    DeltaDirection := -1
                else
                    DeltaDirection := 1;
            TAutoTransConnection.Delta:
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

    if XHXChanged then
    begin
        // should only happen for 2- and 3-winding AutoTrans
        if NumWindings <= 3 then
            for i := 1 to (NumWindings * (NumWindings - 1) div 2) do
                case i of
                    1:
                        puXSC^[1] := puXHX;
                    2:
                        puXSC^[2] := puXHT;
                    3:
                        puXSC^[3] := puXXT;
                else
                end;
        XHXChanged := FALSE;
    end;

   // Set winding voltage bases (in volts)
    for i := 1 to NumWindings do
        with Winding^[i] do  // Get the actual turns voltage base for each winding
            case Connection of
                TAutoTransConnection.Wye:
                    case Fnphases of
                        2, 3:
                            VBase := kVLL * InvSQRT3x1000;   // assume 3-phase for 2-phase designation
                    else
                        VBase := kVLL * 1000.0;
                    end;
                TAutoTransConnection.Delta:
                    VBase := kVLL * 1000.0;
                TAutoTransConnection.Series:
                begin                            // Series winding for Auto  Should be Winding[1]
                    case Fnphases of
                        2, 3:
                            kVseries := (kVLL - Winding^[2].kVLL) / SQRT3;
                    else
                        kVseries := kVLL - Winding^[2].kVLL;
                    end;
                    if kVSeries = 0.0 then
                        kVSeries := kVLL * 0.0001; // In case series has same voltage as common
                    VBase := kVseries * 1000.0;
                end;
            end;

   {Base rating of Winding 1 }
    VABase := Winding^[1].kVA * 1000.0;

   // Set Rdc parameters for each winding.
    for i := 1 to NumWindings do
        with Winding^[i] do
        begin
            if RdcSpecified then
                Rdcpu := RdcOhms / (SQR(VBase) / VABase)
            else
            begin
                Rdcpu := 0.85 * Rpu; // default to 85% of the ac value (does not include stray loss)
                RdcOhms := Rdcpu * SQR(VBase) / VABase;
            end;
        end;

    for i := 1 to NumWindings do
        Winding^[i].ComputeAntiFloatAdder(ppm_FloatFactor, VABase / FNPhases);

   { Normal and Emergency terminal current Rating for UE check}
    Vfactor := 1.0;  // ensure initialization
    case Winding^[1].connection of
        TAutoTransConnection.Wye:
            VFactor := Winding^[1].VBase * 0.001;   // wye
        TAutoTransConnection.Delta:
            case Fnphases of
                1:
                    VFactor := Winding^[1].VBase * 0.001;
                2, 3:
                    VFactor := Winding^[1].VBase * 0.001 / SQRT3;
            else
                VFactor := Winding^[1].VBase * 0.001 * 0.5 / sin(pi / Fnphases);
            end;
        TAutoTransConnection.Series:
            VFactor := Winding^[1].VBase * 0.001;   // Series Winding
    end;

     {Divide per phase kVA by voltage to neutral}
    NormAmps := NormMaxHkVA / Fnphases / Vfactor;
    EmergAmps := EmergMaxHkVA / Fnphases / Vfactor;

    CalcY_Terminal(1.0);   // Calc Y_Terminal at base frequency
end;

procedure TAutoTransObj.SaveWrite(F: TFileStream);
// Override standard SaveWrite
// AutoTrans structure not conducive to standard means of saving
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
                3:
                begin   // if WDG= was ever used write out arrays ...
                    for i := 12 to 16 do
                        FSWrite(F, Format(' %s=%s', [ParentClass.PropertyName^[i], GetPropertyValue(i)]));
                    for i := 1 to Numwindings do
                        FSWrite(F, Format(' wdg=%d %sR=%.7g', [i, '%', Winding^[i].Rpu * 100.0]));
                end;
                4..9:
                    ; // Ignore these properties; use arrays instead
            else
                if Length(PropertyValue[iProp]) > 0 then
                    FSWrite(F, Format(' %s=%s', [ParentClass.PropertyName[iProp], CheckForBlanks(PropertyValue[iProp])]));
            end;
        iProp := GetNextPropertySet(iProp);
    end;
end;

procedure TAutoTransObj.SetTermRef;

// sets an array which maps the two conductors of each winding to the
// phase and neutral conductors of the AutoTrans according to the winding connection

var
    i, j, k: Integer;

begin
    k := 0;

    case Fnphases of
        1:
            for j := 1 to NumWindings do
            begin
                Inc(k);
                TermRef^[k] := (j - 1) * fNconds + 1;  // fNconds = 2
                Inc(k);
                TermRef^[k] := j * fNconds;
            end;
    else
        // Typical array for 3-phase auto
        // This builds the YPrim and the NodeRef array maps it into Y
        //     TermRef^[1] := 1;
        //     TermRef^[2] := 4;
        //     TermRef^[3] := 7;
        //     TermRef^[4] := 10;
        //     TermRef^[5] := 2;
        //     TermRef^[6] := 5;
        //     TermRef^[7] := 8;
        //     TermRef^[8] := 11;
        //     TermRef^[9] := 3;
        //     TermRef^[10] := 6;
        //     TermRef^[11] := 9;
        //     TermRef^[12] := 12;

        for i := 1 to Fnphases do
        begin
            for  j := 1 to NumWindings do
            begin
                Inc(k);
                case Winding^[j].Connection of
                    TAutoTransConnection.Wye:
                    begin      // Wye
                        TermRef^[k] := (j - 1) * FNConds + i;
                        Inc(k);
                        TermRef^[k] := TermRef^[k - 1] + Fnphases;
                    end;
                    // **** WILL THIS WORK for 2-PHASE OPEN DELTA ???? Need to check this sometime

                    TAutoTransConnection.Delta:
                    begin   // Delta
                        TermRef^[k] := (j - 1) * fNconds + i;
                        Inc(k);
                        TermRef^[k] := (j - 1) * fNconds + RotatePhases(i);  // connect to next phase in sequence
                    end;

                    TAutoTransConnection.Series:
                    begin // Series Winding for Auto Transfomer
                        TermRef^[k] := i;
                        Inc(k);
                        TermRef^[k] := i + Fnphases;
                    end;
                end;
            end;
        end;

    end; // CASE Fnphases
end;

procedure TAutoTransObj.CalcYPrim;
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


    // Combine the two Yprim components into Yprim
    YPrim.CopyFrom(YPrim_Series);
    Yprim.AddFrom(Yprim_Shunt);

    // Now Account for Open Conductors
    // For any conductor that is open, zero out row and column
    inherited CalcYPrim;

    YprimInvalid := FALSE;
end;

procedure TAutoTransObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);

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
                FSWriteln(F, Format('~ Wdg=%d bus=%s', [i, nextbus]));
            case Connection of
                TAutoTransConnection.Wye:
                    FSWriteln(F, '~ conn=wye');
                TAutoTransConnection.Delta:
                    FSWriteln(F, '~ conn=delta');
                TAutoTransConnection.Series:
                    FSWriteln(F, '~ conn=Series');
            end;
            FSWriteln(F, Format('~ kv=%.7g', [kVLL]));
            FSWriteln(F, Format('~ kVA=%.7g', [kVA]));
            FSWriteln(F, Format('~ tap=%.7g', [putap]));
            FSWriteln(F, Format('~ %%r=%.7g', [Rpu * 100.0]));
            FSWriteln(F, Format('~ Rdcohms=%.7g', [Rdcohms]));

        end;
    end;

    FSWriteln(F, Format('~ XHL=%.3f', [puXHX * 100.0]));
    FSWriteln(F, Format('~ XHT=%.3f', [puXHT * 100.0]));
    FSWriteln(F, Format('~ XLT=%.3f', [puXXT * 100.0]));
    // FSWriteln(F, Format('~ X12=%.3f', [puXHX * 100.0]));
    // FSWriteln(F, Format('~ X13=%.3f', [puXHT * 100.0]));
    // FSWriteln(F, Format('~ X23=%.3f', [puXXT * 100.0]));
    FSWrite(F, '~ Xscmatrix= "');
    for i := 1 to (NumWindings - 1) * NumWindings div 2 do
        FSWrite(F, Format('%.2f ', [puXSC^[i] * 100.0]));
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

procedure TAutoWinding.ComputeAntiFloatAdder(PPM_Factor, VABase1ph: Double);
begin
    Y_PPM := -PPM_Factor / (SQR(VBase) / VABase1ph) / 2.0;
    // put half on each terminal of the winding.
end;

procedure TAutoWinding.Init(iWinding: Integer);
// Initialize a new winding
begin
    case iWinding of
        1:
        begin
            Connection := TAutoTransConnection.Series;  // First Winding is Series Winding
            kVLL := 115.0;
        end
    else
        Connection := TAutoTransConnection.Wye;
        kVLL := 12.47;
    end;

    VBase := kVLL / SQRT3 * 1000.0;
    kVA := 1000.0;
    puTap := 1.0;
    Rpu := 0.002;
    Rdcpu := Rpu * 0.85;  // default value
    RdcOhms := Sqr(kVLL) / (kVA / 1000.0) * Rdcpu;
    RdcSpecified := FALSE;
    ComputeAntiFloatAdder(1.0e-6, kVA / 3.0 / 1000.0);     //  1 PPM

    TapIncrement := 0.00625;
    NumTaps := 32;
    MaxTap := 1.10;
    MinTap := 0.90;
end;

function TAutoTransObj.Get_PresentTap(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].puTap
    else
        Result := 0.0;
end;

procedure TAutoTransObj.Set_PresentTap(i: Integer; const Value: Double);
var
    TempVal: Double;
begin
    if (i > 0) and (i <= NumWindings) then
        with Winding^[i] do
        begin
           {Range Checking}
            TempVal := Value;
            if (TempVal < MinTap) then
                TempVal := MinTap
            else
            if (TempVal > MaxTap) then
                TempVal := MaxTap;

            if TempVal <> puTap then
            begin    {Only if there's been a change}
                puTap := TempVal;
                YPrimInvalid := TRUE;  // this property triggers setting SystemYChanged=true
                RecalcElementData;
            end;
        end;
end;

function TAutoTransObj.Get_WdgResistance(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].Rpu
    else
        Result := 0.0;
end;

function TAutoTransObj.Get_WdgkVA(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].kVA
    else
        Result := 0.0;
end;

function TAutoTransObj.Get_Xsc(i: Integer): Double;
var
    imax: Integer;
begin
    imax := (NumWindings - 1) * NumWindings div 2;
    if (i > 0) and (i <= imax) then
        Result := puXSC^[i]
    else
        Result := 0.0;
end;

function TAutoTransObj.Get_WdgConnection(i: Integer): Integer;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Ord(Winding^[i].Connection)
    else
        Result := 0;
end;

function TAutoTransObj.Get_MinTap(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].MinTap
    else
        Result := 0.0;
end;

function TAutoTransObj.Get_MaxTap(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].MaxTap
    else
        Result := 0.0;
end;

function TAutoTransObj.Get_NumTaps(i: Integer): Integer;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].NumTaps
    else
        Result := 0;
end;

function TAutoTransObj.Get_TapIncrement(i: Integer): Double;
begin
    if (i > 0) and (i <= NumWindings) then
        Result := Winding^[i].TapIncrement
    else
        Result := 0.0;
end;

procedure TAutoTransObj.GetAllWindingCurrents(CurrBuffer: pComplexArray);
//   Return a vector of complex currents for each Winding of all phases

//   Iterm = Yterm * Vterm

//   Yterm order is 2*NumWindings.  Each phase has same Yterm.
//   Vterm order is 2*NumWindings .

//   Calculate Iterm phase-by-phase and concatenate into CurrBuffer.
var
    jphase, k, iPhase, iWind, i: Integer;
    VTerm: pComplexArray;
    ITerm: pComplexArray;
    ITerm_NL: pComplexArray;

begin
    try
        Vterm := Allocmem(SizeOf(Complex) * 2 * NumWindings);
        Iterm := Allocmem(SizeOf(Complex) * 2 * NumWindings);
        ITerm_NL := Allocmem(SizeOf(Complex) * 2 * NumWindings);

     {Load up Vterminal - already allocated for all cktelements}
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
                i := 2 * iWind - 1;
                case Winding^[iWind].Connection of
                    TAutoTransConnection.Wye:
                    begin   // Wye  (Common winding usually)
                        VTerm^[i] := Vterminal^[iphase + (iWind - 1) * FNconds];
                        VTerm^[i + 1] := Vterminal^[iphase + (iWind - 1) * FNconds + FNphases];
                    end;
                    TAutoTransConnection.Delta:
                    begin   // Delta
                        jphase := RotatePhases(iphase);      // Get next phase in sequence
                        VTerm^[i] := Vterminal^[iphase + (iWind - 1) * FNconds];
                        VTerm^[i + 1] := Vterminal^[jphase + (iWind - 1) * FNconds];
                    end;
                    TAutoTransConnection.Series:
                    begin    // Series Winding
                        VTerm^[i] := Vterminal^[iphase + (iWind - 1) * FNconds];
                        VTerm^[i + 1] := Vterminal^[iphase + Fnphases];
                    end;
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
            DoSimpleMsg('Error filling voltage buffer in GetAllWindingCurrents for Circuit Element:AutoTrans.' + Name + CRLF +
                'Probable Cause: Invalid definition of element.' + CRLF +
                'System Error Message: ' + E.Message, 100115);
    end;
end;

procedure TAutoTransObj.GeTAutoWindingVoltages(iWind: Integer; VBuffer: pComplexArray);
//  Voltages across indicated winding
// Fill Vbuffer array which must be adequately allocated by calling routine
// Order is Number of Phases
var
    i, ii, k, NeutTerm: Integer;
begin
    try
        // return Zero if winding number improperly specified
        if (iWind < 1) or (iWind > NumWindings) then
        begin
            for i := 1 to FNconds do
                VBuffer^[i] := CZERO;
            Exit;
        end;

        // Load up VTerminal - already allocated for all cktelements
        with ActiveCircuit.Solution do
            for i := 1 to Yorder do
                Vterminal^[i] := NodeV^[NodeRef^[i]];


        k := (iWind - 1) * FNconds;    // Offset for winding
        NeutTerm := Fnphases + k + 1;
        for i := 1 to Fnphases do
            case Winding^[iWind].Connection of
                TAutoTransConnection.Wye:
                begin      // Wye
                    VBuffer^[i] := Vterminal^[i + k] - Vterminal^[NeutTerm];
                end;
                TAutoTransConnection.Delta:
                begin   // Delta
                    ii := RotatePhases(i);      // Get next phase in sequence
                    VBuffer^[i] := Vterminal^[i + k] - Vterminal^[ii + k];
                end;
                TAutoTransConnection.Series:
                begin      // Series   (winding 1)
                    VBuffer^[i] := Vterminal^[i + k] - Vterminal^[i + Fnconds];
                end;
            end;

    except
        On E: Exception do
            DoSimpleMsg('Error filling voltage buffer in GeTAutoWindingVoltages for Circuit Element:AutoTrans.' + Name + CRLF +
                'Probable Cause: Invalid definition of element.' + CRLF +
                'System Error Message: ' + E.Message, 100114);
    end;
end;

function TAutoTransObj.Get_BaseVoltage(i: Integer): Double;
begin
    if (i < 1) or (i > NumWindings) then
        Result := Winding^[1].VBase
    else
        Result := Winding^[i].VBase;
end;

procedure TAutoTransObj.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    inherited GetCurrents(Curr);

    // Combine Series (wdg 1) and Common winding (2) Currents to get X Terminal Currents
    for i := 1 to Fnphases do
        Curr^[i + FnConds] += Curr^[i + Fnphases];
end;

procedure TAutoTransObj.GetLosses(var TotalLosses, LoadLosses, NoLoadLosses: Complex);
var
    cTempIterminal: pComplexArray;
    i: Integer;
begin
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

function TAutoTransObj.RotatePhases(iPhs: Integer): Integer;
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

procedure TAutoTransObj.MakePosSequence();
// Converts default 3-phase AutoTrans model into equivalent positive-sequence
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
    begin //disable if any terminal not connected to phase one
        for iW := 1 to NumWindings do
        begin
            OnPhase1 := FALSE;
            // Load up auxiliary parser
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
            if (NPhases > 1) or (Connection <> TAutoTransConnection.Wye) then
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


procedure TAutoTransObj.BuildYPrimComponent(YPrim_Component, Y_Terminal: TCMatrix);
var
    NW2: Integer;
    i: Integer;
    k: Integer;
    Value: complex;
    j: Integer;
begin
    with YPrim_Component do
    begin
    { Now, Put in Yprim matrix }
    {have to add every element of Y_terminal into Yprim somewhere}
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

function TAutoTransObj.Get_BasekVLL(i: Integer): Double;
begin
    Result := Winding^[i].kVLL;
end;


procedure TAutoTransObj.GICBuildYTerminal;
// Build YTerminal considering only resistance and no coupling to other winding.
var
    i, j, idx: Integer;
    yR: Complex;
    Yadder: Complex;
begin
    Y_Term.Clear;
    Y_Term_NL.Clear;

    for i := 1 to NumWindings do
    begin
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

procedure TAutoTransObj.CalcY_Terminal(FreqMult: Double);
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
    ZCorrected: Double;
    puXst, Vs, Vc: Double;
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
    // check for GIC build
    if ActiveCircuit.Solution.Frequency < 0.51 then
        // Build Yterminal for GIC ~dc simulation
        GICBuildYTerminal
    else
    begin  //Normal Y matrix build

        if XRConst then
            RMult := FreqMult
        else
            RMult := 1.0;

        // Construct ZBMatrix;
        ZB.Clear;
        ZBase := 1.0 / (VABase / Fnphases); // base ohms on 1.0 volt basis

        // Adjust specified XSC by SQR(1 + Vcommon/Vseries)
        ZCorrected := ZBase * SQR(1.0 + Winding^[2].vbase / Winding^[1].Vbase); // Correction factor for Series, as in Dommel (6.45) for Zsc or puXSC^[1]
        // since the losses are correct as they are, mimic Dommel (6.50) for Zst or puXSC^[2], without disturbing Zbase or Zcorrected
        // Dommel: Xst = Xhl*Vh*Vl/(Vh-Vl)^2 + Xht*Vh/(Vh-Vl) - Xlt*Vl/(Vh-Vl)
        //             = Xhl*(Vs+Vc)*Vc/Vs^2 + Xht*(Vs+Vc)/Vs - Xlt*Vc/Vs
        if NumWindings > 2 then
        begin
            Vc := Winding^[2].VBase;
            Vs := Winding^[1].VBase;
            puXst := puXSC^[1] * (Vs + Vc) * Vc / Vs / Vs + puXSC^[2] * (Vs + Vc) / Vs - puXSC^[3] * Vc / Vs;
        end
        else
            puXst := 0.0;

        for i := 1 to Numwindings - 1 do
        begin
            if i = 1 then
                ZB.SetElement(i, i, Cmplx(Rmult * (Winding^[1].Rpu + Winding^[i + 1].Rpu), Freqmult * puXSC^[i]) * ZCorrected)
            else
            if i = 2 then
                ZB.SetElement(i, i, Cmplx(Rmult * (Winding^[1].Rpu + Winding^[i + 1].Rpu), Freqmult * puXst) * Zbase)
            else
                ZB.SetElement(i, i, Cmplx(Rmult * (Winding^[1].Rpu + Winding^[i + 1].Rpu), Freqmult * puXSC^[i]) * Zbase);
        end;

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
                            - Cmplx(Rmult * (Winding^[i + 1].Rpu + Winding^[j + 1].Rpu), Freqmult * puXSC^[k]) * ZBase
                        ) * 0.5
                    );
                    Inc(k);
                end;
            end;

        ZB.Invert;   // mhos on one volt base

        if ZB.InvertError > 0 then
        begin
            DoErrorMsg('TAutoTransObj.CalcYPrim', 
                Format(_('Matrix Inversion Error for AutoTrans "%s"'), [Name]),
                _('Invalid impedance specified. Replaced with tiny conductance to ground.'), 117);
            ZB.Clear;
            for i := 1 to ZB.Order do
                ZB.SetElement(i, i, Cmplx(EPSILON, 0.0));
        end;

        // Now construct Y_Oneturn = AT * ZB.Invert * A
        //      -1 1 0 ...
        //  A = -1 0 1 ..   order:  N-1 x N   N = NumWindings
        //      ...
        //                        -1 -1 ...
        //  AT = Transpose of A =  1  0 ...    N X N-1
        //                         0  1 ..
        //
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

        // should have admittance of one phase of the AutoTrans on a one-volt, wye-connected base

        // Now make into terminal admittance matrix and correct for actual voltage ratings
        // Y_Terminal = AT * Y_onevolt * A  where V_onevolt = A * V_terminal

        AT.Free;

        Y_Term.Clear;
        Y_Term_NL.Clear;
        AT := TcMatrix.Creatematrix(NumWindings * 2);

        // 8/22/2013 Added ZeroTapFix so that regcontrol can set a tap to zero

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
            // Main AutoTrans part
            Y_1Volt.MVmult(ctemparray1, A);
            AT.MVmult(ctemparray2, ctemparray1);    // AT * Result
            for j := 1 to 2 * NumWindings do
                Y_Term.SetElement(j, i, ctemparray2^[j]);
            // No Load part
            Y_1Volt_NL.MVmult(ctemparray1, A);
            AT.MVmult(ctemparray2, ctemparray1);    // AT * Result
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

finalization    AutoTransConnectionEnum.Free;
end.
