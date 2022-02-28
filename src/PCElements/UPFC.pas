unit UPFC;

{
  ----------------------------------------------------------
  Copyright (c) 2021,  Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Classes,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    UComplex, DSSUcomplex,
    Spectrum,
    Arraydef,
    Loadshape,
    XYCurve;

type
{$SCOPEDENUMS ON}
    TUPFCProp = (
        INVALID = 0,
        bus1 = 1,
        bus2 = 2,
        refkv = 3,
        pf = 4,
        frequency = 5,
        phases = 6,
        Xs = 7,
        Tol1 = 8,
        Mode = 9,
        VpqMax = 10,
        LossCurve = 11,
        VHLimit = 12,
        VLLimit = 13,
        CLimit = 14,
        refkv2 = 15,
        kvarLimit = 16
    );
{$SCOPEDENUMS OFF}
    TUPFC = class(TPCClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TUPFCObj = class(TPCElement)
    PRIVATE
        VRef: Double; //Expected vooltage in the output (only magnitude)
        pf: Double; //Expected power factor (under revision)
        Xs: Double; //Impedance of the series Xfmr
        Sr0: pComplexArray; //Shift register for controller 1
        Sr1: pComplexArray; //Shift register for controller 2
        Vbin: Complex; // Voltage at the input of the device
        Vbout: Complex; // Voltage at the output of the device
        Tol1: Double;   //Tolerance (dead band) specified for the controller 1
        ERR0: array[1..6] of Double; //Error controller 1 for Dual mode
        ZBase: Double;
        Freq: Double;
        VHLimit: Double;   // High limit for the input voltage in volts (default 300V)
        VLLimit: Double;   // Low limit for the input voltage in volts (default 125V)
        CLimit: Double;   // Limit for the maximum current in amperes
        UPFCON: Boolean;   // Flag to indicate when the UPFC operation is out of boundaries
        VRef2: Double;   // Value for deadband's upper limit, it is calculated if tolerance is specified
        VRefD: Double;   // Dynamic reference for control modes 4 and 5
        KVARLim: Double;   // kvar limit, defines the maximum amount of kvars that the UPFC can absorb


        // some state vars for reporting
        Losses: Double;
        IUPFC: complex;
        UPFC_Power: Complex;
        QIdeal: Double;

        ModeUPFC: Integer;
        Vpqmax: Double;
        SyncFlag: Boolean;   // Flag used to synchronize controllers in Dual mode
        SF2: Boolean;   // Flag used to Synch control modes 4 and 5

        UPFCLossCurveObj: TXYCurveObj; //Losses curve reference

        function GetinputCurr(Cond: Integer): Complex;
        function GetOutputCurr(Cond: Integer): Complex;
        function CalcUPFCPowers(ModeUP, Cond: Integer): Complex;
        function CalcUPFCLosses(Vpu: Double): Double;
        procedure GetInjCurrents(Curr: pComplexArray);

    PROTECTED
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;

    PUBLIC
        InCurr, OutCurr: Array of Complex; // for storing the input and output currents

        Z: TCmatrix;  // Base Frequency Series Z matrix
        Zinv: TCMatrix;
        VMag: Double;

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        function InjCurrents: Integer; OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE;

        // Uploads the input/output currents when commanded by the controller - 09/02/2021
        procedure UploadCurrents;
        function CheckStatus: Boolean;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;

        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;

        function VariableName(i: Integer): String; OVERRIDE;

    end;

implementation

uses
    Circuit,
    UPFCControl,
    DSSClassDefs,
    DSSGlobals,
    Dynamics,
    Utilities,
    Sysutils,
    Command,
    solution,
    YMatrix,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TUPFCObj;
    TProp = TUPFCProp;
const
    NumPropsThisClass = Ord(High(TProp));
    NumUPFCVariables = 14;
var
    PropInfo: Pointer = NIL;    

constructor TUPFC.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, UPFC_ELEMENT, 'UPFC');
end;

destructor TUPFC.Destroy;
begin
    inherited Destroy;
end;

procedure TUPFC.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyType[ord(TProp.bus2)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;
    PropertyOffset[ord(TProp.bus2)] := 2;

    // object properties
    PropertyType[ord(TProp.LossCurve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.LossCurve)] := ptruint(@obj.UPFCLossCurveObj);
    PropertyOffset2[ord(TProp.LossCurve)] := ptruint(DSS.XYCurveClass);

    // integer properties
    PropertyType[ord(TProp.Mode)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Mode)] := ptruint(@obj.ModeUPFC);
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    // double properties (default type)
    PropertyOffset[ord(TProp.refkv)] := ptruint(@obj.VRef);
    PropertyOffset[ord(TProp.pf)] := ptruint(@obj.pf);
    PropertyOffset[ord(TProp.frequency)] := ptruint(@obj.Freq);
    PropertyOffset[ord(TProp.Xs)] := ptruint(@obj.Xs);
    PropertyOffset[ord(TProp.Tol1)] := ptruint(@obj.Tol1);
    PropertyOffset[ord(TProp.VpqMax)] := ptruint(@obj.VpqMax);
    PropertyOffset[ord(TProp.VHLimit)] := ptruint(@obj.VHLimit);
    PropertyOffset[ord(TProp.VLLimit)] := ptruint(@obj.VLLimit);
    PropertyOffset[ord(TProp.CLimit)] := ptruint(@obj.CLimit);
    PropertyOffset[ord(TProp.refkv2)] := ptruint(@obj.VRef2);
    PropertyOffset[ord(TProp.kvarLimit)] := ptruint(@obj.kvarLim);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TUPFC.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TUPFCObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of
        ord(TProp.phases):
            if Fnphases <> previousIntVal then
            begin
                NConds := Fnphases;  // Force Reallocation of terminal info
                SetLength(OutCurr, FNphases + 1);
                SetLength(InCurr, FNphases + 1);
            end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TUPFC.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        RecalcElementData;
        YPrimInvalid := TRUE;
        Exclude(Flags, Flg.EditionActive);
    end;
    Result := True;
end;

procedure TUPFCObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);

    Other := TObj(OtherPtr);
    if Fnphases <> Other.Fnphases then
    begin
        FNphases := Other.Fnphases;
        NConds := Fnphases;  // Forces reallocation of terminal stuff

        Yorder := Fnconds * Fnterms;
        YPrimInvalid := TRUE;

        if Z <> NIL then
            Z.Free;
        if Zinv <> NIL then
            Zinv.Free;

        Z := TCmatrix.CreateMatrix(Fnphases);
        Zinv := TCMatrix.CreateMatrix(Fnphases);
    end;

    Z.CopyFrom(Other.Z);
    VRef := Other.VRef;
    pf := Other.pf;
    Xs := Other.Xs;
    Tol1 := Other.Tol1;
    ZBase := Other.ZBase;
    Freq := Other.Freq;
    ModeUPFC := Other.ModeUPFC;
    VpqMax := Other.VpqMax;
    UPFCLossCurveObj := UPFCLossCurveObj;
    VHLimit := Other.VHLimit;
    VLLimit := Other.VLLimit;
    CLimit := Other.CLimit;
    VRef2 := Other.VRef2;
    kvarLim := Other.kvarLim;
end;

constructor TUPFCObj.Create(ParClass: TDSSClass; const SourceName: String);
var
    i: Integer;
    ctrl: TUPFCControlObj;
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; //SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

    FNphases := 1;
    Fnconds := 1;   // number conductors per terminal
    Nterms := 2;   // A 2-terminal device

    Z := NIL;
    Zinv := NIL;
    VRef := 0.24;
    pf := 1.0;
    Xs := 0.7540; // Xfmr series inductace 2e-3 H
    Tol1 := 0.02;
    Freq := Round(ActiveCircuit.Fundamental);
    enabled := TRUE;
    ModeUPFC := 1;
    VpqMax := 24.0;     // From the data provided
    UPFCLossCurveObj := NIL;
    VHLimit := 300.0;
    VLLimit := 125.0;
    CLimit := 265.0;
    UPFCON := TRUE;
    Sr0 := NIL;
    Sr1 := NIL;
    VRef2 := 0.0;
    kvarLim := 5;

    QIdeal := 0.0;

     // Initialize shift registers
    Reallocmem(SR0, SizeOf(Sr0^[1]) * Fnphases);
    Reallocmem(SR1, SizeOf(Sr1^[1]) * Fnphases);
    for i := 1 to FNphases do
        Sr0^[i] := CZERO; //For multiphase model
    for i := 1 to FNphases do
        Sr1^[i] := CZERO; //For multiphase model
    for i := 1 to FNphases do
        ERR0[i] := 0; //For multiphase model

    SetLength(OutCurr, FNphases + 1);
    SetLength(InCurr, FNphases + 1);
    for i := 0 to FNphases do
    begin
        OutCurr[i] := CZERO; //For multiphase model
        InCurr[i] := CZERO; //For multiphase model
    end;

    // If there is a controller, sets the flag for it to consider de new UPFC
    if ParentClass.ElementCount > 0 then
    begin
        ctrl := ParentClass.ElementList.Get(1);
        ctrl.UPFCList.Clear;
        ctrl.ListSize := 0;
    end;

    Yorder := Fnterms * Fnconds;
    RecalcElementData;
end;

destructor TUPFCObj.Destroy;
begin
    Z.Free;
    Zinv.Free;

    Reallocmem(SR0, 0);
    Reallocmem(SR1, 0);

    inherited Destroy;
end;

procedure TUPFCObj.RecalcElementData;
var
    Z1: Complex;
    Value: Complex;
    i: Integer;
begin
    if Z <> NIL then
        Z.Free;
    if Zinv <> NIL then
        Zinv.Free;

    // For a Source, nphases = ncond, for now
    Z := TCmatrix.CreateMatrix(Fnphases);
    Zinv := TCMatrix.CreateMatrix(Fnphases);

    Qideal := 0.0;

    // Update property Value array
    // Don't change a specified value; only computed ones

    Z1 := Cmplx(0, Xs);
     // Diagonals  (all the same)
    Value := Z1;
    for i := 1 to Fnphases do
        Z.SetElement(i, i, Value);

    Reallocmem(SR0, SizeOf(Sr0^[1]) * Fnphases);
    Reallocmem(SR1, SizeOf(Sr1^[1]) * Fnphases);

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);
end;


procedure TUPFCObj.CalcYPrim;

var
    Value: Complex;
    i, j: Integer;
    FreqMultiplier: Double;

begin
    // Calc UPFC Losses
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

    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

    // Put in Series RL Adjusted for frequency
    for i := 1 to Fnphases do
    begin
        for j := 1 to Fnphases do
        begin
            Value := Z.GetElement(i, j);
            Value.im := Value.im * FreqMultiplier; // Modify from base freq
            Zinv.SetElement(i, j, value);
        end;
    end;
    Zinv.Invert; // Invert in place

    if Zinv.InvertError > 0 then
    begin // If error, put in Large series conductance
        DoErrorMsg('TUPFCObj.CalcYPrim', 
            Format(_('Matrix Inversion Error for UPFC "%s"'), [Name]),
            _('Invalid impedance specified. Replaced with small resistance.'), 325);
        Zinv.Clear;
        for i := 1 to Fnphases do
            Zinv.SetElement(i, i, Cmplx(1.0 / EPSILON, 0.0));
    end;

   // YPrim_Series.CopyFrom(Zinv);

    for i := 1 to FNPhases do
    begin
        for j := 1 to FNPhases do
        begin
            Value := Zinv.GetElement(i, j);
            YPrim_series.SetElement(i, j, Value);
            YPrim_series.SetElement(i + FNPhases, j + FNPhases, Value);
            //YPrim_series.SetElemsym(i + FNPhases, j, -Value)
            YPrim_series.SetElement(i, j + Fnphases, -Value);
            YPrim_series.SetElement(i + Fnphases, j, -Value);
        end;
    end;

    YPrim.CopyFrom(YPrim_Series);

    // Now Account for Open Conductors
    // For any conductor that is open, zero out row and column
    inherited CalcYPrim;

    YPrimInvalid := FALSE;
end;

function TUPFCObj.CalcUPFCLosses(Vpu: Double): Double;
begin
    //  Calculates the Active power losses at the input of the device
    //  By using the Load powers, the approach is based in the data provided
    Result := UPFCLossCurveObj.GetYValue(Vpu);
end;

function TUPFCObj.InjCurrents: Integer;
begin
    GetInjCurrents(InjCurrent);
    // This is source injection
    Result := inherited InjCurrents; // Add into system array
end;

//Taken from ISources due to the kind of model

//Calculates the output current for the UPFC device

//        Vbin   Xs  Vbout
//     <---*--=======--*--->
//         |           |
// I input ^           ^ I output
//         |           |
//
//  4 modes of operation:
//  mode 0: UPFC Off
//  mode 1: UPFC in voltage regulation mode
//  mode 2: UPFC in reactive power compensation mode
//  mode 3: Mode 1 and 2 working together
function TUPFCObj.GetoutputCurr(Cond: Integer): Complex;

var
    Error: Double;
    TError: Double;
    VinMag: Double;
    RefH: Double;
    RefL: Double;
    Vpolar: polar;
    VTemp: complex;
    CurrOut: complex;


begin
    try
        with ActiveCircuit.Solution do
            UPFCON := TRUE;
        VinMag := cabs(Vbin);
        if (VinMag > VHLimit) or (VinMag < VLLimit) then
        begin   // Check Limits (Voltage)
            UPFCON := FALSE;
            CurrOut := cmplx(0, 0);
        end
        else                                                       // Limits OK
        begin
            case ModeUPFC of
                0:
                    CurrOut := cmplx(0, 0); //UPFC off
                1:
                begin              //UPFC as a voltage regulator
                    Vpolar := ctopolar(Vbout);
                    Error := abs(1 - abs(Vpolar.mag / (VRef * 1000)));
                    if Error > Tol1 then
                    begin
                        Vtemp := Vbout - Vbin;
                        Vpolar := ctopolar(Vbin);
                        TError := (VRef * 1000) - Vpolar.mag;
                        if TError > VpqMax then
                            TError := VpqMax
                        else
                        if TError < -VpqMax then
                            TError := -VpqMax;
                        Vpolar := topolar(TError, Vpolar.ang);
                        VTemp := ptocomplex(Vpolar) - VTemp; //Calculates Vpq
                        CurrOut := SR0^[Cond] + VTemp / cmplx(0, Xs);
                        SR0^[Cond] := CurrOut;
                    end
                    else
                    begin
                        CurrOut := SR0^[Cond];
                    end;
                end;
                2:
                    CurrOut := cmplx(0, 0); //UPFC as a phase angle regulator
                3:
                begin              //UPFC in Dual mode Voltage and Phase angle regulator
                    Vpolar := ctopolar(Vbout);
                    Error := abs(1 - abs(Vpolar.mag / (VRef * 1000)));
                    if Error > Tol1 then
                    begin
                        Vtemp := Vbout - Vbin;
                        Vpolar := ctopolar(Vbin);
                        TError := (VRef * 1000) - Vpolar.mag;
                        if TError > VpqMax then
                            TError := VpqMax
                        else
                        if TError < -VpqMax then
                            TError := -VpqMax;
                        Vpolar := topolar(TError, Vpolar.ang);
                        VTemp := ptocomplex(Vpolar) - VTemp; //Calculates Vpq
                        CurrOut := SR0^[Cond] + VTemp / cmplx(0, Xs);
                        SR0^[Cond] := CurrOut;
                        SyncFlag := FALSE;
                    end
                    else
                    begin
                        CurrOut := SR0^[Cond];
                        SyncFlag := TRUE;
                    end;
                end;
                4:
                begin                // Double reference control mode (only voltage control)
                    Vpolar := ctopolar(Vbin);       // Takes the input voltage to verify the operation
                    // Verifies if the Voltage at the input is out of the gap defined with VRef and VRef2
                    RefH := (VRef * 1000) + (VRef * 1000 * Tol1);
                    RefL := (VRef2 * 1000) - (VRef2 * 1000 * Tol1);
                    if (Vpolar.mag > RefH) or (Vpolar.mag < RefL) then
                    begin
                        // Sets the New reference by considering the value at the input of the device
                        if (Vpolar.mag > RefH) then
                            VRefD := VRef
                        else
                        if (Vpolar.mag < RefL) then
                            VRefD := VRef2;
                        // Starts the control routine for voltage control only
                        Vpolar := ctopolar(Vbout);
                        Error := abs(1 - abs(Vpolar.mag / (VRefD * 1000)));
                        if Error > Tol1 then
                        begin
                            Vtemp := Vbout - Vbin;
                            Vpolar := ctopolar(Vbin);
                            TError := (VRefD * 1000) - Vpolar.mag;
                            if TError > VpqMax then
                                TError := VpqMax
                            else
                            if TError < -VpqMax then
                                TError := -VpqMax;
                            Vpolar := topolar(TError, Vpolar.ang);
                            VTemp := ptocomplex(Vpolar) - VTemp; //Calculates Vpq
                            CurrOut := SR0^[Cond] + VTemp / cmplx(0, Xs);
                            SR0^[Cond] := CurrOut;
                        end
                        else
                        begin
                            CurrOut := SR0^[Cond];
                        end;
                        SF2 := TRUE;   // Normal control routine
                    end
                    else
                    begin
                        CurrOut := cmplx(0, 0); //UPFC off
                        SR0^[Cond] := CurrOut;
                        SF2 := FALSE;   // Says to the other controller to do nothing
                    end;
                end;
                5:
                begin                // Double reference control mode (Dual mode)
                    Vpolar := ctopolar(Vbin);       // Takes the input voltage to verify the operation
                    // Verifies if the Voltage at the input is out of the gap defined with VRef and VRef2
                    RefH := (VRef * 1000) + (VRef * 1000 * Tol1);
                    RefL := (VRef2 * 1000) - (VRef2 * 1000 * Tol1);
                    if (Vpolar.mag > RefH) or (Vpolar.mag < RefL) then
                    begin
                        // Sets the New reference by considering the value at the input of the device
                        if (Vpolar.mag > RefH) then
                            VRefD := VRef
                        else
                        if (Vpolar.mag < RefL) then
                            VRefD := VRef2;
                        // Starts standard control (the same as Dual control mode)
                        Vpolar := ctopolar(Vbout);
                        Error := abs(1 - abs(Vpolar.mag / (VRefD * 1000)));
                        if Error > Tol1 then
                        begin
                            Vtemp := Vbout - Vbin;
                            Vpolar := ctopolar(Vbin);
                            TError := (VRefD * 1000) - Vpolar.mag;
                            if TError > VpqMax then
                                TError := VpqMax
                            else
                            if TError < -VpqMax then
                                TError := -VpqMax;
                            Vpolar := topolar(TError, Vpolar.ang);
                            VTemp := ptocomplex(Vpolar) - VTemp; //Calculates Vpq
                            CurrOut := SR0^[Cond] + VTemp / cmplx(0, Xs);
                            SR0^[Cond] := CurrOut;
                            SyncFlag := FALSE;
                        end
                        else
                        begin
                            CurrOut := SR0^[Cond];
                            SyncFlag := TRUE;
                        end;
                        SF2 := TRUE;   // Normal control routine
                    end
                    else
                    begin
                        CurrOut := cmplx(0, 0); //UPFC off
                        SR0^[Cond] := CurrOut;
                        SF2 := FALSE;   // Says to the other controller to do nothing
                        SyncFlag := FALSE;
                    end;
                end
            else
                DoSimpleMsg(_('Control mode not recognized for UPFC'), 790);
            end;
        end;
        Result := CurrOut;
    except
        DoSimpleMsg('Error computing current for "%s". Check specification. Aborting.', [FullName], 334);
        if DSS.In_Redirect then
            DSS.Redirect_Abort := TRUE;
    end;
end;

function TUPFCObj.CalcUPFCPowers(ModeUP, Cond: Integer): Complex;
begin
    case ModeUP of
        1:
        begin                                                //Dual mode
            IUPFC := (Vbout - Vbin) / cmplx(0, Xs);
//            SOut=cmul(Vbout,cong(cadd(IUPFC,SR0[Cond])))     // Just if you want to know the power at the output
            Result := -Vbin * cong(IUPFC + SR1^[Cond]);
        end;
        2:
        begin                                              //StatCOM
            IUPFC := (Vbin - Vbout) / cmplx(0, Xs);
            Result := Vbin * cong(IUPFC);
        end;
    end;
end;

//Calculates the input current to absorb reactive power from UPFC
//
//        Vbin   Xs  Vbout
//     <---*--=======--*--->
//         |           |
// I input ^           ^ I output
//         |           |
function TUPFCObj.GetinputCurr(Cond: Integer): Complex;
var
    CurrIn: complex;
    S: Double;
begin
    try
        with ActiveCircuit.Solution do
            // Get first Phase Current
            if UPFCON then
            begin
                case ModeUPFC of
                    0:
                    begin
                        CurrIn := cmplx(0, 0);
                        UPFC_Power := CZERO;
                    end;
                    1:
                    begin                     // Voltage regulation mode
                        CurrIn := CZERO;
                        // Ctemp := cong(cmul(cdiv(Vbout, Vbin), cong(SR0^[Cond]))); //Balancing powers
                        Losses := CalcUPFCLosses(Cabs(Vbin) / (VRef * 1000));
                        // CurrIn := -cmplx((Ctemp.re * Losses), SR0^[Cond].im);
                        CurrIn := -cmplx(Losses * SR0^[Cond].re, SR0^[Cond].im);
                        SR1^[Cond] := CurrIn;
                    end;
                    2:
                    begin                    // Reactive compensation mode
                        UPFC_Power := CalcUPFCPowers(2, 0);
                        S := abs(UPFC_Power.re) / pf;
                        QIdeal := UPFC_Power.im - sqrt(1 - pf * pf) * S;   //This is the expected compensating reactive power
                        if (QIdeal > (kvarLim * 1000)) then
                            QIdeal := kvarLim * 1000;
                        CurrIn := cong(cmplx(0, QIdeal) / Vbin); //Q in terms of current  *** cong
                    end;
                    3:
                    begin                    // Dual mode
                        CurrIn := CZERO;
                        // Ctemp := cong(cmul(cdiv(Vbout, Vbin), cong(SR0^[Cond]))); //Balancing powers
                        Losses := CalcUPFCLosses(Cabs(Vbin) / (VRef * 1000));
                        // CurrIn := -cmplx((Ctemp.re * Losses), SR0^[Cond].im);
                        CurrIn := -cmplx(Losses * SR0^[Cond].re, SR0^[Cond].im);
                        SR1^[Cond] := CurrIn;
                        if SyncFlag then
                        begin
                    // Starts Power Calculations to copensate the reactive power
                            UPFC_Power := CalcUPFCPowers(1, Cond);
                            S := abs(UPFC_Power.re) / pf;
                            QIdeal := UPFC_Power.im - sqrt(1 - pf * pf) * S;   //This is the expected compensating reactive power
                            if (QIdeal > (kvarLim * 1000)) then
                                QIdeal := kvarLim * 1000;
                            CurrIn := cong(cmplx(0, QIdeal) / Vbin) + SR1^[Cond]; //Q in terms of current  *** cong
                    // This partial result is added to the one obtained previously to balance the control loop
                        end;
                    end;
                    4:
                    begin                   // Two band reference Mode   (Only Voltage control mode)
                        if SF2 then
                        begin    // Normal control routine considering the dynamic reference
                            CurrIn := CZERO;
                            // Ctemp := cong(cmul(cdiv(Vbout, Vbin), cong(SR0^[Cond]))); //Balancing powers
                            Losses := CalcUPFCLosses(Cabs(Vbin) / (VRefD * 1000));
                            // CurrIn := -cmplx((Ctemp.re * Losses), SR0^[Cond].im);
                            CurrIn := -cmplx(Losses * SR0^[Cond].re, SR0^[Cond].im);
                            SR1^[Cond] := CurrIn;
                        end
                        else
                        begin   // Do nothing, aparently the input voltage is OK
                            CurrIn := cmplx(0, 0);
                            SR0^[Cond] := CurrIn;
                            UPFC_Power := CZERO;
                        end;
                    end;
                    5:
                    begin                    // Two band reference mode (Dual control mode)
                        if SF2 then
                        begin
                            CurrIn := CZERO;
                            // Ctemp := cong(cmul(cdiv(Vbout, Vbin), cong(SR0^[Cond]))); //Balancing powers
                            Losses := CalcUPFCLosses(Cabs(Vbin) / (VRefD * 1000));
                            // CurrIn := -cmplx((Ctemp.re * Losses), SR0^[Cond].im);
                            CurrIn := -cmplx(Losses * SR0^[Cond].re, SR0^[Cond].im);
                            SR1^[Cond] := CurrIn;
                        end
                        else
                        begin   // Do nothing, aparently the input voltage is OK
                            CurrIn := CZERO;
                            SR1^[Cond] := CurrIn;
                            UPFC_Power := CZERO;
                        end;
                        //Always corrects PF
                        if SyncFlag then
                        begin
                            // Starts Power Calculations to compensate the reactive power
                            UPFC_Power := CalcUPFCPowers(1, Cond);
                            S := abs(UPFC_Power.re) / pf;
                            QIdeal := UPFC_Power.im - sqrt(1 - pf * pf) * S;   //This is the expected compensating reactive power
                            if (QIdeal > (kvarLim * 1000)) then
                                QIdeal := kvarLim * 1000;
                            CurrIn := cong(cmplx(0, QIdeal) / Vbin) + SR1^[Cond]; //Q in terms of current  *** cong
                            // This partial result is added to the one obtained previously to balance the control loop
                        end;
                    end;
                end;
            end
            else
                CurrIn := cmplx(0, 0);
        Result := CurrIn;
    except
        DoSimpleMsg('Error computing current for "%s". Check specification. Aborting.', [FullName], 334);
        if DSS.In_Redirect then
            DSS.Redirect_Abort := TRUE;
    end;
end;

procedure TUPFCObj.GetInjCurrents(Curr: pComplexArray);
// Fill Up an array of injection currents
var
    i: Integer;
begin
    with ActiveCircuit.solution do
    begin
        for i := 1 to fnphases do
        begin
            Vbin := NodeV^[NodeRef^[i]];           //Gets voltage at the input of UPFC Cond i
            Vbout := NodeV^[NodeRef^[i + fnphases]]; //Gets voltage at the output of UPFC Cond i

            // These functions were modified to follow the UPFC Dynamic
            // (Different from VSource)
            Curr^[i + fnphases] := OutCurr[i];
            Curr^[i] := InCurr[i];
        end;
    end;
end;

// Checks if the UPFC control needs an update, returns true if so
function TUPFCObj.CheckStatus: Boolean;
var
    Error,
    VinMag,
    RefH,
    RefL: Double;
    Vpolar: polar;
    // CurrOut: Complex;
begin
    Result := FALSE;
  
    UPFCON := True;
    VinMag := cabs(Vbin);

    if (VinMag > VHLimit) or (VinMag < VLLimit) then
    begin   // Check Limits (Voltage)
        UPFCON := False;
        //CurrOut := cmplx(0,0);
        Exit;
    end;

    // Limits OK
    case ModeUPFC of
        0:
            // CurrOut := cmplx(0,0); //UPFC off
            ;
        1:  
        begin //UPFC as a voltage regulator
            Vpolar := ctopolar(Vbout);
            Error := abs(1 - abs(Vpolar.mag / (VRef * 1000)));
            if Error > Tol1 then 
                Result := True;
        end;
        2:  
            // CurrOut := cmplx(0,0); //UPFC as a phase angle regulator
            ;
        3:
        begin //UPFC in Dual mode Voltage and Phase angle regulator
            Vpolar := ctopolar(Vbout);
            Error := abs(1 - abs(Vpolar.mag / (VRef * 1000)));
            if Error > Tol1 then 
                Result := TRUE;
        end;
        4:
        begin // Double reference control mode (only voltage control)
            Vpolar := ctopolar(Vbin); // Takes the input voltage to verify the operation
            
            // Verifies if the Voltage at the input is out of the gap defined with VRef and VRef2
            RefH := (VRef*1000) + (VRef*1000*Tol1);
            RefL := (VRef2*1000) - (VRef2*1000*Tol1);
            if (Vpolar.mag > RefH) or (Vpolar.mag < RefL) then
            begin
                // Sets the New reference by considering the value at the input of the device
                if (Vpolar.mag > RefH) then 
                    VRefD:=VRef
                else if (Vpolar.mag < RefL) then 
                    VRefD:=VRef2;

                // Starts the control routine for voltage control only
                Vpolar := ctopolar(Vbout);
                Error := abs(1 - abs(Vpolar.mag / (VRefD*1000)));
                if Error > Tol1 then  
                    Result  :=  True;
            end
        end;
        5:  
        begin // Double reference control mode (Dual mode)
            Vpolar := ctopolar(Vbin); // Takes the input voltage to verify the operation
            
            // Verifies if the Voltage at the input is out of the gap defined with VRef and VRef2
            RefH := (VRef * 1000) + (VRef * 1000 * Tol1);
            RefL := (VRef2 * 1000) - (VRef2 * 1000 * Tol1);
            if (Vpolar.mag > RefH) or (Vpolar.mag < RefL) then
            begin
                // Sets the New reference by considering the value at the input of the device
                if (Vpolar.mag > RefH) then 
                    VRefD:=VRef
                else if (Vpolar.mag < RefL) then
                    VRefD:=VRef2;

                // Starts standard control (the same as Dual control mode)
                Vpolar := ctopolar(Vbout);
                Error := abs(1-abs(Vpolar.mag/(VRefD*1000)));
                if Error > Tol1 then 
                    Result :=  True;   // In case we need a control action
            end
        end
    end;
end;

// Uploads the calculated currents into memeory for further use
procedure TUPFCObj.UploadCurrents;
var
   i: Integer;
begin
    for i := 1 to fnphases do
    begin
        OutCurr[i] := GetOutputCurr(i);
        InCurr[i] := GetInputCurr(i);
    end;
end;

procedure TUPFCObj.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    try
        with ActiveCircuit.Solution do
        begin
            ComputeVTerminal;

            YPrim.MVMult(Curr, Vterminal);  // Current from Elements in System Y

            GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
            // Add Together  with yprim currents
            for i := 1 to Yorder do
                Curr^[i] := Curr^[i] - ComplexBuffer^[i];
        end;
    except
        On E: Exception do
            DoErrorMsg(Format(_('GetCurrents for Element: %s.'), [FullName]), E.Message,
                _('Inadequate storage allotted for circuit element.'), 327);
    end;
end;

procedure TUPFCObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);
var
    i, j: Integer;
    c: Complex;
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
        FSWriteln(F, Format('BaseFrequency=%.1f', [BaseFrequency]));
        // FSWriteln(F,'VMag=',VMag:0:2);
        FSWriteln(F, 'Z Matrix=');
        for i := 1 to Fnphases do
        begin
            for j := 1 to i do
            begin
                c := Z.GetElement(i, j);
                FSWrite(F, Format('%.8g +j %.8g ', [C.re, C.im]));
            end;
            FSWriteln(F);
        end;
    end;
end;

procedure TUPFCObj.MakePosSequence();
begin
end;

function TUPFCObj.NumVariables: Integer;
begin
    Result := NumUPFCVariables;
end;

procedure TUPFCObj.Set_Variable(i: Integer; Value: Double);
begin
  // inherited;

    case i of
        1:
            ModeUPFC := round(Value);
        2: ; // can't set this one  -readonly
        3: ; // can't set this one  -readonly
        4: ; // can't set this one  -readonly
        5: ; // can't set this one  -readonly
        6: ; // can't set this one  -readonly
        7: ; // can't set this one  -readonly
        8: ; // can't set this one  -readonly
        9: ; // can't set this one  -readonly
        10: ; // can't set this one  -readonly
        11:
            Sr0^[1].re := Value;
        12:
            Sr0^[1].im := Value;
        13:
            Sr1^[1].re := Value;
        14:
            Sr1^[1].im := Value;
    end;
end;

function TUPFCObj.Get_Variable(i: Integer): Double;
begin
    Result := -1.0;
    case i of
        1:
            Result := ModeUPFC;
        2:
            Result := Cabs(IUPFC);
        3:
            Result := Vbin.re;
        4:
            Result := Vbin.im;
        5:
            Result := Vbout.re;
        6:
            Result := Vbout.im;
        7:
            Result := Losses;
        8:
            Result := UPFC_Power.re;
        9:
            Result := UPFC_Power.im;
        10:
            Result := QIdeal;
        11:
            Result := SR0^[1].re;
        12:
            Result := SR0^[1].im;
        13:
            Result := SR1^[1].re;
        14:
            Result := SR1^[1].im;
    end;
end;

procedure TUPFCObj.GetAllVariables(States: pDoubleArray);
var
    i: Integer;
begin
    for i := 1 to NumUPFCVariables do
        States^[i] := Variable[i];
end;

function TUPFCObj.VariableName(i: Integer): String;
begin
    if i < 1 then
        Exit;  // Someone goofed

    case i of
        1:
            Result := 'ModeUPFC';
        2:
            Result := 'IUPFC';
        3:
            Result := 'Re{Vbin}';
        4:
            Result := 'Im{Vbin}';
        5:
            Result := 'Re{Vbout}';
        6:
            Result := 'Im{Vbout}';
        7:
            Result := 'Losses';
        8:
            Result := 'P_UPFC';
        9:
            Result := 'Q_UPFC';
        10:
            Result := 'Qideal';
        11:
            Result := 'Re{Sr0^[1]}';
        12:
            Result := 'Im{Sr0^[1]}';
        13:
            Result := 'Re{Sr1^[1]}';
        14:
            Result := 'Im{Sr1^[1]}';
    end;
end;

end.