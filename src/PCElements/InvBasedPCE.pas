unit InvBasedPCE;

// InvBasedPCE is an abstract class for grouping inverter based functions.
// In the upstream OpenDSS, this is included in the base PCElement, 
// but it feels better to create a dedicated class, even in the restrictive
// environment of the Object Pascal language.

interface

uses
    DSSClass,
    DynEqPCE,
    InvDynamics,
    UComplex,
    LoadShape,
    XYCurve,
    mathutil,
    Classes,
    Ucmatrix,
    DSSUcomplex;

type
    TInvBasedPCEClass = class(TDynEqPCEClass)
    PROTECTED
        // procedure DefineProperties; override;

    PUBLIC
        constructor Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
        destructor Destroy; OVERRIDE;
    end;

    TInvBasedPCE = class(TDynEqPCE)
    PUBLIC
        dynVars: TInvDynamicVars;
        GFM_Mode: LongBool; // To indicate if the PCE (normally IBR) is working in Grod forming inverter mode
        InverterON: Boolean;
        varMode: Integer; // 0=constant PF; 1=kvar specified
        VWMode: Boolean; //boolean indicating if under volt-watt control mode from InvControl (not ExpControl)
        VVMode: Boolean; //boolean indicating if under volt-var mode from InvControl
        WVMode: Boolean; //boolean indicating if under watt-var mode from InvControl
        WPMode: Boolean; //boolean indicating if under watt-pf mode from InvControl
        DRCMode: Boolean; //boolean indicating if under DRC mode from InvControl
        AVRMode: Boolean; //boolean indicating whether under AVR mode from ExpControl (or InvControl, but that does not seem to be implemented yet)
        
        VBase: Double;  // Base volts suitable for computing currents
        Vmaxpu: Double;
        Vminpu: Double;
        kvar_out: Double;
        kW_out: Double;
        pctR: Double;
        pctX: Double;
        Pnominalperphase: Double;
        Qnominalperphase: Double;

        Reg_Hours: Integer;
        Reg_kvarh: Integer;
        Reg_kWh: Integer;
        Reg_MaxkVA: Integer;
        Reg_MaxkW: Integer;
        Reg_Price: Integer;
        ShapeFactor: Complex;

        RandomMult: Double;

        Connection: Integer;  // 0 = line-neutral; 1=Delta

        DailyShapeObj: TLoadShapeObj;  // Daily Storage element Shape for this load
        DutyShapeObj: TLoadShapeObj;  // Shape for this Storage element
        YearlyShapeObj: TLoadShapeObj;  // Shape for this Storage element
        // Inverter efficiency curve
        InverterCurveObj: TXYCurveObj;

        // Variables for Inverter functionalities
        FpctCutIn: Double;
        FpctCutOut: Double;
        CutInkW: Double;
        CutOutkW: Double;
        CurrentkvarLimit: Double;
        CurrentkvarLimitNeg: Double;
        VoltageModel: Integer;   // Variation with voltage
        PFNominal: Double;
        PICtrl: Array of TPICtrl;
        VarFollowInverter: LongBool;

        YEQ: Complex;   // at nominal
        YEQ_Min: Complex;   // at Vmin
        YEQ_Max: Complex;   // at VMax
        VBaseMax: Double;
        VBaseMin: Double;

        kvarLimitSet: Boolean;
        kvarLimitNegSet: Boolean;
        PhaseCurrentLimit: Complex;
        DebugTrace: LongBool;

        FpctPminNoVars: Double;
        FpctPminkvarLimit: Double;
        PminNoVars: Double;
        PminkvarLimit: Double;
        pf_wp_nominal: Double;

        ForceBalanced: LongBool;
        CurrentLimited: LongBool;
        UserModelNameStr, UserModelEditStr: String;

        YPrimOpenCond: TCmatrix;
        Tracefile: TFileStream;
        FirstSampleAfterReset: Boolean;

        constructor Create(ParClass: TDSSClass);
        destructor Destroy; OVERRIDE;

        function IsPVSystem(): Boolean; virtual;
        function IsStorage(): Boolean; virtual;
        function GetPFPriority(): Boolean; virtual;
        procedure SetPFPriority(value: Boolean); virtual; abstract;
        function CheckOLInverter(): Boolean; virtual; abstract;
        function UsingCIMDynamics(): Boolean;
        function CheckAmpsLimit(): Boolean;
        procedure Randomize(Opt: Integer);
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE;
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);
    end;

implementation

uses
    PCElement,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    DSSHelper,
    SysUtils;

constructor TInvBasedPCEClass.Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
begin
    inherited Create(dssContext, DSSClsType, DSSClsName);
end;

destructor TInvBasedPCEClass.Destroy;
begin
    inherited Destroy;
end;

constructor TInvBasedPCE.Create(ParClass: TDSSClass);
begin
    inherited Create(ParClass);

    GFM_Mode := FALSE;

    with dynVars do
    begin
        ILimit := -1; // No Amps limit
        IComp := 0;
        VError := 0.8;
    end;
    RandomMult := 1.0;

    YearlyShapeObj := NIL;
    DailyShapeObj := NIL;
    DutyShapeObj := NIL;

    InverterCurveObj := NIL;
end;

destructor TInvBasedPCE.Destroy;
begin
    inherited Destroy;
end;

function TInvBasedPCE.IsPVSystem(): Boolean;
begin
    Result := False;
end;

function TInvBasedPCE.IsStorage(): Boolean;
begin
    Result := False;
end;

function TInvBasedPCE.GetPFPriority(): Boolean;
begin
    Result := False;
end;

function TInvBasedPCE.UsingCIMDynamics(): Boolean;
begin
    Result := VWMode or VVMode or WVMode or AVRMode or DRCMode; // WPMode not in CIM Dynamics
end;

function TInvBasedPCE.CheckAmpsLimit(): Boolean;
var
    curr: Complex;
    volts, NomP, PhaseP, PhaseAmps: Double;
    i: Integer;
begin
    // Check if reaching saturation point in GFM
    Result := FALSE;
    NomP := dynvars.ILimit * VBase;
    if not GFM_Mode then
        Exit;

    GetCurrents(Iterminal);
    dynVars.IComp := 0.0;
    for i := 1 to NPhases do
    begin
        curr := Iterminal[i];
        PhaseAmps := cabs(curr);
        volts := cabs(DSS.ActiveCircuit.Solution.NodeV[NodeRef[i]]);
        PhaseP := PhaseAmps * volts;
        if PhaseP > NomP then
        begin
            if PhaseP > dynVars.IComp then
                dynVars.IComp := PhaseP;
            Result := TRUE;
        end;
    end;
end;

procedure TInvBasedPCE.Randomize(Opt: Integer);
begin
    case Opt of
        0:
            RandomMult := 1.0;
        GAUSSIAN:
            RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
        UNIfORM:
            RandomMult := Random;  // number between 0 and 1.0
        LOGNORMAL:
            RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
    end;
end;

procedure TInvBasedPCE.GetCurrents(Curr: pComplexArray);
// Required for operation in GFM mode
var
    i: Integer;
begin
    if not GFM_Mode then
    begin
        inherited GetCurrents(Curr);
        Exit;
    end;

    try
        with DSS.ActiveCircuit.Solution do
        begin
            for i := 1 to Yorder do
                Vterminal[i] := NodeV[NodeRef[i]];

            YPrim.MVMult(Curr, Vterminal); // Current from Elements in System Y
            // CalcInjCurrentArray();
            
            // Add Together with yprim currents
            for i := 1 to Yorder do
                Curr[i] -= InjCurrent[i];
        end;
    except
        On E: Exception do
            DoErrorMsg(Format(_('GetCurrents for Element: %s.'), [FullName]), E.Message, _('Inadequate storage allotted for circuit element.'), 327);
    end;
end;

procedure TInvBasedPCE.StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);
 // Add the current into the proper location according to connection
 // Reverse of similar routine in load  (Cnegates are switched)
var
    j: Integer;
begin
    case Connection of
        0:
        begin  //Wye
            TermArray[i] += Curr;
            TermArray[Fnconds] -= Curr; // Neutral
        end;
        1:
        begin //DELTA
            TermArray[i] += Curr;
            j := i + 1;
            if j > Fnconds then
                j := 1;
            TermArray[j] -= Curr;
        end;
    end;
end;

end.