unit WTG3_Model;

interface

Uses Dynamics, math, ucomplex, Mathutil, ParserDel, Command, WindGenVars;

Const
  NumProperties = 23; // motor model parameters
  NumVariables = 18; // runtime variables

{$INCLUDE ..\Common\DSSCallBackStructDef.pas}

Type

  TSymCompArray = Array [0 .. 2] of Complex;
  TPhArray = Array [1 .. 3] of Complex;

  pTDynamicsRec = ^TDynamicsRec;
  pTWindGenVars = ^TWindGenVars;
  // pTDynaCallBacks = ^TDynaCallBacks;

  TGE_WTG3_Model = class(TObject)
  private

    // ratings
    ratedHz, ratedKVA, ratedOmg, ratedKVll, ratedVln, ratedAmp: Double;

    // filter time constant
    TfltPQM: Double;
    TfltVfbk: Double;
    VmeasMax, ImeasMax: Double;
    // PLL
    KpPLL, KiPLL, dOmgLim: Double;
    VdPos, VqPos, VdNeg, VqNeg: Double;
    VdFbkPos, VqFbkPos, VdFbkNeg, VqFbkNeg: Double;
    IdPos, IqPos, IdNeg, IqNeg: Double;
    dOmg, Vang: Double;
    // PQ priority control
    QordMax, QordMin, Iphl, Iqhl, ImaxTD: Double;
    TfltIqmxvUp, TfltIqmxvDn: Double;
    Iqmxv, Ipmx, Iqmx, Ipmn, Iqmn: Double;
    // Active and reactive power regulator

    V1_VoltVar, V2_VoltVar, V3_VoltVar, V4_VoltVar: Double;
    Q1_VoltVar, Q2_VoltVar, Q3_VoltVar, Q4_VoltVar: Double;
    VCurveVoltVar, QCurveVoltVar: Array [0 .. 5] of Double;
    Qref, PFref, rrlQcmd: Double;
    PordMax, PordMin, Pcurtail, Pord, Pcmd: Double;
    KpQreg, KiQreg, VrefMin, VrefMax: Double;
    Qcmd, errQgen: Double;
    // Voltage regulator
    KpVreg, KiVreg: Double;
    Vref, errVmag: Double;
    // LVPL logic
    TfltVmagLVPL, TfltPplvLim0: Double;
    V0LVPL, P0LVPL, V1LVPL, P1LVPL, MaxTrq: Double;
    TfltPplvLimUp, TfltPplvLimDn: Double;
    VmagLVPL, PplvLim0, PplvLim: Double;
    // LVQL logic
    TfltVmagLVQL: Double;
    V0LVQL, I0LVQL, V1LVQL, I1LVQL: Double;
    TfltIqlvLimUp, TfltIqlvLimDn: Double;
    IqLimAsymFlt: Double;
    VmagLVQL, IqlvLim: Double;
    // Ireg
    TfltIcmdPos, KpIregPos, KiIregPos, rrlIqCmd: Double;
    KpIregNeg, KiIregNeg, AngIregNeg, dE2Lim, E2magLim: Double;
    IdCmdPos, IqCmdPos, IdCmdNeg, IqCmdNeg: Double;
    Iplv, Iqlv: Double;
    errIdPos, errIqPos, errIdNeg, errIqNeg: Double;
    // fault detection
    VthrsAsymFlt, TthrsAsymFlt: Double;
    AsymFltFlag: Integer;
    TmrAsymFlt: Double;
    underSpeedTrip, wtgTrip: Integer;
    userTrip: Integer;

    // Aerodynamic model
    KbAero, HalfRhoArAero: Double;
    AlphaAero: Array [0 .. 4, 0 .. 4] of Double;
    WtOpt, PmechMax: Double;
    // torque regulator
    WtRefMin, WtRefMax: Double;
    TfltWtRef, KpTrqReg, KiTrqReg, TrqRefMax, TrqRefMin: Double;
    TfltPinp, PinpMax, PinpMin, rrlPinp, TfltErrPinp: Double;
    WtRef, errWt, errWtOld, Pinp1, Pinp, TrqRef, errPinp, errPinpFlt: Double;
    // pitch control
    KpPitchCtrl, KiPitchCtrl, KpPitchComp, KiPitchComp: Double;
    thetaPitchMax, thetaPitchMin, TfltPitch, rrlThetaPitch: Double;
    thetaPitch, thetaPitch0, errPstl, Pmech, PmechAvl: Double;

    TfltPavlAPC: Double;
    FrqTableAPC, PwrTableAPC: Array [0 .. 4] of Double;
    TfltPsetAPC, TdelayAPC: Double;
    PavlAPC, PsetAPC, PadeAPC, Pstl: Double;
    // wind inertia
    dbWindInertia, TfltDFrqWindInertia, KWindInertia,
      TfltDPinpWindInertia: Double;
    dPinpMax, dPinpMin, rruDPinp, rrdDPinp: Double;
    dFrqPuTest, dFrqWindInertia, y3Lpf, dPinpWindInertia: Double;
    // swing model
    WtBase, Hwtg, Dshaft: Double;
    Wt, dWt: Double;
    // regulator output
    dEmax, dEmin: Double;
    EdPos, EqPos, EdNeg, EqNeg: Double;
    // integrator
    intg_x, intg_d, intg_d_old: Array [0 .. 11] of Double;
    DebugTrace: Integer;
    TraceFile: TextFile;
    debugVar: Array [1 .. 10] of Double;

    Function Get_Variable(i: Integer): Double;
    Procedure Set_Variable(i: Integer; const Value: Double);
    Procedure abc2seq(Var abc: TPhArray; Var seq: TSymCompArray; ang: Double);
    Procedure seq2abc(Var abc: TPhArray; Var seq: TSymCompArray; ang: Double);
    Function MagLimiter(x: Complex; magmin: Double; magMax: Double): Complex;
    Function LinearInterp(Var xTable: Array of Double;
      Var yTable: Array of Double; x: Double): Double;
    Function CalcCp(theta: Double; lmbda: Double): Double;
    Function CalcPmech(theta: Double; wrotor: Double; spdwind: Double): Double;
    Function CalcWtRef(elePwr: Double): Double;
    Procedure Instrumentation(Var V, i: pComplexArray);
    Procedure PllLogic;
    Procedure PQPriority(PQFlag: Integer);
    Procedure LVPL;
    Procedure LVQL;
    Procedure RealPowerReg;
    Procedure ReactivePowerReg;
    Procedure VoltageReg;
    Procedure CurrentReg;
    Procedure CurrentLimiting;
    Procedure FaultDetection;
    Procedure AeroMPPT;
    Procedure AeroDynamic;
    Procedure TorqueReg;
    Procedure PitchControl;
    Procedure APCLogic;
    Procedure WindInertia;
    Procedure SwingModel;
    Procedure CalcCurrent(Var i: pComplexArray);
    Procedure DoHelpCmd;
    Procedure InitTraceFile;
    Procedure WriteTraceRecord;

  protected

  public
    // simulation time setup
    tsim, deltSim, delt0, delt: Double;
    nRec, nIterLF: Integer;
    QMode, QFlg: Integer;
    // active power control
    APCFLG: Integer;
    // simulate mechanical system
    SimMechFlg: Integer;
    // number of WTG
    N_WTG: Integer;
    // terminal impedance
    Xthev, Rthev: Double;
    Zthev: Complex;
    // terminal voltage and current
    Vabc, Iabc, Eabc: TPhArray;
    V012, I012, E012: TSymCompArray;
    Vmag, VmagMin: Double;
    Emag, Eang: Double;
    Sele: Complex;
    Pele, Qele: Double;
    Pgen, Qgen: Double;
    // steady state conditions for initialization
    Vss, Pss, Qss: Double;
    // wind speed
    vwind: Double;

    DynaData: pTDynamicsRec;
    GenData: pTWindGenVars;

    Procedure Init(Var V, i: pComplexArray);
    Procedure Edit; // Uses ModelParser
    Procedure EditProp(ParamPointer : integer; StrVal : String);
    Procedure Integrate;
    Procedure CalcDynamic(Var V, i: pComplexArray);
    Procedure CalcPFlow(Var V, i: pComplexArray);
    Procedure ReCalcElementData;

    Property Variable[i: Integer]: Double Read Get_Variable Write Set_Variable;

    constructor Create(Var GenVars: TWindGenVars; Var DynaVars: TDynamicsRec);
    destructor Destroy; override;

  end;

Var

  ActiveModel: TGE_WTG3_Model;
  ModelParser: TParser;
  CommandList: TCommandlist;

implementation

Uses SysUtils;
{ ------------------------------------------------------------------------------------------------------------- }
{ Model Class code }
{ ------------------------------------------------------------------------------------------------------------- }

{ TGE_WTG3_Model }
{ ------------------------------------------------------------------------------------------------------------- }
constructor TGE_WTG3_Model.Create(Var GenVars: TWindGenVars;
  Var DynaVars: TDynamicsRec);
{ ------------------------------------------------------------------------------------------------------------- }
begin
  { default parameter values }
  delt0 := 0.000050;
  ratedHz := 60;
  ratedKVA := 3600;
  ratedKVll := 0.69;
  //
  N_WTG := 1;
  //
  Vss := 1;
  Pss := 1;
  Qss := 0;
  vwind := 14;
  //
  Xthev := 0.05;
  Rthev := 0.0;
  //
  SimMechFlg := 1;
  APCFLG := 0;
  QFlg := 1;
  //
  TfltPQM := 0.02;
  TfltVfbk := 0.001;
  VmeasMax := 2.0;
  ImeasMax := 2.0;
  //
  KpPLL := 60;
  KiPLL := 300;
  //
  QordMax := 0.436;
  QordMin := -0.436;
  Iphl := 1.24;
  Iqhl := 1.25;
  ImaxTD := 1.25;
  TfltIqmxvUp := 0.016;
  TfltIqmxvDn := 0.160;
  //
  PordMin := 0.0;
  PordMax := 1.12;
  Pcurtail := 1.12;
  //
  QMode := 0; // 0 -> Constant Q, 1 -> Constant PF, 2 -> Volt-Var
  // IEEE 1547-2018 CAT-B Volt-Var curve
  V1_VoltVar := 0.92;
  V2_VoltVar := 0.98;
  V3_VoltVar := 1.02;
  V4_VoltVar := 1.08;
  Q1_VoltVar := 0.44;
  Q2_VoltVar := 0.0;
  Q3_VoltVar := 0.0;
  Q4_VoltVar := -0.44;
  // Q regulator
  rrlQcmd := 0.2;
  KpQreg := 0.0;
  KiQreg := 0.2;
  VrefMax := 1.1;
  VrefMin := 0.9;
  //
  KpVreg := 0;
  KiVreg := 40;
  //
  TfltVmagLVPL := 0.002;
  TfltPplvLim0 := 0.01;
  V0LVPL := 0.4875;
  P0LVPL := 0.0;
  V1LVPL := 0.9;
  P1LVPL := 1.13625;
  MaxTrq := (ratedKVA * 1000 / 1454 / 2 / PI * 60) * 1.1931;
  TfltPplvLimUp := 0.160;
  TfltPplvLimDn := 0.016;
  //
  TfltVmagLVQL := 0.01;
  V0LVQL := 0.5;
  I0LVQL := 0.9;
  V1LVQL := 0.9;
  I1LVQL := 0.79;
  TfltIqlvLimUp := 0.016;
  TfltIqlvLimDn := 0.160;
  IqLimAsymFlt := 0.447;
  //
  TfltIcmdPos := 0.002;
  // KpIregPos := 0.9*Xthev;
  // KiIregPos := 100*KpIregPos;
  rrlIqCmd := 0.5;
  //
  // KiIregNeg := KiIregPos*0.1;
  AngIregNeg := 65 * PI / 180;
  dE2Lim := 0.05;
  E2magLim := 0.105;
  //
  dEmax := 0.1;
  dEmin := -0.1;
  //
  VthrsAsymFlt := 30 / (0.69 * 1000 * sqrt(2) / sqrt(3));
  TthrsAsymFlt := 0.03;
  //
  KbAero := 69.5;
  HalfRhoArAero := 0.00145;
  AlphaAero[0, 0] := -0.41909;
  AlphaAero[0, 1] := 0.21808;
  AlphaAero[0, 2] := -0.012406;
  AlphaAero[0, 3] := -0.00013365;
  AlphaAero[0, 4] := 0.000011524;
  AlphaAero[1, 0] := -0.067606;
  AlphaAero[1, 1] := 0.060405;
  AlphaAero[1, 2] := -0.013934;
  AlphaAero[1, 3] := 0.0010683;
  AlphaAero[1, 4] := -0.000023895;
  AlphaAero[2, 0] := 0.015727;
  AlphaAero[2, 1] := -0.010996;
  AlphaAero[2, 2] := 0.0021495;
  AlphaAero[2, 3] := -0.00014855;
  AlphaAero[2, 4] := 2.7937E-06;
  AlphaAero[3, 0] := -0.00086018;
  AlphaAero[3, 1] := 0.00057051;
  AlphaAero[3, 2] := -0.00010479;
  AlphaAero[3, 3] := 5.9924E-06;
  AlphaAero[3, 4] := -8.9194E-08;
  AlphaAero[4, 0] := 0.000014787;
  AlphaAero[4, 1] := -9.4839E-06;
  AlphaAero[4, 2] := 1.6167E-06;
  AlphaAero[4, 3] := -7.1535E-08;
  AlphaAero[4, 4] := 4.9686E-10;
  //
  WtRefMin := 0;
  WtRefMax := 1.2;
  TfltWtRef := 60;
  KpTrqReg := 3;
  KiTrqReg := 0.6;
  TrqRefMax := 1.2;
  TrqRefMin := 0.08;
  TfltPinp := 0.05;
  PinpMax := 1.12;
  PinpMin := 0.04;
  rrlPinp := 0.45;
  TfltErrPinp := 1.0;
  //
  KpPitchCtrl := 150;
  KiPitchCtrl := 25;
  KpPitchComp := 3;
  KiPitchComp := 30;
  thetaPitchMax := 27;
  thetaPitchMin := 0;
  TfltPitch := 0.3;
  rrlThetaPitch := 10;
  //
  TfltPavlAPC := 0.15;
  FrqTableAPC[0] := 0.96;
  FrqTableAPC[1] := 0.996;
  FrqTableAPC[2] := 1.004;
  FrqTableAPC[3] := 1.04;
  FrqTableAPC[4] := 1.0662;
  PwrTableAPC[0] := 1.0;
  PwrTableAPC[1] := 0.95;
  PwrTableAPC[2] := 0.95;
  PwrTableAPC[3] := 0.40;
  PwrTableAPC[4] := 0.0;
  TfltPsetAPC := 5;
  TdelayAPC := 0.15;
  //
  dbWindInertia := 0.0025;
  TfltDFrqWindInertia := 1;
  KWindInertia := 10;
  TfltDPinpWindInertia := 5.5;
  dPinpMax := 0.5;
  dPinpMin := 0;
  rruDPinp := 0.1;
  rrdDPinp := 1;
  //
  // WtBase := 2*PI*(ratedHz/3);
  Hwtg := 5.23;
  Dshaft := 0.0;
  //
  DebugTrace := 0;
  //
  GenData := @GenVars; // Make pointer to data in main DSS
  DynaData := @DynaVars;

  ReCalcElementData;

end;

destructor TGE_WTG3_Model.Destroy;
begin

  inherited;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.Edit;
{ ------------------------------------------------------------------------------------------------------------- }

VAR
  ParamPointer: Integer;
  ParamName: String;
  Param: String;

begin
  { This DLL has a version of the DSS Parser compiled into it directly because it
    was written on the same platform as the DSS. Otherwise, one should use the Callbacks. }

  ParamPointer := 0;
  ParamName := ModelParser.NextParam;
  Param := ModelParser.StrValue;
  WHILE Length(Param) > 0 DO
    BEGIN
      IF Length(ParamName) = 0 THEN
        Begin
          If Comparetext(Param, 'help') = 0 then
            ParamPointer := 23
          Else
            Inc(ParamPointer);
        End
      ELSE
        ParamPointer := CommandList.GetCommand(ParamName);

      CASE ParamPointer OF
        // 0: DoSimpleMsg('Unknown parameter "'+ParamName+'" for Object "'+Name+'"');
        1:
          Rthev := ModelParser.DblValue;
        2:
          Xthev := ModelParser.DblValue;
        3:
          Vss := ModelParser.DblValue;
        4:
          Pss := ModelParser.DblValue;
        5:
          Qss := ModelParser.DblValue;
        6:
          vwind := ModelParser.DblValue;
        7:
          QMode := ModelParser.IntValue;
        8:
          SimMechFlg := ModelParser.IntValue;
        9:
          APCFLG := ModelParser.IntValue;
        10:
          QFlg := ModelParser.IntValue;
        11:
          DebugTrace := ModelParser.IntValue;
        12:
          delt0 := ModelParser.DblValue;
        13:
          ratedKVA := ModelParser.DblValue;
        14:
          V1_VoltVar := ModelParser.DblValue;
        15:
          V2_VoltVar := ModelParser.DblValue;
        16:
          V3_VoltVar := ModelParser.DblValue;
        17:
          V4_VoltVar := ModelParser.DblValue;
        18:
          Q1_VoltVar := ModelParser.DblValue;
        19:
          Q2_VoltVar := ModelParser.DblValue;
        20:
          Q3_VoltVar := ModelParser.DblValue;
        21:
          Q4_VoltVar := ModelParser.DblValue;
        22:
          N_WTG := ModelParser.IntValue;
        23:
          DoHelpCmd; // whatever the option, do help
      ELSE
      END;

      ParamName := ModelParser.NextParam;
      Param := ModelParser.StrValue;
    END;

  ReCalcElementData;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.EditProp(ParamPointer : integer; StrVal : String);
{ ------------------------------------------------------------------------------------------------------------- }

VAR
  Param: String;

begin
    CASE ParamPointer OF
      // 0: DoSimpleMsg('Unknown parameter "'+ParamName+'" for Object "'+Name+'"');
      1:
        Rthev := StrToFloat(StrVal);
      2:
        Xthev := StrToFloat(StrVal);
      3:
        Vss := StrToFloat(StrVal);
      4:
        Pss := StrToFloat(StrVal);
      5:
        Qss := StrToFloat(StrVal);
      6:
        vwind := StrToFloat(StrVal);
      7:
        QMode := StrToInt(StrVal);
      8:
        SimMechFlg := StrToInt(StrVal);
      9:
        APCFLG := StrToInt(StrVal);
      10:
        QFlg := StrToInt(StrVal);
      11:
        DebugTrace := StrToInt(StrVal);
      12:
        delt0 := StrToFloat(StrVal);
      13:
        ratedKVA := StrToFloat(StrVal);
      14:
        V1_VoltVar := StrToFloat(StrVal);
      15:
        V2_VoltVar := StrToFloat(StrVal);
      16:
        V3_VoltVar := StrToFloat(StrVal);
      17:
        V4_VoltVar := StrToFloat(StrVal);
      18:
        Q1_VoltVar := StrToFloat(StrVal);
      19:
        Q2_VoltVar := StrToFloat(StrVal);
      20:
        Q3_VoltVar := StrToFloat(StrVal);
      21:
        Q4_VoltVar := StrToFloat(StrVal);
      22:
        N_WTG := StrToInt(StrVal);
      23:
        DoHelpCmd; // whatever the option, do help
    ELSE
    END;


  ReCalcElementData;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.ReCalcElementData;
{ ------------------------------------------------------------------------------------------------------------- }
begin

  // execution order: Create(Recalc) -> CalcPFlow -> Init
  ratedOmg := 2 * PI * ratedHz;
  ratedVln := ratedKVll / sqrt(3.0) * 1000;
  ratedAmp := ratedKVA * 1000 / ratedVln / 3;
  MaxTrq := (ratedKVA * 1000 / 1454 / 2 / PI * 60) * 1.1931;
  Zthev := cmplx(Rthev, Xthev);
  dOmgLim := 0.2 * ratedOmg;

  // current regulator parameters
  KpIregPos := 0.9 * Xthev;
  KiIregPos := 25. * KpIregPos;
  //
  KpIregNeg := KpIregPos * 1.5;
  KiIregNeg := KiIregPos * 1.5;

  // volt-var curve
  VCurveVoltVar[0] := max(0.0, V1_VoltVar - 0.2);
  VCurveVoltVar[1] := V1_VoltVar;
  VCurveVoltVar[2] := V2_VoltVar;
  VCurveVoltVar[3] := V3_VoltVar;
  VCurveVoltVar[4] := V4_VoltVar;
  VCurveVoltVar[5] := min(2.0, V4_VoltVar + 0.2);
  QCurveVoltVar[0] := Q1_VoltVar;
  QCurveVoltVar[1] := Q1_VoltVar;
  QCurveVoltVar[2] := Q2_VoltVar;
  QCurveVoltVar[3] := Q3_VoltVar;
  QCurveVoltVar[4] := Q4_VoltVar;
  QCurveVoltVar[5] := Q4_VoltVar;

  // turbine rotation speed base
  WtBase := 2 * PI * (ratedHz / 3);

  // 1.5MW parameters
  if (ratedKVA < 2000) then
    begin
      Hwtg := 4.94;
      KbAero := 56.6;
      HalfRhoArAero := 0.00159;
    end;

  // time steps
  deltSim := DynaData^.h;
  nRec := trunc(int(DynaData^.h / delt0 / 2) * 2 + 1);
  delt := DynaData^.h / nRec;
  tsim := DynaData^.t;
  nIterLF := 100;

  // initialize trace file
  If DebugTrace = 1 Then
    InitTraceFile;
end;

{ ------------------------------------------------------------------------------------------------------------- }
function TGE_WTG3_Model.MagLimiter(x: Complex; magmin: Double;
  magMax: Double): Complex;
{ ------------------------------------------------------------------------------------------------------------- }

begin

  Result := pclx(max(magmin, min(magMax, cabs(x))), cang(x));

end;

{ ------------------------------------------------------------------------------------------------------------- }
function TGE_WTG3_Model.LinearInterp(Var xTable: Array of Double;
  Var yTable: Array of Double; x: Double): Double;
{ ------------------------------------------------------------------------------------------------------------- }
Var
  iLeft, iRight, ii: Integer;

begin
  iLeft := Low(xTable);
  iRight := High(xTable);
  Result := yTable[iLeft];
  if x < xTable[iLeft] then
    Result := yTable[iLeft]
  else if x > xTable[iRight] then
    Result := yTable[iRight]
  else
    begin
      for ii := iLeft to iRight - 1 do
        if (x >= xTable[ii]) and (x <= xTable[ii + 1]) then
          begin
            Result := (yTable[ii + 1] - yTable[ii]) /
              (xTable[ii + 1] - xTable[ii]) * (x - xTable[ii]) + yTable[ii];
            break;
          end;
    end;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.abc2seq(Var abc: TPhArray; Var seq: TSymCompArray;
  ang: Double);
{ ------------------------------------------------------------------------------------------------------------- }
Var
  ii: Integer;
  temp: Complex;

begin
  // phase to sequence conversion
  Phase2SymComp(@abc, @seq);
  // rotation of the sequence components
  temp := cmplx(cos(-ang), sin(-ang));
  for ii := 0 to 2 do
    begin
      seq[ii] := cmul(seq[ii], temp);
    end;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.seq2abc(Var abc: TPhArray; Var seq: TSymCompArray;
  ang: Double);
{ ------------------------------------------------------------------------------------------------------------- }
Var
  ii: Integer;
  temp: Complex;

begin
  // sequence to phase conversion
  SymComp2Phase(@abc, @seq);
  // rotation of the sequence components
  temp := cmplx(cos(ang), sin(ang));
  for ii := 1 to 3 do
    begin
      abc[ii] := cmul(abc[ii], temp);
    end;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.Instrumentation(Var V, i: pComplexArray);
{ ------------------------------------------------------------------------------------------------------------- }
Var
  ii: Integer;
  ktemp: Double;

begin
  // per-unitize abc voltage and current
  for ii := 1 to 3 do
    begin
      Vabc[ii] := MagLimiter(cdivreal(V[ii], ratedVln), 0, VmeasMax);
      // Iabc = -(I/AmpBase+Vabc/Zthev)
      Iabc[ii] := MagLimiter(csub(cdivreal(i[ii], -ratedAmp * N_WTG),
        cdiv(Vabc[ii], Zthev)), 0, ImeasMax);
    end;

  // phase to sequence conversion
  abc2seq(Vabc, V012, Vang);
  abc2seq(Iabc, I012, Vang);

  // get rid of zero sequence component in voltage
  V012[0] := cmplx(0, 0);
  seq2abc(Vabc, V012, Vang);

  // voltage magnitude
  Vmag := cabs(V012[1]);

  // minimum voltage for fault ride through
  VmagMin := min(min(cabs(Vabc[1]), cabs(Vabc[2])), cabs(Vabc[3]));

  // calculate output power
  Sele := cmplx(0, 0);
  for ii := 1 to 3 do
    Sele := cadd(Sele, cdivreal(cmul(Vabc[ii], conjg(Iabc[ii])), 3));
  Pele := Sele.re;
  Qele := Sele.im;

  ktemp := min(1, deltSim / TfltPQM);
  Pgen := Pgen + (Pele - Pgen) * ktemp;
  Qgen := Qgen + (Qele - Qgen) * ktemp;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.CalcPFlow(Var V, i: pComplexArray);
{ ------------------------------------------------------------------------------------------------------------- }

Var
  Vtemp, Itemp, Etemp: Complex;
  kCnvg: Double;

begin
  // instrumentation
  Instrumentation(V, i);

  // solve Emag and Eang
  if nIterLF = 1 then
    begin
      Vtemp := cmulreal(cdivreal(V012[1], max(0.000001, cabs(V012[1]))), Vss);
      Emag := cabs(Vtemp);
      Eang := cang(Vtemp);
    end
  else
    Vtemp := V012[1];
  Itemp := conjg(cdiv(cmplx(Pss, Qss), Vtemp));
  Etemp := cadd(Vtemp, cmul(Zthev, Itemp));
  if nIterLF < 10 then
    begin
      kCnvg := max(0.4, min(1.0, 1 - (nIterLF - 1) * 0.1));
      Emag := max(0.0, min(2.0, Emag + (cabs(Etemp) - Emag) * kCnvg));
      Eang := Eang + (cang(Etemp) - Eang) * kCnvg;
    end;
  nIterLF := nIterLF + 1;

  // update output
  E012[0] := cmplx(0, 0);
  E012[1] := cmplx(Emag * cos(Eang), Emag * sin(Eang));
  E012[2] := cmplx(0, 0);
  CalcCurrent(i);

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.Init(Var V, i: pComplexArray);
{ ------------------------------------------------------------------------------------------------------------- }
// Init for Dynamics mode
// execution order: Create(Recalc) -> CalcPFlow -> Init
var
  ii: Integer;
  eIter: Double;
  kIter: Double;

begin
  // check for available wind power and update initial power condition
  AeroMPPT;
  if Pss > PmechMax then
    // not enough wind power to support Pss, update Pss
    begin
      Pss := PmechMax;
      Wt := WtOpt;
    end
  else
    Wt := CalcWtRef(Pss);
  // run iteration to solve for thetaPitch
  thetaPitch := thetaPitchMax / 2;
  eIter := 0.01;
  kIter := 10;
  for ii := 1 to 10 do
    begin
      AeroDynamic;
      if abs(Pmech - Pss) < eIter then
        break
      else
        begin
          thetaPitch := thetaPitch + kIter * (Pmech - Pss);
          thetaPitch := min(thetaPitchMax, max(thetaPitchMin, thetaPitch));
        end;
    end;

  // run a load flow
  nIterLF := 1;
  CalcPFlow(V, i);

  // initialize control variables
  VdFbkPos := Vss;
  VqFbkPos := 0;
  VdFbkNeg := 0;
  VqFbkNeg := 0;
  dOmg := 0;
  Vang := cang(Vabc[1]);
  VqPos := 0;
  PplvLim0 := P1LVPL;
  PplvLim := PplvLim0;
  IqlvLim := I0LVQL;
  VmagLVPL := VdFbkPos;
  VmagLVQL := VdFbkPos;
  Pord := Pss;
  Pgen := Pss;
  IdCmdPos := Pss / Vss;
  Iplv := IdCmdPos;
  if (QMode = 0) or (QMode = 1) then
    Qcmd := Qss
  else
    Qcmd := LinearInterp(VCurveVoltVar, QCurveVoltVar, Vss);
  PFref := abs(Pss) / max(0.000001, sqrt(Pss * Pss + Qss * Qss));
  if Qss < 0 then
    PFref := -PFref;
  Qgen := Qcmd;
  errQgen := 0;
  Vref := Vss;
  IqCmdPos := -Qcmd / Vss;
  Iqlv := IqCmdPos;
  Iqmxv := QordMax / Vss;
  errVmag := 0;
  errIdPos := 0;
  errIqPos := 0;
  EdPos := Emag * cos(Eang);
  EqPos := Emag * sin(Eang);
  // negative sequence current regulator
  errIdNeg := 0;
  errIqNeg := 0;
  EdNeg := 0;
  EqNeg := 0;
  // fault detection
  underSpeedTrip := 0;
  wtgTrip := 0;
  AsymFltFlag := 0;
  TmrAsymFlt := 0;
  // torque regulator
  WtRef := Wt;
  errWt := 0;
  Pinp1 := Pss;
  Pinp := Pss;
  TrqRef := Pss / Wt;
  errPinp := 0;
  errPinpFlt := 0;
  // pitch control
  errPstl := 0;
  thetaPitch0 := thetaPitch;
  // active power control
  // Pcurtail := Pss;
  PavlAPC := Pss;
  PsetAPC := Pss;
  PadeAPC := Pss;
  Pstl := Pss;
  // wind inertia
  dFrqPuTest := 0;
  dFrqWindInertia := 0;
  y3Lpf := 0;
  dPinpWindInertia := 0;
  // swing model
  dWt := Wt - 1;

  // initialize integrator
  for ii := Low(intg_x) to High(intg_x) do
    begin
      intg_x[ii] := 0;
      intg_d[ii] := 0;
      intg_d_old[ii] := 0;
    end;
  intg_x[0] := dOmg;
  intg_x[1] := Vang;
  intg_x[2] := 0;
  intg_x[3] := 0;
  intg_x[4] := 0;
  intg_x[5] := 0;
  intg_x[6] := IqCmdPos;
  intg_x[7] := Vref;
  intg_x[8] := TrqRef;
  intg_x[9] := thetaPitch;
  intg_x[10] := 0;
  intg_x[11] := dWt;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.PllLogic;
{ ------------------------------------------------------------------------------------------------------------- }

Var
  VqPosOld: Double;
  kFltTemp: Double;
  drvTemp: Double;

begin
  // PI regulator (Vq to dFrq)
  VqPosOld := VqPos;
  VdPos := V012[1].re;
  VqPos := V012[1].im;
  drvTemp := KiPLL * VqPos + KpPLL * (VqPos - VqPosOld) / deltSim;
  dOmg := max(-dOmgLim, min(dOmgLim, dOmg + drvTemp * deltSim));
  // integrator (dFrq to Vang)
  Vang := Vang + dOmg * deltSim;
  // other sequence components
  VdNeg := V012[2].re;
  VqNeg := -V012[2].im;
  IdPos := I012[1].re;
  IqPos := I012[1].im;
  IdNeg := I012[2].re;
  IqNeg := -I012[2].im;
  // LPF on voltage feedback
  kFltTemp := min(1, DynaData^.h / TfltVfbk);
  VdFbkPos := VdFbkPos + (VdPos - VdFbkPos) * kFltTemp;
  VqFbkPos := VqFbkPos + (VqPos - VqFbkPos) * kFltTemp;
  VdFbkNeg := VdFbkNeg + (VdNeg - VdFbkNeg) * kFltTemp;
  VqFbkNeg := VqFbkNeg + (VqNeg - VqFbkNeg) * kFltTemp;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.PQPriority(PQFlag: Integer);
{ ------------------------------------------------------------------------------------------------------------- }

Var
  y0, temp: Double;

begin
  y0 := min(Iqhl, max(QordMax, (QordMax - 2.15) * Vmag + 2.15));
  if y0 > Iqmxv then
    temp := min(1, delt / TfltIqmxvUp)
  else
    temp := min(1, delt / TfltIqmxvDn);
  Iqmxv := Iqmxv + (y0 - Iqmxv) * temp;
  Iqmxv := min(Iqhl, max(0, Iqmxv));
  if PQFlag = 1 then
    begin
      // P priority
      Ipmx := min(ImaxTD, Iphl);
      Iqmx := min(Iqhl, sqrt(max(0, sqr(ImaxTD) - sqr(Iplv))));
    end
  else
    begin
      Iqmx := min(ImaxTD, Iqmxv);
      Ipmx := min(Iphl, sqrt(max(0, sqr(ImaxTD) - sqr(Iqlv))));
    end;

  Ipmn := -Ipmx;
  Iqmn := -Iqmx;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.LVPL;
{ ------------------------------------------------------------------------------------------------------------- }

Var
  temp: Double;
  y0, y0Lim, y1, y1Sub, y2: Double;

begin
  // low pass filter on Vmag
  temp := min(1, delt / TfltVmagLVPL);
  VmagLVPL := VmagLVPL + (VmagMin - VmagLVPL) * temp;
  // LVPL curve interpolation
  y0 := (P1LVPL - P0LVPL) / (V1LVPL - V0LVPL) * (VmagLVPL - V0LVPL) + P0LVPL;
  y0 := max(P0LVPL, min(P1LVPL, y0));
  y0 := y0 + 0.02;
  // upper limit to y0
  if AsymFltFlag = 1 then
    y0Lim := 0.75
  else
    y0Lim := max(0.5, min(2.0, Pord));
  y1 := min(y0Lim, y0);
  // subtraction term to y1
  if (Vmag < (0.91 - 0.05)) and (Vmag > (0.65 - 0.05)) then
    y1Sub := 0.2
  else
    y1Sub := 0.0;
  y1 := max(0, y1 - y1Sub);
  // limit on torque
  y2 := min(MaxTrq * Wt * WtBase / ratedKVA / 1000, y1);
  // low pass filter
  if y2 > PplvLim then
    temp := min(1, delt / TfltPplvLimUp)
  else
    temp := min(1, delt / TfltPplvLimDn);
  PplvLim := PplvLim + (y2 - PplvLim) * temp;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.LVQL;
{ ------------------------------------------------------------------------------------------------------------- }

Var
  temp: Double;
  y0: Double;

begin
  // low pass filter on Vmag
  temp := min(1, delt / TfltVmagLVQL);
  VmagLVQL := VmagLVQL + (VmagMin - VmagLVQL) * temp;
  // LVQL curve interpolation
  y0 := (I1LVQL - I0LVQL) / (V1LVQL - V0LVQL) * (VmagLVQL - V0LVQL) + I0LVQL;
  y0 := min(I0LVQL, max(I1LVQL, y0));
  // low pass filter
  if y0 > IqlvLim then
    temp := min(1, delt / TfltIqlvLimUp)
  else
    temp := min(1, delt / TfltIqlvLimDn);
  IqlvLim := IqlvLim + (y0 - IqlvLim) * temp;
  if AsymFltFlag = 1 then
    IqlvLim := IqLimAsymFlt;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.RealPowerReg;
{ ------------------------------------------------------------------------------------------------------------- }

begin
  // active power regulator
  Pcmd := max(PordMin, min(PordMax, min(PplvLim, Pord)));
  Pcmd := min(Pcurtail, Pcmd);
  IdCmdPos := Pcmd / max(0.000001, Vmag);
  IdCmdPos := max(Ipmn, min(Ipmx, IdCmdPos));

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.ReactivePowerReg;
{ ------------------------------------------------------------------------------------------------------------- }

Var
  Qord, errQgenOld: Double;
  temp: Double;

begin
  // calculation of desired Q output
  if QMode = 0 then
    // constant Q mode
    Qord := Qref
  else if QMode = 1 then
    // constant PF mode (negative means absorption)
    begin
      Qord := sqrt(1 - PFref * PFref) / max(0.000001, abs(PFref)) * Pgen;
      if PFref < 0 then
        Qord := -Qord;
    end
  else
    // volt-var mode
    Qord := LinearInterp(VCurveVoltVar, QCurveVoltVar, Vmag);
  // hard limiter on Qord
  Qord := min(QordMax, max(QordMin, Qord));
  // ramp rate limiter on Qcmd
  temp := rrlQcmd * delt;
  Qcmd := min(Qcmd + temp, max(Qcmd - temp, Qord));

  // reactive power regulator
  errQgenOld := errQgen;
  errQgen := Qcmd - Qgen;
  intg_d[7] := KiQreg * errQgen + KpQreg * (errQgen - errQgenOld) / delt;
  intg_x[7] := max(VrefMin, min(VrefMax, intg_x[7]));
  Vref := intg_x[7];

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.VoltageReg;
{ ------------------------------------------------------------------------------------------------------------- }

Var
  errVmagOld: Double;

begin
  errVmagOld := errVmag;
  errVmag := -(Vref - Vmag);
  intg_d[6] := KiVreg * errVmag + KpVreg * (errVmag - errVmagOld) / delt;
  intg_x[6] := max(Iqmn, min(Iqmx, intg_x[6]));
  IqCmdPos := intg_x[6];

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.CurrentReg;
{ ------------------------------------------------------------------------------------------------------------- }

Var
  ktemp, errIdPosOld, errIqPosOld: Double;
  errIdNegOld, errIqNegOld: Double;
  dE2Real, dE2Imag: Double;
  tempE2: Complex;

begin
  // positive sequence current regulator
  ktemp := min(1, delt / TfltIcmdPos);
  Iplv := Iplv + (IdCmdPos - Iplv) * ktemp;
  Iqlv := min(IqlvLim, max(-IqlvLim, Iqlv + (IqCmdPos - Iqlv) * ktemp));
  // anti windup
  if ((Iqlv = IqlvLim) and (IqCmdPos > Iqlv)) or
    ((Iqlv = -IqlvLim) and (IqCmdPos < Iqlv)) then
    begin
      intg_d[6] := 0;
      intg_d[7] := 0;
    end;

  // PI regulator for IdPos
  errIdPosOld := errIdPos;
  errIdPos := Iplv - IdPos;
  intg_d[2] := KiIregPos * errIdPos + KpIregPos *
    (errIdPos - errIdPosOld) / delt;
  intg_x[2] := max(dEmin, min(dEmax, intg_x[2]));
  EdPos := intg_x[2] + Rthev * Iplv - Xthev * Iqlv + VdFbkPos;
  // PI regulator for IqPos
  errIqPosOld := errIqPos;
  errIqPos := Iqlv - IqPos;
  intg_d[3] := KiIregPos * errIqPos + KpIregPos *
    (errIqPos - errIqPosOld) / delt;
  intg_x[3] := max(dEmin, min(dEmax, intg_x[3]));
  EqPos := intg_x[3] + Rthev * Iqlv + Xthev * Iplv + VqFbkPos;

  // negative sequence current regulator
  // be carefule with signs: E2=Ed-jEq, I2=Id-jIq
  IdCmdNeg := 0;
  IqCmdNeg := 0;
  // PI regulator
  errIdNegOld := errIdNeg;
  errIdNeg := IdCmdNeg - IdNeg;
  intg_d[4] := KiIregNeg * errIdNeg + KpIregNeg *
    (errIdNeg - errIdNegOld) / delt;
  errIqNegOld := errIqNeg;
  errIqNeg := IqCmdNeg - IqNeg;
  intg_d[5] := KiIregNeg * errIqNeg + KpIregNeg *
    (errIqNeg - errIqNegOld) / delt;
  // limiter on integrator
  intg_x[4] := min(dE2Lim, max(-dE2Lim, intg_x[4]));
  intg_x[5] := min(dE2Lim, max(-dE2Lim, intg_x[5]));
  // angle rotation (for better damping)
  dE2Real := intg_x[4] * cos(AngIregNeg) + intg_x[5] * sin(AngIregNeg);
  dE2Imag := intg_x[4] * sin(AngIregNeg) - intg_x[5] * cos(AngIregNeg);
  // V2 feedforwarding and E2 magnitude limiting
  tempE2 := cmplx(VdFbkNeg + dE2Real, -VqFbkNeg + dE2Imag);
  tempE2 := MagLimiter(tempE2, 0, E2magLim);
  EdNeg := tempE2.re;
  EqNeg := -tempE2.im;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.Integrate;
{ ------------------------------------------------------------------------------------------------------------- }

Var
  ii: Integer;

begin
  for ii := Low(intg_x) to High(intg_x) do
    begin
      intg_x[ii] := intg_x[ii] + delt / 2 * (intg_d_old[ii] + intg_d[ii]);
      intg_d_old[ii] := intg_d[ii];
    end;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.CurrentLimiting;
{ ------------------------------------------------------------------------------------------------------------- }

Var
  I2Max: Double;

begin
  E012[0] := cmplx(0, 0);
  // current limit on positive sequence
  E012[1] := cmplx(EdPos, EqPos);
  I012[1] := cdiv(csub(E012[1], V012[1]), Zthev);
  if cabs(I012[1]) > ImaxTD then
    begin
      I012[1] := cmulreal(cdivreal(I012[1], max(0.000001, cabs(I012[1]))
        ), ImaxTD);
      E012[1] := cadd(V012[1], cmul(I012[1], Zthev));
    end;
  // current limit on negative sequence
  E012[2] := cmplx(EdNeg, -EqNeg);
  I012[2] := cdiv(csub(E012[2], V012[2]), Zthev);
  I2Max := max(0, 1.1 - cabs(I012[1]));
  if cabs(I012[2]) > I2Max then
    begin
      I012[2] := cmulreal(cdivreal(I012[2], max(0.000001, cabs(I012[2])
        )), I2Max);
      E012[2] := cadd(V012[2], cmul(I012[2], Zthev));
    end;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.FaultDetection;
{ ------------------------------------------------------------------------------------------------------------- }

begin
  if cabs(V012[2]) > VthrsAsymFlt then
    TmrAsymFlt := TmrAsymFlt + deltSim
  else
    begin
      TmrAsymFlt := 0;
      AsymFltFlag := 0;
    end;
  if TmrAsymFlt > TthrsAsymFlt then
    AsymFltFlag := 1;

  // under speed fault
  if Wt < 0.1 then
    underSpeedTrip := 1;

  // tripping of WTG (more tripping logics to be added in the future)
  if (userTrip = 1) or (underSpeedTrip = 1) then
    wtgTrip := 1;

end;

{ ------------------------------------------------------------------------------------------------------------- }
function TGE_WTG3_Model.CalcCp(theta: Double; lmbda: Double): Double;
{ ------------------------------------------------------------------------------------------------------------- }
Var
  ii, jj: Integer;

begin
  Result := 0;
  for ii := 0 to 4 do
    for jj := 0 to 4 do
      Result := Result + AlphaAero[ii, jj] * Power(theta, ii) *
        Power(lmbda, jj);

end;

{ ------------------------------------------------------------------------------------------------------------- }
function TGE_WTG3_Model.CalcPmech(theta: Double; wrotor: Double;
  spdwind: Double): Double;
{ ------------------------------------------------------------------------------------------------------------- }
Var
  lmbda, Cp: Double;

begin
  lmbda := min(20, max(0, wrotor / max(0.01, spdwind) * KbAero));
  Cp := CalcCp(theta, lmbda);
  Result := min(1.2, HalfRhoArAero * Power(spdwind, 3) * Cp);

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.AeroMPPT;
{ ------------------------------------------------------------------------------------------------------------- }
Var
  WtList, PmechList: Array [0 .. 100] of Double;
  tempWt, stepWt: Double;
  ii, max_ii: Integer;

begin
  stepWt := (WtRefMax - WtRefMin) / 100;
  for ii := 0 to 100 do
    begin
      tempWt := WtRefMin + ii * stepWt;
      WtList[ii] := tempWt;
      PmechList[ii] := CalcPmech(0, tempWt, vwind);
    end;
  // find optimal Wt and maximum Pmech
  PmechMax := -100000;
  max_ii := 0;
  for ii := 0 to 100 do
    if PmechList[ii] > PmechMax then
      begin
        max_ii := ii;
        PmechMax := PmechList[ii];
      end;
  WtOpt := WtList[max_ii];

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.AeroDynamic;
{ ------------------------------------------------------------------------------------------------------------- }

begin
  Pmech := CalcPmech(thetaPitch, Wt, vwind);
  PmechAvl := CalcPmech(0.001, Wt, vwind);

end;

{ ------------------------------------------------------------------------------------------------------------- }
function TGE_WTG3_Model.CalcWtRef(elePwr: Double): Double;
{ ------------------------------------------------------------------------------------------------------------- }
Var
  temp: Double;

begin
  temp := min(1, elePwr);
  Result := max(WtRefMin, min(WtRefMax, -0.75 * temp * temp + 1.59 *
    temp + 0.63));

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.TorqueReg;
{ ------------------------------------------------------------------------------------------------------------- }
Var
  y1, y2, temp: Double;
  PinpSat, errPinpHpf: Double;

begin
  y1 := CalcWtRef(Pele);
  // low pass filter
  temp := min(1, delt / TfltWtRef);
  WtRef := WtRef + (y1 - WtRef) * temp;
  // PI regulator
  errWtOld := errWt;
  errWt := Wt - WtRef;
  intg_d[8] := KiTrqReg * errWt + KpTrqReg * (errWt - errWtOld) / delt;
  intg_x[8] := max(TrqRefMin, min(TrqRefMax, intg_x[8]));
  TrqRef := intg_x[8];
  // convert torque to power
  y2 := TrqRef * Wt;
  // low pass filter on Pinp
  temp := min(1, delt / TfltPinp);
  Pinp1 := min(PinpMax, max(PinpMin, Pinp1 + (y2 - Pinp1) * temp));
  // ramp rate limiter
  temp := rrlPinp * delt;
  Pinp := min(Pinp + temp, max(Pinp - temp, Pinp1));
  // power response rate limit
  PinpSat := min(Pstl, max(0, Pinp));
  errPinp := Pinp - PinpSat;
  // high pass filter on errPinp
  temp := min(1, delt / TfltErrPinp);
  errPinpFlt := errPinpFlt + (errPinp - errPinpFlt) * temp;
  errPinpHpf := errPinp - errPinpFlt;
  // get the final Pord
  Pord := PinpSat + errPinpHpf + dPinpWindInertia;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.PitchControl;
{ ------------------------------------------------------------------------------------------------------------- }
Var
  x1, y1, y2, errPstlOld, temp: Double;

begin
  // Note: this function should go after torque regulator where errWt and errWtOld are calculated
  // PI regulator for pitch control
  intg_d[9] := KiPitchCtrl * errWt + KpPitchCtrl * (errWt - errWtOld) / delt;
  // PI regulator for pitch compensator
  errPstlOld := errPstl;
  errPstl := Pinp - Pstl;
  intg_d[10] := KiPitchComp * errPstl + KpPitchComp *
    (errPstl - errPstlOld) / delt;
  // anti-windup
  x1 := intg_d[9] + intg_d[10];
  y1 := intg_x[9] + intg_x[10];
  if ((y1 >= thetaPitchMax) and (x1 > 0)) or ((y1 <= thetaPitchMin) and (x1 < 0))
  then
    begin
      intg_d[9] := 0;
      intg_d[10] := 0;
    end;
  y2 := max(thetaPitchMin, min(thetaPitchMax, y1));
  // low pass filter
  temp := min(1, delt / TfltPitch);
  thetaPitch0 := thetaPitch0 + (y2 - thetaPitch0) * temp;
  // ramp rate limiter
  temp := rrlThetaPitch * delt;
  thetaPitch := min(thetaPitch + temp, max(thetaPitch - temp, thetaPitch0));

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.APCLogic;
{ ------------------------------------------------------------------------------------------------------------- }
Var
  y1, y2, y3, y4, temp, gridFrq, PmechMax, PmechMin: Double;

begin
  y1 := min(1, max(0.000001, PmechAvl));
  // low pass filter on available power
  temp := min(1, delt / TfltPavlAPC);
  PavlAPC := PavlAPC + (y1 - PavlAPC) * temp;
  // power curtailment
  temp := max(0.4, min(1.0, Pcurtail / PavlAPC));
  PwrTableAPC[1] := temp;
  PwrTableAPC[2] := temp;
  // power frequency curve
  gridFrq := 1 + dOmg / ratedOmg;
  y2 := LinearInterp(FrqTableAPC, PwrTableAPC, gridFrq);
  y3 := PavlAPC * y2;
  // low pass filter on set power
  temp := min(1, delt / TfltPsetAPC);
  PsetAPC := PsetAPC + (y3 - PsetAPC) * temp;
  // APCFLG
  if APCFLG = 0 then
    y4 := Pcurtail
  else
    y4 := PsetAPC;
  // enforce user-defined PmechMax in normal condition
  if (gridFrq >= FrqTableAPC[1]) and (gridFrq <= FrqTableAPC[2]) then
    PmechMax := 1.0
  else
    PmechMax := 1.2;
  PmechMin := 0.2;
  y4 := min(PmechMax, max(PmechMin, y4));
  // Pade delay function
  temp := min(1, delt / TdelayAPC * 2);
  PadeAPC := PadeAPC + (y4 - PadeAPC) * temp;
  Pstl := min(PmechMax, max(PmechMin, 2 * PadeAPC - y4));

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.WindInertia;
{ ------------------------------------------------------------------------------------------------------------- }
Var
  y1, y2, y3, y4, temp, temp1, temp2: Double;

begin
  y1 := -dOmg / ratedOmg + dFrqPuTest;
  // deadband
  y2 := max(0, y1 - dbWindInertia);
  // low pass filter
  temp := min(1, delt / TfltDFrqWindInertia);
  dFrqWindInertia := dFrqWindInertia + (y2 - dFrqWindInertia) * temp;
  // multiplier
  y3 := dFrqWindInertia * KWindInertia;
  // high pass filter
  temp := min(1, delt / TfltDPinpWindInertia);
  y3Lpf := y3Lpf + (y3 - y3Lpf) * temp;
  y4 := min(dPinpMax, max(dPinpMin, y3 - y3Lpf));
  // ramp rate limiter
  temp1 := rruDPinp * delt;
  temp2 := rrdDPinp * delt;
  dPinpWindInertia := min(dPinpWindInertia + temp1,
    max(dPinpWindInertia - temp2, y4));

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.SwingModel;
{ ------------------------------------------------------------------------------------------------------------- }
Var
  Tmech, Tele, Tdamp: Double;

begin
  Tmech := Pmech / Wt;
  Tele := Pele / Wt;
  Tdamp := Dshaft * dWt;
  intg_d[11] := (Tmech - Tele - Tdamp) / 2 / Hwtg;
  dWt := intg_x[11];
  Wt := max(0.01, 1.0 + dWt);

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.CalcCurrent(Var i: pComplexArray);
{ ------------------------------------------------------------------------------------------------------------- }
Var
  ii: Integer;

begin
  // sequence to phase
  seq2abc(Eabc, E012, Vang);

  // Thevenin to Norton (current injection)
  for ii := 1 to 3 do
    i[ii] := cmulreal(cdiv(Eabc[ii], Zthev), -ratedAmp * N_WTG);

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.CalcDynamic(Var V, i: pComplexArray);
{ ------------------------------------------------------------------------------------------------------------- }
Var
  ii: Integer;

begin
  deltSim := DynaData^.h;

  // instrumentation
  Instrumentation(V, i);
  // PLL
  PllLogic;
  // fault detection
  FaultDetection;

  // start small time step iteration on when time proceeds
  if (DynaData^.t > tsim) and (DynaData^.IterationFlag = 1) then
    begin
      nRec := trunc(int(DynaData^.h / delt0 / 2) * 2 + 1);
      delt := DynaData^.h / nRec;
      tsim := DynaData^.t;
      if (wtgTrip = 0) then
        begin
          for ii := 1 to nRec do
            begin
              // PQ Priority
              PQPriority(0);

              // real power regulation
              RealPowerReg;

              // reactive power and voltage regulation
              if QFlg = 0 then
                begin
                  IqCmdPos := Qcmd / max(0.000001, Vmag);
                  IqCmdPos := max(Iqmn, min(Iqmx, IqCmdPos));
                end
              else
                begin
                  ReactivePowerReg;
                  VoltageReg;
                end;

              // Current regulator
              LVPL;
              LVQL;
              CurrentReg;

              if (SimMechFlg > 0) then
                begin
                  AeroDynamic;
                  TorqueReg;
                  PitchControl;
                  APCLogic;
                  WindInertia;
                  SwingModel;
                end;

              // perform integration
              Integrate;

              // current limiting logic
              CurrentLimiting;
            end;
        end
      else
        begin
          E012[0] := V012[0];
          E012[1] := V012[1];
          E012[2] := V012[2];
        end;

      CalcCurrent(i);

      If DebugTrace = 1 Then
        WriteTraceRecord;
    end;

end;

{ ------------------------------------------------------------------------------------------------------------- }
PROCEDURE TGE_WTG3_Model.DoHelpCmd;
{ ------------------------------------------------------------------------------------------------------------- }

Var
  HelpStr: String;
  AnsiHelpStr: AnsiString;
  CRLF: String;

Begin

  CRLF := #13#10;
  HelpStr := 'Rthev= per unit Thevenin equivalent R.' + CRLF;
  HelpStr := HelpStr + 'Xthev= per unit Thevenin equivalent X.' + CRLF;
  HelpStr := HelpStr + 'Vss= steady state voltage magnitude.' + CRLF;
  HelpStr := HelpStr + 'Pss= steady state output real power.' + CRLF;
  HelpStr := HelpStr + 'Qss= steady state output reactive power.' + CRLF;
  HelpStr := HelpStr + 'vwind= wind speed in m/s' + CRLF;
  HelpStr := HelpStr + 'QMOde= Q control mode (0:Q, 1:PF, 2:VV)' + CRLF;
  HelpStr := HelpStr + 'SimMechFlg= 1 to simulate mechanical system' + CRLF;
  HelpStr := HelpStr + 'APCFlg= 1 to enable active power control' + CRLF;
  HelpStr := HelpStr +
    'QFlg= 1 to enable reactive power and voltage control' + CRLF;
  HelpStr := HelpStr +
    'DebugTrace= 1 to save dynamic simulation result in csv file' + CRLF;
  HelpStr := HelpStr + 'delt0= user defined internal simulation step' + CRLF;
  HelpStr := HelpStr +
    'ratedKVA= WTG power rating (either 3600 or 1500)' + CRLF;
  HelpStr := HelpStr + 'V#_VoltVar= V points on Volt-Var curve' + CRLF;
  HelpStr := HelpStr + 'Q#_VoltVar= Q points on Volt-Var curve' + CRLF;
  HelpStr := HelpStr + 'N_WTG= number of WTG in aggregation' + CRLF;
  HelpStr := HelpStr + 'Help: this help message.';

  AnsiHelpStr := AnsiString(HelpStr); // Implicit typecast

End;

{ ------------------------------------------------------------------------------------------------------------- }
function TGE_WTG3_Model.Get_Variable(i: Integer): Double;
{ ------------------------------------------------------------------------------------------------------------- }
begin

  Result := -9999;
  Case i of
    1:
      Result := userTrip;
    2:
      Result := wtgTrip;
    3:
      Result := Pcurtail;
    4:
      Result := Pcmd;
    5:
      Result := Pgen;
    6:
      Result := Qcmd;
    7:
      Result := Qgen;
    8:
      Result := Vref;
    9:
      Result := Vmag;
    10:
      Result := vwind;
    11:
      Result := WtRef;
    12:
      Result := Wt;
    13:
      Result := dOmg;
    14:
      Result := dFrqPuTest;
    15:
      Result := QMode;
    16:
      Result := Qref;
    17:
      Result := PFref;
    18:
      Result := thetaPitch;
  Else

  End;

end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.Set_Variable(i: Integer; const Value: Double);
{ ------------------------------------------------------------------------------------------------------------- }
begin
  Case i of
    1:
      userTrip := round(Value);
    3:
      Pcurtail := Value;
    10:
      vwind := Value;
    14:
      dFrqPuTest := Value;
    15:
      QMode := round(Value);
    16:
      Qref := Value;
    17:
      PFref := Value;
  Else
    { Do Nothing for other variables: they are read only }
  End;
end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.InitTraceFile;
{ ------------------------------------------------------------------------------------------------------------- }
Var
  headerStr: string;
begin

  AssignFile(TraceFile, 'GE_WTG3_Trace.CSV');
  Rewrite(TraceFile);

  headerStr := 'Time,Iteration,delt,nRec,ratedVln,ratedAmp,' +
    'vwind,thetaPitch,WtRef,Wt,Pmech,Pcmd,Pele,Pgen,Qcmd,Qele,Qgen,' +
    'Vref,Vmag,VdPos,VqPos,VdNeg,VqNeg,IdPos,IqPos,IdNeg,IqNeg,dOmg,' +
    'debug1,debug2,debug3,debug4,debug5,debug6,debug7,debug8,debug9,debut10';
  Write(TraceFile, headerStr);

  Writeln(TraceFile);

  CloseFile(TraceFile);
end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TGE_WTG3_Model.WriteTraceRecord;
{ ------------------------------------------------------------------------------------------------------------- }
begin
  // AssignFile(TraceFile, 'GE_WTG3_Trace.CSV');
  Append(TraceFile);
  Write(TraceFile, DynaData^.t, ',');
  Write(TraceFile, DynaData^.IterationFlag, ',');
  Write(TraceFile, delt, ',');
  Write(TraceFile, nRec, ',');
  Write(TraceFile, ratedVln, ',');
  Write(TraceFile, ratedAmp, ',');
  Write(TraceFile, vwind, ',');
  Write(TraceFile, thetaPitch, ',');
  Write(TraceFile, WtRef, ',');
  Write(TraceFile, Wt, ',');
  Write(TraceFile, Pmech, ',');
  Write(TraceFile, Pcmd, ',');
  Write(TraceFile, Pele, ',');
  Write(TraceFile, Pgen, ',');
  Write(TraceFile, Qcmd, ',');
  Write(TraceFile, Qele, ',');
  Write(TraceFile, Qgen, ',');
  Write(TraceFile, Vref, ',');
  Write(TraceFile, Vmag, ',');
  Write(TraceFile, VdPos, ',');
  Write(TraceFile, VqPos, ',');
  Write(TraceFile, VdNeg, ',');
  Write(TraceFile, VqNeg, ',');
  Write(TraceFile, IdPos, ',');
  Write(TraceFile, IqPos, ',');
  Write(TraceFile, IdNeg, ',');
  Write(TraceFile, IqNeg, ',');
  Write(TraceFile, dOmg, ',');
  Write(TraceFile, debugVar[1], ',');
  Write(TraceFile, debugVar[2], ',');
  Write(TraceFile, debugVar[3], ',');
  Write(TraceFile, debugVar[4], ',');
  Write(TraceFile, debugVar[5], ',');
  Write(TraceFile, debugVar[6], ',');
  Write(TraceFile, debugVar[7], ',');
  Write(TraceFile, debugVar[8], ',');
  Write(TraceFile, debugVar[9], ',');
  Write(TraceFile, debugVar[10]);

  Writeln(TraceFile);
  CloseFile(TraceFile);
end;

initialization

end.
