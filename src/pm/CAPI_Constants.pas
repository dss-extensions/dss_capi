UNIT CAPI_Constants;

{
    Constants extracted from OpenDSSEngine_TLB.pas
}

INTERFACE

// Constants for enum MonitorModes
const
  dssVI = $00000000;
  dssPower = $00000001;
  dssSequence = $00000010;
  dssMagnitude = $00000020;
  dssPosOnly = $00000040;
  dssTaps = $00000002;
  dssStates = $00000003;

// Constants for enum SolveModes
const
  dssSnapShot = $00000000;
  dssDutyCycle = $00000006;
  dssDirect = $00000007;
  dssDaily = $00000001;
  dssMonte1 = $00000003;
  dssMonte2 = $0000000A;
  dssMonte3 = $0000000B;
  dssFaultStudy = $00000009;
  dssYearly = $00000002;
  dssMonteFault = $00000008;
  dssPeakDay = $00000005;
  dssLD1 = $00000004;
  dssLD2 = $0000000C;
  dssAutoAdd = $0000000D;
  dssHarmonic = $0000000F;
  dssDynamic = $0000000E;

// Constants for enum Options
const
  dssPowerFlow = $00000001;
  dssAdmittance = $00000002;
  dssNormalSolve = $00000000;
  dssNewtonSolve = $00000001;
  dssStatic = $00000000;
  dssEvent = $00000001;
  dssTime = $00000002;
  dssMultiphase = $00000000;
  dssPositiveSeq = $00000001;
  dssGaussian = $00000001;
  dssUniform = $00000002;
  dssLogNormal = $00000003;
  dssAddGen = $00000001;
  dssAddCap = $00000002;
  dssControlOFF = $FFFFFFFF;

// Constants for enum CapControlModes
const
  dssCapControlVoltage = $00000001;
  dssCapControlKVAR = $00000002;
  dssCapControlCurrent = $00000000;
  dssCapControlPF = $00000004;
  dssCapControlTime = $00000003;

// Constants for enum ActionCodes
const
  dssActionNone = $00000000;
  dssActionOpen = $00000001;
  dssActionClose = $00000002;
  dssActionReset = $00000003;
  dssActionLock = $00000004;
  dssActionUnlock = $00000005;
  dssActionTapUp = $00000006;
  dssActionTapDown = $00000007;

// Constants for enum LoadStatus
const
  dssLoadVariable = $00000000;
  dssLoadFixed = $00000001;
  dssLoadExempt = $00000002;

// Constants for enum LoadModels
const
  dssLoadConstPQ = $00000001;
  dssLoadConstZ = $00000002;
  dssLoadMotor = $00000003;
  dssLoadCVR = $00000004;
  dssLoadConstI = $00000005;
  dssLoadConstPFixedQ = $00000006;
  dssLoadConstPFixedX = $00000007;
  dssLoadZIPV = $00000008;

// Constants for enum LineUnits
const
  dssLineUnitsNone = $00000000;
  dssLineUnitsMiles = $00000001;
  dssLineUnitskFt = $00000002;
  dssLineUnitskm = $00000003;
  dssLineUnitsmeter = $00000004;
  dssLineUnitsft = $00000005;
  dssLineUnitsinch = $00000006;
  dssLineUnitscm = $00000007;
  dssLineUnitsmm = $00000008;
  dssLineUnitsMaxnum = $00000009;


IMPLEMENTATION

END.