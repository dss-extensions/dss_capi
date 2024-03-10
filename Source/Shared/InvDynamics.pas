unit InvDynamics;

interface

uses
    PCElement,
    Circuit,
    UComplex,
    mathutil,
    Utilities,
    UcMatrix;

type
  {Structure for hosting data and solving for each inverter based element}
    TInvDynamicVars = packed record
        Vgrid: array of Polar;     // Grid voltage at the point of connection per phase
        dit,                                        // Current's first derivative per phase
        it,                                         // Current's integration per phase
        itHistory,                                  // Shift register for it
        VDelta,                                     // for black start operation (GFM)
        ISPDelta,                                   // for moving the current target per phase in black start operation (GFM)
        AngDelta,                                   // For correcting the phase angle
        m: array of Double;    // Average duty cycle per phase
        iMaxPPhase,
        kP,                                         // PI controller gain
        CtrlTol,                                    // Control loop tolerance
        SMThreshold,                                // Voltage threshold for entering into safe mode
        RatedVDC,                                   // Rated DC voltage at the inverter's input
        LS,                                         // Series inductance, careful, it cannot be 0 in dyn mode
        RS,                                         // Series resistance (filter)
        BasekV,                                     // BAse kV depending on the number of phases
        BaseV,                                      // For GFM mode, avoid messing with things
        MaxVS,                                      // Max Voltage at the inverter terminal to safely operate the inverter
        MinVS,                                      // Min Voltage at the inverter terminal to safely operate the inverter
        MinAmps,                                    // Min amps required for exporting energy
        mKVARating,                                 // For GFM impedance calculation and avoid messing with other calcs
        RatedkVLL,                                  // As defined when decalred the element, for GFM impedance calculation and avoid messing with other calcs
        ILimit,                                     // Created for limiting the output current without entering into safe mode
        IComp,                                      // For storing the compensation value when the Amps limiter is active
        VError,                                     // Stores the systemic error correction factor for current limiting
        ISP: Double;             // Current setpoint according to the actual DER kW
        Discharging,                                // To verify if the storage device is discharging
        ResetIBR,                                   // flag for forcing the IBR to turn OFF
        SafeMode: Boolean;            // To indicate weather the Inverter has entered into safe mode
        SfModePhase: array of Boolean;   // To identify when to restart the phase

        function Get_InvDynValue(myindex, NumPhases: Integer): Double;
        function Get_InvDynName(myindex: Integer): String;
        procedure Set_InvDynValue(myindex: Integer; myValue: Double);
        procedure SolveDynamicStep(i, ActorID: Integer; PICtrl: PPICtrl);
        procedure SolveModulation(i, ActorID: Integer; PICtrl: PPICtrl);
        procedure FixPhaseAngle(ActorID, idx: Integer);
        procedure CalcGFMYprim(ActorID, NPhases: Integer; YMatrix: pTcMatrix);
        procedure CalcGFMVoltage(ActorID, NPhases: Integer; x: pComplexArray);
        procedure InitDynArrays(NPhases: Integer);

    end;

var
    NumInvDynVars: Integer = 9;

implementation

uses
    DSSGlobals;

  // Returns the value of the given state variable using the index lo localize it
function TInvDynamicVars.Get_InvDynValue(myindex, NumPhases: Integer): Double;
begin
    case myindex of
        0:
            if length(Vgrid) > 0 then
                Result := Vgrid[NumPhases - 1].mag
            else
                Result := 0;
        1:
            if length(dit) > 0 then
                Result := dit[NumPhases - 1]
            else
                Result := 0;
        2:
            if length(it) > 0 then
                Result := it[NumPhases - 1]
            else
                Result := 0;
        3:
            if length(ithistory) > 0 then
                Result := ithistory[NumPhases - 1]
            else
                Result := 0;
        4:
            Result := RatedVDC;
        5:
            if length(m) > 0 then
                Result := m[0]
            else
                Result := 0;
        6:
            Result := ISP;
        7:
            Result := LS;
        8:
            Result := iMaxPPhase;
    else
        Result := 0;
    end;

end;

  // Sets the value for the state variable indicated in the index
procedure TInvDynamicVars.Set_InvDynValue(myindex: Integer; myValue: Double);
begin
    case myindex of
        0: ;  // Read only
        1:
            dit[0] := myValue;
        2:
            it[0] := myValue;
        3:
            ithistory[0] := myValue;
        4:
            RatedVDC := myValue;
        5:
            m[0] := myValue;
        6: ; // Read only
        7:
            LS := myValue;
        8:
            iMAxPPhase := myValue;
    else
      // Do nothing
    end;

end;

  // Returns the name of the state variable located by the given index
function TInvDynamicVars.Get_InvDynName(myindex: Integer): String;
begin
    case myindex of
        0:
            Result := 'Grid voltage';
        1:
            Result := 'di/dt';
        2:
            Result := 'it';
        3:
            Result := 'it History';
        4:
            Result := 'Rated VDC';
        5:
            Result := 'Avg duty cycle';
        6:
            Result := 'Target (Amps)';
        7:
            Result := 'Series L';
        8:
            Result := 'Max. Amps (phase)';
    else
        Result := 'Unknown variable';
    end;

end;

  // Solves the derivative term of the differential equation to be integrated
procedure TInvDynamicVars.SolveDynamicStep(i, ActorID: Integer; PICtrl: PPICtrl);
  {
  var
    myDCycle,
    iDelta,
    iErrorPct,
    iError  : Double;
  }
begin
    with ActiveCircuit[ActorID].Solution do
    begin
        SolveModulation(i, ActorID, PICtrl);
        if SafeMode then
            dit[i] := 0
        else
            dit[i] := ((m[i] * RatedVDC) - (RS * it[i]) - Vgrid[i].mag) / LS;  // Solves derivative
    end;
end;

  // Calculates and stores the averaged modulation factor for controlling the inverter output
procedure TInvDynamicVars.SolveModulation(i, ActorID: Integer; PICtrl: PPICtrl);
var
    myDCycle,
    iDelta,
    iErrorPct,
    iError: Double;

begin
    with ActiveCircuit[ActorID].Solution do
    begin
        if (DynaVars.IterationFlag <> 0) then
        begin                                                     // duty cycle at time h
            iError := (ISP - it[i]);                          // Only recalculated on the second iter
            iErrorPct := iError / ISP;
            if Abs(iErrorPct) > CtrlTol then
            begin
                iDelta := PICtrl^.SolvePI(IError);
                myDCycle := m[i] + iDelta;
                if (Vgrid[i].mag > MinVS) or (MinVS = 0) then
                begin
                    if SafeMode or SfModePhase[i] then
                    begin
             //Coming back from safe operation, need to boost duty cycle
                        m[i] := ((RS * it[i]) + Vgrid[i].mag) / RatedVDC;
                        SafeMode := FALSE;
                        SfModePhase[i] := FALSE;
                    end
                    else
                    if (myDCycle <= 1) and (myDCycle > 0) then
                        m[i] := myDCycle;
                end
                else
                begin
                    m[i] := 0;
                    it[i] := 0;
                    itHistory[i] := 0;
                    SafeMode := TRUE;
                    SfModePhase[i] := TRUE;
                end;
            end;
        end;

    end;
end;
//---------------------------------------------------------------------------------------
//|             Calculates the current phasors to match with the target (v)             |
//---------------------------------------------------------------------------------------
procedure TInvDynamicVars.FixPhaseAngle(ActorID, idx: Integer);
{  Var
    myError     : Double;}

begin
    // Corrects the phase angle
    AngDelta[idx] := AngDelta[idx] + (((idx * TwoPi) / -3) - Vgrid[idx].ang);
    Vgrid[idx].ang := AngDelta[idx];
end;

//---------------------------------------------------------------------------------------
//| Calculates the equivalent short circuit impedance for the inverter operating in GFM |
//| Similar to the calculation user for VSource, replacing some variables with constants|
//---------------------------------------------------------------------------------------
procedure TInvDynamicVars.CalcGFMYprim(ActorID, NPhases: Integer; YMatrix: pTcMatrix);
var
    Z: TcMatrix;
    Zs,
    Zm: Complex;
    a,
    b,
    c,
    R0,
    X0,
    X1,
    R1,
   // R2,
   // X2,
   // X1R1,
    X0R0,
    Isc1,
    Xs,
    Rs,
    Rm,
    Xm: Double;
    i,
    j: Integer;
begin
    Z := TCmatrix.CreateMatrix(YMatrix^.Order);

    X1 := (Sqr(RatedkVLL) / mKVARating) / Sqrt(1.0 + 0.0625);
    R1 := X1 / 4; // Uses defaults
    // R2    := R1;     // default Z2 = Z1
    // X2    := X1;
    R0 := 1.9;
    X0 := 5.7;
    // X1R1  := X1/R1;
    X0R0 := X0 / R0;
    Isc1 := (mKVARating * 1000.0 / (sqrt(3) * RatedkVLL)) / NPhases;
  //  Compute R0, X0
    a := 10;
    b := (4.0 * (R1 + (X1 * X0R0)));
    c := (4.0 * (R1 * R1 + X1 * X1) - SQR((sqrt(3) * RatedkVLL * 1000.0) / Isc1));
    R0 := QuadSolver(a, b, c);
    X0 := R0 * X0R0;
    // for Z matrix
    Xs := (2.0 * X1 + X0) / 3.0;
    Rs := (2.0 * R1 + R0) / 3.0;
    Rm := (R0 - R1) / 3.0;
    Xm := (X0 - X1) / 3.0;
    Zs := cmplx(Rs, Xs);
    Zm := cmplx(Rm, Xm);

    for i := 1 to NPhases do
    begin
        Z.SetElement(i, i, Zs);
        for j := 1 to i - 1 do
        begin
            Z.SetElemsym(i, j, Zm);
        end;
    end;

    Z.Invert();
    YMatrix^.CopyFrom(Z);
end;
//---------------------------------------------------------------------------------------
//|   Calculates the voltage magnitudes and angles for facilitating GFM control mode    |
//---------------------------------------------------------------------------------------
procedure TInvDynamicVars.CalcGFMVoltage(ActorID, NPhases: Integer; x: pComplexArray);
var
    refAngle: Double;
    i: Integer;
begin
    refAngle := 0;
    for i := 1 to NPhases do
        x^[i] := pdegtocomplex(BaseV, (360.0 + refAngle - ((i - 1) * 360.0) / NPhases));
end;
//---------------------------------------------------------------------------------------
//|  Initializes all the local vectors using the number of phases given by the caller    |
//---------------------------------------------------------------------------------------
procedure TInvDynamicVars.InitDynArrays(NPhases: Integer);
var
    i: Integer;
begin
    setlength(dit, NPhases);     // Includes the current and past values
    setlength(it, NPhases);
    setlength(itHistory, NPhases);
    setlength(Vgrid, NPhases);
    setlength(m, NPhases);
    setlength(VDelta, NPhases);
    setlength(ISPDelta, NPhases);
    setlength(AngDelta, NPhases);
    setlength(SfModePhase, NPhases);

    for i := 0 to (NPhases - 1) do
    begin
        SfModePhase[i] := FALSE;
        it[i] := 0;
        itHistory[i] := 0;
        dit[i] := 0;
        m[i] := 0;
    end;
    SafeMode := FALSE;
end;


end.
