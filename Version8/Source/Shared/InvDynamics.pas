unit InvDynamics;

interface

uses PCElement, Circuit, UComplex, mathutil, Utilities, UcMatrix;

type
  {Structure for hosting data and solving for each inverter based element}
  TInvDynamicVars = Packed Record
    Vgrid                 : array of Polar;     // Grid voltage at the point of connection per phase
    dit,                                        // Current's first derivative per phase
    it,                                         // Current's integration per phase
    itHistory,                                  // Shift register for it
    m                     : array of Double;    // Average duty cycle per phase
    iMaxPPhase,
    kP,                                         // PI controller gain
    CtrlTol,                                    // Control loop tolerance
    SMThreshold,                                // Voltage threshold for entering into safe mode
    RatedVDC,                                   // Rated DC voltage at the inverter's input
    LS,                                         // Series inductance, careful, it cannot be 0 in dyn mode
    RS,                                         // Series resistance (filter)
    BasekV,                                     // BAse kV depending on the number of phases
    MaxVS,                                      // Max Voltage at the inverter terminal to safely operate the inverter
    MinVS,                                      // Min Voltage at the inverter terminal to safely operate the inverter
    MinAmps,                                    // Min amps required for exporting energy
    ISP                   : Double;             // Current setpoint according to the actual DER kW
    Discharging,                                // To verify if the storage device is discharging
    SafeMode              : Boolean;            // To indicate weather the Inverter has entered into safe mode

    function Get_InvDynValue(myindex : Integer): Double;
    function Get_InvDynName(myindex : Integer): String;
    Procedure Set_InvDynValue(myindex : Integer; myValue : Double);
    Procedure SolveDynamicStep(i, ActorID : Integer; PICtrl : PPICtrl);
    Procedure SolveModulation(i, ActorID : Integer; PICtrl : PPICtrl);
    function DoGFM_Mode(ActorID, NPhases : Integer): Boolean;
    Procedure CalcGFMYprim(ActorID, NPhases : Integer; YMatrix  : pTcMatrix);
    Procedure CalcGFMVoltage(ActorID, NPhases : Integer; x : pComplexArray);

  End;

  var
  NumInvDynVars           : Integer = 9;

implementation

uses DSSGlobals;

  // Returns the value of the given state variable using the index lo localize it
  function TInvDynamicVars.Get_InvDynValue(myindex : Integer): Double;
  Begin
    case myindex of
      0 :  if length(Vgrid) > 0       then Result :=  Vgrid[0].mag  else Result :=  0;
      1 :  if length(dit) > 0         then Result :=  dit[0]        else Result :=  0;
      2 :  if length(it) > 0          then Result :=  it[0]         else Result :=  0;
      3 :  if length(ithistory) > 0   then Result :=  ithistory[0]  else Result :=  0;
      4 :  Result :=  RatedVDC;
      5 :  if length(m) > 0           then Result :=  m[0]          else Result :=  0;
      6 :  Result :=  ISP;
      7 :  Result :=  LS;
      8 :  Result :=  iMaxPPhase;
    else
      Result :=  0;
    end;

  End;

  // Sets the value for the state variable indicated in the index
  Procedure TInvDynamicVars.Set_InvDynValue(myindex : Integer; myValue : Double);
  Begin
      case myindex of
      0 :  ;  // Read only
      1 :  dit[0]       :=  myValue;
      2 :  it[0]        :=  myValue;
      3 :  ithistory[0] :=  myValue;
      4 :  RatedVDC     :=  myValue;
      5 :  m[0]         :=  myValue;
      6 :  ; // Read only
      7 :  LS           :=  myValue;
      8 :  iMAxPPhase   :=  myValue;
    else
      // Do nothing
    end;

  End;

  // Returns the name of the state variable located by the given index
  function TInvDynamicVars.Get_InvDynName(myindex : Integer): String;
  Begin
      case myindex of
        0 : Result := 'Grid voltage';
        1 : Result := 'di/dt';
        2 : Result := 'it';
        3 : Result := 'it History';
        4 : Result := 'Rated VDC';
        5 : Result := 'Avg duty cycle';
        6 : Result := 'Target (Amps)';
        7 : Result := 'Series L';
        8 : Result := 'Max. Amps (phase)';
    else
      Result :=  'Unknown variable';
    end;

  End;

  // Solves the derivative term of the differential equation to be integrated
  Procedure TInvDynamicVars.SolveDynamicStep(i, ActorID : Integer; PICtrl : PPICtrl);
  var
    myDCycle,
    iDelta,
    iErrorPct,
    iError  : Double;

  Begin
    with ActiveCircuit[ActorID].Solution do
    Begin
      SolveModulation(i, ActorID, PICtrl);
      dit[i]  := ( (m[i] * RatedVDC) - (RS * it[i]) - Vgrid[i].mag ) / LS;  // Solves derivative
    End;
  End;

  // Calculates and stores the averaged modulation factor for controlling the inverter output
  Procedure TInvDynamicVars.SolveModulation(i, ActorID : Integer; PICtrl : PPICtrl);
  var
    myDCycle,
    iDelta,
    iErrorPct,
    iError  : Double;

  Begin
    with ActiveCircuit[ActorID].Solution do
    Begin
      if (DynaVars.IterationFlag <> 0) then
      Begin                                                     // duty cycle at time h
        iError    :=  ( ISP - it[i] );                          // Only recalculated on the second iter
        iErrorPct :=  iError/ISP;
        if Abs(iErrorPct) > CtrlTol  then
        Begin
            iDelta    :=  PICtrl^.SolvePI( IError );
            myDCycle  :=  m[i] + iDelta;
            if Vgrid[i].mag > MinVS then
            Begin
              if SafeMode then
              Begin
               //Coming back from safe operation, need to boost duty cycle
               m[i]     :=  ( ( RS * it[i] ) + Vgrid[i].mag ) / RatedVDC;
               SafeMode :=  False;
              End
              else
                if ( myDCycle <= 1 ) and ( myDCycle > 0 ) then m[i]    :=  myDCycle;
            End
            else
            Begin
                m[i]      := 0;
                SafeMode  :=  True;
            End;
        End;
      End;

    End;
  End;
//---------------------------------------------------------------------------------------
//|             Calculates the current phasors to match with the target (v)             |
//---------------------------------------------------------------------------------------
  function TInvDynamicVars.DoGFM_Mode(ActorID, NPhases : Integer): Boolean;
  Var
    j,
    i           : Integer;
    myError     : Double;

  Begin
    // Checks for initialization
    if length(it) < NPhases then
    Begin
      // If enters here is because probably this is the first iteration
      setlength(it, NPhases);  // Used to correct the magnitude
      setlength(dit, NPhases); // Used to correct the phase angle
      // Initializes the currents vector for phase and angle
      for j := 1 to NPhases do
      Begin
        it[j - 1]   :=  0;
        dit[j - 1]  :=  ( j - 1 ) * ( TwoPi / -3  );
      End;
    End;

    with ActiveCircuit[ActorID].Solution do
    Begin
      iMaxPPhase :=  ISP / BasekV;
      for i := 1 to NPhases do
      Begin
        myError   :=  1 - ( Vgrid[i - 1].mag / BasekV );
        if Discharging then
        Begin
          // Corrects the magnitude
          it[i - 1]         :=  it[i - 1] + ( myError * ( ISP / BasekV ) * kP * 1000 );
          // Checks if the IBR is out of the saturaton point
          if it[i - 1] > ( ISP / BasekV ) then it[i - 1]  :=  ( ISP / BasekV );

          // Corrects the phase angle
          myError           :=  ( ( (i - 1) * TwoPi / -3 ) - Vgrid[i - 1].ang );
          dit[i - 1]        :=  dit[i - 1] + myError;
          Vgrid[i - 1].ang  :=  dit[i - 1];  // Saves at the Vgrid register for future use
        End
        else
          if not Discharging then Result  :=  False;
      End;
    End;
  End;

//---------------------------------------------------------------------------------------
//| Calculates the equivalent short circuit impedance for the inverter operating in GFM |
//| Similar to the calculation user for VSource, replacing some variables with constants|
//---------------------------------------------------------------------------------------
  Procedure TInvDynamicVars.CalcGFMYprim(ActorID, NPhases : Integer; YMatrix  : pTcMatrix);
  var
    Z       : TcMatrix;
    Zs,
    Zm      : Complex;
    a,
    b,
    c,
    R0,
    X0,
    X1,
    R1,
    R2,
    X2,
    Isc1,
    Xs,
    Rs,
    Rm,
    Xm      : Double;
    i,
    j       : Integer;
  Begin
    Z    := TCmatrix.CreateMatrix(YMatrix^.Order);

    X1    := ( Sqr( BasekV ) / ISP ) / Sqrt(1.0 + 0.0625);
    R1    := X1 /4; // Uses defaults
    R2    := R1;     // default Z2 = Z1
    X2    := X1;
    Isc1  := ( ISP * 1000.0 / ( sqrt(3) * BasekV ) ) / NPhases;
  //  Compute R0, X0
    a     :=  10;
    b     :=  ( 4.0*( R1 + X1 * 3 ) );
    c     :=  ( 4.0 * ( R1*R1 + X1*X1 )- SQR( ( sqrt(3) * BasekV * 1000.0 ) / Isc1));
    R0    := QuadSolver( a, b, c );
    X0    := R0 * 3;
    // for Z matrix
    Xs    := ( 2.0 * X1 + X0 ) / 3.0;
    Rs    := ( 2.0 * R1 + R0 ) / 3.0;
    Rm    := ( R0 - R1 ) / 3.0;
    Xm    := ( X0 - X1 ) / 3.0;
    Zs    :=  cmplx( Rs, Xs );
    Zm    :=  cmplx( Rm, Xm );

    FOR i := 1 to NPhases DO Begin
       Z.SetElement( i, i, Zs );
       FOR j := 1 to i-1 DO Begin
           Z.SetElemsym( i, j, Zm );
       End;
    End;

    Z.Invert();
    YMatrix^.CopyFrom(Z);
  End;
//---------------------------------------------------------------------------------------
//|   Calculates the voltage magnitudes and angles for facilitating GFM control mode    |
//---------------------------------------------------------------------------------------
  Procedure TInvDynamicVars.CalcGFMVoltage(ActorID, NPhases : Integer; x : pComplexArray);
  var
    refAngle  : Double;
    i         : Integer;
  Begin
    refAngle  :=  0;
    FOR i := 1 to NPhases Do
      x^[i] :=  pdegtocomplex( BasekV, ( 360.0 + refAngle - ( ( i - 1 ) * 360.0 ) / NPhases ) );
  End;

end.
