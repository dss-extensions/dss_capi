unit InvDynamics;

interface

type
  {Struct to pass basic data to user-written DLLs}
  TInvDynamicVars = Packed Record
    Vgrid,                                      // Grid voltage at the point of connection per phase
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
    MaxVS,                                      // Max Voltage at the inverter terminal to safely operate the inverter
    MinVS,                                      // Min Voltage at the inverter terminal to safely operate the inverter
    ISP                   : Double;             // Current setpoint according to the actual DER kW
    SafeMode              : Boolean;            // To indicate weather the Inverter has entered into safe mode

  End;

implementation

end.
