unit Storage2Vars;

{
  ----------------------------------------------------------
  Copyright (c) 2009-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

       Definition of Storage Public Data Record for passing to DLLs and other object
}

interface

uses
    UComplex, DSSUcomplex;

type

    // Struct to pass basic data to user-written DLLs
    TStorage2Vars = {$IFNDEF DSS_CAPI_NO_PACKED_RECORDS}packed{$ENDIF} record

        kWrating: Double;
        kWhRating: Double;
        kWhStored: Double;
        kWhReserve: Double;

        ChargeEff: Double;
        DisChargeEff: Double;
        kVStorageBase: Double;
        RThev: Double;
        XThev: Double;

        // Inverter Related Properties
        FkVArating: Double;
        Fkvarlimit: Double;
        Fkvarlimitneg: Double;
        P_Priority: Boolean;
        PF_Priority: Boolean;
        pctkWrated: Double;
        EffFactor: Double;


        // Interaction with InvControl
        Vreg: Double;
        Vavg: Double;
        VVOperation: Double;
        VWOperation: Double;
        DRCOperation: Double;
        VVDRCOperation: Double;
        WPOperation: Double;
        WVOperation: Double;
//        kW_out_desired  :Double;


        // Dynamics variables
        Vthev: Complex;  {Thevenin equivalent voltage (complex) for dynamic model}
        ZThev: Complex;
        Vthevharm: Double;  {Thevenin equivalent voltage mag and angle reference for Harmonic model}
        Thetaharm: Double;  {Thevenin equivalent voltage mag and angle reference for Harmonic model}
        VthevMag: Double;    {Thevenin equivalent voltage for dynamic model}
        Theta: Double;   {Power angle between voltage and current}
        w_grid: Double;   {Grid frequency}
        TotalLosses: Double;
        IdlingLosses: Double;

                {32-bit integers}
        NumPhases,       {Number of phases}
        NumConductors,   {Total Number of conductors (wye-connected will have 4)}
        Conn: Integer;   // 0 = wye; 1 = Delta


    end;

implementation

end.
