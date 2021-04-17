unit StorageVars;

{
  ----------------------------------------------------------
  Copyright (c) 2009-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

       Definition of Storage Public Data Record for passing to DLLs and other object
}

interface

uses
    Ucomplex;

type

{Struct to pass basic data to user-written DLLs}
    TStorageVars = packed record

        kWrating: Double;
        kWhRating: Double;
        kWhStored: Double;
        kWhReserve: Double;
        ChargeEff: Double;
        DisChargeEff: Double;
        kVArating: Double;
        kVStorageBase: Double;
        kvarRequested: Double;
        RThev: Double;
        XThev: Double;
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
