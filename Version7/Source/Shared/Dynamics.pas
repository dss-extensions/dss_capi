unit Dynamics;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Definitions of constants and structures for the Solution object and user-written dynamic models}

interface

{Solution modes}

type
{$PUSH}
{$SCOPEDENUMS ON}
{$Z4} // keep it as as int32 for backwards compatibility
    TSolveMode = (
        SNAPSHOT = 0,
        DAILYMODE = 1,
        YEARLYMODE = 2,  // 8760 hour
        MONTECARLO1 = 3,
        LOADDURATION1 = 4,
        PEAKDAY = 5,
        DUTYCYCLE = 6,
        DIRECT = 7,
        MONTEFAULT = 8,  // Monte Carlo Fault Study
        FAULTSTUDY = 9,  // Run through all buses and compute Voc and Zsc; Then ask for fault current.
        MONTECARLO2 = 10,
        MONTECARLO3 = 11,
        LOADDURATION2 = 12,
        AUTOADDFLAG = 13,
        DYNAMICMODE = 14,
        HARMONICMODE = 15,
        GENERALTIME = 16,
        HARMONICMODET = 17 // Adds the variable for the Sequential-time harmonics mode
    );
{$SCOPEDENUMS OFF}
{$POP}

   {Variables needed for dynamics and user-written models.}
    TDynamicsRec = packed record
         {time vars}
        h,     // Time step size in sec for dynamics
        t,     // sec from top of hour
        tstart,
        tstop: Double;
        IterationFlag: Integer;  {0=New Time Step; 1= Same Time Step as last iteration}
        SolutionMode: TSolveMode;   //  PEAKSNAP, DAILYMODE, YEARLYMODE, MONTECARLO, etc.  (see DSSGlobals)
        intHour: Integer;  // time, in hours as an integer
        dblHour: Double;   // time, in hours as a floating point number including fractional part

    end;


implementation


end.
