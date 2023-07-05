#ifndef DSS_USER_MODELS_H
#define DSS_USER_MODELS_H

#ifdef __cplusplus
#    include <cstdint>
#    include <cstddef>
#else
#    include <stdint.h>
#    include <stdbool.h>
#endif

#ifndef dss_long_bool
#ifdef DSS_CAPI_DLL
#define dss_long_bool int32_t
#else
#define dss_long_bool bool
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

enum SolutionMode 
{
    SOLUTION_SNAPSHOT = 0,
    SOLUTION_DAILYMODE = 1,
    SOLUTION_YEARLYMODE = 2, 
    SOLUTION_MONTECARLO1 = 3,
    SOLUTION_LOADDURATION1 = 4,
    SOLUTION_PEAKDAY = 5,
    SOLUTION_DUTYCYCLE = 6,
    SOLUTION_DIRECT = 7,
    SOLUTION_MONTEFAULT = 8,
    SOLUTION_FAULTSTUDY = 9,  
    SOLUTION_MONTECARLO2 = 10,
    SOLUTION_MONTECARLO3 = 11,
    SOLUTION_LOADDURATION2 = 12,
    SOLUTION_AUTOADDFLAG = 13,
    SOLUTION_DYNAMICMODE = 14,
    SOLUTION_HARMONICMODE = 15,
    SOLUTION_GENERALTIME = 16,
    SOLUTION_HARMONICMODET = 17 
};

enum ECapControlType
{
    CAP_CURRENTCONTROL = 0,
    CAP_VOLTAGECONTROL = 1,
    CAP_KVARCONTROL = 2,
    CAP_TIMECONTROL = 3,
    CAP_PFCONTROL = 4,
    CAP_USERCONTROL = 5
};

enum EControlAction
{
    CTRL_NONE = 0,
    CTRL_OPEN = 1,
    CTRL_CLOSE = 2,
    CTRL_RESET = 3,
    CTRL_LOCK = 4,
    CTRL_UNLOCK = 5,
    CTRL_TAPUP = 6,
    CTRL_TAPDOWN = 7
};

#pragma pack(push, 1)
struct TGeneratorVars 
{
    double 
        Theta,      // Direct-Axis voltage magnitude & angle
        Pshaft,
        Speed,
        w0,         // present Shaft Power and relative Speed, rad/sec, difference from Synchronous speed, w0
                    // actual speed = Speed + w0
        Hmass,      // Per unit mass constant
        Mmass,      // Mass constant actual values (Joule-sec/rad
        D, Dpu,     // Actual and per unit damping factors
        kVArating,
        kVGeneratorBase,
        Xd, Xdp, Xdpp,   // machine Reactances, ohms
        puXd, puXdp, puXdpp,   // machine Reactances, per unit
        dTheta,
        dSpeed,     // Derivatives of Theta and Speed
        ThetaHistory,
        SpeedHistory,   // history variables for integration
        PNominalPerPhase,
        QNominalPerPhase;  //  Target P and Q for power flow solution, watts, vars

    // 32-bit integers}
    int32_t 
        NumPhases,       //Number of phases
        NumConductors,   // Total Number of conductors (wye-connected will have 4)
        Conn;   // 0 = wye; 1 = Delta

    
    //  Revisions (additions) to structure ...
    //  Later additions are appended to end of the structure so that
    //  previously compiled DLLs do not break

    double VThevMag;  // Thevinen equivalent voltage for dynamic model
    double VThevHarm; // Thevinen equivalent voltage mag reference for Harmonic model
    double ThetaHarm; // Thevinen equivalent voltage angle reference for Harmonic model
    double VTarget;   // Target voltage for generator with voltage control
    double RThev;
    double XThev;
    double XRdp;  // Assumed X/R for Xd'
};


struct TCapControlVars 
{
    int32_t
        FCTPhase,
        FPTPhase;   // "ALL" is -1

    double
        ON_Value,
        OFF_Value,
        PFON_Value,
        PFOFF_Value,
        CTRatio,
        PTRatio,
        ONDelay,
        OFFDelay,
        DeadTime,
        LastOpenTime;

    dss_long_bool
        Voverride;

    bool 
        VoverrideEvent,
        VoverrideBusSpecified;     // Added 8-11-11

    int32_t VOverrideBusIndex;

    double 
        Vmax,
        Vmin;
    
    uint8_t /*enum EControlAction*/ FPendingChange;
    bool 
        ShouldSwitch,  // True: action is pending
        Armed;  // Control is armed for switching unless reset
    uint8_t /*enum EControlAction*/ PresentState;
    uint8_t /*enum EControlAction*/ InitialState;

    double 
        SampleP,        // 64-bit number, kW
        SampleQ,        // 64-bit number, kvar
        SampleV,
        SampleCurr;

    int32_t 
        NumCapSteps,
        AvailableSteps,     // available steps in controlled capacitor
        LastStepInService;  // Change this to force an update of cap states

    
    // NOTE: VOverrideBusName and CapacitorName may be UnicodeStrings in Delphi.
    // These pointers could be processed different according to the Pascal compiler.
    char* VOverrideBusName; // Actual string data encoded in UTF-16 
    char* CapacitorName; // Actual string data encoded in UTF-16
    
    int32_t ControlActionHandle;
    int32_t CondOffset; // Offset for monitored terminal
};


struct TStorageVars
{
    double
        kWrating,
        kWhRating,
        kWhStored,
        kWhReserve,
        ChargeEff,
        DisChargeEff,
        kVStorageBase,
        RThev,
        XThev;

    double
        // Inverter Related Properties
        kVArating,
        kvarlimit,
        kvarlimitneg;

    dss_long_bool
        P_Priority,
        PF_Priority;
    
    double
        pctkWrated,
        EffFactor;

    double
        // Interaction with InvControl
        Vreg,
        Vavg,
        VVOperation,
        VWOperation,
        DRCOperation,
        VVDRCOperation,
        WPOperation,
        WVOperation;

    double 
        // Dynamics variables
        Vthev_re,  // Thevenin equivalent voltage (complex) for dynamic model
        Vthev_im,
        ZThev_re,
        ZThev_im,
        Vthevharm, // Thevenin equivalent voltage mag and angle reference for Harmonic model
        Thetaharm, // Thevenin equivalent voltage mag and angle reference for Harmonic model
        VthevMag,  // Thevenin equivalent voltage for dynamic model
        Theta,     // Power angle between voltage and current
        w_grid,    // Grid frequency
        TotalLosses,
        IdlingLosses;

    //32-bit integers
    int32_t 
        NumPhases, // Number of phases
        NumConductors,   // Total Number of conductors (wye-connected will have 4)
        Conn;   // 0 = wye; 1 = Delta
};



struct TDynamicsRec 
{
   // time vars
   double
       h,     // Time step size in sec for dynamics
       t,     // sec from top of hour
       tstart,
       tstop;
   int32_t IterationFlag;  // 0=New Time Step; 1= Same Time Step as last iteration
   int32_t SolutionMode;   //  PEAKSNAP, DAILYMODE, YEARLYMODE, MONTECARLO, etc.  (see DSSGlobals)
   int32_t intHour;  // time, in hours as an integer
   double dblHour;   // time, in hours as a floating point number including fractional part
};


// NOTE: Maxlen argument is to better accommodate Fortran strings.  VB also
//       Caller must allocate space for pchar values

#ifdef _WIN32
#    define DSS_MODEL_CALLBACK(ret_type, fname) ret_type (__stdcall *fname)
#else
#    define DSS_MODEL_CALLBACK(ret_type,fname ) ret_type (*fname)
#endif

struct TDSSCallBacks 
{
    DSS_MODEL_CALLBACK(void, MsgCallBack)(char *S, uint32_t Maxlen); // Make use of DSS Message handling

    // Routines for using DSS Parser.  This allows you to write models that accept
    // syntax like other DSS scripts.
    DSS_MODEL_CALLBACK(void, GetIntValue)(int32_t *i); // Get next param as an integer
    DSS_MODEL_CALLBACK(void, GetDblValue)(double *x); // Get next param as a double
    DSS_MODEL_CALLBACK(void, GetStrValue)(char *S, uint32_t MaxLen);
    
    // Get next param as a string <= maxlen characters  (Cardinal = 32-bit unsigned)}
    // caller must allocate space for s (Maxlen chars)
    DSS_MODEL_CALLBACK(void, LoadParser)(char *S, uint32_t MaxLen); // Copies a string into a special instance of the DSS parser
    DSS_MODEL_CALLBACK(int32_t, NextParam)(char *S, uint32_t MaxLen);
    // Advance to the next parameter and
    // Get name of the param just retrieved, if one was given.
    // Returns length of parameter found.  If 0, then end of string.
    // This is to handle the syntax "paramname=paramvalue" commonly used in DSS scripts
    // Copies the string to the location specified by s up to maxlen characters.
    // Caller must allocate space (MaxLen chars)

    DSS_MODEL_CALLBACK(void, DoDSSCommand)(char *S, uint32_t Maxlen);
    DSS_MODEL_CALLBACK(void, GetActiveElementBusNames)(char *Name1, uint32_t Len1, char *Name2, uint32_t Len2);
    DSS_MODEL_CALLBACK(void, GetActiveElementVoltages)(int32_t *NumVoltages, double /* complex */ **V);
    DSS_MODEL_CALLBACK(void, GetActiveElementCurrents)(int32_t *NumCurrents, double /* complex */ **Curr);
    DSS_MODEL_CALLBACK(void, GetActiveElementLosses)(double /* complex */ *TotalLosses, double /* complex */ *LoadLosses, double /* complex */ *NoLoadLosses);
    DSS_MODEL_CALLBACK(void, GetActiveElementPower)(int32_t Terminal, double /* complex */ *TotalPower);
    DSS_MODEL_CALLBACK(void, GetActiveElementNumCust)(int32_t *NumCust, int32_t *TotalCust);
    DSS_MODEL_CALLBACK(void, GetActiveElementNodeRef)(int32_t Maxsize, int32_t** NodeReferenceArray);  // calling program must allocate
    DSS_MODEL_CALLBACK(int32_t, GetActiveElementBusRef)(int32_t Terminal);
    DSS_MODEL_CALLBACK(void, GetActiveElementTerminalInfo)(int32_t *NumTerminals, int32_t *NumConds, int32_t *NumPhases);
    DSS_MODEL_CALLBACK(void, GetPtrToSystemVarray)(void *V, int32_t *iNumNodes); // Returns pointer to Solution.V and size
    DSS_MODEL_CALLBACK(int32_t, GetActiveElementIndex)(void);
    
    DSS_MODEL_CALLBACK(bool, IsActiveElementEnabled)(void);
    DSS_MODEL_CALLBACK(bool, IsBusCoordinateDefined)(int32_t BusRef);
    DSS_MODEL_CALLBACK(void, GetBusCoordinate)(int32_t BusRef, double *X, double* Y);
    DSS_MODEL_CALLBACK(double, GetBuskVBase)(int32_t BusRef);
    DSS_MODEL_CALLBACK(double, GetBusDistFromMeter)(int32_t BusRef);

    DSS_MODEL_CALLBACK(void, GetDynamicsStruct)(void **DynamicsStruct); // Returns pointer to dynamics variables structure
    DSS_MODEL_CALLBACK(double, GetStepSize)(void); // Return just 'h' from dynamics record
    DSS_MODEL_CALLBACK(double, GetTimeSec)(void); // returns t in sec from top of hour
    DSS_MODEL_CALLBACK(double, GetTimeHr)(void); // returns time as a double in hours

    DSS_MODEL_CALLBACK(void, GetPublicDataPtr)(void **PublicData, int32_t *PublicDataBytes);
    DSS_MODEL_CALLBACK(int32_t, GetActiveElementName)(char *FullName, uint32_t MaxNameLen);
    DSS_MODEL_CALLBACK(void*, GetActiveElementPtr)(void);  // Returns pointer to active circuit element
    
    //TODO: check FPC vs Delphi compatibility for const parameters in ControlQueuePush
    DSS_MODEL_CALLBACK(int32_t, ControlQueuePush)(const int32_t Hour, const double Sec, const int32_t Code, const int32_t ProxyHdl, void *Owner);
    DSS_MODEL_CALLBACK(void, GetResultStr)(char *S, uint32_t Maxlen);
};

#ifdef __cplusplus
} // extern "C"
#    ifdef _WIN32
#        define DSS_MODEL_DLL(ret_type) extern "C" __declspec(dllexport) ret_type __stdcall
#    else
#        define DSS_MODEL_DLL(ret_type) extern "C" ret_type
#    endif
#else
#    ifdef _WIN32
#        define DSS_MODEL_DLL(ret_type) __declspec(dllexport) ret_type __stdcall
#    else
#        define DSS_MODEL_DLL(ret_type) ret_type
#    endif
#endif

#undef DSS_MODEL_CALLBACK
#pragma pack(pop)


#endif // DSS_USER_MODELS_H
